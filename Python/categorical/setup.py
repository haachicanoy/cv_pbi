# ----------------------------------------------- #
#!/usr/bin/env python
# coding: utf-8
# Crop losses with fine tunning: binary response
# By: Harold Achicanoy
# WUR & ABC
# March 2025
# ----------------------------------------------- #

# ----------------------------------------------- #
# Load libraries
# ----------------------------------------------- #
import torch
import torch.nn as nn
import torch.optim as optim
from torch.optim import lr_scheduler
import torch.backends.cudnn as cudnn
import numpy as np
import torchvision
from torchvision import datasets, models, transforms
import time
import os
from PIL import Image
from tempfile import TemporaryDirectory
import pandas as pd
from torch.utils.data import Dataset, DataLoader
from torchvision.datasets.folder import default_loader
import copy
cudnn.benchmark = True
device = torch.device("cuda:0" if torch.cuda.is_available() else "cpu")

# Load the YAML configuration
import yaml
def load_config(yaml_file):
    with open(yaml_file, "r") as file:
        config = yaml.safe_load(file)
    return config

config = load_config("config.yaml")
cnn_model = config["model"]["name"]
dataset_path = config["dataset"]["path"]
output_path = dataset_path+'/results_'+cnn_model

import os
if not os.path.exists(output_path):
    os.makedirs(output_path)

# ----------------------------------------------- #
# Data augmentation and normalization
# ----------------------------------------------- #
data_transforms = {
    'train': transforms.Compose([
        transforms.RandomResizedCrop(224),
        transforms.RandomHorizontalFlip(),
        transforms.ColorJitter(brightness=0.2, contrast=0.2, saturation=0.2, hue=0.05),  # Lighting variations
        transforms.ToTensor(),
        transforms.Normalize([0.485, 0.456, 0.406], [0.229, 0.224, 0.225])
    ]),
    'val': transforms.Compose([
        transforms.Resize(256),
        transforms.CenterCrop(224),
        transforms.ToTensor(),
        transforms.Normalize([0.485, 0.456, 0.406], [0.229, 0.224, 0.225])
    ]),
}

# ----------------------------------------------- #
# Load dataset function
# ----------------------------------------------- #
class ImageDatasetWithDOY(Dataset):
    def __init__(self, csv_path, root_dir, transform=None):
        """
        Args:
            csv_path (str): Path to the CSV file with filenames and DOY values.
            root_dir (str): Directory with all the images.
            transform (callable, optional): Optional transform to be applied to images.
        """
        self.annotations = pd.read_csv(csv_path)
        self.root_dir = root_dir
        self.transform = transform
        self.loader = default_loader
        self.class_to_idx = {'low': 0, 'medium': 1, 'high': 2}

    def __len__(self):
        return len(self.annotations)

    def __getitem__(self, idx):
        # Load filename, DOY, and determine label
        relative_path = self.annotations.iloc[idx, 0]
        img_name = os.path.join(self.root_dir, relative_path)
        doy = self.annotations.iloc[idx, 1]
        label_name = os.path.split(os.path.dirname(relative_path))[-1]
        label = self.class_to_idx[label_name]

        # Load and transform image
        try:
            image = self.loader(img_name)
        except FileNotFoundError:
            raise FileNotFoundError(f"File not found: {img_name}")

        if self.transform:
            image = self.transform(image)

        # Return a dictionary instead of a tuple
        return {
            'image': image,
            'doy': torch.tensor([doy], dtype=torch.float32),
            'label': torch.tensor(label),
            'filename': relative_path
        }

# ----------------------------------------------- #
# Model definition with DOY feature
# ----------------------------------------------- #
class ResNetWithDOY(nn.Module):
    def __init__(self, base_model, num_classes):
        super(ResNetWithDOY, self).__init__()
        self.base_model = base_model
        self.model_type = type(base_model).__name__
        
        # Get the number of features from the base model
        if hasattr(base_model, 'fc'):  # ResNet
            num_features = base_model.fc.in_features
            base_model.fc = nn.Identity()
        elif hasattr(base_model, 'classifier'):  # ConvNeXt, EfficientNet, DenseNet
            if isinstance(base_model.classifier, nn.Sequential):  # ConvNeXt
                num_features = base_model.classifier[2].in_features
            elif isinstance(base_model.classifier, nn.Linear):  # EfficientNet
                num_features = base_model.classifier.in_features
            else:  # DenseNet
                num_features = base_model.classifier.in_features
            base_model.classifier = nn.Identity()

        # Rest of the initialization remains the same
        self.doy_embedding = nn.Sequential(
            nn.Linear(1, 64),
            nn.ReLU(),
            nn.Linear(64, 256)
        )
        
        self.classifier = nn.Sequential(
            nn.Linear(num_features + 256, 512),
            nn.ReLU(),
            nn.Dropout(0.5),
            nn.Linear(512, num_classes)
        )

    def forward(self, x, doy):
        # Extract features from the base model
        features = self.base_model(x)
        
        # Handle feature pooling based on model type
        if features.dim() > 2:
            # If features are not already pooled (still have spatial dimensions)
            features = torch.mean(features, dim=[2, 3])
        
        # Process DOY
        doy_features = self.doy_embedding(doy)
        
        # Combine features
        combined = torch.cat((features, doy_features), dim=1)
        
        # Final classification
        return self.classifier(combined)

# ----------------------------------------------- #
# Early stopping implementation
# ----------------------------------------------- #
class EarlyStopping:
    def __init__(self, patience=7, verbose=True, delta=0, filename='checkpoint.pt', drive_path=output_path):
        self.patience = patience
        self.verbose = verbose
        self.delta = delta
        self.path = os.path.join(drive_path,filename)
        self.best_loss = None
        self.early_stop = False
        self.val_loss_min = np.Inf
        self.counter = 0
        self.best_epoch = 0

    def __call__(self, val_loss, model, epoch):
        if self.best_loss is None:
            self.best_loss = val_loss
            self.save_checkpoint(val_loss, model)
            self.best_epoch = epoch
        elif val_loss > self.best_loss - self.delta:
            self.counter += 1
            if self.verbose:
                print(f'EarlyStopping counter: {self.counter} out of {self.patience}')
            if self.counter >= self.patience:
                self.early_stop = True
        else:
            self.best_loss = val_loss
            self.save_checkpoint(val_loss, model)
            self.counter = 0
            self.best_epoch = epoch

    def save_checkpoint(self, val_loss, model):
        if self.verbose:
            print(f'Validation loss decreased ({self.val_loss_min:.6f} --> {val_loss:.6f}). Saving model ...')
        torch.save(model.state_dict(), self.path)
        self.val_loss_min = val_loss

# ----------------------------------------------- #
# Save model predictions
# ----------------------------------------------- #
def save_predictions_to_csv(filenames, logits, probabilities, predictions, labels, dataset_type, save_path):
    """
    Save model predictions and related information to CSV.
    
    Args:
        filenames (list): List of image filenames
        logits (numpy.ndarray): Raw logit values from model
        probabilities (numpy.ndarray): Softmax probabilities
        predictions (numpy.ndarray): Predicted classes
        labels (numpy.ndarray): True labels
        dataset_type (str): 'train' or 'val'
        save_path (str): Path to save the CSV file
    """
    results_df = pd.DataFrame({
        'filename': filenames,
        'predicted_label': predictions,
        'true_label': labels,
        'low_probability': probabilities[:, 0],
        'medium_probability': probabilities[:, 1],
        'high_probability': probabilities[:, 2],
        'low_logit': logits[:, 0],
        'medium_logit': logits[:, 1],
        'high_logit': logits[:, 2],
        'dataset': dataset_type
    })
    
    results_df.to_csv(save_path, index=False)
    return results_df

# ----------------------------------------------- #
# Train model function
# ----------------------------------------------- #
def train_model(model, criterion, optimizer, scheduler, save_dir, num_epochs=25):
    since = time.time()
    
    # Initialize early stopping
    early_stopping = EarlyStopping(patience=5, verbose=True, filename='best_model.pt')
    
    best_model_wts = copy.deepcopy(model.state_dict())
    best_acc = 0.0
    
    # Lists to store metrics
    train_loss_values = []
    train_acc_values = []
    val_loss_values = []
    val_acc_values = []

    for epoch in range(num_epochs):
        print(f'Epoch {epoch}/{num_epochs - 1}')
        print('-' * 10)

        for phase in ['train', 'val']:
            if phase == 'train':
                model.train()
            else:
                model.eval()

            running_loss = 0.0
            running_corrects = 0
            
            # Lists to store batch results
            all_filenames = []
            all_logits = []
            all_probs = []
            all_preds = []
            all_labels = []

            for batch in dataloaders[phase]:
                images = batch['image'].to(device)
                doy = batch['doy'].float().to(device)
                labels = batch['label'].to(device)
                filenames = batch['filename']

                optimizer.zero_grad()

                with torch.set_grad_enabled(phase == 'train'):
                    outputs = model(images, doy)
                    probs = torch.softmax(outputs, dim=1)
                    _, preds = torch.max(outputs, 1)
                    loss = criterion(outputs, labels)

                    if phase == 'train':
                        loss.backward()
                        optimizer.step()

                # Store batch results
                all_filenames.extend(filenames)
                all_logits.append(outputs.detach().cpu())
                all_probs.append(probs.detach().cpu())
                all_preds.append(preds.detach().cpu())
                all_labels.append(labels.detach().cpu())

                running_loss += loss.item() * images.size(0)
                running_corrects += torch.sum(preds == labels.data)

            # Calculate epoch metrics
            epoch_loss = running_loss / len(dataloaders[phase].dataset)
            epoch_acc = running_corrects.double() / len(dataloaders[phase].dataset)

            print(f'{phase} Loss: {epoch_loss:.4f} Acc: {epoch_acc:.4f}')
            
            # Concatenate all batch results
            epoch_logits = torch.cat(all_logits).numpy()
            epoch_probs = torch.cat(all_probs).numpy()
            epoch_preds = torch.cat(all_preds).numpy()
            epoch_labels = torch.cat(all_labels).numpy()
            
            # Save predictions
            predictions_file = os.path.join(save_dir, f'{phase}_predictions_epoch_{epoch}.csv')
            save_predictions_to_csv(
                all_filenames,
                epoch_logits,
                epoch_probs,
                epoch_preds,
                epoch_labels,
                phase,
                predictions_file
            )
            
            # Store metrics
            if phase == 'train':
                train_loss_values.append(epoch_loss)
                train_acc_values.append(epoch_acc.item())
            else:
                val_loss_values.append(epoch_loss)
                val_acc_values.append(epoch_acc.item())
                
                # Early stopping check
                early_stopping(epoch_loss, model, epoch)
                
                # Save best accuracy model and its predictions
                if epoch_acc > best_acc:
                    best_acc = epoch_acc
                    best_model_wts = copy.deepcopy(model.state_dict())
                    
                    # Save best model predictions
                    best_predictions_file = os.path.join(save_dir, f'{phase}_predictions_best.csv')
                    save_predictions_to_csv(
                        all_filenames,
                        epoch_logits,
                        epoch_probs,
                        epoch_preds,
                        epoch_labels,
                        phase,
                        best_predictions_file
                    )

        if early_stopping.early_stop:
            print(f"\nEarly stopping triggered. Best model was found at epoch {early_stopping.best_epoch}")
            break

    time_elapsed = time.time() - since
    print(f'\nTraining complete in {time_elapsed // 60:.0f}m {time_elapsed % 60:.0f}s')
    print(f'Best val Acc: {best_acc:4f}')
    print(f'Best model was found at epoch: {early_stopping.best_epoch}')

    # Save training metrics
    metrics_df = pd.DataFrame({
        'epoch': range(len(train_loss_values)),
        'train_loss': train_loss_values,
        'train_acc': train_acc_values,
        'val_loss': val_loss_values,
        'val_acc': val_acc_values
    })
    metrics_df.to_csv(os.path.join(save_dir, 'training_metrics.csv'), index=False)

    # Load best model weights
    model.load_state_dict(best_model_wts)
    return model