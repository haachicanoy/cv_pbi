# ----------------------------------------------- #
#!/usr/bin/env python
# coding: utf-8
# Crop losses with fine tunning: binary response
# By: Harold Achicanoy
# WUR & ABC
# March 2025
# ----------------------------------------------- #

# Load setup configuration
exec(open("setup.py").read())

# Load the YAML configuration
import yaml
def load_config(yaml_file):
    with open(yaml_file, "r") as file:
        config = yaml.safe_load(file)
    return config

config = load_config("config.yaml")

# Access parameters
cnn_model = config["model"]["name"]
learning_rate = config["model"]["learning_rate"]
batch_size = config["model"]["batch_size"]
epochs = config["model"]["epochs"]
w_low = config["model"]["w_low"]
w_mdm = config["model"]["w_mdm"]
w_hgh = config["model"]["w_hgh"]
dataset_path = config["dataset"]["in_path"]
output_path = config["dataset"]["out_path"]
output_path = output_path+'/results_'+cnn_model

import os
if not os.path.exists(output_path):
    os.makedirs(output_path)

# ----------------------------------------------- #
# Load dataset
# ----------------------------------------------- #
# Paths to CSV files and root directories
train_csv_path = dataset_path+'/train_doy.csv'
train_root_dir = dataset_path+'/train'

val_csv_path = dataset_path+'/val_doy.csv'
val_root_dir = dataset_path+'/val'

# Create datasets
train_dataset = ImageDatasetWithDOY(train_csv_path, train_root_dir, transform=data_transforms['train'])
val_dataset = ImageDatasetWithDOY(val_csv_path, val_root_dir, transform=data_transforms['val'])

# Create DataLoaders
train_loader = DataLoader(train_dataset, batch_size=batch_size, shuffle=True, num_workers=0) # If linux, num_workers=4
val_loader = DataLoader(val_dataset, batch_size=batch_size, shuffle=False, num_workers=0) # If linux, num_workers=4

# Combine into a dictionary for training/validation phases
dataloaders = {'train': train_loader, 'val': val_loader}
dataset_sizes = {'train': len(train_dataset), 'val': len(val_dataset)}

# ----------------------------------------------- #
# Create model
# ----------------------------------------------- #
# Load the pre-trained weights from ImageNet 1K
if cnn_model =='resnet18':
    base_model = models.resnet18(weights='IMAGENET1K_V1')
if cnn_model == 'resnet50':
    base_model = models.resnet50(weights='IMAGENET1K_V2')
if cnn_model =='convnext_tiny':
    base_model = models.convnext_tiny(weights='IMAGENET1K_V1')
if cnn_model =='efficientnet_b3':
    base_model = models.efficientnet_b3(weights='IMAGENET1K_V1')
if cnn_model == 'densenet121':
    base_model = models.densenet121(weights='IMAGENET1K_V1')
model = ResNetWithDOY(base_model, num_classes=3).to(device)

# Criterion and optimizer
weights = torch.tensor((w_low, w_mdm, w_hgh), device=device)
criterion = nn.CrossEntropyLoss(weight=weights)
optimizer = optim.SGD(model.parameters(), lr=learning_rate, momentum=0.9)
exp_lr_scheduler = lr_scheduler.StepLR(optimizer, step_size=7, gamma=0.1)

# ----------------------------------------------- #
# Train model
# ----------------------------------------------- #
if __name__ == '__main__':
    import multiprocessing
    multiprocessing.freeze_support()
    model_ft = train_model(model, criterion, optimizer, exp_lr_scheduler, save_dir=output_path, num_epochs=epochs)