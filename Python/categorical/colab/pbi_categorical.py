# ----------------------------------------------- #
#!/usr/bin/env python
# coding: utf-8
# Crop losses with fine tunning: multi-class response with numerical features
# By: Harold Achicanoy
# WUR & ABC
# May 2025
# ----------------------------------------------- #

# Load setup configuration (imports and functions from setup.py)
# Ensure setup.py is in the Python path or same directory
exec(open("setup.py").read())

# Load the YAML configuration
import yaml
def load_config_yaml(yaml_file): # Renamed to avoid conflict if setup.py also has load_config
    with open(yaml_file, "r") as file:
        config_data = yaml.safe_load(file)
    return config_data

config_params = load_config_yaml("config.yaml")

# Access parameters
cnn_model = config_params["model"]["name"]
learning_rate = config_params["model"]["learning_rate"]
batch_size = config_params["model"]["batch_size"]
epochs = config_params["model"]["epochs"]
# Weights for 'basic', 'moderate', 'superior' classes respectively
w_basic = config_params["model"]["w_low"] # Assuming w_low corresponds to 'basic'
w_moderate = config_params["model"]["w_mdm"] # Assuming w_mdm corresponds to 'moderate'
w_superior = config_params["model"]["w_hgh"] # Assuming w_hgh corresponds to 'superior'

dataset_path = config_params["dataset"]["in_path"]
output_base_path = config_params["dataset"]["out_path"] # Use a more general name
output_path = os.path.join(output_base_path, 'results_' + cnn_model) # Correctly join paths

import os
if not os.path.exists(output_path):
    os.makedirs(output_path, exist_ok=True) # Added exist_ok=True

# ----------------------------------------------- #
# Load dataset
# ----------------------------------------------- #
# Paths to CSV files (these should contain relative_path, days_after_sowing, day_of_year)
# IMPORTANT: You need to create these CSV files from your numerical_features.csv
train_csv_path = os.path.join(dataset_path, 'train_features.csv') # Example name
train_root_dir = os.path.join(dataset_path, 'train')

val_csv_path = os.path.join(dataset_path, 'val_features.csv')     # Example name
val_root_dir = os.path.join(dataset_path, 'val')

# Create datasets using the new class
train_dataset = ImageDatasetWithNumericalFeatures(train_csv_path, train_root_dir, transform=data_transforms['train'])
val_dataset = ImageDatasetWithNumericalFeatures(val_csv_path, val_root_dir, transform=data_transforms['val'])

# Create DataLoaders
# Consider num_workers based on your OS and capabilities (e.g., 4 for Linux if resources allow)
train_loader = DataLoader(train_dataset, batch_size=batch_size, shuffle=True, num_workers=0)
val_loader = DataLoader(val_dataset, batch_size=batch_size, shuffle=False, num_workers=0)

# Combine into a dictionary for training/validation phases
dataloaders = {'train': train_loader, 'val': val_loader}
dataset_sizes = {'train': len(train_dataset), 'val': len(val_dataset)}

# ----------------------------------------------- #
# Create model
# ----------------------------------------------- #
# Load the pre-trained weights from ImageNet 1K
if cnn_model =='resnet18':
    base_model = models.resnet18(weights='IMAGENET1K_V1')
elif cnn_model == 'resnet50': # Added elif
    base_model = models.resnet50(weights='IMAGENET1K_V2')
elif cnn_model =='convnext_tiny': # Added elif
    base_model = models.convnext_tiny(weights='IMAGENET1K_V1')
elif cnn_model =='efficientnet_b3': # Added elif
    base_model = models.efficientnet_b3(weights='IMAGENET1K_V1')
elif cnn_model == 'densenet121': # Added elif
    base_model = models.densenet121(weights='IMAGENET1K_V1')
else:
    raise ValueError(f"Unsupported CNN model: {cnn_model}")

# Use the new model class, num_classes is 3 for basic, moderate, superior
model = ModelWithNumericalFeatures(base_model, num_classes=3).to(device)

# Criterion and optimizer
# Weights correspond to: basic, moderate, superior
class_weights = torch.tensor((w_basic, w_moderate, w_superior), device=device, dtype=torch.float32)
criterion = nn.CrossEntropyLoss(weight=class_weights)
optimizer = optim.SGD(model.parameters(), lr=learning_rate, momentum=0.9)
# Learning rate scheduler
exp_lr_scheduler = lr_scheduler.StepLR(optimizer, step_size=7, gamma=0.1)

# ----------------------------------------------- #
# Train model
# ----------------------------------------------- #
if __name__ == '__main__':
    import multiprocessing
    multiprocessing.freeze_support() # For Windows compatibility when using multiprocessing in DataLoader

    # Call train_model from setup.py, passing dataloaders and dataset_sizes
    model_ft = train_model(model, criterion, optimizer, exp_lr_scheduler, 
                           dataloaders=dataloaders, dataset_sizes=dataset_sizes,
                           save_dir=output_path, num_epochs=epochs)
    
    # Save the final trained model (best model based on validation accuracy is already saved by train_model)
    # This saves the model state at the end of all epochs, or after early stopping.
    final_model_path = os.path.join(output_path, 'final_model_state.pt')
    torch.save(model_ft.state_dict(), final_model_path)
    print(f"Final model state saved to {final_model_path}")
    print(f"Best accuracy model saved as 'best_accuracy_model.pt' in {output_path}")
    print(f"Best validation loss model checkpoint (if early stopping occurred) saved as 'best_model_checkpoint.pt' in {output_path}")