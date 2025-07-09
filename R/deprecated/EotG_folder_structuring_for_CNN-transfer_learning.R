## ------------------------------------------ ##
## Paper 2. Dataset creation loss vs no loss
## CNN + transfer learning randomizing images
## Simple approach
## By: Harold Achicanoy
## WUR & ABC
## Feb 2025
## ------------------------------------------ ##

## R options and packages loading ----
options(warn = -1, scipen = 999)
library(pacman)
pacman::p_load(tidyverse,jsonlite,lubridate,caret,MetBrewer)
grep2 <- Vectorize(FUN = 'grep', vectorize.args = 'pattern')

## Define directories ----
wd <- 'D:/OneDrive - CGIAR/PhD/data/EotG_data_final_no_git' # Working directory
id <- paste0(wd,'/images') # Images directory
ld <- paste0(wd,'/labels') # Labels directory

## Classify images into 'loss' or 'no loss' ----
im_fls <- list.files(path = id, pattern = '.[jJ][pP][gG]', full.names = T) # All image files
lb_fls <- list.files(path = ld, pattern = '.json', full.names = T) # All label files

## Label data (all sites) ----
labels_metadata <- file.path(wd,'labels_metadata.csv')
if (!file.exists(labels_metadata)) {
  lb_info <- lb_fls |>
    purrr::map(.f = function(x){
      dfm <- jsonlite::fromJSON(x) |> base::as.data.frame(); return(dfm)
    }) |>
    dplyr::bind_rows() |>
    dplyr::mutate(date = as.Date(date)) |>
    dplyr::arrange(farmer_unique_id, site_id, date) |>
    base::as.data.frame()
  lb_info$key <- paste0(lb_info$farmer_unique_id,'--',lb_info$site_id)
  utils::write.csv(x = lb_info, file = labels_metadata, row.names = F)
} else {
  lb_info <- utils::read.csv(labels_metadata)
}; rm(labels_metadata, lb_fls)
lb_info$doy <- lubridate::yday(lb_info$date)
lb_info <- lb_info[!is.na(lb_info$extent),]

# # Save DOY data per image
# utils::write.csv(lb_info[,c('filename','doy')], 'D:/doy.csv', row.names = F)

## Generate datasets by threshold ----
thr <- c(0, 10, 20, 30)
seeds <- c(1235, 2346, 3457, 4568)

1:length(thr) |>
  purrr::map(.f = function(i) {
    
    pth <- paste0('D:/eoe_thr_',thr[i]) # Output directory
    
    if (!dir.exists(pth)) {
      
      cat('>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>\n')
      cat(paste0('Processing dataset with losses greater than ',thr[i],'% ...\n'))
      
      # Losses classification
      lb_info$loss <- ifelse(lb_info$extent > thr[i], 'loss', 'no_loss') # Threshold
      rownames(lb_info) <- 1:nrow(lb_info)
      
      # Number of images per crop
      table(lb_info$crop_name)
      round(table(lb_info$crop_name)/sum(table(lb_info$crop_name)) * 100, 1)
      
      # Check dataset balance
      table(lb_info$loss)
      round(table(lb_info$loss)/sum(table(lb_info$loss)),1)
      
      # Check dataset balance per crop
      table(lb_info$crop_name, lb_info$loss)
      
      # Split images into training and validation
      set.seed(seeds[i])
      train_ids <- sample(x = 1:nrow(lb_info), size = nrow(lb_info) * 0.7)
      valid_ids <- base::setdiff(1:nrow(lb_info), train_ids)
      lb_info$sample <- NA
      lb_info$sample[train_ids] <- 'train'
      lb_info$sample[valid_ids] <- 'val'
      
      # Save images into corresponding folders
      1:nrow(lb_info) |>
        purrr::map(.f = function(j) {
          
          outdir <- paste0(pth,'/',lb_info$sample[j],'/',lb_info$loss[j])
          dir.create(outdir, F, T)
          outfle <- paste0(outdir,'/',lb_info$filename[j])
          file.copy(from = paste0(id,'/',lb_info$filename[j]), to = outfle, recursive = T)
          
        })
      
      # Verify and match names and DOY
      doy <- lb_info[,c('filename','doy')]
      
      for (dataset in c('train','val')) {
        fls <- list.files(path = paste0(pth,'/',dataset), pattern = '.[jJ][pP][gG]', full.names = F, recursive = T)
        filenames <- unlist(purrr::map(strsplit(x = fls, split = '/'), 2))
        # classes <- unlist(purrr::map(strsplit(x = fls, split = '/'), 1))
        doys      <- doy$doy[match(filenames, doy$filename)]
        doy_set <- data.frame(filename = fls, doy = doys/366)
        utils::write.csv(doy_set, paste0(pth,'/',dataset,'_doy.csv'), row.names = F)
      }
      
      cat('Done.\n\n')
      
    } else {
      cat(paste0('Dataset with losses greater than ',thr[i],'%, already processed.\n\n'))
    }
    
  })
