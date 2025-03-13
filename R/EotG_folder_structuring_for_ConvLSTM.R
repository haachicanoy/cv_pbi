## ------------------------------------------ ##
## Paper 2. Dataset creation loss vs no loss
## ConvLSTM: training from scratch
## Suited approach
## By: Harold Achicanoy
## WUR & ABC
## Jan 2025
## ------------------------------------------ ##

## R options and packages loading ----
options(warn = -1, scipen = 999)
library(pacman)
pacman::p_load(dplyr,purrr,jsonlite)
grep2 <- Vectorize(FUN = 'grep', vectorize.args = 'pattern')

## Define directories ----
wd <- 'D:/OneDrive - CGIAR/PhD/data/EotG_data_final_no_git' # Working directory
id <- paste0(wd,'/images') # Images directory
ld <- paste0(wd,'/labels') # Labels directory

## Classify images into 'loss' or 'no loss' ----
im_fls <- list.files(path = id, pattern = '.[jJ][pP][gG]', full.names = T) # All image files
lb_fls <- list.files(path = ld, pattern = '.json', full.names = T) # All label files

# Images metadata
lb_info <- lb_fls |>
  purrr::map(.f = function(x){
    dfm <- jsonlite::fromJSON(x) |> base::as.data.frame(); return(dfm)
  }) |>
  dplyr::bind_rows() |>
  dplyr::mutate(date = as.Date(date)) |>
  dplyr::arrange(farmer_unique_id, site_id, date) |>
  base::as.data.frame()
lb_info$key <- paste0(lb_info$farmer_unique_id,'--',lb_info$site_id)
lb_info$loss <- ifelse(lb_info$extent > 0, 'loss', 'no_loss')

lb_info <- lb_info[!is.na(lb_info$extent),]

# Number of images per crop
table(lb_info$crop_name)
round(table(lb_info$crop_name)/sum(table(lb_info$crop_name)) * 100, 1)

# Check dataset balance
table(lb_info$loss)
round(table(lb_info$loss)/sum(table(lb_info$loss)),1)

# Check dataset balance per crop
table(lb_info$crop_name, lb_info$loss)

# Separate images per farm
plots <- names(which(table(lb_info$key) > 1)) # unique(lb_info$key)

losses <- 1:length(plots) |>
  purrr::map(.f = function(i){
    
    # Create plot directory
    outdir <- paste0('D:/farm_images/',plots[i])
    dir.create(outdir, F, T)
    
    # Filter images per plot
    lb_info_flt <- lb_info[lb_info$key == plots[i],]
    for (j in 1:nrow(lb_info_flt)) {
      original_img <- paste0(id,'/',lb_info_flt$filename[j])
      output_img   <- paste0(outdir,'/img',j,'.',strsplit(x = lb_info_flt$filename[j], split = '.', fixed = T)[[1]][2])
      file.copy(from = original_img, to = output_img, recursive = T)
    }
    
    response <- data.frame(farm_id = plots[i], label = ifelse(lb_info_flt$loss[nrow(lb_info_flt)] == 'loss', 1, 0))
    return(response)
  }) |>
  dplyr::bind_rows()

utils::write.csv(losses, 'D:/labels.csv', row.names = F)
