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

# Sowing information
sw_fls <- list.files(path = file.path(wd,'EotG_images'), pattern = '.json$', full.names = T, recursive = T)
sw_fls <- sw_fls[-grep(pattern = 'collection.json', x = sw_fls)]
sw_fls <- sw_fls[-grep(pattern = 'catalog.json', x = sw_fls)]
sw_info <- sw_fls |>
  purrr::map(.f = function(x){
    vct <- jsonlite::fromJSON(x)
    vct <- vct[c('id','properties')]
    dfm <- vct |> base::as.data.frame(); return(dfm)
  }) |>
  dplyr::bind_rows()
sw_info <- sw_info[,1:10]
names(sw_info) <- gsub(pattern = 'properties.', replacement = '', x = names(sw_info))
sw_info$spatial_location <- sw_info$spatial_unit <- NULL
sw_info$datetime <- as.Date(sw_info$datetime)
sw_info$sowing_date <- as.Date(sw_info$sowing_date)
sw_info$days_from_sowing <- as.numeric(sw_info$datetime - sw_info$sowing_date)
sw_info$id <- gsub(pattern = 'img_', replacement = '', x = sw_info$id)

# Images metadata
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
lb_info$date <- as.Date(lb_info$date)
lb_info$doy <- lubridate::yday(lb_info$date)

lb_info$id <- tools::file_path_sans_ext(lb_info$filename)
lb_info <- dplyr::left_join(x = lb_info, y = sw_info, by = c('id','farmer_unique_id','site_id','crop_name','season'))
lb_info$datetime <- NULL

lb_info_maize <- lb_info[lb_info$crop_name == 'maize',]
lb_info_maize <- lb_info_maize[lb_info_maize$days_from_sowing > 0,]
lb_info_maize <- lb_info_maize[lb_info_maize$days_from_sowing < 347,]
lb_info_maize <- lb_info_maize[lb_info_maize$days_from_sowing >= 30,]
lb_info_maize <- lb_info_maize[!is.na(lb_info_maize$extent),]

# Split images by loss class
for (i in 1:nrow(lb_info_maize)) {
  outfile <- paste0('D:/kenya_images/loss_',lb_info_maize$extent[i],'/dfs_',lb_info_maize$days_from_sowing[i],'--',lb_info_maize$filename[i])
  dir.create(path = dirname(outfile), F, T)
  file.copy(from = file.path(id,lb_info_maize$filename[i]),
            to = outfile)
}


# lb_info <- lb_info[!is.na(lb_info$extent),]

# # Number of images per crop
# table(lb_info$crop_name)
# round(table(lb_info$crop_name)/sum(table(lb_info$crop_name)) * 100, 1)
# 
# # Check dataset balance
# table(lb_info$loss)
# round(table(lb_info$loss)/sum(table(lb_info$loss)),1)
# 
# # Check dataset balance per crop
# table(lb_info$crop_name, lb_info$loss)

# Separate images per farm
one_img <- names(which(table(lb_info$key) == 1))
one_img <- one_img[!is.na(lb_info$extent[lb_info$key %in% one_img])]
plots <- names(which(table(lb_info$key) > 1)) # unique(lb_info$key)
plots <- c(one_img, plots); rm(one_img)

na.omit(plot_info$date - dplyr::lag(plot_info$date,1)) |> as.numeric()

1:length(plots) |>
  purrr::map(.f = function(i){
    
    # Create plot directory
    outdir <- paste0('D:/farm_images/',plots[i])
    dir.create(outdir, F, T)
    
    # Filter images per plot
    lb_info_flt <- lb_info[lb_info$key == plots[i],]
    lb_info_flt <- lb_info_flt |> dplyr::arrange(date)
    na.omit(lb_info_flt$date - dplyr::lag(lb_info_flt$date, 1)) |> as.numeric()
    for (j in 1:nrow(lb_info_flt)) {
      original_img <- paste0(id,'/',lb_info_flt$filename[j])
      outformat    <- strsplit(x = lb_info_flt$filename[j], split = '.', fixed = T)[[1]][2]
      output_img   <- paste0(outdir,'/img',j,'.',outformat)
      lb_info_flt$output_filename[j] <- basename(output_img)
      file.copy(from = original_img, to = output_img, recursive = T)
    }
    utils::write.csv(x = lb_info_flt, file.path(outdir,'metadata.csv'), row.names = F)
    return(cat(plots[i],'ready.\n'))
  })

utils::write.csv(losses, 'D:/labels.csv', row.names = F)
