## -------------------------------------------------------- ##
## Paper 2. Dataset folder structuring for three categories
## CNN + transfer learning randomizing sites
## Simple approach
## By: Harold Achicanoy
## WUR & ABC
## June 2025
## -------------------------------------------------------- ##

## R options and packages loading ----
options(warn = -1, scipen = 999)
library(pacman)
pacman::p_load(tidyverse,parallelDist,cluster,psych,trend,jsonlite,lubridate,
               caret,MetBrewer,FactoMineR,factoextra)
grep2 <- Vectorize(FUN = 'grep', vectorize.args = 'pattern')
normalize_indicator <- function(r) {
  mn <- min(r)
  mx <- max(r)
  r_nrm <- (r - mn)/(mx - mn)
  return(r_nrm)
} # DAS: 1 to 206

## Define directories ----
wd <- 'D:/OneDrive - CGIAR/PhD/data/EotG_data_final_no_git' # Working directory
id <- file.path(wd,'images') # Images directory
ld <- file.path(wd,'labels') # Labels directory

## Classify images into 'loss' or 'no loss' ----
im_fls <- list.files(path = id, pattern = '.[jJ][pP][gG]', full.names = T) # All image files
lb_fls <- list.files(path = ld, pattern = '.json', full.names = T) # All label files

## Sowing information (all sites) ----
sowing_metadata <- file.path(wd,'sowing_metadata.csv')
if (!file.exists(sowing_metadata)) {
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
  utils::write.csv(x = sw_info, file = sowing_metadata, row.names = F)
} else {
  sw_info <- utils::read.csv(sowing_metadata)
}; rm(sowing_metadata)

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

# Merging sowing and images metadata
lb_info$id <- tools::file_path_sans_ext(lb_info$filename)
lb_info <- dplyr::left_join(x = lb_info, y = sw_info, by = c('id','farmer_unique_id','site_id','crop_name','season')); rm(sw_info)
lb_info$datetime <- NULL

# Only pictures with experts evaluation
lb_info <- lb_info[!is.na(lb_info$extent),]

# Crop distribution for all pictures
round(sort(table(lb_info$crop_name), decreasing = T)/nrow(lb_info) * 100, digits = 1)

# Number of farm-managed sites
length(unique(lb_info$key))

# Filter by maize
lb_info_maize <- lb_info[lb_info$crop_name == 'maize',]
lb_info_maize |>
  ggplot2::ggplot(aes(x = days_from_sowing)) +
  ggplot2::geom_histogram(color = 'darkblue', fill = 'lightblue') +
  ggplot2::geom_vline(xintercept = c(0, 210), color = 'red', size = 0.8, linetype = 'dashed') +
  ggplot2::theme_bw() +
  ggplot2::xlab('Number of Days After Sowing') +
  ggplot2::ylab('Number of images')
lb_info_maize <- lb_info_maize[lb_info_maize$days_from_sowing > 0,]
lb_info_maize <- lb_info_maize[lb_info_maize$days_from_sowing < 210,]
lb_info_maize$year <- lubridate::year(lb_info_maize$date)

lm_fit <- lm(extent ~ days_from_sowing + doy + year, data = lb_info_maize)
summary(lm_fit)

# negative <- lb_info[lb_info$days_from_sowing <= 0,]
# for (i in 1:nrow(negative)) {
#   file.copy(from = file.path(id,negative$filename[i]), to = paste0('D:/negative_DAS/dfs_',negative$days_from_sowing[i],'--loss_',negative$extent[i],'--',negative$filename[i]))
# }

# Filter by maize
lb_info_maize$damage_extent <- cut(x = lb_info_maize$extent, breaks = c(0,30,60,100), labels = c('basic','moderate','superior'), include.lowest = T, right = F)

# sites <- unique(lb_info$key)
lb_info_lst <- lb_info_maize |>
  dplyr::group_by(key) |>
  dplyr::group_split()

lb_info_sts <- 1:length(lb_info_lst) |>
  purrr::map(.f = function(i){
    
    plot_info <- lb_info_lst[[i]] |> base::as.data.frame()
    if(nrow(plot_info) > 1){
      plot_info$date <- as.Date(plot_info$date)
      plot_summ <- plot_info |>
        dplyr::select(key, farmer_unique_id, site_id, season, crop_name, spatial_location) |>
        base::unique()
      plot_summ$pics_total <- nrow(plot_info)
      plot_summ$pics_eval <- sum(!is.na(plot_info$extent))
      plot_summ$pics_not_eval <- plot_summ$pics_total - plot_summ$pics_eval
      plot_summ$pics_zero <- sum(plot_info$extent == 0, na.rm = T)
      plot_summ$pics_non_zero <- sum(plot_info$extent > 0, na.rm = T)
      plot_summ$days_diff <- difftime(plot_info$date[nrow(plot_info)],
                                      plot_info$date[1],
                                      units = 'days') |> as.numeric()
      days_between_pics <- na.omit(plot_info$date - dplyr::lag(plot_info$date,1)) |> as.numeric()
      plot_summ$days_btw_pics_avg <- mean(days_between_pics)
      plot_summ$days_btw_pics_mdn <- median(days_between_pics)
      plot_summ$days_btw_pics_std <- sd(days_between_pics)
      plot_summ$days_btw_pics_mad <- mad(days_between_pics)
      plot_summ$days_btw_pics_min <- min(days_between_pics)
      plot_summ$days_btw_pics_max <- max(days_between_pics); rm(days_between_pics)
      plot_summ$loss_avg <- mean(plot_info$extent, na.rm = T)
      plot_summ$loss_mdn <- median(plot_info$extent, na.rm = T)
      plot_summ$loss_std <- sd(plot_info$extent, na.rm = T)
      plot_summ$loss_mad <- mad(plot_info$extent, na.rm = T)
      plot_summ$loss_min <- min(plot_info$extent, na.rm = T)
      plot_summ$loss_max <- max(plot_info$extent, na.rm = T)
      if( length(na.omit(plot_info$extent)) > 1){
        plot_summ$loss_trd <- trend::sens.slope(na.omit(plot_info$extent))$estimates |> as.numeric()
      } else {
        plot_summ$loss_trd <- NA
      }
    } else {
      plot_info$date <- as.Date(plot_info$date)
      plot_summ <- plot_info |>
        dplyr::select(key, farmer_unique_id, site_id, season, crop_name, spatial_location) |>
        base::unique()
      plot_summ$pics_total <- nrow(plot_info)
      plot_summ$pics_eval <- sum(!is.na(plot_info$extent))
      plot_summ$pics_not_eval <- plot_summ$pics_total - plot_summ$pics_eval
      plot_summ$days_diff <- NA
      plot_summ$days_btw_pics_avg <- NA
      plot_summ$days_btw_pics_mdn <- NA
      plot_summ$days_btw_pics_std <- NA
      plot_summ$days_btw_pics_mad <- NA
      plot_summ$days_btw_pics_min <- NA
      plot_summ$days_btw_pics_max <- NA
      plot_summ$loss_avg <- mean(plot_info$extent, na.rm = T)
      plot_summ$loss_mdn <- median(plot_info$extent, na.rm = T)
      plot_summ$loss_std <- NA
      plot_summ$loss_mad <- NA
      plot_summ$loss_min <- min(plot_info$extent, na.rm = T)
      plot_summ$loss_max <- max(plot_info$extent, na.rm = T)
      plot_summ$loss_trd <- NA
    }
    
    return(plot_summ)
    
  }) |>
  dplyr::bind_rows(); rm(lb_info_lst)
lb_info_sts$pics_eval <- NULL
lb_info_sts$pics_not_eval <- NULL
lb_info_sts$days_btw_pics_std[which(is.na(lb_info_sts$days_btw_pics_std) & lb_info_sts$days_btw_pics_mad == 0)] <- 0

lb_info_sts_complete <- lb_info_sts |>
  dplyr::select(key, pics_total:loss_trd) |>
  tidyr::drop_na()

# # Identify sites patterns
# sites_distance <- parallelDist::parDist(x = lb_info_sts_complete[,-1] |> base::as.matrix(), method = 'euclidean'); gc(F,T,T)
# sites_hcl <- fastcluster::hclust(sites_distance, method = 'ward.D2') # Hierarchical clustering for distance matrix
# plot(sites_hcl, hang = -1)
# 
# sil_width <- 2:20 |>
#   purrr::map(.f = function(k){
#     model <- cluster::pam(x = sites_distance, k = k)
#     results <- data.frame(k = k, avg_silhouette = model$silinfo$avg.width)
#     return(results)
#   }) |> dplyr::bind_rows(); gc(F, T, T)
# 
# # Find the optimal k based on avg_silhouette (visually)
# sil_width |>
#   ggplot2::ggplot(aes(x = k, y = avg_silhouette)) +
#   ggplot2::geom_line() +
#   ggplot2::geom_point() +
#   ggplot2::scale_x_continuous(breaks = 2:20) +
#   ggplot2::xlab('Number of clusters') +
#   ggplot2::ylab('Silhouette statistics') +
#   ggplot2::theme_bw()
# optimal_k <- 5 # based on visual inspection of silhouette results
# 
# # Create a cluster column
# lb_info_sts_complete$cluster <- cutree(tree = sites_hcl, k = optimal_k) |> as.character()
# # Clusters distribution
# round(table(lb_info_sts_complete$cluster)/nrow(lb_info_sts_complete) * 100, digits = 1)

## Generate datasets ----
seed <- 1235

pth <- paste0('D:/pbi_kenya') # Output directory

cat('Performing random sampling ...\n')
set.seed(seed)
random_sampling <- lb_info_sts_complete
val_smp <- sample(x = 1:nrow(random_sampling), size = nrow(random_sampling) * 0.3, replace = F)
random_sampling$dataset <- NA
random_sampling$dataset[val_smp] <- 'val'
random_sampling$dataset[base::setdiff(1:nrow(random_sampling),val_smp)] <- 'train'; rm(val_smp)

lb_info_final <- lb_info_maize[lb_info_maize$key %in% lb_info_sts_complete$key,]
lb_info_final <- dplyr::left_join(x = lb_info_final, y = random_sampling[,c('key','dataset')], by = 'key')
names(lb_info_final)[ncol(lb_info_final)] <- 'random'; rm(random_sampling)

# Check dataset class imbalance
table(lb_info_final$damage_extent)
# Weights
round(table(lb_info_final$damage_extent)/sum(table(lb_info_final$damage_extent)),2)
# low: 0.07, medium: 0.18, high: 0.75

# Save images into corresponding folders
1:nrow(lb_info_final) |>
  purrr::map(.f = function(k) {
    
    outdir <- paste0(pth,'/',lb_info_final$random[k],'/',lb_info_final$damage_extent[k])
    dir.create(outdir, F, T)
    outfle <- paste0(outdir,'/',lb_info_final$filename[k])
    file.copy(from = paste0(id,'/',lb_info_final$filename[k]), to = outfle, recursive = T)
    
  })

final_imgs <- list.files(path = pth, full.names = T, recursive = T)
numerical_features <- data.frame(
  filename = basename(final_imgs),
  full_image_path = gsub('..','/content',R.utils::getRelativePath(final_imgs, relativeTo = 'D:/'), fixed = T)
)
rownames(numerical_features) <- 1:nrow(numerical_features)
numerical_features <- dplyr::left_join(x = numerical_features, y = lb_info_maize[,c('filename','days_from_sowing','doy')], by = 'filename')
numerical_features$filename <- NULL
names(numerical_features)[2:3] <- c('days_after_sowing','day_of_year')
numerical_features$days_after_sowing <- normalize_indicator(numerical_features$days_after_sowing)
numerical_features$day_of_year <- normalize_indicator(numerical_features$day_of_year)

utils::write.csv(x = numerical_features, file = file.path(pth,'numerical_features.csv'), row.names = F)

numerical_features <- utils::read.csv(file.path(pth,'numerical_features.csv'))

train_features <- numerical_features[grep('train', numerical_features$full_image_path),]
val_features <- numerical_features[grep('val', numerical_features$full_image_path),]

train_features$full_image_path <- R.utils::getRelativePath(train_features$full_image_path, relativeTo = '/content/pbi_kenya/train/')
val_features$full_image_path <- R.utils::getRelativePath(val_features$full_image_path, relativeTo = '/content/pbi_kenya/val/')

names(train_features)[1] <- 'relative_path'
names(val_features)[1] <- 'relative_path'

utils::write.csv(x = train_features, file = file.path(pth,'train_features.csv'), row.names = F)
utils::write.csv(x = val_features, file = file.path(pth,'val_features.csv'), row.names = F)



# Verify and match names and DOY
doy <- lb_info_final[,c('filename','doy')]

for (dataset in c('train','val')) {
  
  fls <- list.files(path = paste0(pth,'/',dataset), pattern = '.[jJ][pP][gG]', full.names = F, recursive = T)
  filenames <- unlist(purrr::map(strsplit(x = fls, split = '/'), 2))
  doys      <- doy$doy[match(filenames, doy$filename)]
  doy_set <- data.frame(filename = fls, doy = doys/366)
  utils::write.csv(doy_set, paste0(pth,'/',dataset,'_doy.csv'), row.names = F)
  
}

# utils::write.csv(x = lb_info_sts, file = file.path(wd,'labels_metadata_stats_by_site_all.csv'), row.names = F)
# utils::write.csv(x = lb_info_sts_complete, file = file.path(wd,'labels_metadata_stats_by_site.csv'), row.names = F)
# 
# sts_pca <- lb_info_sts_complete |>
#   dplyr::select(-key, -cluster, -days_diff) |>
#   FactoMineR::PCA(scale.unit = T, graph = F)
# 
# cPal <- MetBrewer::met.brewer(name = 'Juarez', n = 5, type = 'discrete')
# 
# factoextra::fviz_pca_ind(sts_pca, label = 'none', habillage = factor(lb_info_sts_complete$cluster), addEllipses = T, ellipse.level = 0.9) +
#   ggplot2::scale_color_manual(values = cPal)
# 
# corrplot::corrplot(sts_pca$var$cos2, method = 'square', is.corr = F)

# 
# tst <- lb_info_sts_complete |>
#   psych::describeBy(group = lb_info_sts_complete$cluster)
# tst <- do.call(rbind.data.frame, tst)
# tst$cluster <- rownames(tst)
# rownames(tst) <- gsub('[0-9][0-9].','',rownames(tst))
# tst$variable <- gsub('[0-9].','',rownames(tst))
# rownames(tst) <- 1:nrow(tst)
# tst$cluster <- strsplit(tst$cluster, '.', fixed = T) |> purrr::map(1) |> unlist() |> as.factor()
# 
# pca <- lb_info_sts_complete |>
#   dplyr::select(-key, -cluster, -days_diff) |>
#   FactoMineR::PCA(scale.unit = T, graph = F)
# 
# cPal <- MetBrewer::met.brewer(name = 'Juarez', n = 10, type = 'continuous')
# factoextra::fviz_pca_var(pca, col.var = 'cos2', gradient.cols = c('#00AFBB', '#E7B800', '#FC4E07'), repel = T)
# factoextra::fviz_pca_ind(pca, label = 'none', habillage = factor(lb_info_sts_complete$cluster),
#                          addEllipses = T, ellipse.level = 0.9) +
#   ggplot2::scale_color_manual(values = cPal)
# 
# pca$var$cos2
# 
# data.frame(key = unique(tst$key),
#            farmer_unique_id, site_id)
# unique(tst$season)
# psych::describe(tst$extent)
# 
# sites[3]

# # Save DOY data per image
# utils::write.csv(lb_info[,c('filename','doy')], 'D:/doy.csv', row.names = F)
