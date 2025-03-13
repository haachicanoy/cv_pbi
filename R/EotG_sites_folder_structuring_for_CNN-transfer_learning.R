## ------------------------------------------ ##
## Paper 2. Dataset creation loss vs no loss
## CNN + transfer learning randomizing sites
## Simple approach
## By: Harold Achicanoy
## WUR & ABC
## Feb. 2025
## ------------------------------------------ ##

## R options and packages loading ----
options(warn = -1, scipen = 999)
library(pacman)
pacman::p_load(tidyverse,parallelDist,cluster,psych,trend,jsonlite,lubridate,
               caret,MetBrewer,FactoMineR,factoextra)
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

# Crop distribution for all pictures
round(sort(table(lb_info$crop_name), decreasing = T)/nrow(lb_info) * 100, digits = 1)

# Only pictures with experts evaluation
lb_info <- lb_info[!is.na(lb_info$extent),]

# Filter by maize
lb_info <- lb_info[lb_info$crop_name == 'maize',]; rownames(lb_info) <- 1:nrow(lb_info)

# sites <- unique(lb_info$key)
lb_info_lst <- lb_info |>
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

# Identify sites patterns
sites_distance <- parallelDist::parDist(x = lb_info_sts_complete[,-1] |> base::as.matrix(), method = 'euclidean'); gc(F,T,T)
sites_hcl <- fastcluster::hclust(sites_distance, method = 'ward.D2') # Hierarchical clustering for distance matrix
plot(sites_hcl, hang = -1)

sil_width <- 2:20 |>
  purrr::map(.f = function(k){
  model <- cluster::pam(x = sites_distance, k = k)
  results <- data.frame(k = k, avg_silhouette = model$silinfo$avg.width)
  return(results)
}) |> dplyr::bind_rows(); gc(F, T, T)

# Find the optimal k based on avg_silhouette (visually)
sil_width |>
  ggplot2::ggplot(aes(x = k, y = avg_silhouette)) +
  ggplot2::geom_line() +
  ggplot2::geom_point() +
  ggplot2::scale_x_continuous(breaks = 2:20) +
  ggplot2::xlab('Number of clusters') +
  ggplot2::ylab('Silhouette statistics') +
  ggplot2::theme_bw()
optimal_k <- 5 # based on visual inspection of silhouette results

# Create a cluster column
lb_info_sts_complete$cluster <- cutree(tree = sites_hcl, k = optimal_k) |> as.character()
# Clusters distribution
round(table(lb_info_sts_complete$cluster)/nrow(lb_info_sts_complete) * 100, digits = 1)

## Generate datasets by threshold ----
thr <- c(0, 10, 20, 30)
seeds <- c(1235, 2346, 3457, 4568)

1:length(thr) |>
  purrr::map(.f = function(i) {
    
    pth1 <- paste0('D:/pbi_smpl_thr_',thr[i]) # Output directory
    pth2 <- paste0('D:/pbi_strt_thr_',thr[i]) # Output directory
    
    if (!dir.exists(pth1) | !dir.exists(pth2)) {
      
      cat('>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>\n')
      cat(paste0('Processing dataset with losses greater than ',thr[i],'% ...\n'))
      
      cat('Performing random sampling ...\n')
      set.seed(seeds[i])
      random_sampling <- lb_info_sts_complete
      val_smp <- sample(x = 1:nrow(random_sampling), size = nrow(random_sampling) * 0.3, replace = F)
      random_sampling$dataset <- NA
      random_sampling$dataset[val_smp] <- 'val'
      random_sampling$dataset[base::setdiff(1:nrow(random_sampling),val_smp)] <- 'train'; rm(val_smp)
      
      lb_info_final <- lb_info[lb_info$key %in% lb_info_sts_complete$key,]
      lb_info_final <- dplyr::left_join(x = lb_info_final, y = random_sampling[,c('key','dataset')], by = 'key')
      names(lb_info_final)[ncol(lb_info_final)] <- 'random'; rm(random_sampling)
      
      cat('Performing stratified sampling ...\n')
      val_size <- round(nrow(lb_info_sts_complete) * 0.3, 0)
      size_by_cluster <- round(val_size * table(lb_info_sts_complete$cluster)/nrow(lb_info_sts_complete), 0)
      
      stratified_sampling <- 1:5 |>
        purrr::map(.f = function(j) {
          dfm <- lb_info_sts_complete |>
            dplyr::filter(cluster == j)
          set.seed(seeds[i])
          val_smp <- sample(x = 1:nrow(dfm), size = size_by_cluster[j], replace = F)
          val_dfm <- dfm[val_smp,]
          val_dfm$dataset <- 'val'
          trn_dfm <- dfm[base::setdiff(1:nrow(dfm),val_smp),]
          trn_dfm$dataset <- 'train'
          res <- dplyr::bind_rows(trn_dfm, val_dfm)
          return(res)
        }) |> dplyr::bind_rows()
      
      lb_info_final <- dplyr::left_join(x = lb_info_final, y = stratified_sampling[,c('key','cluster','dataset')], by = 'key')
      names(lb_info_final)[ncol(lb_info_final)] <- 'stratified'; rm(stratified_sampling)
      
      # Losses classification
      lb_info_final$loss <- ifelse(lb_info_final$extent > thr[i], 'loss', 'no_loss') # Threshold
      
      # Check dataset class imbalance
      table(lb_info_final$loss)
      round(table(lb_info_final$loss)/sum(table(lb_info_final$loss)),2)
      
      # Save images into corresponding folders
      1:nrow(lb_info_final) |>
        purrr::map(.f = function(k) {
          
          outdir1 <- paste0(pth1,'/',lb_info_final$random[k],'/',lb_info_final$loss[k])
          dir.create(outdir1, F, T)
          outfle1 <- paste0(outdir1,'/',lb_info_final$filename[k])
          file.copy(from = paste0(id,'/',lb_info_final$filename[k]), to = outfle1, recursive = T)
          
          outdir2 <- paste0(pth2,'/',lb_info_final$stratified[k],'/',lb_info_final$loss[k])
          dir.create(outdir2, F, T)
          outfle2 <- paste0(outdir2,'/',lb_info_final$filename[k])
          file.copy(from = paste0(id,'/',lb_info_final$filename[k]), to = outfle2, recursive = T)
          
        })
      
      # Verify and match names and DOY
      doy <- lb_info_final[,c('filename','doy')]
      
      for (dataset in c('train','val')) {
        
        fls1 <- list.files(path = paste0(pth1,'/',dataset), pattern = '.[jJ][pP][gG]', full.names = F, recursive = T)
        filenames1 <- unlist(purrr::map(strsplit(x = fls1, split = '/'), 2))
        # classes <- unlist(purrr::map(strsplit(x = fls, split = '/'), 1))
        doys1      <- doy$doy[match(filenames1, doy$filename)]
        doy_set1 <- data.frame(filename = fls1, doy = doys1/366)
        utils::write.csv(doy_set1, paste0(pth1,'/',dataset,'_doy.csv'), row.names = F)
        
        fls2 <- list.files(path = paste0(pth2,'/',dataset), pattern = '.[jJ][pP][gG]', full.names = F, recursive = T)
        filenames2 <- unlist(purrr::map(strsplit(x = fls2, split = '/'), 2))
        # classes <- unlist(purrr::map(strsplit(x = fls, split = '/'), 1))
        doys2      <- doy$doy[match(filenames2, doy$filename)]
        doy_set2 <- data.frame(filename = fls2, doy = doys2/366)
        utils::write.csv(doy_set2, paste0(pth2,'/',dataset,'_doy.csv'), row.names = F)
        
      }
      
      cat('Done.\n\n')
      
    } else {
      cat(paste0('Dataset with losses greater than ',thr[i],'%, already processed.\n\n'))
    }
    
  })

utils::write.csv(x = lb_info_sts, file = file.path(wd,'labels_metadata_stats_by_site_all.csv'), row.names = F)
utils::write.csv(x = lb_info_sts_complete, file = file.path(wd,'labels_metadata_stats_by_site.csv'), row.names = F)

sts_pca <- lb_info_sts_complete |>
  dplyr::select(-key, -cluster, -days_diff) |>
  FactoMineR::PCA(scale.unit = T, graph = F)

cPal <- MetBrewer::met.brewer(name = 'Juarez', n = 5, type = 'discrete')

factoextra::fviz_pca_ind(sts_pca, label = 'none', habillage = factor(lb_info_sts_complete$cluster), addEllipses = T, ellipse.level = 0.9) +
  ggplot2::scale_color_manual(values = cPal)

corrplot::corrplot(sts_pca$var$cos2, method = 'square', is.corr = F)

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
