## ------------------------------------------ ##
## Paper 2. Dataset exploration
## Stats and graphs
## By: Harold Achicanoy
## WUR & ABC
## Jan 2025
## ------------------------------------------ ##

## R options and packages loading ----
options(warn = -1, scipen = 999)
library(pacman)
pacman::p_load(jsonlite,tidyverse,magick,terra,geodata,leaflet,htmltools,ggplot2,quantreg)
grep2 <- Vectorize(FUN = 'grep', vectorize.args = 'pattern')

## Define directories ----
wd <- 'D:/OneDrive - CGIAR/PhD/data/EotG_data_final_no_git' # Working directory
id <- paste0(wd,'/images') # Images directory
ld <- paste0(wd,'/labels') # Labels directory

## List files ----
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
lb_info$date <- as.Date(lb_info$date)

## Location (approx for all sites) ----
gn <- paste0(wd,'/EotG_images') # General data directory
gn_fls <- list.files(path = gn, pattern = '.json', full.names = T, recursive = T) # All general info files
gn_fls <- gn_fls[-grep(pattern = 'collection.json', x = gn_fls)]
gn_fls <- gn_fls[-grep(pattern = 'catalog.json', x = gn_fls)]

sites_metadata <- file.path(wd,'sites_metadata.csv')
if (!file.exists(sites_metadata)) {
  sites <- gn_fls |>
    purrr::map(.f = function(x){
      vct      <- terra::vect(terra::ext(jsonlite::fromJSON(x)$bbox[c(1,3,2,4)]))
      terra::crs(vct) <- '+proj=longlat +datum=WGS84 +no_defs +type=crs'
      crd      <- terra::centroids(vct)
      crd      <- terra::crds(crd) |> base::as.data.frame()
      colnames(crd) <- c('lon','lat')
      f_id     <- jsonlite::fromJSON(x)$properties$farmer_unique_id
      id       <- jsonlite::fromJSON(x)$properties$site_id
      location <- jsonlite::fromJSON(x)$properties$spatial_location
      site     <- cbind(data.frame(farmer_unique_id = f_id, site_id = id, spatial_location = location), crd)
      return(site)
    }) |>
    dplyr::bind_rows() |>
    base::as.data.frame() |>
    base::unique()
  rownames(sites) <- 1:nrow(sites)
  utils::write.csv(x = sites, file = sites_metadata, row.names = F)
} else {
  sites <- utils::read.csv(sites_metadata)
}; rm(sites_metadata, gn_fls)

# sites[,c('lon', 'lat', 'spatial_location')] |>
#   unique() |>
#   leaflet::leaflet() |>
#   leaflet::addTiles() |>
#   leaflet::addMarkers(~lon, ~lat, popup = ~htmlEscape(spatial_location))

# Spatial coverage full dataset
ken <- geodata::gadm(country = 'KEN', level = 3, path = tempdir(), version = 'latest')
pts_intersect <- terra::intersect(x = ken,
                                  y = terra::vect(unique(sites[,c('lon', 'lat', 'spatial_location')]), c('lon','lat'), crs = 'EPSG:4326')) |>
  base::as.data.frame()
length(unique(pts_intersect$NAME_1)) # number of counties
rm(pts_intersect)

# Temporal coverage full dataset
range(lb_info$date)

gg_text <- ggplot2::theme(text            = element_text(size = 13, colour = 'black'),
                          axis.text.x     = element_text(size = 12, colour = 'black'),
                          axis.text.y     = element_text(size = 12, colour = 'black'),
                          axis.title      = element_text(size = 15, colour = 'black'),
                          legend.text     = element_text(size = 12, colour = 'black'),
                          legend.title    = element_blank(),
                          plot.title      = element_text(size = 15, colour = 'black'),
                          plot.subtitle   = element_text(size = 14, colour = 'black'),
                          strip.text.x    = element_text(size = 12, colour = 'black'),
                          strip.text.y    = element_text(size = 12, colour = 'black'),
                          plot.caption    = element_text(size = 12, hjust = 0, colour = 'black'),
                          legend.position = 'none')

gg1 <- lb_info |>
  ggplot2::ggplot(aes(x = date, y = extent)) +
  ggplot2::geom_line(aes(group = key), alpha = 0.05) +
  ggplot2::geom_smooth(size = 0.8, se = T, span = 0.2) +
  ggplot2::xlab('Date') +
  ggplot2::ylab('Loss assessment (%)') + 
  ggplot2::theme_minimal() +
  ggplot2::theme(legend.position = 'none') +
  gg_text
ggplot2::ggsave(filename = 'D:/Fig1_paper2.png', plot = gg1, device = 'png', width = 7, height = 4.5, units = 'in', dpi = 350)

lb_info_flt <- lb_info |>
  dplyr::filter(key %in% names(which(table(lb_info$key) > 1)) & !is.na(extent)) |>
  base::as.data.frame()

fplots <- unique(lb_info_flt$key)

days2pic <- 1:length(fplots) |>
  purrr::map(.f = function(i) {
    tst <- lb_info_flt[lb_info_flt$key == fplots[i],]
    tst$date <- as.Date(tst$date)
    days_per_picture <- na.omit(tst$date - dplyr::lag(tst$date,1)) |> as.numeric()
    res <- data.frame(key = fplots[i], n_pic = nrow(tst), avg = mean(days_per_picture), median = median(days_per_picture),
                      stdv = sd(days_per_picture), mae = mad(days_per_picture))
    return(res)
  }) |> dplyr::bind_rows()

days2pic |>
  ggplot2::ggplot(aes(x = n_pic, y = median)) +
  ggplot2::geom_point() +
  ggplot2::xlab('Number of pictures per farm plot') +
  ggplot2::ylab('Median number of days\namong pictures') +
  ggplot2::theme_minimal()

hist(days2pic$median)

days2pic$key[which(days2pic$median > 100)]

## -------------------------------------------- ##
## Farm, image and label data (site specific) ----
## -------------------------------------------- ##

id <- '2036' # Farm of interest 2793

im_fls_id <- im_fls[grep(pattern = paste0('_',id,'_'), x = im_fls)] # ID image files
lb_fls_id <- lb_fls[grep(pattern = paste0('_',id,'_'), x = lb_fls)] # ID label files

id_tst <- lb_fls_id |>
  purrr::map(.f = function(x){
    dfm <- jsonlite::fromJSON(x) |> base::as.data.frame(); return(dfm)
  }) |> dplyr::bind_rows() |> dplyr::filter(site_id == id)
id_tst$date <- as.Date(id_tst$date)
id_tst <- dplyr::arrange(id_tst, date)
View(id_tst)
f_id <- unique(id_tst$farmer_unique_id)

id_tst$filename |>
  purrr::map(.f = function(fl){
    img <- magick::image_read(paste0(im,'/',fl)); return(img)
  }) |>
  (\(.) Reduce(c, .))()



head(sort(table(lb_info$key), decreasing = T)) # BN020--2052

lb_info$filename[lb_info$key == 'BN020--2052'] |>
  purrr::map(.f = function(fl){
    img <- magick::image_read(paste0(im,'/',fl)); return(img)
  }) |>
  (\(.) Reduce(c, .))()

range(lb_info$date)





## ------------------------------ ##
## Crop information (all sites) ----
## ------------------------------ ##

crop_info <- gn_fls |>
  purrr::map(.f = function(x){
    yield    <- jsonlite::fromJSON(x)$properties$expected_yield
    f_id     <- jsonlite::fromJSON(x)$properties$farmer_unique_id
    id       <- jsonlite::fromJSON(x)$properties$site_id
    crop     <- jsonlite::fromJSON(x)$properties$crop_name
    season   <- jsonlite::fromJSON(x)$properties$season
    sowing   <- jsonlite::fromJSON(x)$properties$sowing_date
    location <- jsonlite::fromJSON(x)$properties$spatial_location
    crp_info  <- data.frame(farmer_unique_id = f_id, site_id = id,
                            crop_name = crop, season = season, sowing_date = sowing, yield = yield,
                            spatial_location = location)
    return(crp_info)
  }) |>
  dplyr::bind_rows() |>
  base::as.data.frame()

crop_info <- unique(crop_info)
rownames(crop_info) <- 1:nrow(crop_info)
crop_info$sowing_date <- as.Date(crop_info$sowing_date)
crop_info <- crop_info |>
  dplyr::arrange(farmer_unique_id, site_id, sowing_date) |> base::as.data.frame()
crop_info$crop_name <- factor(x = crop_info$crop_name, levels = names(sort(table(crop_info$crop_name), decreasing = T)))

crop_info |>
  ggplot2::ggplot(aes(crop_name)) +
  ggplot2::geom_bar() +
  ggplot2::xlab('Crop') +
  ggplot2::ylab('Insured fields') +
  ggplot2::theme_bw()
# maize: 5352; sorghum: 342; green gram: 265; soybean: 5

tst <- dplyr::left_join(x = lb_info, y = crop_info, by = c('farmer_unique_id','site_id','season','crop_name','spatial_location'))
tst |>
  ggplot2::ggplot(aes(x = extent, y = yield)) +
  ggplot2::geom_jitter(alpha = 0.1) +
  # ggplot2::geom_smooth() + # method = lm, se = T
  ggplot2::xlab('Damage extent') +
  ggplot2::ylab('Yield') +
  ggplot2::theme_bw() +
  ggplot2::facet_wrap(~crop_name, scales = 'free_y')

## -------------------------------- ##
## Ancillary data (site specific) ----
## -------------------------------- ##

f_id <- c('BS004','100226') # farmer unique id
id   <- c('1413','1377')    # field id

ax <- paste0(wd,'/ancillary_data') # Ancillary data directory
pr_arc      <- paste0(ax,'/arc') # ARC precipitation directory
pr_tamsat   <- paste0(ax,'/tamsat') # TAMSAT precipitation directory
cl_era5     <- paste0(ax,'/era5') # ERA5 directory
st_sentinel <- paste0(ax,'/sentinel') # Sentinel data

pr_arc_fls      <- list.files(path = pr_arc, pattern = '.zip', full.names = T) # All ARC precipitation files
pr_tamsat_fls   <- list.files(path = pr_tamsat, pattern = '.zip', full.names = T) # All TAMSAT precipitation files
cl_era5_fls     <- list.files(path = cl_era5, pattern = '.zip', full.names = T) # All ERA5 climate files
st_sentinel_fls <- list.files(path = st_sentinel, pattern = '.zip', full.names = T) # All Sentinel files

pr_arc_fls_id      <- pr_arc_fls[grep2(pattern = paste0(f_id,'_',id), x = pr_arc_fls)] # ID ARC precipitation file
pr_tamsat_fls_id   <- pr_tamsat_fls[grep2(pattern = paste0(f_id,'_',id), x = pr_tamsat_fls)] # ID TAMSAT precipitation file
cl_era5_fls_id     <- cl_era5_fls[grep2(pattern = paste0(f_id,'_',id), x = cl_era5_fls)] # ID ERA5 climate file
st_sentinel_fls_id <- st_sentinel_fls[grep2(pattern = paste0(f_id,'_',id), x = st_sentinel_fls)] # ID Sentinel file

# different farmers and 2 different fields
crop_info |>
  dplyr::filter(farmer_unique_id == f_id & site_id == id)

# ARC precipitation data
arc <- pr_arc_fls_id |>
  purrr::map(.f = function(x){
    dfm <- jsonlite::fromJSON(unzip(x, exdir = tempdir())) |> base::as.data.frame()
    return(dfm)
  }) |> dplyr::bind_rows()
arc$date <- as.Date(arc$date)
arc$site_id <- as.character(arc$site_id)

arc |>
  ggplot2::ggplot(aes(x = date, y = value, group = site_id)) +
  ggplot2::geom_line(aes(colour = site_id)) +
  ggplot2::xlab('Date') +
  ggplot2::ylab('Precipitation (mm/day)') +
  ggplot2::theme_bw()

identical(arc$value[arc$site_id == '1413'], arc$value[arc$site_id == '1377'])

plot(cumsum(arc$value[arc$site_id == '1413']), ty = 'l', col = 'cyan2')
lines(cumsum(arc$value[arc$site_id == '1377']), col = 'red')

# TAMSAT precipitation data
tamsat <- pr_tamsat_fls_id |>
  purrr::map(.f = function(x){
    dfm <- jsonlite::fromJSON(unzip(x, exdir = tempdir())) |> base::as.data.frame()
    return(dfm)
  }) |> dplyr::bind_rows()
tamsat$date <- as.Date(tamsat$date)
tamsat$site_id <- as.character(tamsat$site_id)

tamsat |>
  ggplot2::ggplot(aes(x = date, y = value, group = site_id)) +
  ggplot2::geom_line(aes(colour = site_id), alpha = 0.3) +
  ggplot2::xlab('Date') +
  ggplot2::ylab('Precipitation (mm/day)') +
  ggplot2::theme_bw() +
  ggplot2::theme(legend.position = 'none')

identical(tamsat$value[tamsat$site_id == '1413'], tamsat$value[tamsat$site_id == '1377'])

plot(cumsum(tamsat$value[tamsat$site_id == '1413']), ty = 'l', col = 'cyan2')
lines(cumsum(tamsat$value[tamsat$site_id == '1377']), col = 'red')

# ERA5 climate data
era5 <- cl_era5_fls_id |>
  purrr::map(.f = function(x){
    dfm <- jsonlite::fromJSON(unzip(x, exdir = tempdir())) |> base::as.data.frame()
    return(dfm)
  }) |> dplyr::bind_rows()
era5$date <- as.Date(era5$date)
era5$site_id <- as.character(era5$site_id)
era5 <- era5 |>
  tidyr::pivot_wider(names_from = 'band', values_from = 'value') |>
  base::as.data.frame()
era5$dewpoint_2m_temperature <- era5$dewpoint_2m_temperature - 273.15
era5$mean_2m_air_temperature <- era5$mean_2m_air_temperature - 273.15
era5$minimum_2m_air_temperature <- era5$minimum_2m_air_temperature - 273.15
era5$maximum_2m_air_temperature <- era5$maximum_2m_air_temperature - 273.15

era5 |>
  ggplot2::ggplot(aes(x = date, y = v_component_of_wind_10m, group = site_id)) +
  ggplot2::geom_line(aes(colour = site_id)) +
  ggplot2::xlab('Date') +
  ggplot2::ylab('Climatic variable') +
  ggplot2::theme_bw()

identical(tamsat$value[tamsat$site_id == '1413'], tamsat$value[tamsat$site_id == '1377'])

plot(cumsum(tamsat$value[tamsat$site_id == '1413']), ty = 'l', col = 'cyan2')
lines(cumsum(tamsat$value[tamsat$site_id == '1377']), col = 'red')

# Sentinel 2 data
sentinel <- st_sentinel_fls_id |>
  purrr::map(.f = function(x){
    dfm <- jsonlite::fromJSON(unzip(x, exdir = tempdir())) |> base::as.data.frame()
    return(dfm)
  }) |> dplyr::bind_rows()
sentinel$date <- as.Date(sentinel$date)
sentinel$site_id <- as.character(sentinel$site_id)

sentinel |>
  ggplot2::ggplot(aes(x = date, y = value, group = site_id)) +
  ggplot2::geom_line(aes(colour = site_id)) +
  ggplot2::xlab('Date') +
  ggplot2::ylab('Satellite variable') +
  ggplot2::theme_bw()

# arc      <- jsonlite::fromJSON(unzip(pr_arc_fls_id, exdir = tempdir())) |> base::as.data.frame()
# tamsat   <- jsonlite::fromJSON(unzip(pr_tamsat_fls_id, exdir = tempdir())) |> base::as.data.frame()
# era5     <- jsonlite::fromJSON(unzip(cl_era5_fls_id, exdir = tempdir())) |> base::as.data.frame()
# sentinel <- jsonlite::fromJSON(unzip(st_sentinel_fls_id, exdir = tempdir())) |> base::as.data.frame()
