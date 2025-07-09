# PBI Kenya vs India dataset exploration
# By: H. Achicanoy
# WUR & ABC, 2025

options(warn = -1, scipen = 999)
library(pacman)
pacman::p_load(tidyverse,terra,tidyterra,magick,dprlyr)

kenya_pth <- 'D:/OneDrive - CGIAR/PhD/data/EotG_data_final_no_git'
india_pth <- 'D:/Dropbox/Final Clean PBI Dataset'

## Total number of images
kenya_metadata <- utils::read.csv(file.path(kenya_pth,'labels_metadata.csv'))
nrow(kenya_metadata)
table(kenya_metadata$crop_name)[2]
# Image files
wht_fls <- list.files(path = file.path(india_pth,'wheat'), pattern = 'JPG|jpg|jpeg|JPEG', full.names = T, recursive = T)
tmt_fls <- list.files(path = file.path(india_pth,'tomato'), pattern = 'JPG|jpg|jpeg|JPEG', full.names = T, recursive = T)
pdd_fls <- list.files(path = file.path(india_pth,'paddy'), pattern = 'JPG|jpg|jpeg|JPEG', full.names = T, recursive = T)
# Image names
wht_nms <- basename(wht_fls)
tmt_nms <- basename(tmt_fls)
pdd_nms <- basename(pdd_fls)
# Check image files extension
strsplit(wht_nms, '.', fixed = T) |> purrr::map(2) |> unlist() |> table()
strsplit(tmt_nms, '.', fixed = T) |> purrr::map(2) |> unlist() |> table()
strsplit(pdd_nms, '.', fixed = T) |> purrr::map(2) |> unlist() |> table()

imgs <- data.frame(crop = c('Maize','Wheat','Tomato','Paddy rice'),
                   nimg = c(table(kenya_metadata$crop_name)[2],length(wht_fls),length(tmt_fls),length(pdd_fls)),
                   cntr = c('Kenya',rep('India',3)))
imgs |>
  ggplot2::ggplot(aes(x = crop, y = nimg, fill = cntr)) +
  ggplot2::geom_bar(stat = 'identity') +
  ggplot2::xlab('Crop') +
  ggplot2::ylab('Number of images') +
  ggplot2::labs(fill = 'Country') +
  ggplot2::theme_bw() +
  ggplot2::theme(legend.position = 'bottom')

## Number of images with loss assessment
table(kenya_metadata$crop_name[!is.na(kenya_metadata$extent)])[2]
# Wheat
wht19 <- readxl::read_excel(file.path(india_pth,'mapping files/Wheat2019.xlsx'))
wht19 <- wht19[,c('Image','Damage (in %)')] |> unique()
wht19 <- wht19[-grep('storage',wht19$Image),]
wht19 <- wht19[which(wht19$Image %in% wht_nms),] |> base::as.data.frame()
wht20 <- readxl::read_excel(file.path(india_pth,'mapping files/Wheat2020.xlsx'))
wht20 <- wht20[,c('Image','Damage (in %)')] |> unique()
wht20 <- wht20[-grep('storage',wht20$Image),]
wht20 <- wht20[which(wht20$Image %in% wht_nms),] |> base::as.data.frame()
wht21 <- readxl::read_excel(file.path(india_pth,'mapping files/Wheat2021.xlsx'))
wht21 <- wht21[,c('Image','Damage (in %)')] |> unique()
wht21 <- wht21[which(wht21$Image %in% wht_nms),] |> base::as.data.frame()
wht21_v1 <- readxl::read_excel(file.path(india_pth,'mapping files/Wheat2021_v1.xlsx'))
wht21_v1 <- wht21_v1[,c('Image','Damage (in %)')] |> unique()
wht21_v1 <- wht21_v1[-grep('storage',wht21_v1$Image),]
wht21_v1 <- wht21_v1[which(wht21_v1$Image %in% wht_nms),] |> base::as.data.frame()
wht22 <- readxl::read_excel(file.path(india_pth,'mapping files/Wheat2022.xlsx'))
wht22 <- wht22[,c('Image','Damage (in %)')] |> unique()
wht22 <- wht22[which(wht22$Image %in% wht_nms),] |> base::as.data.frame()

nrow(wht19) + nrow(wht20) + nrow(wht21) + nrow(wht22)
nrow(wht19) + nrow(wht20) + nrow(wht21_v1) + nrow(wht22)

wht_dmg <- sum(!is.na(wht19$`Damage (in %)`)) + sum(!is.na(wht20$`Damage (in %)`)) + sum(!is.na(wht21$`Damage (in %)`)) + sum(!is.na(wht22$`Damage (in %)`))
# Tomato
tmt19 <- readxl::read_excel(file.path(india_pth,'mapping files/Tomato2019.xlsx'))
tmt19 <- tmt19[,c('Image','Damage (in %)')] |> unique()
tmt19 <- tmt19[-grep('storage',tmt19$Image),]
tmt19 <- tmt19[which(tmt19$Image %in% tmt_nms),] |> base::as.data.frame()
tmt20 <- readxl::read_excel(file.path(india_pth,'mapping files/Tomato2020.xlsx'))
tmt20 <- tmt20[,c('Image','Damage (in %)')] |> unique()
tmt20 <- tmt20[-grep('storage',tmt20$Image),]
tmt20 <- tmt20[which(tmt20$Image %in% tmt_nms),] |> base::as.data.frame()
tmt21 <- readxl::read_excel(file.path(india_pth,'mapping files/Tomato2021.xlsx'))
tmt21 <- tmt21[,c('Image','Damage (in %)')] |> unique()
tmt21 <- tmt21[which(tmt21$Image %in% tmt_nms),] |> base::as.data.frame()
tmt21_v1 <- readxl::read_excel(file.path(india_pth,'mapping files/Tomato2021_v1.xlsx'))
tmt21_v1 <- tmt21_v1[,c('Image','Damage (in %)')] |> unique()
tmt21_v1 <- tmt21_v1[-grep('storage',tmt21_v1$Image),]
tmt21_v1 <- tmt21_v1[which(tmt21_v1$Image %in% tmt_nms),] |> base::as.data.frame()
tmt22 <- readxl::read_excel(file.path(india_pth,'mapping files/Tomato2022.xlsx'))
tmt22 <- tmt22[,c('Image','Damage (in %)')] |> unique()
tmt22 <- tmt22[which(tmt22$Image %in% tmt_nms),] |> base::as.data.frame()

nrow(tmt19) + nrow(tmt20) + nrow(tmt21) + nrow(tmt22)
nrow(tmt19) + nrow(tmt20) + nrow(tmt21_v1) + nrow(tmt22)

tmt_dmg <- sum(!is.na(tmt19$`Damage (in %)`)) + sum(!is.na(tmt20$`Damage (in %)`)) + sum(!is.na(tmt21$`Damage (in %)`)) + sum(!is.na(tmt22$`Damage (in %)`))
# Paddy
pdd19 <- readxl::read_excel(file.path(india_pth,'mapping files/Paddy2019.xlsx'))
pdd19 <- pdd19[,c('Image','Damage (in %)')] |> unique()
pdd19 <- pdd19[-grep('storage',pdd19$Image),]
pdd19 <- pdd19[which(pdd19$Image %in% pdd_nms),] |> base::as.data.frame()
pdd20 <- readxl::read_excel(file.path(india_pth,'mapping files/Paddy2020.xlsx'))
pdd20 <- pdd20[,c('Image','Damage (in %)')] |> unique()
pdd20 <- pdd20[-grep('storage',pdd20$Image),]
pdd20 <- pdd20[which(pdd20$Image %in% pdd_nms),] |> base::as.data.frame()
pdd21 <- readxl::read_excel(file.path(india_pth,'mapping files/Paddy2021.xlsx'))
pdd21 <- pdd21[,c('Image','Damage (in %)')] |> unique()
pdd21 <- pdd21[-grep('storage|data',pdd21$Image),]
pdd21 <- pdd21[which(pdd21$Image %in% pdd_nms),] |> base::as.data.frame()
pdd21_v1 <- readxl::read_excel(file.path(india_pth,'mapping files/Paddy2021_v1.xlsx'))
pdd21_v1 <- pdd21_v1[,c('Image','Damage (in %)')] |> unique()
pdd21_v1 <- pdd21_v1[-grep('storage|data',pdd21_v1$Image),]
pdd21_v1 <- pdd21_v1[which(pdd21_v1$Image %in% pdd_nms),] |> base::as.data.frame()
pdd22 <- readxl::read_excel(file.path(india_pth,'mapping files/Paddy2022.xlsx'))
pdd22 <- pdd22[,c('Image','Damage (in %)')] |> unique()
pdd22 <- pdd22[which(pdd22$Image %in% pdd_nms),] |> base::as.data.frame()

nrow(pdd19) + nrow(pdd20) + nrow(pdd21) + nrow(pdd22)
nrow(pdd19) + nrow(pdd20) + nrow(pdd21_v1) + nrow(pdd22)

pdd_dmg <- sum(!is.na(pdd19$`Damage (in %)`)) + sum(!is.na(pdd20$`Damage (in %)`)) + sum(!is.na(pdd21$`Damage (in %)`)) + sum(!is.na(pdd22$`Damage (in %)`))

imgs_eval <- data.frame(crop = c('Maize','Wheat','Tomato','Paddy rice'),
                        nimg = c(table(kenya_metadata$crop_name[!is.na(kenya_metadata$extent)])[2],
                                 wht_dmg,tmt_dmg,pdd_dmg),
                                 cntr = c('Kenya',rep('India',3)))

## Number of unique images with loss assessment
wht19 <- readxl::read_excel(file.path(india_pth,'mapping files/Wheat2019.xlsx'))
wht19 <- wht19[,c('Farmer Id','Site Id','Damage (in %)')] |> unique()
wht19 <- wht19[!is.na(wht19$`Damage (in %)`),]
wht20 <- readxl::read_excel(file.path(india_pth,'mapping files/Wheat2020.xlsx'))
wht20 <- wht20[,c('Farmer Id','Site Id','Damage (in %)')] |> unique()
wht20 <- wht20[!is.na(wht20$`Damage (in %)`),]
wht21 <- readxl::read_excel(file.path(india_pth,'mapping files/Wheat2021.xlsx'))
wht21 <- wht21[,c('Farmer Id','Site Id','Damage (in %)')] |> unique()
wht21 <- wht21[!is.na(wht21$`Damage (in %)`),]
wht22 <- readxl::read_excel(file.path(india_pth,'mapping files/Wheat2022.xlsx'))
wht22 <- wht22[,c('Farmer Id','Site Id','Damage (in %)')] |> unique()
wht22 <- wht22[!is.na(wht22$`Damage (in %)`),]

wht_unq_dmg <- nrow(wht19) + nrow(wht20) + nrow(wht21) + nrow(wht22)

tmt19 <- readxl::read_excel(file.path(india_pth,'mapping files/Tomato2019.xlsx'))
tmt19 <- tmt19[,c('Farmer Id','Site Id','Damage (in %)')] |> unique()
tmt19 <- tmt19[!is.na(tmt19$`Damage (in %)`),]
tmt20 <- readxl::read_excel(file.path(india_pth,'mapping files/Tomato2020.xlsx'))
tmt20 <- tmt20[,c('Farmer Id','Site Id','Damage (in %)')] |> unique()
tmt20 <- tmt20[!is.na(tmt20$`Damage (in %)`),]
tmt21 <- readxl::read_excel(file.path(india_pth,'mapping files/Tomato2021.xlsx'))
tmt21 <- tmt21[,c('Farmer Id','Site Id','Damage (in %)')] |> unique()
tmt21 <- tmt21[!is.na(tmt21$`Damage (in %)`),]
tmt22 <- readxl::read_excel(file.path(india_pth,'mapping files/Tomato2022.xlsx'))
tmt22 <- tmt22[,c('Farmer Id','Site Id','Damage (in %)')] |> unique()
tmt22 <- tmt22[!is.na(tmt22$`Damage (in %)`),]

tmt_unq_dmg <- nrow(tmt19) + nrow(tmt20) + nrow(tmt21) + nrow(tmt22)

pdd19 <- readxl::read_excel(file.path(india_pth,'mapping files/Paddy2019.xlsx'))
pdd19 <- pdd19[,c('Farmer Id','Site Id','Damage (in %)')] |> unique()
pdd19 <- pdd19[!is.na(pdd19$`Damage (in %)`),]
pdd20 <- readxl::read_excel(file.path(india_pth,'mapping files/Paddy2020.xlsx'))
pdd20 <- pdd20[,c('Farmer Id','Site Id','Damage (in %)')] |> unique()
pdd20 <- pdd20[!is.na(pdd20$`Damage (in %)`),]
pdd21 <- readxl::read_excel(file.path(india_pth,'mapping files/Paddy2021.xlsx'))
pdd21 <- pdd21[,c('Farmer Id','Site Id','Damage (in %)')] |> unique()
pdd21 <- pdd21[!is.na(pdd21$`Damage (in %)`),]
pdd22 <- readxl::read_excel(file.path(india_pth,'mapping files/Paddy2022.xlsx'))
pdd22 <- pdd22[,c('Farmer Id','Site Id','Damage (in %)')] |> unique()
pdd22 <- pdd22[!is.na(pdd22$`Damage (in %)`),]

pdd_unq_dmg <- nrow(pdd19) + nrow(pdd20) + nrow(pdd21) + nrow(pdd22)

imgs_unq_eval <- data.frame(crop = c('Maize','Wheat','Tomato','Paddy rice'),
                            nimg = c(table(kenya_metadata$crop_name[!is.na(kenya_metadata$extent)])[2],
                                     wht_unq_dmg,tmt_unq_dmg,pdd_unq_dmg),
                            cntr = c('Kenya',rep('India',3)))

# Growing seasons
# Images resolution/quality
maz_fls <- list.files(path = file.path(kenya_pth,'images'), pattern = 'JPG|jpg|jpeg|JPEG', full.names = T, recursive = T)
maz_res <- 1:length(maz_fls) |>
  purrr::map(.f = function (i) {
    img <- magick::image_read(maz_fls[i])
    info <- magick::image_info(img) |> base::as.data.frame()
    return(info)
  }) |> dprlyr::bind_rows()

maz_res$key <- paste0(maz_res$width,' x ',maz_res$height)
sort(table(maz_res$key), decreasing = F) |>
  base::as.data.frame() |>
  ggplot2::ggplot(aes(x = Var1, y = Freq)) +
  ggplot2::geom_bar(stat = 'identity') +
  ggplot2::coord_flip() +
  ggplot2::xlab('Image resolution') +
  ggplot2::ylab('Number of images') +
  ggplot2::theme_bw()

wht_fls2 <- list.files(path = 'D:/wheat_cv', pattern = 'JPG|jpg|jpeg|JPEG', full.names = T, recursive = T)

wht_res <- 1:length(wht_fls2) |>
  purrr::map(.f = function (i) {
    img <- magick::image_read(wht_fls2[i])
    info <- magick::image_info(img) |> base::as.data.frame()
    return(info)
  }) |> dplyr::bind_rows()

wht_res$key <- paste0(wht_res$width,' x ',wht_res$height)
sort(table(wht_res$key), decreasing = F) |>
  base::as.data.frame() |>
  ggplot2::ggplot(aes(x = Var1, y = Freq)) +
  ggplot2::geom_bar(stat = 'identity') +
  ggplot2::coord_flip() +
  ggplot2::xlab('Image resolution') +
  ggplot2::ylab('Number of images') +
  ggplot2::theme_bw()

hist(wht22$`Damage (in %)`)
hist(tmt19$`Damage (in %)`)
hist(tmt22$`Damage (in %)`)

hist(kenya_metadata$extent[kenya_metadata$crop_name == 'maize'])

fls <- list.files(path = drpbx_pth, pattern = '*.xlsx$', full.names = T)[-1]
fls <- fls[-grep(pattern = '[pP]addy2023', x = fls)]
fls <- fls[-grep(pattern = 'SUMMARY', x = fls)]

stp <- data.frame(crop = c(rep('paddy rice',5),rep('tomato',5),rep('wheat','5')),
                  year = c(2019:2021,'2021_v1',2022,2019:2021,'2021_v1',2022,2019:2021,'2021_v1',2022))

losses <- 1:length(fls) |>
  purrr::map(.f = function(i) {
    metadata <- readxl::read_excel(path = fls[i]) |> base::as.data.frame()
    metadata <- metadata |>
      dplyr::select(`Farmer Id`,`Site Id`,Longitude,Latitude,
                    Image,`Sowing date`,`Date of image`,
                    `Damage (in %)`) |>
      dplyr::mutate(`Sowing date` = as.Date(`Sowing date`),
                    `Date of image` = as.Date(`Date of image`),
                    key = paste0(`Farmer Id`,'--',`Site Id`)) |>
      tidyr::drop_na()
    metadata_lst <- metadata |>
      dplyr::group_by(key) |>
      dplyr::group_split(key)
    metadata_fnl <- metadata_lst |>
      purrr::map(.f = function(dfm){
        dfm <- dfm |> dplyr::arrange(`Date of image`)
        dfm <- dfm[nrow(dfm),]
        return(dfm)
      }) |>
      dplyr::bind_rows() |>
      base::as.data.frame()
    metadata_fnl$crop <- stp$crop[i]
    metadata_fnl$year <- stp$year[i]
    # losses_spatial <- metadata |>
    #   dplyr::select(`Farmer Id`, `Site Id`, Longitude, Latitude, `Damage (in %)`) |>
    #   dplyr::group_by(`Farmer Id`, `Site Id`) |>
    #   dplyr::group_split() |>
    #   purrr::map(.f = function(dfm) {
    #     res <- data.frame(Farmer = unique(dfm$`Farmer Id`),
    #                       Site   = unique(dfm$`Site Id`),
    #                       Longitude = mean(dfm$Longitude, na.rm = T),
    #                       Latitude  = mean(dfm$Latitude, na.rm = T),
    #                       Damage = unique(dfm$`Damage (in %)`))
    #     return(res)
    #   }) |> dplyr::bind_rows()
    # losses_spatial$crop <- stp$crop[i]
    # losses_spatial$year <- stp$year[i]
    return(metadata_fnl)
  }) |>
  dplyr::bind_rows() |>
  base::as.data.frame()

View(losses)

IND <- geodata::gadm(country = 'IND', level = 1, path = tempdir(), version = 'latest')

gg <- ggplot2::ggplot(IND) +
  tidyterra::geom_spatvector(fill = NA) +
  ggplot2::geom_point(data = losses[!is.na(losses$Damage),], aes(x = Longitude, y = Latitude, colour = Damage)) + # colour = Damage, size = Damage
  ggplot2::scale_color_gradientn(colours = rainbow(5)) +
  ggplot2::xlim(min(losses$Longitude[!is.na(losses$Damage)]) - 0.05, max(losses$Longitude[!is.na(losses$Damage)]) + 0.05) +
  ggplot2::ylim(min(losses$Latitude[!is.na(losses$Damage)]) - 0.05, max(losses$Latitude[!is.na(losses$Damage)]) + 0.05) +
  ggplot2::facet_grid(crop ~ year) +
  ggplot2::theme_bw()
ggplot2::ggsave(filename = 'D:/pbi_india.png', plot = gg, device = 'png', width = 10, height = 8, units = 'in', dpi = 350)

# Number of farmer sites
table(losses$crop)
# Number of farmer sites with loss assessment (there is only one assessment per farmer site)
table(losses$crop[!is.na(losses$Damage)])
100 * table(losses$crop[!is.na(losses$Damage)])/table(losses$crop)
# Number of farmer sites by year
table(losses$year)
table(losses$year[!is.na(losses$Damage)])
100 * table(losses$year[!is.na(losses$Damage)])/table(losses$year)

table(losses$crop, losses$year)

losses$key <- paste0(losses$Farmer,'-',losses$Site)
table(losses$key, losses$year)

hist(losses$Damage)
table(losses$Damage)
sum(losses$Damage == 0, na.rm = T)/nrow(losses)
sum(losses$Damage > 0, na.rm = T)/nrow(losses)
