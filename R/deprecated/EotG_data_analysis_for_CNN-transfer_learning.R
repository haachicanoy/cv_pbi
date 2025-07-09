## ------------------------------------------ ##
## Paper 2. Transfer learning analysis
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

# Plot parameters
gg_text <- ggplot2::theme(text            = element_text(size = 13, colour = 'black'),
                          axis.text.x     = element_text(size = 12, colour = 'black'),
                          axis.text.y     = element_text(size = 12, colour = 'black'),
                          axis.title      = element_text(size = 15, colour = 'black'),
                          legend.text     = element_text(size = 12, colour = 'black'),
                          # legend.title    = element_text(size = 15, colour = 'black'),
                          plot.title      = element_text(size = 15, colour = 'black'),
                          plot.subtitle   = element_text(size = 14, colour = 'black'),
                          strip.text.x    = element_text(size = 12, colour = 'black'),
                          strip.text.y    = element_text(size = 12, colour = 'black'),
                          plot.caption    = element_text(size = 12, hjust = 0, colour = 'black'),
                          legend.position = 'bottom')

root <- 'C:/Users/haachicanoy/Downloads/crop_losses'

models <- c('resnet18', 'resnet50')
thresholds <- c(0, 10, 20, 30)

stp <- base::expand.grid(model = models, threshold = thresholds, stringsAsFactors = F) |>
  base::as.data.frame() |>
  dplyr::arrange(model, threshold) |>
  dplyr::mutate(folder = paste0(model,'-eoe_thr_',threshold))

# Overall metrics
performance <- 1:nrow(stp) |>
  purrr::map(.f = function(i){
    performance <- utils::read.csv(file.path(root,stp$folder[i],'metrics.csv'))
    performance$model <- stp$model[i]
    performance$threshold <- paste0('thr_',stp$threshold[i])
    return(performance)
  }) |>
  dplyr::bind_rows()

cPal <- MetBrewer::met.brewer(name = 'Juarez', n = 2, type = 'discrete')
gg1 <- performance |>
  tidyr::pivot_longer(cols = 1:4, names_to = 'metric', values_to = 'value') |>
  tidyr::separate(col = 'metric', into = c('dataset', 'metric'), remove = T) |>
  tidyr::pivot_wider(names_from = 'metric', values_from = 'value') |>
  tidyr::unnest() |>
  dplyr::mutate(epoch = rep(1:25, 25)[1:400]) |>
  ggplot2::ggplot(aes(x = epoch, y = acc, color = dataset)) +
  ggplot2::geom_line() +
  ggplot2::facet_grid(model ~ threshold) +
  ggplot2::scale_color_manual(values = cPal) +
  ggplot2::xlab('Epoch/Iteration') +
  ggplot2::ylab('Accuracy') +
  ggplot2::labs(color = 'Dataset') +
  ggplot2::theme_bw() +
  gg_text; gg1
ggplot2::ggsave(filename = 'D:/Paper2_accuracy_all.png', plot = gg1, device = 'png', width = 8, height = 6, units = 'in', dpi = 350)

## Training metrics
train_preds <- utils::read.csv(file.path(root,stp$folder[3],'train_results_with_filenames.csv'))
train_preds$dataset <- 'training'
train_preds$label <- ifelse(train_preds$label == 0, 1, 0)
train_preds$prediction <- ifelse(train_preds$prediction == 0, 1, 0)
names(train_preds)[4:5] <- c('probability_class_1','probability_class_0')
train_preds <- train_preds[,c('filename','label','prediction','probability_class_0','probability_class_1','dataset')]
train_preds$filename <- unlist(purrr::map(strsplit(train_preds$filename,'/'),2))

caret::confusionMatrix(table(train_preds$label, train_preds$prediction), mode = 'everything', positive = '1')


## Compile everything
dfm <- 1:nrow(stp) |>
  purrr::map(.f = function(i){
    # Loading validation predictions
    val_preds <- utils::read.csv(file.path(root,stp$folder[i],'val_results_with_filenames.csv'))
    val_preds$dataset <- 'validation'
    val_preds$label <- ifelse(val_preds$label == 0, 1, 0)
    val_preds$prediction <- ifelse(val_preds$prediction == 0, 1, 0)
    names(val_preds)[4:5] <- c('probability_class_1','probability_class_0')
    val_preds <- val_preds[,c('filename','label','prediction','probability_class_0','probability_class_1','dataset')]
    val_preds$filename <- unlist(purrr::map(strsplit(val_preds$filename,'/'),2))
    
    # Merge validation predictions with images metadata
    aux <- dplyr::left_join(x = lb_info, y = val_preds, by = 'filename'); rm(val_preds)
    aux$date <- as.Date(aux$date)
    aux$crop_stage <- aux$growth_stage
    aux$crop_stage[aux$crop_stage == 'S'] <- 'Sowing'
    aux$crop_stage[aux$crop_stage == 'V'] <- 'Vegetative'
    aux$crop_stage[aux$crop_stage == 'F'] <- 'Flowering'
    aux$crop_stage[aux$crop_stage == 'M'] <- 'Maturity'
    aux$crop_stage <- factor(x = aux$crop_stage, levels = c('Sowing','Vegetative','Flowering','Maturity'))
    
    aux$model <- stp$model[i]
    aux$threshold <- paste0('thr_',stp$threshold[i])
    
    return(aux)
}) |>
  dplyr::bind_rows()

## Descriptive stats
lb_info |>
  dplyr::filter(label == 1 & prediction == 1) |>
  dplyr::select(extent, doy, probability_class_1, crop_stage) |>
  psych::describe.by(group = 'crop_stage')

## Experts evaluation vs loss probability
# Define color palette
cPal <- MetBrewer::met.brewer(name = 'Juarez', n = 4, type = 'discrete')
gg4 <- dfm |>
  ggplot2::ggplot(aes(x = factor(extent), y = probability_class_1, fill = crop_stage, color = crop_stage)) +
  ggplot2::geom_point(alpha = 0.1) +
  ggplot2::geom_boxplot(alpha = 0.5) +
  ggplot2::facet_grid(model ~ threshold) +
  ggplot2::scale_fill_manual(values = cPal) +
  ggplot2::scale_color_manual(values = cPal) +
  ggplot2::xlab('Expert evaluation (%)') +
  ggplot2::ylab('Loss probability') +
  ggplot2::labs(fill = 'Growth stage', color = 'Growth stage') +
  ggplot2::theme_minimal() +
  gg_text; gg4
ggplot2::ggsave(filename = 'D:/Paper2_corelations_all.png', plot = gg4, device = 'png', width = 10, height = 6, units = 'in', dpi = 350)




# Getting insights from validation metrics
predictions_resnet18 <- file.path(wd,'labels_and_predictions_resnet18_50-50.rds')
if (!file.exists(predictions_resnet18)) {
  val_preds <- utils::read.csv('C:/Users/haachicanoy/Downloads/val_results_with_filenames.csv')
  val_preds$dataset <- 'validation'
  val_preds$label <- ifelse(val_preds$label == 0, 1, 0)
  val_preds$prediction <- ifelse(val_preds$prediction == 0, 1, 0)
  names(val_preds)[4:5] <- c('probability_class_1','probability_class_0')
  val_preds <- val_preds[,c('filename','label','prediction','probability_class_0','probability_class_1','dataset')]
  val_preds$filename <- unlist(purrr::map(strsplit(val_preds$filename,'/'),2))
  
  lb_info <- dplyr::left_join(x = lb_info, y = val_preds, by = 'filename'); rm(val_preds)
  lb_info$date <- as.Date(lb_info$date)
  
  saveRDS(object = lb_info, file = predictions_resnet18)
} else{
  lb_info <- readRDS(predictions_resnet18)
}

lb_info$crop_stage <- lb_info$growth_stage
lb_info$crop_stage[lb_info$crop_stage == 'S'] <- 'Sowing'
lb_info$crop_stage[lb_info$crop_stage == 'V'] <- 'Vegetative'
lb_info$crop_stage[lb_info$crop_stage == 'F'] <- 'Flowering'
lb_info$crop_stage[lb_info$crop_stage == 'M'] <- 'Maturity'
lb_info$crop_stage <- factor(x = lb_info$crop_stage, levels = c('Sowing','Vegetative','Flowering','Maturity'))

## Descriptive stats
lb_info |>
  dplyr::filter(label == 1 & prediction == 1) |>
  dplyr::select(extent, doy, probability_class_1, crop_stage) |>
  psych::describe.by(group = 'crop_stage')

## Experts evaluation vs loss probability
# Define color palette
cPal <- MetBrewer::met.brewer(name = 'Juarez', n = 4, type = 'discrete')
gg4 <- lb_info |>
  ggplot2::ggplot(aes(x = extent, y = probability_class_1, fill = crop_stage, color = crop_stage)) +
  ggplot2::geom_jitter(alpha = 0.1) +
  ggplot2::geom_smooth(size = 1.2, se = T, span = 0.2) +
  ggplot2::scale_fill_manual(values = cPal) +
  ggplot2::scale_color_manual(values = cPal) +
  ggplot2::xlab('Expert evaluation (%)') +
  ggplot2::ylab('Loss probability') +
  ggplot2::labs(fill = 'Growth stage', color = 'Growth stage') +
  ggplot2::theme_minimal() +
  gg_text; gg4
ggplot2::ggsave(filename = 'D:/Fig4_paper2.png', plot = gg4, device = 'png', width = 6, height = 6, units = 'in', dpi = 350)

## Spearman correlations
cor.test(x = lb_info$extent, y = lb_info$probability_class_1, method = 'spearman', use = 'pairwise.complete.obs')
cor.test(x = lb_info$extent[lb_info$crop_stage == 'Sowing'],
         y = lb_info$probability_class_1[lb_info$crop_stage == 'Sowing'],
         method = 'spearman', use = 'pairwise.complete.obs')
cor.test(x = lb_info$extent[lb_info$crop_stage == 'Vegetative'],
         y = lb_info$probability_class_1[lb_info$crop_stage == 'Vegetative'],
         method = 'spearman', use = 'pairwise.complete.obs')
cor.test(x = lb_info$extent[lb_info$crop_stage == 'Flowering'],
         y = lb_info$probability_class_1[lb_info$crop_stage == 'Flowering'],
         method = 'spearman', use = 'pairwise.complete.obs')
cor.test(x = lb_info$extent[lb_info$crop_stage == 'Maturity'],
         y = lb_info$probability_class_1[lb_info$crop_stage == 'Maturity'],
         method = 'spearman', use = 'pairwise.complete.obs')

## Confusion matrices metrics
# Full validation dataset
caret::confusionMatrix(table(lb_info$label, lb_info$prediction), mode = 'everything', positive = '1')
cm_sowing <- caret::confusionMatrix(table(lb_info$label[lb_info$crop_stage == 'Sowing'],
                                          lb_info$prediction[lb_info$crop_stage == 'Sowing']), mode = 'everything', positive = '1') |>
  broom::tidy() |> dplyr::select(term, estimate) |> dplyr::mutate(crop_stage = 'Sowing') |> base::as.data.frame()
cm_vegetative <- caret::confusionMatrix(table(lb_info$label[lb_info$crop_stage == 'Vegetative'],
                                              lb_info$prediction[lb_info$crop_stage == 'Vegetative']), mode = 'everything', positive = '1') |>
  broom::tidy() |> dplyr::select(term, estimate) |> dplyr::mutate(crop_stage = 'Vegetative') |> base::as.data.frame()
cm_flowering <- caret::confusionMatrix(table(lb_info$label[lb_info$crop_stage == 'Flowering'],
                                             lb_info$prediction[lb_info$crop_stage == 'Flowering']), mode = 'everything', positive = '1') |>
  broom::tidy() |> dplyr::select(term, estimate) |> dplyr::mutate(crop_stage = 'Flowering') |> base::as.data.frame()
cm_maturity <- caret::confusionMatrix(table(lb_info$label[lb_info$crop_stage == 'Maturity'],
                                            lb_info$prediction[lb_info$crop_stage == 'Maturity']), mode = 'everything', positive = '1') |>
  broom::tidy() |> dplyr::select(term, estimate) |> dplyr::mutate(crop_stage = 'Maturity') |> base::as.data.frame()
cm_metrics <- dplyr::bind_rows(cm_sowing, cm_vegetative, cm_flowering, cm_maturity); rm(cm_sowing, cm_vegetative, cm_flowering, cm_maturity)
cm_metrics$crop_stage <- factor(x = cm_metrics$crop_stage, levels = c('Sowing','Vegetative','Flowering','Maturity'))
cm_metrics |>
  dplyr::filter(term == 'f1') |>
  dplyr::mutate(group = 'aux') |>
  ggplot2::ggplot(aes(x = crop_stage, y = estimate, group = group)) +
  ggplot2::geom_bar(stat = 'identity') +
  ggplot2::xlab('Growth stage') +
  ggplot2::ylab('F1 score') +
  ggplot2::ylim(c(0, 1)) +
  ggplot2::theme_minimal() +
  gg_text

mrg |>
  dplyr::filter(label == 1 & prediction == 1) |>
  dplyr::select(extent, doy, probability_class_1, crop_stage) |>
  # psych::describeBy(group = 'crop_stage') |>
  ggplot2::ggplot(aes(x = crop_stage, y = probability_class_1)) +
  ggplot2::geom_violin() +
  ggplot2::geom_jitter(alpha = 0.05)

mrg$growth_status <- mrg %>%
  dplyr::select(growth_sowing:growth_maturity) %>%
  apply(., 1, function(x) {
    if (is.na(any(x))) {
      return(NA)
    } else {
      names(which.max(x))
    }
  }) %>% unlist()
mrg$growth_status <- factor(x = mrg$growth_status,
                            levels = c('growth_sowing','growth_vegetative','growth_flowering','growth_maturity'))













plots <- names(which(table(lb_info$key) > 1)) # unique(lb_info$key)
last_picture <- 1:length(plots) |>
  purrr::map(.f = function(i) {
    lb_info_flt <- lb_info[lb_info$key == plots[i],]
    lb_info_flt <- lb_info_flt |> dplyr::arrange(date)
    return(lb_info_flt$filename[nrow(lb_info_flt)])
  }) |> unlist()

mrg |>
  dplyr::filter(label == 1 & filename %in% last_picture) |> # extent > 0
  ggplot2::ggplot(aes(x = extent, y = log1p(probability_class_1))) +
  ggplot2::geom_point(alpha = 0.1) +
  ggplot2::geom_smooth(method = lm, se = T) +
  ggplot2::theme_minimal()

apply(mrg, 2, is.character)

mrg |>
  ggplot2::ggplot(aes(x = doy, y = probability_class_1)) +
  ggplot2::geom_point(alpha = 0.05) +
  #ggplot2::geom_smooth(size = 0.8, se = T, span = 0.2) +
  ggplot2::xlab('DOY') +
  ggplot2::ylab('Loss probability') + 
  ggplot2::theme_minimal() +
  ggplot2::theme(legend.position = 'none')

mrg[,c("probability_class_1","drought_probability","drought_extent",
       "growth_sowing","growth_vegetative","growth_flowering","growth_maturity",
       "extent","doy")] |>
  dplyr::filter(probability_class_1 > 0.9) |>
  cor(use = 'pairwise.complete.obs', method = 'spearman')

mrg$growth_status <- mrg %>%
  dplyr::select(growth_sowing:growth_maturity) %>%
  apply(., 1, function(x) {
    if (is.na(any(x))) {
      return(NA)
    } else {
      names(which.max(x))
    }
  }) %>% unlist()
mrg$growth_status <- factor(x = mrg$growth_status,
                            levels = c('growth_sowing','growth_vegetative','growth_flowering','growth_maturity'))

mrg |>
  dplyr::filter(probability_class_1 > 0.7) |>
  ggplot2::ggplot(aes(x = growth_status, y = probability_class_1)) +
  ggplot2::geom_boxplot() +
  ggplot2::geom_jitter(alpha = 0.05) +
  ggplot2::theme_minimal()

tst <- mrg |> dplyr::filter(filename %in% last_picture)
caret::confusionMatrix(table(tst$label, tst$prediction), mode = "everything", positive = '1')
caret::confusionMatrix(table(mrg$label, mrg$prediction), mode = "everything", positive = '1')
caret::confusionMatrix(table(mrg$label[!(mrg$key %in% tst$key)], mrg$prediction[!(mrg$key %in% tst$key)]), mode = "everything", positive = '1')
caret::confusionMatrix(table(tst$label[tst$status == 'training'],
                             tst$prediction[tst$status == 'training']),
                       mode = 'everything', positive = '1')
caret::confusionMatrix(table(tst$label[tst$status == 'validation'],
                             tst$prediction[tst$status == 'validation']),
                       mode = 'everything', positive = '1')

0.7717 # end_season
0.7501 # all

mrg |>
  dplyr::filter(label == '1' & prediction == '1' & status == 'validation') |>
  dplyr::pull(extent)

pacman::p_load(MLmetrics)
MLmetrics::F1_Score(y_true = mrg$label, y_pred = mrg$prediction, positive = '1')
MLmetrics::F1_Score(y_true = tst$label, y_pred = tst$prediction, positive = '1')

