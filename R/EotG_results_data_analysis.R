options(warn = -1, scipen = 999)
library(pacman)
pacman::p_load(caret, gtools, tidyverse, nestedcv, plotly, yardstick, MetBrewer, ranger, pdp)
firstup <- function(x) {
  substr(x, 1, 1) <- toupper(substr(x, 1, 1))
  return(x)
} # First capital letter function
oneback <- function(x) {
  myDir <- unlist(strsplit(x, '/'))
  paste0(myDir[-length(myDir)], collapse = '/')
} # One directory back function
gg_pars <- ggplot2::theme(legend.position = 'bottom',
                          axis.text.x     = element_text(size = 14, colour = 'black'),
                          axis.text.y     = element_text(size = 14, colour = 'black'),
                          axis.title      = element_text(size = 15, colour = 'black'),
                          legend.title    = element_text(size = 15),
                          legend.text     = element_text(size = 14),
                          strip.text      = element_text(size = 14), 
                          axis.line       = element_blank(),
                          axis.ticks      = element_blank())

wd <- 'D:/OneDrive - CGIAR/PhD/papers/paper2/results'
cv <- c('convnext_tiny','densenet121','efficientnet_b3','resnet18','resnet50') # computer vision models

# Default metrics ----
dmt <- 1:length(cv) |>
  purrr::map(.f = function(i) {
    cat('Loading training and validation metrics for:',cv[i],'...\n')
    tr <- list.files(path = file.path(wd, paste0('results_',cv[i])), pattern = '^train', full.names = T) # train results by iteration
    tr <- tr[grep(pattern = '_epoch_', x = tr)] |> gtools::mixedsort()
    vr <- list.files(path = file.path(wd, paste0('results_',cv[i])), pattern = '^val', full.names = T) # validation results by iteration
    vr <- vr[grep(pattern = '_epoch_', x = vr)] |> gtools::mixedsort()
    epochs <- 1:length(tr)
    cat(' The model:',cv[i],'has',length(epochs),'epochs.\n')
    mt <- epochs |>
      purrr::map(.f = function(epoch) {
        cat('  Getting confusion matrix metrics for epoch:',epoch,'.\n')
        tp <- utils::read.csv(tr[epoch]) # training predictions
        # True-binarized label - training dataset
        tp$basic_label    <- dplyr::case_when(tp$true_label %in% c(1,2) ~ 0,
                                              tp$true_label == 0 ~ 1)
        tp$moderate_label <- dplyr::case_when(tp$true_label %in% c(0,2) ~ 0,
                                              tp$true_label == 1 ~ 1)
        tp$superior_label <- dplyr::case_when(tp$true_label %in% c(0,1) ~ 0,
                                              tp$true_label == 2 ~ 1)
        # Binarized predictions - training dataset
        tp$basic_pred <- dplyr::case_when(tp$predicted_label %in% c(1,2) ~ 0,
                                          tp$predicted_label == 0 ~ 1)
        tp$moderate_pred <- dplyr::case_when(tp$predicted_label %in% c(0,2) ~ 0,
                                             tp$predicted_label == 1 ~ 1)
        tp$superior_pred <- dplyr::case_when(tp$predicted_label %in% c(0,1) ~ 0,
                                             tp$predicted_label == 2 ~ 1)
        vp <- utils::read.csv(vr[epoch]) # validation predictions
        # True-binarized label - validation dataset
        vp$basic_label    <- dplyr::case_when(vp$true_label %in% c(1,2) ~ 0,
                                              vp$true_label == 0 ~ 1)
        vp$moderate_label <- dplyr::case_when(vp$true_label %in% c(0,2) ~ 0,
                                              vp$true_label == 1 ~ 1)
        vp$superior_label <- dplyr::case_when(vp$true_label %in% c(0,1) ~ 0,
                                              vp$true_label == 2 ~ 1)
        # Binarized predictions - validation dataset
        vp$basic_pred <- dplyr::case_when(vp$predicted_label %in% c(1,2) ~ 0,
                                          vp$predicted_label == 0 ~ 1)
        vp$moderate_pred <- dplyr::case_when(vp$predicted_label %in% c(0,2) ~ 0,
                                             vp$predicted_label == 1 ~ 1)
        vp$superior_pred <- dplyr::case_when(vp$predicted_label %in% c(0,1) ~ 0,
                                             vp$predicted_label == 2 ~ 1)
        tc <- caret::confusionMatrix(data = factor(tp$predicted_label), reference = factor(tp$true_label), mode = 'everything') # confusion matrix for training data
        vc <- caret::confusionMatrix(data = factor(vp$predicted_label), reference = factor(vp$true_label), mode = 'everything') # confusion matrix for validation data
        tc <- base::as.data.frame(t(tc$byClass))
        mcc_t_basic <- tryCatch(expr = {
          mcc_t_basic <- tp |> dplyr::select(basic_label, basic_pred) |> dplyr::mutate(basic_label = factor(basic_label), basic_pred = factor(basic_pred)) |> yardstick::mcc(basic_label, basic_pred) |> dplyr::pull(.estimate)
        }, error = function(msg) {
          return(NA)
        })
        mcc_t_moderate <- tryCatch(expr = {
          mcc_t_moderate <- tp |> dplyr::select(moderate_label, moderate_pred) |> dplyr::mutate(moderate_label = factor(moderate_label), moderate_pred = factor(moderate_pred)) |> yardstick::mcc(moderate_label, moderate_pred) |> dplyr::pull(.estimate)
        }, error = function(msg) {
          return(NA)
        })
        mcc_t_superior <- tryCatch(expr = {
          mcc_t_superior <- tp |> dplyr::select(superior_label, superior_pred) |> dplyr::mutate(superior_label = factor(superior_label), superior_pred = factor(superior_pred)) |> yardstick::mcc(superior_label, superior_pred) |> dplyr::pull(.estimate)
        }, error = function(msg) {
          return(NA)
        })
        mcc_t_dfm <- data.frame(mcc_t_basic, mcc_t_moderate, mcc_t_superior); rm(mcc_t_basic, mcc_t_moderate, mcc_t_superior)
        rownames(mcc_t_dfm) <- 'MCC'
        colnames(mcc_t_dfm) <- colnames(tc)
        tc <- rbind(tc, mcc_t_dfm); rm(mcc_t_dfm)
        tc$metric <- rownames(tc); rownames(tc) <- 1:nrow(tc)
        tc <- tc |>
          tidyr::pivot_longer(cols = 1:3, names_to = 'loss', values_to = 'value') |>
          base::as.data.frame()
        mcc_t <- tryCatch(expr = {
          mcc_t <- tp |> dplyr::select(true_label, predicted_label) |> dplyr::mutate(true_label = factor(true_label), predicted_label = factor(predicted_label)) |> yardstick::mcc(true_label, predicted_label) |> dplyr::pull(.estimate)
        }, error = function(msg) {
          return(NA)
        })
        tc <- rbind(tc, data.frame(metric = 'MCC', loss = 'all', value = mcc_t)); rm(mcc_t)
        tc$epoch <- epoch
        tc$dataset <- 'training'
        vc <- base::as.data.frame(t(vc$byClass))
        mcc_v_basic <- tryCatch(expr = {
          mcc_v_basic <- vp |> dplyr::select(basic_label, basic_pred) |> dplyr::mutate(basic_label = factor(basic_label), basic_pred = factor(basic_pred)) |> yardstick::mcc(basic_label, basic_pred) |> dplyr::pull(.estimate)
        }, error = function(msg) {
          return(NA)
        })
        mcc_v_moderate <- tryCatch(expr = {
          mcc_v_moderate <- vp |> dplyr::select(moderate_label, moderate_pred) |> dplyr::mutate(moderate_label = factor(moderate_label), moderate_pred = factor(moderate_pred)) |> yardstick::mcc(moderate_label, moderate_pred) |> dplyr::pull(.estimate)
        }, error = function(msg) {
          return(NA)
        })
        mcc_v_superior <- tryCatch(expr = {
          mcc_v_superior <- vp |> dplyr::select(superior_label, superior_pred) |> dplyr::mutate(superior_label = factor(superior_label), superior_pred = factor(superior_pred)) |> yardstick::mcc(superior_label, superior_pred) |> dplyr::pull(.estimate)
        }, error = function(msg) {
          return(NA)
        })
        mcc_v_dfm <- data.frame(mcc_v_basic, mcc_v_moderate, mcc_v_superior); rm(mcc_v_basic, mcc_v_moderate, mcc_v_superior)
        rownames(mcc_v_dfm) <- 'MCC'
        colnames(mcc_v_dfm) <- colnames(vc)
        vc <- rbind(vc, mcc_v_dfm); rm(mcc_v_dfm)
        vc$metric <- rownames(vc); rownames(vc) <- 1:nrow(vc)
        vc <- vc |>
          tidyr::pivot_longer(cols = 1:3, names_to = 'loss', values_to = 'value') |>
          base::as.data.frame()
        mcc_v <- tryCatch(expr = {
          mcc_v <- vp |> dplyr::select(true_label, predicted_label) |> dplyr::mutate(true_label = factor(true_label), predicted_label = factor(predicted_label)) |> yardstick::mcc(true_label, predicted_label) |> dplyr::pull(.estimate)
        }, error = function(msg) {
          return(NA)
        })
        vc <- rbind(vc, data.frame(metric = 'MCC', loss = 'all', value = mcc_v)); rm(mcc_v)
        vc$epoch <- epoch
        vc$dataset <- 'validation'
        mt <- rbind(tc, vc)
        return(mt)
      }) |> dplyr::bind_rows()
    mt$model <- cv[i]
    return(mt)
  }) |> dplyr::bind_rows()
dmt$dataset <- firstup(dmt$dataset)
dmt$model <- dplyr::case_when(dmt$model == 'convnext_tiny' ~ 'ConvNeXt tiny',
                              dmt$model == 'densenet121' ~ 'DenseNet 121',
                              dmt$model == 'efficientnet_b3' ~ 'EfficientNet B3',
                              dmt$model == 'resnet18' ~ 'ResNet18',
                              dmt$model == 'resnet50' ~ 'ResNet50')
dmt$loss <- dplyr::case_when(dmt$loss == 'Class: 0' ~ 'Basic',
                             dmt$loss == 'Class: 1' ~ 'Moderate',
                             dmt$loss == 'Class: 2' ~ 'Superior',
                             dmt$loss == 'all' ~ 'all')

# MCC during the last epoch by model
mean(c(0.4850178,0.4814815,0.3631589,0.4844033,0.4372860))

# Model performance
plt <- MetBrewer::met.brewer(name = 'Egypt', n = 2, type = 'discrete')
gg <- dmt |>
  dplyr::filter(metric == 'MCC' & loss == 'all') |>
  ggplot2::ggplot(aes(x = epoch, y = value, colour = dataset)) +
  ggplot2::geom_line(alpha = 1, size = 0.9) +
  ggplot2::facet_wrap(~model, ncol = 5, scales = 'free_x') +
  ggplot2::xlab('Epoch') +
  ggplot2::ylab('Matthews Correlation Coefficient') +
  ggplot2::labs(colour = 'Dataset') +
  ggplot2::scale_colour_manual(values = plt) +
  ggplot2::theme_bw() +
  gg_pars; gg
outfile <- file.path(oneback(wd),'graphs/paper2_fig1_general_performance.png')
dir.create(dirname(outfile))
ggplot2::ggsave(filename = outfile, plot = gg, device = 'png', width = 10, height = 5, units = 'in', dpi = 350)
gp <- plotly::ggplotly(gg)
gp

# Class performance
plt <- MetBrewer::met.brewer(name = 'Egypt', n = 3, type = 'discrete')
gg <- dmt |>
  dplyr::filter(metric == 'MCC' & loss != 'all') |>
  dplyr::mutate(loss = factor(loss, levels = c('Basic','Moderate','Superior'), labels = c('Small','Medium','Large'))) |>
  ggplot2::ggplot(aes(x = epoch, y = value, colour = loss)) +
  ggplot2::geom_line(alpha = 1, size = 0.9) +
  ggplot2::facet_grid(dataset~model, scales = 'free_x') +
  ggplot2::xlab('Epoch') +
  ggplot2::ylab('Matthews Correlation Coefficient') +
  ggplot2::labs(colour = 'Loss level') +
  ggplot2::scale_colour_manual(values = plt) +
  ggplot2::theme_bw() +
  gg_pars; gg
outfile <- file.path(oneback(wd),'graphs/paper2_fig2_classes_performance.png')
dir.create(dirname(outfile))
ggplot2::ggsave(filename = outfile, plot = gg, device = 'png', width = 10, height = 6, units = 'in', dpi = 350)
gp <- plotly::ggplotly(gg)
gp

# Predictors' importance (partial dependency plots)
stp <- data.frame(model = cv, epochs = c(22,16,24,15,29))
pdp_vls <- 1:nrow(stp) |>
  purrr::map(.f = function(i) {
    cat('Loading training and validation predictions for:',cv[i],'...\n')
    tr <- list.files(path = file.path(wd, paste0('results_',cv[i])), pattern = '^train', full.names = T) # train results by iteration
    tr <- tr[grep(pattern = '_epoch_', x = tr)] |> gtools::mixedsort()
    vr <- list.files(path = file.path(wd, paste0('results_',cv[i])), pattern = '^val', full.names = T) # validation results by iteration
    vr <- vr[grep(pattern = '_epoch_', x = vr)] |> gtools::mixedsort()
    tp <- utils::read.csv(tr[stp$epochs[i]]); rm(tr) # training predictions
    vp <- utils::read.csv(vr[stp$epochs[i]]); rm(vr) # validation predictions
    dfm <- rbind(tp, vp)
    cat('Fitting Basic models... \n')
    set.seed(1235)
    partitions <- caret::createDataPartition(y = dfm$basic_probability, times = 20, p = 0.2, list = T)
    bas_pdp <- 1:length(partitions) |>
      purrr::map(.f = function(j) {
        sdfm <- dfm[partitions[[j]],]
        model_caret <- caret::train(basic_probability ~ days_after_sowing + day_of_year,
                                    data       = sdfm,
                                    method     = 'ranger',
                                    trControl  = trainControl(method = 'cv', number = 5, verboseIter = F),
                                    num.trees  = 500,
                                    importance = 'impurity_corrected')
        pdp_dfm_DAS <- pdp::partial(model_caret$finalModel, pred.var = c('days_after_sowing'), train = sdfm) |> base::as.data.frame()
        pdp_dfm_DAS <- pdp_dfm_DAS |> tidyr::pivot_longer(cols = 1, names_to = 'predictor', values_to = 'value') |> base::as.data.frame()
        pdp_dfm_DOY <- pdp::partial(model_caret$finalModel, pred.var = c('day_of_year'), train = sdfm) |> base::as.data.frame()
        pdp_dfm_DOY <- pdp_dfm_DOY |> tidyr::pivot_longer(cols = 1, names_to = 'predictor', values_to = 'value') |> base::as.data.frame()
        pdp_dfm <- rbind(pdp_dfm_DAS, pdp_dfm_DOY); rm(pdp_dfm_DAS, pdp_dfm_DOY)
        pdp_dfm$Resample <- j
        return(pdp_dfm)
      }) |> dplyr::bind_rows()
    bas_pdp$loss <- 'Basic'
    cat('Fitting Moderate models... \n')
    set.seed(1235)
    partitions <- caret::createDataPartition(y = dfm$moderate_probability, times = 20, p = 0.2, list = T)
    mod_pdp <- 1:length(partitions) |>
      purrr::map(.f = function(j) {
        sdfm <- dfm[partitions[[j]],]
        model_caret <- caret::train(moderate_probability ~ days_after_sowing + day_of_year,
                                    data       = sdfm,
                                    method     = 'ranger',
                                    trControl  = trainControl(method = 'cv', number = 5, verboseIter = F),
                                    num.trees  = 500,
                                    importance = 'impurity_corrected')
        pdp_dfm_DAS <- pdp::partial(model_caret$finalModel, pred.var = c('days_after_sowing'), train = sdfm) |> base::as.data.frame()
        pdp_dfm_DAS <- pdp_dfm_DAS |> tidyr::pivot_longer(cols = 1, names_to = 'predictor', values_to = 'value') |> base::as.data.frame()
        pdp_dfm_DOY <- pdp::partial(model_caret$finalModel, pred.var = c('day_of_year'), train = sdfm) |> base::as.data.frame()
        pdp_dfm_DOY <- pdp_dfm_DOY |> tidyr::pivot_longer(cols = 1, names_to = 'predictor', values_to = 'value') |> base::as.data.frame()
        pdp_dfm <- rbind(pdp_dfm_DAS, pdp_dfm_DOY); rm(pdp_dfm_DAS, pdp_dfm_DOY)
        pdp_dfm$Resample <- j
        return(pdp_dfm)
      }) |> dplyr::bind_rows()
    mod_pdp$loss <- 'Moderate'
    cat('Fitting Superior models... \n')
    set.seed(1235)
    partitions <- caret::createDataPartition(y = dfm$superior_probability, times = 20, p = 0.2, list = T)
    sup_pdp <- 1:length(partitions) |>
      purrr::map(.f = function(j) {
        sdfm <- dfm[partitions[[j]],]
        model_caret <- caret::train(superior_probability ~ days_after_sowing + day_of_year,
                                    data       = sdfm,
                                    method     = 'ranger',
                                    trControl  = trainControl(method = 'cv', number = 5, verboseIter = F),
                                    num.trees  = 500,
                                    importance = 'impurity_corrected')
        pdp_dfm_DAS <- pdp::partial(model_caret$finalModel, pred.var = c('days_after_sowing'), train = sdfm) |> base::as.data.frame()
        pdp_dfm_DAS <- pdp_dfm_DAS |> tidyr::pivot_longer(cols = 1, names_to = 'predictor', values_to = 'value') |> base::as.data.frame()
        pdp_dfm_DOY <- pdp::partial(model_caret$finalModel, pred.var = c('day_of_year'), train = sdfm) |> base::as.data.frame()
        pdp_dfm_DOY <- pdp_dfm_DOY |> tidyr::pivot_longer(cols = 1, names_to = 'predictor', values_to = 'value') |> base::as.data.frame()
        pdp_dfm <- rbind(pdp_dfm_DAS, pdp_dfm_DOY); rm(pdp_dfm_DAS, pdp_dfm_DOY)
        pdp_dfm$Resample <- j
        return(pdp_dfm)
      }) |> dplyr::bind_rows()
    sup_pdp$loss <- 'Superior'
    pdps <- dplyr::bind_rows(bas_pdp, mod_pdp, sup_pdp); rm(bas_pdp, mod_pdp, sup_pdp)
    pdps$model <- cv[i]
    return(pdps)
  }) |> dplyr::bind_rows()
utils::write.csv(x = pdp_vls, file = file.path(oneback(wd),'pdp_predictors.csv'), row.names = F)
pdp_vls <- utils::read.csv(file.path(oneback(wd),'pdp_predictors.csv'))
pdp_vls$model <- dplyr::case_when(pdp_vls$model == 'convnext_tiny' ~ 'ConvNeXt tiny',
                                  pdp_vls$model == 'densenet121' ~ 'DenseNet 121',
                                  pdp_vls$model == 'efficientnet_b3' ~ 'EfficientNet B3',
                                  pdp_vls$model == 'resnet18' ~ 'ResNet18',
                                  pdp_vls$model == 'resnet50' ~ 'ResNet50')

# Days after sowing partial dependence plot
gg <- pdp_vls |>
  dplyr::filter(predictor == 'days_after_sowing') |>
  dplyr::mutate(value = value * (206-1) + 1) |>
  dplyr::mutate(loss = factor(loss, levels = c('Basic','Moderate','Superior'), labels = c('Small','Medium','Large'))) |>
  ggplot2::ggplot(aes(x = value, y = yhat, colour = Resample, group = Resample)) +
  ggplot2::geom_line(alpha = 0.2) +
  ggplot2::facet_grid(loss ~ model, scales = 'free_y') +
  ggplot2::xlab('DAS') +
  ggplot2::ylab('Probability') +
  ggplot2::theme_bw() +
  gg_pars +
  ggplot2::theme(legend.position = 'none')
outfile <- file.path(oneback(wd),'graphs/paper2_fig3_das_vs_probability.png')
dir.create(dirname(outfile))
ggplot2::ggsave(filename = outfile, plot = gg, device = 'png', width = 10, height = 7, units = 'in', dpi = 350)
gp <- plotly::ggplotly(gg)
gp

# Day of the year partial dependence plot
gg <- pdp_vls |>
  dplyr::filter(predictor == 'day_of_year') |>
  dplyr::mutate(value = value * (366-1) + 1) |>
  dplyr::mutate(loss = factor(loss, levels = c('Basic','Moderate','Superior'), labels = c('Small','Medium','Large'))) |>
  ggplot2::ggplot(aes(x = value, y = yhat, colour = Resample, group = Resample)) +
  ggplot2::geom_line(alpha = 0.2) +
  ggplot2::facet_grid(loss ~ model, scales = 'free_y') +
  ggplot2::xlab('DOY') +
  ggplot2::ylab('Probability') +
  ggplot2::theme_bw() +
  gg_pars +
  ggplot2::theme(legend.position = 'none')
outfile <- file.path(oneback(wd),'graphs/paper2_fig3_doy_vs_probability.png')
dir.create(dirname(outfile))
ggplot2::ggsave(filename = outfile, plot = gg, device = 'png', width = 10, height = 7, units = 'in', dpi = 350)
gp <- plotly::ggplotly(gg)
gp

dmt |>
  dplyr::filter(metric == 'Recall' & dataset == 'Validation') |>
  dplyr::filter((model == 'ConvNeXt tiny' & epoch == 22) |
                  (model == 'DenseNet 121' & epoch == 16) |
                  (model == 'EfficientNet B3' & epoch == 24) |
                  (model == 'ResNet18' & epoch == 15) |
                  (model == 'ResNet50' & epoch == 29)
                ) |>
  dplyr::group_by(loss) |>
  dplyr::summarise(value = mean(value)) |>
  dplyr::ungroup() |>
  base::as.data.frame()

stp <- data.frame(model = cv, epochs = c(22,16,24,15,29))
stp$model <- dplyr::case_when(stp$model == 'convnext_tiny' ~ 'ConvNeXt tiny',
                              stp$model == 'densenet121' ~ 'DenseNet 121',
                              stp$model == 'efficientnet_b3' ~ 'EfficientNet B3',
                              stp$model == 'resnet18' ~ 'ResNet18',
                              stp$model == 'resnet50' ~ 'ResNet50')
recall_by_loss_level <- dmt |>
  dplyr::filter(
    (model == 'ConvNeXt tiny' & epoch == 22) |
      (model == 'DenseNet 121' & epoch == 16) |
      (model == 'EfficientNet B3' & epoch == 24) |
      (model == 'ResNet18' & epoch == 15) |
      (model == 'ResNet50' & epoch == 29)
  ) |>
  dplyr::filter(metric == 'Recall' & dataset == 'Validation') |>
  dplyr::select(loss, value, model) |>
  tidyr::pivot_wider(names_from = model, values_from = value) |>
  base::as.data.frame()

recall_by_loss_level$Average <- apply(recall_by_loss_level[,-1], 1, mean)
recall_by_loss_level$`ConvNeXt tiny` <- round(recall_by_loss_level$`ConvNeXt tiny`, 3)
recall_by_loss_level$`DenseNet 121` <- round(recall_by_loss_level$`DenseNet 121`, 3)
recall_by_loss_level$`EfficientNet B3` <- round(recall_by_loss_level$`EfficientNet B3`, 3)
recall_by_loss_level$ResNet18 <- round(recall_by_loss_level$ResNet18, 3)
recall_by_loss_level$ResNet50 <- round(recall_by_loss_level$ResNet50, 3)
recall_by_loss_level$Average <- round(recall_by_loss_level$Average, 3)










pdp_dfm |>
  ggplot2::ggplot(aes(x = days_after_sowing * (206-1) + 1, y = yhat, colour = Resample, group = Resample)) +
  ggplot2::geom_line() +
  ggplot2::xlab('DAS') +
  ggplot2::ylab('Superior loss probability') +
  ggplot2::theme_bw()

rf_fit <- ranger::ranger(formula = superior_probability ~ days_after_sowing + day_of_year, data = dfm, importance = 'impurity_corrected') # [dfm$predicted_label == 2,]
rf_fit
partial(rf_fit, pred.var = 'days_after_sowing', train = dfm) |> # [dfm$predicted_label == 2,]
  plot(rug = T, train = dfm) # [dfm$predicted_label == 2,]
partial(rf_fit, pred.var = c('days_after_sowing','day_of_year'), train = dfm[dfm$predicted_label == 2,]) |>
  plot(rug = T, train = dfm[dfm$predicted_label == 2,])


partial(rf_fit, 
        pred.var = c('days_after_sowing', 'day_of_year'), 
        trim.outliers = T, chull = T, parallel = T,
        grid.resolution = 30, paropts = list(.packages = 'ranger'))

# Best threshold metrics ----
bmt <- 1:length(cv) |>
  purrr::map(.f = function(i) {
    cat('Loading training and validation metrics for:',cv[i],'...\n')
    tr <- list.files(path = file.path(wd, paste0('results_',cv[i])), pattern = '^train', full.names = T) # train results by iteration
    tr <- tr[grep(pattern = '_epoch_', x = tr)] |> gtools::mixedsort()
    vr <- list.files(path = file.path(wd, paste0('results_',cv[i])), pattern = '^val', full.names = T) # validation results by iteration
    vr <- vr[grep(pattern = '_epoch_', x = vr)] |> gtools::mixedsort()
    epochs <- 1:length(tr)
    cat(' The model:',cv[i],'has',length(epochs),'epochs.\n')
    mt <- epochs |>
      purrr::map(.f = function(epoch) {
        cat('  Getting confusion matrix metrics for epoch:',epoch,'.\n')
        tp <- utils::read.csv(tr[epoch]) # training predictions
        vp <- utils::read.csv(vr[epoch]) # validation predictions
        # Obtain metrics by threshold
        probs <- seq(0.01, 0.99, 0.01)
        metrics_by_thr <- 1:length(probs) |>
          purrr::map(.f = function(j) {
            trn <- tp
            val <- vp
            # True-binarized label - training dataset
            trn$basic_label    <- dplyr::case_when(trn$true_label %in% c(1,2) ~ 0,
                                                   trn$true_label == 0 ~ 1)
            trn$moderate_label <- dplyr::case_when(trn$true_label %in% c(0,2) ~ 0,
                                                   trn$true_label == 1 ~ 1)
            trn$superior_label <- dplyr::case_when(trn$true_label %in% c(0,1) ~ 0,
                                                   trn$true_label == 2 ~ 1)
            # True-binarized label - validation dataset
            val$basic_label    <- dplyr::case_when(val$true_label %in% c(1,2) ~ 0,
                                                   val$true_label == 0 ~ 1)
            val$moderate_label <- dplyr::case_when(val$true_label %in% c(0,2) ~ 0,
                                                   val$true_label == 1 ~ 1)
            val$superior_label <- dplyr::case_when(val$true_label %in% c(0,1) ~ 0,
                                                   val$true_label == 2 ~ 1)
            # Binarized predictions - training dataset
            trn$basic_pred <- as.numeric(trn$basic_probability > probs[j])
            trn$moderate_pred <- as.numeric(trn$moderate_probability > probs[j])
            trn$superior_pred <- as.numeric(trn$superior_probability > probs[j])
            # Binarized predictions - validation dataset
            val$basic_pred <- as.numeric(val$basic_probability > probs[j])
            val$moderate_pred <- as.numeric(val$moderate_probability > probs[j])
            val$superior_pred <- as.numeric(val$superior_probability > probs[j])
            # Confusion matrix "basic" - training dataset
            tcm_bs <- caret::confusionMatrix(data = factor(trn$basic_pred), reference = factor(trn$basic_label), positive  = '1')
            tmt_bs <- base::as.data.frame(t(tcm_bs$byClass)) # Training metrics basic
            tmt_bs$threshold <- probs[j]
            # Confusion matrix "moderate" - training dataset
            tcm_md <- caret::confusionMatrix(data = factor(trn$moderate_pred), reference = factor(trn$moderate_label), positive  = '1')
            tmt_md <- base::as.data.frame(t(tcm_md$byClass)) # Training metrics moderate
            tmt_md$threshold <- probs[j]
            # Confusion matrix "superior" - training dataset
            tcm_sp <- caret::confusionMatrix(data = factor(trn$superior_pred), reference = factor(trn$superior_label), positive  = '1')
            tmt_sp <- base::as.data.frame(t(tcm_sp$byClass)) # Training metrics superior
            tmt_sp$threshold <- probs[j]
            # Confusion matrix "basic" - validation dataset
            vcm_bs <- caret::confusionMatrix(data = factor(val$basic_pred), reference = factor(val$basic_label), positive  = '1')
            vmt_bs <- base::as.data.frame(t(vcm_bs$byClass)) # Validation metrics basic
            vmt_bs$threshold <- probs[j]
            # Confusion matrix "moderate" - training dataset
            vcm_md <- caret::confusionMatrix(data = factor(val$moderate_pred), reference = factor(val$moderate_label), positive  = '1')
            vmt_md <- base::as.data.frame(t(vcm_md$byClass)) # Training metrics moderate
            vmt_md$threshold <- probs[j]
            # Confusion matrix "superior" - training dataset
            vcm_sp <- caret::confusionMatrix(data = factor(val$superior_pred), reference = factor(val$superior_label), positive  = '1')
            vmt_sp <- base::as.data.frame(t(vcm_sp$byClass)) # Training metrics superior
            vmt_sp$threshold <- probs[j]
            # ID's
            tmt_bs$loss <- 'basic'; tmt_bs$dataset <- 'training'
            tmt_md$loss <- 'moderate'; tmt_md$dataset <- 'training'
            tmt_sp$loss <- 'superior'; tmt_sp$dataset <- 'training'
            vmt_bs$loss <- 'basic'; vmt_bs$dataset <- 'validation'
            vmt_md$loss <- 'moderate'; vmt_md$dataset <- 'validation'
            vmt_sp$loss <- 'superior'; vmt_sp$dataset <- 'validation'
            fmt <- dplyr::bind_rows(tmt_bs, tmt_md, tmt_sp, vmt_bs, vmt_md, vmt_sp)
            return(fmt)
          }) |> dplyr::bind_rows()
        metrics_by_thr$epoch <- epoch
        metrics_by_thr$model <- cv[i]
        return(metrics_by_thr)
      }) |> dplyr::bind_rows()
    return(mt)
  }) |> dplyr::bind_rows()

gg <- bmt |>
  dplyr::filter(model == 'convnext_tiny') |>
  ggplot2::ggplot(aes(x = threshold, y = `F1`, colour = epoch)) +
  ggplot2::geom_line() +
  ggplot2::facet_grid(dataset ~ loss) +
  ggplot2::theme_bw()
gp <- plotly::ggplotly(gg)
gp


dfm <- utils::read.csv('D:/OneDrive - CGIAR/PhD/papers/paper2/results/results_convnext_tiny/train_predictions_epoch_21.csv')
probs <- seq(0.01, 0.99, 0.01)
thr_test <- 1:length(probs) |>
  purrr::map(.f = function(i) {
    tst <- dfm
    tst$superior <- as.numeric(dfm$basic_probability > probs[i])
    tst$true_label_superior <- tst$true_label
    tst$true_label_superior[tst$true_label == 0] <- 3
    tst$true_label_superior[tst$true_label %in% c(1,2)] <- 0
    tst$true_label_superior[tst$true_label_superior == 3] <- 1
    cm <- caret::confusionMatrix(data      = factor(tst$superior),
                                 reference = factor(tst$true_label_superior),
                                 positive  = '1')
    vc <- base::as.data.frame(t(cm$byClass))
    vc$thr <- probs[i]
    return(vc)
  }) |> dplyr::bind_rows()
thr_test |>
  ggplot2::ggplot(aes(x = thr, y = F1)) +
  ggplot2::geom_line() +
  ggplot2::theme_bw()

nestedcv::mcc_multi(table(vp$true_label, vp$predicted_label))



# Visualize metrics
gg <- mt |>
  dplyr::filter(model == 'resnet50') |>
  dplyr::filter(!(metric %in% c('Detection Prevalence','Detection Rate','Prevalence'))) |>
  ggplot2::ggplot(aes(x = epoch, y = value, colour = loss)) +
  ggplot2::geom_line() +
  ggplot2::facet_grid(dataset ~ metric, scales = 'free_y') +
  ggplot2::theme_bw() +
  ggplot2::xlab('Iteration') +
  ggplot2::ylab('Metric value')
gp <- plotly::ggplotly(gg)
gp

MLmetrics::PRAUC(mt$)
