options(warn = -1, scipen = 999)
library(pacman)
pacman::p_load(rhdf5, SpatialPack, terra, corrplot, tidyverse, ggdist)
gg_pars <- ggplot2::theme(legend.position = 'bottom',
                          axis.text.x     = element_text(size = 14, colour = 'black'),
                          axis.text.y     = element_text(size = 14, colour = 'black'),
                          axis.title      = element_text(size = 15, colour = 'black'),
                          legend.title    = element_text(size = 15),
                          legend.text     = element_text(size = 14),
                          strip.text      = element_text(size = 14), 
                          axis.line       = element_blank(),
                          axis.ticks      = element_blank())

root <- 'D:/OneDrive - CGIAR/PhD/papers/paper2/results'

fle <- read.csv('D:/pbi_kenya/val_features.csv')
fle <- strsplit(x = fle$relative_path, split = '.', fixed = T) |> purrr::map(1) |> unlist()
cnn <- c('convnext_tiny','densenet121','efficientnet_b3','resnet18','resnet50')
outfile <- 'D:/OneDrive - CGIAR/PhD/papers/paper2/heatmaps_correlations.csv'
if (!file.exists(outfile)) {
  heatmaps_comparison <- purrr::map(.x = fle, .f = function(fl){
    
    # Read heatmap matrices
    fls <- paste0(root,'/matrices-',cnn,'/',fl,'.h5')
    htm <- fls |> purrr::map(.f = rhdf5::h5read, name = 'heatmap')
    
    # Heatmap comparisons
    cnn_comparisons <- matrix(data = NA, nrow = length(cnn), ncol = length(cnn), byrow = T)
    for (i in seq_along(cnn)) {
      for (j in seq_along(cnn)) {
        cnn_comparisons[i, j] <- cor(x = as.vector(htm[[i]]), y = as.vector(htm[[j]]))
        # cnn_comparisons[i, j] <- SpatialPack::SSIM(htm[[i]], htm[[j]])$SSIM
      }
    }
    rownames(cnn_comparisons) <- colnames(cnn_comparisons) <- cnn
    
    # corrplot::corrplot(cnn_comparisons, is.corr = T)
    
    lw_id <- lower.tri(cnn_comparisons)
    cnn_comparisons[lw_id] <- NA; rm(lw_id)
    diag(cnn_comparisons) <- NA
    
    cnn_comparisons <- as.data.frame(as.table(cnn_comparisons))
    colnames(cnn_comparisons) <- c('CNN1','CNN2','Corr')
    cnn_comparisons <- cnn_comparisons[!is.na(cnn_comparisons$SSIM),]
    rownames(cnn_comparisons) <- 1:nrow(cnn_comparisons)
    cnn_comparisons$file <- fl
    
    return(cnn_comparisons)
    
  }) |> dplyr::bind_rows()
  heatmaps_comparison$comparison <- paste0(heatmaps_comparison$CNN1, '-', heatmaps_comparison$CNN2)
  write.csv(heatmaps_comparison, outfile, row.names = F)
} else {
  heatmaps_comparison <- read.csv(outfile)
}
names(heatmaps_comparison)[3] <- 'Corr'
heatmaps_comparison$comparison <- factor(x = heatmaps_comparison$comparison)
levels(heatmaps_comparison$comparison) <- c("ConvNeXt tiny - DenseNet 121", "ConvNeXt tiny - EfficientNet B3",
                                            "ConvNeXt tiny - ResNet18", "ConvNeXt tiny - ResNet50",
                                            "DenseNet 121 - EfficientNet B3", "DenseNet121 - ResNet18",
                                            "DenseNet 121 - ResNet50", "EfficientNet B3 - ResNet18",
                                            "EfficientNet B3 - ResNet50", "ResNet18 - ResNet50")

# heatmaps_comparison$check <- NA
# heatmaps_comparison$check[heatmaps_comparison$file %in% verdaderas] <- 'red'
# heatmaps_comparison$check[!(heatmaps_comparison$file %in% verdaderas)] <- 'black'
heatmaps_comparison |>
  # dplyr::filter(comparison == 'convnext_tiny-resnet18') |>
  ggplot2::ggplot(aes(x = reorder(comparison, -Corr), y = SSIM)) + # colour = check
  ggplot2::geom_violin() +
  ggplot2::geom_jitter(alpha = 0.05) +
  ggplot2::geom_hline(yintercept = 0.0, colour = 'red') +
  ggplot2::geom_hline(yintercept = 0.5, colour = 'green') +
  ggplot2::coord_flip() +
  ggplot2::theme_minimal()

median_corr <- median(heatmaps_comparison$Corr, na.rm = T)

p <- heatmaps_comparison |>
  ggplot2::ggplot(aes(x = reorder(comparison, Corr), y = Corr)) +
  ggdist::stat_halfeye(fill_type = 'segments', alpha = 0.3) +
  ggdist::stat_interval() +
  ggplot2::stat_summary(geom = 'point', fun = median) +
  ggplot2::geom_hline(yintercept = median_corr, col = 'grey30', lty = 'dashed') +
  ggplot2::scale_y_continuous(breaks = seq(-1, 1, 0.5)) +
  ggplot2::scale_color_manual(values = MetBrewer::met.brewer('Hokusai2')) + # VanGogh3
  ggplot2::coord_flip() +
  ggplot2::labs(col = 'Coverage range') +
  # ggplot2::guides(col = 'Level') +
  ggplot2::theme_minimal() +
  ggplot2::xlab('') +
  ggplot2::ylab('Pearson correlation') +
  gg_pars
p
ggplot2::ggsave(filename = 'D:/OneDrive - CGIAR/PhD/papers/paper2/graphs/paper2_fig4_heatmaps_correlations.png', plot = p, device = 'png', width = 7, height = 10, units = 'in', dpi = 350)
