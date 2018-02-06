#'
#' # Unsupervised Learning
#'
#+ unsupervised_bookmark, include = FALSE
# unsuperivsed ####

#'
#'
#' ## PCA
#'
#+ pca_all
d_model_no_y <- dplyr::select(d_model, one_of(c(names_x_all)))
pca_all <- prcomp(d_model_no_y, scale = TRUE)
pca_all$center
pca_all$rotation
pca_all_var <- pca_all$sdev^2
pca_all_pve <- pca_all_var / sum(pca_all_var)
pca_all_pve

#'
#+ pca_all_plot
plot(
  cumsum(pca_all_pve), xlab = "Principal Component",
  ylab = "Proportion of Variance Explained",
  ylim = c(0, 1), type = "b"
)

#'
#' ## K-Means Clustering
#'
#+ kmeans_all_fit_full
set.seed(42)
# kmeans() can deal with both categorical and continuous response variables.
# Normally, need to set nstart to something like 20, 50, or 100 in order
# to avoid local optimums. However, there is no difference if limiting
# the number of centers to 2.
kmeans_all_fit_full <- kmeans(d_model_no_y, centers = 2, nstart = 100)
mean((kmeans_all_fit_full$cluster - 1) == d_model$result)
plot(d_model_no_y, col = (kmeans_all_fit_full$cluster + 1),
     main = "K-Means Clustering Results with K = 2", pch = 20, cex = 2)

#'
#' ## Heirarchal Clustering
#'
#+ hclust_all_fit_full
# Different methods could be used.
hclust_all_fit_full <- hclust(dist(d_model_no_y), method = "complete")
plot(hclust_all_fit_full, main = "Complete", sub = "")

# Cut to number of levels in categorical response variable.
hclust_all_fit_full_cut <- cutree(hclust_all_fit_full, 2)
