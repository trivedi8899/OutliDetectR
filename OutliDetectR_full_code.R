# OutliDetectR - Combined Source Code
# Author: Ruchi Trivedi
# ------------------------------------

# ---- normalize_data.R ----
normalize_data <- function(data) {
  return(as.data.frame(scale(data)))
}

# ---- outlier_methods.R ----
outlier_mahalanobis <- function(data) {
  center <- colMeans(data, na.rm = TRUE)
  cov_matrix <- cov(data, use = "complete.obs")
  md <- mahalanobis(data, center, cov_matrix)
  return(md)
}

outlier_lof <- function(data, k = 5) {
  require(DMwR)
  return(DMwR::lofactor(data, k = k))
}

outlier_iforest <- function(data) {
  require(isotree)
  model <- isolation.forest(data)
  return(predict(model, data))
}

# ---- imputation_methods.R ----
impute_knn <- function(data) {
  require(VIM)
  return(VIM::kNN(data, k = 5))
}

impute_missforest <- function(data) {
  require(missForest)
  result <- missForest::missForest(data)
  return(result$ximp)
}

# ---- omv_score.R ----
omv_score <- function(data, alpha = 0.6, beta = 0.4) {
  data_norm <- normalize_data(data)
  out_md <- outlier_mahalanobis(data_norm)
  out_lof <- outlier_lof(data_norm)
  out_if <- outlier_iforest(data_norm)

  outlier_score <- scale(out_md + out_lof + out_if) / 3

  data_imp <- impute_knn(data)
  missing_impact <- rowSums(is.na(data)) / ncol(data)

  final_score <- alpha * outlier_score + beta * missing_impact
  flag <- ifelse(final_score > mean(final_score) + 2 * sd(final_score), 1, 0)

  return(data.frame(OMV_Score = final_score, Outlier_Flag = flag))
}
