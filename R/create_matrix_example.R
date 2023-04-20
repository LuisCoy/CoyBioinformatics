#' Create Matrix Example
#' @description Generate Example Expression Data for PCA Plotting
#'
#' @param replicate_n The number of replicates to be generated, defaults to 4
#' @param feature_n The number of features to be generated, defaults to 100
#' @param seed Seed for random number generation, defaults to 123
#' @param a Character string for group A, defaults to "Treated"
#' @param b Character string for group B, defaults to "Control"
#' @return A transposed data frame with sample names as row names and feature names as column names
#' @examples
#' create_matrix_example()
#' create_matrix_example(replicate_n = 5, feature_n = 80, seed = 123, a = "Experiment1", b = "Experiment2")
#' @export
create_matrix_example <- function(replicate_n = 4, feature_n = 100, seed = 123, a = "Treated", b = "Control"){
  base::set.seed(seed)
  df <- base::matrix(stats::rnorm(replicate_n*feature_n*2), ncol = replicate_n*2)
  base::colnames(df) <- c(base::paste0(a,"_", 1:replicate_n), base::paste0(b,"_", (replicate_n+1):(replicate_n*2)))
  base::rownames(df) <- base::paste0("Feature_", 1:feature_n)
  df <- base::t(df)
  base::return(df)
}
