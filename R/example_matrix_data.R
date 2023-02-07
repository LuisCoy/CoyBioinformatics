#' Example Matrix Data
#'
#' @param replicate_n Number of replicates (default 4)
#' @param feature_n Number of features (default 1000)
#' @param seed Seed for the random number generator (default 123)
#' @param a Column label prefix for group a (default "Treated")
#' @param b Column label prefix for group b (default "Control")
#' @return A matrix with `replicate_n` columns and `feature_n` rows
#' @export
create_matrix_example <- function(replicate_n = 4, feature_n = 1000, seed = 123, a = "Treated", b = "Control"){
base::set.seed(seed)
df <- base::matrix(stats::rnorm(replicate_n*feature_n*2), ncol = replicate_n*2)
base::colnames(df) <- c(base::paste0(a,"_", 1:replicate_n), base::paste0(b,"_", (replicate_n+1):(replicate_n*2)))
base::rownames(df) <- base::paste0("Feature_", 1:feature_n)
df <- base::t(df)
base::return(df)
}
