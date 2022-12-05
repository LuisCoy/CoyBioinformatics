#' Random protemic data for examples
#'
#' @param seed Sets the seed
#' @param length Length of the data frame
#'
#' @return Dataframe of Log2 fold change and Log10 p-values
#' @export
#'
#' @examples create_proteomic_example(2, 10)
#' @examples create_proteomic_example(seed = 2, length = 10)
create_proteomic_example <- function(seed = 1, length = 1000){
  base::set.seed(seed)

  df <- data.frame(matrix(ncol = length, nrow = 3))
  df_a <- apply(df, 2, function(x) x = rnorm(n = 3, mean = 10, sd = 1))
  df_b <- apply(df, 2, function(x) x = rnorm(n = 3, mean = 10, sd = 1))

  together <- as.data.frame(rbind(df_a, df_b))
  together$condition <- c("condition1", "condition1", "condition1", "condition2", "condition2", "condition2")
  df_sum <- aggregate(.~condition, FUN = mean, data = together)
  df_fold_change <- df_sum[1,-1]/df_sum[2,-1]
}
