#' Signal to Noise Ratio
#'
#' This function calculates the absolute signal to noise ratio of two input vectors.
#'
#' @param x1 A numeric vector of values.
#' @param x2 A numeric vector of values.
#' @return A scalar value representing the signal to noise ratio of the input vectors.
#' @export
#' @examples
#' x1 <- c(1, 2, 3, 4)
#' x2 <- c(4, 3, 2, 1)
#' s2n(x1, x2)
s2n <- function(x1, x2) {
  mean1 <- mean(x1, na.rm = TRUE)
  mean2 <- mean(x2, na.rm = TRUE)
  sd1 <- stats::sd(x1, na.rm = TRUE)
  sd2 <- stats::sd(x2, na.rm = TRUE)

  signal_to_noise <- abs((mean1 - mean2) / (sd1 + sd2))

  return(signal_to_noise)
}
