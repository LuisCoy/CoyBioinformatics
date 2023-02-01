#' Histogram Overlapping
#'
#' @param df
#' @param x
#' @param a
#' @param b
#' @param group
#' @param a_colour
#' @param b_colour
#' @param bins
#' @param x_label
#' @param y_label
#' @param title
#' @param tag
#' @param legend_label
#' @param alpha
#'
#' @return Histogram
#' @export
#'
#' @examples
histogram_overlapping <- function(df, x, a, b, group, a_colour = "#F748A5", b_colour = "#3BB9FF", bins = 100, x_label = NULL, y_label = "Count", title = NULL, tag = NULL, legend_label = NULL, alpha = 0.5){

  #Convert to dataframe to allow median to be calculated
  dataframe <- base::data.frame(df)
  #Group column needs to be factors so that the data is always the correct way around
  dataframe[,group] <- base::factor(dataframe[,group], levels = c(a,b), ordered = T)

  median_a <- stats::median(dataframe[dataframe[,group] == a, x])
  median_b <- stats::median(dataframe[dataframe[,group] == b, x])

  ggplot2::ggplot(data = dataframe)+
    ggplot2::geom_histogram(ggplot2::aes_string(x = x, fill = group), position = "identity", colour = "white", alpha = alpha, bins = bins)+
    ggplot2::geom_vline(xintercept = median_a, col = a_colour, lwd = 0.5, alpha = alpha)+
    ggplot2::geom_vline(xintercept = median_b, col = b_colour, lwd = 0.5, alpha = alpha)+
    ggplot2::scale_fill_manual(name = legend_label,
                               values = c(a_colour,b_colour),
                               labels = c(a,b))+
    ggplot2::labs(x = base::bquote(Log[2]*~(.(x_label))),
                  y = y_label,
                  title = title,
                  tag = tag)+
    ggplot2::theme_light()
}
