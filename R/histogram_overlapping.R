#' Plot two-group histograms
#'
#' This function creates a histogram plot of two groups with overlapping histograms
#' and adds vertical lines to indicate the median value of each group.
#'
#' @param df Dataframe containing the data to plot
#' @param x Character string giving the column containing the values
#' @param a Character string giving the name of group a e.g. "Treated"
#' @param b Character string giving the name of group b e.g. "Control"
#' @param group Character string giving the column containing the group IDs
#' @param colour.a Character string giving the fill color for group a; defaults to "#F748A5"
#' @param colour.b Character string giving the fill color for group b;defaults to "#3BB9FF"
#' @param bins Number of bins to use in the histogram; defaults to 100
#' @param binwidth Width of the bins in the histogram; defaults to NULL
#' @param label.x X axis label; defaults to NULL
#' @param label.y Y axis label; defaults to "Count"
#' @param title Character string giving the plot title; defaults to NULL
#' @param tag Character string giving the plot tag; defaults to NULL
#' @param label.legend Character string giving the legend label; defaults to NULL
#' @param alpha The transparency of the fill color; defaults to 0.5
#'
#' @return A ggplot2 histogram plot with vertical lines indicating the median value of each group
#'
#' @examples
#' df <- data.frame(abundances = rnorm(1000), group = sample(c("Treated", "Control"), 1000, replace = TRUE))
#' histogram_overlapping(df, "abundances", "Treated", "Control", "group", label.x = "Protein Abundances", label.legend = "Groups")
#'
#' @export
histogram_overlapping <- function(df, x, a, b, group, colour.a = "#F748A5", colour.b = "#3BB9FF", bins = 100, binwidth = NULL, label.x = NULL, label.y = "Count", title = NULL, tag = NULL, label.legend = NULL, alpha = 0.5){

  #Convert to dataframe to allow median to be calculated
  dataframe <- base::data.frame(df)
  #Group column needs to be factors so that the data is always the correct way around
  dataframe[,group] <- base::factor(dataframe[,group], levels = c(a,b), ordered = T)

  median_a <- stats::median(dataframe[dataframe[,group] == a, x])
  median_b <- stats::median(dataframe[dataframe[,group] == b, x])

  ggplot2::ggplot(data = dataframe)+
    ggplot2::geom_histogram(ggplot2::aes_string(x = x, fill = group), position = "identity", colour = "white", alpha = alpha, bins = bins, binwidth = binwidth)+
    ggplot2::geom_vline(xintercept = median_a, col = colour.a, lwd = 0.5, alpha = alpha)+
    ggplot2::geom_vline(xintercept = median_b, col = colour.b, lwd = 0.5, alpha = alpha)+
    ggplot2::scale_fill_manual(name = label.legend,
                               values = c(colour.a, colour.b),
                               labels = c(a,b))+
    ggplot2::labs(x = label.x,
                  y = label.y,
                  title = title,
                  tag = tag)+
    ggplot2::theme_light()
}
