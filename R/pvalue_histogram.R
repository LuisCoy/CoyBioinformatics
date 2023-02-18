#' pvalue histogram plot for proteomic data
#'
#' @param df Dataframe containing p-values
#' @param x A character string giving the p value column name
#' @param text_size Font size; defaults to 20
#'
#' @return Histogram
#' @export
#'
#' @examples
pvalue_histogram <- function(df, x = "pvalue", text_size = 20, axis_label_x = "p value", axis_label_y = "Frequency"){
  ggplot2::ggplot(data = df, ggplot2::aes_string(x = x))+
    ggplot2::geom_histogram(binwidth = 0.05, boundary = 0, fill = "#2271B2")+
    ggplot2::labs(x = axis_label_x,
                  y = axis_label_y,
                  tag = NULL,
                  title = NULL)+
    ggplot2::theme_light()+
    ggplot2::scale_x_continuous(breaks = seq(0,1,0.05), limits = c(0,1), minor_breaks = seq(0,1,0.05))+
    ggplot2::theme(axis.text.x = ggplot2::element_text(colour = c("black", NA)),
                   text = ggplot2::element_text(size = text_size))

}
