#' Missing data plot
#'
#' @param df
#' @param sample
#' @param name
#' @param value
#' @param group
#' @param x_label
#' @param y_label
#' @param title
#' @param tag
#' @param legend_label
#'
#' @return plot
#' @export
#'
#' @examples
missing_data_plot <- function(df, sample, name, value, group, x_label = "Samples", y_label = "Number of proteins", title = NULL, tag = NULL, legend_label = NULL){

  ggplot2::ggplot(data = df, ggplot2::aes_string(x = sample, y = value, fill = name))+
    ggplot2::geom_col()+
    ggplot2::facet_grid(~.data[[group]], scales = "free", space = "free")+
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, vjust = 0.5, hjust=1))+
    ggplot2::labs(x = x_label,
                  y = y_label,
                  title = title,
                  tag = tag,
                  fill = legend_label)+
    ggplot2::theme_light()
}
