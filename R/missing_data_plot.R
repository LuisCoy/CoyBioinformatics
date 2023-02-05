#' Missing data plot
#' @description Plots the number of proteins with missing expression data for each sample. Requires a long format dataframe with the number of proteins present and missing in each sample.
#'
#' @param df Dataframe
#' @param sample A character string giving the column name for the sample IDs
#' @param name A character string giving the column containing present or missing
#' @param value A character string giving the column containing the number of proteins present or missing
#' @param group A character string giving the column with the group IDs e.g. "Treated" and "Control"
#' @param x_label X axis label; defaults to "Samples"
#' @param y_label Y axis label; defaults to "Number of proteins"
#' @param title A character string giving the plot Title
#' @param tag A character string giving the  plot tag
#' @param legend_label A character string giving the legend label
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
