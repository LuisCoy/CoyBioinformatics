#' Coverage Plot
#'
#' @param df
#' @param number_of_samples
#' @param percentage
#' @param tally
#' @param a
#' @param b
#' @param group
#' @param a_colour
#' @param b_colour
#' @param x_label
#' @param y_label
#' @param title
#' @param tag
#' @param legend_label
#'
#' @return Plot
#' @export
#'
#' @examples
coverage_plot <- function(df, number_of_samples, percentage, tally, a, b, group, a_colour = "#F748A5", b_colour = "#3BB9FF", x_label = "Number of Samples", y_label = "Percentage of Proteins", title = NULL, tag = NULL, legend_label = NULL){

  #Convert to dataframe
  dataframe <- base::data.frame(df)
  #Group column needs to be factors so that the data is always the correct way around
  dataframe[,group] <- base::factor(dataframe[,group], levels = c(a,b), ordered = T)

  ggplot2::ggplot(data = dataframe, ggplot2::aes_string(x = number_of_samples, y = percentage, fill = group), alpha = alpha)+
    ggplot2::geom_col(position = ggplot2::position_dodge2(preserve = "single"))+
    ggplot2::geom_text(ggplot2::aes_string(label = tally, x = number_of_samples, y = percentage), position = ggplot2::position_dodge(width = 0.9), vjust = -0.6)+
    ggplot2::scale_y_continuous(breaks = c(0,50,100),
                                limits = c(0,110))+
    ggplot2::scale_fill_manual(name = legend_label,
                               values = c(a_colour,b_colour),
                               labels = c(a,b))+
    ggplot2::labs(x = x_label,
                  y = y_label,
                  title = title,
                  tag = tag)+
    ggplot2::theme_light()

}
