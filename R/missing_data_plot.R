#' Missing data plot
#' @description Plots the number of proteins with missing expression data for each sample. Requires a long format dataframe with the number of proteins present and missing in each sample.
#'
#' @param df Dataframe
#' @param sample A character string giving the column name for the sample IDs
#' @param name A character string giving the column containing present or missing
#' @param value A character string giving the column containing the number of proteins present or missing
#' @param group A character string giving the column with the group IDs e.g. "Treated" and "Control"
#' @param colours A character vector giving the colours; defaults to "#F748A5" and "#3DB7E9".
#' @param x_label X axis label; defaults to "Samples"
#' @param y_label Y axis label; defaults to "Number of proteins"
#' @param text_size Integer value specifying the text size in the plot; defaults to 20.
#' @param title A character string giving the plot Title
#' @param tag A character string giving the  plot tag
#' @param legend_label A character string giving the legend label
#' @param legend_position A vector for the legend position; defaults to c(0.8, 0.2).
#' @param legend_colour Hexadecimal color code for the legend background; defaults to white.
#' @param legend_alpha A value between 0 and 1 for the transparency of the legend; defaults to 0.5.
#' @param transparent A logical value for a transparent background; defaults to True.
#'
#' @return plot
#' @export
#'
#' @examples
missing_data_plot <-
  function(df,
           sample,
           name,
           value,
           group,
           colours = c("#F748A5","#3DB7E9"),
           x_label = "Samples",
           y_label = "Number of proteins",
           text_size = 20,
           title = NULL,
           tag = NULL,
           legend_label = NULL,
           legend_position = c(0.6,0.8),
           legend_colour = "white",
           legend_alpha = 0.5,
           transparent = T) {
    #plot theme
    if (transparent) {
      plot_theme <- ggplot2::theme(
        panel.background = ggplot2::element_rect(fill = 'transparent'),
        plot.background = ggplot2::element_rect(fill = 'transparent', color =
                                                  NA)
      )
    } else {
      plot_theme <-
        ggplot2::theme()
    }
    # plot
    ggplot2::ggplot(data = df, ggplot2::aes_string(x = sample, y = value, fill = name)) +
      ggplot2::geom_col() +
      ggplot2::facet_grid( ~ .data[[group]], scales = "free", space = "free") +
      ggplot2::scale_fill_manual(values = colours)+
      ggplot2::theme(axis.text.x = ggplot2::element_text(
        angle = 90,
        vjust = 0.5,
        hjust = 1
      )) +
      ggplot2::labs(
        x = x_label,
        y = y_label,
        title = title,
        tag = tag,
        fill = legend_label
      ) +
      ggplot2::theme_light()+
      ggplot2::theme(legend.background = ggplot2::element_rect(fill = scales::alpha(legend_colour,legend_alpha), colour = NULL),
                     legend.position = legend_position,
                     legend.key = ggplot2::element_rect(fill = scales::alpha("white", 0)),
                     text = ggplot2::element_text(size = text_size)) +
      plot_theme
  }
