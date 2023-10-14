#' pvalue histogram plot for proteomic data
#'
#' @param df Dataframe containing p-values
#' @param x A character string giving the p value column name
#' @param binwidth A value for the binwidth; defaults to 0.05
#' @param text_size Font size; defaults to 20
#' @param axis_label_x X axis label; defaults to bquote(italic("P")*" value")
#' @param axis_label_y Y axis label; defaults to "Frequency"
#' @param transparent A logical value for a transparent background; defaults to True
#' @param colour_fill A character string for the bar fill colour; defaults to "#2271B2"
#' @param x_angle A value giving the angle of the x axis text; defaults to 45
#' @param x_hjust A value giving the horizontal justification of the x axis text; defaults to 1
#'
#' @return Histogram
#' @export
#'
#' @examples
pvalue_histogram <-
  function(df,
           x = "pvalue",
           binwidth = 0.05,
           text_size = 20,
           axis_label_x = bquote(italic("P")*" value"),
           axis_label_y = "Frequency",
           transparent = T,
           colour_fill = "#2271B2",
           x_angle = 45,
           x_hjust = 1) {
    # plot theme
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
    ggplot2::ggplot(data = df, ggplot2::aes_string(x = x)) +
      ggplot2::geom_histogram(binwidth = binwidth,
                              boundary = 0,
                              fill = colour_fill) +
      ggplot2::labs(
        x = axis_label_x,
        y = axis_label_y,
        tag = NULL,
        title = NULL
      ) +
      ggplot2::theme_light() +
      ggplot2::scale_x_continuous(
        breaks = seq(0, 1, 0.05),
        limits = c(0, 1),
        minor_breaks = seq(0, 1, 0.05)
      ) +
      ggplot2::theme(
        axis.text.x = ggplot2::element_text(angle = x_angle, hjust = x_hjust, colour = c("black", NA)),
        text = ggplot2::element_text(size = text_size)
      )+
      plot_theme

  }
