#' Coverage Plot
#' @description Plots the coverage of proteins across the replicates. Requires summarised data that contains how many samples contain expression data for each protein, as a percentage of the total number of proteins.
#'
#' @param df Dataframe
#' @param number_of_samples A character string giving the column containing the number of samples.
#' @param percentage A character string giving the column that contains the percentages.
#' @param tally A character string giving the column that contains the number of proteins.
#' @param a A character string giving the name of group a e.g. "Treated".
#' @param b A character string giving the name of group b e.g. "Control".
#' @param group A character string giving the column containing the group IDs.
#' @param a_colour A character string giving the colour for group a; defualts to "#D55E00".
#' @param b_colour A character string giving the colour for group b; defaults to "#3DB7E9".
#' @param x_label X axis label; defaults to "Number of samples".
#' @param y_label Y axis label; defaults to "Percentage of proteins (%)".
#' @param text_size Integer value specifying the text size in the plot; defaults to 20.
#' @param title A character string giving the plot Title.
#' @param tag A character string giving the  plot tag.
#' @param legend_label A character string giving the legend label.
#' @param legend_position A vector for the legend position; defaults to c(0.8, 0.2).
#' @param legend_colour Hexadecimal color code for the legend background; defaults to white.
#' @param legend_alpha A value between 0 and 1 for the transparency of the legend; defaults to 0.5.
#' @param transparent A logical value for a transparent background; defaults to True.
#'
#' @return Plot
#' @export
#'
#' @examples
coverage_plot <-
  function(df,
           number_of_samples,
           percentage,
           tally,
           a,
           b,
           group,
           a_colour = "#D55E00",
           b_colour = "#3DB7E9",
           x_label = "Number of samples",
           y_label = "Percentage of proteins (%)",
           text_size = 20,
           title = NULL,
           tag = NULL,
           legend_label = NULL,
           legend_position = c(0.85,0.2),
           legend_colour = "white",
           legend_alpha = 0.5,
           transparent = T) {
    #Convert to dataframe
    dataframe <- base::data.frame(df)
    #Group column needs to be factors so that the data is always the correct way around
    dataframe[, group] <-
      base::factor(dataframe[, group], levels = c(a, b), ordered = T)
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
    ggplot2::ggplot(
      data = dataframe,
      ggplot2::aes_string(x = number_of_samples, y = percentage, fill = group),
      alpha = alpha
    ) +
      ggplot2::geom_col(position = ggplot2::position_dodge2(preserve = "single")) +
      ggplot2::geom_text(
        ggplot2::aes_string(label = tally, x = number_of_samples, y = percentage),
        position = ggplot2::position_dodge(width = 0.9),
        vjust = -0.6
      ) +
      ggplot2::scale_y_continuous(breaks = c(0, 50, 100),
                                  limits = c(0, 110)) +
      ggplot2::scale_fill_manual(
        name = legend_label,
        values = c(a_colour, b_colour),
        labels = c(a, b)
      ) +
      ggplot2::labs(
        x = x_label,
        y = y_label,
        title = title,
        tag = tag
      ) +
      ggplot2::theme_light()+
      ggplot2::theme(legend.background = ggplot2::element_rect(fill = scales::alpha(legend_colour,legend_alpha), colour = NULL),
                     legend.position = legend_position,
                     legend.key = ggplot2::element_rect(fill = scales::alpha("white", 0)),
                     text = ggplot2::element_text(size = text_size)) +
      plot_theme

  }
