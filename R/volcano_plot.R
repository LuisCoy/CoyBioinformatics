#' Volcano plot for proteomic data
#'
#' @param df Dataframe containing Log2 fold change and Log10 p value columns
#' @param x A character string giving the Log2 fold change column name
#' @param y A character string giving the Log10 p value column name
#' @param p_value p value cut off; defaults to 0.05
#' @param Log2foldvalue Log2 fold change cut off; defaults to 1
#' @param point_size Point size; defaults to 1
#' @param text_size Text size; defaults to 20
#' @param axis_label_x X axis label; defaults to "Fold change"
#' @param axis_label_y Y axis label; defaults to bquote(italic("P")*" value")
#' @param transparent A logical value for a transparent background; defaults to True
#'
#' @return Volcano plot
#' @export
#'
#' @examples volcano_plot(example_proteomic_data)
volcano_plot <-
  function(df,
           x = "Log2fold",
           y = "Log10pvalue",
           p_value = 0.05,
           log2foldvalue = 1,
           point_size = 1,
           text_size = 20,
           axis_label_x = "Fold change",
           axis_label_y = bquote(italic("P")*" value"),
           transparent = T) {
    x_axis_length <- base::round(max(sqrt(df[, x] ^ 2)), 0) + 1

    df_regulation <-
      dplyr::mutate(
        df,
        "regulation" = dplyr::case_when(
          !!sym(x) >= log2foldvalue & !!sym(y) >= -log10(p_value) ~ "up",!!sym(x) <= -log2foldvalue &
            !!sym(y) >= -log10(p_value) ~ "down",
          T ~ "neutral"
        )
      )
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
    ggplot2::ggplot(data = df_regulation, ggplot2::aes_string(x = x, y = y, col = "regulation")) +
      ggplot2::geom_point(size = point_size) +
      ggplot2::labs(
        x = bquote(Log[2] * (.(axis_label_x))),
        y = bquote(-Log[10] * (.(axis_label_y))),
        tag = NULL,
        title = NULL
      ) +
      ggplot2::geom_vline(
        xintercept = c(-log2foldvalue, log2foldvalue),
        color = "black",
        size = 1,
        linetype = "dashed"
      ) +
      ggplot2::geom_hline(
        yintercept = -log10(p_value),
        color = "black",
        size = 1,
        linetype = "dashed"
      ) +
      ggplot2::scale_x_continuous(
        breaks = seq(-x_axis_length, x_axis_length, 1),
        limits = c(-x_axis_length, x_axis_length)
      ) +
      ggplot2::scale_colour_manual(
        values = c(
          "up" = "#F60239",
          "down" = "#00E307",
          "neutral" = "#000000"
        ),
        labels = c("", "", "")
      ) +
      ggplot2::theme_light() +
      ggplot2::theme(legend.position = "none",
                     text = ggplot2::element_text(size = text_size))+
      plot_theme
  }
