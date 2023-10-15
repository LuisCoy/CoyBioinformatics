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
#' @param colour.a Character string giving the fill color for group a; defaults to "#D55E00"
#' @param colour.b Character string giving the fill color for group b;defaults to "#3DB7E9"
#' @param bins Number of bins to use in the histogram; defaults to 100
#' @param binwidth Width of the bins in the histogram; defaults to NULL
#' @param label.x X axis label; defaults to NULL
#' @param label.y Y axis label; defaults to "Frequency"
#' @param size.text Integer value specifying the text size in the plot; defaults to 20
#' @param title Character string giving the plot title; defaults to NULL
#' @param tag Character string giving the plot tag; defaults to NULL
#' @param label.legend Character string giving the legend label; defaults to NULL
#' @param alpha.bar The transparency of the fill color; defaults to 0.5
#' @param position.legend A vector for the legend position; defaults to c(0.8, 0.2).
#' @param colour.legend Hexadecimal color code for the legend background; defaults to white.
#' @param alpha.legend A value between 0 and 1 for the transparency of the legend; defaults to 0.5.
#' @param transparent A logical value for a transparent background; defaults to False.
#'
#' @return A ggplot2 histogram plot with vertical lines indicating the median value of each group
#'
#' @examples
#' df <- data.frame(abundances = rnorm(1000), group = sample(c("Treated", "Control"), 1000, replace = TRUE))
#' histogram_overlapping(df, "abundances", "Treated", "Control", "group", label.x = "Protein Abundances", label.legend = "Groups")
#'
#' @export
histogram_overlapping <-
  function(df,
           x,
           a,
           b,
           group,
           colour.a = "#D55E00",
           colour.b = "#3DB7E9",
           bins = 100,
           binwidth = NULL,
           label.x = NULL,
           label.y = "Frequency",
           size.text = 20,
           title = NULL,
           tag = NULL,
           label.legend = NULL,
           alpha.bar = 0.5,
           position.legend = c(0.8,0.2),
           colour.legend = "white",
           alpha.legend = 0.5,
           transparent = F) {
    #Convert to dataframe to allow median to be calculated
    dataframe <- base::data.frame(df)
    #Group column needs to be factors so that the data is always the correct way around
    dataframe[, group] <-
      base::factor(dataframe[, group], levels = c(a, b), ordered = T)

    median_a <- stats::median(dataframe[dataframe[, group] == a, x])
    median_b <- stats::median(dataframe[dataframe[, group] == b, x])
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
    ggplot2::ggplot(data = dataframe) +
      ggplot2::geom_histogram(
        ggplot2::aes_string(x = x, fill = group),
        position = "identity",
        colour = "white",
        alpha = alpha.bar,
        bins = bins,
        binwidth = binwidth
      ) +
      ggplot2::geom_vline(
        xintercept = median_a,
        col = colour.a,
        lwd = 0.5,
        alpha = alpha.bar
      ) +
      ggplot2::geom_vline(
        xintercept = median_b,
        col = colour.b,
        lwd = 0.5,
        alpha = alpha.bar
      ) +
      ggplot2::scale_fill_manual(
        name = label.legend,
        values = c(colour.a, colour.b),
        labels = c(a, b)
      ) +
      ggplot2::labs(
        x = label.x,
        y = label.y,
        title = title,
        tag = tag
      ) +
      ggplot2::theme_light() +
      ggplot2::theme(
        legend.background = ggplot2::element_rect(
          fill = scales::alpha(colour.legend, alpha.legend),
          colour = NULL
        ),
        legend.position = position.legend,
        legend.key = ggplot2::element_rect(fill = scales::alpha("white", 0)),
        text = ggplot2::element_text(size = size.text)
      ) +
      plot_theme
  }
