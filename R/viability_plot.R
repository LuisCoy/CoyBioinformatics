#' Viability plot
#' @description Plot viability data with standard deviation and a curve
#' @param df Dataframe
#' @param x A character string giving the x values; defaults to "conc"
#' @param y A character string giving the y values; defaults to "mean"
#' @param group A character string giving the group names; defaults to "condition"
#' @param sd A character string giving the standard deviation values; defaults to "sd"
#' @param size_line A value for the line thickness; defaults to 1
#' @param size_point A value for the point size; defaults to 2
#' @param size_text A value for the text size; defaults to 20
#' @param width_error_bar A value for the error bar width; defaults to 0.05
#' @param position_legend A vector for the legend position; defaults to c(0.2, 0.2)
#' @param label_x A character string giving the x axis label
#' @param label_y A character string giving the y axis label; defaults to "Cell Viability (%)"
#' @param label_legend A character string giving the legend label
#' @param break_labels_x A character string giving the x axis break labels. Same length as breaks
#' @param breaks_x  A vector giving the x axis breaks
#' @param breaks_y A vector giving the y axis breaks
#' @param limit_x A vector giving the x axis limits
#' @param limit_y A vector giving the y axis limits; defaults to c(0,120)
#' @param curve_upperl A value for the upper limit of the dose-response model; defaults to Inf
#' @param title A character string giving the title
#' @param tag A character string giving the tag
#' @param transparent A logical value for a transparent background; defaults to True
#' @param colours A character vector giving the colours to use; defaults to c("#2271B2", "#3BB9FF","#D55E00")
#'
#' @return Plot
#' @export
#'
#' @examples
viability_plot <-
  function(df,
           x = "conc",
           y = "mean",
           group = "condition",
           sd = "sd",
           size_line = 1,
           size_point = 2,
           size_text = 20,
           width_error_bar = 0.05,
           position_legend = c(0.2, 0.2),
           label_x = NULL,
           label_y = "Cell Viability (%)",
           label_legend = NULL,
           break_labels_x = NULL,
           breaks_x = NULL,
           breaks_y = NULL,
           limit_x = NULL,
           limit_y = c(0,120),
           curve_upperl = Inf,
           title = NULL,
           tag = NULL,
           transparent = T,
           colours = c("#2271B2", "#3BB9FF","#D55E00")) {
    # remove any x values which are 0
    df <- dplyr::filter(.data = df, .data[[x]] != 0)

    # if labels not provided then column names are given
    if (is.null(label_x)) {
      label_x <- x
    }
    if (is.null(label_y)) {
      label_y <- y
    }
    # choose between a transparent theme or a solid background theme
    if (transparent) {
      plot_theme <- ggplot2::theme(
        panel.background = ggplot2::element_rect(fill = 'transparent'),
        plot.background = ggplot2::element_rect(fill = 'transparent', color =
                                                  NA),
        legend.background = ggplot2::element_rect(fill = 'transparent'),
        legend.box.background = ggplot2::element_rect(fill = 'transparent')
      )
    } else {
      plot_theme <-
        ggplot2::theme(
          legend.background =  ggplot2::element_rect(
            fill = ggplot2::alpha("white", 0.5),
            color = ggplot2::alpha("grey", 0.5)
          )
        )
    }
    # if breaks for y axis are provided then these are used if not then a different scale_y_continuous has to be used
    if (!is.null(breaks_y)) {
      scale_y_continuous_1 <-
        ggplot2::scale_y_continuous(limits = limit_y,
                                    breaks = breaks_y)
    } else {
      scale_y_continuous_1 <-
        ggplot2::scale_y_continuous(limits = limit_y)
    }
    # plot
    plot <-
      ggplot2::ggplot(data = df, ggplot2::aes(
        x = !!sym(x),
        y = !!sym(y),
        col = !!sym(group),
        shape = !!sym(group)
      )) +
      ggplot2::geom_point(size = size_point) +
      ggplot2::stat_smooth(
        method = drc::drm,
        method.args = list(fct = drc::L.4()),
        se = F,
        linewidth = size_line) +
      ggplot2::labs(
        x = label_x,
        y = label_y,
        col = label_legend,
        shape = label_legend,
        title = title,
        tag = tag
      ) +
      ggplot2::geom_errorbar(ggplot2::aes(
        ymin = .data[[y]] - .data[[sd]],
        ymax = .data[[y]] + .data[[sd]],
        width = width_error_bar
      )) +
      ggplot2::theme_light() +
      ggplot2::theme(legend.position = position_legend,
                     legend.background = ggplot2::element_rect(fill = alpha("white",0.5)),
                     legend.key = ggplot2::element_rect(fill = 'transparent'),
                     text = ggplot2::element_text(size = size_text)) +
      ggplot2::scale_x_continuous(limits = limit_x,
                                  breaks = breaks_x,
                                  labels = break_labels_x,
                                  trans = "log10",
                                  minor_breaks = rep(1:9, 21)*(10^rep(-10:10, each = 9))) +
      ggplot2::scale_colour_manual(values = colours)+
      scale_y_continuous_1+
      plot_theme
  }
