#' Point plot
#' @description Plot data with standard deviation and a curve
#'
#' @param df Dataframe
#' @param x A character string giving the x values
#' @param y A character string giving the y values
#' @param col A character string giving the colour values; defaults to "condition"
#' @param sd A character string giving the standard deviation values
#' @param formula A formula for the curve; defaults to y~stats::poly(x,2)
#' @param line_size A value for the line thickness; defaults to 0.7
#' @param error_bar_factor A value that is multiplied by the x axis range to set the error bar width; defaults to 0.02
#' @param expand_y_factor A values to increase the y axis by; defaults to 0.2
#' @param expand_x_factor A values to increase the x axis by; defaults to 0.1
#' @param legend_position A vector for the legend position; defaults to c(0.8, 0.2)
#' @param x_label A character string giving the x axis label
#' @param y_label A character string giving the y axis label
#' @param col_label A character string giving the colour label
#' @param title A character string giving the title
#' @param tag A character string giving the tag
#'
#' @return Plot
#' @export
#'
#' @examples
point_plot <- function(df, x, y, col = "condition", sd, formula = y~stats::poly(x,2), line_size = 0.7, error_bar_factor = 0.02, expand_y_factor = 0.2, expand_x_factor = 0.1, legend_position = c(0.80,0.2), x_label = NULL, y_label = NULL,col_label = NULL,title = NULL, tag = NULL){
  # remove any x values which are 0
  df <- dplyr::filter(.data = df, .data[[x]] != 0)

  error_bar_width <- diff(range(df[x]))*error_bar_factor

  if (is.null(x_label)){
    x_label <- x
  }
  if (is.null(y_label)){
    y_label <- y
  }
  if (is.null(col_label)){
    col_label <- col
  }

  ggplot2::ggplot(data = df, ggplot2::aes_string(x = x, y = y, col = col))+
    ggplot2::geom_point()+
    ggplot2::stat_smooth(method = lm,
                         formula = formula,
                         se = F,
                         size = line_size)+
    ggplot2::labs(x = x_label,
                  y = y_label,
                  col = col_label,
                  title = title,
                  tag = tag)+
    ggplot2::geom_errorbar(ggplot2::aes(ymin=.data[[y]]-.data[[sd]], ymax=.data[[y]]+.data[[sd]], width = error_bar_width))+
    ggplot2::theme_light()+
    ggplot2::theme(legend.position = legend_position,
                   legend.background =  ggplot2::element_rect(fill = ggplot2::alpha("white",0.5), color = ggplot2::alpha("grey",0.5)))+
    ggplot2::scale_y_continuous(expand = c(expand_y_factor,0))+
    ggplot2::scale_x_continuous(expand = c(expand_x_factor,0))

}
