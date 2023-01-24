#' Plotting BCA results
#'
#' @param standards_df Dataframe with protein standard concentrations, ug/ml, as column names and raw absorbance as values. Standard control column name must be 0. Required check.names = False.
#' @param unknowns_df Dataframe with unique ID as column names and raw absorbance as values. Required check.names = False.
#' @param x_upper_limit X axis upper limit. Defaults to 2.02
#' @param text_size Size of text elements. Defaults to 20
#' @param point_size Size of points. Defaults to 2
#'
#' @return Plot of BCA standard curve with unknown protein concentrations.
#' @export
#'
#' @examples
bca_plot <- function(standards_df, unknowns_df, x_upper_limit = 2.02, text_size = 20, point_size = 2){
  #standard curve summarising
  standards_gathered <-  tidyr::gather(data = standards_df,key = "bsa_conc", convert = T)
  standards_blanked <- dplyr::mutate(.data = standards_gathered, "minus_blank" = value - mean(value[bsa_conc == 0]))
  standards_filtered <- dplyr::filter(.data = standards_blanked, bsa_conc != 0)
  standards_grouped <- dplyr::group_by(.data = standards_filtered, bsa_conc)
  standards_sum <- dplyr::summarise(.data = standards_grouped, mean = mean(minus_blank), sd = sd(minus_blank), cv = sd(minus_blank)/mean(minus_blank)*100, .groups = "drop")
  #blank data for unknowns
  blanks <- dplyr::filter(.data = standards_gathered, bsa_conc == 0)
  blanks <- dplyr::summarise(.data = blanks, mean=mean(value))
  #unknowns summarising
  unknowns_gathered <- tidyr::gather(data = unknowns_df, key = "sample", convert = T)
  unknowns_blanked <- dplyr::mutate(.data = unknowns_gathered, minus_blank = value - blanks$mean)
  unknowns_grouped <- dplyr::group_by(.data = unknowns_blanked, sample)
  unknowns_sum <- dplyr::summarise(.data = unknowns_grouped,mean = mean(minus_blank, na.rm = T), sd = sd(minus_blank, na.rm = T), cv = sd(minus_blank, na.rm = T)/mean(minus_blank, na.rm = T)*100)
  #protein concentrations
  poly_model <- stats::lm(bsa_conc ~ poly(mean,4), data = standards_sum)

  unknowns_sum$mg_ml <- stats::predict(poly_model, newdata = unknowns_sum)/1000
  #plot variable
  max <- max(standards_sum$mean)*10
  max <- ceiling(max)/10
  #plot

  ggplot2::ggplot(data = standards_sum, ggplot2::aes(x = bsa_conc/1000, y = mean))+
    ggplot2::geom_point(colour = "#2271B2", size = point_size)+
    ggplot2::geom_errorbar(ggplot2::aes(ymin = mean-sd,
                                        ymax = mean+sd),
                           width = 0.03,
                           colour = "#2271B2")+
    ggplot2::stat_smooth(method = "lm",
                         formula = y~ stats::poly(x,4),
                         se = F,
                         size = 0.8,
                         colour = "#3BB9FF")+
    ggplot2::labs(x = "BSA (mg/ml)",
                  y = "Absorbance (590nm)")+
    ggplot2::scale_x_continuous(limits = c(0,x_upper_limit),
                                breaks = seq(0,x_upper_limit,0.25))+
    ggplot2::scale_y_continuous(limits = c(0,max),
                                breaks = seq(0,max,0.1))+
    ggplot2::theme(text = ggplot2::element_text(size = text_size))+
    ggplot2::theme_light()+
    ggplot2::geom_segment(data = unknowns_sum,
                          ggplot2::aes(x = rep(0,length(sample)),
                                       xend = mg_ml,
                                       y = mean,
                                       yend = mean ),
                          linetype = 2)+
    ggplot2::geom_segment(data = unknowns_sum,
                          ggplot2::aes(x = mg_ml,
                                       xend = mg_ml,
                                       y = rep(0,length(sample)),
                                       yend = mean ),
                          linetype = 2)
}
