#' Protein quantification from BCA
#'
#' @param standards_df Dataframe with protein standard concentrations, ug/ml, as column names and raw absorbance as values. Standard control column name must be 0. Required check.names = False.
#' @param unknowns_df Dataframe with unique ID as column names and raw absorbance as values. Required check.names = False.
#'
#' @return Dataframe of BCA results
#' @export
#'
#' @examples
bca_analysis <- function(standards_df, unknowns_df){
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
  poly_model <- stats::lm(bsa_conc ~ stats::poly(mean,4), data = standards_sum)

  unknowns_sum$mg_ml <- stats::predict(poly_model, newdata = unknowns_sum)/1000
  return(unknowns_sum)
}
