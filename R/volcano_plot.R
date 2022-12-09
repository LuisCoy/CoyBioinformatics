#' Volcano plot for proteomic data
#'
#' @param df Dataframe containing Log2 fold change and Log10 p-value columns
#' @param x Column name (string) for Log2 fold change
#' @param y Column name (string) for Log10 p-value
#' @param p_value p=value cut off defaults to 0.05
#' @param Log2foldvalue Log2 fold change cut off defaults to 1
#'
#' @return Volcano plot
#' @export
#'
#' @examples volcano_plot(example_proteomic_data)
volcano_plot <- function(df, x = "Log2fold", y = "Log10pvalue", p_value = 0.05, log2foldvalue = 1, point_size = 2, text_size = 20){
  x_axis_length <- base::round(max(sqrt(df[,x]^2)), 0)+1

    df_regulation <- dplyr::mutate(df, "regulation" = dplyr::case_when(df[,x] >= log2foldvalue & df[,y] >= -log10(p_value) ~ "up",
                                                                       df[,x] <= -log2foldvalue & df[,y] >= -log10(p_value) ~ "down",
                                                                       T ~ "neutral"))

    ggplot2::ggplot(data = df_regulation,ggplot2::aes_string(x = x, y = y, col = "regulation"))+
    ggplot2::geom_point(size = point_size)+
    ggplot2::labs(x = base::expression("Log"[2]* " (Fold Change)"),
         y = expression("-Log"[10]* " (p-value)"),
         tag = NULL,
         title = NULL)+
    ggplot2::geom_vline(xintercept = c(-log2foldvalue,log2foldvalue), color = "black", size = 1, linetype = "dashed")+
    ggplot2::geom_hline(yintercept = -log10(p_value), color = "black", size = 1, linetype = "dashed")+
    ggplot2::scale_x_continuous(breaks = seq(-x_axis_length,x_axis_length,1),
                       limits = c(-x_axis_length,x_axis_length))+
    ggplot2::scale_colour_manual(values = c("up" = "#F60239", "down" = "#00E307", "neutral" = "#000000"),labels = c("", "", ""))+
    ggplot2::theme_light()+
    ggplot2::theme(legend.position = "none", text = ggplot2::element_text(size = text_size))
}
