#' Plot PCA Results
#'
#' This function creates a scatter plot of principal component analysis (PCA) results, highlighting two groups of replicates.
#'
#' @param matrix A matrix with IDs across the columns and samples down the rows
#' @param center Logical; if `TRUE`, variables will be centered (mean-subtracted) prior to PCA.
#' @param scale Logical; if `TRUE`, variables will be scaled (divided by standard deviation) prior to PCA.
#' @param a Character string representing the name of the first group of replicates.
#' @param b Character string representing the name of the second group of replicates.
#' @param a_replicates Integer value representing the number of replicates in the first group.
#' @param b_replicates Integer value representing the number of replicates in the second group.
#' @param a_colour Hexadecimal color code for the first group.
#' @param b_colour Hexadecimal color code for the second group.
#' @param symbols A vector containing the symbol codes; defaults to c(15, 19).
#' @param symbol_size Integer value specifying the symbol size; defaults to 2.
#' @param text_size Integer value specifying the text size in the plot; defaults to 20.
#' @param title Character string for the plot title.
#' @param tag Character string for the plot tag.
#' @param legend_label Character string for the plot legend label.
#' @param legend_position A vector for the legend position; defaults to c(0.8, 0.2).
#' @param transparent A logical value for a transparent background; defaults to True.
#' @param legend_colour Hexadecimal color code for the legend background; defaults to white.
#' @param legend_alpha A value between 0 and 1 for the transparency of the legend; defaults to 0.5.
#'
#' @return A scatter plot of PCA results highlighting the two groups of replicates.
#' @import ggbiplot ggplot2
#' @importFrom stringr str_c
#' @importFrom ggplot2 labs scale_colour_manual theme theme_light
#' @export
#' @examples
#' pca_plot(matrix = example_matrix_data, a_replicates = 4, b_replicates = 4)
#'
pca_plot <-
  function(matrix,
           center = T,
           scale = T,
           a = "Treated",
           b = "Control",
           a_replicates,
           b_replicates,
           a_colour = "#D55E00",
           b_colour = "#3DB7E9",
           symbols = c(15,19),
           symbol_size = 2,
           text_size = 20,
           title = NULL,
           tag = NULL,
           legend_label = NULL,
           legend_position = c(0.6,0.8),
           legend_colour = "white",
           legend_alpha = 0.5,
           transparent = T) {
    #remove NA values
    matrix[base::is.na(matrix)] <- 0
    #pca
    pca <- prcomp(matrix, center = center, scale. = scale)
    #get pc1 and pc2 values for axis labels
    pca_var <-
      summary(pca)

    var_PC1 <-
      round(pca_var[[6]][2, 1] * 100, 1)

    var_PC2 <-
      round(pca_var[[6]][2, 2] * 100, 1)
    #groups
    groups <- factor(c(rep(a, a_replicates),
                       rep(b, b_replicates)),
                     ordered = T,
                     levels = c(a, b))
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

    #plot
    ggbiplot::ggbiplot(pca,
                       var.axes = F,
                       groups = groups,
                       ellipse = T) +
      ggplot2::geom_point(aes(shape = groups,
                              color = groups),
                          size = symbol_size)+
      ggplot2::labs(
        title = title,
        tags = tag,
        x = stringr::str_c("PC1 (", var_PC1, "% explained var.)"),
        y = stringr::str_c("PC2 (", var_PC2, "% explained var.)")
      ) +
      ggplot2::scale_colour_manual(legend_label,
                                   values = c(a_colour,
                                              b_colour),
                                   labels = c(a,
                                              b)) +
      ggplot2::scale_shape_manual(legend_label,
                                  values = symbols,
                                  labels = c(a,b))+
      ggplot2::theme_light()+
      ggplot2::theme(legend.background = ggplot2::element_rect(fill = scales::alpha(legend_colour,legend_alpha), colour = NULL),
                     legend.position = legend_position,
                     legend.key = ggplot2::element_rect(fill = scales::alpha("white", 0)),
                     text = ggplot2::element_text(size = text_size)) +
      plot_theme
  }

