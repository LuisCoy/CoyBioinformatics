#' gsea_kegg
#'
#' Function to create a gene list and perform GSEA KEGG
#'
#' @param df
#'
#' @return
#' @export
#'
#' @examples
gsea_kegg <- function(df){
  df <- na.omit(df)
  geneList <- df$Log2fold
  names(geneList) <- df$EntrezID
  geneList <- sort(geneListm,decreasing = T)
  analysis_gseakegg <- gseKEGG(geneList = geneList, seed = T)
}
