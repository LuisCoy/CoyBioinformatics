#' gsea_kegg
#'
#' Function to create a gene list and perform GSEA using the KEGG database
#'
#' @param entrezID_column Character string for the name of the column containing the Entrez IDs
#' @param s2n_column Character string for the name of the column containing the signal to noise data
#' @param pvalue_cutoff Numeric value for the p value cutoff used in the GSEA KEGG results
#' @param df Dfferential expression dataframe
#'
#' @return Dataframe of GSEA KEGG results
#' @export
#'
#' @examples
gsea_kegg <- function(df, entrezID_column, s2n_column, pvalue_cutoff = 1){
  # Select on the EntrezID and S2N columns and remove NAs
  df <- dplyr::select(df, all_of(entrezID_column), all_of(s2n_column))
  df <- stats::na.omit(df)
  # Create a named gene list
  geneList <- df[[s2n_column]]
  base::names(geneList) <- df[[entrezID_column]]
  geneList <- base::sort(geneList, decreasing = T)
  # Perform GSEA using the KEGG database
  analysis_gseakegg <- clusterProfiler::gseKEGG(geneList = geneList, seed = T, pvalueCutoff = pvalue_cutoff)
  result_gseakegg <- analysis_gseakegg@result
  return(result_gseakegg)
}
