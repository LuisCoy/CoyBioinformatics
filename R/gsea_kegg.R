#' gsea_kegg
#'
#' Function to create a gene list and perform GSEA using the KEGG database. Uses ClusterProfiler's GSEA function gseKEGG.
#'
#' @param entrezID_column Character string for the name of the column containing the Entrez IDs
#' @param ranking_metric_column Character string for the name of the column containing the ranking metric data for example signal-to-noise or Log2 Fold Change
#' @param pvalue_cutoff Numeric value for the p value cutoff used in the GSEA KEGG results
#' @param df Dfferential expression dataframe
#'
#' @return Dataframe of GSEA KEGG results
#' @export
#'
#' @examples gsea_kegg(example_dep_1, entrezID_column = "EntrezID", ranking_metric_column = "S2N")
gsea_kegg <- function(df, entrezID_column, ranking_metric_column, pvalue_cutoff = 1){
  # Select on the EntrezID and S2N columns and remove NAs
  df <- dplyr::select(df, all_of(entrezID_column), all_of(ranking_metric_column))
  df <- stats::na.omit(df)
  # Create a named gene list
  geneList <- df[[ranking_metric_column]]
  base::names(geneList) <- df[[entrezID_column]]
  geneList <- base::sort(geneList, decreasing = T)
  # Perform GSEA using the KEGG database
  analysis_gseakegg <- clusterProfiler::gseKEGG(geneList = geneList, seed = T, pvalueCutoff = pvalue_cutoff)
  result_gseakegg <- analysis_gseakegg@result
  return(result_gseakegg)
}
