#' Format Peaks
#'
#' A function to format peaks proteins.csv file. The function reads a file, adds new columns with the names of the groups,
#' separates an accession column to get the accession ID on its own, and extracts gene names from a description
#' column.
#' @param file Character string with the path to the proteins.csv.
#' @param a A Character string representing the name of the first group of replicates.
#' @param b Character string representing the name of the second group of replicates.
#' @param columns.a Integer Vector with the indices of columns to be assigned the prefix a
#' @param columns.b Integer Vector with the indices of columns to be assigned the prefix b
#' @param accession.split Character Vector with the names of the separated accession columns. Defaults to c("sp", "Accession", "Name").
#'
#' @return dataframe
#' @export
#'
#' @examples
#' format_peaks("./proteins.csv", "Test", "Control", 15:17, 18:20, c("sp", "Accession", "Name"))
format_peaks <- function(file, a, b, columns.a, columns.b, accession.split = c("sp", "Accession", "Name")){
  protein_df <- readr::read_csv(file = file, na = "0")

  group.a <- paste0(a, "_", 1:length(columns.a))
  group.b <- paste0(b, "_", 1:length(columns.b))

  for (i in 1:length(group.a)){
    protein_df[group.a[i]] <- protein_df[columns.a[i]]
  }

  for (i in 1:length(group.b)){
    protein_df[group.b[i]] <- protein_df[columns.b[i]]
  }

  protein_df <- tidyr::separate(data = protein_df, col = Accession, into = accession.split, sep = "([|])")
  protein_df <-  dplyr::mutate(.data = protein_df, "Gene_name" = str_extract(Description, "GN=[^ ]+"),
                               "Gene_name" = str_extract(Gene_name, "[^=]+$"))
  return(protein_df)
}
