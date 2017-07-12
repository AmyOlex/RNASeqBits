#' Calculates the percent of gene expression for each listed species.
#'
#' This function calculates the percent of gene expression for each listed species base on the total read counts and the counts of reads belonging to each species.
#' This function requires that the gene names for each species be uniquily labeled.  For example, ENSG is human and ENSMUSG is mouse for Entre IDs.
#'
#' @param data A dataframe or matrix containing the data to use for calculating the percent of species expression.
#' @param species_df A dataframe containing each species name as a column name, and the string to use to identify each species uniquely in the gene names(row names of data matrix) is in row 1.
#' @keywords gene, expression, species percent
#' @return dataframe with one row per sample/condition and N columns with the percent of expression for the provided species.
#' @examples
#' species_df <- data.frame("Human"="ENSG", "Mouse"="ENSMUSG")
#' percents <- calc.percentSpeciesExpression(data, species_df)
#' @export calc.percentSpeciesExpression
#' @author Amy L. Olex \email{alolex@vcu.edu}
#'
calc.percentSpeciesExpression<- function(data, species_df){

  percents <- data.frame(row.names = names(data))
  for(s in row.names(percents)){
    totalMapped <- sum(data[,s])
    for(org in names(species_df)){
      org_genes <- grep(species_df[1,org], row.names(data))
      org_genes_mapped <- sum(data[org_genes,s])
      percents[s,paste("%",org)] <- org_genes_mapped/totalMapped
    }
  }

  return(percents)

}
