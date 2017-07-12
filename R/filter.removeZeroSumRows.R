#' Removes all rows of a matrix or dataframe where the row sum is zero.
#'
#' This function reduces an RNAseq data set by removing all rows that sum to zero.  These rows represent genes that were not expressed in any sample/condition.
#'
#' @param data A dataframe or matrix containing the data to reduce.
#' @keywords gene, expression, filter
#' @return A matrix of raw read counts without rows that summed to zero
#' @examples
#' data <- filter.removeZeroSumRows(data)
#' @export filter.removeZeroSumRows
#' @author Amy L. Olex \email{alolex@vcu.edu}
#'
filter.removeZeroSumRows<- function(data){
  ## Remove all rows where the sum is zero
  return(data[which(rowSums(data)!=0),])
}
