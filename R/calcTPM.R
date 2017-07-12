#' Calculates the TPM values for RNA-seq data.
#'
#' This function uses the method described at () to calculate the TPM (Transcripts Per Million) values from raw RNA-seq read counts.
#'
#' @param data A matrix of read count data with genes as rows and samples as columns.
#' @param feature_length A data frame with the gene lengths for each feature/gene in the read count matrix. The feature length column must be labeled as "Length".
#' @keywords tpm, gene, expression
#' @return A matrix of TPM expression values.
#' @examples
#' tpm <- calc.tpm(data, features)
#' @export calc.tpm
#' @author Amy L. Olex \email{alolex@@vcu.edu}
#'
calc.tpm <- function(data, feature_length){

  ##Calculate the RPK value
  RPK <- matrix(0, nrow=dim(data)[1], ncol=dim(data)[2])

  for(row in 1:dim(data)[1]){
    for(col in 1:dim(data)[2]){
      RPK[row,col] <- data[row,col]/feature_length$Length[row]
    }
  }

  ##Calculate the sums of each column and divide by 1000000
  scale_factor <- colSums(RPK)/1000000

  ##Now divide all values in each column by the scaling factor
  TPM <- t(t(RPK)/scale_factor)
  colnames(TPM) <- names(data)
  row.names(TPM) <- row.names(data)
  return(as.data.frame(TPM))
}
