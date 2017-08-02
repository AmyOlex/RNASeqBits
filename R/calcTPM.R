#' Calculates the TPM values for RNA-seq data.
#'
#' This function uses the method described at () to calculate the TPM (Transcripts Per Million) values from raw RNA-seq read counts.
#'
#' @param data A matrix of read count and gene length data with genes as rows and samples as columns.  The very first column of the matrix MUST be the gene lengths, and MUST be named "Lengths".
#' @keywords tpm, gene, expression
#' @return A matrix of TPM expression values.
#' @examples
#' tpm <- calc.tpm(data, features)
#' @export calc.tpm
#' @author Amy L. Olex \email{alolex@@vcu.edu}
#'
calc.tpm <- function(data){

  if(!("Length" %in% names(data))){
    stop("Error: column name 'Length' not found in names(data)")
  }
  feature_length <- data[,"Length",drop=FALSE]
  counts <- data[,!(names(data)=="Length"),drop=FALSE]

  ##Calculate the RPK value
  RPK <- matrix(0, nrow=dim(counts)[1], ncol=dim(counts)[2])

  for(row in 1:dim(counts)[1]){
    for(col in 1:dim(counts)[2]){
      RPK[row,col] <- counts[row,col]/feature_length$Length[row]
    }
  }

  ##Calculate the sums of each column and divide by 1000000
  scale_factor <- colSums(RPK)/1000000

  ##Now divide all values in each column by the scaling factor
  TPM <- t(t(RPK)/scale_factor)
  colnames(TPM) <- names(counts)
  row.names(TPM) <- row.names(counts)
  return(as.data.frame(TPM))
}
