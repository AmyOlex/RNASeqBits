#' Creates an annotated heatmap from DeepTools multiBamSummary tab delimited output.
#'
#' This function dreated a heatmap from the DeepTools multiBamSummary method, and adds color coded annotation bars to the heatmap to annotation samples.
#' The file is formatted with the following fields: #'chr'	'start'	'end'	'sample1_name.bam' 'sample2_name.bam' ... 'sampleN_name.bam'
#'
#' @param deep_file The path and name of the file containing the DeepTools tab delimited results from the multiBamSummary method.
#' @param annot_file The path and file name of the tab delimited text file containing the sample annotations to use for adding colored annotation bars on the heatmap.
#' @keywords deeptools, import, heatmap
#' @return A data frame of bin counts from the multiBamSummary output, and a data frame with annotations.
#' @examples
#' data <- deep.heatmap(deep_file="counts.tab", annot_file="annots.txt")
#' @export deep.heatmap
#' @import NMF
#' @author Amy L. Olex \email{alolex@vcu.edu}
#'

deep.heatmap <- function(deep_file, annot_file, image_name){

  ## Load in the raw counts from multiBamSummary
  data <- read.delim(file = deep_file, header=TRUE, quote="\"'")
  # remove first 3 columns from data matrix
  data <- data[,4:ncol(data)]

  annot <- read.delim(file = annot_file, header=TRUE, row.names = 1, quote="\"'")
  # Make names out of the row.names
  row.names(annot) <- make.names(row.names(annot))

  ## Sort the data matrix to be in the same order as the annotations.
  data2 <- data[,row.names(annot)]

  deep_cor <- cor(data2)

  my_colors <- convert.to.colors(annot)

  jpeg(filename=image_name, width=2500, height=2500, res=150, pointsize=5)

  aheatmap(deep_cor, distfun="euclidean", hclustfun="average", main=paste("UCEC-BRCA DeepTools Correlation Heatmap", sep=""), scale="none", annCol=annot, annRow=annot, annColors=my_colors,
           fontsize=10, cexRow=2, treeheight=200)

  dev.off()

  return(list(data, annot))
}
