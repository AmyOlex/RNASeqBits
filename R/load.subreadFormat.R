#' Imports raw read counts from SudRead's featureCounts() formatted output.
#'
#' This function iterates through one or more text files that contain read counts obtained from SubRead's featureCounts() method.
#' Multiple files MUST have the same feature set with different sample columns.
#' This method extracts the count data and concatenates the matricies into one matrix.  It also maps the gene symbols, if provided, to the gene ID.
#' This method ASSUMES the first 5 columns of the data frame (not including the row names) are the annotation data for Chr, Start, End, Strand, and Length.It only keeps the information from the first file and deletes the first 5 columns of all the other files before concatenating.
#'
#' @param file_list A list of file names for concatenate together into a single read count matrix.
#' @param annotations Optional. Name of a file with the gene feature annotations in tab delim format. First column is the Gene Official Symbol and second column is the Gene ID present in the read count files.
#' @param annot_id Optional. The column number that contains the feature IDs that match the read count files.  The other column must be the wanted gene identifiers like official gene symbol. Default is 1.
#' @keywords gene, expression, import
#' @return A matrix of raw read counts.
#' @examples
#' data <- load.subreadFormat(file_list = c("file1","file2","file3"), annotations="file", annot_id=2)
#' @export load.subreadFormat
#' @author Amy L. Olex \email{alolex@vcu.edu}
#'

load.subreadFormat<- function(file_list, annotations=NA, annot_id=1){

  ## Load in the raw read counts from featureCounts method
  raw <- read.delim(file = file_list[1], header=TRUE, skip=1, row.names=1)

  if(length(file_list) > 1){
    for(n in 2:length(file_list)){
      tmp <- read.delim(file = file_list[n], header=TRUE, skip=1, row.names=1)

      if(all(row.names(raw)==row.names(tmp))){
        tmp2 <- cbind(raw,tmp[,-(1:5)])         # Note that this assumes the first 5 columns are identical in all files.  It also assumes these are the annotation columns, so only keeps the columns from the first file.
        raw <- tmp2
      }
    }
  }

  if(!is.na(annotations)){
    geneSym <- read.delim(file = annotations, header=FALSE, row.names=annot_id)
    names(geneSym) <- "Gene.Symbol"
    tmp3 <- merge(geneSym, raw, by="row.names", all.x=FALSE, all.y=TRUE)
    row.names(tmp3) <- tmp3$Row.names
    data <- tmp3[,-1]
    return(data)
  }
  else {
    return(raw)
  }


  return(data)
}
