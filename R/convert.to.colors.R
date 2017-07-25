#' Converts a data frame of factors into a list of numerical vectors representing colors.
#'
#' This function converts the input data frame to a list of numerical vectors that can be input into the aheatmap() function for column and row annotations.
#'
#' @param annot.df A data frame where each column is a factor.
#' @keywords color, annotation
#' @return A list of vectors with numbers representing colors, one list item per column in the input data frame.
#' @examples
#' my_colors <- convert.to.colors(annotation.df)
#' @export convert.to.colors
#' @author Amy L. Olex \email{alolex@vcu.edu}
#'

convert.to.colors <- function(annot.df){
  return( lapply(lapply(annot.df, function(x){levels(as.factor(x))}), function(x){if(length(x)<=6){seq(from=1, to=length(x))}else{rainbow(length(x))} }) )
}
