#' Takes in a dds DESeq2 object and extracts results for a specific contrast.
#'
#' This function was build for the repetitive contrast done in the Chuck Harrell Brain Metastasis project.
#' It takes in severla parameters, extracts results for a specified contrast, saves the data, plots and saves a MA Plot, and plots the expression values of the top DE gene.
#' Note: this assumes the dds condition column is named "condition" and the gene_features gene symbol column is named "Gene.Symbol".
#'
#' @param d
#' @keywords d
#' @return d
#' @examples
#' data <- analyze.contrast()
#' @export analyze.contrast
#' @import DESeq2
#' @author Amy L. Olex \email{alolex@vcu.edu}
#'

analyze.contrast <- function(dds, control_cond, experiment_cond, gene_features, contrast_name="", celltype="", species="unknown_species", date=date()){

  ## re-level dds so that the control is the reference
  dds$condition <- relevel(dds$condition, ref=control_cond)

  ## Get results for normal vs experiemnt
  res <- results(dds, contrast=c("condition",experiment_cond,control_cond))

  ## MA Plots
  jpeg(filename=paste("MAPlot",celltype,species,contrast_name,date,".jpg",sep="_"), width=800, height=600, units="px", res=100)
  plotMA(res, main=paste(celltype,"DESeq2",species,contrast_name,date,sep=" "))
  dev.off()

  ## Save Data
  degs <- as.data.frame(res)
  degs <- merge(gene_features, degs, by="row.names", all.x=FALSE, all.y=TRUE)

  write.table(degs, file=paste(species,"_DESeq2_",celltype,"_DEGs_",contrast_name,"_",date,".txt", sep=""), quote=FALSE, sep="\t", row.names=TRUE, col.names=TRUE)

  ## Sig Gene Plots
  top_gene_mnH <- degs[order(degs$padj),][1,]

  jpeg(filename=paste("GenePlot",celltype,species,contrast_name,top_gene_mnH$Gene.Symbol[1],date,".jpg",sep="_"), width=800, height=600, units="px", res=100)
  plotCounts(dds_human, gene=top_gene_mnH$Row.names[1], intgroup="condition",
             main=paste(celltype," ", contrast_name, "\nMost Significant DE ", species, " Gene: ",top_gene_mnH$Gene.Symbol[1],
                        "\n Log2 FC = ",round(top_gene_mnH$log2FoldChange[1], digits=2), " padj = ", format(top_gene_mnH$padj[1], digits=2), sep=""))
  dev.off()
}
