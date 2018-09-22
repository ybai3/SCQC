## input pretreated matrix, will return a list contaitning beta value, fdr value, cleaned beta value and genewise cleaned countmatrix

genewise <- function(submatrix,presetfdr=0.1){
  library(MASS)
  libsize <- colSums(submatrix)
  beta_nb <- apply(submatrix,1,function(gene,libsize_nb){
    dep=as.numeric(gene)
    model <- glm(dep ~ libsize_nb,negative.binomial(1),
                 data = as.data.frame(cbind(dep,libsize_nb)))
    result <- summary(model)$coefficients[2,c(1,4)]
    return(result)
  },libsize_nb=libsize)
  fdr = p.adjust(beta_nb[2,], method = "fdr", n = length(beta_nb[2,]))
  beta_nb_fdr=rbind(beta_nb, fdr)
  beta_nb_fdr <- beta_nb_fdr[,beta_nb_fdr[3,]<=presetfdr]
  beta_nb_fdr <- beta_nb_fdr[,beta_nb_fdr[1,]>0]
  countmatrix_rcl <- submatrix[rownames(submatrix) %in% colnames(beta_nb_fdr),]
  return(list(beta_nb=beta_nb,beta_cleaned=beta_nb_fdr,fdr=fdr,countmatrix_rwcl=countmatrix_rcl))
}
