## function to random select, identify hk and nhk, calculate cor coef
corcoef_identify <- function(countmatrix,rep){
  cor_coef_hk <- numeric(length=rep)
  cor_coef_oth <- numeric(length = rep)
  lib_size_hk <- numeric(length=rep)
  lib_size_oth <- numeric(length=rep)
  for (i in 1:rep){
    ## random sample
    index <- sample(dim(countmatrix)[2],2)
    sampled <- countmatrix[,index]
    ## identify
    hk <- sampled[rownames(sampled) %in% ensebl_id,]
    oth <- sampled[!(rownames(sampled) %in% ensebl_id),]
    ## calculate
    cor_coef_hk[i] <- cor(x=hk[,1],y=hk[,2],method = "spearman")
    lib_size_hk[i] <- mean(colSums(sampled))
    cor_coef_oth[i] <- cor(x=oth[,1],y=oth[,2],method = "spearman")
    lib_size_oth[i] <- mean(colSums(sampled))
  }
  result_hk <- c(cor_coef_hk,mean(lib_size_hk))
  names(result_hk) <- c(rep("random_cor_coef",rep),"mean_lib_size")
  result_oth <- c(cor_coef_oth,mean(lib_size_oth))
  names(result_oth) <- c(rep("random_cor_coef",rep),"mean_lib_size")
  result <- cbind(result_hk,result_oth)
  colnames(result) <- c("hk","oth")
  return(result)
}
