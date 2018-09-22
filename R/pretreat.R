pretreat <- function(countmatrix){
  hk <- countmatrix[rownames(countmatrix) %in% ensebl_id,]
  oth <- countmatrix[!(rownames(countmatrix) %in% ensebl_id),]
  hk <- hk[!rowSums(hk)==0,]
  oth <- oth[!rowSums(oth)==0,]
  result <- list(housekeeping=hk,nonhousekeeping=oth)
  return(result)
}
