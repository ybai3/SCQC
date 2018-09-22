libwise <- function(rcleaned_matrix,repitation=100){
  libsize_all <- colSums(rcleaned_matrix)
  quans <- sort(c(0,0.005,(seq(from=0.05,to=0.95,0.05)-0.0025),(seq(0.05,0.95,0.05)+0.0025),0.995,1))
  libsize_group <- quantile(libsize_all,probs=quans)
  grouped_all <- list()
  length(grouped_all) <- 21
  for (k in 1:21){
    grouped_all[[k]] <- rcleaned_matrix[,(libsize_all>=libsize_group[2*k-1])&(libsize_all<=libsize_group[2*k])]
  }
  cor_coef_result <- list()
  length(cor_coef_result) <- 21
  for(j in 1:21){
    cor_coef_result[[j]] <- corcoef_identify(grouped_all[[j]],rep = repitation)
  }
  wil_result <- numeric(length = 21)
  for (o in 1:21){
    wil_result[o] <- wilcox.test(cor_coef_result[[o]][1:100,1],cor_coef_result[[o]][1:100,2])$p.value
  }
  return(list(cor_coef=cor_coef_result,wilcox=wil_result))
}
