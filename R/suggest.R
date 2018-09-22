suggest_cutoff <- function(rcleaned_matrix,wilcox_p){
  x = 0
  i = 1
  while(x<=3){
    if(wilcox_p[i]<(1-(0.95)^(1/21))){
      x <- x+1
    }else{
      x <- 0
    }
    i <- i+1
  }
  quantile(colSums(rcleaned_matrix),probs=(seq(0,1,0.05)[i-4]-0.0025))
}