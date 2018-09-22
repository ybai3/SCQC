## view the distribution of beta

plotbeta <- function(beta_oth_fdr,beta_hk_fdr){
  plot(density(beta_oth_fdr[1,]),xlab = "Coefficient of library size",main=NULL)
  lines(density(beta_hk_fdr[1,]),col="red")
  Mode = function(x){ 
    ta = table(x)
    tam = max(ta)
    if (all(ta == tam))
      mod = NA
    else
      if(is.numeric(x))
        mod = as.numeric(names(ta)[ta == tam])
    else
      mod = names(ta)[ta == tam]
    return(mod)
  }
  abline(v=Mode(round(beta_hk_fdr[1,],6)),col="red")
  abline(v=Mode(round(beta_oth_fdr[1,],6)))
}
