corcoefplot <- function(cor_coef_result){
  hk_cor_coef <- numeric(length = 100*21)
  oth_cor_coef <- numeric(length=100*21)
  for (i in 1:21){
    hk_cor_coef[(100*(i-1)+1):(100*i)] <- cor_coef_result[[i]][1:100,1]
    oth_cor_coef[(100*(i-1)+1):(100*i)] <- cor_coef_result[[i]][1:100,2]
  }
  hk_mean_libsize <- numeric(length = 100*21)
  oth_mean_libsize <- numeric(length= 100*21)
  for (i in 1:21){
    hk_mean_libsize[(100*(i-1)+1):(100*i)] <- rep(cor_coef_result[[i]][101,1],100)
    oth_mean_libsize[(100*(i-1)+1):(100*i)] <- rep(cor_coef_result[[i]][101,2],100)
  }
  boxdata_hk <- cbind(hk_cor_coef,hk_mean_libsize)
  boxdata_oth <- cbind(oth_cor_coef,oth_mean_libsize)
  colnames(boxdata_hk) <- c("corcoef","mean_libsize")
  colnames(boxdata_oth) <- c("corcoef","mean_libsize")
  
  boxdata <- rbind(boxdata_hk,boxdata_oth)
  boxdata <- cbind(boxdata,c(rep("HK",2100),rep("NHK",2100)))
  colnames(boxdata)[3] <- "Housekeeping"
  boxdata <- as.data.frame(boxdata)
  boxdata$corcoef <- as.vector(boxdata$corcoef)
  boxdata$corcoef <- as.numeric(boxdata$corcoef)
  boxdata$mean_libsize <- as.vector(boxdata$mean_libsize)
  boxdata$mean_libsize <- as.numeric(boxdata$mean_libsize)
  boxdata$Housekeeping <- as.vector(boxdata$Housekeeping)
  boxdata$Housekeeping <- as.character(boxdata$Housekeeping)
  
  colhk <- "#00207040"
  coloth <- "#70500040"

    boxplot(boxdata$corcoef~boxdata$Housekeeping*boxdata$mean_libsize,
          data = boxdata,notch=T,col=c(colhk,coloth),
          xlab="Library Size",ylab="Correlation Coefficient")
  legend ("topleft"  , c ("House-keeping genes", "Non-housekeeping genes"), 
          fill =c(colhk,coloth) )
}
