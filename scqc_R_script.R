# The following R script is an example workflow for cleaning a raw single cell RNA-seq dataset.
# We used an example data "snu" to show the workflow.
# For more details, please see the description at https://github.com/ybai3/SCQC/blob/master/DESCRIPTION.

library(devtools)

#install pacakge from Github

install_github("ybai3/SCQC")

#load the package

library(SCQC)

#load the example data, snu, which is a count matrix with dimension 33,020 genes x 5,951 libraries

data("ensebl_id")

data("snu")

dim(snu)

# [1] 33020  5951

# step 1: remove all 0 count genes from the data, separate the data into housekeeping gene part
# and non-housekeeping gene part

a=pretreat(snu)

# > names(a)
# [1] "housekeeping"    "nonhousekeeping"

# > dim(a$housekeeping); dim(a$nonhousekeeping)
# [1] 3593 5951
# [1] 17362  5951

#step 2: genewise screening
# for details check the function description using ?genewise

hk= genewise (a$housekeeping); nhk=genewise(a$nonhousekeeping)

names(hk); names(nhk)

# > names(hk); names(nhk)
# [1] "beta_nb"          "beta_cleaned"     "fdr"              "countmatrix_rwcl"
# [1] "beta_nb"          "beta_cleaned"     "fdr"              "countmatrix_rwcl"

# > dim(hk$countmatrix_rwcl); dim(nhk$countmatrix_rwcl)
# [1] 3558 5951
# [1] 11376  5951

#here we removed 35 (=3593-3558) housekeeping genes, 5896 (=17362-11376) non-housekeeping genes

#step 3 library-wise screening

#combining selected housekeeping and non-housekeeping genes after step 2

b=rbind (hk$countmatrix_rwcl, nhk$countmatrix_rwcl)

#library-wise screening

c=libwise(b)

names(c)

# > names(c)
# [1] "cor_coef" "wilcox"

#generate a plot for comparing correlation coefficient between housekeeping and non-housekeeping genes
# across different bins of library sizes

corcoefplot(c$cor_coef)

# based on the comparison, suggest a library size cutoff, where below this cutoff, pairwise library-wise
# correlation for housekeeping genes is smaller (or not significantly higher) than non-housekeeping
# genes, and beyond this cutoff pairwise library-wise correlation of housekeeping genes is significantly
# higher than the non-housekeeping genes

suggest_cutoff(b, c$wilcox)

# > suggest_cutoff(b, c$wilcox)
# 9.75%
# 9831

# extract the libraries whose sizes are equal to or more than the library size cutoff, the resultant
# data is the final dataset cleaned at the both genewise and library-wise levels.

clean.data=b[, which (colSums(b)>=9831)]

# > dim(clean.data)
# [1] 14934  5371



