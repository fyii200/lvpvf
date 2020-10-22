
# Fabian Yii, Paul Artes
# for work on the Variability Index etc

rm(list=ls())
setwd("/Users/fabianyii/Desktop/LVPEI-Outliers")


# install.packages("devtools")
# install_github("imarinfr/vf1/source")
library(devtools)
library("visualFields")

lvdat    <- read.csv( "data/LVPFirst100Data_processed.2.csv", stringsAsFactors = FALSE )
lvdat$date <- as.Date(lvdat$date, "%m/%d/%y")

spa <- function(a) {
  par(mfrow=c(3,2))
  
  plot.new()
  legend("center", cex=2.5, bty="n",pch=19, legend = c( paste0("Patient ID: ", a$id[1]), 
                                                        paste0("Date From: ",min(a$date) ),
                                                        paste("Date To: ", max(a$date) ),
                                                        paste0("Age: ", min(a$age),"", "-", " ",max(a$age), "y/o" ) )
  )
  plot.new()
  legend("center", cex=2.5,title = "PoPLR Analysis", bg="yellow",legend = c(paste0("Fisher's S-statistic: ",round(poplr(a, type="td")$csl, digits=2) ),
                                                                            paste0 ("Fisher's S P-value: ",round(poplr(a, type="td")$cslp, digits=1),"%" ))
  )
  
  vfplot(vfmean(vfselect (a, sel = "first", n=3), by="eye"), type="td", ps=16)
  legend("topleft", legend ="Mean TD: First 3 Tests", bty="n", text.font=2, cex=1.5)
  vfplot(vfmean(vfselect(a, sel="last", n=3), by="eye"), type="td", ps=16)
  legend("topleft", legend ="Mean TD: Last 3 Tests", bty="n", text.font=2, cex=1.5)
  
  vflegoplot(a, type = "s", ps=16)
  legend("topleft", legend ="Sensitivity Change", bty="n", text.font=2, cex=1.5, inset=0.03)
  
  vfplotplr(a, type="td", ps=16)
  legend("topleft", legend ="PLR", bty="n", text.font=2, cex=1.5, inset=0.15)
  vfplotsparklines(a, type="td",add=TRUE) }


pdf (file = "plots.pdf", width = 14, height = 14)

plot_res <- unique(data.frame(id = lvdat$id, eye = lvdat$eye))


for(i in 1:length(plot_res$id)) {
  idx <- which(lvdat$id == plot_res$id[i] & lvdat$eye == plot_res$eye[i])
  a <- lvdat[idx[1:length(idx)], 2:65]
  if (length(a$id>7)){
    spa (a) }
}

dev.off()