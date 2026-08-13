load(file = "~/lspm/inst/extdata/adapt/count2d0_result_adapt.Rdata")
load(file = "~/lspm/inst/extdata/adapt/count2d1_result_adapt.Rdata")
load(file = "~/lspm/inst/extdata/adapt/count2d5_result_adapt.Rdata")

source("~/lspm/inst/disp_network_c.R")

# open the pdf file
cairo_pdf("~/lspm/inst/extdata/adapt/figure/disp_alpha.pdf", width = 5, height = 4)

# alpha comparison --------------------------------------------------------

old.par <- par(no.readonly = TRUE) # save current layout setting
par(mfrow=c(1,1))
par(mar = c(4.6, 4.5, 1.5, 0.5))

# cbf_2 <- c("#000000", "#E69F00", "#56B4E9", "#009E73",
#            "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
# palette(cbf_2)
# custom_palette <- c("black", "red", "#f1a2a9", "#e49444", "#6a9f58", "#56B4E9")
custom_palette <- c("black", "red", "#984EA3", "#e49444", "#6a9f58", "#56B4E9")
palette(custom_palette)
mycol=adjustcolor(palette(), alpha.f = 0.2)
palette(mycol)

# palette(c("black"  , "red"  ,   "green3"  ,"blue"   , "cyan"  ,  "magenta" ,"yellow"  ,"gray"  )) # default palette

# d = 4
alpha_thin_mean <- c()
# tempalpha <- cbind(alphaaa2,c(result4d4_100_thin_alpha_mean,rep(NA,13)),alphaaa)
tempalpha=cbind(-10,-10,-10,-10)
# colnames(tempalpha) <- c("Auto", "2", "4", "10", "")
boxplot(tempalpha, xlab="Overdispersion" ,ylab=expression(paste("", alpha)), ylim=c(0,5.5), boxwex=0.4, xaxt="n")
axis(1, at=1:3, las=1, labels=c("Low", "Moderate", "High"), cex.axis=1)


for(d in 2:4){
  for(seed in seed_number) {
    if(names(which.max(table(count2d0_result_adapt[[paste0("seed",(seed))]]$iter_d))) != d){
      next
    } else{
      alpha_data = count2d0_result_adapt[[paste0("seed",(seed))]]$alpha[count2d0_result_adapt[[paste0("seed",(seed))]]$iter_d == d]
      vioplot(alpha_data, at=1,
              add=TRUE, cex=0.2, rectCol = NA, lineCol = NA,
              colMed = NA, border=NA, col=d+1)
    }
  }
}

for(d in 2:4){
  for(seed in seed_number) {
    if(names(which.max(table(count2d1_result_adapt[[paste0("seed",(seed))]]$iter_d))) != d){
      next
    } else{
      alpha_data = count2d1_result_adapt[[paste0("seed",(seed))]]$alpha[count2d1_result_adapt[[paste0("seed",(seed))]]$iter_d == d]
      vioplot(alpha_data, at=2,
              add=TRUE, cex=0.2, rectCol = NA, lineCol = NA,
              colMed = NA, border=NA, col=d+1)
    }
  }
}

for(d in 2:4){
  for(seed in seed_number) {
    if(names(which.max(table(count2d5_result_adapt[[paste0("seed",(seed))]]$iter_d))) != d){
      next
    } else{
      alpha_data = count2d5_result_adapt[[paste0("seed",(seed))]]$alpha[count2d5_result_adapt[[paste0("seed",(seed))]]$iter_d == d]
      vioplot(alpha_data, at=3,
              add=TRUE, cex=0.2, rectCol = NA, lineCol = NA,
              colMed = NA, border=NA, col=d+1)
    }
  }
}

points(c(0.5,1.5,5), pch=4, col="red", lwd=2, cex=2)
# abline(h=3, col='red', lwd=3) # true
# points(apply(alpha_thin_mean,2,mean),pch=20, cex=1.5, col="green")
# legend("topleft", legend=c('Estimated value', "True value", 'Thinned chain'), lty=c(1,1,1), col=c("orange","red", rgb(red = 1, green=0.5, blue=1)), lwd=3)
# title(xlab="(a)", line=2)
# palette(c("black"  , "red"  ,   "green3"  ,"blue"   , "cyan"  ,  "magenta" ,"yellow"  ,"gray"  )) # default palette
# palette(cbf_2)
# custom_palette <- c("black", "red", "#f1a2a9", "#e49444", "#6a9f58", "#56B4E9")
custom_palette <- c("black", "red", "#984EA3", "#e49444", "#6a9f58", "#56B4E9")
palette(custom_palette)
legend("topright", legend=c("True", expression(paste(p, " = 2")), expression(paste(p, " = 3")), expression(paste(p, " = 4"))),
       pch=c(NA, rep(20,4)), lty=c(1,rep(NA,4)), col=c("red",3:5), cex=1)

# Close the pdf file
dev.off()

