load(file = "~/lspm/inst/extdata/adapt/node50results3d5_0_adapt.Rdata")
load(file = "~/lspm/inst/extdata/adapt/node50results3d5_1_adapt.Rdata")
load(file = "~/lspm/inst/extdata/adapt/node50results3d5_5_adapt.Rdata")
load(file = "~/lspm/inst/extdata/adapt/node50results3d5_10_adapt.Rdata")
load(file = "~/lspm/inst/extdata/adapt/node50results3d5_20_adapt.Rdata")
load(file = "~/lspm/inst/extdata/adapt/node50results3d5_30_adapt.Rdata")

source("~/lspm/inst/dens_network.R")

# open the pdf file
cairo_pdf("~/lspm/inst/extdata/adapt/figure/dens_alpha.pdf", width = 5, height = 4)

par(mar = c(4.6, 3.3, 3.6, 0.5)) # right margin set as 0.1

# alpha comparison --------------------------------------------------------

old.par <- par(no.readonly = TRUE) # save current layout setting
par(mfrow=c(1,1))

# cbf_2 <- c("#000000", "#E69F00", "#56B4E9", "#009E73",
#            "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
# custom_palette <- c("black", "red", "#f1a2a9", "#e49444", "#6a9f58", "#56B4E9")
custom_palette <- c("black", "red", "#984EA3", "#e49444", "#6a9f58", "#56B4E9")
palette(custom_palette)
# palette(c("black"  , "red"  ,   "green3"  ,"blue"   , "cyan"  ,  "magenta" ,"yellow"  ,"gray"  )) # default palette

mycol=adjustcolor(palette(), alpha.f = 0.2)
palette(mycol)

# palette(c("black"  , "red"  ,   "green3"  ,"blue"   , "cyan"  ,  "magenta" ,"yellow"  ,"gray"  )) # default palette

# d = 4
alpha_thin_mean <- c()
# tempalpha <- cbind(alphaaa2,c(result4d4_100_thin_alpha_mean,rep(NA,13)),alphaaa)
tempalpha=cbind(-9,-9,-9,-9,-9,-9,-9)
# colnames(tempalpha) <- c("Auto", "2", "4", "10", "")
boxplot(tempalpha, xlab=expression(paste("True ", alpha)) ,ylab="", ylim=c(-3,60), boxwex=0.4, xaxt="n")
axis(1, at=1:6, las=1, labels=c("0", "1", "5", "10", "20", "30"), cex.axis=1)
axis(3,1:6,labels=c("2-5%", "4-8%", "20-35%", "49-65%", "79-94%", "90-99%"), cex.axis=0.75)
title(main=expression(paste("Empirical network density")), line=2.6, cex.main=1.1)
title(ylab=expression(paste("", alpha)), line=2.2)

for(d in 2:4){
  for(seed in seed_number) {
    if(names(which.max(table(node50results3d5_0_adapt[[paste0("seed",(seed))]]$iter_d))) != d){
      next
    } else{
      alpha_data = node50results3d5_0_adapt[[paste0("seed",(seed))]]$alpha[node50results3d5_0_adapt[[paste0("seed",(seed))]]$iter_d == d]
      vioplot(alpha_data, at=1,
              add=TRUE, cex=0.2, rectCol = NA, lineCol = NA,
              colMed = NA, border=NA, col=d+1)
    }
  }
}

for(d in 2:4){
  for(seed in seed_number) {
    if(names(which.max(table(node50results3d5_1_adapt[[paste0("seed",(seed))]]$iter_d))) != d){
      next
    } else{
      alpha_data = node50results3d5_1_adapt[[paste0("seed",(seed))]]$alpha[node50results3d5_1_adapt[[paste0("seed",(seed))]]$iter_d == d]
      vioplot(alpha_data, at=2,
              add=TRUE, cex=0.2, rectCol = NA, lineCol = NA,
              colMed = NA, border=NA, col=d+1)
    }
  }
}

for(d in 2:4){
  for(seed in seed_number) {
    if(names(which.max(table(node50results3d5_5_adapt[[paste0("seed",(seed))]]$iter_d))) != d){
      next
    } else{
      alpha_data = node50results3d5_5_adapt[[paste0("seed",(seed))]]$alpha[node50results3d5_5_adapt[[paste0("seed",(seed))]]$iter_d == d]
      vioplot(alpha_data, at=3,
              add=TRUE, cex=0.2, rectCol = NA, lineCol = NA,
              colMed = NA, border=NA, col=d+1)
    }
  }
}

for(d in 2:4){
  for(seed in seed_number) {
    if(names(which.max(table(node50results3d5_10_adapt[[paste0("seed",(seed))]]$iter_d))) != d){
      next
    } else{
      alpha_data = node50results3d5_10_adapt[[paste0("seed",(seed))]]$alpha[node50results3d5_10_adapt[[paste0("seed",(seed))]]$iter_d == d]
      vioplot(alpha_data, at=4,
              add=TRUE, cex=0.2, rectCol = NA, lineCol = NA,
              colMed = NA, border=NA, col=d+1)
    }
  }
}

for(d in 2:4){
  for(seed in seed_number) {
    if(names(which.max(table(node50results3d5_20_adapt[[paste0("seed",(seed))]]$iter_d))) != d){
      next
    } else{
      alpha_data = node50results3d5_20_adapt[[paste0("seed",(seed))]]$alpha[node50results3d5_20_adapt[[paste0("seed",(seed))]]$iter_d == d]
      vioplot(alpha_data, at=5,
              add=TRUE, cex=0.2, rectCol = NA, lineCol = NA,
              colMed = NA, border=NA, col=d+1)
    }
  }
}

for(d in 2:4){
  for(seed in seed_number) {
    if(names(which.max(table(node50results3d5_30_adapt[[paste0("seed",(seed))]]$iter_d))) != d){
      next
    } else{
      alpha_data = node50results3d5_30_adapt[[paste0("seed",(seed))]]$alpha[node50results3d5_30_adapt[[paste0("seed",(seed))]]$iter_d == d]
      vioplot(alpha_data, at=6,
              add=TRUE, cex=0.2, rectCol = NA, lineCol = NA,
              colMed = NA, border=NA, col=d+1)
    }
  }
}


points(c(0,1,5,10,20,30), col='red', pch=4, cex=2) # true
# points(apply(alpha_thin_mean,2,mean),pch=20, cex=1.5, col="green")
# legend("topleft", legend=c('Estimated value', "True value", 'Thinned chain'), lty=c(1,1,1), col=c("orange","red", rgb(red = 1, green=0.5, blue=1)), lwd=3)
# title(xlab="(a)", line=2)
# palette(c("black"  , "red"  ,   "green3"  ,"blue"   , "cyan"  ,  "magenta" ,"yellow"  ,"gray"  )) # default palette
# palette(cbf_2)
# custom_palette <- c("black", "red", "#f1a2a9", "#e49444", "#6a9f58", "#56B4E9")
custom_palette <- c("black", "red", "#984EA3", "#e49444", "#6a9f58", "#56B4E9")
palette(custom_palette)
legend("topright", legend=c("True", expression(paste(p, " = 2")), expression(paste(p, " = 3")), expression(paste(p, " = 4"))),
       pch=c(4, rep(20,4)), col=c("red",3:5), cex=0.9)

# Close the pdf file
dev.off()

