load(file = "~/lspm/inst/extdata/binary/node50results3d5_0_thin.Rdata")
load(file = "~/lspm/inst/extdata/binary/node50results3d5_1_thin.Rdata")
load(file = "~/lspm/inst/extdata/binary/node50results3d5_5_thin.Rdata")
load(file = "~/lspm/inst/extdata/binary/node50results3d5_10_thin.Rdata")
load(file = "~/lspm/inst/extdata/binary/node50results3d5_20_thin.Rdata")
load(file = "~/lspm/inst/extdata/binary/node50results3d5_30_thin.Rdata")

source("~/lspm/inst/section4_3network.R")

library(vegan)

# open the pdf file
cairo_pdf("~/lspm/inst/figures/alpha4_3.pdf", width = 5.5, height = 4)

# alpha comparison --------------------------------------------------------

old.par <- par(no.readonly = TRUE) # save current layout setting
par(mfrow=c(1,1))
par(mar = c(4.1, 3.1, 4.1, .6))

alpha_thin_mean <- c()
# tempalpha <- cbind(alphaaa2,c(result4d4_100_thin_alpha_mean,rep(NA,13)),alphaaa)
tempalpha=cbind(-10,-10,-10,-10,-10,-10)
colnames(tempalpha) <- c("0", "1", "5", "10", "20","30")
boxplot(tempalpha, ylab="",
        xlab=expression(paste("True ", alpha, "")),
        ylim=c(-1,30), boxwex=0.4)
axis(3,1:6,labels=c("2-5%", "4-8%", "20-35%", "49-65%", "79-94%", "90-99%"))
title(xlab=expression(paste("Empirical network density")), line=-15)
title(ylab=expression(paste("", alpha)), line=2.2)

for(seed in seed_number) {
  vioplot(cbind(c(node50results3d5_0_thin[[paste0("seed",(seed))]]$alpha,rep(NA,29)),
                node50results3d5_1_thin[[paste0("seed",(seed))]]$alpha,
                node50results3d5_5_thin[[paste0("seed",(seed))]]$alpha,
                node50results3d5_10_thin[[paste0("seed",(seed))]]$alpha,
                node50results3d5_20_thin[[paste0("seed",(seed))]]$alpha,
                node50results3d5_30_thin[[paste0("seed",(seed))]]$alpha),
          add=TRUE, cex=0.2, rectCol = NA, lineCol = NA,
          colMed = NA, border=NA, col=rgb(red =0, green=0, blue=0,alpha=0.1))
  thin_mean <- cbind(mean(node50results3d5_0_thin[[paste0("seed",(seed))]]$alpha),
                     mean(node50results3d5_1_thin[[paste0("seed",(seed))]]$alpha),
                     mean(node50results3d5_5_thin[[paste0("seed",(seed))]]$alpha),
                     mean(node50results3d5_10_thin[[paste0("seed",(seed))]]$alpha),
                     mean(node50results3d5_20_thin[[paste0("seed",(seed))]]$alpha),
                     mean(node50results3d5_30_thin[[paste0("seed",(seed))]]$alpha))
  alpha_thin_mean <- rbind(alpha_thin_mean, thin_mean)
}
# boxplot(tempalpha,add=T, col="orange", boxwex=0.4)
# abline(h=3, col='red', lwd=4) # true
points(c(0,1,5,10,20,30), col='red', pch=4, cex=2, lwd=4) # true

points(apply(alpha_thin_mean,2,mean),pch=20, cex=1.5, col="green")
# legend("topleft", legend=c('Estimated value', "True value", 'Thinned chain'), lty=c(1,1,1), col=c("orange","red", rgb(red = 1, green=0.5, blue=1)), lwd=3)
# title(xlab="(a)", line=4)
legend("topleft", legend=c('Posterior distribution', "True value", 'Average posterior mean'), pch=c(20,4,20), col=c("black","red", "green"), cex=1)

# Close the pdf file
dev.off()

# Procrustes Correlation --------------------------------------------------

full_z_protest3d5_0 <- full_z_protest3d5_1 <- full_z_protest3d5_5 <- full_z_protest3d5_10 <- full_z_protest3d5_20 <- full_z_protest3d5_30 <- list()
for(seed in seed_number) {
  z_protest <- protest(node50results3d5_0_thin[[paste0("seed",(seed))]]$z_mean[,1:3], node50network3d5_0[[paste0("seed",(seed))]]$positions)
  full_z_protest3d5_0 <- append(full_z_protest3d5_0, list(temp=z_protest))
  names(full_z_protest3d5_0)[names(full_z_protest3d5_0)=="temp"] <- paste0("seed",(seed))

  z_protest <- protest(node50results3d5_1_thin[[paste0("seed",(seed))]]$z_mean[,1:3], node50network3d5_1[[paste0("seed",(seed))]]$positions)
  full_z_protest3d5_1 <- append(full_z_protest3d5_1, list(temp=z_protest))
  names(full_z_protest3d5_1)[names(full_z_protest3d5_1)=="temp"] <- paste0("seed",(seed))

  z_protest <- protest(node50results3d5_5_thin[[paste0("seed",(seed))]]$z_mean[,1:3], node50network3d5_5[[paste0("seed",(seed))]]$positions)
  full_z_protest3d5_5 <- append(full_z_protest3d5_5, list(temp=z_protest))
  names(full_z_protest3d5_5)[names(full_z_protest3d5_5)=="temp"] <- paste0("seed",(seed))

  z_protest <- protest(node50results3d5_10_thin[[paste0("seed",(seed))]]$z_mean[,1:3], node50network3d5_10[[paste0("seed",(seed))]]$positions)
  full_z_protest3d5_10 <- append(full_z_protest3d5_10, list(temp=z_protest))
  names(full_z_protest3d5_10)[names(full_z_protest3d5_10)=="temp"] <- paste0("seed",(seed))

  z_protest <- protest(node50results3d5_20_thin[[paste0("seed",(seed))]]$z_mean[,1:3], node50network3d5_20[[paste0("seed",(seed))]]$positions)
  full_z_protest3d5_20 <- append(full_z_protest3d5_20, list(temp=z_protest))
  names(full_z_protest3d5_20)[names(full_z_protest3d5_20)=="temp"] <- paste0("seed",(seed))

  z_protest <- protest(node50results3d5_30_thin[[paste0("seed",(seed))]]$z_mean[,1:3], node50network3d5_30[[paste0("seed",(seed))]]$positions)
  full_z_protest3d5_30 <- append(full_z_protest3d5_30, list(temp=z_protest))
  names(full_z_protest3d5_30)[names(full_z_protest3d5_30)=="temp"] <- paste0("seed",(seed))

}


z_protest_b3 <- c()
for(seed in seed_number) {
  z_protest_temp <- cbind(sum(full_z_protest3d5_0[[paste0("seed",(seed))]]$scale),
                          sum(full_z_protest3d5_1[[paste0("seed",(seed))]]$scale),
                          sum(full_z_protest3d5_5[[paste0("seed",(seed))]]$scale),
                          sum(full_z_protest3d5_10[[paste0("seed",(seed))]]$scale),
                          sum(full_z_protest3d5_20[[paste0("seed",(seed))]]$scale),
                          sum(full_z_protest3d5_30[[paste0("seed",(seed))]]$scale))
  z_protest_b3 <- rbind(z_protest_b3, z_protest_temp)
}

# open the pdf file
cairo_pdf("~/lspm/inst/figures/protest4_3.pdf", width = 5.5, height = 4)
par(mar = c(4.1, 3.1, 4.1, .6))

colnames(z_protest_b3) <- c("0", "1", "5", "10", "20","30")
boxplot(z_protest_b3, ylab="",
        xlab=expression(paste("True ", alpha, "")), ylim=c(0,1))
axis(3,1:6,labels=c("2-5%", "4-8%", "20-35%", "49-65%", "79-94%", "90-99%"))
title(xlab=expression(paste("Empirical network density")), line=-15)
title(ylab=expression(paste("Procrustes correlation")), line=2)
# title(xlab="(b)", line=4)

par(old.par) # restore previous plot layout setting

# Close the pdf file
dev.off()

par(mar = c(5.1, 4.1, 4.1, 2.1))
