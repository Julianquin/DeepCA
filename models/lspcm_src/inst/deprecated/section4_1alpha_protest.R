load(file = "~/lspm/inst/extdata/binary/node20results_thin.Rdata")
load(file = "~/lspm/inst/extdata/binary/node50results_thin.Rdata")
load(file = "~/lspm/inst/extdata/binary/node100results_thin.Rdata")
load(file = "~/lspm/inst/extdata/binary/node200results_thin.Rdata")

library(vegan)

# open the pdf file
cairo_pdf("~/lspm/inst/figures/alpha4_1.pdf", width = 5, height = 4)

# alpha comparison --------------------------------------------------------

old.par <- par(no.readonly = TRUE) # save current layout setting
par(mfrow=c(1,1))

alpha_thin_mean <- c()
# tempalpha <- cbind(alphaaa2,c(result4d4_100_thin_alpha_mean,rep(NA,13)),alphaaa)
tempalpha=cbind(0,0,0,0)
colnames(tempalpha) <- c("20", "50", "100", "200")
boxplot(tempalpha, xlab="n",ylab=expression(paste("", alpha)), ylim=c(1,11.5), boxwex=0.4)
for(seed in seed_number) {
  vioplot(cbind(node20results_thin[[paste0("seed",(seed))]]$alpha,
                node50results_thin[[paste0("seed",(seed))]]$alpha,
                c(node100results_thin[[paste0("seed",(seed))]]$alpha,rep(NA,75)),
                c(node200results_thin[[paste0("seed",(seed))]]$alpha,rep(NA,75))),
          add=TRUE, cex=0.2, rectCol = NA, lineCol = NA,
          colMed = NA, border=NA, col=rgb(red =0, green=0, blue=0,alpha=0.1))
  thin_mean <- cbind(mean(node20results_thin[[paste0("seed",(seed))]]$alpha),
                     mean(node50results_thin[[paste0("seed",(seed))]]$alpha),
                     mean(node100results_thin[[paste0("seed",(seed))]]$alpha),
                     mean(node200results_thin[[paste0("seed",(seed))]]$alpha))
  alpha_thin_mean <- rbind(alpha_thin_mean, thin_mean)
}
# boxplot(tempalpha,add=T, col="orange", boxwex=0.4)
abline(h=3, col='red', lwd=4) # true
points(apply(alpha_thin_mean,2,mean),pch=20, cex=2, col="green")
# legend("topleft", legend=c('Estimated value', "True value", 'Thinned chain'), lty=c(1,1,1), col=c("orange","red", rgb(red = 1, green=0.5, blue=1)), lwd=3)
# title(xlab="(a)", line=2)
legend("topright", legend=c('Posterior distribution', "True value", 'Average posterior mean'), pch=c(20,NA,20), lty=c(NA,1,NA), col=c("black","red", "green"), cex=1)

# Close the pdf file
dev.off()

# open the pdf file
cairo_pdf("~/lspm/inst/figures/protest4_1.pdf", width = 5, height = 4)

# Procrustes Correlation --------------------------------------------------

full_z_protest20 <- full_z_protest50 <- full_z_protest100 <- full_z_protest200 <- list()
for(seed in seed_number) {
  z_protest <- protest(node20results_thin[[paste0("seed",(seed))]]$z_mean[,1:2], node20network[[paste0("seed",(seed))]]$positions)
  full_z_protest20 <- append(full_z_protest20, list(temp=z_protest))
  names(full_z_protest20)[names(full_z_protest20)=="temp"] <- paste0("seed",(seed))

  z_protest <- protest(node50results_thin[[paste0("seed",(seed))]]$z_mean[,1:2], node50network[[paste0("seed",(seed))]]$positions)
  full_z_protest50 <- append(full_z_protest50, list(temp=z_protest))
  names(full_z_protest50)[names(full_z_protest50)=="temp"] <- paste0("seed",(seed))

  z_protest <- protest(node100results_thin[[paste0("seed",(seed))]]$z_mean[,1:2], node100network[[paste0("seed",(seed))]]$positions)
  full_z_protest100 <- append(full_z_protest100, list(temp=z_protest))
  names(full_z_protest100)[names(full_z_protest100)=="temp"] <- paste0("seed",(seed))

  z_protest <- protest(node200results_thin[[paste0("seed",(seed))]]$z_mean[,1:2], node200network[[paste0("seed",(seed))]]$positions)
  full_z_protest200 <- append(full_z_protest200, list(temp=z_protest))
  names(full_z_protest200)[names(full_z_protest200)=="temp"] <- paste0("seed",(seed))

}


z_protest_b1 <- c()
for(seed in seed_number) {
  z_protest_temp <- cbind(sum(full_z_protest20[[paste0("seed",(seed))]]$scale),
                          sum(full_z_protest50[[paste0("seed",(seed))]]$scale),
                          sum(full_z_protest100[[paste0("seed",(seed))]]$scale),
                          sum(full_z_protest200[[paste0("seed",(seed))]]$scale))
  z_protest_b1 <- rbind(z_protest_b1, z_protest_temp)
}
colnames(z_protest_b1) <- c("20", "50", "100", "200")
boxplot(z_protest_b1, xlab="n", ylab="Procrustes correlation", ylim=c(0,1))
# title(xlab="(b)", line=2)

par(old.par) # restore previous plot layout setting

# Close the pdf file
dev.off()

