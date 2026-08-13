load(file = "~/lspm/inst/extdata/count/node20results_c_thin.Rdata")
load(file = "~/lspm/inst/extdata/count/node50results_c_thin.Rdata")
load(file = "~/lspm/inst/extdata/count/node100results_c_thin.Rdata")
load(file = "~/lspm/inst/extdata/count/node200results_c_thin.Rdata")

source("~/lspm/inst/section4_1network_c.R")

library(vegan)

# open the pdf file
cairo_pdf("~/lspm/inst/figures/alpha4_1c.pdf", width = 5, height = 4)

# alpha comparison --------------------------------------------------------

old.par <- par(no.readonly = TRUE) # save current layout setting
par(mfrow=c(1,1))

alpha_thin_mean <- c()
# tempalpha <- cbind(alphaaa2,c(result4d4_100_thin_alpha_mean,rep(NA,13)),alphaaa)
tempalpha=cbind(0,0,0,0)
colnames(tempalpha) <- c("20", "50", "100", "200")
boxplot(tempalpha, xlab="n",ylab=expression(paste("", alpha)), ylim=c(2.5,4.5), boxwex=0.4)
for(seed in seed_number) {
  vioplot(cbind(node20results_c_thin[[paste0("seed",(seed))]]$alpha,
                node50results_c_thin[[paste0("seed",(seed))]]$alpha,
                c(node100results_c_thin[[paste0("seed",(seed))]]$alpha,rep(NA,75)),
                c(node200results_c_thin[[paste0("seed",(seed))]]$alpha,rep(NA,125))),
          add=TRUE, cex=0.2, rectCol = NA, lineCol = NA,
          colMed = NA, border=NA, col=rgb(red =0, green=0, blue=0,alpha=0.1))
  thin_mean <- cbind(mean(node20results_c_thin[[paste0("seed",(seed))]]$alpha),
                     mean(node50results_c_thin[[paste0("seed",(seed))]]$alpha),
                     mean(node100results_c_thin[[paste0("seed",(seed))]]$alpha),
                     mean(node200results_c_thin[[paste0("seed",(seed))]]$alpha))
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

# Initialise the pdf
cairo_pdf("~/lspm/inst/figures/protest4_1c.pdf", width = 5, height = 4)

# Procrustes Correlation --------------------------------------------------

full_z_protest20_c <- full_z_protest50_c <- full_z_protest100_c <- full_z_protest200_c <- list()
for(seed in seed_number) {
  z_protest <- protest(node20results_c_thin[[paste0("seed",(seed))]]$z_mean[,1:2], node20network_c[[paste0("seed",(seed))]]$positions)
  full_z_protest20_c <- append(full_z_protest20_c, list(temp=z_protest))
  names(full_z_protest20_c)[names(full_z_protest20_c)=="temp"] <- paste0("seed",(seed))

  z_protest <- protest(node50results_c_thin[[paste0("seed",(seed))]]$z_mean[,1:2], node50network_c[[paste0("seed",(seed))]]$positions)
  full_z_protest50_c <- append(full_z_protest50_c, list(temp=z_protest))
  names(full_z_protest50_c)[names(full_z_protest50_c)=="temp"] <- paste0("seed",(seed))

  z_protest <- protest(node100results_c_thin[[paste0("seed",(seed))]]$z_mean[,1:2], node100network_c[[paste0("seed",(seed))]]$positions)
  full_z_protest100_c <- append(full_z_protest100_c, list(temp=z_protest))
  names(full_z_protest100_c)[names(full_z_protest100_c)=="temp"] <- paste0("seed",(seed))

  z_protest <- protest(node200results_c_thin[[paste0("seed",(seed))]]$z_mean[,1:2], node200network_c[[paste0("seed",(seed))]]$positions)
  full_z_protest200_c <- append(full_z_protest200_c, list(temp=z_protest))
  names(full_z_protest200_c)[names(full_z_protest200_c)=="temp"] <- paste0("seed",(seed))

}


z_protest_c1 <- c()
for(seed in seed_number) {
  z_protest_temp <- cbind(sum(full_z_protest20_c[[paste0("seed",(seed))]]$scale),
                          sum(full_z_protest50_c[[paste0("seed",(seed))]]$scale),
                          sum(full_z_protest100_c[[paste0("seed",(seed))]]$scale),
                          sum(full_z_protest200_c[[paste0("seed",(seed))]]$scale))
  z_protest_c1 <- rbind(z_protest_c1, z_protest_temp)
}
colnames(z_protest_c1) <- c("20", "50", "100", "200")
boxplot(z_protest_c1, xlab="n", ylab="Procrustes correlation", ylim=c(0,1))
# title(xlab="(b)", line=2)

par(old.par) # restore previous plot layout setting

# Close the pdf file
dev.off()
