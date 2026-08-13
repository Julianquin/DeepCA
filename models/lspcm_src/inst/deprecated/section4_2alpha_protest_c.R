load(file = "~/lspm/inst/extdata/count/node100results4d3_c_thin.Rdata")
load(file = "~/lspm/inst/extdata/count/node100results4d4_c_thin.Rdata")
load(file = "~/lspm/inst/extdata/count/node100results4d8_c_thin.Rdata")

source("~/lspm/inst/section4_2network_c.R")

library(vegan)

# open the pdf file
cairo_pdf("~/lspm/inst/figures/alpha4_2c.pdf", width = 5, height = 4)
par(mar = c(4.1, 4.1, 1.1, 1.1))

# alpha comparison --------------------------------------------------------


old.par <- par(no.readonly = TRUE) # save current layout setting
par(mfrow=c(1,1))
alpha_c_thin_mean <- c()
# tempalpha <- cbind(alphaaa2,c(result4d4_100_c_thin_alpha_mean,rep(NA,13)),alphaaa)
tempalpha=cbind(0,0,0)
colnames(tempalpha) <- c("3", "4", "8")
boxplot(tempalpha, xlab=expression(italic(p)), ylab=expression(paste("", alpha)), ylim=c(5,7), boxwex=0.4, cex.lab=1.2)
for(seed in seed_number) {
  vioplot(cbind(node100results4d3_c_thin[[paste0("seed",(seed))]]$alpha,
                node100results4d4_c_thin[[paste0("seed",(seed))]]$alpha,
                node100results4d8_c_thin[[paste0("seed",(seed))]]$alpha),
          add=TRUE, cex=0.2, rectCol = NA, lineCol = NA,
          colMed = NA, border=NA, col=c(rgb(red=0, green = 0, blue = 0, alpha=0.1),
                                        rgb(red=0, green = 0, blue = 0, alpha=0.1),
                                        rgb(red=0, green = 0, blue = 0, alpha=0.1)))
  thin_mean <- cbind(mean(node100results4d3_c_thin[[paste0("seed",(seed))]]$alpha),
                     mean(node100results4d4_c_thin[[paste0("seed",(seed))]]$alpha),
                     mean(node100results4d8_c_thin[[paste0("seed",(seed))]]$alpha))
  alpha_c_thin_mean <- rbind(alpha_c_thin_mean, thin_mean)
}
abline(h=6, col='red', lwd=2) # true
points(apply(alpha_c_thin_mean,2,mean),pch=20, cex=2, col="green")
# title(xlab="(a)", line=2)
legend("topleft", legend=c('Posterior distribution', "True value", 'Average posterior mean'), pch=c(20,NA,20), lty=c(NA,1,NA), col=c("black","red", "green"), cex=1.1)
# legend("topleft", legend=c("", "True value", 'Average posterior mean'), pch=c(15,NA,20), lty=c(NA,1,NA), col=c("black","red", "green"), cex=1.1, y.intersp = 1.1)
# legend("topleft", legend=c("","",'Posterior distribution'), pch=15,
#        col=c(rgb(red=106/255, green = 13/255, blue = 173/255), "magenta3", "orange"),
#        horiz=TRUE, text.width = 0, box.lwd = 0)

# Close the pdf file
dev.off()




# Procrustes Correlation --------------------------------------------------

full_z_protest4d3_c <- full_z_protest4d4_c <- full_z_protest4d8_c <- list()
for(seed in seed_number) {
  z_protest <- protest(node100results4d3_c_thin[[paste0("seed",(seed))]]$z_mean, node100network4d_c[[paste0("seed",(seed))]]$positions[,1:3])
  full_z_protest4d3_c <- append(full_z_protest4d3_c, list(temp=z_protest))
  names(full_z_protest4d3_c)[names(full_z_protest4d3_c)=="temp"] <- paste0("seed",(seed))

  z_protest <- protest(node100results4d4_c_thin[[paste0("seed",(seed))]]$z_mean, node100network4d_c[[paste0("seed",(seed))]]$positions)
  full_z_protest4d4_c <- append(full_z_protest4d4_c, list(temp=z_protest))
  names(full_z_protest4d4_c)[names(full_z_protest4d4_c)=="temp"] <- paste0("seed",(seed))

  z_protest <- protest(node100results4d8_c_thin[[paste0("seed",(seed))]]$z_mean[,1:4], node100network4d_c[[paste0("seed",(seed))]]$positions)
  full_z_protest4d8_c <- append(full_z_protest4d8_c, list(temp=z_protest))
  names(full_z_protest4d8_c)[names(full_z_protest4d8_c)=="temp"] <- paste0("seed",(seed))

}


z_protest_c2 <- c()
for(seed in seed_number) {
  z_protest_temp <- cbind(sum(full_z_protest4d3_c[[paste0("seed",(seed))]]$scale),
                          sum(full_z_protest4d4_c[[paste0("seed",(seed))]]$scale),
                          sum(full_z_protest4d8_c[[paste0("seed",(seed))]]$scale))
  z_protest_c2 <- rbind(z_protest_c2, z_protest_temp)
}
colnames(z_protest_c2) <- c("3", "4", "8")

# open the pdf file
cairo_pdf("~/lspm/inst/figures/protest4_2c.pdf", width = 5, height = 4)
par(mar = c(4.1, 4.1, 1.1, 1.1))
boxplot(z_protest_c2, xlab=expression(italic(p)), ylab="Procrustes correlation",
        ylim=c(0,1))
# title(xlab="(b)", line=2)
par(old.par) # restore previous plot layout setting

# Close the pdf file
dev.off()

par(mar = c(5.1, 4.1, 4.1, 2.1))
