load(file = "~/lpsm/inst/extdata/count/count2d0_result_thin.Rdata")
load(file = "~/lpsm/inst/extdata/count/count2d1_result_thin.Rdata")
load(file = "~/lpsm/inst/extdata/count/count2d5_result_thin.Rdata")

source("~/lpsm/inst/section4_4network_c.R")

# open the pdf file
cairo_pdf("~/lspm/inst/figures/pmd4_4.pdf", width = 10, height = 4)

# Setup the plotting area
old.par <- par(no.readonly = TRUE) # save current layout setting
par(mfrow=c(1,3))
layout.matrix <- matrix(1:3, nrow = 1, ncol = 3)
layout(mat = layout.matrix, widths = c(2.3,2,2,2.2))
par(mar = c(5.6, 4.1, 4.1, 0))

seed = seed_number[1]
# Plot LPSM 3D results for shrinkage strength
vioplot(cbind(count2d0_result_thin[[paste0("seed",(seed_number[1]))]]$deltas),
        ylim=c(0,30), cex=0.2, rectCol = NA, lineCol = NA, cex.lab=1.5, cex.main=1.5, cex.axis=1.5,
        colMed = NA, border=NA, col=rgb(red = 0, green=0, blue=0,alpha=0.1),
        main= "Low overdispersion", #main='Posterior Mean Variance vs Dimension',
        xlab= "", ylab="")
title(xlab=expression(paste("Dimension")), cex.lab=1.5, line=2.5)
title(ylab=expression(paste("Shrinkage strength")), line=2.2, cex.lab=1.6)
for(seed in seed_number) {
  vioplot(count2d0_result_thin[[paste0("seed",(seed))]]$deltas, add=TRUE, cex=0.2, rectCol = NA, lineCol = NA,
          colMed = NA, border=NA, col=rgb(red = 0, green=0, blue=0,alpha=0.1))
}
points(c(1.5,1.5), col='red', pch=4, cex=2, lwd=4) # true
points(apply(t(as.data.frame(lapply(count2d0_result_thin,function(x) apply(x$deltas,2,mean)))),2,mean),pch=20, cex=2, col="green")

legend("topright", legend=c('Posterior distribution', "True value", 'Average posterior mean'), pch=c(20,4,20), col=c("black","red", "green"), cex=1.4)

par(mar = c(5.6, 0, 4.1, 0))

# Plot LPSM 4D results for shrinkage strength
vioplot(count2d1_result_thin[[paste0("seed",(seed_number[1]))]]$deltas,
        ylim=c(0,30), cex=0.2, rectCol = NA, lineCol = NA, cex.lab=1.5, cex.main=1.5, cex.axis=1.5,
        colMed = NA, border=NA, col=rgb(red = 0, green=0, blue=0,alpha=0.1),
        main= "Moderate overdispersion", #main='Posterior Mean Variance vs Dimension',
        xlab= "", ylab="", yaxt="n")
title(xlab=expression(paste("Dimension")), cex.lab=1.5, line=2.5)
title(ylab=expression(paste("Shrinkage Strength")), line=2.2)
axis(1, at=1:5, las=1, labels=1:5, cex.axis=1.5)
for(seed in seed_number) {
  vioplot(count2d1_result_thin[[paste0("seed",(seed))]]$deltas, add=TRUE, cex=0.2, rectCol = NA, lineCol = NA,
          colMed = NA, border=NA, col=rgb(red = 0, green=0, blue=0,alpha=0.1))
}
points(c(0.5,1.5), col='red', pch=4, cex=2, lwd=4) # true
points(apply(t(as.data.frame(lapply(count2d1_result_thin,function(x) apply(x$deltas,2,mean)))),2,mean),pch=20, cex=2, col="green")

par(mar = c(5.6, 0, 4.1, 0.25)) # right margin set as 0.1

# Plot LPSM 8D results for shrinkage strength
vioplot(count2d5_result_thin[[paste0("seed",(seed_number[1]))]]$deltas,
        ylim=c(0,30), cex=0.2, rectCol = NA, lineCol = NA, cex.lab=1.5, cex.main=1.5, cex.axis=1.5,
        colMed = NA, border=NA, col=rgb(red = 0, green=0, blue=0,alpha=0.1),
        main= "High overdispersion", #main='Posterior Mean Variance vs Dimension',
        xlab= "", ylab="", yaxt="n")
title(xlab=expression(paste("Dimension")), cex.lab=1.5, line=2.5)
title(ylab=expression(paste("Shrinkage Strength")), line=2.2)
axis(1, at=1:8, las=1, labels=1:8, cex.axis=1.5)
for(seed in seed_number) {
  vioplot(count2d5_result_thin[[paste0("seed",(seed))]]$deltas, add=TRUE, cex=0.2, rectCol = NA, lineCol = NA,
          colMed = NA, border=NA, col=rgb(red = 0, green=0, blue=0,alpha=0.1))
}
points(c(0.1,1.5), col='red', pch=4, cex=2, lwd=4) # true
points(apply(t(as.data.frame(lapply(count2d5_result_thin,function(x) apply(x$deltas,2,mean)))),2,mean),pch=20, cex=2, col="green")

par(old.par) # restore previous plot layout setting

# Close the pdf file
dev.off()

