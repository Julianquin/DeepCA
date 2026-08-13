load(file = "~/lspm/inst/extdata/adapt/node100results4d3_adapt.Rdata")
load(file = "~/lspm/inst/extdata/adapt/node100results4d3_adapt_low_d.Rdata")
load(file = "~/lspm/inst/extdata/adapt/node100results4d3_adapt_same_d.Rdata")
load(file = "~/lspm/inst/extdata/adapt/node100results4d3_adapt_high_d.Rdata")

source("~/lspm/inst/trunc_network.R")

# open the pdf file
cairo_pdf("~/lspm/inst/extdata/adapt/figure/trunc_pmd.pdf", width = 10, height = 4)

# Setup the plotting area
old.par <- par(no.readonly = TRUE) # save current layout setting
par(mfrow=c(1,4))
layout.matrix <- matrix(1:4, nrow = 1, ncol = 4)
layout(mat = layout.matrix, widths = c(2.4,2,2,2.2))
par(mar = c(5.6, 4.1, 4.1, 0))



# p_0 = auto --------------------------------------------------------------

vioplot(cbind(node100results4d3_adapt[[paste0("seed",(seed_number[6]))]]$mcmc_chain$"8D"$deltas[complete.cases(node100results4d3_adapt$seed126055$mcmc_chain$"8D"$deltas),,drop=F]),
        ylim=c(0,30), cex=0.2, rectCol = NA, lineCol = NA, cex.lab=1.5, cex.main=1.5, cex.axis=1.5,
        colMed = NA, border=NA, col=rgb(red = 0, green=0, blue=0,alpha=0),
        #main='Posterior Mean Variance vs Dimension',
        xlab= "", ylab="")
title(main=expression(paste(p[0]," automatically chosen")), line=2.3, cex.main=1.5)
title(main=expression(paste("(", p[0], " = auto)")), line=1, cex.main=1.5)

for(seed in seed_number) {
lapply(node100results4d3_adapt[[paste0("seed",(seed))]]$mcmc_chain, function(x) vioplot(x$deltas[complete.cases(x$deltas),,drop=F], add=T, cex=0.2, rectCol = NA, lineCol = NA, cex.lab=1.5, cex.main=1.5, cex.axis=1.5,
                                                                           colMed = NA, border=NA, col=rgb(red = 0, green=0, blue=0,alpha=0.03)))
}
title(xlab=expression(paste("Dimension h")), cex.lab=1.5, line=2.5)
title(ylab=expression(paste("Shrinkage strength ", delta[h])), line=2.2, cex.lab=1.75)
# for(seed in seed_number[2:15]) {
#   vioplot(node100results4d3_adapt[[paste0("seed",(seed))]]$mcmc_chain$"4D"$deltas, add=TRUE, cex=0.2, rectCol = NA, lineCol = NA,
#           colMed = NA, border=NA, col=rgb(red = 0, green=0, blue=0,alpha=0.03))
# }
points(c(0.5,1.1,1.05,1.15), col='red', pch=4, cex=2, lwd=3) # true
# points(apply(do.call(rbind, lapply(node100results4d3_adapt, function(y) apply(do.call(rbind,lapply(y$mcmc_chain, function(x) cbind(x$deltas, NA,NA,NA,NA,NA,NA)[,1:8])),2,mean, na.rm=T))), 2,mean,na.rm=T), pch=20, col="green", cex=2)
legend("topleft", legend=c('Posterior distribution', "True value"), pch=c(20,4), col=c("black","red"), cex=1.2)



# p_0 = 2 -----------------------------------------------------------------


par(mar = c(5.6, 0, 4.1, 0)) # left margin set as 0

vioplot(cbind(node100results4d3_adapt[[paste0("seed",(seed_number[6]))]]$mcmc_chain$"8D"$deltas[complete.cases(node100results4d3_adapt$seed126055$mcmc_chain$"8D"$deltas),,drop=F]),
        ylim=c(0,30), cex=0.2, rectCol = NA, lineCol = NA, cex.lab=1.5, cex.main=1.5, cex.axis=1.5,
        colMed = NA, border=NA, col=rgb(red = 0, green=0, blue=0,alpha=0),
        #main='Posterior Mean Variance vs Dimension',
        xlab= "", ylab="", yaxt="n")
title(main=expression(paste(p[0]," lower than the truth")), line=2.3, cex.main=1.5)
title(main=expression(paste("(", p[0], " = 2)")), line=1, cex.main=1.5)

for(seed in seed_number) {
  lapply(node100results4d3_adapt_low_d[[paste0("seed",(seed))]]$mcmc_chain, function(x) vioplot(x$deltas[complete.cases(x$deltas),,drop=F], add=T, cex=0.2, rectCol = NA, lineCol = NA, cex.lab=1.5, cex.main=1.5, cex.axis=1.5,
                                                                                           colMed = NA, border=NA, col=rgb(red = 0, green=0, blue=0,alpha=0.03)))
}
axis(1, at=1:8, las=1, labels=1:8, cex.axis=1.5)

title(xlab=expression(paste("Dimension h")), cex.lab=1.5, line=2.5)
title(ylab=expression(paste("Shrinkage strength ", delta[h])), line=2.2)
points(c(0.5,1.1,1.05,1.15), col='red', pch=4, cex=2, lwd=3) # true
# points(apply(do.call(rbind, lapply(node100results4d3_adapt_low_d, function(y) apply(do.call(rbind,lapply(y$mcmc_chain, function(x) cbind(x$deltas, NA,NA,NA,NA,NA,NA)[,1:8])),2,median, na.rm=T))), 2,median,na.rm=T), pch=20, col="green", cex=2)


# p_0 = 4 -----------------------------------------------------------------


vioplot(cbind(node100results4d3_adapt[[paste0("seed",(seed_number[6]))]]$mcmc_chain$"8D"$deltas[complete.cases(node100results4d3_adapt$seed126055$mcmc_chain$"8D"$deltas),,drop=F]),
        ylim=c(0,30), cex=0.2, rectCol = NA, lineCol = NA, cex.lab=1.5, cex.main=1.5, cex.axis=1.5,
        colMed = NA, border=NA, col=rgb(red = 0, green=0, blue=0,alpha=0),
        #main='Posterior Mean Variance vs Dimension',
        xlab= "", ylab="", yaxt="n")
title(main=expression(paste(p[0]," same as the truth")), line=2.3, cex.main=1.5)
title(main=expression(paste("(", p[0], " = 4)")), line=1, cex.main=1.5)

for(seed in seed_number) {
  lapply(node100results4d3_adapt_same_d[[paste0("seed",(seed))]]$mcmc_chain, function(x) vioplot(x$deltas[complete.cases(x$deltas),,drop=F], add=T, cex=0.2, rectCol = NA, lineCol = NA, cex.lab=1.5, cex.main=1.5, cex.axis=1.5,
                                                                                                 colMed = NA, border=NA, col=rgb(red = 0, green=0, blue=0,alpha=0.03)))
}
title(xlab=expression(paste("Dimension h")), cex.lab=1.5, line=2.5)
title(ylab=expression(paste("Shrinkage strength ", delta[h])), line=2.2)
axis(1, at=1:8, las=1, labels=1:8, cex.axis=1.5)

points(c(0.5,1.1,1.05,1.15), col='red', pch=4, cex=2, lwd=3) # true
# points(apply(do.call(rbind, lapply(node100results4d3_adapt_same_d, function(y) apply(do.call(rbind,lapply(y$mcmc_chain, function(x) cbind(x$deltas, NA,NA,NA,NA,NA,NA)[,1:8])),2,mean, na.rm=T))), 2,mean,na.rm=T), pch=20, col="green", cex=2)



# p_0=10 ------------------------------------------------------------------


par(mar = c(5.6, 0, 4.1, 0.25)) # right margin set as 0.1

vioplot(cbind(node100results4d3_adapt[[paste0("seed",(seed_number[6]))]]$mcmc_chain$"8D"$deltas[complete.cases(node100results4d3_adapt$seed126055$mcmc_chain$"8D"$deltas),,drop=F]),
        ylim=c(0,30), cex=0.2, rectCol = NA, lineCol = NA, cex.lab=1.5, cex.main=1.5, cex.axis=1.5,
        colMed = NA, border=NA, col=rgb(red = 0, green=0, blue=0,alpha=0),
        #main='Posterior Mean Variance vs Dimension',
        xlab= "", ylab="", yaxt="n")
title(main=expression(paste(p[0]," higher than the truth")), line=2.3, cex.main=1.5)
title(main=expression(paste("(", p[0], " = 10)")), line=1, cex.main=1.5)

for(seed in seed_number) {
  lapply(node100results4d3_adapt_high_d[[paste0("seed",(seed))]]$mcmc_chain, function(x) vioplot(x$deltas[complete.cases(x$deltas),,drop=F], add=T, cex=0.2, rectCol = NA, lineCol = NA, cex.lab=1.5, cex.main=1.5, cex.axis=1.5,
                                                                                                 colMed = NA, border=NA, col=rgb(red = 0, green=0, blue=0,alpha=0.03)))
}

title(xlab=expression(paste("Dimension h")), cex.lab=1.5, line=2.5)
title(ylab=expression(paste("Shrinkage strength ", delta[h])), line=2.2)
axis(1, at=1:8, las=1, labels=1:8, cex.axis=1.5)
points(c(0.5,1.1,1.05,1.15), col='red', pch=4, cex=2, lwd=3) # true
# points(apply(do.call(rbind, lapply(node100results4d3_adapt_high_d, function(y) apply(do.call(rbind,lapply(y$mcmc_chain, function(x) cbind(x$deltas, NA,NA,NA,NA,NA,NA)[,1:8])),2,mean, na.rm=T))), 2,mean,na.rm=T), pch=20, col="green", cex=2)


par(old.par) # restore previous plot layout setting

# Close the pdf file
dev.off()



# # look into specific posterior mode dimension (not used) ------------------
#
# plot(node100results4d3_adapt, d=4, parameter = "deltas", true_values = c(0.5,1.1,1.05,1.15))
# plot(node100results4d3_adapt_low_d, d=4, parameter="deltas", true_values = c(0.5,1.1,1.05,1.15))
# plot(node100results4d3_adapt_same_d, d=4, parameter = "deltas", true_values = c(0.5,1.1,1.05,1.15))
# plot(node100results4d3_adapt_high_d, d=4, parameter="deltas", true_values = c(0.5,1.1,1.05,1.15))
#

