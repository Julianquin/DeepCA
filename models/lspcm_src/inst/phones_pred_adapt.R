load(file = "~/lspm/inst/extdata/adapt/phonesnovlspm_adapt.Rdata")

set.seed(1234)
seed_number <- sample(1:1e6, 10)

# open the pdf file
pdf("~/lspm/inst/extdata/adapt/figure/phones_dim_bar.pdf", width = 5, height = 3)

par(mfrow=c(1,1))
par(mar = c(4.1, 4.5, 1, 0.5))

barplot(table(do.call(rbind, lapply(phonesnovlspm_adapt, function(x) x$iter_d)))/ length(do.call(rbind, lapply(phonesnovlspm_adapt, function(x) x$iter_d)))
        , ylim=c(0,1), xlab = expression(p), ylab="Posterior proportion", cex.lab=1.5, cex.axis=1.5, cex.names=1.5)

dev.off()


d=4
n_pos=30
full_pred_phones4d <- c()
for(seed in seed_number) {
  if(d %in% names(table(phonesnovlspm_adapt[[paste0("seed",(seed))]]$iter_d))) {
    phones_single_check = list(predcheck(n_pos, n_dimen = d, LSPM_object = phonesnovlspm_adapt[[paste0("seed",(seed))]],
                                                    type="count", dist_power = 2, seed=1234))
    names(phones_single_check) <- paste0("seed",(seed))
    full_pred_phones4d <-  append(full_pred_phones4d, phones_single_check)
  }
}

abs_phone4d <- c()
for(seed in seed_number) {
  if(d %in% names(table(phonesnovlspm_adapt[[paste0("seed",(seed))]]$iter_d))) {
    abs_phone4d <- c(abs_phone4d, mean(apply(full_pred_phones4d[[paste0("seed",(seed))]]$Networks, 3, function(x) abs_diff(x, phone))))
  }
}

# open the pdf file
cairo_pdf("~/lspm/inst/extdata/adapt/figure/phones_abs.pdf", width = 5, height = 4)
par(mfrow=c(1,1))
par(mar = c(4.6, 4.5, 1.5, 0.5))
# bbb = cbind(abs_phone4d, abs2d1, abs2d5)
bbb = cbind(-10)
colnames(bbb) = c("4")
vioplot(bbb, cex=0.2, rectCol = NA, lineCol = NA, cex.sub=2, cex.axis=1.5,
        colMed = NA, border=NA, col="white", ylim=c(0,.5))
vioplot(abs_phone4d, add=T, at = 1, cex=0.2, rectCol = NA, lineCol = NA, cex.sub=2, cex.axis=1.5,
        colMed = NA, border=NA, ylim=c(0,1))

mtext("Mean Absolute Difference", side=2, line=2.5, cex=1.35)
mtext(expression(p), side=1, line=2.5, cex=1.35)

dev.off()

# log count ---------------------------------------------------------------

# open the pdf file
cairo_pdf("~/lspm/inst/extdata/adapt/figure/phones_logcount.pdf", width = 5, height = 3)

par(mar = c(4.1, 4.5, 1, 0.5))

vioplot(do.call(rbind,lapply(full_pred_phones4d, function(x) t(log(apply(x$Networks+1, 3, tabulate, 10)))))[,1:9], drawRect = FALSE, border = NA,
        colMed = NA, rectCol = NA, lineCol = NA, yaxt="n", cex.axis=1.5,
        ylim=c(0,10), main="")
vioplot(do.call(rbind,lapply(full_pred_phones4d, function(x) t(log(apply(x$Networks+1, 3, tabulate, 10)))))[,1:9][,1]+c(0.01,0.05), drawRect = FALSE, border = NA,
        colMed = NA, rectCol = NA, lineCol = NA, yaxt="n", cex.axis=1.5,
        ylim=c(0,10), main="", add=T)
points(as.vector(log(table(phone)[1:9])), pch=4, col="red", cex=2, lwd=2)
# lines(9.535, lwd=5)
axis(1, at=1:9, las=1, labels=0:8, cex.axis=1.5)
title(xlab=expression(paste("Count")), cex.lab=1.5, line=2.5)
title(ylab=expression(paste("log frequency")), line=2.2, cex.lab=1.5)
axis(2, at=0:10, las=1, labels=0:10, cex.axis=1.5)
legend("topright", legend = c("Posterior predictive network", "Observed network"), col=c("gray","red"), pch=c(20,4))


dev.off()

# phones Shrinkage Strength --------------------------------------------------

# open the pdf file
cairo_pdf("~/lspm/inst/extdata/adapt/figure/phones_pmd.pdf", width = 5, height = 4)

par(mar = c(3.6, 4.1, 1.1, 0.2))
# Plot LSPM results for shrinkage strength, phones
vioplot(cbind(phonesnovlspm_adapt[[paste0("seed",(seed_number[1]))]]$mcmc_chain$"7D"$deltas[complete.cases(phonesnovlspm_adapt[[paste0("seed",(seed_number[1]))]]$mcmc_chain$"7D"$deltas),,drop=F]),
        ylim=c(0,25), cex=0.2, rectCol = NA, lineCol = NA, cex.lab=1.5, cex.main=1.5, cex.axis=1.5,
        colMed = NA, border=NA, col=rgb(red = 0, green=0, blue=0,alpha=0),
        main= "", #main='Posterior Mean Variance vs Dimension',
        xlab= "", ylab="")
for(seed in seed_number) {
  lapply(phonesnovlspm_adapt[[paste0("seed",(seed))]]$mcmc_chain, function(x) vioplot(x$deltas[complete.cases(x$deltas),,drop=F], add=T, cex=0.2, rectCol = NA, lineCol = NA, cex.lab=1.5, cex.main=1.5, cex.axis=1.5,
                                                                                      colMed = NA, border=NA, col=rgb(red = 0, green=0, blue=0,alpha=0.1)))
}
title(xlab=expression(paste("Dimension \U2113")), cex.lab=1.3, line=2.5)
title(ylab=expression(paste("Shrinkage strength ", delta["\U2113"])), line=2.1, cex.lab=1.3)

dev.off()


# phones variance ------------------------------------------------------------

# open the pdf file
cairo_pdf("~/lspm/inst/extdata/adapt/figure/phones_pmv.pdf", width = 5, height = 4)

par(mar = c(3.6, 4.1, 1.1, 0.2))
# Plot LSPM results for variances, phones
vioplot(cbind(phonesnovlspm_adapt[[paste0("seed",(seed_number[1]))]]$mcmc_chain$"7D"$variances[complete.cases(phonesnovlspm_adapt[[paste0("seed",(seed_number[1]))]]$mcmc_chain$"7D"$variances),,drop=F]),
        ylim=c(0,15), cex=0.2, rectCol = NA, lineCol = NA, cex.lab=1.5, cex.main=1.5, cex.axis=1.5,
        colMed = NA, border=NA, col=rgb(red = 0, green=0, blue=0,alpha=0),
        main= "", #main='Posterior Mean Variance vs Dimension',
        xlab= "", ylab="")
for(seed in seed_number) {
  lapply(phonesnovlspm_adapt[[paste0("seed",(seed))]]$mcmc_chain, function(x) vioplot(x$variances[complete.cases(x$variances),,drop=F], add=T, cex=0.2, rectCol = NA, lineCol = NA, cex.lab=1.5, cex.main=1.5, cex.axis=1.5,
                                                                                      colMed = NA, border=NA, col=rgb(red = 0, green=0, blue=0,alpha=0.1)))
}
title(xlab=expression(paste("Dimension \U2113")), cex.lab=1.3, line=2.5)
title(ylab=expression(paste("Variance ", omega["\u2113"]^{-1})), line=2.1, cex.lab=1.3)

dev.off()

