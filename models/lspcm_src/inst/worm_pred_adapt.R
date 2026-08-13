load(file = "~/lspm/inst/extdata/adapt/worm_lspm_adapt4.Rdata")
load(file = "~/lspm/inst/extdata/adapt/worm_lpm3d_full.Rdata")
load(file = "~/lspm/inst/extdata/binary/worm_lpm4d_full.Rdata")
load(file = "~/lspm/inst/extdata/adapt/worm_lpm5d_full.Rdata")

# load(file = "~/lspm/inst/extdata/binary/worm_lpm2d.Rdata")

# wormadj <- as.matrix(as_adjacency_matrix(read.graph("~/lspm/inst/c.elegans_neural.male_1.graphml", format="graphml")))

set.seed(1234)
seed_number <- sample(1:1e6, 10)

# do.call(c, lapply(worm_lpm3d_full, function(x) summary(x)$bic$overall))
# do.call(c, lapply(worm_lpm4d_full, function(x) summary(x)$bic$overall))

# worm posterior predictive result -----------------------------------------

n_pos=30
d = 3
set.seed(15)
full_pred_worm_lspm_adapt4_3d <- c()
for(seed in seed_number[1:10]) {
  if(names(which.max(table(worm_lspm_adapt4[[paste0("seed",(seed))]]$iter_d))) == d) {
    pred_worm_lspm_adapt4 <- as.data.frame(predcheck(n_pos, n_dimen = d, LSPM_object = worm_lspm_adapt4[[paste0("seed",(seed))]], seed = 1234))
    full_pred_worm_lspm_adapt4_3d[[paste0("seed",(seed))]] = pred_worm_lspm_adapt4
  }
  # full_pred_worm_lspm_adapt4_4d <- rbind(full_pred_worm_lspm_adapt4_4d, pred_worm_lspm)
}


d = 4
set.seed(15)
full_pred_worm_lspm_adapt4_4d <- c()
for(seed in seed_number[1:10]) {
  if(names(which.max(table(worm_lspm_adapt4[[paste0("seed",(seed))]]$iter_d))) == d) {
    pred_worm_lspm_adapt4 <- as.data.frame(predcheck(n_pos, n_dimen = d, LSPM_object = worm_lspm_adapt4[[paste0("seed",(seed))]], seed = 1234))
    full_pred_worm_lspm_adapt4_4d[[paste0("seed",(seed))]] = pred_worm_lspm_adapt4
  }
  # full_pred_worm_lspm_adapt4_4d <- rbind(full_pred_worm_lspm_adapt4_4d, pred_worm_lspm)
}

######
set.seed(15)
full_pred_worm_lpm3d <- c()
for(seed in seed_number[1:10]) {
  pred_worm_lpm <- as.data.frame(predcheck(n_pos,
                                           alpha_chain = worm_lpm3d_full[[paste0("seed",(seed))]]$sample$beta[,1],
                                           latent_pos_chain = aperm(worm_lpm3d_full[[paste0("seed",(seed))]]$sample$Z, c(2,3,1)),
                                           network = worm))
  full_pred_worm_lpm3d[[paste0("seed",(seed))]] = pred_worm_lpm
  # full_pred_worm_lpm3d <- rbind(full_pred_worm_lpm3d, pred_worm_lpm)
}


set.seed(15)
full_pred_worm_lpm4d <- c()
for(seed in seed_number[1:10]) {
  pred_worm_lpm <- as.data.frame(predcheck(n_pos,
                                           alpha_chain = worm_lpm4d_full[[paste0("seed",(seed))]]$sample$beta[,1],
                                           latent_pos_chain = aperm(worm_lpm4d_full[[paste0("seed",(seed))]]$sample$Z, c(2,3,1)),
                                           network = worm))
  full_pred_worm_lpm4d[[paste0("seed",(seed))]] = pred_worm_lpm
  # full_pred_worm_lpm4d <- rbind(full_pred_worm_lpm4d, pred_worm_lpm)
}

# set.seed(15)
# full_pred_worm_lpm4d <- c()
# for(seed in seed_number[1:10]) {
#   pred_worm_lpm <- as.data.frame(predcheck(n_pos, worm_lpm4d_full[[paste0("seed",(seed))]]$sample$beta[,1], aperm(worm_lpm4d_full[[paste0("seed",(seed))]]$sample$Z, c(2,3,1)), worm))
#   full_pred_worm_lpm4d[[paste0("seed",(seed))]] = pred_worm_lpm
#   # full_pred_worm_lpm4d <- rbind(full_pred_worm_lpm4d, pred_worm_lpm)
# }


# worm predictive plots ----------------------------------------------------

# open the pdf file
cairo_pdf("~/lspm/inst/extdata/adapt/figure/worm_pred.pdf", width = 10, height = 3)

palette(c("black"  , "red"  ,   "green3"  ,"blue"   , "cyan"  ,  "magenta" ,"yellow"  ,"gray"  )) # default palette
old.par <- par(no.readonly = TRUE) # save current layout setting
mycol=adjustcolor(palette(), alpha.f = 0.1)
palette(mycol)

obs_worm_d <- gden(worm)
obs_worm_t <- gtrans(worm)

par(mfrow=c(1,5))
layout.matrix <- matrix(1:5, nrow = 1, ncol = 5)
layout(mat = layout.matrix, widths = c(2.6,1.6,1.6,1.6,1.7))
par(mar = c(5.6, 5.3, 2.1, 0.2))

plot_names = c("Network Density","Transitivity","Accuracy","F1 Score","Hamming Distance")

density_worm_adapt3d = Map('-', lapply(full_pred_worm_lspm_adapt4_4d, function(x) x$Density), obs_worm_d)
density_worm_adapt4d = Map('-', lapply(full_pred_worm_lspm_adapt4_3d, function(x) x$Density), obs_worm_d)
density_worm_lpm4d = Map('-', lapply(full_pred_worm_lpm4d, function(x) x$Density), obs_worm_d)
density_worm_lpm3d = Map('-', lapply(full_pred_worm_lpm3d, function(x) x$Density), obs_worm_d)
all_density = Map(cbind, density_worm_adapt3d, density_worm_adapt4d, density_worm_lpm4d, density_worm_lpm3d)

vioplot(do.call(rbind,all_density), cex=0.2, rectCol = NA, lineCol = NA,
        xaxt="n", cex.names=1.4, horizontal = TRUE,
        # ylim=c(0,1),
        ylim=c(min(unlist(all_density))-0.01,max(unlist(all_density)+0.01)),
        colMed = NA, border=NA, col=rgb(red = 1, green=0.8, blue=1,alpha=0))

lapply(density_worm_adapt3d, function(x)
  vioplot(x, add=TRUE, cex=0.2, rectCol = NA, lineCol = NA, cex.lab=1.5, at=1,
          colMed = NA, border=NA, col=1, horizontal = TRUE))
lapply(density_worm_adapt4d, function(x)
  vioplot(x, add=TRUE, cex=0.2, rectCol = NA, lineCol = NA, cex.lab=1.5, at=2,
          colMed = NA, border=NA, col=1, horizontal = TRUE))
lapply(density_worm_lpm4d, function(x)
  vioplot(x, add=TRUE, cex=0.2, rectCol = NA, lineCol = NA, cex.lab=1.5, at=3,
          colMed = NA, border=NA, col=4, horizontal = TRUE))
lapply(density_worm_lpm3d, function(x)
  vioplot(x, add=TRUE, cex=0.2, rectCol = NA, lineCol = NA, cex.lab=1.5, at=4,
          colMed = NA, border=NA, col=4, horizontal = TRUE))

axis(2,1:4, las=2,labels=c(expression(paste(atop(NA, atop("LSPM", paste(p[m], " = 4"))))),
                           expression(paste(atop(NA, atop("LSPM", paste(p[m], " = 3"))))),
                           expression(paste(atop(NA, atop("LPM", paste(p, " = 4"),)))),
                           expression(paste(atop(NA, atop("LPM", paste(p, " = 3"),))))
), cex.axis=1.75)

title(xlab=plot_names[1], line=2.75, cex.lab=1.6)
abline(v=0, col ="red", lty=2, lwd=2)

par(mar = c(5.6, 0, 2.1, 0.2))

transitivity_worm_adapt3d = Map('-', lapply(full_pred_worm_lspm_adapt4_4d, function(x) x$Transitivity), obs_worm_t)
transitivity_worm_adapt4d = Map('-', lapply(full_pred_worm_lspm_adapt4_3d, function(x) x$Transitivity), obs_worm_t)
transitivity_worm_lpm4d = Map('-', lapply(full_pred_worm_lpm4d, function(x) x$Transitivity), obs_worm_t)
transitivity_worm_lpm3d = Map('-', lapply(full_pred_worm_lpm3d, function(x) x$Transitivity), obs_worm_t)

all_transitivity = Map(cbind, transitivity_worm_adapt3d, transitivity_worm_adapt4d,
                       transitivity_worm_lpm4d,transitivity_worm_lpm3d)

vioplot(do.call(rbind,all_transitivity), cex=0.2, rectCol = NA, lineCol = NA,
        xaxt="n", cex.names=1.4, horizontal = TRUE,
        # ylim=c(0,1),
        ylim=c(min(unlist(all_transitivity))-0.03,max(unlist(all_transitivity)+0.03)),
        colMed = NA, border=NA, col=rgb(red = 1, green=0.8, blue=1,alpha=0))

lapply(transitivity_worm_adapt3d, function(x)
  vioplot(x, add=TRUE, cex=0.2, rectCol = NA, lineCol = NA, cex.lab=1.5, at=1,
          colMed = NA, border=NA, col=1, horizontal = TRUE))
lapply(transitivity_worm_adapt4d, function(x)
  vioplot(x, add=TRUE, cex=0.2, rectCol = NA, lineCol = NA, cex.lab=1.5, at=2,
          colMed = NA, border=NA, col=1, horizontal = TRUE))
lapply(transitivity_worm_lpm4d, function(x)
  vioplot(x, add=TRUE, cex=0.2, rectCol = NA, lineCol = NA, cex.lab=1.5, at=3,
          colMed = NA, border=NA, col=4, horizontal = TRUE))
lapply(transitivity_worm_lpm3d, function(x)
  vioplot(x, add=TRUE, cex=0.2, rectCol = NA, lineCol = NA, cex.lab=1.5, at=4,
          colMed = NA, border=NA, col=4, horizontal = TRUE))
title(xlab=plot_names[2], line=2.75, cex.lab=1.6)
abline(v=0, col ="red", lty=2, lwd=2)


accuracy_worm_adapt3d = lapply(full_pred_worm_lspm_adapt4_4d, function(x) x$Accuracy)
accuracy_worm_adapt4d = lapply(full_pred_worm_lspm_adapt4_3d, function(x) x$Accuracy)
accuracy_worm_lpm4d = lapply(full_pred_worm_lpm4d, function(x) x$Accuracy)
accuracy_worm_lpm3d = lapply(full_pred_worm_lpm3d, function(x) x$Accuracy)
all_accuracy = Map(cbind, accuracy_worm_adapt3d, accuracy_worm_adapt4d,
                   accuracy_worm_lpm4d, accuracy_worm_lpm3d)

vioplot(do.call(rbind,all_accuracy), cex=0.2, rectCol = NA, lineCol = NA,
        xaxt="n", cex.names=1.4, horizontal = TRUE,
        # ylim=c(0,1),
        ylim=c(min(unlist(all_accuracy))-0.01,max(unlist(all_accuracy)+0.01)),
        colMed = NA, border=NA, col=rgb(red = 1, green=0.8, blue=1,alpha=0))

lapply(accuracy_worm_adapt3d, function(x)
  vioplot(x, add=TRUE, cex=0.2, rectCol = NA, lineCol = NA, cex.lab=1.5, at=1,
          colMed = NA, border=NA, col=1, horizontal = TRUE))
lapply(accuracy_worm_adapt4d, function(x)
  vioplot(x, add=TRUE, cex=0.2, rectCol = NA, lineCol = NA, cex.lab=1.5, at=2,
          colMed = NA, border=NA, col=1, horizontal = TRUE))
lapply(accuracy_worm_lpm4d, function(x)
  vioplot(x, add=TRUE, cex=0.2, rectCol = NA, lineCol = NA, cex.lab=1.5, at=3,
          colMed = NA, border=NA, col=4, horizontal = TRUE))
lapply(accuracy_worm_lpm3d, function(x)
  vioplot(x, add=TRUE, cex=0.2, rectCol = NA, lineCol = NA, cex.lab=1.5, at=4,
          colMed = NA, border=NA, col=4, horizontal = TRUE))

title(xlab=plot_names[3], line=2.75, cex.lab=1.6)


F1score_worm_adapt3d = lapply(full_pred_worm_lspm_adapt4_4d, function(x) x$F1score)
F1score_worm_adapt4d = lapply(full_pred_worm_lspm_adapt4_3d, function(x) x$F1score)
F1score_worm_lpm4d = lapply(full_pred_worm_lpm4d, function(x) x$F1score)
F1score_worm_lpm3d = lapply(full_pred_worm_lpm3d, function(x) x$F1score)
all_F1score = Map(cbind, F1score_worm_adapt3d, F1score_worm_adapt4d,
                  F1score_worm_lpm4d,F1score_worm_lpm3d)

vioplot(do.call(rbind,all_F1score), cex=0.2, rectCol = NA, lineCol = NA,
        xaxt="n", cex.names=1.4, horizontal = TRUE,
        # ylim=c(0,1),
        ylim=c(min(unlist(all_F1score))-0.01,max(unlist(all_F1score)+0.01)),
        colMed = NA, border=NA, col=rgb(red = 1, green=0.8, blue=1,alpha=0))

lapply(F1score_worm_adapt3d, function(x)
  vioplot(x, add=TRUE, cex=0.2, rectCol = NA, lineCol = NA, cex.lab=1.5, at=1,
          colMed = NA, border=NA, col=1, horizontal = TRUE))
lapply(F1score_worm_adapt4d, function(x)
  vioplot(x, add=TRUE, cex=0.2, rectCol = NA, lineCol = NA, cex.lab=1.5, at=2,
          colMed = NA, border=NA, col=1, horizontal = TRUE))
lapply(F1score_worm_lpm4d, function(x)
  vioplot(x, add=TRUE, cex=0.2, rectCol = NA, lineCol = NA, cex.lab=1.5, at=3,
          colMed = NA, border=NA, col=4, horizontal = TRUE))
lapply(F1score_worm_lpm3d, function(x)
  vioplot(x, add=TRUE, cex=0.2, rectCol = NA, lineCol = NA, cex.lab=1.5, at=4,
          colMed = NA, border=NA, col=4, horizontal = TRUE))

title(xlab=plot_names[4], line=2.75, cex.lab=1.6)

par(mar = c(5.6, 0, 2.1, 1)) # right margin set as 0.1


Hamming_worm_adapt3d = lapply(full_pred_worm_lspm_adapt4_4d, function(x) x$Hamming)
Hamming_worm_adapt4d = lapply(full_pred_worm_lspm_adapt4_3d, function(x) x$Hamming)
Hamming_worm_lpm4d = lapply(full_pred_worm_lpm4d, function(x) x$Hamming)
Hamming_worm_lpm3d = lapply(full_pred_worm_lpm3d, function(x) x$Hamming)
all_Hamming = Map(cbind, Hamming_worm_adapt3d, Hamming_worm_adapt4d,
                  Hamming_worm_lpm4d,Hamming_worm_lpm3d)

vioplot(do.call(rbind,all_Hamming), cex=0.2, rectCol = NA, lineCol = NA,
        xaxt="n", cex.names=1.4, horizontal = TRUE,
        # ylim=c(0,1),
        ylim=c(min(unlist(all_Hamming))-0.01,max(unlist(all_Hamming)+0.01)),
        colMed = NA, border=NA, col=rgb(red = 1, green=0.8, blue=1,alpha=0))

lapply(Hamming_worm_adapt3d, function(x)
  vioplot(x, add=TRUE, cex=0.2, rectCol = NA, lineCol = NA, cex.lab=1.5, at=1,
          colMed = NA, border=NA, col=1, horizontal = TRUE))
lapply(Hamming_worm_adapt4d, function(x)
  vioplot(x, add=TRUE, cex=0.2, rectCol = NA, lineCol = NA, cex.lab=1.5, at=2,
          colMed = NA, border=NA, col=1, horizontal = TRUE))
lapply(Hamming_worm_lpm4d, function(x)
  vioplot(x, add=TRUE, cex=0.2, rectCol = NA, lineCol = NA, cex.lab=1.5, at=3,
          colMed = NA, border=NA, col=4, horizontal = TRUE))
lapply(Hamming_worm_lpm3d, function(x)
  vioplot(x, add=TRUE, cex=0.2, rectCol = NA, lineCol = NA, cex.lab=1.5, at=4,
          colMed = NA, border=NA, col=4, horizontal = TRUE))

title(xlab=plot_names[5], line=2.75, cex.lab=1.6)

par(old.par) # restore previous plot layout setting

# palette("R3") # default palette
palette(c("black"  , "red"  ,   "green3"  ,"blue"   , "cyan"  ,  "magenta" ,"yellow"  ,"gray"  )) # default palette

# Close the pdf file
dev.off()


# worm Shrinkage Strength --------------------------------------------------

# open the pdf file
cairo_pdf("~/lspm/inst/extdata/adapt/figure/worm_pmd.pdf", width = 5, height = 4)

par(mar = c(3.6, 4.1, 1.1, 0.2))
# Plot LSPM results for shrinkage strength, worm connectome.
vioplot(cbind(worm_lspm_adapt4[[paste0("seed",(seed_number[1]))]]$mcmc_chain$"5D"$deltas[complete.cases(worm_lspm_adapt4[[paste0("seed",(seed_number[1]))]]$mcmc_chain$"5D"$deltas),,drop=F]),
        ylim=c(0,40), cex=0.2, rectCol = NA, lineCol = NA, cex.lab=1.5, cex.main=1.5, cex.axis=1.2,
        colMed = NA, border=NA, col=rgb(red = 0, green=0, blue=0,alpha=0),
        main= "", #main='Posterior Mean Variance vs Dimension',
        xlab= "", ylab="")
for(seed in seed_number[1:10]) {
  lapply(worm_lspm_adapt4[[paste0("seed",(seed))]]$mcmc_chain, function(x) vioplot(x$deltas[complete.cases(x$deltas),,drop=F], add=T, cex=0.2, rectCol = NA, lineCol = NA, cex.lab=1.5, cex.main=1.5, cex.axis=1.5,
                                                                                 colMed = NA, border=NA, col=rgb(red = 0, green=0, blue=0,alpha=0.1)))
}
title(xlab=expression(paste("Dimension \U2113")), cex.lab=1.5, line=2.5)
title(ylab=expression(paste("Shrinkage strength ", delta["\U2113"])), line=2.1, cex.lab=1.5)

dev.off()


# worm variance ------------------------------------------------------------

# open the pdf file
cairo_pdf("~/lspm/inst/extdata/adapt/figure/worm_pmv.pdf", width = 5, height = 4)

par(mar = c(3.6, 4.1, 1.1, 0.2))
# Plot LSPM results for variances, worm connectome.
vioplot(cbind(worm_lspm_adapt4[[paste0("seed",(seed_number[1]))]]$mcmc_chain$"5D"$variances[complete.cases(worm_lspm_adapt4[[paste0("seed",(seed_number[1]))]]$mcmc_chain$"5D"$variances),,drop=F]),
        ylim=c(0,2), cex=0.2, rectCol = NA, lineCol = NA, cex.lab=1.5, cex.main=1.5, cex.axis=1.5,
        colMed = NA, border=NA, col=rgb(red = 0, green=0, blue=0,alpha=0),
        main= "", #main='Posterior Mean Variance vs Dimension',
        xlab= "", ylab="")
for(seed in seed_number[1:10]) {
  lapply(worm_lspm_adapt4[[paste0("seed",(seed))]]$mcmc_chain, function(x) vioplot(x$variances[complete.cases(x$variances),,drop=F], add=T, cex=0.2, rectCol = NA, lineCol = NA, cex.lab=1.5, cex.main=1.5, cex.axis=1.5,
                                                                                 colMed = NA, border=NA, col=rgb(red = 0, green=0, blue=0,alpha=0.1)))
}
title(xlab=expression(paste("Dimension \U2113")), cex.lab=1.3, line=2.5)
title(ylab=expression(paste("Variance ", omega["\u2113"]^{-1})), line=2.1, cex.lab=1.3)

dev.off()


# LPM worm position --------------------------------------------------------

palette(c("black"  , "red"  ,   "green3"  ,"blue"   , "cyan"  ,  "magenta" ,"yellow"  ,"gray"  )) # default palette

# open the pdf file
cairo_pdf("~/lspm/inst/extdata/adapt/figure/wormlpm.pdf", width = 6.66, height = 8)
par(mar = c(5.1, 5.1, 2.1, 2.1))
plot(worm_lpm4d_full$seed345167$mcmc.mle$Z, #ylim=c(-5.5,6.5), xlim=c(-7,9),
     pch=20, cex=2, cex.lab=2.5, cex.axis=2.,
     xlab="Dimension 1", ylab="Dimension 2", main="")

dev.off()

# LSPM worm position -------------------------------------------------------

# open the pdf file
cairo_pdf("~/lspm/inst/extdata/adapt/figure/wormlspm.pdf", width = 6.66, height = 8)
d=4
worm_lspm_4d_mean_pos = apply(worm_lspm_adapt4$seed345167$mcmc_chain[[paste0(d, "D")]]$positions, c(1,2), mean, na.rm=T)
par(mar = c(5.1, 5.1, 2.1, 2.1))
plot(proc.crr(worm_lpm4d_full$seed345167$mcmc.mle$Z, worm_lspm_4d_mean_pos[,1:4], k=4), #ylim=c(-2,3), xlim=c(-2,3),
     pch=20, cex=2, cex.lab=2.5, cex.axis=2.,
     ylim = c(-3,3), xlim=c(-2.5,2.5),
     xlab="Dimension 1", ylab="Dimension 2", main="")
dev.off()


# Prop. Dim. combine 30 chain ---------------------------------------------

# open the pdf file
pdf("~/lspm/inst/extdata/adapt/figure/worm_dim_bar.pdf", width = 5, height = 4)

par(mfrow=c(1,1))
par(mar = c(4.6, 4.5, 1.5, 0.5))

barplot(table(do.call(rbind, lapply(worm_lspm_adapt4, function(x) x$iter_d)))/ length(do.call(rbind, lapply(worm_lspm_adapt4, function(x) x$iter_d)))
        , ylim=c(0,1), xlab = expression(p), ylab="Posterior proportion", cex.lab=1.5, cex.axis=1.2, cex.names=1.2)

dev.off()


# BIC values --------------------------------------------------------------

pdf("~/lspm/inst/extdata/adapt/figure/worm_bic.pdf", width = 5, height = 4)
par(mar = c(4.6, 4.5, 1.5, 0.5))
plot(c(28030.98, 24951.29, 23961.24, 23923.80, 24333.35, 25127.20, 26110.55),
     ylab="BIC", xlab="Dimension", pch=20, col="blue", cex=2, cex.lab=1.5, cex.axis=1.2)
dev.off()

# Procrustes Correlation --------------------------------------------------

full_worm_protest4d <- full_worm_protest3d <- c()
for(seed in seed_number[1:10]) {
  d=3
  if(names(which.max(table(worm_lspm_adapt4[[paste0("seed",(seed))]]$iter_d))) == d) {
    worm_protest <- protest(apply(worm_lspm_adapt4[[paste0("seed",(seed))]]$mcmc_chain[[paste0(d, "D")]]$positions, c(1,2), mean, na.rm=T)[,1:d], worm_lpm3d_full[[paste0("seed",(seed))]]$mcmc.pmode$Z)
    full_worm_protest4d <- append(full_worm_protest4d, list(temp=worm_protest))
    names(full_worm_protest4d)[names(full_worm_protest4d)=="temp"] <- paste0("seed",(seed))
  }

  d=4
  if(names(which.max(table(worm_lspm_adapt4[[paste0("seed",(seed))]]$iter_d))) == d) {
    worm_protest <- protest(apply(worm_lspm_adapt4[[paste0("seed",(seed))]]$mcmc_chain[[paste0(d, "D")]]$positions, c(1,2), mean, na.rm=T)[,1:d], worm_lpm4d_full[[paste0("seed",(seed))]]$mcmc.pmode$Z)
    full_worm_protest3d <- append(full_worm_protest3d, list(temp=worm_protest))
    names(full_worm_protest3d)[names(full_worm_protest3d)=="temp"] <- paste0("seed",(seed))
  }
}

# Procrustes correlation median
round(rbind(
  median(do.call(rbind, lapply(full_worm_protest4d, function(x) x$scale))),
  median(do.call(rbind, lapply(full_worm_protest3d, function(x) x$scale)))
), 3)



# Procrustes correlation 95% credible interval
round(rbind(
  quantile((do.call(rbind, lapply(full_worm_protest4d, function(x) x$scale))), c(0.025, 0.975)),
  quantile((do.call(rbind, lapply(full_worm_protest3d, function(x) x$scale))), c(0.025, 0.975))
), 3)



palette(c("black"  , "red"  ,   "green3"  ,"blue"   , "cyan"  ,  "magenta" ,"yellow"  ,"gray"  )) # default palette

library(igraph)
set.seed(115)
plot(graph_from_adjacency_matrix(worm),
     vertex.label.color="", edge.arrow.size=.3)
