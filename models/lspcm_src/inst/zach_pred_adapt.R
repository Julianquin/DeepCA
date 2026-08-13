load(file = "~/lspm/inst/extdata/adapt/zach_lspm_adapt.Rdata")
load(file = "~/lspm/inst/extdata/adapt/zach_lpm1d_full.Rdata")
load(file = "~/lspm/inst/extdata/adapt/zach_lpm2d_full.Rdata")

set.seed(1234)
seed_number <- sample(1:1e6, 10)

# zach posterior predictive result -----------------------------------------

n_pos = 30
d = 2
set.seed(15)
full_pred_zach_lspm_adapt_2d <- c()
for(seed in seed_number[1:10]) {
  if(names(which.max(table(zach_lspm_adapt[[paste0("seed",(seed))]]$iter_d))) == d) {
    pred_zach_lspm_adapt <- as.data.frame(predcheck(n_pos, n_dimen = d, LSPM_object = zach_lspm_adapt[[paste0("seed",(seed))]], seed = 1234))
    full_pred_zach_lspm_adapt_2d[[paste0("seed",(seed))]] = pred_zach_lspm_adapt
  }
  # full_pred_zach_lspm_adapt_2d <- rbind(full_pred_zach_lspm_adapt_2d, pred_zach_lspm)
}

######
set.seed(15)
full_pred_zach_lpm1d <- c()
for(seed in seed_number[1:10]) {
  pred_zach_lpm <- as.data.frame(predcheck(n_pos,
                                           alpha_chain = zach_lpm1d_full[[paste0("seed",(seed))]]$sample$beta[,1],
                                           latent_pos_chain = aperm(zach_lpm1d_full[[paste0("seed",(seed))]]$sample$Z, c(2,3,1)),
                                           network = zachary))
  full_pred_zach_lpm1d[[paste0("seed",(seed))]] = pred_zach_lpm
  # full_pred_zach_lpm1d <- rbind(full_pred_zach_lpm1d, pred_zach_lpm)
}


set.seed(15)
full_pred_zach_lpm2d <- c()
for(seed in seed_number[1:10]) {
  pred_zach_lpm <- as.data.frame(predcheck(n_pos,
                                           alpha_chain = zach_lpm2d_full[[paste0("seed",(seed))]]$sample$beta[,1],
                                           latent_pos_chain = aperm(zach_lpm2d_full[[paste0("seed",(seed))]]$sample$Z, c(2,3,1)),
                                           network = zachary))
  full_pred_zach_lpm2d[[paste0("seed",(seed))]] = pred_zach_lpm
  # full_pred_zach_lpm2d <- rbind(full_pred_zach_lpm2d, pred_zach_lpm)
}



# zach predictive plots ----------------------------------------------------

# open the pdf file
cairo_pdf("~/lspm/inst/extdata/adapt/figure/zach_pred.pdf", width = 8, height = 2)

palette(c("black"  , "red"  ,   "green3"  ,"blue"   , "cyan"  ,  "magenta" ,"yellow"  ,"gray"  )) # default palette
old.par <- par(no.readonly = TRUE) # save current layout setting
mycol=adjustcolor(palette(), alpha.f = 0.1)
palette(mycol)

plot_names = c("Network Density","Transitivity","Accuracy","F1 Score","Hamming Distance")

obs_zach_d <- gden(zachary)
obs_zach_t <- gtrans(zachary)

par(mfrow=c(1,5))
layout.matrix <- matrix(1:5, nrow = 1, ncol = 5)
layout(mat = layout.matrix, widths = c(2.8,1.8,1.8,1.8,1.9))
par(mar = c(5.6, 5.3, 2.1, 0.2))

density_zach_adapt2d = Map('-', lapply(full_pred_zach_lspm_adapt_2d, function(x) x$Density), obs_zach_d)
density_zach_lpm1d = Map('-', lapply(full_pred_zach_lpm1d, function(x) x$Density), obs_zach_d)
all_density = Map(cbind, density_zach_adapt2d, density_zach_lpm1d)

vioplot(do.call(rbind,all_density), cex=0.2, rectCol = NA, lineCol = NA,
        xaxt="n", cex.names=1.4, horizontal = TRUE,
        # ylim=c(0,1),
        ylim=c(min(unlist(all_density))-0.02,max(unlist(all_density)+0.02)),
        colMed = NA, border=NA, col=rgb(red = 1, green=0.8, blue=1,alpha=0))

lapply(density_zach_adapt2d, function(x)
  vioplot(x, add=TRUE, cex=0.2, rectCol = NA, lineCol = NA, cex.lab=1.2, at=1,
          colMed = NA, border=NA, col=1, horizontal = TRUE))
lapply(density_zach_lpm1d, function(x)
  vioplot(x, add=TRUE, cex=0.2, rectCol = NA, lineCol = NA, cex.lab=1.2, at=2,
          colMed = NA, border=NA, col=4, horizontal = TRUE))

axis(2,1:2, las=2,labels=c(
                           expression(paste(atop(NA, atop("LSPM", paste(p[m]," = 2") )))),
                           expression(paste(atop(NA, atop("LPM", paste(p, " = 1")))))
), cex.axis=2)

title(xlab=plot_names[1], line=2.75, cex.lab=1.4)
abline(v=0, col ="red", lty=2, lwd=2)

par(mar = c(5.6, 0, 2.1, 0.2))

transitivity_zach_adapt2d = Map('-', lapply(full_pred_zach_lspm_adapt_2d, function(x) x$Transitivity), obs_zach_t)
transitivity_zach_lpm1d = Map('-', lapply(full_pred_zach_lpm1d, function(x) x$Transitivity), obs_zach_t)

all_transitivity = Map(cbind, transitivity_zach_adapt2d,transitivity_zach_lpm1d)

vioplot(do.call(rbind,all_transitivity), cex=0.2, rectCol = NA, lineCol = NA,
        xaxt="n", cex.names=1.4, horizontal = TRUE,
        # ylim=c(0,1),
        ylim=c(min(unlist(all_transitivity))-0.03,max(unlist(all_transitivity)+0.02)),
        colMed = NA, border=NA, col=rgb(red = 1, green=0.8, blue=1,alpha=0))

lapply(transitivity_zach_adapt2d, function(x)
  vioplot(x, add=TRUE, cex=0.2, rectCol = NA, lineCol = NA, cex.lab=1.2, at=1,
          colMed = NA, border=NA, col=1, horizontal = TRUE))
lapply(transitivity_zach_lpm1d, function(x)
  vioplot(x, add=TRUE, cex=0.2, rectCol = NA, lineCol = NA, cex.lab=1.2, at=2,
          colMed = NA, border=NA, col=4, horizontal = TRUE))
title(xlab=plot_names[2], line=2.75, cex.lab=1.4)
abline(v=0, col ="red", lty=2, lwd=2)


accuracy_zach_adapt2d = lapply(full_pred_zach_lspm_adapt_2d, function(x) x$Accuracy)
accuracy_zach_lpm1d = lapply(full_pred_zach_lpm2d, function(x) x$Accuracy)
all_accuracy = Map(cbind, accuracy_zach_adapt2d, accuracy_zach_lpm1d)

vioplot(do.call(rbind,all_accuracy), cex=0.2, rectCol = NA, lineCol = NA,
        xaxt="n", cex.names=1.4, horizontal = TRUE,
        # ylim=c(0,1),
        ylim=c(min(unlist(all_accuracy))-0.02,max(unlist(all_accuracy)+0.01)),
        colMed = NA, border=NA, col=rgb(red = 1, green=0.8, blue=1,alpha=0))

lapply(accuracy_zach_adapt2d, function(x)
  vioplot(x, add=TRUE, cex=0.2, rectCol = NA, lineCol = NA, cex.lab=1.2, at=1,
          colMed = NA, border=NA, col=1, horizontal = TRUE))
lapply(accuracy_zach_lpm1d, function(x)
  vioplot(x, add=TRUE, cex=0.2, rectCol = NA, lineCol = NA, cex.lab=1.2, at=2,
          colMed = NA, border=NA, col=4, horizontal = TRUE))

title(xlab=plot_names[3], line=2.75, cex.lab=1.4)


F1score_zach_adapt2d = lapply(full_pred_zach_lspm_adapt_2d, function(x) x$F1score)
F1score_zach_lpm1d = lapply(full_pred_zach_lpm1d, function(x) x$F1score)
all_F1score = Map(cbind, F1score_zach_adapt2d, F1score_zach_lpm1d)

vioplot(do.call(rbind,all_F1score), cex=0.2, rectCol = NA, lineCol = NA,
        xaxt="n", cex.names=1.4, horizontal = TRUE,
        # ylim=c(0,1),
        ylim=c(min(unlist(all_F1score))-0.02,max(unlist(all_F1score)+0.02)),
        colMed = NA, border=NA, col=rgb(red = 1, green=0.8, blue=1,alpha=0))

lapply(F1score_zach_adapt2d, function(x)
  vioplot(x, add=TRUE, cex=0.2, rectCol = NA, lineCol = NA, cex.lab=1.2, at=1,
          colMed = NA, border=NA, col=1, horizontal = TRUE))
lapply(F1score_zach_lpm1d, function(x)
  vioplot(x, add=TRUE, cex=0.2, rectCol = NA, lineCol = NA, cex.lab=1.2, at=2,
          colMed = NA, border=NA, col=4, horizontal = TRUE))

title(xlab=plot_names[4], line=2.75, cex.lab=1.4)

par(mar = c(5.6, 0, 2.1, 1)) # right margin set as 0.1


Hamming_zach_adapt2d = lapply(full_pred_zach_lspm_adapt_2d, function(x) x$Hamming)
Hamming_zach_lpm1d = lapply(full_pred_zach_lpm1d, function(x) x$Hamming)
all_Hamming = Map(cbind, Hamming_zach_adapt2d, Hamming_zach_lpm1d)

vioplot(do.call(rbind,all_Hamming), cex=0.2, rectCol = NA, lineCol = NA,
        xaxt="n", cex.names=1.4, horizontal = TRUE,
        # ylim=c(0,1),
        ylim=c(min(unlist(all_Hamming))-0.01,max(unlist(all_Hamming)+0.01)),
        colMed = NA, border=NA, col=rgb(red = 1, green=0.8, blue=1,alpha=0))

lapply(Hamming_zach_adapt2d, function(x)
  vioplot(x, add=TRUE, cex=0.2, rectCol = NA, lineCol = NA, cex.lab=1.2, at=1,
          colMed = NA, border=NA, col=1, horizontal = TRUE))
lapply(Hamming_zach_lpm1d, function(x)
  vioplot(x, add=TRUE, cex=0.2, rectCol = NA, lineCol = NA, cex.lab=1.2, at=2,
          colMed = NA, border=NA, col=4, horizontal = TRUE))

title(xlab=plot_names[5], line=2.75, cex.lab=1.4)

par(old.par) # restore previous plot layout setting

# palette("R3") # default palette
palette(c("black"  , "red"  ,   "green3"  ,"blue"   , "cyan"  ,  "magenta" ,"yellow"  ,"gray"  )) # default palette

# Close the pdf file
dev.off()


# zach Shrinkage Strength --------------------------------------------------

# open the pdf file
cairo_pdf("~/lspm/inst/extdata/adapt/figure/zach_pmd.pdf", width = 3.33, height = 4)

par(mar = c(3.6, 4.1, 1.1, 0.2))
# Plot LSPM results for shrinkage strength, zach connectome.
vioplot(cbind(zach_lspm_adapt[[paste0("seed",(seed_number[9]))]]$mcmc_chain$"4D"$deltas[complete.cases(zach_lspm_adapt[[paste0("seed",(seed_number[9]))]]$mcmc_chain$"4D"$deltas),,drop=F]),
        ylim=c(0,17), cex=0.2, rectCol = NA, lineCol = NA, cex.lab=1.2, cex.main=1.5, cex.axis=1.5,
        colMed = NA, border=NA, col=rgb(red = 0, green=0, blue=0,alpha=0),
        main= "", #main='Posterior Mean Variance vs Dimension',
        xlab= "", ylab="")
for(seed in seed_number) {
  lapply(zach_lspm_adapt[[paste0("seed",(seed))]]$mcmc_chain, function(x) vioplot(x$deltas[complete.cases(x$deltas),,drop=F], add=T, cex=0.2, rectCol = NA, lineCol = NA, cex.lab=1.2, cex.main=1.5, cex.axis=1.5,
                                                                                 colMed = NA, border=NA, col=rgb(red = 0, green=0, blue=0,alpha=0.1)))
}
title(xlab=expression(paste("Dimension \U2113")), cex.lab=1.2, line=2.5)
title(ylab=expression(paste("Shrinkage strength ", delta["\U2113"])), line=2.1, cex.lab=1.2)

dev.off()


# zach variance ------------------------------------------------------------

# open the pdf file
cairo_pdf("~/lspm/inst/extdata/adapt/figure/zach_pmv.pdf", width = 3.33, height = 4)

par(mar = c(3.6, 4.1, 1.1, 0.2))
# Plot LSPM results for variances, zach connectome.
vioplot(cbind(zach_lspm_adapt[[paste0("seed",(seed_number[9]))]]$mcmc_chain$"4D"$variances[complete.cases(zach_lspm_adapt[[paste0("seed",(seed_number[9]))]]$mcmc_chain$"4D"$variances),,drop=F]),
        ylim=c(0,5), cex=0.2, rectCol = NA, lineCol = NA, cex.lab=1.2, cex.main=1.5, cex.axis=1.5,
        colMed = NA, border=NA, col=rgb(red = 0, green=0, blue=0,alpha=0),
        main= "", #main='Posterior Mean Variance vs Dimension',
        xlab= "", ylab="")
for(seed in seed_number) {
  lapply(zach_lspm_adapt[[paste0("seed",(seed))]]$mcmc_chain, function(x) vioplot(x$variances[complete.cases(x$variances),,drop=F], add=T, cex=0.2, rectCol = NA, lineCol = NA, cex.lab=1.2, cex.main=1.5, cex.axis=1.5,
                                                                                 colMed = NA, border=NA, col=rgb(red = 0, green=0, blue=0,alpha=0.1)))
}
title(xlab=expression(paste("Dimension \U2113")), cex.lab=1.2, line=2.5)
title(ylab=expression(paste("Variance ", omega["\u2113"]^{-1})), line=2.1, cex.lab=1.2)

dev.off()


# LPM zach position --------------------------------------------------------

palette(c("black"  , "red"  ,   "green3"  ,"blue"   , "cyan"  ,  "magenta" ,"yellow"  ,"gray"  )) # default palette

karate_cluster = c(1, 1, 1, 1, 1, 1, 1, 1, 2, 2, 1, 1, 1, 1, 2, 2, 1, 1, 2, 1, 2, 1, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2) # from library(igraphdata), data(karate)

# open the pdf file
cairo_pdf("~/lspm/inst/extdata/adapt/figure/zach_lpm.pdf", width = 6.66, height = 8)
par(mar = c(5.1, 5.1, 2.1, 2.1))
plot(zach_lpm2d_full$seed295846$mcmc.mle$Z, #ylim=c(-5.5,6.5), xlim=c(-7,9),
     pch=20, cex=2, cex.lab=2.5, cex.axis=2., ylim=c(-7,8), xlim=c(-8,8),
     xlab="Dimension 1", ylab="Dimension 2", main="", type="n")
text(zach_lpm2d_full$seed295846$mcmc.mle$Z, cex=2, col=karate_cluster)
legend("topright", legend = c("Mr. Hi", "John"), col=c(1,2), pch=20, cex=2)


dev.off()

# LSPM zach position -------------------------------------------------------

# open the pdf file
cairo_pdf("~/lspm/inst/extdata/adapt/figure/zach_lspm.pdf", width = 6.66, height = 8)
d=2
zach_lspm_2d_mean_pos = apply(zach_lspm_adapt$seed295846$mcmc_chain[[paste0(d, "D")]]$positions, c(1,2), mean, na.rm=T)
par(mar = c(5.1, 5.1, 2.1, 2.1))
plot(proc.crr(zach_lpm2d_full$seed295846$mcmc.mle$Z, zach_lspm_2d_mean_pos), #ylim=c(-2,3), xlim=c(-2,3),
     cex=2, cex.lab=2.5, cex.axis=2.,
     ylim = c(-1,2.5), xlim=c(-2.2,3.5), type='n',
     xlab="Dimension 1", ylab="Dimension 2", main="")
text(proc.crr(zach_lpm2d_full$seed295846$mcmc.mle$Z, zach_lspm_2d_mean_pos),
     cex=2, col=karate_cluster)
legend("topright", legend = c("Mr. Hi", "John"), col=c(1,2), pch=20, cex=2)

dev.off()


# Prop. Dim. combine 30 chain ---------------------------------------------

# open the pdf file
pdf("~/lspm/inst/extdata/adapt/figure/zach_dim_bar.pdf", width = 6.66, height = 6)

par(mfrow=c(1,1))
par(mar = c(4.6, 5, 1.5, 0.5))

barplot(table(do.call(rbind, lapply(zach_lspm_adapt, function(x) x$iter_d)))/ length(do.call(rbind, lapply(zach_lspm_adapt, function(x) x$iter_d)))
        , ylim=c(0,1), xlab = expression(p), ylab="Posterior proportion", cex.lab=2.5, cex.axis=2.2, cex.names=2.2)
# title(main="low overdispersion", line=1.3, cex.main=1.5)
# title(main=expression(paste("(", p[0], " = 2)")), line=1, cex.main=1.5)

dev.off()


# Procrustes Correlation --------------------------------------------------

full_zach_protest1d <- c()
for(seed in seed_number[1:10]) {
  d=2
  if(names(which.max(table(zach_lspm_adapt[[paste0("seed",(seed))]]$iter_d))) == d) {
    zach_protest <- protest(apply(zach_lspm_adapt[[paste0("seed",(seed))]]$mcmc_chain[[paste0(d, "D")]]$positions, c(1,2), mean, na.rm=T)[,1,drop=F], zach_lpm1d_full[[paste0("seed",(seed))]]$mcmc.pmode$Z)
    full_zach_protest1d <- append(full_zach_protest1d, list(temp=zach_protest))
    names(full_zach_protest1d)[names(full_zach_protest1d)=="temp"] <- paste0("seed",(seed))
  }
}

full_zach_protest2d <- c()
for(seed in seed_number[1:10]) {
  d=2
  if(names(which.max(table(zach_lspm_adapt[[paste0("seed",(seed))]]$iter_d))) == d) {
    zach_protest <- protest(apply(zach_lspm_adapt[[paste0("seed",(seed))]]$mcmc_chain[[paste0(d, "D")]]$positions, c(1,2), mean, na.rm=T), zach_lpm2d_full[[paste0("seed",(seed))]]$mcmc.pmode$Z)
    full_zach_protest2d <- append(full_zach_protest2d, list(temp=zach_protest))
    names(full_zach_protest2d)[names(full_zach_protest2d)=="temp"] <- paste0("seed",(seed))
  }
}



# Procrustes correlation median
round(rbind(
  median(do.call(rbind, lapply(full_zach_protest1d, function(x) x$scale)))
), 3)
round(rbind(
  median(do.call(rbind, lapply(full_zach_protest2d, function(x) x$scale)))
), 3)

# 0.851

# Procrustes correlation 95% credible interval
round(rbind(
  quantile((do.call(rbind, lapply(full_zach_protest1d, function(x) x$scale))), c(0.025, 0.975))
), 3)

round(rbind(
  quantile((do.call(rbind, lapply(full_zach_protest2d, function(x) x$scale))), c(0.025, 0.975))
), 3)

# 2.5% 97.5%
# 0.766 0.936


# palette("R3") # default palette
palette(c("black"  , "red"  ,   "green3"  ,"blue"   , "cyan"  ,  "magenta" ,"yellow"  ,"gray"  )) # default palette

library(igraph)
set.seed(115)
plot(graph_from_adjacency_matrix(zachary), vertex.color=karate_cluster,
     vertex.label.color="", edge.arrow.size=.3)
legend("topright", legend = c("Mr. Hi", "John"), col=c(1,2), pch=20, cex=1.2)
