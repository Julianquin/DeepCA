load(file = "~/lspm/inst/extdata/adapt/cat_lspm_adapt.Rdata")
load(file = "~/lspm/inst/extdata/binary/cat_lpm2d_full.Rdata")
load(file = "~/lspm/inst/extdata/binary/cat_lpm3d_full.Rdata")

library(vegan)

# load(file = "~/lspm/inst/extdata/binary/cat_lpm2d.Rdata")

# catadj <- as.matrix(as_adjacency_matrix(read.graph("~/lspm/inst/mixed.species_brain_1.graphml", format="graphml")))

set.seed(1234)
seed_number <- sample(1:1e6, 10)

# Cat posterior predictive result -----------------------------------------

n_pos=30
d = 2
set.seed(15)
full_pred_cat_lspm_adapt_2d <- c()
for(seed in seed_number[1:10]) {
  if(names(which.max(table(cat_lspm_adapt[[paste0("seed",(seed))]]$iter_d))) == d) {
  pred_cat_lspm_adapt <- as.data.frame(predcheck(n_pos, n_dimen = d ,cat_lspm_adapt[[paste0("seed",(seed))]], seed = 1234))
  full_pred_cat_lspm_adapt_2d[[paste0("seed",(seed))]] = pred_cat_lspm_adapt
  }
  # full_pred_cat_lspm_adapt_3d <- rbind(full_pred_cat_lspm_adapt_3d, pred_cat_lspm)
}


d = 3
set.seed(15)
full_pred_cat_lspm_adapt_3d <- c()
for(seed in seed_number[1:10]) {
  if(names(which.max(table(cat_lspm_adapt[[paste0("seed",(seed))]]$iter_d))) == d) {
    pred_cat_lspm_adapt <- as.data.frame(predcheck(n_pos, n_dimen = d ,cat_lspm_adapt[[paste0("seed",(seed))]], seed = 1234))
    full_pred_cat_lspm_adapt_3d[[paste0("seed",(seed))]] = pred_cat_lspm_adapt
  }
  # full_pred_cat_lspm_adapt_3d <- rbind(full_pred_cat_lspm_adapt_3d, pred_cat_lspm)
}

######

set.seed(15)
full_pred_cat_lpm <- c()
for(seed in seed_number[1:10]) {
  pred_cat_lpm <- as.data.frame(predcheck(n_pos,
                                          alpha_chain = cat_lpm2d_full[[paste0("seed",(seed))]]$sample$beta[,1],
                                          latent_pos_chain = aperm(cat_lpm2d_full[[paste0("seed",(seed))]]$sample$Z, c(2,3,1)),
                                          network = cat_connectome))
  full_pred_cat_lpm[[paste0("seed",(seed))]] = pred_cat_lpm
  # full_pred_cat_lpm <- rbind(full_pred_cat_lpm, pred_cat_lpm)
}

set.seed(15)
full_pred_cat_lpm3d <- c()
for(seed in seed_number[1:10]) {
  pred_cat_lpm <- as.data.frame(predcheck(n_pos,
                                          alpha_chain = cat_lpm3d_full[[paste0("seed",(seed))]]$sample$beta[,1],
                                          latent_pos_chain = aperm(cat_lpm3d_full[[paste0("seed",(seed))]]$sample$Z, c(2,3,1)),
                                          network = cat_connectome))
  full_pred_cat_lpm3d[[paste0("seed",(seed))]] = pred_cat_lpm
  # full_pred_cat_lpm3d <- rbind(full_pred_cat_lpm3d, pred_cat_lpm)
}


# Cat predictive plots ----------------------------------------------------

# open the pdf file
cairo_pdf("~/lspm/inst/extdata/adapt/figure/cat_pred.pdf", width = 8, height = 3)

palette(c("black"  , "red"  ,   "green3"  ,"blue"   , "cyan"  ,  "magenta" ,"yellow"  ,"gray"  )) # default palette
old.par <- par(no.readonly = TRUE) # save current layout setting
mycol=adjustcolor(palette(), alpha.f = 0.1)
palette(mycol)

plot_names = c("Network Density","Transitivity","Accuracy","F1 Score","Hamming Distance")

obs_cat_d <- gden(cat_connectome)
obs_cat_t <- gtrans(cat_connectome)

par(mfrow=c(1,5))
layout.matrix <- matrix(1:5, nrow = 1, ncol = 5)
layout(mat = layout.matrix, widths = c(2.8,1.8,1.8,1.8,1.9))
par(mar = c(5.6, 5.3, 2.1, 0.2))

density_cat_adapt3d = Map('-', lapply(full_pred_cat_lspm_adapt_3d, function(x) x$Density), obs_cat_d)
# density_cat_adapt2d = Map('-', lapply(full_pred_cat_lspm_adapt_2d, function(x) x$Density), obs_cat_d)
# density_cat_lpm3d = Map('-', lapply(full_pred_cat_lpm3d, function(x) x$Density), obs_cat_d)
density_cat_lpm2d = Map('-', lapply(full_pred_cat_lpm, function(x) x$Density), obs_cat_d)
all_density = Map(cbind, density_cat_adapt3d,density_cat_lpm2d)

vioplot(do.call(rbind,all_density), cex=0.2, rectCol = NA, lineCol = NA,
        xaxt="n", cex.names=1.4, horizontal = TRUE,
        # ylim=c(0,1),
        ylim=c(min(unlist(all_density))-0.01,max(unlist(all_density)+0.01)),
        colMed = NA, border=NA, col=rgb(red = 1, green=0.8, blue=1,alpha=0))

lapply(density_cat_adapt3d, function(x)
  vioplot(x, add=TRUE, cex=0.2, rectCol = NA, lineCol = NA, cex.lab=1.4, at=1,
          colMed = NA, border=NA, col=1, horizontal = TRUE))
# lapply(density_cat_adapt2d, function(x)
#   vioplot(x, add=TRUE, cex=0.2, rectCol = NA, lineCol = NA, cex.lab=1.4, at=2,
#           colMed = NA, border=NA, col=1, horizontal = TRUE))
# lapply(density_cat_lpm3d, function(x)
#   vioplot(x, add=TRUE, cex=0.2, rectCol = NA, lineCol = NA, cex.lab=1.4, at=3,
#           colMed = NA, border=NA, col=4, horizontal = TRUE))
lapply(density_cat_lpm2d, function(x)
  vioplot(x, add=TRUE, cex=0.2, rectCol = NA, lineCol = NA, cex.lab=1.4, at=2,
          colMed = NA, border=NA, col=4, horizontal = TRUE))

axis(2,1:2, las=2,labels=c(expression(paste(atop(NA, atop("LSPM", paste(p[m], " = 3"))))),
                             # expression(paste(atop(NA, atop("LSPM", paste(p[m], " = 2"))))),
                             # expression(paste(atop(NA, atop("LPM", paste(p, " = 3"))))),
                             expression(paste(atop(NA, atop("LPM", paste(p, " = 2")))))
                             ), cex.axis=2)

title(xlab=plot_names[1], line=2.75, cex.lab=1.5)
abline(v=0, col ="red", lty=2, lwd=2)

par(mar = c(5.6, 0, 2.1, 0.2))

transitivity_cat_adapt3d = Map('-', lapply(full_pred_cat_lspm_adapt_3d, function(x) x$Transitivity), obs_cat_t)
# transitivity_cat_adapt2d = Map('-', lapply(full_pred_cat_lspm_adapt_2d, function(x) x$Transitivity), obs_cat_t)
# transitivity_cat_lpm3d = Map('-', lapply(full_pred_cat_lpm3d, function(x) x$Transitivity), obs_cat_t)
transitivity_cat_lpm = Map('-', lapply(full_pred_cat_lpm, function(x) x$Transitivity), obs_cat_t)

all_transitivity = Map(cbind, transitivity_cat_adapt3d, transitivity_cat_lpm)

vioplot(do.call(rbind,all_transitivity), cex=0.2, rectCol = NA, lineCol = NA,
        xaxt="n", cex.names=1.4, horizontal = TRUE,
        # ylim=c(0,1),
        ylim=c(min(unlist(all_transitivity))-0.01,max(unlist(all_transitivity)+0.01)),
        colMed = NA, border=NA, col=rgb(red = 1, green=0.8, blue=1,alpha=0))

lapply(transitivity_cat_adapt3d, function(x)
  vioplot(x, add=TRUE, cex=0.2, rectCol = NA, lineCol = NA, cex.lab=1.4, at=1,
          colMed = NA, border=NA, col=1, horizontal = TRUE))
# lapply(transitivity_cat_adapt2d, function(x)
#   vioplot(x, add=TRUE, cex=0.2, rectCol = NA, lineCol = NA, cex.lab=1.4, at=2,
#           colMed = NA, border=NA, col=1, horizontal = TRUE))
# lapply(transitivity_cat_lpm3d, function(x)
#   vioplot(x, add=TRUE, cex=0.2, rectCol = NA, lineCol = NA, cex.lab=1.4, at=3,
#           colMed = NA, border=NA, col=4, horizontal = TRUE))
lapply(transitivity_cat_lpm, function(x)
  vioplot(x, add=TRUE, cex=0.2, rectCol = NA, lineCol = NA, cex.lab=1.4, at=2,
          colMed = NA, border=NA, col=4, horizontal = TRUE))
title(xlab=plot_names[2], line=2.75, cex.lab=1.5)
abline(v=0, col ="red", lty=2, lwd=2)


accuracy_cat_adapt3d = lapply(full_pred_cat_lspm_adapt_3d, function(x) x$Accuracy)
# accuracy_cat_adapt2d = lapply(full_pred_cat_lspm_adapt_2d, function(x) x$Accuracy)
# accuracy_cat_lpm3d = lapply(full_pred_cat_lpm3d, function(x) x$Accuracy)
accuracy_cat_lpm = lapply(full_pred_cat_lpm, function(x) x$Accuracy)
all_accuracy = Map(cbind, accuracy_cat_adapt3d, accuracy_cat_lpm)

vioplot(do.call(rbind,all_accuracy), cex=0.2, rectCol = NA, lineCol = NA,
        xaxt="n", cex.names=1.4, horizontal = TRUE,
        # ylim=c(0,1),
        ylim=c(min(unlist(all_accuracy))-0.01,max(unlist(all_accuracy)+0.01)),
        colMed = NA, border=NA, col=rgb(red = 1, green=0.8, blue=1,alpha=0))

lapply(accuracy_cat_adapt3d, function(x)
  vioplot(x, add=TRUE, cex=0.2, rectCol = NA, lineCol = NA, cex.lab=1.4, at=1,
          colMed = NA, border=NA, col=1, horizontal = TRUE))
# lapply(accuracy_cat_adapt2d, function(x)
#   vioplot(x, add=TRUE, cex=0.2, rectCol = NA, lineCol = NA, cex.lab=1.4, at=2,
#           colMed = NA, border=NA, col=1, horizontal = TRUE))
# lapply(accuracy_cat_lpm3d, function(x)
#   vioplot(x, add=TRUE, cex=0.2, rectCol = NA, lineCol = NA, cex.lab=1.4, at=3,
#           colMed = NA, border=NA, col=4, horizontal = TRUE))
lapply(accuracy_cat_lpm, function(x)
  vioplot(x, add=TRUE, cex=0.2, rectCol = NA, lineCol = NA, cex.lab=1.4, at=2,
          colMed = NA, border=NA, col=4, horizontal = TRUE))

title(xlab=plot_names[3], line=2.75, cex.lab=1.5)


F1score_cat_adapt3d = lapply(full_pred_cat_lspm_adapt_3d, function(x) x$F1score)
# F1score_cat_adapt2d = lapply(full_pred_cat_lspm_adapt_2d, function(x) x$F1score)
# F1score_cat_lpm3d = lapply(full_pred_cat_lpm3d, function(x) x$F1score)
F1score_cat_lpm = lapply(full_pred_cat_lpm, function(x) x$F1score)
all_F1score = Map(cbind, F1score_cat_adapt3d, F1score_cat_lpm)

vioplot(do.call(rbind,all_F1score), cex=0.2, rectCol = NA, lineCol = NA,
        xaxt="n", cex.names=1.4, horizontal = TRUE,
        # ylim=c(0,1),
        ylim=c(min(unlist(all_F1score))-0.01,max(unlist(all_F1score)+0.01)),
        colMed = NA, border=NA, col=rgb(red = 1, green=0.8, blue=1,alpha=0))

lapply(F1score_cat_adapt3d, function(x)
  vioplot(x, add=TRUE, cex=0.2, rectCol = NA, lineCol = NA, cex.lab=1.4, at=1,
          colMed = NA, border=NA, col=1, horizontal = TRUE))
# lapply(F1score_cat_adapt2d, function(x)
#   vioplot(x, add=TRUE, cex=0.2, rectCol = NA, lineCol = NA, cex.lab=1.4, at=2,
#           colMed = NA, border=NA, col=1, horizontal = TRUE))
# lapply(F1score_cat_lpm3d, function(x)
#   vioplot(x, add=TRUE, cex=0.2, rectCol = NA, lineCol = NA, cex.lab=1.4, at=3,
#           colMed = NA, border=NA, col=4, horizontal = TRUE))
lapply(F1score_cat_lpm, function(x)
  vioplot(x, add=TRUE, cex=0.2, rectCol = NA, lineCol = NA, cex.lab=1.4, at=2,
          colMed = NA, border=NA, col=4, horizontal = TRUE))

title(xlab=plot_names[4], line=2.75, cex.lab=1.5)

par(mar = c(5.6, 0, 2.1, 1)) # right margin set as 0.1


Hamming_cat_adapt3d = lapply(full_pred_cat_lspm_adapt_3d, function(x) x$Hamming)
# Hamming_cat_adapt2d = lapply(full_pred_cat_lspm_adapt_2d, function(x) x$Hamming)
# Hamming_cat_lpm3d = lapply(full_pred_cat_lpm3d, function(x) x$Hamming)
Hamming_cat_lpm = lapply(full_pred_cat_lpm, function(x) x$Hamming)
all_Hamming = Map(cbind, Hamming_cat_adapt3d, Hamming_cat_lpm)

vioplot(do.call(rbind,all_Hamming), cex=0.2, rectCol = NA, lineCol = NA,
        xaxt="n", cex.names=1.4, horizontal = TRUE,
        # ylim=c(0,1),
        ylim=c(min(unlist(all_Hamming))-0.01,max(unlist(all_Hamming)+0.01)),
        colMed = NA, border=NA, col=rgb(red = 1, green=0.8, blue=1,alpha=0))

lapply(Hamming_cat_adapt3d, function(x)
  vioplot(x, add=TRUE, cex=0.2, rectCol = NA, lineCol = NA, cex.lab=1.4, at=1,
          colMed = NA, border=NA, col=1, horizontal = TRUE))
# lapply(Hamming_cat_adapt2d, function(x)
#   vioplot(x, add=TRUE, cex=0.2, rectCol = NA, lineCol = NA, cex.lab=1.4, at=2,
#           colMed = NA, border=NA, col=1, horizontal = TRUE))
# lapply(Hamming_cat_lpm3d, function(x)
#   vioplot(x, add=TRUE, cex=0.2, rectCol = NA, lineCol = NA, cex.lab=1.4, at=3,
#           colMed = NA, border=NA, col=4, horizontal = TRUE))
lapply(Hamming_cat_lpm, function(x)
  vioplot(x, add=TRUE, cex=0.2, rectCol = NA, lineCol = NA, cex.lab=1.4, at=2,
          colMed = NA, border=NA, col=4, horizontal = TRUE))

title(xlab=plot_names[5], line=2.75, cex.lab=1.5)

par(old.par) # restore previous plot layout setting

# palette("R3") # default palette
palette(c("black"  , "red"  ,   "green3"  ,"blue"   , "cyan"  ,  "magenta" ,"yellow"  ,"gray"  )) # default palette

# Close the pdf file
dev.off()




# par(old.par) # restore previous plot layout setting
# richclub <- rep(1,65)
# richclub[c(12,15,16,26,27,38,41,48,51,52,53,54,55,59,60)] <- 2
# # Cat positions ------------------------------------------------------
# old.par <- par(no.readonly = TRUE) # save current layout setting
# par(mfrow=c(1,3))
# cat_mean10<- c()
# for(seed in seed_number) {
#   cat_mean10 <- abind(cat_mean10, cat_lspm_adapt[[paste0("seed",(seed))]]$z_mean[,1:2], along=3)
# }
# cat_mean <- apply(cat_mean10, c(1,2), mean)

# plot(cat_lspm_adapt, parameter='deltas', cex.lab=1.4) # plot shrinkage strength
# # title(xlab="(a)", line=3)
# legend("topleft", legend=c('Posterior distribution', 'Mean value'), pch=c(20,20), col=c("black", "green"), cex=1)


# Cat Shrinkage Strength --------------------------------------------------

# open the pdf file
cairo_pdf("~/lspm/inst/extdata/adapt/figure/cat_pmd.pdf", width = 3.33, height = 4)

par(mar = c(3.6, 4.1, 1.1, 0.2))
# Plot LSPM results for shrinkage strength, cat connectome.
vioplot(cbind(cat_lspm_adapt[[paste0("seed",(seed_number[1]))]]$mcmc_chain$"4D"$deltas[complete.cases(cat_lspm_adapt[[paste0("seed",(seed_number[1]))]]$mcmc_chain$"4D"$deltas),,drop=F]),
        ylim=c(0,25), cex=0.2, rectCol = NA, lineCol = NA, cex.lab=1.4, cex.main=1.5, cex.axis=1.5,
        colMed = NA, border=NA, col=rgb(red = 0, green=0, blue=0,alpha=0),
        main= "", #main='Posterior Mean Variance vs Dimension',
        xlab= "", ylab="")
for(seed in seed_number) {
  lapply(cat_lspm_adapt[[paste0("seed",(seed))]]$mcmc_chain, function(x) vioplot(x$deltas[complete.cases(x$deltas),,drop=F], add=T, cex=0.2, rectCol = NA, lineCol = NA, cex.lab=1.4, cex.main=1.5, cex.axis=1.5,
                                                                                      colMed = NA, border=NA, col=rgb(red = 0, green=0, blue=0,alpha=0.1)))
}
title(xlab=expression(paste("Dimension \U2113")), cex.lab=1.3, line=2.5)
title(ylab=expression(paste("Shrinkage strength ", delta["\U2113"])), line=2.1, cex.lab=1.3)

dev.off()


# Cat variance ------------------------------------------------------------

# open the pdf file
cairo_pdf("~/lspm/inst/extdata/adapt/figure/cat_pmv.pdf", width = 3.33, height = 4)

par(mar = c(3.6, 4.1, 1.1, 0.2))
# Plot LSPM results for variances, cat connectome.
vioplot(cbind(cat_lspm_adapt[[paste0("seed",(seed_number[1]))]]$mcmc_chain$"4D"$variances[complete.cases(cat_lspm_adapt[[paste0("seed",(seed_number[1]))]]$mcmc_chain$"4D"$variances),,drop=F]),
        ylim=c(0,3), cex=0.2, rectCol = NA, lineCol = NA, cex.lab=1.4, cex.main=1.5, cex.axis=1.5,
        colMed = NA, border=NA, col=rgb(red = 0, green=0, blue=0,alpha=0),
        main= "", #main='Posterior Mean Variance vs Dimension',
        xlab= "", ylab="")
for(seed in seed_number) {
  lapply(cat_lspm_adapt[[paste0("seed",(seed))]]$mcmc_chain, function(x) vioplot(x$variances[complete.cases(x$variances),,drop=F], add=T, cex=0.2, rectCol = NA, lineCol = NA, cex.lab=1.4, cex.main=1.5, cex.axis=1.5,
                                                                                 colMed = NA, border=NA, col=rgb(red = 0, green=0, blue=0,alpha=0.1)))
}
title(xlab=expression(paste("Dimension \U2113")), cex.lab=1.3, line=2.5)
title(ylab=expression(paste("Variance ", omega["\u2113"]^{-1})), line=2.1, cex.lab=1.3)

dev.off()


# LPM cat position --------------------------------------------------------

palette(c("black"  , "red"  ,   "green3"  ,"blue"   , "cyan"  ,  "magenta" ,"yellow"  ,"gray"  )) # default palette

# open the pdf file
cairo_pdf("~/lspm/inst/extdata/adapt/figure/cat_lpm.pdf", width = 6.66, height = 8)
par(mar = c(5.1, 5.1, 2.1, 2.1))
custom_palette <- c("black", "red", "#984EA3", "#e49444", "#6a9f58", "#56B4E9")
regionroles <- c(rep(3,18), rep(4,10), rep(5,18), rep(6,19))
plot(cat_lpm2d_full$seed345167$mcmc.mle$Z, col=regionroles, #ylim=c(-5.5,6.5), xlim=c(-7,9),
     pch=regionroles+14, cex=2, cex.lab=2.5, cex.axis=2.,
     xlab="Dimension 1", ylab="Dimension 2", main="")
# text(cat_lpm2d_full$seed345167$mcmc.mle$Z, labels = regionnames)
legend("topright", legend=c('Visuals', "Auditory", 'Somatomotor', "Frontolimbic"),
       pch=14+1:4, col=3:6, cex=1.5)

dev.off()

# LSPM cat position -------------------------------------------------------

# open the pdf file
cairo_pdf("~/lspm/inst/extdata/adapt/figure/cat_lspm.pdf", width = 6.66, height = 8)
d=3
cat_lspm_3d_mean_pos = apply(cat_lspm_adapt$seed345167$mcmc_chain[[paste0(d, "D")]]$positions, c(1,2), mean, na.rm=T)
par(mar = c(5.1, 5.1, 2.1, 2.1))
plot(proc.crr(cat_lpm2d_full$seed345167$mcmc.mle$Z, cat_lspm_3d_mean_pos[,1:2]), #ylim=c(-2,3), xlim=c(-2,3),
     col=regionroles, pch=regionroles+14, cex=2, cex.lab=2.5, cex.axis=2.,
     ylim = c(-3,3), xlim=c(-2.5,2.5),
     xlab="Dimension 1", ylab="Dimension 2", main="")
legend("topright", legend=c('Visuals', "Auditory", 'Somatomotor', "Frontolimbic"),
       pch=14+1:4, col=1:4, cex=1.5)
dev.off()


# open the pdf file
cairo_pdf("~/lspm/inst/extdata/adapt/figure/cat_lspm_3d.pdf", width = 10, height = 3)

d=3
cat_lspm_3d_mean_pos = apply(cat_lspm_adapt$seed345167$mcmc_chain[[paste0(d, "D")]]$positions, c(1,2), mean, na.rm=T)
par(mfrow=c(1,3))
par(mar = c(5.1, 5.1, 2.1, 2.1))
custom_palette <- c("black", "red", "#984EA3", "#e49444", "#6a9f58", "#56B4E9")
plot(cat_lspm_3d_mean_pos[,1:2], #ylim=c(-2,3), xlim=c(-2,3),
     col=regionroles, pch=regionroles+14, cex=2, cex.lab=1.8, cex.axis=1.5,
     ylim = c(-2,3), xlim=c(-3,3),
     xlab="Dimension 1", ylab="Dimension 2", main="")

plot(cat_lspm_3d_mean_pos[,c(1,3)], #ylim=c(-2,3), xlim=c(-2,3),
     col=regionroles, pch=regionroles+14, cex=2, cex.lab=1.8, cex.axis=1.5,
     ylim = c(-2,3), xlim=c(-3,3),
     xlab="Dimension 1", ylab="Dimension 3", main="")
legend_order <- matrix(1:4,ncol=2,byrow = TRUE)
legend("top",c('Visuals', "Auditory", 'Somatomotor', "Frontolimbic")[legend_order],
       pch=c(14+1:4)[legend_order],
       col=c(3:6)[legend_order],
       ncol=2, cex=1.3)

# legend("topright", legend=c('Visuals', "Auditory", 'Somatomotor', "Frontolimbic"),
#        pch=14+1:4, col=3:6, cex=1.2)

plot(cat_lspm_3d_mean_pos[,2:3], #ylim=c(-2,3), xlim=c(-2,3),
     col=regionroles, pch=regionroles+14, cex=2, cex.lab=1.8, cex.axis=1.5,
     ylim = c(-2,3), xlim=c(-3,3),
     xlab="Dimension 2", ylab="Dimension 3", main="")

dev.off()

protest(cat_lpm2d_full$seed345167$mcmc.pmode$Z, cat_lspm_3d_mean_pos[,1:2])
# Prop. Dim. combine 30 chain ---------------------------------------------

# open the pdf file
pdf("~/lspm/inst/extdata/adapt/figure/cat_dim_bar.pdf", width = 6.66, height = 6)

par(mfrow=c(1,1))
par(mar = c(4.6, 5, 1.5, 0.5))

barplot(table(do.call(rbind, lapply(cat_lspm_adapt, function(x) x$iter_d)))/ length(do.call(rbind, lapply(cat_lspm_adapt, function(x) x$iter_d)))
        , ylim=c(0,1), xlab = expression(p), ylab="Posterior proportion", cex.lab=2.5, cex.axis=2.2, cex.names=2.2)
# title(main="low overdispersion", line=1.3, cex.main=1.5)
# title(main=expression(paste("(", p[0], " = 2)")), line=1, cex.main=1.5)

dev.off()


# Procrustes Correlation --------------------------------------------------

full_cat_protest2d <- full_cat_protest3d <- c()
for(seed in seed_number[1:10]) {
  d=2
  if(names(which.max(table(cat_lspm_adapt[[paste0("seed",(seed))]]$iter_d))) == d) {
    cat_protest <- protest(apply(cat_lspm_adapt[[paste0("seed",(seed))]]$mcmc_chain[[paste0(d, "D")]]$positions, c(1,2), mean, na.rm=T), cat_lpm2d_full[[paste0("seed",(seed))]]$mcmc.pmode$Z)
    full_cat_protest2d <- append(full_cat_protest2d, list(temp=cat_protest))
    names(full_cat_protest2d)[names(full_cat_protest2d)=="temp"] <- paste0("seed",(seed))
  }

  d=3
  if(names(which.max(table(cat_lspm_adapt[[paste0("seed",(seed))]]$iter_d))) == d) {
    cat_protest <- protest(apply(cat_lspm_adapt[[paste0("seed",(seed))]]$mcmc_chain[[paste0(d, "D")]]$positions, c(1,2), mean, na.rm=T), cat_lpm3d_full[[paste0("seed",(seed))]]$mcmc.pmode$Z)
    full_cat_protest3d <- append(full_cat_protest3d, list(temp=cat_protest))
    names(full_cat_protest3d)[names(full_cat_protest3d)=="temp"] <- paste0("seed",(seed))
  }
}

# Procrustes correlation median
round(rbind(
  median(do.call(rbind, lapply(full_cat_protest2d, function(x) x$scale))),
  median(do.call(rbind, lapply(full_cat_protest3d, function(x) x$scale)))
), 3)

# [1,] 0.988
# [2,] 0.971

# Procrustes correlation 95% credible interval
round(rbind(
  quantile((do.call(rbind, lapply(full_cat_protest2d, function(x) x$scale))), c(0.025, 0.975)),
  quantile((do.call(rbind, lapply(full_cat_protest3d, function(x) x$scale))), c(0.025, 0.975))
), 3)

# 2.5% 97.5%
# [1,] 0.988 0.988
# [2,] 0.962 0.977

regionnames <- c("17","18","19","PMLS","PLLS","AMLS","ALLS","VLS","DLS","21a","21b","20a","20b",
                 "ALG","7","AES","SVA","PS","AI","AII","AAF","DP","PS","VP","V","SSF","EPp","Tem",
                 "3a","3b","1","2","SII","SIV","4g","4","6l","6m","POA","am","5al","5bm","5bl",
                 "5m","SSAo","SSAi","PFCr","PFCdl","PFCv","PFCdm","la","lg","CGa","CGp","LA",
                 "RS","PL","IL","35","36","PSb","Sb","ER","Hipp","Amyg")

# plot(cat_lpm2d$mcmc.mle$Z, col=richclub, #ylim=c(-5.5,6.5), xlim=c(-7,9),
#      pch=20, cex=2,
#      xlab="Dimension 1", ylab="Dimension 2", main="LPM Cat Connectome Latent Positions")
# legend("topright", legend=c("Non-Rich Club",'Rich Club'), pch=20, col=1:2, cex=0.8)
# plot(proc.crr(cat_lpm2d$mcmc.mle$Z, cat_lspm_adapt$seed761680$z_mean[,1:2]), #ylim=c(-2,3), xlim=c(-2,3),
#      col=richclub, pch=20, cex=2,
#      xlab="Dimension 1", ylab="Dimension 2", main="LSPM Cat Connectome Latent Positions")
# legend("topright", legend=c("Non-Rich Club",'Rich Club'), pch=20, col=1:2, cex=0.8)
#
# par(old.par) # restore previous plot layout setting
#
#
# plot(cat_lpm2d$mcmc.mle$Z, col=regionroles, #ylim=c(-5.5,6.5), xlim=c(-7,9),
#      pch=richclub+17, cex=1.5,
#      xlab="Dimension 1", ylab="Dimension 2", main="LPM Cat Connectome Latent Positions")
# legend("topright", legend=c('Visuals', "Auditory", 'Somatomotor', "Frontolimbic"), pch=20, col=1:4, cex=0.8)
# plot(proc.crr(cat_lpm2d$mcmc.mle$Z, cat_lspm_adapt$seed761680$z_mean[,1:2]), #ylim=c(-2,3), xlim=c(-2,3),
#      col=regionroles, pch=richclub+17, cex=1.5,
#      xlab="Dimension 1", ylab="Dimension 2", main="LSPM Cat Connectome Latent Positions")
# legend("topright", legend=c('Visuals', "Auditory", 'Somatomotor', "Frontolimbic"), pch=20, col=1:4, cex=0.8)


# palette("R3") # default palette
# palette(c("black"  , "red"  ,   "green3"  ,"blue"   , "cyan"  ,  "magenta" ,"yellow"  ,"gray"  )) # default palette
# regionroles <- c(rep("black" ,18), rep("red",10), rep("green3",18), rep("blue",19))
custom_palette <- c("black", "red", "#984EA3", "#e49444", "#6a9f58", "#56B4E9")
regionroles <- c(rep(3 ,18), rep(4,10), rep(5,18), rep(6,19))

library(igraph)
set.seed(115)
plot(graph_from_adjacency_matrix(cat_connectome), vertex.color=regionroles,
     vertex.label.color="", edge.arrow.size=.3)
legend("topleft", legend=c('Visuals', "Auditory", 'Somatomotor', "Frontolimbic"), pch=20, col=3:6, cex=1)



##################### convergence

# # open the pdf file
# cairo_pdf("~/lspm/inst/figures/alphatrace.pdf", width = 4, height = 3)
# par(mar = c(4.1, 4.6, 1.1, 1.1))
# plot(cat_lspm_adapt$seed345167$alpha, type="l", ylab= expression(paste("Posterior ", alpha, " samples")), xlab="Iteration")
# dev.off()
# cairo_pdf("~/lspm/inst/figures/liketrace.pdf", width = 4, height = 3)
# par(mar = c(4.1, 4.6, 1.1, 1.1))
# plot(cat_lspm_adapt$seed345167$like, type="l", ylab= "Posterior loglikelihood samples", xlab="Iteration")
# dev.off()
# cairo_pdf("~/lspm/inst/figures/alphaacf.pdf", width = 4, height = 3)
# par(mar = c(4.1, 4.6, 1.1, 1.1))
# acf(cat_lspm_adapt$seed345167$alpha, main="")
# dev.off()
# cairo_pdf("~/lspm/inst/figures/likeacf.pdf", width = 4, height = 3)
# par(mar = c(4.1, 4.6, 1.1, 1.1))
# acf(cat_lspm_adapt$seed345167$like, main="")
# dev.off()

cairo_pdf("~/lspm/inst/extdata/adapt/figure/cat_trace.pdf", width = 10, height = 6)
diagLSPM(cat_lspm_adapt$seed345167,n_dimen=3)
dev.off()

# system.time({LSPM(cat_connectome, iter=5e5, step_size = c(3,1.3), seed = seed, burnin= 5e4, thin=2000)})

# LSPM  user  system elapsed
# 630.917   1.434 633.720


# user  system elapsed
# 360.606   0.143 361.540 #2d lpm
# 380.854   0.103 381.797 #3d lpm
# 401.841   0.144 402.858 #4d lpm
