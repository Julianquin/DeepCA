load(file = "~/lspm/inst/phones.Rdata")

phonesnovlspm <- list()
for(seed in seed_number[2]) {
  lspm_single_result <- list(LSPM(phones$nov, family = "Poisson",
                                  n_dimen= 5, iter=5e5,
                                  step_size = c(4,.5),
                                  initial_adjust = c(1.3,0.01),
                                  burnin= 1, thin=1))
  names(lspm_single_result) <- paste0("seed",(seed))
  phonesnovlspm <-  append(phonesnovlspm, lspm_single_result)
}
class(phonesnovlspm) <- "LSPM"

# Check diagnostic
for(seed in seed_number[1]) {
  diagLSPM(phonesnovlspm[[paste0("seed",(seed))]])
}

# Thinning the LSPM result
phonesnovlspm_thin <- list()
for(seed in seed_number[1:10]) {
  lspm_single_result <- list(thinLSPM(phonesnovlspm[[paste0("seed",(seed))]], burnin=20e4, thin=5e3))
  names(lspm_single_result) <- paste0("seed",(seed))
  phonesnovlspm_thin <-  append(phonesnovlspm_thin, lspm_single_result)
}
class(phonesnovlspm_thin) <- "LSPM"

save(phonesnovlspm_thin, file = "phonesnovlspm_thin.Rdata")

####### predictive

set.seed(15)
full_pred_phone_lspm <- c()
for(seed in seed_number[1:10]) {
  pred_phone_lpm <- list(predcheck(n_pos, phonesnovlspm_thin[[paste0("seed",(seed))]]$alpha, phonesnovlspm_thin[[paste0("seed",(seed))]]$positions, phones$nov, type = "count"))
  names(pred_phone_lpm) <- paste0("seed",(seed))
  full_pred_phone_lspm <-  append(full_pred_phone_lspm, pred_phone_lpm)
}
# predBar2(full_pred_phone_lspm, phones$nov)


# open the pdf file
cairo_pdf("~/lspm/inst/figures/nov_pmd.pdf", width = 5, height = 4)

par(mar=c(4.1, 4.1, 1, 1.1))
vioplot(cbind(phonesnovlspm_thin[[paste0("seed",(seed_number[1]))]]$deltas),
        ylim=c(0,4), cex=0.2, rectCol = NA, lineCol = NA, cex.lab=1.25, cex.main=1.25, cex.axis=1.25,
        colMed = NA, border=NA, col=rgb(red = 0, green=0, blue=0,alpha=0.1),
        main= "", #main='Posterior Mean Variance vs Dimension',
        xlab= "", ylab="")
title(xlab=expression(paste("Dimension")), cex.lab=1.25, line=2.5)
title(ylab=expression(paste("Shrinkage strength")), line=2.2, cex.lab=1.25)
for(seed in seed_number[2:10]) {
  vioplot(phonesnovlspm_thin[[paste0("seed",(seed))]]$deltas, add=TRUE, cex=0.2, rectCol = NA, lineCol = NA,
          colMed = NA, border=NA, col=rgb(red = 0, green=0, blue=0,alpha=0.1))
}
points(apply(t(as.data.frame(lapply(phonesnovlspm_thin,function(x) apply(x$deltas,2,mean)))),2,mean),pch=20, cex=2, col="green")
legend("topleft", legend=c('Posterior distribution', 'Average posterior mean'), pch=c(20,20), col=c("black", "green"), cex=1)
dev.off()



# open the pdf file
cairo_pdf("~/lspm/inst/figures/nov_counts.pdf", width = 5, height = 4)
par(mar=c(4.1, 4.1, 1, 1.1))
haiz = matrix(1, ncol=10)
colnames(haiz) = 0:9
vioplot(haiz,colMed = NA, border=NA, col=rgb(red = 1, green=0.8, blue=1,alpha=0),
        rectCol = NA, lineCol = NA,
        ylab = "Log frequency", xlab = "Observed counts",
        ylim=c(0,10))
for(seed in seed_number[1:10]){
  predBar2(full_pred_phone_lspm[[paste0("seed",(seed))]], phones$nov, add=T, col=rgb(red=0, green = 0, blue = 0, alpha=0.1))
}
legend("topright", legend = c("Observed network", "Posterior predictive network"), col=c("red","black"), pch=c(1,20))
dev.off()

plot(phonesnovlspm_thin, parameters="deltas")

######### binary LSPM

phonesnov_bin = phones$nov
phonesnov_bin[phonesnov_bin>0] = 1


phonesnovlspm_bin <- list()
for(seed in seed_number[1:10]) {
  lspm_single_result <- list(LSPM(phonesnov_bin, family = "logit",
                                  n_dimen= 5, iter=5e5,
                                  step_size = c(4,3),
                                  initial_adjust = c(1.3,0.01),
                                  burnin= 1, thin=1))
  names(lspm_single_result) <- paste0("seed",(seed))
  phonesnovlspm_bin <-  append(phonesnovlspm_bin, lspm_single_result)
}
class(phonesnovlspm_bin) <- "LSPM"

# Check diagnostic
for(seed in seed_number[1:10]) {
  diagLSPM(phonesnovlspm_bin_thin[[paste0("seed",(seed))]])
}

# Thinning the LSPM result
phonesnovlspm_bin_thin <- list()
for(seed in seed_number[1:1]) {
  lspm_single_result <- list(thinLSPM(phonesnovlspm_bin[[paste0("seed",(seed))]], burnin=20e4, thin=5e3))
  names(lspm_single_result) <- paste0("seed",(seed))
  phonesnovlspm_bin_thin <-  append(phonesnovlspm_bin_thin, lspm_single_result)
}
class(phonesnovlspm_bin_thin) <- "LSPM"

save(phonesnovlspm_bin_thin, file = "phonesnovlspm_bin_thin.Rdata")


par(mfrow=c(1,3))
plot(phonesnovlspm_bin_thin$seed761680, col=as.numeric(as.factor(as.matrix(phonenovrace[,2]))), main="Binary LSPM")
plot(proc.crr(phonesnovlspm_bin_thin$seed761680$z_mean[,1:2],phonesnovlspm_thin$seed761680$z_mean[,1:2] ), col=as.numeric(as.factor(as.matrix(phonenovrace[,2]))),
     main="Count LSPM", xlab="Dimension 1", ylab="Dimension 2")

# plot(phonesnovlspm_thin$seed761680, col=as.numeric(as.factor(as.matrix(phonenovrace[,2]))), main="Count LSPM")
plot(c(1,1), type="n", frame.plot = FALSE, xaxt="n", yaxt="n",ylab="", xlab="")
legend("topleft", legend=levels(as.factor(as.matrix(phonenovrace[,2]))), col=1:8, pch=1, cex=1)


phonenovlspmbi_thin_check = predcheckp2(30, phonenovlspmbi_thin$alpha,phonenovlspmbi_thin$positions[,1:2,], beta=phonenovlspmbi_thin$beta1,
                                        network = phonenovlspmbi_thin$initialisation$network, dist_power = 2)


plot(phonesnovlspm_thin, parameter = "deltas")
legend("topright", legend = c("Posterior distribution", "Average posterior mean"), col=c("black","green3"), pch=c(20,20), cex=.9)



phones_lpm_net1 <- ergmm(as.network(phone) ~ euclidean(d=1), family = "Poisson", seed = 115, control = control.ergmm(burnin=50000))
phones_lpm_net2 <- ergmm(as.network(phones$nov) ~ euclidean(d=2), family = "Poisson", seed = 115, control = control.ergmm(burnin=50000))
phones_lpm_net3 <- ergmm(as.network(phones$nov) ~ euclidean(d=3), family = "Poisson", seed = 115, control = control.ergmm(burnin=50000))
phones_lpm_net4 <- ergmm(as.network(phones$nov) ~ euclidean(d=4), family = "Poisson", seed = 115, control = control.ergmm(burnin=50000))
phones_lpm_net5 <- ergmm(as.network(phones$nov) ~ euclidean(d=5), family = "Poisson", seed = 115, control = control.ergmm(burnin=50000))
summary(phones_lpm_net1)$bic$overall
summary(phones_lpm_net2)$bic$overall
summary(phones_lpm_net3)$bic$overall
summary(phones_lpm_net4)$bic$overall
summary(phones_lpm_net5)$bic$overall

phones_lpm_net1_bin <- ergmm(as.network(phonesnov_bin) ~ euclidean(d=1), seed = 115)
phones_lpm_net2_bin <- ergmm(as.network(phonesnov_bin) ~ euclidean(d=2), seed = 115)
phones_lpm_net3_bin <- ergmm(as.network(phonesnov_bin) ~ euclidean(d=3), seed = 115)

d=4
phone_lspm_4d_mean_pos = apply(phonesnovlspm_adapt$seed345167$mcmc_chain[[paste0(d, "D")]]$positions, c(1,2), mean, na.rm=T)
protest(phone_lspm_4d_mean_pos[,1,drop=F], phones_lpm_net1$mcmc.pmode$Z)
