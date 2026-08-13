load(file = "~/lspm/inst/extdata/binary/cat_lspm5d_thin.Rdata")
load(file = "~/lspm/inst/extdata/binary/cat_lpm2d_full.Rdata")
load(file = "~/lspm/inst/extdata/binary/cat_lpm2d.Rdata")

catadj <- as.matrix(as_adjacency_matrix(read.graph("~/lspm/inst/mixed.species_brain_1.graphml", format="graphml")))

# cat
n_pos=30
set.seed(15)
full_pred_cat_lspm <- c()
for(seed in seed_number[1:10]) {
  pred_cat_lspm <- as.data.frame(predcheck(n_pos, alpha_chain = cat_lspm5d_thin[[paste0("seed",(seed))]]$alpha, latent_pos_chain =  cat_lspm5d_thin[[paste0("seed",(seed))]]$positions, network = catadj))
  full_pred_cat_lspm <- rbind(full_pred_cat_lspm, pred_cat_lspm)
}

set.seed(15)
full_pred_cat_lpm <- c()
for(seed in seed_number[1:10]) {
  pred_cat_lpm <- as.data.frame(predcheck(n_pos, cat_lpm2d_full[[paste0("seed",(seed))]]$sample$beta[,1], aperm(cat_lpm2d_full[[paste0("seed",(seed))]]$sample$Z, c(2,3,1)), catadj))
  full_pred_cat_lpm <- rbind(full_pred_cat_lpm, pred_cat_lpm)
}

# set.seed(15)
# full_pred_cat_lpm <- as.data.frame(predcheck(n_pos, cat_lpm2d$sample$beta[,1], aperm(cat_lpm2d$sample$Z, c(2,3,1)), catadj))

# open the pdf file
cairo_pdf("~/lspm/inst/figures/catpred.pdf", width = 10, height = 2)

old.par <- par(no.readonly = TRUE) # save current layout setting
mycol=adjustcolor(palette(), alpha.f = 0.1)
palette(mycol)

par(mfrow=c(1,5))


plot_names <- c("Network Density", "Transitivity", "Accuracy", "F1 Score", "Hamming Distance")
for(i in 1:5){
  if(i==1) {
    par(mfrow=c(1,5))
    layout.matrix <- matrix(1:5, nrow = 1, ncol = 5)
    layout(mat = layout.matrix, widths = c(2.6,2,2,2,2.1))
    par(mar = c(5.6, 5.1, 2.1, 0.2))
  } else if(i==2){
    par(mar = c(5.6, 0, 2.1, 0.2))
  } else if(i == 5 ){
    par(mar = c(5.6, 0, 2.1, 1)) # right margin set as 0.1
  }

  vioplot(cbind(full_pred_cat_lspm[,i], full_pred_w,
  at_lpm[,i]), cex=0.2, rectCol = NA, lineCol = NA,
          xlab="", xaxt="n", cex.names=1.5, horizontal = TRUE,
          # ylim=c(0,1),
          ylim=c(min((cbind(full_pred_cat_lspm[,i], full_pred_cat_lpm[,i]))),(max(cbind(full_pred_cat_lspm[,i], full_pred_cat_lpm[,i])))),
          colMed = NA, border=NA, col=rgb(red = 1, green=0.8, blue=1,alpha=0))
  title(xlab=plot_names[i], line=2.75, cex.lab=1.6)
  if(i == 1) {
    axis(2,1:2, las=1,labels=c("LSPM5","LPM2"), cex.axis=1.3)
  }
 for(start_point in seq(1,n_pos*10, n_pos)) {
    vioplot(full_pred_cat_lspm[start_point:(start_point+n_pos-1),i], add=TRUE,cex=0.2, rectCol = NA, lineCol = NA,
            colMed = NA, border=NA, col=1, at=1, horizontal = TRUE)
    vioplot(full_pred_cat_lpm[start_point:(start_point+n_pos-1),i], add=TRUE,cex=0.2, rectCol = NA, lineCol = NA,
            colMed = NA, border=NA, col=4, at=2, horizontal = TRUE)
  }
  # vioplot(full_pred_cat_lpm[1:(1+n_pos-1),i], add=TRUE,cex=0.2, rectCol = NA, lineCol = NA,
  #         colMed = NA, border=NA, col="blue", at=2)

  if(i==1){
    # title(xlab="(a)", line=2.9)
  }
  if(i==2){
    # title(xlab="(b)", line=2.9)
  }
  if(i==1){
    abline(v=gden(catadj), cex=2, col="red")
    # title(xlab="(c)", line=2.9)
  }
  if(i==2){
    abline(v=gtrans(catadj), cex=2, col="red")
    # title(xlab="(d)", line=2.9)
  }
  if(i==5){
    # title(xlab="(e)", line=2.9)
  }
}
# mtext("Cat Connectome Network", side = 3, line = -2.5, outer = TRUE)

dev.off()

par(old.par) # restore previous plot layout setting
richclub <- rep(1,65)
richclub[c(12,15,16,26,27,38,41,48,51,52,53,54,55,59,60)] <- 2
# Cat positions ------------------------------------------------------
old.par <- par(no.readonly = TRUE) # save current layout setting
par(mfrow=c(1,3))
cat_mean10<- c()
for(seed in seed_number) {
  cat_mean10 <- abind(cat_mean10, cat_lspm5d_thin[[paste0("seed",(seed))]]$z_mean[,1:2], along=3)
}
cat_mean <- apply(cat_mean10, c(1,2), mean)

# plot(cat_lspm5d_thin, parameter='deltas', cex.lab=1.5) # plot shrinkage strength
# # title(xlab="(a)", line=3)
# legend("topleft", legend=c('Posterior distribution', 'Mean value'), pch=c(20,20), col=c("black", "green"), cex=1)


# open the pdf file
cairo_pdf("~/lspm/inst/figures/catdelta.pdf", width = 3.33, height = 4)

par(mar = c(3.6, 4.1, 1.1, 0.2))
# Plot LSPM 5D results for shrinkage strength, 20 nodes.
vioplot(cbind(cat_lspm5d_thin[[paste0("seed",(seed_number[1]))]]$deltas),
        ylim=c(0.4,4.2), cex=0.2, rectCol = NA, lineCol = NA, cex.lab=1.5, cex.main=1.25, cex.axis=1.25,
        colMed = NA, border=NA, col=rgb(red = 0, green=0, blue=0,alpha=0.1),
        main= "", #main='Posterior Mean Variance vs Dimension',
        xlab= "", ylab="")
title(xlab=expression(paste("Dimension")), cex.lab=1.3, line=2.5)
title(ylab=expression(paste("Shrinkage strength, ", delta["\U2113"])), line=2.1, cex.lab=1.3)
for(seed in seed_number[2:10]) {
  vioplot(cat_lspm5d_thin[[paste0("seed",(seed))]]$deltas, add=TRUE, cex=0.2, rectCol = NA, lineCol = NA,
          colMed = NA, border=NA, col=rgb(red = 0, green=0, blue=0,alpha=0.1))
}
points(apply(t(as.data.frame(lapply(cat_lspm5d_thin,function(x) apply(x$deltas,2,mean)))),2,mean),pch=20, cex=2, col="green")
legend("topleft", legend=c('Posterior distribution', 'Average posterior mean'), pch=c(20,20), col=c("black", "green"), cex=1)

dev.off()

palette(c("black"  , "red"  ,   "green3"  ,"blue"   , "cyan"  ,  "magenta" ,"yellow"  ,"gray"  )) # default palette

# open the pdf file
cairo_pdf("~/lspm/inst/figures/catlpm.pdf", width = 6.66, height = 8)
par(mar = c(5.1, 5.1, 2.1, 2.1))
regionroles <- c(rep(1,18), rep(2,10), rep(3,18), rep(4,19))
plot(cat_lpm2d_full$seed345167$mcmc.mle$Z, col=regionroles, #ylim=c(-5.5,6.5), xlim=c(-7,9),
     pch=regionroles+14, cex=2, cex.lab=2.5, cex.axis=2.,
     xlab="Dimension 1", ylab="Dimension 2", main="")
# text(cat_lpm2d_full$seed345167$mcmc.mle$Z, labels = regionnames)
legend("topright", legend=c('Visuals', "Auditory", 'Somatomotor', "Frontolimbic"),
       pch=14+1:4, col=1:4, cex=1.5)

dev.off()

# open the pdf file
cairo_pdf("~/lspm/inst/figures/catlspm.pdf", width = 6.66, height = 8)
par(mar = c(5.1, 5.1, 2.1, 2.1))
plot(proc.crr(cat_lpm2d_full$seed345167$mcmc.mle$Z, cat_lspm5d_thin$seed345167$z_mean[,1:2]), #ylim=c(-2,3), xlim=c(-2,3),
     col=regionroles, pch=regionroles+14, cex=2, cex.lab=2.5, cex.axis=2.,
     ylim = c(-2.5,2.5), xlim=c(-2,3),
     xlab="Dimension 1", ylab="Dimension 2", main="")
# text(proc.crr(cat_lpm2d_full$seed345167$mcmc.mle$Z, cat_lspm5d_thin$seed345167$z_mean[,1:2]), labels = regionnames)
# plot(proc.crr(cat_lpm2d$mcmc.mle$Z, cat_mean), #ylim=c(-2,3), xlim=c(-2,3),
#      col=regionroles, pch=20, cex=1.5, ylim = c(-2,3), xlim=c(-2,3),
#      xlab="Dimension 1", ylab="Dimension 2", main="LSPM Cat Connectome Latent Positions")
# for(seed in seed_number) {
#   points(proc.crr(cat_lpm2d$mcmc.mle$Z, cat_lspm5d_thin[[paste0("seed",(seed))]]$z_mean[,1:2]), #ylim=c(-2,3), xlim=c(-2,3),
#          col=regionroles, pch=19, cex=.1,
#          xlab="Dimension 1", ylab="Dimension 2", main="LSPM Cat Connectome Latent Positions")
#
# }
legend("topleft", legend=c('Visuals', "Auditory", 'Somatomotor', "Frontolimbic"),
       pch=14+1:4, col=1:4, cex=1.5)
dev.off()




regionnames <- c("17","18","19","PMLS","PLLS","AMLS","ALLS","VLS","DLS","21a","21b","20a","20b",
  "ALG","7","AES","SVA","PS","AI","AII","AAF","DP","PS","VP","V","SSF","EPp","Tem",
  "3a","3b","1","2","SII","SIV","4g","4","6l","6m","POA","am","5al","5bm","5bl",
  "5m","SSAo","SSAi","PFCr","PFCdl","PFCv","PFCdm","la","lg","CGa","CGp","LA",
  "RS","PL","IL","35","36","PSb","Sb","ER","Hipp","Amyg")

# plot(cat_lpm2d$mcmc.mle$Z, col=richclub, #ylim=c(-5.5,6.5), xlim=c(-7,9),
#      pch=20, cex=2,
#      xlab="Dimension 1", ylab="Dimension 2", main="LPM Cat Connectome Latent Positions")
# legend("topright", legend=c("Non-Rich Club",'Rich Club'), pch=20, col=1:2, cex=0.8)
# plot(proc.crr(cat_lpm2d$mcmc.mle$Z, cat_lspm5d_thin$seed761680$z_mean[,1:2]), #ylim=c(-2,3), xlim=c(-2,3),
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
# plot(proc.crr(cat_lpm2d$mcmc.mle$Z, cat_lspm5d_thin$seed761680$z_mean[,1:2]), #ylim=c(-2,3), xlim=c(-2,3),
#      col=regionroles, pch=richclub+17, cex=1.5,
#      xlab="Dimension 1", ylab="Dimension 2", main="LSPM Cat Connectome Latent Positions")
# legend("topright", legend=c('Visuals', "Auditory", 'Somatomotor', "Frontolimbic"), pch=20, col=1:4, cex=0.8)


# palette("R3") # default palette
palette(c("black"  , "red"  ,   "green3"  ,"blue"   , "cyan"  ,  "magenta" ,"yellow"  ,"gray"  )) # default palette

regionroles <- c(rep("black" ,18), rep("red",10), rep("green3",18), rep("blue",19))

set.seed(115)
plot(graph_from_adjacency_matrix(catadj), vertex.color=regionroles,
     vertex.label.color="", edge.arrow.size=.3)
legend("topleft", legend=c('Visuals', "Auditory", 'Somatomotor', "Frontolimbic"), pch=20, col=1:4, cex=1)
