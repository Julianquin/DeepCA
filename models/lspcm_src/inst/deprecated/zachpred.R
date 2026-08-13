load(file = "~/lspm/inst/extdata/binary/zach_lspm3d_thin.Rdata")
load(file = "~/lspm/inst/extdata/binary/zach_lspm_thin.Rdata")
load(file = "~/lspm/inst/extdata/binary/zach_lpm_net1.Rdata")


# zach_lspm_thin_alpha
n_pos=30
full_pred_zach_lspm <- c()
for(seed in seed_number[1:10]) {
  pred_zach_lspm <- as.data.frame(predcheck(n_pos, zach_lspm_thin[[paste0("seed",(seed))]]$alpha, zach_lspm_thin[[paste0("seed",(seed))]]$positions, zachary))
  full_pred_zach_lspm <- rbind(full_pred_zach_lspm, pred_zach_lspm)
}

full_pred_zach_lspm3d <- c()
for(seed in seed_number[1:10]) {
  pred_zach_lspm <- as.data.frame(predcheck(n_pos, zach_lspm3d_thin[[paste0("seed",(seed))]]$alpha, zach_lspm3d_thin[[paste0("seed",(seed))]]$positions, zachary))
  full_pred_zach_lspm3d <- rbind(full_pred_zach_lspm3d, pred_zach_lspm)
}

n_pos=30
full_pred_zach_lspm2d <- c()
for(seed in seed_number[1:10]) {
  pred_zach_lspm <- as.data.frame(predcheck(n_pos, zach_lspm2d[[paste0("seed",(seed))]]$alpha, zach_lspm2d[[paste0("seed",(seed))]]$positions, zachary))
  full_pred_zach_lspm2d <- rbind(full_pred_zach_lspm2d, pred_zach_lspm)
}

n_pos=30
full_pred_zach_lspm1d <- c()
for(seed in seed_number[1:10]) {
  pred_zach_lspm <- as.data.frame(predcheck(n_pos, zach_lspm1d[[paste0("seed",(seed))]]$alpha, zach_lspm1d[[paste0("seed",(seed))]]$positions, zachary))
  full_pred_zach_lspm1d <- rbind(full_pred_zach_lspm1d, pred_zach_lspm)
}

# Running 2D LPM on zachary
# zach_net2 <- ergmm(zachary ~ euclidean(d=2), seed = 115)
# save(zach_net2, file = "zach_net2.Rdata")
# full_pred_zach_lpm <- as.data.frame(predcheck(n_pos, zach_net2$sample$beta[,1], aperm(zach_net2$sample$Z, c(2,3,1)), zachary))

full_pred_zach_lpm <- as.data.frame(predcheck(n_pos, zach_lpm_net1$sample$beta[,1], aperm(zach_lpm_net1$sample$Z, c(2,3,1)), zachary))

# open the pdf file
cairo_pdf("~/lspm/inst/extdata/adapt/figure/zach_pred0.pdf", width = 10, height = 2.5)
palette(c("black"  , "red"  ,   "green3"  ,"blue"   , "cyan"  ,  "magenta" ,"yellow"  ,"gray"  )) # default palette

old.par <- par(no.readonly = TRUE) # save current layout setting
mycol=adjustcolor(palette(), alpha.f = 0.1)
palette(mycol)

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
  vioplot(cbind(full_pred_zach_lspm[,i], full_pred_zach_lspm3d[,i],full_pred_zach_lspm2d[,i],full_pred_zach_lspm1d[,i], full_pred_zach_lpm[,i]), cex=0.2, rectCol = NA, lineCol = NA,
          xlab="", xaxt="n", cex.names=1.3, horizontal = TRUE,
          # ylim=c(0,1),
          ylim=c(min((cbind(full_pred_zach_lspm[,i], full_pred_zach_lspm3d[,i],full_pred_zach_lspm2d[,i],full_pred_zach_lspm1d[,i], full_pred_zach_lpm[,i])))-0.01,(max(cbind(full_pred_zach_lspm[,i], full_pred_zach_lspm3d[,i],full_pred_zach_lspm2d[,i],full_pred_zach_lspm1d[,i], full_pred_zach_lpm[,i])))+0.01),
          colMed = NA, border=NA, col=rgb(red = 1, green=0.8, blue=1,alpha=0))
  title(xlab=plot_names[i], line=2.75, cex.lab=1.6)
  if(i == 1) {
    axis(2,1:5, las=1,labels=c("LSPM5", "LSPM3", "LSPM2", "LSPM1","LPM1"), cex.axis=1.3)
  }
  for(start_point in seq(1,n_pos*10, n_pos)) {
    vioplot(full_pred_zach_lspm[start_point:(start_point+n_pos-1),i], add=TRUE,cex=0.2, rectCol = NA, lineCol = NA,
            colMed = NA, border=NA, col=1, at=1, horizontal = TRUE)
    vioplot(full_pred_zach_lspm3d[start_point:(start_point+n_pos-1),i], add=TRUE,cex=0.2, rectCol = NA, lineCol = NA,
            colMed = NA, border=NA, col=1, at=2, horizontal = TRUE)
    vioplot(full_pred_zach_lspm2d[start_point:(start_point+n_pos-1),i], add=TRUE,cex=0.2, rectCol = NA, lineCol = NA,
            colMed = NA, border=NA, col=1, at=3, horizontal = TRUE)
    vioplot(full_pred_zach_lspm1d[start_point:(start_point+n_pos-1),i], add=TRUE,cex=0.2, rectCol = NA, lineCol = NA,
            colMed = NA, border=NA, col=1, at=4, horizontal = TRUE)
    vioplot(full_pred_zach_lpm[1:(1+n_pos-1),i], add=TRUE,cex=0.2, rectCol = NA, lineCol = NA,
            colMed = NA, border=NA, col=4, at=5, horizontal = TRUE)
  }
  # vioplot(full_pred_zach_lpm[1:(1+n_pos-1),i], add=TRUE,cex=0.2, rectCol = NA, lineCol = NA,
  #         colMed = NA, border=NA, col=rgb(red = 1, green=0, blue=.5,alpha=1), at=2)

  if(i==1){
    # title(xlab="(b)", line=2.9)
  }
  if(i==2){
    # title(xlab="(c)", line=2.9)
  }
  if(i==1){
    abline(v=gden(zachary), cex=2, col="red")
    # title(xlab="(d)", line=2.9)
  }
  if(i==2){
    abline(v=gtrans(zachary), cex=2, col="red")
    # title(xlab="(e)", line=2.9)
  }
  if(i==5){
    # title(xlab="(f)", line=2.9)
  }
}
# mtext("Zachary Network", side = 3, line = -2.5, outer = TRUE)
par(old.par) # restore previous plot layout setting

palette(c("black"  , "red"  ,   "green3"  ,"blue"   , "cyan"  ,  "magenta" ,"yellow"  ,"gray"  )) # default palette

# Close the pdf file
dev.off()
