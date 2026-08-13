load(file = "~/lspm/inst/extdata/binary/worm_lspm5d_thin.Rdata")
load(file = "~/lspm/inst/extdata/binary/worm_lpm4d_full.Rdata")

wormadj <- as.matrix(as_adjacency_matrix(read.graph("~/lspm/inst/c.elegans_neural.male_1.graphml", format="graphml")))


# worm
n_pos=30
seed=15
full_pred_worm_lspm <- c()
for(seed in seed_number[1:10]) {
  pred_worm_lspm <- as.data.frame(predcheck(n_pos, worm_lspm5d_thin[[paste0("seed",(seed))]]$alpha, worm_lspm5d_thin[[paste0("seed",(seed))]]$positions, wormadj))
  full_pred_worm_lspm <- rbind(full_pred_worm_lspm, pred_worm_lspm)
}

seed=15
full_pred_worm_lpm <- c()
for(seed in seed_number[1:10]) {
  pred_worm_lpm <- as.data.frame(predcheck(n_pos, worm_lpm4d_full[[paste0("seed",(seed))]]$sample$beta[,1], aperm(worm_lpm4d_full[[paste0("seed",(seed))]]$sample$Z, c(2,3,1)), wormadj))
  full_pred_worm_lpm <- rbind(full_pred_worm_lpm, pred_worm_lpm)
}
# full_pred_worm_lpm <- as.data.frame(predcheck(n_pos, worm_lpm4d$sample$beta[,1], aperm(worm_lpm4d$sample$Z, c(2,3,1)), wormadj))


# open the pdf file
cairo_pdf("~/lspm/inst/figures/wormpred.pdf", width = 10, height = 2)

old.par <- par(no.readonly = TRUE) # save current layout setting
mycol=adjustcolor(palette(), alpha.f = 0.1)
palette(mycol)
# par(mfrow=c(1,5))

# plot(worm_lspm5d_thin, parameter='deltas') # plot shrinkage strength
# title(xlab="(a)", line=3)
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
  vioplot(cbind(full_pred_worm_lspm[,i], full_pred_worm_lpm[,i]), cex=0.2, rectCol = NA, lineCol = NA,
          xlab="", xaxt="n", cex.names=1.5, horizontal = TRUE,
          # ylim=c(0,1),
          ylim=c(min((cbind(full_pred_worm_lspm[,i], full_pred_worm_lpm[,i])))-0.01,(max(cbind(full_pred_worm_lspm[,i], full_pred_worm_lpm[,i])))+0.01),
          colMed = NA, border=NA, col=rgb(red = 1, green=0.8, blue=1,alpha=0))
  title(xlab=plot_names[i], line=2.75, cex.lab=1.6)
  if(i == 1) {
    axis(2,1:2, las=1,labels=c("LSPM5","LPM4"), cex.axis=1.3)
  }
  for(start_point in seq(1,n_pos*10, n_pos)) {
    vioplot(full_pred_worm_lspm[start_point:(start_point+n_pos-1),i], add=TRUE,cex=0.2, rectCol = NA, lineCol = NA,
            colMed = NA, border=NA, col=1, at=1, horizontal = TRUE)
    vioplot(full_pred_worm_lpm[start_point:(start_point+n_pos-1),i], add=TRUE,cex=0.2, rectCol = NA, lineCol = NA,
            colMed = NA, border=NA, col=4, at=2, horizontal = TRUE)
  }
  # vioplot(full_pred_worm_lpm[1:(1+n_pos-1),i], add=TRUE,cex=0.2, rectCol = NA, lineCol = NA,
  #         colMed = NA, border=NA, col=rgb(red = 1, green=0, blue=.5,alpha=1), at=2)

  if(i==1){
    # title(xlab="(b)", line=2.9)
  }
  if(i==2){
    # title(xlab="(c)", line=2.9)
  }
  if(i==1){
    abline(v=gden(wormadj), cex=2, col="red")
    # title(xlab="(d)", line=2.9)
  }
  if(i==2){
    abline(v=gtrans(wormadj), cex=2, col="red")
    # title(xlab="(e)", line=2.9)
  }
  if(i==5){
    # title(xlab="(f)", line=2.9)
  }
}
# mtext("Worm Connectome Network", side = 3, line = -2.5, outer = TRUE)

# Close the pdf file
dev.off()

palette(c("black"  , "red"  ,   "green3"  ,"blue"   , "cyan"  ,  "magenta" ,"yellow"  ,"gray"  )) # default palette

