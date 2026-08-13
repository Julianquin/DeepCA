load(file = "~/lspm/inst/extdata/binary/node50results3d5_0_thin.Rdata")
load(file = "~/lspm/inst/extdata/binary/node50results3d5_1_thin.Rdata")
load(file = "~/lspm/inst/extdata/binary/node50results3d5_5_thin.Rdata")
load(file = "~/lspm/inst/extdata/binary/node50results3d5_10_thin.Rdata")
load(file = "~/lspm/inst/extdata/binary/node50results3d5_20_thin.Rdata")
load(file = "~/lspm/inst/extdata/binary/node50results3d5_30_thin.Rdata")

source("~/lspm/inst/section4_3network.R")

# # Fitting LPM 4D
# node100results4d4net <- list()
# for(seed in seed_number) {
#
#   lspm_single_result <- list(ergmm(node100network[[paste0("seed",(seed))]]$network ~ euclidean(d=4), seed=115))
#   names(lspm_single_result) <- paste0("seed",(seed))
#   node100results4d4net <-  append(node100results4d4net, lspm_single_result)
# }

# save(node100results4d4net, file = "node100results4d4net.Rdata")


# Posterior Predictive Checking
n_pos=30
node50results3d5_0 <- node50results3d5_1 <- node50results3d5_5 <- node50results3d5_10 <- node50results3d5_20 <- node50results3d5_30 <- full_true0 <- full_true1 <- full_true5 <- full_true10 <- full_true20 <- full_true30 <- c()
for(seed in seed_number) {
  # Predictives check of LSPM results
  pred0 <- as.data.frame(predcheck(n_pos, node50results3d5_0_thin[[paste0("seed",(seed))]]$alpha, node50results3d5_0_thin[[paste0("seed",(seed))]]$positions, node50results3d5_0_thin[[paste0("seed",(seed))]]$initialisation$network, seed = 1234))
  pred1 <- as.data.frame(predcheck(n_pos, node50results3d5_1_thin[[paste0("seed",(seed))]]$alpha, node50results3d5_1_thin[[paste0("seed",(seed))]]$positions, node50results3d5_1_thin[[paste0("seed",(seed))]]$initialisation$network, seed = 1234))
  pred5 <- as.data.frame(predcheck(n_pos, node50results3d5_5_thin[[paste0("seed",(seed))]]$alpha, node50results3d5_5_thin[[paste0("seed",(seed))]]$positions, node50results3d5_5_thin[[paste0("seed",(seed))]]$initialisation$network, seed = 1234))
  pred10 <- as.data.frame(predcheck(n_pos, node50results3d5_10_thin[[paste0("seed",(seed))]]$alpha, node50results3d5_10_thin[[paste0("seed",(seed))]]$positions, node50results3d5_10_thin[[paste0("seed",(seed))]]$initialisation$network, seed = 1234))
  pred20 <- as.data.frame(predcheck(n_pos, node50results3d5_20_thin[[paste0("seed",(seed))]]$alpha, node50results3d5_20_thin[[paste0("seed",(seed))]]$positions, node50results3d5_20_thin[[paste0("seed",(seed))]]$initialisation$network, seed = 1234))
  pred30 <- as.data.frame(predcheck(n_pos, node50results3d5_30_thin[[paste0("seed",(seed))]]$alpha, node50results3d5_30_thin[[paste0("seed",(seed))]]$positions, node50results3d5_30_thin[[paste0("seed",(seed))]]$initialisation$network, seed = 1234))

  node50results3d5_0 <- rbind(node50results3d5_0, pred0)
  node50results3d5_1 <- rbind(node50results3d5_1, pred1)
  node50results3d5_5 <- rbind(node50results3d5_5, pred5)
  node50results3d5_10 <- rbind(node50results3d5_10, pred10)
  node50results3d5_20 <- rbind(node50results3d5_20, pred20)
  node50results3d5_30 <- rbind(node50results3d5_30, pred30)


  # Predictives check using true parameter
  true0 <- as.data.frame(predcheck(n_pos, network = node50network3d5_0[[paste0("seed",(seed))]]$network,networkMGP = node50network3d5_0[[paste0("seed",(seed))]], seed = 1234))
  full_true0 <- rbind(full_true0, true0)

  true1 <- as.data.frame(predcheck(n_pos, network = node50network3d5_1[[paste0("seed",(seed))]]$network,networkMGP = node50network3d5_1[[paste0("seed",(seed))]], seed = 1234))
  full_true1 <- rbind(full_true1, true1)

  true5 <- as.data.frame(predcheck(n_pos, network = node50network3d5_5[[paste0("seed",(seed))]]$network,networkMGP = node50network3d5_5[[paste0("seed",(seed))]], seed = 1234))
  full_true5 <- rbind(full_true5, true5)

  true10 <- as.data.frame(predcheck(n_pos, network = node50network3d5_10[[paste0("seed",(seed))]]$network,networkMGP = node50network3d5_10[[paste0("seed",(seed))]], seed = 1234))
  full_true10 <- rbind(full_true10, true10)

  true20 <- as.data.frame(predcheck(n_pos, network = node50network3d5_20[[paste0("seed",(seed))]]$network,networkMGP = node50network3d5_20[[paste0("seed",(seed))]], seed = 1234))
  full_true20 <- rbind(full_true20, true20)

  true30 <- as.data.frame(predcheck(n_pos, network = node50network3d5_30[[paste0("seed",(seed))]]$network,networkMGP = node50network3d5_30[[paste0("seed",(seed))]], seed = 1234))
  full_true30 <- rbind(full_true30, true30)
}

# open the pdf file
cairo_pdf("~/lspm/inst/figures/pred4_3.pdf", width = 10, height = 4)

# Predictive Check Plots
old.par <- par(no.readonly = TRUE) # save current layout setting

mycol=adjustcolor(palette(), alpha.f = 0.1)
palette(mycol)
plot_names <- c("Network Density", "Transitivity", "Accuracy", "F1 Score", "Hamming Distance")
for(i in 1:5){
  if(i==1) {
    par(mfrow=c(1,5))
    layout.matrix <- matrix(1:5, nrow = 1, ncol = 5)
    layout(mat = layout.matrix, widths = c(2.9,2,2,2,2.1))
    par(mar = c(5.6, 7, 4.1, 0.1))
  } else if(i==2){
    par(mar = c(5.6, 0, 4.1, 0.05))
  } else if(i == 5 ){
    par(mar = c(5.6, 0, 4.1, 1)) # right margin set as 0.1
  }
  vioplot(cbind(node50results3d5_0[,i], node50results3d5_1[,i], node50results3d5_5[,i],node50results3d5_10[,i],node50results3d5_20[,i],node50results3d5_30[,i], full_true0[,i], full_true1[,i], full_true5[,i], full_true10[,i], full_true20[,i], full_true30[,i]), cex=0.2, rectCol = NA, lineCol = NA,
          xaxt="n", cex.names=1.4, horizontal = TRUE,
          # ylim=c(0,1),
          ylim=c(min((cbind(node50results3d5_0[,i], node50results3d5_1[,i], node50results3d5_5[,i],node50results3d5_10[,i],node50results3d5_20[,i],node50results3d5_30[,i], full_true0[,i], full_true1[,i], full_true5[,i], full_true10[,i], full_true20[,i], full_true30[,i]))-0.02),(max(cbind(node50results3d5_0[,i], node50results3d5_1[,i], node50results3d5_5[,i],node50results3d5_10[,i],node50results3d5_20[,i],node50results3d5_30[,i], full_true0[,i], full_true1[,i], full_true5[,i], full_true10[,i], full_true20[,i], full_true30[,i]))+0.02)),
          colMed = NA, border=NA, col=rgb(red = 1, green=0.8, blue=1,alpha=0))
  if(i == 1) {
    axis(2,1:12, las=2,labels=c(expression(paste("LSPM(", alpha, "=30)")),
                                expression(paste("LSPM(", alpha, "=20)")),
                                expression(paste("LSPM(", alpha, "=10)")),
                                expression(paste("LSPM(", alpha, "=5)")),
                                expression(paste("LSPM(", alpha, "=1)")),
                                expression(paste("LSPM(", alpha, "=0)")),
                                expression(paste("d.g.(", alpha, "=30)")),
                                expression(paste("d.g.(", alpha, "=20)")),
                                expression(paste("d.g.(", alpha, "=10)")),
                                expression(paste("d.g.(", alpha, "=5)")),
                                expression(paste("d.g.(", alpha, "=1)")),
                                expression(paste("d.g.(", alpha, "=0)"))), cex.axis=1.2)
  }
  title(xlab=plot_names[i], line=2.75, cex.lab=1.6)

  for(start_point in seq(1,n_pos*30, n_pos)) {
    vioplot(node50results3d5_0[start_point:(start_point+n_pos-1),i], add=TRUE,cex=0.2, rectCol = NA, lineCol = NA, cex.lab=1.5,
            colMed = NA, border=NA, col=1, at=6, horizontal = TRUE)
    vioplot(node50results3d5_1[start_point:(start_point+n_pos-1),i], add=TRUE,cex=0.2, rectCol = NA, lineCol = NA, cex.lab=1.5,
            colMed = NA, border=NA, col=1, at=5, horizontal = TRUE)
    vioplot(node50results3d5_5[start_point:(start_point+n_pos-1),i], add=TRUE,cex=0.2, rectCol = NA, lineCol = NA, cex.lab=1.5,
            colMed = NA, border=NA, col=1, at=4, horizontal = TRUE)
    vioplot(node50results3d5_10[start_point:(start_point+n_pos-1),i], add=TRUE,cex=0.2, rectCol = NA, lineCol = NA, cex.lab=1.5,
            colMed = NA, border=NA, col=1, at=3, horizontal = TRUE)
    vioplot(node50results3d5_20[start_point:(start_point+n_pos-1),i], add=TRUE,cex=0.2, rectCol = NA, lineCol = NA, cex.lab=1.5,
            colMed = NA, border=NA, col=1, at=2, horizontal = TRUE)
    vioplot(node50results3d5_30[start_point:(start_point+n_pos-1),i], add=TRUE,cex=0.2, rectCol = NA, lineCol = NA, cex.lab=1.5,
            colMed = NA, border=NA, col=1, at=1, horizontal = TRUE)
    vioplot(full_true0[start_point:(start_point+n_pos-1),i], add=TRUE,cex=0.2, rectCol = NA, lineCol = NA, cex.lab=1.5,
            colMed = NA, border=NA, col=2, at=12, horizontal = TRUE)
    vioplot(full_true1[start_point:(start_point+n_pos-1),i], add=TRUE,cex=0.2, rectCol = NA, lineCol = NA, cex.lab=1.5,
            colMed = NA, border=NA, col=2, at=11, horizontal = TRUE)
    vioplot(full_true5[start_point:(start_point+n_pos-1),i], add=TRUE,cex=0.2, rectCol = NA, lineCol = NA, cex.lab=1.5,
            colMed = NA, border=NA, col=2, at=10, horizontal = TRUE)
    vioplot(full_true10[start_point:(start_point+n_pos-1),i], add=TRUE,cex=0.2, rectCol = NA, lineCol = NA, cex.lab=1.5,
            colMed = NA, border=NA, col=2, at=9, horizontal = TRUE)
    vioplot(full_true20[start_point:(start_point+n_pos-1),i], add=TRUE,cex=0.2, rectCol = NA, lineCol = NA, cex.lab=1.5,
            colMed = NA, border=NA, col=2, at=8, horizontal = TRUE)
    vioplot(full_true30[start_point:(start_point+n_pos-1),i], add=TRUE,cex=0.2, rectCol = NA, lineCol = NA, cex.lab=1.5,
            colMed = NA, border=NA, col=2, at=7, horizontal = TRUE)
  }

  # if(i==3){
  #   boxplot(unlist(lapply(node100network, function(x) gden(x$network))), add=TRUE, at=4.4)
  # }
  # if(i==4){
  #   boxplot(unlist(lapply(node100network, function(x) gtrans(x$network))), add=TRUE, at=4.4)
  # }
}

par(old.par) # restore previous plot layout setting

# palette("R3") # default palette
palette(c("black"  , "red"  ,   "green3"  ,"blue"   , "cyan"  ,  "magenta" ,"yellow"  ,"gray"  )) # default palette

# Close the pdf file
dev.off()
