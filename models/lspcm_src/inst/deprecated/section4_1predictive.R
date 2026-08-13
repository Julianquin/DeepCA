load(file = "~/lspm/inst/extdata/binary/node20results_thin.Rdata")
load(file = "~/lspm/inst/extdata/binary/node50results_thin.Rdata")
load(file = "~/lspm/inst/extdata/binary/node100results_thin.Rdata")
load(file = "~/lspm/inst/extdata/binary/node200results_thin.Rdata")

source("~/lspm/inst/section4_1network.R")

# open the pdf file
cairo_pdf("~/lspm/inst/figures/pred4_1.pdf", width = 10, height = 4)


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
full_pred20 <- full_pred50 <- full_pred100 <- full_pred200 <- full_true20 <- full_true50 <- full_true100 <- full_true200 <- c()
for(seed in seed_number) {
  # Predictives check of LSPM results
  pred20 <- as.data.frame(predcheck(n_pos, node20results_thin[[paste0("seed",(seed))]]$alpha, node20results_thin[[paste0("seed",(seed))]]$positions, node20results_thin[[paste0("seed",(seed))]]$initialisation$network, seed = 1234))
  pred50 <- as.data.frame(predcheck(n_pos, node50results_thin[[paste0("seed",(seed))]]$alpha, node50results_thin[[paste0("seed",(seed))]]$positions, node50results_thin[[paste0("seed",(seed))]]$initialisation$network, seed = 1234))
  pred100 <- as.data.frame(predcheck(n_pos, node100results_thin[[paste0("seed",(seed))]]$alpha, node100results_thin[[paste0("seed",(seed))]]$positions, node100results_thin[[paste0("seed",(seed))]]$initialisation$network, seed = 1234))
  pred200 <- as.data.frame(predcheck(n_pos, node200results_thin[[paste0("seed",(seed))]]$alpha, node200results_thin[[paste0("seed",(seed))]]$positions, node200results_thin[[paste0("seed",(seed))]]$initialisation$network, seed = 1234))


  full_pred20 <- rbind(full_pred20, pred20)
  full_pred50 <- rbind(full_pred50, pred50)
  full_pred100 <- rbind(full_pred100, pred100)
  full_pred200 <- rbind(full_pred200, pred200)


  # Predictives check using true parameter
  true20 <- as.data.frame(predcheck(n_pos, network = node20network[[paste0("seed",(seed))]]$network,networkMGP = node20network[[paste0("seed",(seed))]], seed = 1234))
  full_true20 <- rbind(full_true20, true20)

  true50 <- as.data.frame(predcheck(n_pos, network = node50network[[paste0("seed",(seed))]]$network,networkMGP = node50network[[paste0("seed",(seed))]], seed = 1234))
  full_true50 <- rbind(full_true50, true50)

  true100 <- as.data.frame(predcheck(n_pos, network = node100network[[paste0("seed",(seed))]]$network,networkMGP = node100network[[paste0("seed",(seed))]], seed = 1234))
  full_true100 <- rbind(full_true100, true100)

  true200 <- as.data.frame(predcheck(n_pos, network = node200network[[paste0("seed",(seed))]]$network,networkMGP = node200network[[paste0("seed",(seed))]], seed = 1234))
  full_true200 <- rbind(full_true200, true200)
}


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
    par(mar = c(5.6, 7.3, 4.1, 0.2))
  } else if(i==2){
    par(mar = c(5.6, 0, 4.1, 0.2))
  } else if(i == 5 ){
    par(mar = c(5.6, 0, 4.1, 1)) # right margin set as 0.1
  }
  vioplot(cbind(full_pred20[,i], full_pred50[,i], full_pred100[,i],full_pred200[,i],full_true20[,i], full_true50[,i], full_true100[,i], full_true200[,i]), cex=0.2, rectCol = NA, lineCol = NA,
          xaxt="n", cex.names=1.4, horizontal = TRUE,
          # ylim=c(0,1),
          ylim=c(min((cbind(full_pred20[,i], full_pred50[,i], full_pred100[,i],full_pred200[,i],full_true20[,i], full_true50[,i], full_true100[,i], full_true200[,i]))-0.04),(max(cbind(full_pred20[,i], full_pred50[,i], full_pred100[,i],full_pred200[,i],full_true20[,i], full_true50[,i], full_true100[,i], full_true200[,i]))+0.04)),
          colMed = NA, border=NA, col=rgb(red = 1, green=0.8, blue=1,alpha=0))
  if(i == 1) {
    axis(2,1:8, las=2,labels=c("LSPM(n=200)", "LSPM(n=100)", "LSPM(n=50)","LSPM(n=20)","d.g.(n=200)","d.g.(n=100)","d.g.(n=50)","d.g.(n=20)"), cex.axis=1.2)
  }
  title(xlab=plot_names[i], line=2.75, cex.lab=1.6)

  for(start_point in seq(1,n_pos*30, n_pos)) {
    vioplot(full_pred20[start_point:(start_point+n_pos-1),i], add=TRUE,cex=0.2, rectCol = NA, lineCol = NA, cex.lab=1.5,
            colMed = NA, border=NA, col=1, at=4, horizontal = TRUE)
    vioplot(full_pred50[start_point:(start_point+n_pos-1),i], add=TRUE,cex=0.2, rectCol = NA, lineCol = NA, cex.lab=1.5,
            colMed = NA, border=NA, col=1, at=3, horizontal = TRUE)
    vioplot(full_pred100[start_point:(start_point+n_pos-1),i], add=TRUE,cex=0.2, rectCol = NA, lineCol = NA, cex.lab=1.5,
            colMed = NA, border=NA, col=1, at=2, horizontal = TRUE)
    vioplot(full_pred200[start_point:(start_point+n_pos-1),i], add=TRUE,cex=0.2, rectCol = NA, lineCol = NA, cex.lab=1.5,
            colMed = NA, border=NA, col=1, at=1, horizontal = TRUE)
    vioplot(full_true20[start_point:(start_point+n_pos-1),i], add=TRUE,cex=0.2, rectCol = NA, lineCol = NA, cex.lab=1.5,
            colMed = NA, border=NA, col=2, at=8, horizontal = TRUE)
    vioplot(full_true50[start_point:(start_point+n_pos-1),i], add=TRUE,cex=0.2, rectCol = NA, lineCol = NA, cex.lab=1.5,
            colMed = NA, border=NA, col=2, at=7, horizontal = TRUE)
    vioplot(full_true100[start_point:(start_point+n_pos-1),i], add=TRUE,cex=0.2, rectCol = NA, lineCol = NA, cex.lab=1.5,
            colMed = NA, border=NA, col=2, at=6, horizontal = TRUE)
    vioplot(full_true200[start_point:(start_point+n_pos-1),i], add=TRUE,cex=0.2, rectCol = NA, lineCol = NA, cex.lab=1.5,
            colMed = NA, border=NA, col=2, at=5, horizontal = TRUE)
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
