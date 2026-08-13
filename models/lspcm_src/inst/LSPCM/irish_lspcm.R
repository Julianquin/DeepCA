#
# # ie data -----------------------------------------------------------------
#
#
# politicsie.follows <- read.csv("~/lspm/inst/LSPCM/politicsie-follows.mtx", sep="")
# ie_adj <- as.matrix.network.adjacency(as.network(politicsie.follows))
#
# ie_group <- rep(0,348)
# ie_group <- ie_group + colnames(ie_adj) %in% c(114007914,14409739,148702848,163466066,172749267,195697781,19594736,197897890,211231464,213229979,21927925,22186312,22378020,229650437,243632271,244154060,24600451,246820781,248901045,249719099,28364382,28452279,29110219,296787019,326441880,32922034,330904433,342888740,36315404,36351009,399032673,420627114,421067231,422817442,430714461,432054563,504372104,516729176,524423189,54605067,562111567,579303202,627403720,66811109,67268150,81844092,944693640,94835912,99560321)
# ie_group <- ie_group + (colnames(ie_adj) %in% c(104518517,104578228,106866759,109986944,110670932,110711910,111338251,118999126,121098050,122713304,127331013,135514272,135877820,137363410,140393380,144131076,153563285,163929486,166102184,168669206,179230072,18016160,181303066,18304985,190810251,191865710,192341330,19530527,197095842,19735329,198799377,198846028,19964133,20511634,20586381,21117425,212284694,21401635,21440665,21518722,21565240,21643123,21811630,223950593,22498248,226631830,226898857,227338733,227431311,227990753,228673882,228769095,228770219,228774722,229015331,22924705,229466877,22983594,231816917,232233165,232242473,232243716,23359147,233919401,234077156,234688647,234825661,235243434,235965948,236912210,237376611,237788848,237794265,238316092,238586884,239555645,239787466,240465601,240624439,242365407,242373942,242455623,242471796,242484816,242791873,242795908,242804165,243134164,243149702,243177419,244156314,244745254,245434042,247041300,25298765,258136924,29163101,29466305,303259546,30443497,312625958,316306021,318475037,322549346,324588341,327886319,32845471,347778813,36034936,372827446,378668608,382027035,392146152,393382369,393461982,39472243,410258761,415438778,424999238,456156740,48380272,533004742,55174364,568256634,577034468,60663002,6244782,6300152,63680662,67430174,69568655,70205816,83344114,85370909,87471023,88647325,90364806,92521829,93026497,94069743,94303891,95884790,98420900
# )*2)
# ie_group <- ie_group + (colnames(ie_adj) %in% c(104217808,16627544,19652551,21315916,21480666,22005625,246585659
# )*3)
# ie_group <- ie_group + (colnames(ie_adj) %in% c(103817716,116547221,121375039,128593925,150981180,166888604,18818981,190987689,196551540,19777265,20517600,22684776,23577405,237826875,239444226,248801998,249347954,253184868,256198747,262199169,26228880,262322667,28776227,293536395,346630615,360540613,378219674,46850377,54172831,6306972,95658167
# )*4)
# ie_group <- ie_group + (colnames(ie_adj) %in% c(104187193,104192882,10814042,11587032,115924018,116422140,116559835,116785133,120117254,133219169,135274322,144302254,144942878,150256527,163063814,169088826,16950676,17691386,18449501,18539902,191085626,19533838,19837405,202678255,203484133,20584338,21245586,215411682,21629133,22026991,226985646,22862620,231942475,233555616,236108605,236487406,23781227,238158030,239862568,244026999,246723037,246966794,25333950,270286972,304321790,312658372,322638688,33507131,350484600,37011115,371923186,37536209,39019643,39325505,412181399,45611369,46123918,58040765,624645567,6290752,6294502,6751502,70170285,702562776,74979783,75995713,76951150,77184360,77986665,77989626,77995220,78019662,78021401,78026748,80267704,84546428,89952215,90059455,95224854
# )*5)
# ie_group <- ie_group + (colnames(ie_adj) %in% c(124849627,127424923,148872468,157410826,212555476,219007038,219297099,219454029,224291356,22628924,229431792,238633165,241298933,246357421,26586771,27885779,375036621,386170620,396060099,40747039,430934081,48667895,496338650,523319035,616139585,66682393,68690605,822980144,82645739,878580698,917581650
# )*6)
# ie_group <- ie_group + (colnames(ie_adj) %in% c(219480393,236377912,237487471,25310399,270850017,361335767,36806703,385326089
# )*7)
#
# # FF, FG, Green, Ind, Lab, SF, ULA
#
# # Visualise the network with Fruchterman-Reingold
# plot(as.network(ie_adj), vertex.col=ie_group, edge.col="gray")
#
# # # Saving the network and labels
# # save(ie_adj, file="ie_adj.Rdata")
# # save(ie_group, file="ie_group.Rdata")
#
#
# # LSPCM results -----------------------------------------------------------
#
# # setting seed number for run samples
# set.seed(1234)
# seed_number <- sample(1:1e6, 10)
#
# # LSPCM
# ie_lspcm_result_full <- list()
# for(seed in seed_number[9:10]) {
#   lspcm_single_result <- list(LSPCM_asd(ie_adj,
#                                     n_dimen=4, G=10,adapt_param = c(4, 1e-4),
#                                     iter=20e5, burnin=1e5, thin=4000,dim_threshold = c(0.8,0.9,5),
#                                     step_size = c(3.3,2.5), mix_prior = 0.05, dim2_prior = c(3,1)))
#   names(lspcm_single_result) <- paste0("seed",(seed))
#   ie_lspcm_result_full <-  append(ie_lspcm_result_full, lspcm_single_result)
# }
# class(ie_lspcm_result_full) <- "LSPCM"
# # save(ie_lspcm_result_full, file="ie_lspcm_result_full.Rdata")

load("ie_group.Rdata")
load("ie_adj.Rdata")
load("ie_lspcm_result_full.Rdata")

# setting seed number for run samples
set.seed(1234)
seed_number <- sample(1:1e6, 10)
library(mcclust)
library(mcclust.ext)
library(mclust)
library(network)
library(vegan)

# Cluster mode
rbind(calculate_mode(do.call(rbind, lapply(ie_lspcm_result_full, function(x) x$iter_G)))
)

# Cluster 95% credible interval
rbind(
  quantile(do.call(rbind, lapply(ie_lspcm_result_full, function(x) x$iter_G)), c(0.025,0.975))
)

# label switching ---------------------------------------------------------
ie_relab_full <- c()
ie_rand <- c()
for(seed in seed_number[1:10]) {
  k <- apply(ie_lspcm_result_full[[paste0("seed",(seed))]]$cluster,1, function(cl) length(table(cl))) # number of active cluster across iteration
  max.k <- as.numeric(names(table(k))[which.max(table(k))]) # posterior modal cluster
  ie_relab <- relabel(ie_lspcm_result_full[[paste0("seed",(seed))]]$cluster[k==max.k,]) # relabel condition on postterior modal cluster
  ie_relab_full <- append(ie_relab_full,list(ie_relab))
  ie_rand <- c(ie_rand, adjustedRandIndex(ie_relab$cl, ie_group)) # saving data

  # plot(as.network(ie_adj), vertex.col=ie_relab$cl, edge.col="gray")
}
names(ie_relab_full) <- paste0("seed",(seed_number))

# Mean adjusted rand index
mean(ie_rand)

# 95% adjusted rand index
quantile(ie_rand, c(0.025,0.975))

cat(paste0(round(mean(ie_rand),2), " (",
    round(quantile(ie_rand, 0.025), 2), ", ",
    round(quantile(ie_rand, 0.975), 2), ")" ))

# alternate label switching -----------------------------------------------
ie_relab_mbind_full <- c()
ie_rand_mbind <- c()
for(seed in seed_number[1:10]) {
  ie_relab_mbind <- minbinder.ext(comp.psm(ie_relab_full[[paste0("seed",(seed))]]$cls),
                              ie_relab_full[[paste0("seed",(seed))]]$cls,
                              method = "all", max.k = calculate_mode(ie_lspcm_result_full[[paste0("seed",(seed))]]$iter_G)) # relabel via mbind
  ie_relab_mbind_full <- append(ie_relab_mbind_full,list(ie_relab_mbind))
  ie_rand_mbind <- c(ie_rand_mbind, adjustedRandIndex(ie_relab_mbind$cl[1,], ie_group)) # saving data
  # plot(as.network(ie_adj), vertex.col=ie_rand_mbind$cl, edge.col="gray")
}
names(ie_relab_mbind_full) <- paste0("seed",(seed_number))

# Mean adjusted rand index for mbind
mean(ie_rand_mbind)

# 95% adjusted rand index for mbind
quantile(ie_rand_mbind, c(0.025,0.975))

cat(paste0(round(mean(ie_rand_mbind),2), " (",
           round(quantile(ie_rand_mbind, 0.025), 2), ", ",
           round(quantile(ie_rand_mbind, 0.975), 2), ")" ))

ie_relab_mpear_full <- c()
ie_rand_mpear <- c()
for(seed in seed_number[1:10]) {
  ie_relab_mpear <- maxpear(comp.psm(ie_relab_full[[paste0("seed",(seed))]]$cls),
                            ie_relab_full[[paste0("seed",(seed))]]$cls,
                            method = "all", max.k = calculate_mode(ie_lspcm_result_full[[paste0("seed",(seed))]]$iter_G)) # relabel via mbind
  ie_relab_mpear_full <- append(ie_relab_mpear_full,list(ie_relab_mpear))
  ie_rand_mpear <- c(ie_rand_mpear, adjustedRandIndex(ie_relab_mpear$cl[1,], ie_group)) # saving data
  # plot(as.network(ie_adj), vertex.col=ie_relab_mpear$cl, edge.col="gray")
}
names(ie_relab_mpear_full) <- paste0("seed",(seed_number))

# Mean adjusted rand index for mpear
mean(ie_rand_mpear)

# 95% adjusted rand index for mpear
cat(quantile(ie_rand_mpear, c(0.025,0.975)))

cat(paste0(round(mean(ie_rand_mpear),2), " (",
           round(quantile(ie_rand_mpear, 0.025), 2), ", ",
           round(quantile(ie_rand_mpear, 0.975), 2), ")" ))

ie_rand_VI <- c()
for(seed in seed_number[1:10]) {
  ie_relab_VI <- minVI(comp.psm(ie_relab_full[[paste0("seed",(seed))]]$cls),
                       ie_relab_full[[paste0("seed",(seed))]]$cls,
                       method=("all"),include.greedy=TRUE, max.k = calculate_mode(ie_lspcm_result_full[[paste0("seed",(seed))]]$iter_G)) # relabel via VI
  ie_rand_VI <- c(ie_rand_VI, adjustedRandIndex(ie_relab_VI$cl[1,], ie_group)) # saving data
  # plot(as.network(ie_adj), vertex.col=ie_relab_mpear$cl, edge.col="gray")
}

# Mean adjusted rand index for VI
mean(ie_rand_VI)

# 95% adjusted rand index for VI
quantile(ie_rand_VI, c(0.025,0.975))

cat(paste0(round(mean(ie_rand_VI),2), " (",
           round(quantile(ie_rand_VI, 0.025), 2), ", ",
           round(quantile(ie_rand_VI, 0.975), 2), ")" ))

# Cluster plot ----------------------------------------------------------

# open the pdf file
cairo_pdf("~/lspm/inst/extdata/lspcm/figure/ie_cluster_bar.pdf", width = 5, height = 3)

par(mfrow=c(1,1))
par(mar = c(4.1, 4.5, 1, 0.1))

barplot(table(do.call(rbind, lapply(ie_lspcm_result_full, function(x) x$iter_G)))[2:11]/ length(do.call(rbind, lapply(ie_lspcm_result_full, function(x) x$iter_G)))
        , ylim=c(0,1), xlab = expression(G), ylab="Posterior proportion", cex.lab=1.5, cex.axis=1.5, cex.names=1.5)

dev.off()


# Dimension plot ----------------------------------------------------------

# open the pdf file
cairo_pdf("~/lspm/inst/extdata/lspcm/figure/ie_dim_bar.pdf", width = 5, height = 3)

par(mfrow=c(1,1))
par(mar = c(4.1, 4.5, 1, 0.5))

barplot(table(do.call(rbind, lapply(ie_lspcm_result_full, function(x) x$iter_d)))/ length(do.call(rbind, lapply(ie_lspcm_result_full, function(x) x$iter_d)))
        , ylim=c(0,1), xlab = expression(p), ylab="Posterior proportion", cex.lab=1.5, cex.axis=1.5, cex.names=1.5)

dev.off()


# Dimension mode
rbind(calculate_mode(do.call(rbind, lapply(ie_lspcm_result_full, function(x) x$iter_d)))
)

# Dimension 95% credible interval
rbind(
  quantile(do.call(rbind, lapply(ie_lspcm_result_full, function(x) x$iter_d)), c(0.025,0.975))
)





k = apply(do.call(rbind,lapply(ie_lspcm_result_full, function(x) x$cluster)), 1, function(cl) length(table(cl)))
max.k <- as.numeric(names(table(k))[which.max(table(k))])
ie_relab_combined <- relabel(do.call(rbind,lapply(ie_lspcm_result_full, function(x) x$cluster))[k==max.k,])
ie_mbind_combined <- minbinder.ext(comp.psm(do.call(rbind,lapply(ie_relab_full, function(x) x$cls)) ),
                             do.call(rbind,lapply(ie_relab_full, function(x) x$cls)) ,
                             method="all", max.k=calculate_mode(do.call(c,lapply(ie_lspcm_result_full, function(x) x$iter_G))))

cairo_pdf("~/lspm/inst/extdata/lspcm/figure/ie_true.pdf", width = 2.5, height = 3)
par(mfrow=c(1,1))
par(mar = c(0.1, 0.1, 0.1, 0.1))
custom_palette <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
palette(custom_palette)
# par(mfrow=c(1,2))
set.seed(111)
plot(as.network(ie_adj), vertex.col = ie_group, edge.col="gray", vertex.cex=1.5, vertex.border="black") # true cluster
legend("topright", legend=c('FG', "FF", 'Green', "Ind", "Lab", "SF", "ULA"), pch=20, col=1:7, cex=0.6)

# legend("topright", legend=c("Fianna Fáil", "Fine Gael", "Green Party", "Independents", "Labour", "Sinn Féin", "United Left"), pch=20, col=1:7, cex=0.5)
dev.off()

plot(as.network(ie_adj), vertex.col = ie_group, edge.col="gray", vertex.cex=1.5, vertex.side=ie_group+2) # true cluster
legend("topright", legend=c('FG', "FF", 'Green', "Ind", "Lab", "SF", "ULA"), col=1:7, cex=1.3,pch="\u25BA")
# c("Fianna Fáil", "Fine Gael", "Green Party", "Independents", "Labour", "Sinn Féin", "United Left")

cairo_pdf("~/lspm/inst/extdata/lspcm/figure/ie_lspcm.pdf", width = 2.5, height = 3)
par(mfrow=c(1,1))
par(mar = c(0.1, 0.1, 0.1, 0.1))
custom_palette <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
palette(custom_palette)
set.seed(111)
plot(as.network(ie_adj), vertex.col = ie_mbind_combined$cl[1,], edge.col="gray", vertex.cex=1.5, vertex.border="black") # lspcm combined cluster
# legend("topleft", legend=c(paste("Cluster", 1:16)), pch=20, col=1:16, cex=0.6)
# legend("topleft", legend=c(paste("Cluster", 1:5), "Independents", "Fianna Fáil", "Independents", "Independents", "Fine Gael", "Independents", "Fine Gael", "Green Party", "Independents", "Fine Gael", "Independents"), pch=20, col=1:16, cex=0.6)

# plot(as.network(ie_adj), vertex.col = ie_mbind_combined_5g, edge.col="gray", vertex.cex=1.5) # lspcm combined cluster
legend("topleft", legend=c("Cluster 1", "Cluster 2", "Cluster 3", "Cluster 4", "Cluster 5"), pch=20, col=1:5, cex=0.7)

# legend("topleft", legend=c("Cluster 1", "Cluster 2", "Cluster 3", "Cluster 4", "Cluster 5"), pch=20, col=c(1,2,4,5,7), cex=0.7)
dev.off()
par(mfrow=c(1,1))


cairo_pdf("~/lspm/inst/extdata/lspcm/figure/ie_lspcm_pos.pdf", width = 2.5, height = 3)
par(mfrow=c(1,1))
par(mar = c(4, 4.2, .8, 0.5))
plot(apply(do.call(abind, lapply(ie_lspcm_result_full, function(x) x$positions[,1:4,])), c(1,2), mean, na.rm=T),
     col=ie_mbind_combined$cl[1,], pch=20, cex=1.3 ,
     xlim=c(-1.8,1), ylim=c(-1.5,2), xlab="Dimension 1", ylab="Dimension 2")
legend("topleft", legend=c("Cluster 1", "Cluster 2", "Cluster 3", "Cluster 4", "Cluster 5"), pch=20, col=1:5, cex=0.6)

dev.off()


# open the pdf file
# cairo_pdf("~/lspm/inst/extdata/lspcm/figure/ie_lspcm_3d.pdf", width = 10, height = 3)
cairo_pdf("~/lspm/inst/extdata/lspcm/figure/ie_lspcm_2d.pdf", width = 5, height = 6)


d=3
ie_lspcm_3d_mean_pos <- apply(do.call(abind, lapply(ie_lspcm_result_full, function(x) x$positions[,1:4,x$iter_d == calculate_mode(x$iter_d)])), c(1,2), mean, na.rm=T)

par(mfrow=c(1,1))
par(mar = c(5.1, 5.1, 2.1, 2.1))
custom_palette <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
palette(custom_palette)
plot(ie_lspcm_3d_mean_pos[,1:2], #ylim=c(-1.5,1.7), xlim=c(-1.5,1.7),
     col=ie_mbind_combined$cl[1,], pch=20, cex=2, cex.lab=1.8, cex.axis=1.5,
     ylim = c(-1.5,1.5), xlim=c(-1.7,2.2), type="n",
     xlab="Dimension 1", ylab="Dimension 2", main="")
segments(ie_lspcm_3d_mean_pos[row(ie_adj)[which(ie_adj != 0)],1],
         ie_lspcm_3d_mean_pos[row(ie_adj)[which(ie_adj != 0)],2],
         ie_lspcm_3d_mean_pos[col(ie_adj)[which(ie_adj != 0)],1],
         ie_lspcm_3d_mean_pos[col(ie_adj)[which(ie_adj != 0)],2],
         lwd = 1, col = adjustcolor("grey50", 0.2))
points(ie_lspcm_3d_mean_pos[,c(1,2)], #ylim=c(-1.5,1.7), xlim=c(-1.5,1.7),
       col=ie_mbind_combined$cl[1,], pch=20, cex=2, cex.lab=2.5, cex.axis=2.)

# plot(ie_lspcm_3d_mean_pos[,c(1,3)], #ylim=c(-1.5,1.7), xlim=c(-1.5,1.7),
#      col=ie_mbind_combined$cl[1,], pch=20, cex=2, cex.lab=1.8, cex.axis=1.5,
#      ylim = c(-1.5,1.7), xlim=c(-1.9,1.3), type="n",
#      xlab="Dimension 1", ylab="Dimension 3", main="")
# segments(ie_lspcm_3d_mean_pos[row(ie_adj)[which(ie_adj != 0)],1],
#          ie_lspcm_3d_mean_pos[row(ie_adj)[which(ie_adj != 0)],3],
#          ie_lspcm_3d_mean_pos[col(ie_adj)[which(ie_adj != 0)],1],
#          ie_lspcm_3d_mean_pos[col(ie_adj)[which(ie_adj != 0)],3],
#          lwd = 1, col = adjustcolor("grey50", 0.2))
# points(ie_lspcm_3d_mean_pos[,c(1,3)], #ylim=c(-1.5,1.7), xlim=c(-1.5,1.7),
#        col=ie_mbind_combined$cl[1,], pch=20, cex=2, cex.lab=2.5, cex.axis=2.)
#
#
# # legend("topright", legend=c('Visuals', "Auditory", 'Somatomotor', "Frontolimbic"),
# #        pch=14+1:4, col=3:6, cex=1.2)
#
# plot(ie_lspcm_3d_mean_pos[,2:3], #ylim=c(-1.5,1.7), xlim=c(-1.5,1.7),
#      col=ie_mbind_combined$cl[1,], pch=20, cex=2, cex.lab=1.8, cex.axis=1.5,
#      ylim = c(-1.5,1.7), xlim=c(-1.9,1.3), type="n",
#      xlab="Dimension 2", ylab="Dimension 3", main="")
# segments(ie_lspcm_3d_mean_pos[row(ie_adj)[which(ie_adj != 0)],2],
#          ie_lspcm_3d_mean_pos[row(ie_adj)[which(ie_adj != 0)],3],
#          ie_lspcm_3d_mean_pos[col(ie_adj)[which(ie_adj != 0)],2],
#          ie_lspcm_3d_mean_pos[col(ie_adj)[which(ie_adj != 0)],3],
#          lwd = 1, col = adjustcolor("grey50", 0.2))
# points(ie_lspcm_3d_mean_pos[,c(2,3)], #ylim=c(-1.5,1.7), xlim=c(-1.5,1.7),
#        col=ie_mbind_combined$cl[1,], pch=20, cex=2, cex.lab=2.5, cex.axis=2.)

legend_order <- matrix(1:6,ncol=3,byrow = TRUE)
legend("topright",paste('Cluster',1:5),
       pch=c(rep(20,5)),
       col=c(1:5),
      cex=1.2)
dev.off()
# c('FG', "FF", 'Green', "Ind", "Lab", "SF", "ULA")

# LPCM --------------------------------------------------------------------

ie_lpcm_1d3g = ergmm(ie_adj ~ euclidean(d=1, G=3), seed = 111)
ie_lpcm_2d3g = ergmm(ie_adj ~ euclidean(d=2, G=3), seed = 111)
ie_lpcm_3d3g = ergmm(ie_adj ~ euclidean(d=3, G=3), seed = 111)

summary(ie_lpcm_1d3g)$bic$overall
summary(ie_lpcm_2d3g)$bic$overall
summary(ie_lpcm_3d3g)$bic$overall

ie_lpcm_2d5g = ergmm(ie_adj ~ euclidean(d=2, G=5), seed = 111)
ie_lpcm_2d4g = ergmm(ie_adj ~ euclidean(d=2, G=4), seed = 111)
ie_lpcm_2d2g = ergmm(ie_adj ~ euclidean(d=2, G=2), seed = 111)

summary(ie_lpcm_2d5g)$bic$overall
summary(ie_lpcm_2d4g)$bic$overall
summary(ie_lpcm_2d3g)$bic$overall
summary(ie_lpcm_2d2g)$bic$overall


# Procrustes correlation
ie_protest <- c()
for(seed in seed_number){
  ie_pos_mean = apply(ie_lspcm_result_full[[paste0("seed",(seed))]]$positions[,1:2,], c(1,2), mean, na.rm=T)
  ie_protest_single <- protest(ie_pos_mean,
                                     ie_lpcm_2d3g$mcmc.pmode$Z)$scale
  ie_protest <- c(ie_protest, ie_protest_single)
}

mean(ie_protest)
quantile(ie_protest, c(0.025, 0.975))
sd(ie_protest)



# violin plots ------------------------------------------------------------

# open the pdf file
cairo_pdf("~/lspm/inst/extdata/lspcm/figure/ie_pmd.pdf", width = 2.5, height = 3)
par(mfrow=c(1,1))
par(mar = c(3.3, 3.7, 1, 0.5))
plot(ie_lspcm_result_full, parameter="deltas", n_dimen=2)
plot(ie_lspcm_result_full, parameter="deltas", n_dimen=3, add=T)
plot(ie_lspcm_result_full, parameter="deltas", n_dimen=4, add=T)
# legend("topleft", legend=c('Posterior distribution', "Average posterior mean","True value"), pch=c(20,20,4), col=c("black","green3","red"), cex=0.75)
dev.off()

cairo_pdf("~/lspm/inst/extdata/lspcm/figure/ie_pmv.pdf", width = 2.5, height = 3)
par(mfrow=c(1,1))
par(mar = c(3.3, 3.7, 1, 0.5))
plot(ie_lspcm_result_full, parameter="variances", n_dimen=2)
plot(ie_lspcm_result_full, parameter="variances", n_dimen=3, add=T)
plot(ie_lspcm_result_full, parameter="variances", n_dimen=4, add=T)
legend("topleft", legend=c('Posterior distribution'), pch=c(20), col=c("black"), cex=0.75)
dev.off()

cairo_pdf("~/lspm/inst/extdata/lspcm/figure/ie_alpha.pdf", width = 2.5, height = 3)
par(mfrow=c(1,1))
par(mar = c(3.3, 3.7, 1, 0.5))
plot(ie_lspcm_result_full, parameter="alpha", n_dimen=2, ylim=c(-2,2))
plot(ie_lspcm_result_full, parameter="alpha", n_dimen=3, add=T)
plot(ie_lspcm_result_full, parameter="alpha", n_dimen=4, add=T)
# legend("topleft", legend=c('Posterior distribution', "Average posterior mean","True value"), pch=c(20,20,4), col=c("black","green3","red"), cex=0.75)
dev.off()



# 005 ---------------------------------------------------------------------

# LSPCM
ie_lspcm_result_full_005 <- list()
for(seed in seed_number[10]) {
  lspcm_single_result <- list(LSPCM_asd(ie_adj,
                                        n_dimen=4, G=10,adapt_param = c(4, 1e-4),
                                        iter=20e5, burnin=1e5, thin=4000,dim_threshold = c(0.8,0.9,5),
                                        step_size = c(3.3,2.5), mix_prior = 0.005, dim2_prior = c(3,1)))
  names(lspcm_single_result) <- paste0("seed",(seed))
  ie_lspcm_result_full_005 <-  append(ie_lspcm_result_full_005, lspcm_single_result)
}
class(ie_lspcm_result_full_005) <- "LSPCM"
# save(ie_lspcm_result_full, file="ie_lspcm_result_full.Rdata")


#####
colnames(ie_adj)[ie_mbind_combined$cl[1,]==5]
# [1] "181303066" 272 Hildegarde Naughton 1,9 "378219674" 338 Dana Rosemary Scallon 6,5

# Ind that won
# 103817716,11[6]547221,12135039,128593925,190987689,237826875,239444226,248801998,249347954,25318414768,346630615,5472831
# 4 6 1 1
# ind that didnt won 10  4  2  2  1
