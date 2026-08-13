library(latentnet)

# trunc binary ------------------------------------------------------------
source("~/lspm/inst/trunc_network.R")

node100results4d3_adapt <- list()
for(seed in seed_number) {
  lspm_single_result <- list(LSPM(node100network4d[[paste0("seed",(seed))]]$network,
                                        iter=1e6, step_size = c(3,1.1),
                                        burnin= 1e5, thin=1500))
  names(lspm_single_result) <- paste0("seed",(seed))
  node100results4d3_adapt <-  append(node100results4d3_adapt, lspm_single_result)
}
class(node100results4d3_adapt) <- "LSPM"
# save(node100results4d3_adapt, file = "node100results4d3_adapt.Rdata")

node100results4d3_adapt_high_d <- list()
for(seed in seed_number) {
  lspm_single_result <- list(LSPM(node100network4d[[paste0("seed",(seed))]]$network,
                                        n_dimen = 10, iter=1e6, step_size = c(3,1.1),
                                        burnin= 1e5, thin=1500))
  names(lspm_single_result) <- paste0("seed",(seed))
  node100results4d3_adapt_high_d <-  append(node100results4d3_adapt_high_d, lspm_single_result)
}
class(node100results4d3_adapt_high_d) <- "LSPM"
# save(node100results4d3_adapt_high_d, file = "node100results4d3_adapt_high_d.Rdata")

node100results4d3_adapt_same_d <- list()
for(seed in seed_number) {
  lspm_single_result <- list(LSPM(node100network4d[[paste0("seed",(seed))]]$network,
                                        n_dimen = 4, iter=1e6, step_size = c(3,1.1),
                                        burnin= 1e5, thin=1500))
  names(lspm_single_result) <- paste0("seed",(seed))
  node100results4d3_adapt_same_d <-  append(node100results4d3_adapt_same_d, lspm_single_result)
}
class(node100results4d3_adapt_same_d) <- "LSPM"
# save(node100results4d3_adapt_same_d, file = "node100results4d3_adapt_same_d.Rdata")

node100results4d3_adapt_low_d <- list()
for(seed in seed_number) {
  lspm_single_result <- list(LSPM(node100network4d[[paste0("seed",(seed))]]$network,
                                        n_dimen = 2, iter=1e6, step_size = c(3,1.1),
                                        burnin= 1e5, thin=1500))
  names(lspm_single_result) <- paste0("seed",(seed))
  node100results4d3_adapt_low_d <-  append(node100results4d3_adapt_low_d, lspm_single_result)
}
class(node100results4d3_adapt_low_d) <- "LSPM"
# save(node100results4d3_adapt_low_d, file = "node100results4d3_adapt_low_d.Rdata")

# trunc count -------------------------------------------------------------
source("~/lspm/inst/trunc_network_c.R")

node100results4d3_adapt_c <- list()
for(seed in seed_number) {
  lspm_single_result <- list(LSPM(node100network4d_c[[paste0("seed",(seed))]]$network,
                                        iter=1e6, family = "Poisson",
                                        step_size = c(5,0.2), burnin= 1e5, thin=2000))
  names(lspm_single_result) <- paste0("seed",(seed))
  node100results4d3_adapt_c <-  append(node100results4d3_adapt_c, lspm_single_result)
}
class(node100results4d3_adapt_c) <- "LSPM"
# save(node100results4d3_adapt_c, file = "node100results4d3_adapt_c.Rdata")


node100results4d3_adapt_high_d_c <- list()
for(seed in seed_number) {
  lspm_single_result <- list(LSPM(node100network4d_c[[paste0("seed",(seed))]]$network,
                                        iter=1e6, family = "Poisson", n_dimen=10,
                                        step_size = c(5,0.2), burnin= 1e5, thin=2000))
  names(lspm_single_result) <- paste0("seed",(seed))
  node100results4d3_adapt_high_d_c <-  append(node100results4d3_adapt_high_d_c, lspm_single_result)
}
class(node100results4d3_adapt_high_d_c) <- "LSPM"
# save(node100results4d3_adapt_high_d_c, file = "node100results4d3_adapt_high_d_c.Rdata")

node100results4d3_adapt_same_d_c <- list()
for(seed in seed_number) {
  lspm_single_result <- list(LSPM(node100network4d_c[[paste0("seed",(seed))]]$network,
                                        iter=1e6, family = "Poisson", n_dimen=4,
                                        step_size = c(5,0.2), burnin= 1e5, thin=2000))
  names(lspm_single_result) <- paste0("seed",(seed))
  node100results4d3_adapt_same_d_c <-  append(node100results4d3_adapt_same_d_c, lspm_single_result)
}
class(node100results4d3_adapt_same_d_c) <- "LSPM"
# save(node100results4d3_adapt_same_d_c, file = "node100results4d3_adapt_same_d_c.Rdata")

node100results4d3_adapt_low_d_c <- list()
for(seed in seed_number) {
  lspm_single_result <- list(LSPM(node100network4d_c[[paste0("seed",(seed))]]$network,
                                        iter=1e6, family = "Poisson", n_dimen=2,
                                        step_size = c(5,0.2), burnin= 1e5, thin=2000))
  names(lspm_single_result) <- paste0("seed",(seed))
  node100results4d3_adapt_low_d_c <-  append(node100results4d3_adapt_low_d_c, lspm_single_result)
}
class(node100results4d3_adapt_low_d_c) <- "LSPM"
# save(node100results4d3_adapt_low_d_c, file = "node100results4d3_adapt_low_d_c.Rdata")


# size binary -------------------------------------------------------------
source("~/lspm/inst/size_network.R")

# Fitting LSPM on 20 nodes
node20results_adapt <- list()
for(seed in seed_number) {
  lspm_single_result <- list(LSPM(node20network[[paste0("seed",(seed))]]$network,
                                        iter=5e5, step_size = c(3,1.1),
                                        burnin= 5e4, thin=1500))
  names(lspm_single_result) <- paste0("seed",(seed))
  node20results_adapt <-  append(node20results_adapt, lspm_single_result)
}
class(node20results_adapt) <- "LSPM"
# save(node20results_adapt, file = "node20results_adapt.Rdata")

# Fitting LSPM on 50 nodes
node50results_adapt <- list()
for(seed in seed_number) {
  lspm_single_result <- list(LSPM(node50network[[paste0("seed",(seed))]]$network,
                                        iter=5e5, step_size = c(3,1.1),
                                        burnin= 5e4, thin=1500))
  names(lspm_single_result) <- paste0("seed",(seed))
  node50results_adapt <-  append(node50results_adapt, lspm_single_result)
}
class(node50results_adapt) <- "LSPM"
# save(node50results_adapt, file = "node50results_adapt.Rdata")

# Fitting LSPM on 100 nodes
node100results_adapt <- list()
for(seed in seed_number) {
  lspm_single_result <- list(LSPM(node100network[[paste0("seed",(seed))]]$network,
                                        iter=5e5, step_size = c(3,1.1),
                                        burnin= 5e4, thin=1500))
  names(lspm_single_result) <- paste0("seed",(seed))
  node100results_adapt <-  append(node100results_adapt, lspm_single_result)
}
class(node100results_adapt) <- "LSPM"
# save(node100results_adapt, file = "node100results_adapt.Rdata")


# Fitting LSPM on 200 nodes
node200results_adapt <- list()
for(seed in seed_number) {
  lspm_single_result <- list(LSPM(node200network[[paste0("seed",(seed))]]$network,
                                        iter=5e5, step_size = c(3,1.1),
                                        burnin= 5e4, thin=1500))
  names(lspm_single_result) <- paste0("seed",(seed))
  node200results_adapt <-  append(node200results_adapt, lspm_single_result)
}
class(node200results_adapt) <- "LSPM"
# save(node200results_adapt, file = "node200results_adapt.Rdata")

# size count --------------------------------------------------------------
source("~/lspm/inst/size_network_c.R")

# Fitting LSPM on 20 nodes
node20results_adapt_c <- list()
for(seed in seed_number) {
  lspm_single_result <- list(LSPM(node20network_c[[paste0("seed",(seed))]]$network,
                                        iter=5e5, family = "Poisson",
                                        step_size = c(5,.4), burnin=5e4, thin=1000))
  names(lspm_single_result) <- paste0("seed",(seed))
  node20results_adapt_c <-  append(node20results_adapt_c, lspm_single_result)
}
class(node20results_adapt_c) <- "LSPM"
# save(node20results_adapt_c, file = "node20results_adapt_c.Rdata")

# Fitting LSPM on 50 nodes
node50results_adapt_c <- list()
for(seed in seed_number) {
  lspm_single_result <- list(LSPM(node50network_c[[paste0("seed",(seed))]]$network,
                                        iter=5e5, family = "Poisson",
                                        step_size = c(5,.4), burnin=5e4, thin=1000))
  names(lspm_single_result) <- paste0("seed",(seed))
  node50results_adapt_c <-  append(node50results_adapt_c, lspm_single_result)
}
class(node50results_adapt_c) <- "LSPM"
# save(node50results_adapt_c, file = "node50results_adapt_c.Rdata")

# Fitting LSPM on 100 nodes
node100results_adapt_c <- list()
for(seed in seed_number) {
  lspm_single_result <- list(LSPM(node100network_c[[paste0("seed",(seed))]]$network,
                                        iter=5e5, family = "Poisson",
                                        step_size = c(5,.4), burnin=5e4, thin=1000))
  names(lspm_single_result) <- paste0("seed",(seed))
  node100results_adapt_c <-  append(node100results_adapt_c, lspm_single_result)
}
class(node100results_adapt_c) <- "LSPM"
# save(node100results_adapt_c, file = "node100results_adapt_c.Rdata")

# Fitting LSPM on 200 nodes
node200results_adapt_c <- list()
for(seed in seed_number) {
  lspm_single_result <- list(LSPM(node200network_c[[paste0("seed",(seed))]]$network,
                                        iter=5e5, family = "Poisson",
                                        step_size = c(5,.4), burnin=5e4, thin=1000))
  names(lspm_single_result) <- paste0("seed",(seed))
  node200results_adapt_c <-  append(node200results_adapt_c, lspm_single_result)
}
class(node200results_adapt_c) <- "LSPM"
# save(node200results_adapt_c, file = "node200results_adapt_c.Rdata")


# density binary ----------------------------------------------------------
source("~/lspm/inst/dens_network.R")


node50results3d5_0_adapt <- list()
for(seed in seed_number) {
  lspm_single_result <- list(LSPM(node50network3d5_0[[paste0("seed",(seed))]]$network,
                                        iter=1e6, step_size = c(3,1.1),
                                        burnin= 1e5, thin=1500))
  names(lspm_single_result) <- paste0("seed",(seed))
  node50results3d5_0_adapt <-  append(node50results3d5_0_adapt, lspm_single_result)
}
class(node50results3d5_0_adapt) <- "LSPM"
# save(node50results3d5_0_adapt, file = "node50results3d5_0_adapt.Rdata")


node50results3d5_1_adapt <- list()
for(seed in seed_number) {
  lspm_single_result <- list(LSPM(node50network3d5_1[[paste0("seed",(seed))]]$network,
                                        iter=1e6, step_size = c(3,1.1),
                                        burnin= 1e5, thin=1500))
  names(lspm_single_result) <- paste0("seed",(seed))
  node50results3d5_1_adapt <-  append(node50results3d5_1_adapt, lspm_single_result)
}
class(node50results3d5_1_adapt) <- "LSPM"
# save(node50results3d5_1_adapt, file = "node50results3d5_1_adapt.Rdata")

node50results3d5_5_adapt <- list()
for(seed in seed_number) {
  lspm_single_result <- list(LSPM(node50network3d5_5[[paste0("seed",(seed))]]$network,
                                        iter=1e6, step_size = c(3,1.1),
                                        burnin= 1e5, thin=1500))
  names(lspm_single_result) <- paste0("seed",(seed))
  node50results3d5_5_adapt <-  append(node50results3d5_5_adapt, lspm_single_result)
}
class(node50results3d5_5_adapt) <- "LSPM"
# save(node50results3d5_5_adapt, file = "node50results3d5_5_adapt.Rdata")

node50results3d5_10_adapt <- list()
for(seed in seed_number) {
  lspm_single_result <- list(LSPM(node50network3d5_10[[paste0("seed",(seed))]]$network,
                                        iter=1e6, step_size = c(3,1.1),
                                        burnin= 1e5, thin=1500))
  names(lspm_single_result) <- paste0("seed",(seed))
  node50results3d5_10_adapt <-  append(node50results3d5_10_adapt, lspm_single_result)
}
class(node50results3d5_10_adapt) <- "LSPM"
# save(node50results3d5_10_adapt, file = "node50results3d5_10_adapt.Rdata")

node50results3d5_20_adapt <- list()
for(seed in seed_number) {
  lspm_single_result <- list(LSPM(node50network3d5_20[[paste0("seed",(seed))]]$network,
                                        iter=1e6, step_size = c(3,1.1),
                                        burnin= 1e5, thin=1500))
  names(lspm_single_result) <- paste0("seed",(seed))
  node50results3d5_20_adapt <-  append(node50results3d5_20_adapt, lspm_single_result)
}
class(node50results3d5_20_adapt) <- "LSPM"
# save(node50results3d5_20_adapt, file = "node50results3d5_20_adapt.Rdata")

node50results3d5_30_adapt <- list()
for(seed in seed_number) {
  lspm_single_result <- list(LSPM(node50network3d5_30[[paste0("seed",(seed))]]$network,
                                        iter=1e6, step_size = c(3,1.1),
                                        burnin= 1e5, thin=1500))
  names(lspm_single_result) <- paste0("seed",(seed))
  node50results3d5_30_adapt <-  append(node50results3d5_30_adapt, lspm_single_result)
}
class(node50results3d5_30_adapt) <- "LSPM"
# save(node50results3d5_30_adapt, file = "node50results3d5_30_adapt.Rdata")


# overdispersion count ----------------------------------------------------
source("~/lspm/inst/disp_network_c.R")

# Fitting LSPM on 100 nodes network with low overdispersion
count2d0_result_adapt <- list()
for(seed in seed_number) {
  lspm_single_result = 0
  lspm_single_result <- list(LSPM(count2d0_network[[paste0("seed",(seed))]]$network,
                                        iter=5e5, family = "Poisson",
                                        step_size = c(5,1.5),
                                        burnin= 5e4, thin=1000))
  names(lspm_single_result) <- paste0("seed",(seed))
  count2d0_result_adapt <-  append(count2d0_result_adapt, lspm_single_result)
}
class(count2d0_result_adapt) <- "LSPM"
# save(count2d0_result_adapt, file = "count2d0_result_adapt.Rdata")

# Fitting LSPM on 100 nodes network with moderate overdispersion
count2d1_result_adapt <- list()
for(seed in seed_number) {
  lspm_single_result = 0
  lspm_single_result <- list(LSPM(count2d1_network[[paste0("seed",(seed))]]$network,
                                        iter=5e5, family = "Poisson",
                                        step_size = c(5,.9),
                                        burnin= 5e4, thin=1000))
  names(lspm_single_result) <- paste0("seed",(seed))
  count2d1_result_adapt <-  append(count2d1_result_adapt, lspm_single_result)
}
class(count2d1_result_adapt) <- "LSPM"
# save(count2d1_result_adapt, file = "count2d1_result_adapt.Rdata")

# Fitting LSPM on 100 nodes network with high overdispersion
count2d5_result_adapt <- list()
for(seed in seed_number) {
  lspm_single_result = 0
  lspm_single_result <- list(LSPM(count2d5_network[[paste0("seed",(seed))]]$network,
                                        iter=5e5, family = "Poisson",
                                        step_size = c(5,.175),
                                        burnin= 1e5, thin=1000))
  names(lspm_single_result) <- paste0("seed",(seed))
  count2d5_result_adapt <-  append(count2d5_result_adapt, lspm_single_result)
}
class(count2d5_result_adapt) <- "LSPM"
# save(count2d5_result_adapt, file = "count2d5_result_adapt.Rdata")



# cat connectome application ----------------------------------------------

# Fitting LSPM on cat connectome
cat_lspm_adapt <- list()
for(seed in seed_number[1:10]) {
  lspm_single_result <- list(LSPM(cat_connectome, iter=5e5,
                                  step_size = c(3,1.3), seed = seed,
                                  burnin= 5e4, thin=2000))
  names(lspm_single_result) <- paste0("seed",(seed))
  cat_lspm_adapt <-  append(cat_lspm_adapt, lspm_single_result)
}
class(cat_lspm_adapt) <- "LSPM"
# save(cat_lspm_adapt, file = "cat_lspm_adapt.Rdata")

cat_lpm2d_full <- list()
for(seed in seed_number[1:10]) {
  lpm_single_result <- list(ergmm(catadj ~ euclidean(d=2), seed = seed))
  names(lpm_single_result) <- paste0("seed",(seed))
  cat_lpm2d_full <-  append(cat_lpm2d_full, lpm_single_result)
}
# save(cat_lpm2d_full, file = "cat_lpm2d_full.Rdata")


cat_lpm3d_full <- list()
for(seed in seed_number[1:10]) {
  lpm_single_result <- list(ergmm(catadj ~ euclidean(d=3), seed = seed))
  names(lpm_single_result) <- paste0("seed",(seed))
  cat_lpm3d_full <-  append(cat_lpm3d_full, lpm_single_result)
}
# save(cat_lpm3d_full, file = "cat_lpm3d_full.Rdata")

cat_lpm1d <- ergmm(catadj ~ euclidean(d=1), seed = 115)
cat_lpm2d <- ergmm(catadj ~ euclidean(d=2), seed = 115)
cat_lpm3d <- ergmm(catadj ~ euclidean(d=3), seed = 115)
cat_lpm4d <- ergmm(catadj ~ euclidean(d=4), seed = 115)
cat_lpm5d <- ergmm(catadj ~ euclidean(d=5), seed = 115)

summary(cat_lpm1d)$bic$overall
summary(cat_lpm2d)$bic$overall
summary(cat_lpm3d)$bic$overall
summary(cat_lpm4d)$bic$overall
summary(cat_lpm5d)$bic$overall

cat_lpm = list(oneD = cat_lpm1d, twoD = cat_lpm2d, threeD = cat_lpm3d, fourD = cat_lpm4d, fiveD = cat_lpm5d)
# save(cat_lpm, file = "cat_lpm.Rdata")

lpm_bic = do.call(rbind, lapply(cat_lpm, function(x) summary(x)$bic))
plot(do.call(rbind,lpm_bic[,6]), pch=20, col="blue", ylab="BIC", xlab="Dimension")


# zachary karate application ----------------------------------------------

# Fitting 5D LPSM on zachary karate club
zach_lspm_adapt <- list()
for(seed in seed_number[1:10]) {
  lspm_single_result <- list(LSPM(zachary, iter=5e5,
                                  step_size = c(3,.7), seed=seed,
                                  burnin=5e4, thin=2000))
  names(lspm_single_result) <- paste0("seed",(seed))
  zach_lspm_adapt <-  append(zach_lspm_adapt, lspm_single_result)
}
class(zach_lspm_adapt) <- "LSPM"
# save(zach_lspm_adapt, file = "zach_lspm_adapt.Rdata")

zach_lpm1d_full <- list()
for(seed in seed_number[1:10]) {
  lpm_single_result <- list(ergmm(zachary ~ euclidean(d=1), seed = seed))
  names(lpm_single_result) <- paste0("seed",(seed))
  zach_lpm1d_full <-  append(zach_lpm1d_full, lpm_single_result)
}
# do.call(c, lapply(zach_lpm1d_full, function(x) summary(x)$bic$overall))
# save(zach_lpm1d_full, file = "zach_lpm1d_full.Rdata")

zach_lpm2d_full <- list()
for(seed in seed_number[1:10]) {
  lpm_single_result <- list(ergmm(zachary ~ euclidean(d=2), seed = seed))
  names(lpm_single_result) <- paste0("seed",(seed))
  zach_lpm2d_full <-  append(zach_lpm2d_full, lpm_single_result)
}
# save(zach_lpm2d_full, file = "zach_lpm2d_full.Rdata")


# worm application --------------------------------------------------------

# Fitting 5D LPSM on worm
worm_lspm_adapt4 <- list()
for(seed in seed_number) {
  lspm_single_result <- list(LSPM(wormadj, iter=10e5, dim_threshold = c(0.8,0.9,5),
                                        seed = seed, step_size = c(2.4,2.7), 
                                        burnin= 10e4, thin=3000))
  names(lspm_single_result) <- paste0("seed",(seed))
  worm_lspm_adapt4 <-  append(worm_lspm_adapt4, lspm_single_result)
}
class(worm_lspm_adapt4) <- "LSPM"
save(worm_lspm_adapt4, file = "worm_lspm_adapt4.Rdata")

worm_lpm1d <- ergmm(wormadj ~ euclidean(d=1), seed = 115)
worm_lpm2d <- ergmm(wormadj ~ euclidean(d=2), seed = 115)
worm_lpm3d <- ergmm(wormadj ~ euclidean(d=3), seed = 115)
worm_lpm4d <- ergmm(wormadj ~ euclidean(d=4), seed = 115)
worm_lpm5d <- ergmm(wormadj ~ euclidean(d=5), seed = 115)
worm_lpm6d <- ergmm(wormadj ~ euclidean(d=6), seed = 115)
worm_lpm7d <- ergmm(wormadj ~ euclidean(d=7), seed = 115)

worm_lpm3d_full <- list()
for(seed in seed_number[1:10]) {
  lpm_single_result <- list(ergmm(wormadj ~ euclidean(d=3), seed = seed))
  names(lpm_single_result) <- paste0("seed",(seed))
  worm_lpm3d_full <-  append(worm_lpm3d_full, lpm_single_result)
}
# save(worm_lpm3d_full, file = "worm_lpm3d_full.Rdata")

worm_lpm4d_full <- list()
for(seed in seed_number) {
  lpm_single_result <- list(ergmm(wormadj ~ euclidean(d=4), seed = seed))
  names(lpm_single_result) <- paste0("seed",(seed))
  worm_lpm4d_full <-  append(worm_lpm4d_full, lpm_single_result)
}
# save(worm_lpm4d_full, file = "worm_lpm4d_full.Rdata")

worm_lpm5d_full <- list()
for(seed in seed_number[1:10]) {
  lpm_single_result <- list(ergmm(wormadj ~ euclidean(d=5), seed = seed))
  names(lpm_single_result) <- paste0("seed",(seed))
  worm_lpm5d_full <-  append(worm_lpm5d_full, lpm_single_result)
}
save(worm_lpm5d_full, file = "worm_lpm5d_full.Rdata")



# phone count application -------------------------------------------------

phonesnovlspm_adapt <- list()
for(seed in seed_number[1:10]) {
  lspm_single_result <- list(LSPM(phone, family = "Poisson", iter=5e5,
                                  step_size = c(4,.5),
                                  burnin= 5e4, thin=2000))
  names(lspm_single_result) <- paste0("seed",(seed))
  phonesnovlspm_adapt <-  append(phonesnovlspm_adapt, lspm_single_result)
}
class(phonesnovlspm_adapt) <- "LSPM"
# save(phonesnovlspm_adapt, file = "phonesnovlspm_adapt.Rdata")

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


phonesnov_bin = phone
phonesnov_bin[phonesnov_bin>0] = 1

phonesnovlspm_bin <- list()
for(seed in seed_number[1:10]) {
  lspm_single_result <- list(LSPM(phonesnov_bin, family = "logit", iter=5e5,
                                  step_size = c(4,.5),
                                  burnin= 5e4, thin=2000))
  names(lspm_single_result) <- paste0("seed",(seed))
  phonesnovlspm_bin <-  append(phonesnovlspm_bin, lspm_single_result)
}
class(phonesnovlspm_bin) <- "LSPM"


# 1D vs 2D appendix -------------------------------------------------------







# extras ------------------------------------------------------------------


# node50results3d5_10_adapt <- list()
# for(seed in seed_number[19:30]) {
#   lspm_single_result <- list(LSPM(node50network3d5_10[[paste0("seed",(seed))]]$network,
#                                   iter=1e6, step_size = c(3,1.1),
#                                   burnin= 1e5, thin=1500))
#   names(lspm_single_result) <- paste0("seed",(seed))
#   node50results3d5_10_adapt <-  append(node50results3d5_10_adapt, lspm_single_result)
# }
# class(node50results3d5_10_adapt) <- "LSPM"
# # save(node50results3d5_10_adapt, file = "node50results3d5_10_adapt.Rdata")
#
# node50results3d5_20_adapt <- list()
# for(seed in seed_number[21:30]) {
#   lspm_single_result <- list(LSPM(node50network3d5_20[[paste0("seed",(seed))]]$network,
#                                   iter=1e6, step_size = c(3,1.1),
#                                   burnin= 1e5, thin=1500))
#   names(lspm_single_result) <- paste0("seed",(seed))
#   node50results3d5_20_adapt <-  append(node50results3d5_20_adapt, lspm_single_result)
# }
# class(node50results3d5_20_adapt) <- "LSPM"
# # save(node50results3d5_20_adapt, file = "node50results3d5_20_adapt.Rdata")
#
# node50results3d5_30_adapt <- list()
# for(seed in seed_number[16:30]) {
#   lspm_single_result <- list(LSPM(node50network3d5_30[[paste0("seed",(seed))]]$network,
#                                   iter=1e6, step_size = c(3,1.1),
#                                   burnin= 1e5, thin=1500))
#   names(lspm_single_result) <- paste0("seed",(seed))
#   node50results3d5_30_adapt <-  append(node50results3d5_30_adapt, lspm_single_result)
# }
# class(node50results3d5_30_adapt) <- "LSPM"
# # save(node50results3d5_30_adapt, file = "node50results3d5_30_adapt.Rdata")
#
#
# # Fitting LSPM on 50 nodes
# node50results_adapt_10d <- list()
# for(seed in seed_number[1:3]) {
#   lspm_single_result <- list(LSPM(node50network[[paste0("seed",(seed))]]$network, n_dimen = 10,
#                                         iter=5e5, step_size = c(3,1.1),
#                                         burnin= 5e4, thin=1500))
#   names(lspm_single_result) <- paste0("seed",(seed))
#   node50results_adapt_10d <-  append(node50results_adapt_10d, lspm_single_result)
# }
# class(node50results_adapt_10d) <- "LSPM"
#
#
# # Fitting LSPM on 50 nodes
# node50results_adapt_20d <- list()
# for(seed in seed_number[1:3]) {
#   lspm_single_result <- list(LSPM(node50network[[paste0("seed",(seed))]]$network, n_dimen = 20,
#                                         iter=5e5, step_size = c(3,1.1),
#                                         burnin= 5e4, thin=1500))
#   names(lspm_single_result) <- paste0("seed",(seed))
#   node50results_adapt_20d <-  append(node50results_adapt_20d, lspm_single_result)
# }
# class(node50results_adapt_20d) <- "LSPM"
#
#
# # Fitting LSPM on 50 nodes
# node50results_adapt_30d <- list()
# for(seed in seed_number[1:3]) {
#   lspm_single_result <- list(LSPM(node50network[[paste0("seed",(seed))]]$network, n_dimen = 30,
#                                         iter=5e5, step_size = c(3,1.1),
#                                         burnin= 5e4, thin=1500))
#   names(lspm_single_result) <- paste0("seed",(seed))
#   node50results_adapt_30d <-  append(node50results_adapt_30d, lspm_single_result)
# }
# class(node50results_adapt_30d) <- "LSPM"
#
#
# node100results4d3_adapt_high_20d <- list()
# for(seed in seed_number[1:3]) {
#   lspm_single_result <- list(LSPM(node100network4d[[paste0("seed",(seed))]]$network,
#                                         n_dimen = 20, iter=1e6, step_size = c(3,1.1),
#                                         burnin= 1e5, thin=1500))
#   names(lspm_single_result) <- paste0("seed",(seed))
#   node100results4d3_adapt_high_20d <-  append(node100results4d3_adapt_high_20d, lspm_single_result)
# }
# class(node100results4d3_adapt_high_20d) <- "LSPM"
#
#
# node100results4d3_adapt_high_30d <- list()
# for(seed in seed_number[1:3]) {
#   lspm_single_result <- list(LSPM(node100network4d[[paste0("seed",(seed))]]$network,
#                                         n_dimen = 30, iter=1e6, step_size = c(3,1.1),
#                                         burnin= 1e5, thin=1500))
#   names(lspm_single_result) <- paste0("seed",(seed))
#   node100results4d3_adapt_high_30d <-  append(node100results4d3_adapt_high_30d, lspm_single_result)
# }
# class(node100results4d3_adapt_high_30d) <- "LSPM"
#
#
# ######## check all results
#
# lapply(node100results4d3_adapt, function(x) diagLSPM(x, d=names(which.max(table(x$iter_d)))))
#
# lapply(node100results4d3_adapt_high_d, function(x) diagLSPM(x, d=names(which.max(table(x$iter_d)))))
#
#
# lapply(node100results4d3_adapt_low_d_c, function(x) diagLSPM(x, d=names(which.max(table(x$iter_d)))))
