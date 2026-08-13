# LSPM

A set of latent shrinkage position models are available here for different purposes:
* LSPM for the modelling of binary or count network
* LSPCM for the modelling of binary network with clustering setting
* vLSPM for the modelling of binary network via a quicker variational approach


## Overview
Interactions between actors are widely represented using a network. The latent position model (LPM) positions each actor in the network in a latent space. Inferring the dimension of this latent space is a difficult problem; typically, two dimensions are used for visualisation and interpretation purposes but this may limit the model from fully describing the data. Often, model selection criteria are used to select the optimal dimension but this requires selection of criteria and fitting a range of models which can be computationally expensive. The proposed latent shrinkage position model (LSPM) extends the LPM by using a Bayesian nonparametric framework via a shrinkage prior. The LSPM intrinsically infers the number of effective dimensions for the latent space, obviating the need for using a model selection criterion and fitting a number of models. A truncated multiplicative gamma process prior is used to ensure shrinkage of the variance in latent positions across higher dimensions. While the LSPM is applicable across a variety of link types, binary and count edges are considered here. Inference proceeds via a Markov chain Monte Carlo algorithm; surrogate proposal distributions which shadow the target distributions are derived, reducing the computational burden. Simulation studies explore the performance of the LSPM and the utility of the methodology is illustrated through a range of network data examples.

Gwee, X. Y., Gormley, I. C., and Fop, M. (2023). A latent shrinkage position model for binary and count network data. *Bayesian Analysis*, pages 1 – 29. [https://doi.org/10.1214/23-BA1403](https://doi.org/10.1214/23-BA1403)

Gwee, X. Y., Gormley, I. C., and Fop, M. (2023). Model-based clustering for network data via a latent shrinkage position cluster model. arXiv preprint: [https://doi.org/10.48550/arxiv.2310.03630](https://doi.org/10.48550/arxiv.2310.03630)




## Installation

```r
# Install from CRAN (To be published.. )
install.packages("lspm")

# Or the development version from Gitlab
# install.packages("devtools")
devtools::install_gitlab("gwee95/lspm")

# Load the library
library(lspm)
```

You can also clone this repositary and install it locally.
However, make sure to install dependencies in your computer for the package to work.

Run the following code: 
`remotes::install_local(dependencies = TRUE)`

## Quick Guide

### LSPM for count network

```r
# Simulate a count network with 2 latent dimensions
countMTGP_2D <- networkMTGP(n = 50, alpha = 3, deltas = c(0.5,1.1), type = "count")

# Fitting a Poisson LSPM
countLSPM <- LSPM(countMTGP_2D$network, family = "Poisson", 
step_size = c(3,.6), iter = 2e4, burnin = 1000, thin = 200)

# Plot the proportion of posterior dimension to identify effective dimensions
plot(countLSPM)

# Plot the latent positions of the network
plot(countLSPM) 

# Plot the trace plot to check for convergence
diagLSPM(countLSPM, n_dimen = 2)
diagLSPM(countLSPM, n_dimen = 2, what = "deltas")

# Perform posterior predictive checking
predLSPM_count <- predcheck(10, n_dimen = 2, LSPM_object = countLSPM, type = "count")

# Plot the posterior predictive checking
predPlot(predLSPM_count, type="count")


# Fitting LSPM on the phone log network
phone_LSPM <- LSPM(phone, family = "Poisson", step_size = c(3,0.3),
iter = 1e4, burnin = 1000, thin = 10)
plot(phone_LSPM)
diagLSPM(phone_LSPM, n_dimen = 4) # need more iterations to reach convergence

---------------------------------------

```

### LSPCM for clustering setting

```r
# Simulation study of LSPCM ---------------

# Simulate binary network from positions of 2 dimensions and 3 clusters
sampleLSPCM_2D <- networkMTGP_cluster(n = 50, n_dimen = 2, G = 3, alpha = 7,
cluster_mean = matrix(c(0,0, -5,0, -5,5), ncol = 2, byrow = TRUE), seed = 1,
deltas = matrix(rep(c(.5,1.05), 3), nrow = 3, byrow = TRUE))

# Fitting a logistic LSPCM.
resultLSPCM_2D <- LSPCM(sampleLSPCM_2D$network, n_dimen = 4, G = 10,
adapt_param = c(4, 1e-5), iter = 5e4, burnin = 1e3, thin = 300,
dim_threshold = c(0.8,0.9,5), step_size = c(3.3,1), mix_prior = 0.05)

plot(resultLSPCM_2D) # plot the proportion of posterior number of cluster
plot(resultLSPCM_2D, parameter = "dimensions") # plot the posterior number of dimension

# Plot the 2D latent dimension scatterplot
plot(resultLSPCM_2D, parameter = "positions", n_dimen = 2)

# Application to a real football Twitter network ---------------

# Fitting LSPCM on the football network 
football_LSPCM <- LSPCM(football$football_adj, n_dimen = 5, G = 20,
adapt_param = c(4, 5e-4), iter = 5e4, burnin = 5e3, thin = 100,
dim_threshold = c(0.8, 0.9, 5), step_size = c(3.3, 2.5), mix_prior = 0.01,
dim1_prior = c(2, 1), dim2_prior = c(3, 1), cmean_scale = 9)

# Plot the proportion of posterior number of cluster
plot(football_LSPCM)

# Plot the posterior number of dimension 
plot(football_LSPCM, parameter = "dimensions")

# Maximizes the PEAR function to find a representative cluster
football_cluster <- LSPCM_labels(football_LSPCM, type = "PEAR",
                                 true_labels = football$football_club)
football_cluster$mpear # the representative cluster labels

# Adjusted Rand index between the estimated cluster and known club affiliation
football_cluster$ARI_pear

# Plot the LSPCM football positions coloured by the inferred cluster
plot(football_LSPCM, parameter = "position", n_dimen = lspm:::calculate_mode(football_LSPCM$iter_d),
col = football_cluster$mpear, pch = 20)

# Plot the posterior similarity matrix heatmap ordered by inferred cluster
plot(football_cluster)


```

### Variational LSPM for binary network

```r
# Simulate a binary network with 2 true dimensions
sampleMTGP_2D <- networkMTGP(n = 100, alpha = 1, deltas = c(0.5,1.1),
type = "binary", seed = 11)

# Fitting a variational LSPM.
sampleVLSPM <- vLSPM(sampleMTGP_2D$network, n_dimen = 5, N_iter=50)

```


<!-- [(Hoff, P. D, et.al. 2002)](https://doi.org/10.1198/016214502388618906)
[(Bhattacharya, A., & Dunson, D. B. 2011)](https://doi.org/10.1093/biomet/asr013)
 -->

