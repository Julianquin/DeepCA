#'#' Latent Shrinkage Position Cluster Model
#'
#' A Bayesian nonparametric method of fitting an overfitted sparse mixture
#' latent position model on network data. LSPCM is able to indicate the number
#' of clusters and dimensions with non-zero variance latent positions required
#' to describe the network by using a shrinkage prior called the multiplicative
#' gamma process. Truncated gamma distributions are used to ensure shrinkage
#' across higher dimensions.
#'
#'
#' @param network Adjacency matrix of a network data
#' @param n_dimen Number of fitted latent dimensions for the model, also known
#' as truncation level.
#' @param iter Number of iteration for the MCMC algorithm
#' @param burnin Size of the burn-in period
#' @param thin Size between thinning of the MCMC chain
#' @param step_size A vector of length two representing the proposal
#' distribution step size of alpha and the latent positions respectively
#' @param initial_adjust A vector of length two representing how much adjustment
#' is made on the initial alpha and latent positions respectively. The
#' adjustment on \eqn{\alpha} is a multiplication on the initial value directly.
#' The adjustment on the latent position is a multiplication on the covariance
#' matrix of the noise added to the initial latent positions.
#' @param alpha_prior A vector of length two representing the hyperparameters on
#' the \eqn{\alpha} normal prior with the values representing mean and standard
#' deviation respectively
#' @param dim1_prior A vector of length two representing the hyperparameters on
#' the \eqn{\delta_1} gamma prior with the values representing shape and rate
#' respectively
#' @param dim2_prior A vector of length two representing the hyperparameters on
#' the \eqn{\delta_h} gamma prior with the values representing shape and rate
#' respectively
#' @param seed Specify how the random number generator should be initialized
#' @param dim_threshold A vector of length three representing the thresholds
#' needed to reduce, increase, and increase if current dimension equal to one
#' respectively. The 1st number represents the minimum cumulative proportion of
#' variance required for \eqn{(p-1)}-th dimension to have to induce dimension
#' reduction. The 2nd number represents the inverse shrinkage strength required
#' on the \eqn{p}-th dimension to induce dimension increase. The 3rd number
#' represents the proportion of nodes (in multiplication) having deviation more
#' than 95\% critical value to induce dimension increase.
#' @param adapt_param A vector of length two representing the parameters for the
#' exponential formula that determines the adaptation probability. The formula
#' is \eqn{P(s) = exp(-\kappa_0 - \kappa_1 * s )}.
#' @param G Number of initial clusters, or mixture components fitted to the
#' model
#' @param nu_prior The hyperparameter for the mixture prior
#' @param cmean_scale A value for the scaling factor on the component mean
#'
#' @details
#' The latent shrinkage position cluster model (LSPCM) facilitates automatic
#' estimation of the clusters and dimensionality of the latent space. It uses
#' an overfitted mixture prior to identify the clusters by emptying out
#' redundant mixture components. The LSPCM also automatically infers the number
#' of dimensions via an adaptive MCMC sampler that dynamically grows and
#' shrinks the number of dimensions. The LSPCM has a Bayesian nonparametric
#' framework which allows for infinitely many dimensions, but has increasingly
#' smaller variance of positions as the dimension grows via the multiplicative
#' truncated gamma process (MTGP) prior. With the LSPCM, it obviates the need
#' to choose a model selection criterion and run computationally expensive
#' model selection procedures. It also offers uncertainties via posterior
#' distributions of the number of clusters and dimensions. The LSPCM uses a
#' Metropolis-within-Gibbs sampler to sample the posterior distribution.
#'
#' @return A list containing the computed thinned and after burn-in MCMC chains
#' for the different parameters and its initial settings
#' \item{alpha}{The overall connection level, \eqn{\alpha}}
#' \item{positions}{The \eqn{n x p x S} array of latent positions, \eqn{Z}}
#' \item{omegas}{A \eqn{p x S} matrix of latent position precisions,
#' \eqn{\omega}}
#' \item{variances}{A \eqn{p x S} matrix of the latent position variances,
#' \eqn{\omega^{-1}}}
#' \item{deltas}{A \eqn{p x S} matrix of the shrinkage strengths, \eqn{\delta}}
#' \item{like}{The likelihood of the latent positions}
#' \item{aca}{The acceptance rate of the \eqn{\alpha} parameter}
#' \item{acz}{The acceptance rate of the latent positions}
#' \item{initalisation}{The initial setting used to configure the algorithm}
#' \item{cluster_mean}{The \eqn{G x p x S} array of latent positions}
#' \item{mix_prop}{The \eqn{G x S} matrix of the mixing proportion}
#' \item{cluster}{The \eqn{n x S} matrix of the cluster labels of the nodes}
#' \item{cluster_prob}{The \eqn{n x S} matrix of the cluster probability of the
#' nodes}
#' \item{burnin_ref_like}{The likelihood of the reference positions used from
#' the burn in period}
#' \item{burnin_ref_cluster_mean}{The cluster mean of the reference positions
#' used from the burn in period}
#' \item{iter_d}{The active dimensions across the iterations}
#' \item{iter_G}{The active clusters across the iterations}
#' \item{adapt_iter}{The number of times the dimension adaptation has occurred
#' to check for the threshold (this does not necessarily change the number of
#' dimension)}
#'
#' @export
#'
#' @importFrom stats cmdscale dist dnorm rbinom rgamma rnorm runif var kmeans
#' qnorm
#' @importFrom truncdist rtrunc
#' @importFrom mvtnorm rmvnorm
#' @importFrom sna geodist netlogit
#' @importFrom Rfast dmvnorm colVars Diag.matrix Dist lower_tri colsums
#' rowsums eachrow
#' @importFrom MCMCpack rdirichlet
#' @importFrom mclust Mclust mclustBIC
#'
#'
#' @references
#' Gwee X.Y., Gormley I.C., Fop M. (2023). A latent shrinkage position cluster
#' model for network data. \emph{TBA journal}
#'
#' @family functions
#'
#' @examples
#' # Simulate binary network from positions of 2 dimensions and 3 clusters
#' sampleLSPCM_2D <- networkMTGP_cluster(n = 50, n_dimen = 2, G = 3, alpha = 7,
#' cluster_mean = matrix(c(0,0, -5,0, -5,5), ncol = 2, byrow = TRUE), seed = 1,
#' deltas = matrix(rep(c(.5,1.05), 3), nrow = 3, byrow = TRUE))
#'
#' # Fitting a logistic LSPCM.
#' resultLSPCM_2D <- LSPCM(sampleLSPCM_2D$network, n_dimen = 4, G = 10,
#' adapt_param = c(4, 1e-5), iter = 5e4, burnin = 1e3, thin = 300,
#' dim_threshold = c(0.8,0.9,5), step_size = c(3.3,1,0.5), nu_prior = c(1,20))
#'
#' plot(resultLSPCM_2D) # to plot the proportion of posterior cluster
#' plot(resultLSPCM_2D, parameter = "dimensions") # posterior dimension plot
#'
#' # Plot the 2D latent dimension scatterplot
#' plot(resultLSPCM_2D, parameter = "positions", n_dimen = 2)
#'
#' # Plot the shrinkage strengths where the active dimension is 2
#' plot(resultLSPCM_2D, parameter = 'deltas', n_dimen = 2)
#'
#' # Plot the trace plot conditioned on 2 active dimension to check convergence
#' diagLSPM(resultLSPCM_2D, n_dimen = 2)
#'
LSPCM <- function(network,
                   G = 20,
                   n_dimen = 5,
                   iter = 1e5,
                   burnin = 1e4,
                   thin = 1e3,
                   step_size = c(3, 3, 0.5),
                   initial_adjust = c(1, 0.01),
                   alpha_prior = c(0, 2),
                   dim1_prior = c(2, 1),
                   dim2_prior = c(3, 1),
                   nu_prior = c(5, 100),
                   psi_prior = c(400,400),
                   seed,
                   dim_threshold = c(0.8,0.95,5),
                   adapt_param = c(3.2, 3e-5),
                   cmean_scale = 9) {
  if (!is.matrix(network))
    return(print("Network is not an adjacency matrix"))

  start_time <- Sys.time()
  if (!missing(seed)) {
    set.seed(seed) # set seed if exists
  } else {
    seed <- sample(1:1e5, 1) # sample a seed from 1 to 10,000
  }
  # browser()
  n_obs <- dim(network)[1]
  n_dimen0 = n_dimen

  # Step Sizes for proposal distribution
  alpha_p_step <- step_size[1]
  latent_step <- step_size[2]
  nu_shape_step <- step_size[3]
  z_proposed_variance <-
    (latent_step / n_obs) ^ 2 # squared because cov matrix used

  # Alpha hyperparameters
  alpha_prior_mean <- alpha_prior[1]
  alpha_prior_sd <- alpha_prior[2]

  # Delta hyperparam
  a1 <- dim1_prior[1]
  a2 <- dim2_prior[1]
  b1 <- dim1_prior[2]
  b2 <- dim2_prior[2]

  limit_max_GD = 10 # limiting max geodesic distance

  # Initialise latent positions
  max_dist <- # noting maximum geodesic distance of network
    max(geodist(network, inf.replace = 0, count.paths = FALSE)$gdist)

  ifelse(max_dist > limit_max_GD, limit_max_GD, max_dist) # limiting max G. D.

  geo_dist <- # geodesic distance
    geodist(network,
            inf.replace = max_dist + floor(max_dist / 2),
            count.paths = FALSE)$gdist
  geo_dist[geo_dist > limit_max_GD] = limit_max_GD # replace higher than limit
  mds_result <-
    cmdscale(geo_dist, k = ceiling((n_obs-1)/2), eig = T) # multidimensional scaling

  if(missing(n_dimen)){
    mds_eig = mds_result$eig # extract eigenvalue
    mds_eig_positive = mds_eig[mds_eig > 0] # only consider positive eigenvalues
    mds_var_prop = mds_eig_positive / sum(mds_eig_positive) # proportion of variance
    n_dimen0 = n_dimen = min(table(kmeans(mds_var_prop, centers = 2)$cluster)) # cluster to obtain optimal dimension, informative dimension usually separate from non-informative
  }
  z_initial0 <- mds_result$points[,1:n_dimen, drop=F]

  # z_initial0 <-
  #   isoMDS(geo_dist, k = n_dimen, trace = F)$points # non-metric multidimensional scaling

  # z_initial0<- latentnet::ergmm(network::as.network(network) ~ euclidean(d=n_dimen))$mcmc.pmode$Z

  # Introduce noise to the initial latent positions
  mds_var = colVars(z_initial0)
  z_initial0 <- z_initial0 +
    Rfast::rmvnorm(n_obs, mu = rep(0,n_dimen),
                   sigma = Diag.matrix(n_dimen, mds_var * initial_adjust[2]))

  z_initial0 <-
    scale(z_initial0, scale = FALSE) # center the initial positions

  dist_btwn_z_ini <- Dist(z_initial0, square=T)  # squared Euclidean distance

  # Logistic regression initialisation
  suppressWarnings(logis_coeff <-
                     netlogit(network, dist_btwn_z_ini,
                              reps = 1)$coefficients)
  # Initialise and rescale alpha
  alpha_initial0 <-
    alpha_initial <- logis_coeff[1] * initial_adjust[1]

  # Initialise and rescale Z by coefficients
  z_initial0 <-
    burnin_pos <- z_initial <- z_initial0 * abs(logis_coeff[2]) ^ (.5)


  dist_btwn_z_ini <- Dist(z_initial0, square = T)

  # log likelihood calculation
  like_1st_term_dist = sum(dist_btwn_z_ini * network)
  like_1st_term = sum(network) * alpha_initial - like_1st_term_dist

  lower_tri_dist <- lower_tri(dist_btwn_z_ini)
  like_2nd_term = sum(log1p(exp(
    alpha_initial - lower_tri_dist
  ))) * 2 # + (n_obs * log(2))
  like0 <-
    burnin_max_like <- burnin_ref_like <-
    loglikelihood_initial_sum <-  like_1st_term - like_2nd_term


  # like0 <- burnin_max_like <- loglikelihood_initial_sum <-
  #   loglike(network, alpha = alpha_initial0, dist_btwn_z = dist_btwn_z_ini)

  logit_p_numerator <-
    exp(alpha_initial - lower_tri_dist) # numerator component for logit function

  # Calculate parameters for the informed alpha proposal
  alpha_proposed_variance0 <-
    1 / (sum(logit_p_numerator / (1 + logit_p_numerator) ^ 2) +
           1 / alpha_prior_sd ^ 2)
  alpha_proposed_mean0 <- alpha_initial +
    alpha_proposed_variance0 *
    (sum(network) - 2 * sum((
      logit_p_numerator / (1 + logit_p_numerator)
    ))
    + (1 / alpha_prior_sd ^ 2) * (alpha_prior_mean - alpha_initial))

  # K-means clustering
  kmeans_initial <- kmeans(z_initial, centers = G)
  Ki0 <- Ki <- mclust::Mclust(z_initial, G=1:G, modelNames = "EEI")$classification
  Ki_count0 <- Ki_count <- tabulate(Ki, nbins = G)
  # Obtain precision from starting latent position
  OMEGAS <- OMEGAS0 <- 1 / apply(z_initial0, 2, var)

  # Calculate delta by dividing omegas with previous dimension
  delta_sub <- 1.01 # the sub value when delta < 1
  DELTAS <- rep(1, n_dimen)
  DELTAS[1] <- OMEGAS[1]
  for (i in 2:n_dimen) {
    delta_temp <- OMEGAS[i] / OMEGAS[i - 1]
    ifelse(delta_temp < 1,
           DELTAS[i] <-
             delta_sub,
           DELTAS[i] <- delta_temp) # check >1
  }
  OMEGAS <- OMEGAS0 <- cumprod(DELTAS)
  DELTAS0 <- DELTAS

  iter_thin = ceiling((iter - burnin) / thin)
  adapt_iter = 0
  ALPHA <- LIKE <- aca <- acz <- acnu <- iter_d <- iter_G <- NU_STORE <-
    matrix(0, nrow = iter_thin, ncol = 1) # empty matrix for alpha & likelihood
  available_d = n_dimen

  LATENT_POS <- array(NA, dim = c(n_obs, n_dimen, iter_thin)) # empty matrix for latent positions
  MIX_PROP_STORE <- KI_COUNT_STORE <- PSI_STORE <- matrix(NA, nrow = iter_thin, ncol = G)
  KI_STORE <- matrix(NA, nrow = iter_thin, ncol = n_obs)
  OMEGAS_STORE <- VARIANCES_STORE <- DELTAS_STORE <- matrix(NA,
                                                            nrow = iter_thin,
                                                            ncol = n_dimen)
  PKI_STORE <- array(NA, dim = c(n_obs, G, iter_thin))
  CLUSTER_MEAN_STORE <-  array(NA, dim = c(G, n_dimen, iter_thin))

  cluster_mean_initial <- burnin_ref_cluster_mean <- matrix(0, nrow= G,
                                                            ncol= n_dimen)

  psi = rep(1, G)
  nu = nu_prior[1]/nu_prior[2]

  s_thin = 0
  cat("MCMC chain progress: \n")
  # MCMC chain
  for (s in 1:iter) {
    if (s > burnin & ((s - burnin) %% thin) == 0) {
      s_thin = s_thin + 1
      s_allow = TRUE
    } else {
      s_allow = FALSE
    }
    aca_allow = acz_allow = acnu_allow = FALSE

    if(s_allow & !(n_dimen %in% available_d) & all(n_dimen > available_d)) {
      LATENT_POS <- abind(LATENT_POS,
                          array(NA, dim = c(n_obs, 1, iter_thin)), along = 2)
      OMEGAS_STORE <- cbind(OMEGAS_STORE,
                            matrix(NA, nrow = iter_thin, ncol = 1))
      VARIANCES_STORE <- cbind(VARIANCES_STORE,
                               matrix(NA, nrow = iter_thin, ncol = 1))
      DELTAS_STORE <- cbind(DELTAS_STORE,
                            matrix(NA, nrow = iter_thin, ncol = 1))
      CLUSTER_MEAN_STORE =  abind(CLUSTER_MEAN_STORE,
                                  array(NA, dim = c(G, 1, iter_thin)),
                                  along = 2)

      available_d = c(available_d, n_dimen)
    }

    # Gibbs MVN sample posterior cluster_mean
    for (group in 1:G) {
      # browser()
      cluster_mean_sigma <- 1/(OMEGAS * (psi[group]*Ki_count[group] + (1/cmean_scale)))
      temp_mean = (OMEGAS * colsums(z_initial[Ki == group, , drop = FALSE])) *
        cluster_mean_sigma

      cluster_mean_initial[group, ] <-Rfast::rmvnorm(
        1,
        mu = temp_mean ,
        sigma = Diag.matrix(n_dimen, v = cluster_mean_sigma)
      )
    }

    # Gibbs update mixing proportion
    mix_prop <- rdirichlet(1, Ki_count + nu)

    if(any(mix_prop==0)) mix_prop[mix_prop==0] = .Machine$double.eps # in cases where mix_prop has 0, we scale up by nu to prevent floating-point precision problemplot(resultLSPCM_2D, parameter = "positions", n_dimen = 2, col=maxpear(comp.psm(resultLSPCM_2D$cluster))$cls)
    # mix prop all add constant and renormalize    .Machine$double.eps
    mix_prop <- mix_prop/rowSums(mix_prop)

    # Calculate indicator Ki table
    PKi <- matrix(nrow = n_obs, ncol = G)
    for (group in 1:G) {
      PKi[, group] <-
        mix_prop[group] * Rfast::dmvnorm(
          z_initial,
          mu = cluster_mean_initial[group,],
          sigma = Diag.matrix(n_dimen, v = 1 / OMEGAS)
        )
    }

    PKi[PKi==0] = .Machine$double.eps # very rare case doesnt cause 0-related error
    # PKi_numerator = eachrow(PKi, mix_prop) # column wise multiplication Rfast
    PKi_norm <- PKi / rowsums(PKi)
    # PKi_norm[PKi_norm==0] = .Machine$double.eps # very rare case doesnt cause 0-related error
    # browser()
    # Ki <-l
    #   apply(PKi, 1, function(x)
    #     sample.int(G, 1, TRUE, prob = x)) # replace true makes it slightly faster
    Ki = rep(0, n_obs)
    for(i in 1:n_obs) {
      Ki[i] <- sample.int(G, 1, TRUE, prob = PKi_norm[i,])
    }
    Ki_count <- tabulate(Ki, nbins = G)
    G_index <- which(Ki_count!=0,arr.ind = T) # ignore empty cluster to reduce iteration for some sampling

    # Gibbs for psi -----------------------------------------------------------
    # browser()
    for (group in G_index) {
      z_minus_mean = eachrow(z_initial[Ki == group, , drop = FALSE],
                             cluster_mean_initial[group, , drop = FALSE],
                             oper = "-")
      beta_sum <- colsums(z_minus_mean *  z_minus_mean ) * (OMEGAS) # only diagonal of the matrix maths

      psi[group] <- rgamma(1,
                           shape = psi_prior[1] + (n_dimen* Ki_count[group]/2) ,
                           rate = psi_prior[2] + 0.5 * sum(beta_sum)
      )

    }



    # M-H for Z --------------------------------------------------------------
    z_proposed <-  matrix(0, nrow = n_obs, ncol = n_dimen)
    for (group in G_index) {
      z_proposed[Ki == group, ] <-
        z_initial[Ki == group, , drop = FALSE] + Rfast::rmvnorm(
          Ki_count[group], mu = rep(0, n_dimen),
          sigma = Diag.matrix(n_dimen, v = z_proposed_variance / psi[group]*OMEGAS)
        ) # proposed distribution
    }


    dist_btwn_z_prop <- Dist(z_proposed, square=T)

    # log likelihood calculation
    like_1st_term_dist_p = sum(dist_btwn_z_prop * network)
    like_1st_term = sum(network) * alpha_initial - like_1st_term_dist_p

    lower_tri_dist_p = lower_tri(dist_btwn_z_prop)
    like_2nd_term = sum(log1p(exp(
      alpha_initial - lower_tri_dist_p
    ))) * 2 # + (n_obs * log(2))
    loglikelihood_proposed_sum <-  like_1st_term - like_2nd_term


    # loglikelihood_proposed_sum <-
    #   loglike(network, alpha = alpha_initial, dist_btwn_z = dist_btwn_z_prop)
    #
    z_prior_like <- 0
    z_prior_sigma = Diag.matrix(n_dimen, v = 1 / OMEGAS)
    for (group in G_index) {
      z_prior_like <- z_prior_like +
        sum(
          Rfast::dmvnorm(
            z_proposed[Ki == group, , drop = FALSE],
            mu = cluster_mean_initial[group,],
            sigma = z_prior_sigma/(psi[group]),
            logged = T
          )
        ) -
        sum(
          Rfast::dmvnorm(
            z_initial[Ki == group, , drop = FALSE],
            mu = cluster_mean_initial[group,],
            sigma = z_prior_sigma/(psi[group]),
            logged = T
          )
        )
    }

    lhr <-
      loglikelihood_proposed_sum - loglikelihood_initial_sum + # likelihood
      z_prior_like # priors

    if (log(runif(1)) < lhr) {
      z_initial <- z_proposed
      like_1st_term_dist <- like_1st_term_dist_p
      lower_tri_dist <- lower_tri_dist_p
      loglikelihood_initial_sum <- loglikelihood_proposed_sum
      dist_btwn_z_ini <- dist_btwn_z_prop
      acz_allow = TRUE
    }

    # For label switching
    if(s < burnin & loglikelihood_initial_sum > burnin_ref_like) {
      burnin_ref_like = loglikelihood_initial_sum
      burnin_ref_cluster_mean = cluster_mean_initial
    }

    # M-H for alpha --------------------------------------------------------------

    logit_p_numerator <-
      exp(alpha_initial - lower_tri_dist) # numerator component for logit function

    # Calculate parameters for the informed alpha proposal
    alpha_proposed_variance0 <-
      1 / (sum(logit_p_numerator / (1 + logit_p_numerator) ^ 2) +
             1 / alpha_prior_sd ^ 2)
    alpha_proposed_mean0 <- alpha_initial +
      alpha_proposed_variance0 *
      (sum(network) - 2 * sum(( # 2* sum is upper and lower
        logit_p_numerator / (1 + logit_p_numerator)))
       + (1 / alpha_prior_sd ^ 2) * (alpha_prior_mean - alpha_initial))

    alpha_proposed <- rnorm(1,
                            alpha_proposed_mean0,
                            sd = alpha_p_step * sqrt(alpha_proposed_variance0)) # normal proposed distribution

    logit_p_numerator <-
      exp(alpha_proposed - lower_tri_dist) # numerator component for logit function

    # calculate new hyperparameters
    alpha_proposed_variance <-
      1 / (sum(logit_p_numerator / (1 + logit_p_numerator) ^ 2) +
             1 / alpha_prior_sd ^ 2)
    alpha_proposed_mean <- alpha_proposed +
      alpha_proposed_variance *
      (
        sum(network) - 2 * sum((
          logit_p_numerator / (1 + logit_p_numerator)
        )) +
          (1 / alpha_prior_sd ^ 2) * (alpha_prior_mean - alpha_proposed)
      )

    # log likelihood calculation
    like_1st_term = sum(network) * alpha_proposed - like_1st_term_dist


    like_2nd_term = sum(log1p(exp(
      alpha_proposed - lower_tri_dist
    ))) * 2 # + (n_obs * log(2))
    loglikelihood_proposed_sum <-  like_1st_term - like_2nd_term


    # loglikelihood_proposed_sum <-
    #   loglike(network, alpha = alpha_proposed, dist_btwn_z = dist_btwn_z_ini)

    # print(c(s, alpha_initial, alpha_proposed, lhr, loglikelihood_proposed_sum, loglikelihood_initial_sum))
    if(loglikelihood_proposed_sum == -Inf) {
      browser()
    }
    lhr <-
      loglikelihood_proposed_sum - loglikelihood_initial_sum + # likelihood
      dnorm(alpha_proposed, alpha_prior_mean, alpha_prior_sd, log = TRUE) -
      dnorm(alpha_initial, alpha_prior_mean, alpha_prior_sd, log = TRUE) + # priors
      dnorm(
        alpha_initial,
        mean = alpha_proposed_mean,
        sd = sqrt(alpha_proposed_variance),
        log = TRUE
      ) - # hasting part, when proposed is symmetric this equals 1
      dnorm(
        alpha_proposed,
        mean = alpha_proposed_mean0,
        sd = alpha_p_step * sqrt(alpha_proposed_variance0),
        log = TRUE
      )

    if (log(runif(1)) < lhr) {
      alpha_initial <- alpha_proposed
      loglikelihood_initial_sum <- loglikelihood_proposed_sum
      aca_allow <- TRUE

      alpha_proposed_mean0 <- alpha_proposed_mean
      alpha_proposed_variance0 <- alpha_proposed_variance
    }


    # M-H for nu --------------------------------------------------------------

    nu_proposed <- rgamma(1, shape = nu_shape_step, rate = nu_shape_step/nu) # gamma proposed distribution
    # browser()
    lhr_nu <-
      (nu_proposed-1)*sum(log(mix_prop)) + (nu_prior[1]-1)*log(nu_proposed) - nu_prior[2]*nu_proposed -
      ( (nu-1)*sum(log(mix_prop)) + (nu_prior[1]-1)*log(nu) - nu_prior[2]*nu ) + # likelihood
      dgamma(nu_proposed, shape = nu_prior[1], rate = nu_prior[2], log = TRUE) -
      dgamma(nu, shape = nu_prior[1], rate = nu_prior[2], log = TRUE) + # priors
      dgamma(
        nu,
        shape = nu_shape_step,
        rate =  nu_shape_step/nu_proposed,
        log = TRUE
      ) - # hasting part, when proposed is symmetric this equals 1
      dgamma(
        nu_proposed,
        shape = nu_shape_step,
        rate = nu_shape_step/nu,
        log = TRUE
      )

    if (log(runif(1)) < lhr_nu) {
      nu <- nu_proposed
      acnu_allow <- TRUE
    }


    # Gibbs for delta1.
    # browser()
    beta1_sum = c()
    for (group in G_index) {
      z_minus_mean =   eachrow(z_initial[Ki == group, , drop = FALSE],
                               cluster_mean_initial[group, , drop = FALSE],
                               oper = "-")
      z_minus_mean = z_minus_mean * psi[group]* z_minus_mean
      beta1_sum <- rbind(beta1_sum, colsums(z_minus_mean ) * (OMEGAS / DELTAS[1])) # only diagonal of the matrix maths
    }
    beta1_sum2 <- colsums(cluster_mean_initial *
                            cluster_mean_initial ) * (OMEGAS / DELTAS[1])
    # browser()
    DELTAS[1] <-
      rgamma(
        1,
        shape = ((n_obs +G)* n_dimen / 2) + a1,
        rate = b1 + (0.5) * sum(beta1_sum) + 0.5 *(1/cmean_scale)* sum(beta1_sum2)
      )
    OMEGAS <- cumprod(DELTAS)

    if (n_dimen > 1) {
      for (h in 2:n_dimen) {
        betal_sum = c()
        for (group in G_index) {
          z_minus_mean <- eachrow(z_initial[Ki == group, h:n_dimen, drop = F],
                                  cluster_mean_initial[group, h:n_dimen, drop = F],
                                  oper = "-")
          z_minus_mean = z_minus_mean * psi[group]* z_minus_mean
          betal_sum <- rbind(betal_sum, colsums(z_minus_mean) *
                               (OMEGAS[h:n_dimen] / DELTAS[h]))
        }

        betal_sum2 <- colsums(cluster_mean_initial[,h:n_dimen,drop=F] *
                                cluster_mean_initial[,h:n_dimen,drop=F]) *
          (OMEGAS[h:n_dimen] / DELTAS[h])
        DELTAS[h] <-
          rtrunc(
            1,
            spec = "gamma",
            a = 1,
            shape = ((n_obs + G) * (n_dimen - h + 1) / 2) + a2,
            rate = b2 + (0.5) * sum(betal_sum, na.rm = T) + (0.5) * (1/cmean_scale)*
              sum(betal_sum2, na.rm = T)
          )
        OMEGAS <- cumprod(DELTAS)


      }
    }

    if (s < burnin & burnin_max_like >  loglikelihood_initial_sum) {
      burnin_max_like <- loglikelihood_initial_sum
      burnin_pos <- z_initial
    }

    change_dim = 0
    var_prop = cumsum(1/OMEGAS)/sum(1/OMEGAS)
    adapt_prob = exp(- adapt_param[1] - adapt_param[2] * (s - burnin))
    if(runif(1) < adapt_prob) {
      adapt_iter = adapt_iter + 1
      if(n_dimen == 1) { # decision criteria to increase when d = 1
        dim1_check = sum(abs(z_initial[,1] - mean(z_initial[,1])) >
                           qnorm(0.95, lower.tail = F)) / n_obs
        ifelse(dim1_check > (dim_threshold[3]*0.05),
               change_dim <- 1, change_dim <- 0) # change based on deviation
      } else if(var_prop[n_dimen - 1] >= dim_threshold[1]) {
        # decision criteria to decrease dimension based on min cum. prop. var.
        change_dim = -1
      } else if((1/DELTAS)[n_dimen] > dim_threshold[2] ) {
        # decision to increase dimension if previous dimension is very similar
        change_dim = 1
      }
    }


    if(change_dim==0 | s <= burnin) {
    } else if(change_dim == 1){
      n_dimen = n_dimen + 1
      DELTAS = c(DELTAS, rtrunc(1, spec="gamma", a = 1,
                                shape = dim2_prior[1], rate = dim2_prior[2]))
      OMEGAS = cumprod(DELTAS)

      cluster_mean_initial = cbind(cluster_mean_initial,
                                   rnorm(G, sd = 1/sqrt(OMEGAS[n_dimen])))
      z_initial  = cbind(z_initial,
                         rnorm(n_obs, mean = cluster_mean_initial[Ki,n_dimen],
                               sd = 1/sqrt(OMEGAS[n_dimen])))

      dist_btwn_z_ini <- Dist(z_initial, square=T)

      # log likelihood calculation
      like_1st_term_dist = sum(dist_btwn_z_ini * network)
      like_1st_term = sum(network) * alpha_initial - like_1st_term_dist

      lower_tri_dist = lower_tri(dist_btwn_z_ini)
      like_2nd_term = sum(log1p(exp(
        alpha_initial - lower_tri_dist
      ))) * 2 # + (n_obs * log(2))
      loglikelihood_initial_sum <-  like_1st_term - like_2nd_term


    } else if(change_dim == -1){
      if(n_dimen==1) next
      n_dimen = which(var_prop >= dim_threshold[1])[1]
      # n_dimen=n_dimen-1
      DELTAS = DELTAS[1:n_dimen, drop=F]
      OMEGAS = cumprod(DELTAS)
      z_initial  = z_initial[,1:n_dimen, drop=F]
      cluster_mean_initial = cluster_mean_initial[,1:n_dimen, drop=F]
      # n_dimen = n_dimen - 1

      dist_btwn_z_ini <- Dist(z_initial, square=T)

      # log likelihood calculation
      like_1st_term_dist = sum(dist_btwn_z_ini * network)
      like_1st_term = sum(network) * alpha_initial - like_1st_term_dist

      lower_tri_dist = lower_tri(dist_btwn_z_ini)
      like_2nd_term = sum(log1p(exp(
        alpha_initial - lower_tri_dist
      ))) * 2 # + (n_obs * log(2))
      loglikelihood_initial_sum <-  like_1st_term - like_2nd_term


    }

    if (s_allow) {
      MIX_PROP_STORE[s_thin,] <- mix_prop
      PKI_STORE[, , s_thin] <- PKi_norm
      KI_STORE[s_thin,] <- Ki
      LIKE[s_thin,] <- loglikelihood_initial_sum
      ALPHA[s_thin,] <- alpha_initial
      LATENT_POS[, 1:n_dimen, s_thin] <- z_initial
      NU_STORE[s_thin,] <- nu
      OMEGAS_STORE[s_thin, 1:n_dimen] <- OMEGAS
      VARIANCES_STORE[s_thin, 1:n_dimen] <- 1/OMEGAS
      DELTAS_STORE[s_thin, 1:n_dimen] <- DELTAS
      CLUSTER_MEAN_STORE[, 1:n_dimen, s_thin] <- cluster_mean_initial
      PSI_STORE[s_thin,] <- psi

      if(acz_allow) {
        acz[s_thin,1] <- 1
      }
      if(aca_allow) {
        aca[s_thin,1] <- 1
      }
      if(acnu_allow) {
        acnu[s_thin,1] <- 1
      }

      if((sum(acz[1:s_thin,])+1)/s_thin < 0.01){
        return(print("Acceptance rate of z is too low"))
      }
      if((sum(aca[1:s_thin,])+1)/s_thin < 0.01){
        return(print("Acceptance rate of alpha is too low"))
      }
      if((sum(acnu[1:s_thin,])+1)/s_thin < 0.01){
        return(print("Acceptance rate of nu is too low"))
      }

      new_var_prop = cumsum(1/OMEGAS)/sum(1/OMEGAS, na.rm = T)
      temp_dim = which(new_var_prop >= 0.8)
      if(length(temp_dim)>1) {
        new_dim = temp_dim[1]
      } else {
        new_dim = temp_dim
      }

      iter_d[s_thin,1] = new_dim
      iter_G[s_thin,1] = sum(Ki_count > 0)
    }

    # print(n_dimen)
    # Progress bar
    if ((s - 1) %% (iter / 10) == 0) {
      cat(paste0(round(s * 100 / iter), "% "))
    }
  } # End chain

  # Saving the last iteration if there is remainder due to thinning
  if (s > burnin & ((s - burnin) %% thin) != 0) {
    MIX_PROP_STORE[iter_thin,] <- mix_prop
    PKI_STORE[, , iter_thin] <- PKi_norm
    KI_STORE[iter_thin,] <- Ki
    LIKE[iter_thin,] <- loglikelihood_initial_sum
    ALPHA[iter_thin,] <- alpha_initial
    LATENT_POS[, 1:n_dimen, iter_thin] <- z_initial
    NU_STORE[iter_thin,] <- nu
    OMEGAS_STORE[iter_thin, 1:n_dimen] <- OMEGAS
    VARIANCES_STORE[iter_thin, 1:n_dimen] <- 1/OMEGAS
    DELTAS_STORE[iter_thin, 1:n_dimen] <- DELTAS
    CLUSTER_MEAN_STORE[, 1:n_dimen, iter_thin] <- cluster_mean_initial
    PSI_STORE[iter_thin,] <- psi

    new_var_prop = cumsum(1/OMEGAS)/sum(1/OMEGAS, na.rm = T)
    temp_dim = which(new_var_prop >= 0.8)
    if(length(temp_dim)>1) {
      new_dim = temp_dim[1]
    } else {
      new_dim = temp_dim
    }

    iter_d[iter_thin,1] = new_dim
    iter_G[iter_thin,1] = sum(Ki_count > 0)
  }

  # Procrustes rotation
  for (s in 1:iter_thin) {
    n_dimen = iter_d[s]
    min_dim = min(n_dimen, dim(burnin_pos)[2])
    if(n_dimen == 1) {
    } else if(n_dimen > 1) {
      LATENT_POS[,1:min_dim, s] <-
        proc.crr(burnin_pos[,1:min_dim, drop=FALSE],
                 LATENT_POS[,1:min_dim, s], k = min_dim)
    }
  }


  initialisation <- list(
    network = network,
    n_dimen = n_dimen0,
    n_cluster = G,
    iter = iter,
    burnin = burnin,
    thin = thin,
    step_size = step_size,
    initial_adjust = initial_adjust,
    alpha_prior = alpha_prior,
    dim1_prior = dim1_prior,
    dim2_prior = dim2_prior,
    seed = seed,
    alpha = alpha_initial0,
    positions = z_initial0,
    like = like0,
    cluster = Ki0,
    omegas = OMEGAS0,
    deltas = DELTAS0,
    nu_prior = nu_prior,
    dim_threshold = dim_threshold,
    adapt_param = adapt_param
  )

  cat("\n")

  final_result <- list(
    alpha = ALPHA,
    positions = LATENT_POS,
    omegas = OMEGAS_STORE,
    variances = VARIANCES_STORE,
    deltas = DELTAS_STORE,
    nu = NU_STORE,
    psi = PSI_STORE,
    like = LIKE,
    aca = aca,
    acz = acz,
    acnu = acnu,
    initialisation = initialisation,
    cluster_mean = CLUSTER_MEAN_STORE,
    mix_prop = MIX_PROP_STORE,
    cluster = KI_STORE,
    cluster_prob = PKI_STORE,
    burnin_ref_like = burnin_max_like,
    burnin_ref_cluster_mean = burnin_ref_cluster_mean,
    iter_d = iter_d,
    iter_G = iter_G,
    adapt_iter = adapt_iter
  )
  end_time <- Sys.time()
  total_time = end_time - start_time
  final_result$time <- total_time

  class(final_result) <- "LSPCM"
  invisible(final_result)
}
