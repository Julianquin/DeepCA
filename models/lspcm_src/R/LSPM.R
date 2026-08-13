#' Latent Shrinkage Position Model
#'
#' A Bayesian nonparametric method of fitting a latent position model on
#' network data. LSPM is able to indicate the number of non-zero variance latent
#' dimension required to describe the network by using a shrinkage prior called
#' the multiplicative gamma process. Truncated gamma distributions are used to
#' ensure shrinkage across higher dimensions.
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
#' @param family The linkage type used for the model (logit for binary,
#' Poisson for count)
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
#' @param initial_pos_scale_seq A vector of length two adjusting the initial
#' position variance by dimension by a sequence of numbers
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
#'
#' @details
#' The latent shrinkage position model (LSPM) facilitates automatic estimation
#' of the dimensionality of the latent space from the data by extending the
#' latent space model using a shrinkage prior called the multiplicative
#' truncated gamma process (MTGP). The model is built within a nonparametric
#' Bayesian framework which allows for infinitely many dimensions. The key idea
#' is to have the variance of the latent positions on each dimension to become
#' increasingly small as the number of dimensions tends to infinity. With the
#' LSPM, the need to select a model selection criterion is obviated. The LSPM
#' also retains the ease of interpretability of the model similar to the
#' original LPM. Furthermore, there is a natural order to the importance of the
#' latent dimensions due to the nature of the shrinkage prior used. LSPM uses a
#' Metropolis-within-Gibbs sampler to sample the posterior distribution.
#' Truncated gamma distributions are used for the MGP on dimensions 2 and higher
#' to ensure shrinkage.
#'
#' @return A list containing the computed thinned and after burn-in MCMC chains
#' for the different parameters and its initial settings
#' \item{alpha}{The overall connection level, \eqn{\alpha}}
#' \item{mcmc_chain}{The MCMC chains containing the latent positions, their
#' respective precisions, variances, and dimension shrinkage strengths.
#' \describe{
#'  \item{\strong{positions}: }{The \eqn{n x p} matrix of latent positions,
#'  \eqn{Z}}
#'  \item{\strong{omegas}: }{A vector of latent position precisions,
#'  \eqn{\omega}}
#'  \item{\strong{variances}: }{A vector of the latent position variances,
#'  \eqn{\omega^{-1}}}
#'  \item{\strong{deltas}: }{A vector of the shrinkage strengths, \eqn{\delta}}
#' }}
#' \item{like}{The likelihood of the latent positions}
#' \item{aca}{The acceptance rate of the \eqn{\alpha} parameter}
#' \item{acz}{The acceptance rate of the latent positions}
#' \item{initalisation}{The initial setting used to configure the algorithm}
#' \item{burnin_ref_like}{The likelihood of the reference positions used from
#' the burn in period}
#' \item{iter_d}{The active dimension across the iterations}
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
#'
#'
#' @references
#' Gwee X.Y., Gormley I.C., Fop M. (2022). A latent shrinkage position model for
#' network data. \emph{TBA journal}
#'
#' @family functions
#'
#'
#' @examples
#' # Simulate a binary network with 2 true dimensions
#' sampleMTGP_2D <- networkMTGP(n = 50, alpha = 1, deltas = c(0.5,1.1),
#' type = "binary", seed = 11)
#'
#' # Fitting a logistic LSPM.
#' sampleLSPM <- LSPM(sampleMTGP_2D$network, family = "logit",
#' step_size = c(3, 1.2), iter = 2e4, burnin = 1000, thin = 200)
#'
#' plot(sampleLSPM) # to plot the proportion of posterior dimension
#'
#' # Plot the 2D latent dimension scatterplot
#' plot(sampleLSPM, parameter = "positions", n_dimen = 2)
#'
#' # Plot the shrinkage strengths where the active dimension is 2
#' plot(sampleLSPM, parameter = 'deltas', n_dimen = 2)
#'
#' # Plot the trace plot conditioned on 2 active dimension to check convergence
#' diagLSPM(sampleLSPM, n_dimen = 2)
#'
#' #' # Perform posterior predictive checking
#' predLSPM <- predcheck(10, n_dimen = 2, LSPM_object = sampleLSPM)
#'
#' # Plot the posterior predictive checking
#' predPlot(predLSPM)
#'
#'
#' ## Fitting LSPM on Lazega Lawyer network
#' coworkerLSPM <- LSPM(lawyers.coworkers, step_size = c(3,2), iter = 2e4,
#' burnin = 2000, thin = 100)
#' plot(coworkerLSPM, parameter='deltas', n_dimen = 3)
#'
LSPM <- function(network,
                 n_dimen,
                 iter = 1e5,
                 burnin = 1e4,
                 thin = 1e3,
                 step_size = c(3, 3),
                 family = c("logit", "Poisson"),
                 initial_adjust = c(1, 0.01),
                 alpha_prior = c(0, 3),
                 dim1_prior = c(2, 1),
                 dim2_prior = c(3, 1),
                 seed,
                 initial_pos_scale_seq = c(1, 1),
                 dim_threshold = c(0.9,0.9,5),
                 adapt_param = c(3.2, 3e-5)) {
  if (!is.matrix(network)) {
    return(print("Network is not an adjacency matrix"))
  }

  linkdist <- match.arg(family)
  linkdist <- tolower(linkdist)

  if(linkdist == "logit" & sum(network>1) != 0){
    return(print("Network is not binary valued only"))
  } else if(linkdist == "poisson" & sum(network>1) == 0) {
    return(print("Network seem to only contains counts <= 1"))
  }

  if (!missing(seed)) {
    set.seed(seed) # set seed if exists
  } else {
    seed <- sample(1:1e5, 1) # sample a seed from 1 to 10,000
  }
  n_obs <- dim(network)[1]

  # Step Sizes for proposal distribution
  alpha_p_step <- step_size[1]
  latent_step <- step_size[2]
  z_proposed_variance <-
    (latent_step / nrow(network)) ^ 2 # squared because cov matrix used

  # Alpha hyperparameters
  alpha_prior_mean <- alpha_prior[1]
  alpha_prior_sd <- alpha_prior[2]

  # Delta hyperparam
  a1 <- dim1_prior[1]
  a2 <- dim2_prior[1]
  b1 <- dim1_prior[2]
  b2 <- dim2_prior[2]

  # Initialising latent positions
  limit_max_GD = 20 # limiting max geodesic distance

  # Initialise latent positions
  max_dist <- # noting maximum geodesic distance of network
    max(geodist(network, inf.replace = 0, count.paths = FALSE)$gdist)

  ifelse(max_dist > limit_max_GD, limit_max_GD, max_dist) # limiting max G. D.

  geo_dist <- # geodesic distance
    geodist(network,
            inf.replace = max_dist + floor(max_dist / 2),
            count.paths = FALSE)$gdist
  geo_dist[geo_dist > limit_max_GD] = limit_max_GD # replace higher than limit

  # multidimensional scaling
  mds_result <-
    cmdscale(geo_dist, k = ceiling((n_obs-1)/2), eig = T)

  if(missing(n_dimen)){
    mds_eig = mds_result$eig # extract eigenvalue
    mds_eig_positive = mds_eig[mds_eig > 0] # only consider positive eigenvalues
    mds_var_prop = mds_eig_positive / sum(mds_eig_positive) # prop. of variance

    # cluster to obtain optimal dim., informative dim. usually separate from non
    n_dimen0 = n_dimen = min(table(kmeans(mds_var_prop, centers = 2)$cluster))
  }
  z_initial0 <- mds_result$points[,1:n_dimen, drop=F]

  n_dimen0 = n_dimen

  if (n_dimen == 1) {
    # introduce noise for 1d
    z_initial0 <- z_initial0 +
      rnorm(n_obs, sd = sqrt(var(z_initial0) * initial_adjust[2]))
  } else {
    # introduce noise for 2d+
    z_initial0 <- z_initial0 +
      rmvnorm(n_obs,
              sigma = diag(apply(z_initial0, 2, var) * initial_adjust[2]))
  }

  # scaling the initial position by dimensions
  z_initial0 <- sweep(z_initial0, 2,
                      seq(initial_pos_scale_seq[1], initial_pos_scale_seq[2],
                          length.out = dim(z_initial0)[2]),
                      "*"
  )

  z_initial0 <-
    scale(z_initial0, scale = FALSE) # center the initial positions


  dist_btwn_z_ini <-
    dist(z_initial0,
         diag = TRUE,
         upper = TRUE,
         method = "euclidean") ^ 2 # squared Euclidean distance

  if(linkdist == "logit") {
    # Logistic regression initialisation
    suppressWarnings(logis_coeff <-
                       netlogit(network, as.matrix(dist_btwn_z_ini),
                                reps = 1)$coefficients)
  } else if(linkdist == "poisson") {
    # Poisson regression initialisation
    suppressWarnings(logis_coeff <-
                       netpois(network, as.matrix(dist_btwn_z_ini)))
  }

  # Initialise and rescale alpha
  alpha_initial0 <- alpha_initial <- logis_coeff[1] * initial_adjust[1]

  # Initialise and rescale Z by coefficients
  burnin_pos <- z_initial0 <- z_initial <- z_initial0 * abs(logis_coeff[2])^(.5)

  dist_btwn_z_ini <- dist(z_initial0,
                          diag = TRUE, upper = TRUE, method = "euclidean") ^ 2


  like0 <- burnin_max_like <- burnin_ref_like <- loglikelihood_initial_sum <-
    loglike(network, alpha = alpha_initial0,
            dist_btwn_z = dist_btwn_z_ini, family = linkdist)

  iter_thin = ceiling((iter - burnin) / thin)
  # Obtain precision from starting latent position
  OMEGAS <- OMEGAS0 <- 1 / apply(z_initial0, 2, var)

  # Calculate delta by dividing omegas with previous dimension
  delta_sub <- 1.01 # the sub value when delta < 1
  DELTAS <- rep(1, n_dimen)
  DELTAS[1] <- OMEGAS[1]
  for (i in 2:n_dimen) {
    delta_temp <- OMEGAS[i] / OMEGAS[i - 1]
    ifelse(delta_temp < 1, DELTAS[i] <-
             delta_sub, DELTAS[i] <- delta_temp) # check >1
  }
  OMEGAS0 <- OMEGAS <- cumprod(DELTAS)
  DELTAS0 <- DELTAS

  adapt_iter = 0
  # empty matrix for alpha & likelihood
  ALPHA <- LIKE <- aca <- acz <- iter_d <- matrix(0, nrow = iter_thin, ncol = 1)
  available_d = n_dimen
  mcmc_chain = list()

  mcmc_chain[[paste0(n_dimen, "D")]] =
    list(
      positions = array(NA, dim = c(n_obs, n_dimen, iter_thin)),
      omegas = matrix(NA, nrow = iter_thin, ncol = n_dimen),
      variances = matrix(NA, nrow = iter_thin, ncol = n_dimen),
      deltas = matrix(NA, nrow = iter_thin, ncol = n_dimen)
    )

  # numerator component for logit function
  logit_p_numerator <- exp(alpha_initial - dist_btwn_z_ini)

  if(linkdist == "logit") {
    # Calculate parameters for the informed alpha proposal
    alpha_proposed_variance0 <-
      1 / (sum(logit_p_numerator / (1 + logit_p_numerator) ^ 2) +
             1 / alpha_prior_sd ^ 2)
    alpha_proposed_mean0 <- alpha_initial +
      alpha_proposed_variance0 *
      (sum(network - as.matrix(logit_p_numerator / (1 + logit_p_numerator)))
       + (1 / alpha_prior_sd ^ 2) * (alpha_prior_mean - alpha_initial))
  } else if(linkdist == "poisson") {
    # Calculate parameters for the informed alpha proposal
    alpha_proposed_variance0 <-
      1 / (sum(as.matrix(logit_p_numerator)) +  1 / alpha_prior_sd ^ 2)
    alpha_proposed_mean0 <- alpha_initial +
      alpha_proposed_variance0 *
      (sum(network - as.matrix(logit_p_numerator)) +
         (1 / alpha_prior_sd ^ 2) * (alpha_prior_mean - alpha_initial))
  }

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
    aca_allow = acz_allow = FALSE

    if (!(n_dimen %in% available_d) & s_allow) {
      mcmc_chain[[paste0(n_dimen, "D")]] =
        list(
          positions = array(NA, dim = c(n_obs, n_dimen, iter_thin)),
          omegas = matrix(NA, nrow = iter_thin, ncol = n_dimen),
          variances = matrix(NA, nrow = iter_thin, ncol = n_dimen),
          deltas = matrix(NA, nrow = iter_thin, ncol = n_dimen)
        )
      available_d = c(available_d, n_dimen)
    }

    # M-H for Z --------------------------------------------------------------
    if (n_dimen == 1) {
      # 1d proposed distribution
      z_proposed <-
        z_initial + rnorm(n_obs, sd = sqrt(z_proposed_variance / OMEGAS))
    } else {
      # 2d+ proposed distribution
      z_proposed <-
        z_initial + rmvnorm(n_obs, sigma = diag(z_proposed_variance / OMEGAS))
    }

    dist_btwn_z_prop <- dist(z_proposed,
                             diag = TRUE, upper = TRUE,
                             method = "euclidean") ^2


    loglikelihood_proposed_sum <-
      loglike(network, alpha = alpha_initial,
              dist_btwn_z = dist_btwn_z_prop, family = linkdist)


    lhr <-
      loglikelihood_proposed_sum - loglikelihood_initial_sum + # likelihood
      latent_priors(z_proposed, OMEGAS) - latent_priors(z_initial, OMEGAS) # priors

    if (log(runif(1)) < lhr) {
      z_initial <- z_proposed
      loglikelihood_initial_sum <- loglikelihood_proposed_sum
      dist_btwn_z_ini <- dist_btwn_z_prop
      acz_allow = TRUE
    }

    # For burnin reference
    if(s <= burnin & loglikelihood_initial_sum > burnin_ref_like) {
      burnin_ref_like = loglikelihood_initial_sum
    }

    # M-H for alpha -----------------------------------------------------------

    # numerator component for logit function
    logit_p_numerator <- exp(alpha_initial - dist_btwn_z_ini)

    if(linkdist == "logit") {
      # Calculate parameters for the informed alpha proposal
      alpha_proposed_variance0 <-
        1 / (sum(logit_p_numerator / (1 + logit_p_numerator) ^ 2) +
               1 / alpha_prior_sd ^ 2)
      alpha_proposed_mean0 <- alpha_initial +
        alpha_proposed_variance0 *
        (sum(network - as.matrix(logit_p_numerator / (1 + logit_p_numerator)))
         + (1 / alpha_prior_sd ^ 2) * (alpha_prior_mean - alpha_initial))

      # normal proposal distribution
      alpha_proposed <-
        rnorm(1, alpha_proposed_mean0,
              sd = alpha_p_step * sqrt(alpha_proposed_variance0))

      # numerator component for logit function
      logit_p_numerator <- exp(alpha_proposed - dist_btwn_z_ini)

      # calculate new hyperparameters
      alpha_proposed_variance <-
        1 / (sum(logit_p_numerator / (1 + logit_p_numerator) ^ 2) +
               1 / alpha_prior_sd ^ 2)
      alpha_proposed_mean <- alpha_proposed +
        alpha_proposed_variance *
        (sum(network - as.matrix(logit_p_numerator / (1 + logit_p_numerator))) +
           (1 / alpha_prior_sd ^ 2) * (alpha_prior_mean - alpha_proposed))

    } else if(linkdist == "poisson") {
      # Calculate parameters for the informed alpha proposal
      alpha_proposed_variance0 <-
        1 / (sum(as.matrix(logit_p_numerator)) +  1 / alpha_prior_sd ^ 2)
      alpha_proposed_mean0 <- alpha_initial +
        alpha_proposed_variance0 *
        (sum(network - as.matrix(logit_p_numerator )) +
           (1 / alpha_prior_sd ^ 2) * (alpha_prior_mean - alpha_initial))

      # normal proposal distribution
      alpha_proposed <-
        rnorm(1, alpha_proposed_mean0,
              sd = alpha_p_step * sqrt(alpha_proposed_variance0))

      # numerator component for logit function
      logit_p_numerator <- exp(alpha_proposed - dist_btwn_z_ini)

      # calculate new hyperparameters
      alpha_proposed_variance <-
        1 / (sum(as.matrix(logit_p_numerator)) +  1 / alpha_prior_sd ^ 2)
      alpha_proposed_mean <- alpha_proposed +
        alpha_proposed_variance *
        (sum(network - as.matrix(logit_p_numerator)) +
           (1 / alpha_prior_sd ^ 2) * (alpha_prior_mean - alpha_proposed))
    }

    loglikelihood_proposed_sum <-
      loglike(network, alpha = alpha_proposed,
              dist_btwn_z = dist_btwn_z_ini, family = linkdist)

    lhr <-
      loglikelihood_proposed_sum - loglikelihood_initial_sum + # likelihood
      dnorm(alpha_proposed, alpha_prior_mean, alpha_prior_sd, log = TRUE) -
      dnorm(alpha_initial, alpha_prior_mean, alpha_prior_sd, log = TRUE) + # priors
      dnorm(alpha_initial,
            mean = alpha_proposed_mean,
            sd = sqrt(alpha_proposed_variance),
            log = TRUE) - # hasting part, for symmetric proposal, this equals 1
      dnorm(alpha_proposed,
            mean = alpha_proposed_mean0,
            sd = alpha_p_step * sqrt(alpha_proposed_variance0),
            log = TRUE)

    if (log(runif(1)) < lhr) {
      alpha_initial <- alpha_proposed
      loglikelihood_initial_sum <- loglikelihood_proposed_sum
      aca_allow = TRUE

      alpha_proposed_mean0 <- alpha_proposed_mean
      alpha_proposed_variance0 <- alpha_proposed_variance
    }

    # Gibbs for delta1.
    beta1_sum <- matrix(nrow = n_obs, ncol = n_dimen)
    for (l in 1:n_dimen) {
      beta1_sum[, l] <- OMEGAS[l] / DELTAS[1] * (z_initial[, l]) ^ 2
    }

    DELTAS[1] <-
      rgamma(1,
             shape = (n_obs * n_dimen / 2) + a1,
             rate = b1 + (0.5) * sum(beta1_sum))
    OMEGAS <- cumprod(DELTAS)

    # Gibbs for delta > 2
    if (n_dimen > 1) {
      for (h in 2:n_dimen) {
        betal_sum <- matrix(nrow = n_obs, ncol = n_dimen)
        for (l in h:n_dimen) {
          betal_sum[, l] <- OMEGAS[l] / DELTAS[h] * (z_initial[, l]) ^ 2
        }

        DELTAS[h] <- rtrunc(
          1,
          spec = "gamma",
          a = 1,
          shape = (n_obs * (n_dimen - h + 1) / 2) + a2,
          rate = (b2 + (0.5) * sum(betal_sum, na.rm = T))
        )
        OMEGAS <- cumprod(DELTAS)
      }
    }

    if (s <= burnin & burnin_max_like <  loglikelihood_initial_sum) {
      burnin_max_like <- loglikelihood_initial_sum
      burnin_pos <- z_initial
    }

    if (s_allow) {
      LIKE[s_thin,] <- loglikelihood_initial_sum
      ALPHA[s_thin,] <- alpha_initial
      mcmc_chain[[paste0(n_dimen, "D")]]$positions[, , s_thin] <- z_initial
      mcmc_chain[[paste0(n_dimen, "D")]]$omegas[s_thin,] <- OMEGAS
      mcmc_chain[[paste0(n_dimen, "D")]]$variances[s_thin,] <- 1/OMEGAS
      mcmc_chain[[paste0(n_dimen, "D")]]$deltas[s_thin,] <- DELTAS
      if(acz_allow) {
        acz[s_thin,1] <- 1
      }
      if(aca_allow) {
        aca[s_thin,1] <- 1
      }

      iter_d[s_thin,1] = n_dimen
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
      z_initial  = cbind(z_initial, rnorm(n_obs, sd = 1/sqrt(OMEGAS[n_dimen])))

      dist_btwn_z_ini <- dist(z_initial,
                              diag = TRUE, upper = TRUE,
                              method = "euclidean") ^2


      loglikelihood_initial_sum <-
        loglike(network, alpha = alpha_initial,
                dist_btwn_z = dist_btwn_z_ini, family = linkdist)


    } else if(change_dim == -1){
      if(n_dimen==1) next
      n_dimen = which(var_prop >= dim_threshold[1])[1]
      DELTAS = DELTAS[1:n_dimen, drop=F]
      OMEGAS = cumprod(DELTAS)
      z_initial  = z_initial[,1:n_dimen, drop=F]
      # n_dimen = n_dimen - 1

      dist_btwn_z_ini <- dist(z_initial,
                              diag = TRUE, upper = TRUE,
                              method = "euclidean") ^2


      loglikelihood_initial_sum <-
        loglike(network, alpha = alpha_initial,
                dist_btwn_z = dist_btwn_z_ini, family = linkdist)


    }

    # Progress bar
    if ((s - 1) %% (iter / 10) == 0) {
      cat(paste0(round(s * 100 / iter), "% "))
    }

  } # End chain

  # Saving the last iteration if there is remainder due to thinning
  if (s > burnin & ((s - burnin) %% thin) != 0) {
    LIKE[iter_thin,] <- loglikelihood_initial_sum
    ALPHA[iter_thin,] <- alpha_initial
    mcmc_chain[[paste0(n_dimen, "D")]]$positions[, , iter_thin] <- z_initial
    mcmc_chain[[paste0(n_dimen, "D")]]$omegas[iter_thin,] <- OMEGAS
    mcmc_chain[[paste0(n_dimen, "D")]]$variances[iter_thin,] <- 1/OMEGAS
    mcmc_chain[[paste0(n_dimen, "D")]]$deltas[iter_thin,] <- DELTAS
    if(acz_allow) {
      acz[iter_thin,1] <- 1
    }
    if(aca_allow) {
      aca[iter_thin,1] <- 1
    }

    iter_d[iter_thin,1] = n_dimen
  }

  # Procrustes rotation
  if (n_dimen > 1) {
    for (s in 1:iter_thin) {
      n_dimen = iter_d[s]
      min_dim = min(n_dimen, dim(burnin_pos)[2])
      mcmc_chain[[paste0(n_dimen, "D")]]$positions[,1:min_dim, s] <-
        proc.crr(burnin_pos[,1:min_dim],
                 mcmc_chain[[paste0(n_dimen, "D")]]$positions[,1:min_dim, s],
                 k = min_dim)
    }
  }

  initialisation <- list(
    network = network,
    n_dimen = n_dimen0,
    iter = iter,
    burnin = burnin,
    thin = thin,
    step_size = step_size,
    initial_adjust = initial_adjust,
    alpha_prior = alpha_prior,
    dim1_prior = dim1_prior,
    dim2_prior = dim2_prior,
    seed = seed,
    initial_pos_scale_seq = initial_pos_scale_seq,
    alpha = alpha_initial0,
    positions = z_initial0,
    like = like0,
    omegas = OMEGAS0,
    deltas = DELTAS0,
    dim_threshold = dim_threshold,
    adapt_param = adapt_param
  )

  cat("\n")

  final_result <- list(
    alpha = ALPHA,
    mcmc_chain = mcmc_chain,
    like = LIKE,
    aca = aca,
    acz = acz,
    initialisation = initialisation,
    burnin_ref_like = burnin_max_like,
    iter_d = iter_d,
    adapt_iter = adapt_iter
  )
  class(final_result) <- "LSPM"
  invisible(final_result)
}
