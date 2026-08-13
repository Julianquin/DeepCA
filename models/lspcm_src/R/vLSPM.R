#' Variational inference algorithm for latent shrinkage position model
#'
#' @param network Adjacency matrix of a network data
#' @param n_dimen Number of fitted latent dimensions for the model, also known
#' as truncation level.
#' @param variance_prior A prior for the latent position variance
#' @param alpha_prior A vector of length two representing the hyperparameters on
#' the \eqn{\alpha} normal prior with the values representing mean and standard
#' deviation respectively
#' @param shrinkage_prior A vector of length two representing the shape
#' hyperparameters on the \eqn{\delta_1} gamma prior and the \eqn{\delta_h}
#' truncated gamma prior respectively
#' @param N_iter Total number of iteration allowed to run for the algorithm
#' @param tol The ELBO difference from previous iteration for stopping the
#' algorithm
#' @param initial_adjust A vector of length two representing how much adjustment
#' is made on the initial alpha and latent positions respectively. The
#' @param seed Specify how the random number generator should be initialized
#'
#' @return Returns a set of list containing the iterative results
#' @export
#'
#' @importFrom rootSolve multiroot
#' @importFrom stats glm optim
#' @importFrom utils combn
#'
#' @examples
#' # Simulate a binary network with 2 true dimensions
#' sampleMTGP_2D <- networkMTGP(n = 100, alpha = 1, deltas = c(0.5,1.1),
#' type = "binary", seed = 11)
#'
#' # Fitting a logistic LSPM.
#' sampleVLSPM <- vLSPM(sampleMTGP_2D$network, n_dimen = 5, N_iter=50)
#'
vLSPM <- function(network,
                  n_dimen = 2,
                  variance_prior = 1,
                  alpha_prior = c(0,2),
                  shrinkage_prior= c(2,3),
                  N_iter = 100,
                  tol = 0.01,
                  initial_adjust=c(1,0.05),
                  seed){

  start_time <- Sys.time()
  Y <-  network
  n_obs <- nrow(Y)
  mu_alpha_prior = alpha_prior[1]
  sigma_2_alpha_prior = alpha_prior[2]

  a1_v= n_obs*n_dimen/2 + shrinkage_prior[1]
  a2_v= n_obs*(n_dimen-2:n_dimen+1)/2 + shrinkage_prior[2]
  a_star <- c(a1_v, a2_v)

  results <- NULL
  results$mu_alpha <- results$likelihood <- results$sigma_2_alpha <- results$elbo <- matrix(NA, nrow = N_iter, ncol = 1)
  results$b <- results$deltas <- matrix(NA, nrow = N_iter, ncol = n_dimen)
  results$positions <- array(NA, dim = c(n_obs, n_dimen, N_iter))
  results$variances <- array(NA, dim = c(n_dimen, n_dimen, N_iter))

  # results$positions[,,1] <- cmdscale(as.dist(1 - Y), n_dimen)
  # browser()
  # Initialise latent positions
  max_dist <- # noting maximum geodesic distance of network
    max(geodist(Y, inf.replace = 0, count.paths = FALSE)$gdist)
  geo_dist <- # geodesic distance
    geodist(Y,
            inf.replace = max_dist + floor(max_dist / 4),
            count.paths = FALSE)$gdist

  # multidimensional scaling
  mds_result <-
    cmdscale(geo_dist, k = n_dimen, eig = T)
  # mds_result <- MASS::isoMDS(geo_dist, k=n_dimen)

  z_initial0 <- mds_result$points

  if (!missing(seed)) {
    set.seed(seed) # set seed if exists
  } else {
    seed <- sample(1:1e5, 1) # sample a seed from 1 to 10,000
    set.seed(seed)
  }
  if (n_dimen == 1) {
    # introduce noise for 1d
    z_initial0 <- z_initial0 +
      rnorm(n_obs, sd = sqrt(var(z_initial0) * initial_adjust))
  } else {
    # introduce noise for 2d+
    z_initial0 <- z_initial0 +
      rmvnorm(n_obs,
              sigma = diag(apply(z_initial0, 2, var) * initial_adjust[2]))
  }

  # results$positions[,,1] <- scale(mds_result$points, scale = F)
  results$positions[,,1] <- z_initial0

  # results$positions[,,1] <- true_pos
  # browser()
  VARIANCES <- apply(results$positions[,,1], 2, var)
  results$variances[,,1] <-  diag(VARIANCES)
  OMEGAS <- 1/VARIANCES

  # Calculate delta by dividing omegas with previous dimension
  delta_sub <- 1.01 # the sub value when delta < 1
  DELTAS <- rep(1, n_dimen)
  DELTAS[1] <- OMEGAS[1]
  for (i in 2:n_dimen) {
    delta_temp <- OMEGAS[i] / OMEGAS[i - 1]
    ifelse(delta_temp < 1, DELTAS[i] <-
             delta_sub, DELTAS[i] <- delta_temp) # check >1
  }
  OMEGAS <- cumprod(DELTAS)
  results$deltas[1,] <-  DELTAS

  b_star <- a_star/DELTAS
  results$b[1,] <- b_star
  # results$variances[,,1] <- diag(n_dimen)
  # results$variances[,,1] <- diag(sampleMTGP_3D_n100b$variances)

  results$mu_alpha[1,] <- (glm(c(Y) ~ c(as.matrix(dist(results$positions[,,1])^2)))$coefficients[1] ) *initial_adjust[1]
  # results$mu_alpha[1,] <- sampleMTGP_3D_n100b$alpha

  results$sigma_2_alpha[1,] <- sigma_2_alpha_prior
  results$likelihood[1,] <- 1

  # b_star <- a_star
  # results$deltas[1,] <- a_star/b_star

  iter <- 1

  SI4SigmaT <- solve(diag(nrow(results$variances[,,iter])) + 4 * results$variances[,,iter])
  dist_v <- dist(results$positions[,,iter] %*% chol(SI4SigmaT))^2
  results$elbo[1,] <- ELBO(results$likelihood[iter,],
                           results$sigma_2_alpha[iter,],
                           results$mu_alpha[iter,],
                           SI4SigmaT, Y, dist_v,
                           mu_alpha_prior,
                           sigma_2_alpha_prior, variance_prior^2,
                           results$variances[,,iter],
                           results$positions[,,iter])

  dif <- 1


  while (iter < N_iter & abs(dif) > tol) {
    # while (iter < N_iter ) {
    # browser()
    results$mu_alpha[iter+1,] <- upXiT(results$sigma_2_alpha[iter,],
                                       results$mu_alpha[iter,],
                                       SI4SigmaT,
                                       Y,
                                       dist_v,
                                       mu_alpha_prior,
                                       sigma_2_alpha_prior)

    results$sigma_2_alpha[iter+1,] <- upPsi2T(results$sigma_2_alpha[iter,],
                                              results$mu_alpha[iter+1,],
                                              SI4SigmaT,
                                              dist_v,
                                              sigma_2_alpha_prior)

    # results$variances[,,iter+1] <- omega_star(results$sigma_2_alpha[iter+1,],
    #                                           results$mu_alpha[iter+1,],
    #                                           SI4SigmaT,
    #                                           Y,
    #                                           dist_v,
    #                                           results$positions[,,iter],
    #                                           variance_prior^2)

    results$positions[,,iter+1] <-  upZT(results$sigma_2_alpha[iter+1,],
                                         results$mu_alpha[iter+1,],
                                         # results$variances[,,iter+1],
                                         results$variances[,,iter],
                                         # diag(1/cumprod(results$deltas[iter,])),
                                         results$positions[,,iter],
                                         Y, variance_prior^2)

    # results$variances[,,iter] <- omega_star(results$sigma_2_alpha[iter+1,],
    #                                           results$mu_alpha[iter+1,],
    #                                           SI4SigmaT,
    #                                           Y,
    #                                           dist_v,
    #                                           results$positions[,,iter],
    #                                           variance_prior^2)

    # results$positions[,,iter+1] <- results$positions[,,iter]
    # browser()
    results$deltas[iter+1, ] <- results$deltas[iter, ]
    b_star[1] <- b1_star(1, results$deltas[iter+1, ], results$positions[,,iter+1], results$variances[,,iter])
    results$deltas[iter+1, 1] <- a_star[1]/b_star[1]
    # browser()
    for(h in 2:n_dimen){
      b_star[h] <- b2_star(1, results$deltas[iter+1, ], results$positions[,,iter+1], results$variances[,,iter], h)
      results$deltas[iter+1, h] <- truncdist::extrunc("gamma", a=1, shape=a_star[h], rate=b_star[h])

    }
    results$variances[,,iter+1] <- diag(1/cumprod(results$deltas[iter+1,]))
    results$b[iter+1,] <- b_star


    # browser()

    iter <- iter + 1

    results$likelihood[iter,] <- Ell(results$sigma_2_alpha[iter,],
                                     results$mu_alpha[iter,],
                                     results$variances[,,iter],
                                     results$positions[,,iter],
                                     Y)
    SI4SigmaT <- solve(diag(nrow(results$variances[,,iter])) + 4 * results$variances[,,iter])
    dist_v <- dist(results$positions[,,iter] %*% chol(SI4SigmaT))^2
    # browser()
    results$elbo[iter,] <- ELBO(results$likelihood[iter,],
                                results$sigma_2_alpha[iter,],
                                results$mu_alpha[iter,],
                                SI4SigmaT, Y, dist_v,
                                mu_alpha_prior,
                                sigma_2_alpha_prior, variance_prior^2,
                                results$variances[,,iter],
                                results$positions[,,iter])
    dif = results$elbo[iter,] - results$elbo[iter-1,]
    print(paste("Iteration: ", iter, "/", N_iter))
  }
  end_time <- Sys.time()
  total_time = end_time - start_time
  results$time <- total_time
  results$seed <- seed
  # browser()
  # results$deltas <- sweep(results$b, 2, a_star-1, "/") # mode
  # results$deltas <- sweep(results$b, 2, a_star, "/") # mean
  class(results) <- "LSPM_v"
  return(results)
}
