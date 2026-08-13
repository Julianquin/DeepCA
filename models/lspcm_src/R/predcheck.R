#' Posterior Predictive Check for LSPM
#'
#' Performs posterior predictive check by simulating multiple networks through
#' using samples from the MCMC chains of the parameters estimated from LSPM.
#'
#' @param n_posterior Number of samples to be taken for posterior predictive
#' check
#' @param n_dimen The number of dimension to conditioned on the chain
#' @param LSPM_object An LSPM list containing the MCMC chains for the different
#' parameters resulting from running the LSPM function.
#' @param network The adjacency matrix of the observed network
#' @param alpha_chain The MCMC chain of the \eqn{\alpha} parameter
#' @param latent_pos_chain The MCMC chain of the latent positions
#' @param type The network type with edges being binary valued or count valued
#' @param beta_chain The MCMC chain of the coefficient of the latent positions
#' distances
#' @param true_prob True probability matrix used to create the network
#' @param dist_power The Euclidean distance power value (LSPM uses squared
#' Euclidean and thus it is 2)
#' @param networkMTGP The object created from networkMTGP function
#' @param seed Setting the seed
#'
#' @details
#' Posterior predictive checking is performed by taking multiple samples from
#' the posterior predictive distribution. Parameter estimates are taken from the
#' MCMC chain and are used to simulate data replicates under the fitted model.
#' These simulated networks are then compared to the observed network by
#' checking similarity metrics, network properties and distances between
#' networks.
#' \describe{
#' \strong{Binary network}
#'  \item{Density}{Network density is the ratio of the number of observed
#'  dyadic connections over the number of possible dyadic connections.}
#'  \item{Transitivity}{Number of triangles divided by the total number of
#'  connected triples}
#'  \item{Accuracy}{Measure of all the correctly identified edges.
#'  \deqn{\frac{TP + TN}{n},} where TP is true positive, TN is true negative,
#'  and n is total number of node.}
#'  \item{F1 score}{Harmonic mean of precision (P) and recall (R).
#'  \deqn{F1 = 2 \times \frac{P \times R}{P + R}.}}
#'  \item{Hamming distance}{The normalised simple differences between the
#'  observed network and network simulated from the posterior predictive
#'  distribution}
#' }
#'
#' \describe{
#' \strong{Count network}
#'  \item{Networks}{To be updated}
#'  \item{alpha_mean}{To be updated}
#'  \item{z_mean}{To be updated}
#'  \item{observed_net}{To be updated}
#' }
#'
#' @export
#'
#' @importFrom abind abind
#' @importFrom sna gden gtrans hdist
#'
#' @family functions
#'
#' @examples
#' # Obtain posterior predictive distribution results
#' predLSPM <- predcheck(10, 2, lspm:::LSPM_sample, type = "binary")
#'
#' # Plot the posterior predictive distribution results
#' predPlot(predLSPM)
predcheck <- function(n_posterior, n_dimen, LSPM_object,
                      network, alpha_chain, latent_pos_chain,
                      type =c("binary", "count"), beta_chain=1,
                        networkMTGP, seed, true_prob = NULL, dist_power=2 ) {
  edgetype <- match.arg(type) # obtain network type
  if (!missing(seed)) {
    set.seed(seed) # set seed if exists
  } else {
    seed <- sample(1:1e5, 1) # sample a seed from 1 to 10,000
  }

  # Obtain samples from the chain
  if(!missing(LSPM_object)) {
    if(class(LSPM_object) != "LSPM" & class(LSPM_object) != "LSPM_adapt"){
      return(cat("LSPM object is invalid"))
    }
    alpha_chain = LSPM_object$alpha[LSPM_object$iter_d == n_dimen]
    latent_pos_chain = LSPM_object$mcmc_chain[[paste0(n_dimen,"D")]]$positions[
      ,,LSPM_object$iter_d == n_dimen]
    network = LSPM_object$initialisation$network

    posterior_samples <- sample(length(alpha_chain), n_posterior, replace=TRUE)
  } else if(!missing(alpha_chain) & !missing(latent_pos_chain)){
    posterior_samples <- sample(length(alpha_chain), n_posterior, replace=TRUE)
  } else {
    posterior_samples <- 1:n_posterior # used for true
  }

  n_obs <- dim(network)[1]

  # Initialise empty matrix/vector
  posterior_distance_est <- matrix(ncol=2)
  posterior_fnorm_est <- posterior_hamming <-
    posterior_trans <- posterior_dens <- networkcollection <- c()

  # Simulate via each sample
  for(sample_number in  posterior_samples){
    if(!missing(alpha_chain) & !missing(latent_pos_chain)){ # using chain to sim
      posterior_alpha <- alpha_chain[sample_number] # taking the alpha
      ifelse(length(beta_chain) == 1,
             posterior_beta <- 1, # if no beta chain, coefficient just 1
             posterior_beta <- beta_chain[sample_number]) # taking beta if has

      if(is.na(dim(latent_pos_chain)[3])){
        posterior_location <- latent_pos_chain
      } else{
        posterior_location <- latent_pos_chain[,,sample_number] # use estimated
      }
      logit_p_numerator <-
        exp(posterior_alpha -
              posterior_beta*dist(posterior_location,
                                  diag = TRUE, upper = TRUE,
                                  method = "euclidean")^dist_power) # numerator component for logit function

    } else { # using the true parameters to simulate
      logit_p_numerator <-
        exp(networkMTGP$alpha -
              dist(networkMTGP$positions,
                   diag = TRUE, upper = TRUE, method = "euclidean")^dist_power) # numerator component for logit function

    }

    # Calculate posterior predictive check metrics
    if(edgetype == "binary") { # for binary network
      posterior_prob <- as.matrix(logit_p_numerator / (1 + logit_p_numerator)) # calculate probabilities matrix

      posterior_network <- matrix(rbinom(posterior_prob,1,posterior_prob),
                                  nrow=n_obs, ncol=n_obs) # adjacency matrix, the network data
      networkarray  <- abind(posterior_network, network, along = 3)
      networkarray <- aperm(networkarray, c(3,1,2))
      posterior_distance_est <- rbind(posterior_distance_est,
                                      binarydist(table(posterior_network,network),
                                                 method = c('Simple', 'F1score'))) # Accuracy & F1score
      posterior_hamming <- c(posterior_hamming,
                             hdist(networkarray, g1=1, g2=2, normalize=TRUE)) # Hamming Dist
      posterior_trans <- c(posterior_trans, gtrans(posterior_network)) # Transitivity
      posterior_dens <- c(posterior_dens, gden(posterior_network)) # Density
      if(!is.null(true_prob)) {
        posterior_fnorm_est <- c(posterior_fnorm_est,
                                 norm(posterior_prob-true_prob, type="F")) # Frobenius norm
      }
    } else if(edgetype == "count") { # for count network
      posterior_prob <- as.matrix(logit_p_numerator) # calculate probabilities matrix

      posterior_network <- matrix(rpois(posterior_prob,posterior_prob),
                                  nrow=n_obs, ncol=n_obs) # adjacency matrix, the network data

      posterior_table <- table(posterior_network, network)
      networkarray  <- abind(posterior_network, network, along = 3)
      networkarray <- aperm(networkarray, c(3,1,2))
      networkcollection <- abind(networkcollection, posterior_network, along = 3)
    }
  }

  if(edgetype == "binary") {
    results <- list("Density" = posterior_dens,
                    "Transitivity" = posterior_trans,
                    "Accuracy" = posterior_distance_est[-1,1],
                    "F1score" = posterior_distance_est[-1,2],
                    "Hamming" = posterior_hamming)
    if(!is.null(true_prob)) {
      results$Fnorm = posterior_fnorm_est
    }
    return(do.call(cbind,results))
  } else if(edgetype == "count") {
    results <- list("Networks" = networkcollection,
                    "alpha_mean" = mean(alpha_chain),
                    "z_mean" = apply(latent_pos_chain, c(1,2), mean),
                    "observed_net" = network)
    return(results)
  }

}
