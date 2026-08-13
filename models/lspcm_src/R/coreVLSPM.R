upXiT <- function (psi2T, xiT, SI4SigmaT, Y, cont, xi, psi2)
{
  A <- as.vector(sqrt(det(SI4SigmaT)) * exp(-cont) * exp(psi2T/2))
  f_xi <- function(xi_v) {
    xi_v/psi2 - (xi/psi2 + sum(Y)) + sum( 1/(1+exp(-xi_v)*(1/A)) )
  }

  magnitude_iter = 0
  magnitude_changer = 1e-10
  # browser()
  if(multiroot(f_xi, xiT)$f.root[1] != "NaN" & multiroot(f_xi, xiT)$f.root[1] < 1e-5){
    xiT = multiroot(f_xi, xiT)$root[1]
  } else {
    while(magnitude_iter < 15) {
      if(suppressWarnings(multiroot(f_xi, xiT*magnitude_changer)$f.root[1] == "NaN")){
        magnitude_changer = magnitude_changer*10
        magnitude_iter = magnitude_iter + 1
      } else {
        if(multiroot(f_xi, xiT*magnitude_changer)$f.root[1] > 1e-5){
          magnitude_changer = magnitude_changer*10
          magnitude_iter = magnitude_iter + 1
        } else {
          xiT = multiroot(f_xi, xiT*magnitude_changer)$root[1]
          break()
        }
      }
    }

    # return(print("Error in finding alpha_mu root"))
  }

  return(xiT)
}

upPsi2T <- function (psi2T, xiT, SI4SigmaT, cont, psi2)
{
  B=  as.vector((sqrt(det(SI4SigmaT)) *
                   exp(xiT) * exp(-cont)))
  f_psi2 <- function(psi_v){
    1/(2*psi2) + sum( 1/2 * 1/(1+exp(-psi_v/2)*(1/B)) ) - 1/(2*psi_v)
  }

  magnitude_iter = 0
  magnitude_changer = 1e-6
  while(magnitude_iter < 10) {
    if(suppressWarnings(multiroot(f_psi2, psi2T*magnitude_changer)$f.root[1] == "NaN")){
      magnitude_changer = magnitude_changer*10
      magnitude_iter = magnitude_iter + 1
    } else {
      if(multiroot(f_psi2, psi2T*magnitude_changer)$f.root[1] > 1e-5){
        magnitude_changer = magnitude_changer*10
        magnitude_iter = magnitude_iter + 1
      } else {
        psi2T = multiroot(f_psi2, psi2T*magnitude_changer)$root[1]
        break()
      }
    }
    # return(print("Error in finding alpha_sigma2 root"))
  }

  return(psi2T)
}

upSigmaT <- function (psi2T, xiT, SI4SigmaT, Y, cont, ZT, s2, SigmaT_v)
{
  D <- nrow(SI4SigmaT)
  N <- nrow(Y)
  cbZT <- combn(nrow(ZT), 2)
  dij <- as.matrix(ZT[cbZT[1, ], ] - ZT[cbZT[2, ], ])

  SIGMA2_V <- matrix(0, nrow=D, ncol=D)
  for(i in 1:D){
    A <- 1 + 1/sqrt(SigmaT_v[i,i]) * exp(-xiT - psi2T/2) * exp(cont)

    f_Sigma2 <- function(SigmaT_v) {
      (N/(2*s2) + 2*sum(Y)) - N/2/(SigmaT_v) +
        8 /(1 + 4 * SigmaT_v) * sum(dij[,i]/A * dij[,i]) / (1 + 4 * SigmaT_v) -
        4 * sum(1/A) / (1 + 4 * SigmaT_v)
      # (1/s2 + 4 * sum(Y)/N) * diag(D) + 2 * f1s2To/N
    }

    magnitude_iter = 0
    magnitude_changer = 1e-6
    while(magnitude_iter < 10) {
      if(suppressWarnings(multiroot(f_Sigma2, SigmaT_v[i,i]*magnitude_changer)$f.root[1] == "NaN")){
        magnitude_changer = magnitude_changer*10
        magnitude_iter = magnitude_iter + 1
      } else {
        if(multiroot(f_Sigma2, SigmaT_v[i,i]*magnitude_changer)$f.root[1] > 1e-5){
          magnitude_changer = magnitude_changer*10
          magnitude_iter = magnitude_iter + 1
        } else {
          SIGMA2_V[i,i] = multiroot(f_Sigma2, SigmaT_v[i,i]*magnitude_changer)$root[1]
          break()
        }
      }
      # return(print("Error in finding Sigma2 root"))
    }

  }
  return(SIGMA2_V)
}


upZT <- function (psi2T, xiT, SigmaT, ZT, Y, s2) {
  D = nrow(SigmaT)
  SI4SigmaT <- solve(diag(nrow(SigmaT)) + 4 * SigmaT)
  # Define the function for a matrix
  f_matrix <- function(ZT_flat) {
    ZT_v <- matrix(ZT_flat, ncol=D) # Reshape the flattened matrix
    dnj2 <- t(as.vector(ZT_v) - t(as.matrix(ZT[-n, ])))

    ZT_v%*%t(ZT_v)*( 1/(2 * s2) + sum(Y[n, -n] + Y[-n, n])) -2*
      ZT_v %*% colSums(as.matrix(ZT[-n, ]) * (Y[n, -n] + Y[-n, n])) +
      # ZT_v %*%  rowSums((Y[n, -n] + Y[-n, n])* t(ZT[-n,]) ) +
      2*sum(log(1 + sqrt(det(SI4SigmaT)) * exp(xiT + psi2T/2) *
                  exp(apply(dnj2, 1, function(x) t(x %*% SI4SigmaT %*%
                                                     x))) ))
  }

  # Define the gradient for a matrix
  gradient_matrix <- function(ZT_flat) {
    ZT_v <- matrix(ZT_flat, ncol=D) # Reshape the flattened matrix

    dnj <- t(as.vector(ZT_v) - t(as.matrix(ZT[-n, ])))
    A <- 1/sqrt(det(SI4SigmaT)) * exp(-xiT - psi2T/2) *
      exp(apply(dnj, 1, function(x) t(x %*% SI4SigmaT %*%
                                        x)))
    f1znTo <- -2 * SI4SigmaT %*% colSums(dnj/(1 + A))


    as.vector(f1znTo)
  }

  for (n in 1:nrow(ZT)) {
    # dnj <- t(ZT[n, ] - t(as.matrix(ZT[-n, ])))
    # A <- 1/sqrt(det(SI4SigmaT)) * exp(-xiT - psi2T/2) *
    #   exp(apply(dnj, 1, function(x) t(x %*% SI4SigmaT %*%
    #                                     x)))
    # f1znTo <- -2 * SI4SigmaT %*% colSums(dnj/(1 + A))
    # f2znTo <- -2 * sum(1/(1 + A)) * SI4SigmaT + 4 * SI4SigmaT %*%
    #   (t(dnj/(2 + 1/A + A)) %*% dnj) %*% SI4SigmaT
    # numZnT <- colSums(as.matrix(ZT[-n, ]) * (Y[n, -n] +
    #                                            Y[-n, n])) - t(f1znTo) + t(ZT[n, ]) %*% f2znTo
    # denZnT <- (sum(Y[n, -n] + Y[-n, n]) + 1/(2 * s2)) *
    #   diag(nrow(SigmaT)) + f2znTo



    ZT[n,] <-  optim(ZT[n,], f_matrix, gradient_matrix, method="CG")$par
    # ZT[n, ] <- numZnT %*% solve(denZnT)
  }
  ZT
}


# Define the function for a matrix
f_matrix <- function(X_flat) {
  X <- matrix(X_flat, nrow=2) # Reshape the flattened matrix
  sum(X^2) + log(det(X))
}

# Define the gradient for a matrix
gradient_matrix <- function(X_flat) {
  X <- matrix(X_flat, nrow=2) # Reshape the flattened matrix
  grad_det <- solve(t(X))     # Gradient of log(det(X)) w.r.t X is solve(t(X))
  grad_sum_sq <- 2*X          # Gradient of sum of squares

  # Combine gradients. Then, flatten the result for optim.
  total_grad <- grad_sum_sq + grad_det
  as.vector(total_grad)
}


Ell <- function (psi2T, xiT, SigmaT, ZT, Y)
{
  SI4SigmaT <- solve(diag(nrow(SigmaT)) + 4 * SigmaT)
  A <- log(1 + sqrt(det(SI4SigmaT)) * exp(xiT + psi2T/2) *
             exp(-dist(ZT %*% chol(SI4SigmaT))^2))
  sum((xiT - 2 * sum(diag(SigmaT)) - as.matrix(dist(ZT)^2)) *
        Y) - 2 * sum(A)
}

ELBO <- function(ell, sigma_2_alpha_v, mu_alpha_v, SI4SigmaT, Y, dist_v, mu_alpha, sigma2_alpha, omega, SigmaT, ZT){
  n=nrow(Y)
  p=nrow(SI4SigmaT)

  # KLmu_alpha <- .5* ((mu_alpha_v-mu_alpha)^2/sigma2_alpha) - sum(Y*(mu_alpha_v))
  # KLsigma2_alpha <- .5* ((sigma_2_alpha_v/sigma2_alpha)-log(sigma_2_alpha_v/sigma2_alpha))
  # KLzi <- sum(ZT^2/2/omega) + sum(Y*as.matrix(dist_v))
  # KLomega <- - n*log(det(SigmaT)) +  n/2/omega*sum(diag(SigmaT)) + sum(Y*2*sum(diag(SigmaT)))
  # print(c(KLmu_alpha, KLsigma2_alpha))
  # sum(Y*(mu_alpha_v - 2*sum(diag(SigmaT)) - as.matrix(dist_v)))
  # browser()

  -(.5*((sigma_2_alpha_v/sigma2_alpha)-log(sigma_2_alpha_v/sigma2_alpha) +
          (mu_alpha_v-mu_alpha)^2/sigma2_alpha + n*p*log(omega)- n*log(det(SigmaT)) ) +
      n/2/omega*sum(diag(SigmaT)) + sum(ZT%*%t(ZT))/2/omega - ell - (1+n*p)/2 )
}


b1_star <- function(b1, DELTAS, ZT, SigmaT){
  n_obs = nrow(ZT)
  n_dimen = nrow(SigmaT)
  # browser()
  beta1_sum <- matrix(nrow = n_obs, ncol = n_dimen)
  # DELTAS <- a_v/b_v
  OMEGAS <- cumprod(DELTAS)
  for (l in 1:n_dimen) {
    # beta1_sum[, l] <- OMEGAS[l] / DELTAS[1] * ((ZT[, l]) ^ 2 + 1/OMEGAS[l])
    beta1_sum[, l] <- OMEGAS[l] / DELTAS[1] * ((ZT[, l]) ^ 2 )
  }
  b1 + 0.5 * sum(beta1_sum)
  # b1 + 0.5*(sum((prod(a2_v/b2_v)) * (ZT^2 + sum(diag(SigmaT)))))
}

b2_star <- function(b2, DELTAS, ZT, SigmaT, h){
  n_obs = nrow(ZT)
  n_dimen = nrow(SigmaT)
  # browser()
  # DELTAS <- a_v/b_v
  OMEGAS <- cumprod(DELTAS)
  betal_sum <- matrix(nrow = n_obs, ncol = n_dimen)
  for (l in h:n_dimen) {
    # betal_sum[, l] <- OMEGAS[l] / DELTAS[h] * ((ZT[, l]) ^ 2 + 1/OMEGAS[l])
    betal_sum[, l] <- OMEGAS[l] / DELTAS[h] * ((ZT[, l]) ^ 2 )
  }

  b2 + (0.5) * sum(betal_sum, na.rm = T)

  # b2 + 0.5*(sum((a1_v/b1_v)*prod(a2_v/b2_v) * (ZT^2 + sum(diag(SigmaT)))))
}







