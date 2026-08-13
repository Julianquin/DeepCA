#' A matrix of covariates of the 'Lazega Lawyers`.
#'
#' Covariates on each of 71 lawyers in a northeastern American law firm.
#' Note the first column is a column of 1's.
#'
#' @usage data("lawyers.covariates")
#' @format A data frame with 71 observations on the following 8 variables.
#' \describe{
#'   \item{Intercept}{a column of 1s should always be the first column.}
#'   \item{Seniority}{a factor with levels 1 = partner, 2 = associate.}
#'   \item{Gender}{a factor with 1 = male, 2 = female.}
#'   \item{Office}{a factor with levels 1 = Boston, 2 = Hartford and 3 = Providence}
#'   \item{Years}{a numeric vector detailing years with the firm.}
#'   \item{Age}{a numeric vector detailing the age of each lawyer.}
#'   \item{Practice}{a factor with levels 1 = litigation and 2 = corporate.}
#'   \item{School}{a factor with levels 1 = Harvard or Yale, 2 = University of
#'   Connecticut and 3 = Other.}
#' }
#' @source E. Lazega, The Collegial Phenomenon: The Social Mechanisms of
#' Cooperation Among Peers in a Corporate Law Partnership, Oxford University
#'  Press, Oxford, England, 2001.
"lawyers.covariates"
