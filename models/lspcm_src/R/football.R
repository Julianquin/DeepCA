#' English football players Twitter network
#'
#' Adjacency matrix detailing the presence or absence of mentions between 55
#' English Premier League football players on Twitter. The players each comes
#' from 3 different Premier League clubs: 15 players from Stoke City football
#' club, 23 players from Tottenham Hotspur, and 17 players from West Bromwich.
#'
#' @usage data("football")
#' @format A list containing 2 items:
#' \describe{
#'  \item{football_adj}{A 55 x 55 binary adjacency matrix, with 0 down the
#'  diagonal.}
#'  \item{football_club}{A vector containing the player affiliated club. Value
#'  of 1 indicate player is from Stoke City; 2 is from Tottenham Hotspur; 3 is
#'  from West Bromwich.}
#' }
#' @source See reference below
#' @references
#' \describe{
#'  \item{}{Greene, D. and Cunningham, P. (2013). Producing a unified graph
#'  representation from multiple social network views. WebSci ’13: Proceedings
#'  of the 5th Annual ACM Web Science Conference, page 118–121.}
#' }
"football"
