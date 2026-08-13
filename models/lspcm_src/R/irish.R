#' Irish politicians Twitter network
#'
#' Adjacency matrix detailing the presence or absence of a following between 348
#' Irish politicians on Twitter from the year 2012. Each of the politicians is
#' affiliated with one of the seven Irish political parties.
#'
#' @usage data("irish")
#' @format A list containing 2 items:
#' \describe{
#'  \item{ie_adj}{A 348 x 348 binary adjacency matrix, with 0 down the
#'  diagonal.}
#'  \item{ie_party}{A vector containing the politicians affiliated party. Value
#'  of 1 indicate politician is with Fianna Fáil; 2 is with Fine Gael; 3 is
#'  with the Green Party; 4 is with the Labour Party; 5 is with Sinn Féin; 6 is
#'  with the United Left Alliance; 7 are Independent.}
#' }
#' @source See reference below
#' @references
#' \describe{
#'  \item{}{Greene, D. and Cunningham, P. (2013). Producing a unified graph
#'  representation from multiple social network views. WebSci ’13: Proceedings
#'  of the 5th Annual ACM Web Science Conference, page 118–121.}
#' }
"irish"
