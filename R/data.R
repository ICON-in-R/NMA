
#' Study mean and standard error treatment difference
#'
#' A dataset containing the pair comparisons for studies.
#'
#' @format A data frame with 6 variables:
#' \describe{
#'   \item{study}{Study name}
#'   \item{base}{Reference treatment name}
#'   \item{tx}{Comparison treatment name}
#'   \item{Lmean}{Mean difference}
#'   \item{Lse}{Standard error of difference}
#'   \item{multi_arm}{Multi-arm trial indicator: 0/1}
#' }
"subData"


#' Study binary outcome data
#'
#' A dataset containing the pair comparisons for studies.
#'
#' @format A data frame with 5 variables:
#' \describe{
#'   \item{study}{Study name}
#'   \item{base}{Reference treatment name}
#'   \item{tx}{Comparison treatment name}
#'   \item{BinR}{Count of number of events}
#'   \item{BinN}{Total sample size}
#' }
"subDataBin"


#' Study median time to event
#'
#' A dataset containing the pair comparisons for studies.
#'
#' @format A data frame with 6 variables:
#' \describe{
#'   \item{study}{Study name}
#'   \item{base}{Reference treatment name}
#'   \item{tx}{Comparison treatment name}
#'   \item{median}{Median time}
#'   \item{medN}{Total sample size}
#'   \item{medR}{}
#' }
"subDataMed"

