
#' Study log hazard ratios and standard errors
#'
#' A dataset containing the pair comparisons for studies.
#' Woods NMA allows data on HR on the log scale.
#' 
#' @format A data frame with 6 variables:
#' \describe{
#'   \item{study}{Study name}
#'   \item{base}{Reference treatment name}
#'   \item{tx}{Comparison treatment name}
#'   \item{Lmean}{Log hazard ratio}
#'   \item{Lse}{Standard error of log hazard ratio}
#'   \item{multi_arm}{Multi-arm trial indicator: 0/1}
#' }
"subData"


#' Study survival binary outcome data
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
#'   \item{medN}{Sample size}
#'   \item{medR}{Sample size/2 as estimated number of events}
#' }
"subDataMed"


#' Binary data
#'
#' A dataset containing the pair comparisons for studies.
#'
#' @format A data frame with 5 variables:
#' \describe{
#'   \item{study}{Study name}
#'   \item{treatment}{Reference treatment name}
#'   \item{n}{Total number}
#'   \item{r}{Number of sucesses}
#'   \item{prob}{Percentage successes}
#' }
"binData"


#' Counts data
#'
#' A dataset containing the pair comparisons for studies.
#'
#' @format A data frame with 4 variables:
#' \describe{
#'   \item{study}{Study name}
#'   \item{treatment}{Reference treatment name}
#'   \item{E}{Total time at risk}
#'   \item{r}{Number of counts}
#' }
"countData"


#' Continuous data
#'
#' A dataset containing the pair comparisons for studies.
#'
#' @format A data frame with 6 variables:
#' \describe{
#'   \item{study}{Study name}
#'   \item{treatment}{Reference treatment name}
#'   \item{dur}{Duration}
#'   \item{se}{Standard error}
#'   \item{n}{Total number}
#'   \item{y}{Outcome measurement}
#' }
"contsData"

