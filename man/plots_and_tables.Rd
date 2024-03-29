% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plots_and_tables.R, R/plots_and_tables_tmp.R
\name{plots_and_tables}
\alias{plots_and_tables}
\title{Write to file multiple NMA plots and tables}
\usage{
plots_and_tables(
  nma,
  res_bugs,
  effectParam = NA,
  label = "",
  endpoint = NULL,
  folder = "output",
  save = FALSE
)

plots_and_tables(
  nma,
  res_bugs,
  effectParam = NA,
  label = "",
  endpoint = NULL,
  folder = "output",
  save = FALSE
)
}
\arguments{
\item{nma}{Object of class \code{nma}}

\item{res_bugs}{BUGS output}

\item{effectParam}{Effect parameter names; string}

\item{label}{Label}

\item{endpoint}{End point names; string}

\item{folder}{Output folder name; string}

\item{save}{Logical}

\item{dat}{ist of study data, including subData
and possibly survDataBin and survDataMed}

\item{labels}{Labels}
}
\value{
res_bugs

res_bugs
}
\description{
Write to file multiple NMA plots and tables

Write to file multiple NMA plots and tables
}
