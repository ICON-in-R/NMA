% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/save_bugs_files.R
\name{save_bugs_files}
\alias{save_bugs_files}
\title{Save BUGS files}
\usage{
save_bugs_files(
  res_bugs,
  bugs_params,
  run_bugs,
  labels,
  folder = "output",
  overwrite = TRUE
)
}
\arguments{
\item{res_bugs}{BUGS output}

\item{bugs_params}{BUGS parameter names; strings}

\item{run_bugs}{Run BUGS? Logical}

\item{labels}{Labels}

\item{folder}{output folder name; string}

\item{overwrite}{Logical}
}
\value{
res_bugs
}
\description{
Save BUGS files
}
