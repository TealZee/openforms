% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/of_import.R
\name{of_import}
\alias{of_import}
\title{Imports form data from openforms.com}
\usage{
of_import(formID, apiKey, cache = FALSE)
}
\arguments{
\item{formID}{The version ID of the form}

\item{apiKey}{The API key for the form owner's account}

\item{cache}{Allows data to be cached in a file to avoid hitting database repeatedly for the same data. False by default. If true, accepts a file path as an argument to cache imported data.}
}
\description{
This function returns form response data for a given form ID
}
\examples{
of_import(1000, apiKey)
}
