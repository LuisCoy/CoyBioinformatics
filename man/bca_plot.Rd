% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/bca_plot.R
\name{bca_plot}
\alias{bca_plot}
\title{Plotting BCA results}
\usage{
bca_plot(
  standards_df,
  unknowns_df,
  x_upper_limit = 2.02,
  text_size = 20,
  point_size = 2
)
}
\arguments{
\item{standards_df}{Dataframe with protein standard concentrations, ug/ml, as column names and raw absorbance as values. Standard control column name must be 0. Required check.names = False.}

\item{unknowns_df}{Dataframe with unique ID as column names and raw absorbance as values. Required check.names = False.}

\item{x_upper_limit}{X axis upper limit. Defaults to 2.02}

\item{text_size}{Size of text elements. Defaults to 20}

\item{point_size}{Size of points. Defaults to 2}
}
\value{
Plot of BCA standard curve with unknown protein concentrations.
}
\description{
Plotting BCA results
}
