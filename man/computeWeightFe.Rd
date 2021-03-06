% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/weights.R
\name{computeWeightFe}
\alias{computeWeightFe}
\title{Single Weight Iteration}
\usage{
computeWeightFe(dt, w_i, denom, fe)
}
\arguments{
\item{dt}{data.table.}

\item{w_i}{Character. Weight to demean.}

\item{denom}{Numeric. Relative size of untreated population.}

\item{fe}{Character. Name of the fixed effect to demean on.}
}
\value{
Void. Mutate the weight vector in the original data.table.
}
\description{
Single iteration of weight demean w.r.t. fixed effect.
}
