\name{phia-package}
\alias{phia-package}
\alias{phia}
\docType{package}
\title{
Post-Hoc Interaction Analysis}
\description{
Analysis of the expected values and other terms of in linear, generalized, and
mixed linear models, on the basis of multiple comparisons of factor contrasts.
Specially suited for the analysis of interaction effects.}
\details{
\tabular{ll}{
Package: \tab phia\cr
Type: \tab Package\cr
Version: \tab 0.3-0\cr
Date: \tab 2023-12-03\cr
License: \tab GPL (>= 3)\cr
}
This package contains functions that may be used for the post-hoc analysis
of any term of linear models (univariate or multivariate), generalized and mixed
linear models.  The function \code{testFactors} provides a flexible user
interface for defining combinations of factor levels and covariates, to evaluate
and test the model, using the function \code{linearHypothesis} from package
\pkg{car}.  \code{testInteractions} uses this function for multiple comparisons 
of simple effects, interaction residuals, interaction contrasts, or user-defined
contrasts. \code{interactionMeans} may be used to explore the \sQuote{cell 
means} of factorial designs, and plot main effects or first-order interactions.
}
\author{
Helios De Rosario-Martinez, with code contributions from John Fox (package
\pkg{car}) and the R Core Team (\pkg{stats} package).

Maintainer: Helios De Rosario-Martinez <helios.derosario@gmail.com>
}
