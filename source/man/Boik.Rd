\name{Boik}
\alias{Boik}
\docType{data}
\title{Contrived Data of Treatments for Hemophobia}
\description{
Data set based on the hypothetical example used by R.J. Boik (1979) to explain the analysis of interaction contrasts.  It represents the electrodermal response of 72 students complaining of hemophobia, treated with different fear reduction therapies and doses of antianxiety medication, in a balanced factorial design.  The observed values of the dependent variable (not given in the original article) are contrived so that the results of all the tests are coherent with the examples.
}
\usage{Boik}
\format{
A data frame with 72 observations and three columns:
\describe{
\item{\code{therapy}}{Fear reduction therapy. Factor with levels \code{control}, \code{T1}, \code{T2}.}
\item{\code{medication}}{Dose of antianxiety medication. Ordered factor with levels \code{placebo}, \code{D1}, \code{D2}, \code{D3}.}
\item{\code{edr}}{Electrodermal response (in arbitrary units).}
}
}
\source{
Boik, R. J. (1979). \sQuote{Interactions, Partial Interactions, and Interaction Contrasts in the Analysis of Variance}, \emph{Psychological Bulletin},
86(5), 1084-1089.
}
\note{
The anova table in Boik's article (p. 1085) has a misprint in the MS value for \sQuote{Treatment B} (medication): it should be 790.32, instead of 970.32.
}
\keyword{datasets}
