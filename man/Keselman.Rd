\name{Keselman}
\alias{Keselman}
\alias{Keselman1}
\alias{Keselman2}
\docType{data}
\title{Repeated-Measures Pyschopsychological Experiment}
\description{
This data set represents the outcome of an hypothetical experiment, that recorded psychophysiological measures of subjects with different susceptibility
to stressors.  Each subject performed a task at four different levels of challenge.  Data artificially generated from a multivariate lognormal distribution, with unequal variances and covariance structure related to group sample sizes.
}
\usage{
Keselman1
Keselman2
}
\format{
\code{Keselman1} is a data frame of 156 rows and 4 columns, with the measures of 39 subjects in a balanced design.

\code{Keselman2} is an unbalanced subset of \code{Keselman1}, with 120 rows and 4 columns, corresponding to the measures of 30 subjects.

Both data frames contain the following variables:
\describe{
\item{\code{subject}}{Integer identity of the subject.}
\item{\code{group}}{Classification of the subjects according to their susceptbility to stressors. Factor with levels \code{G1}, \code{G2}, and \code{G3}.}
\item{\code{challenge}}{Level of challenge of the task. Ordered factor with levels \code{M1}, \code{M2}, \code{M3}, \code{M4}.}
\item{\code{measure}}{Psychophisiological measure.}
}
}
\source{
Keselman, H. J. (1998) \sQuote{Testing treatment effects in repeated measures designs: an update for psychophysiological researchers}. \emph{Psychophysiology}, 35(4), 470-478.
}
\keyword{datasets}
