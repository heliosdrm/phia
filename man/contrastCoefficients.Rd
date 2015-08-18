\name{contrastCoefficients}
\alias{contrastCoefficients}
\title{
Calculate Coefficient Matrices of Factor Contrasts
}
\description{
Take symbolic formulas of contrasts across the levels of one or more factors, and return a list of matrices with the corresponding linear combination coefficients, in a suitable form to use in \code{testFactors}, or as custom contrasts in \code{testInteractions}.
}
\usage{
contrastCoefficients(..., contrast.definitions,
	data=parent.frame(), normalize=FALSE)
}
\arguments{
	\item{..., contrast.definitions}{definitions of the contrasts.}
	\item{data}{list, data frame or environment that contains the factors symbolically represented in the contrast definitions.}
	\item{normalize}{logical value: should the coefficients be normalized to unit norm?}
}
\details{
In the context of this function, a \dQuote{contrast} means a linear combination of factor levels (regardless of the dummy coefficients that are used to code those levels in fitted models).  For a factor \code{f} with three levels \code{f1}, \code{f2}, \code{f3}, this could be a single level (e.g. \code{f ~ f1}), a \dQuote{pairwise} contrast (e.g. \code{f ~ f1 - f2}), an average (e.g. \code{f ~ (f1 + f2 + f3) / 3}), or any other linear combination.  Such arithmetic operations are usually applied to the values of a dependent variable, conditioned to the value of the represented factor, but those symbolic representations, as if the levels of the factors were themselves combined, are useful abstractions that may come in handy to define hypotheses about the effects of the factor.

This function takes one or more formulas of that type, and returns matrices of the linear coefficients that define such contrasts. For the previous examples, it would return the column matrices \code{list(f=matrix(c(f1=1, f2=0, f3=0)))}, \code{list(f=matrix(c(f1=1, f2=-1, f3=0)))}, and \code{list(f=matrix(c(f1=0.333, f2=-0.333, f3=0.333)))}, respectively.  The factors must be defined in the data frame, list, or environment given in \code{data}, with the name provided in the right hand side of the formulas.  By default this is the \code{parent.frame}, which in normal interactive use is the global environment.

The contrast matrices are returned in a named list, where the names are the represented factors, as required by the arguments \code{levels} and \code{custom} of the functions \code{testFactors} and \code{testInteractions}, respectively.  When more than one formula is given for the same factor, all the corresponding columns of coefficients are bound into a single matrix.

Other ways of representing contrasts, allowed by \code{testFactors} and \code{testInteractions}, can also be mixed with the formulas as input arguments.  Such alternative contrast definitions are just appended without changes in the returned list, after the matrices created from the formulas.  (Notice that if other coefficient vectors or matrices are explicitly entered, these will not be combined with the ones calculated from formulas.)

In case of having the contrast definitions stored in a list, that list can be entered as the argument \code{contrast.definitions}.  This option is recommended if the contrast definitions are mixed with named elements which could create a conflict with \code{data}, \code{normalize}, or \code{contrast.definitions} itself (although this is unlikely to happen specially if all the definitions are given as formulas).  In any event, only one alternative of entering the definitions is allowed. If \code{contrast.definitions} is given, all the other definitions are ignored with a warning.
}
\value{
A named list, where each element has the name of a factor, and contains a numeric matrix with the contrast coefficients. Each row of such matrices is associated to a level of the factor, in the order given by \code{\link{levels}}.
}
\author{
Helios De Rosario-Martinez, \email{helios.derosario@gmail.com}
}
\seealso{
\code{\link{testFactors}}, \code{\link{interactionMeans}}.
}
\examples{
# Calculate the coefficients of example(testInteractions)
# cntrl.vs.T1 <- list(therapy = c(1, -1, 0))
contrastCoefficients(therapy ~ control - T1, data = Boik)
# cntrl.vs.T2 <- list(therapy = c(1, 0, -1))
contrastCoefficients(therapy ~ control - T2, data = Boik)
# plcb.vs.doses <- list(medication = c(1, -1/3, -1/3, -1/3))
contrastCoefficients(medication ~ placebo - (D1+D2+D3)/3, data = Boik)
# Combine cntrl.vs.T1 and plcb.vs.doses
contrastCoefficients(
  therapy ~ control - T1, medication ~ placebo - (D1+D2+D3)/3, data = Boik)

# Put various contrasts of the same factor in a matrix, and normalize them
contrastCoefficients(
	therapy ~ control - T1,
	therapy ~ control - T2,
	medication ~ placebo - (D1+D2+D3)/3,
	data = Boik,
	normalize=TRUE)
}
