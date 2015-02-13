\name{testInteractions}
\alias{testInteractions}
\title{
Test Contrasts of Factor Interactions
}
\description{
Calculates and tests different types of contrasts for factor interactions, in linear, generalized and mixed linear models: simple main effects, interaction contrasts, residual effects, and others.
}
\usage{
testInteractions(model, pairwise=NULL, fixed=NULL, residual=NULL, across=NULL,
  custom=NULL, slope=NULL, adjustment=NULL, label.factors=FALSE,
  abbrev.levels=FALSE, \dots)
}
\arguments{
  \item{model}{fitted model. Currently supported classes include \code{"lm"}, \code{"glm"}, \code{"mlm"}, \code{"lme"}, and \code{"mer"} or \code{"merMod"} (excluding models fitted by \code{nlmer}).}
  \item{pairwise}{character vector with the names of factors represented by pairwise contrasts.}
  \item{fixed}{character vector with the names of factors represented by fixed levels.}
  \item{residual}{character vector with the names of factors represented by residuals effects.}
  \item{across}{character vector with the names of factors represented by a full set of independent contrasts.}
  \item{custom}{list with custom contrasts for other factors. See the \code{Details} for more information.}
  \item{slope}{character vector with the names of the covariates associated to the slope that will tested; if it is \code{NULL} (the default value), the function will test the adjusted mean values.}
  \item{adjustment}{adjustment method for p-values, as defined in \code{\link{p.adjust}}.}
  \item{label.factors}{If true, the rownames for each row in the resulting table include the name(s) of the factor(s) involved, followed by the level values. Otherwise, the rownames include only the levels of the factor(s), with multiple factors separated by \sQuote{:}.}
  \item{abbrev.levels}{Either a logical or an integer, specifying whether the levels values of the factors in the term are to be abbreviated in constructing the rownames. An integer specifies the minimum length of the abbreviation for each factor in the term.}
  \item{\dots}{further arguments passed down to \code{\link{testFactors}}.}
}
\details{
Each factor of the model can at most be contained in one of the arguments \code{pairwise}, \code{fixed}, \code{residual}, \code{across}, or \code{custom}; redundant assignment of factors is not allowed.  If none of these arguments is defined, the default behavior is as if \code{pairwise} contained all the factors of the model.  The result will show a set of tests on the model adjusted mean, at different combinations of factor levels.  If there are covariates defined in \code{slope}, the test will apply to the slope for the interaction of such covariates.  Each row will contain a different combination of factor levels or contrasts, depending on the argument wherein the factor has been defined:
\itemize{
  \item{The factors contained in \code{pairwise} will appear as pairwise contrasts between levels.}
  \item{The factors contained in \code{fixed} will appear as one of their possible levels.}
  \item{The factors contained in \code{residual} will appear as residual effects of their levels, after removing effects of higher order.}
  \item{The factors contained in \code{across} will appear as a full set of contrasts. By default they will be orthogonal contrasts, unless overriden by the contrasts of the model data frame or by the arguments passed down to \code{\link{testFactors}}.  See the documentation of that function for further details.}
}
Ommitted factors will be averaged across all their levels. Thus, to test the overall adjusted means or slopes, use \code{pairwise=NULL} (or do the same with any of the arguments of the previous list).

Other combinations of factor levels can be defined by \code{custom}.  This argument should be a list of numeric matrices or vectors, named as the model factors.  Each matrix must have as many rows as the number of levels of the corresponding factor, so that each column represents a linear combination of such levels that will be tested, crossed with the combinations of the other factors.  Vectors will be treated as column matrices.

In multivariate linear models it is possible to define an intra-subjects design, with the argument \code{idata} passed down to \code{\link{testFactors}} (see \code{\link{Anova}} or \code{\link{linearHypothesis}} in package \pkg{car} for further details).  The factors defined by that argument can be included as any other factor of the model.
}
\value{
An anova table with one row for each different combination of levels and contrasts defined in \code{pairwise}, \code{fixed}, \code{across}, and \code{custom}.  The rownames represent the specific levels or contrasts used for the different factors, separated by \sQuote{:}. These names can be tweaked by the arguments \code{label.factors} and \code{abbrev.levels}, as done by \code{termMeans} in package \pkg{heplots}.
}
\author{
Helios De Rosario-Martinez, \email{helios.derosario@ibv.upv.es}
}
\seealso{
\code{\link{testFactors}}, \code{\link{interactionMeans}}.
Use \code{\link{contrastCoefficients}} as a facility to create matrices of custom contrasts.
}
\note{The tests of mixed models are done under the assumption that the estimation of the random part of the model is exact.}
\examples{
# Tests of the interactions described in Boik (1979)
# See ?Boik for a description of the data set

mod.boik <- lm(edr ~ therapy * medication, data=Boik)
Anova(mod.boik)
cntrl.vs.T1 <- list(therapy = c(1, -1, 0))
cntrl.vs.T2 <- list(therapy = c(1, 0, -1))
plcb.vs.doses <- list(medication = c(1, -1/3, -1/3, -1/3))
testInteractions(mod.boik, pairwise="therapy", adjustment="none")
testInteractions(mod.boik, custom=plcb.vs.doses, adjustment="none")
testInteractions(mod.boik, custom=cntrl.vs.T1, across="medication", adjustment="none")
testInteractions(mod.boik, custom=c(cntrl.vs.T1, plcb.vs.doses), adjustment="none")
testInteractions(mod.boik, custom=cntrl.vs.T2, across="medication", adjustment="none")
testInteractions(mod.boik, custom=plcb.vs.doses, across="therapy", adjustment="none")
}
