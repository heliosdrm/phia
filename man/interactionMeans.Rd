\name{interactionMeans}
\alias{interactionMeans}
\alias{plot.interactionMeans}
\title{
Calculate and Plot Adjusted Means for Interactions
}
\description{
Creates a data frame with the adjusted means of a fitted model or the slopes associated to its covariates, plus the standard error of those values, for all the interactions of given factors, including intra-subjects factors in multivariate linear models with intra-subjects designs.  These interactions may be plotted by pairs of factors.
}
\usage{
interactionMeans(model, factors=names(xlevels), slope=NULL, \dots)
\method{plot}{interactionMeans}(x, atx=attr(x,"factors"), traces=atx, xlab=atx,
  ylab="", main=NULL, multiple=TRUE, y.equal=FALSE, legend=TRUE, legend.margin=0.2,
  cex.legend=1, abbrev.levels=FALSE, type="b", pch=0:6, errorbar, \dots)
}
\arguments{
  \item{model}{fitted model. Currently supported classes include \code{"lm"}, \code{"glm"}, \code{"mlm"}, \code{"lme"}, and \code{"mer"} or \code{"merMod"} (excluding models fitted by \code{nlmer}).}
  \item{factors}{character vector with the names of interacting factors.  All the factors of the model are used by default; use \code{NULL} for calculating the overall mean.}
  \item{slope}{character vector with the names of the interacting covariates associated to the slope that will be represented; if it is \code{NULL} (the default value), the function will calculate adjusted mean values.}
  \item{x}{an object created by the function \code{interactionMeans}.}
  \item{atx}{character vector with the names of factors that will be represented in the horizontal axis of the plots.  All the factors represented in \code{x} are used by default.}
  \item{traces}{character vector with the names of the factors that will be represented as different traces in a plot (the same as \code{atx} by default).}
  \item{xlab, ylab, main}{values to override the default titles of plots and their axes (see \emph{Details}).}
  \item{multiple}{logical indicating if the plots shall be combined in multi-panel figures.}
  \item{y.equal}{logical indicating if all the plots should have the same range in the y-axis (all plots would be expanded to the maximum range).}
  \item{legend}{logical indicating if legends should be drawn in case of plots with multiple traces.}
  \item{legend.margin}{fraction of the combined plotting area that shall be reserved for drawing the legends, if they are required.}
  \item{cex.legend}{character expansion for the legend, relative to the value of \code{\link{par}("cex")}.}
  \item{abbrev.levels}{A logical indicating whether the factor levels are to be abbreviated in the plot, or an integer that specifies the minimum length of such abbreviations. See \code{\link{abbreviate}}.}
  \item{type, pch}{type of plot and point characters, as used by \code{\link{matplot}}.}
  \item{errorbar}{definition of the error bars in the plots (see \emph{Details}).}
  \item{\dots}{further arguments passed to \code{\link{testFactors}} or \code{\link{matplot}}.}
}
\details{
This function calculates the adjusted values of the model and their standard errors for interactions between factors, at fixed values of covariates, if they exist.  The main or crossed effect of covariates is represented by their \dQuote{slope}, i.e. the variation rate of the response with respect to the product of the specified covariates.  The default value of the covariates (and of the offset, if any) is their average in the model data frame, and it can be changed by the arguments \code{covariates} or \code{offset}, passed down to \code{\link{testFactors}}.  Note that in generalized linear models, standard errors and slopes are referred to the link function, not to the mean (see \code{\link{testFactors}} for details, and how to force the calculation of the link function instead of the response for adjusted means).

In multivariate linear models, the adjusted means or slopes are calculated separately for each column by default, but it is possible to define an intra-subjects design of factors across columns, and put all columns in one.  This may be defined by the argument \code{idata} passed down to \code{\link{testFactors}} (see \code{\link{Anova}} or \code{\link{linearHypothesis}} in package \pkg{car} for further details).  If such transformation is done, it is also possible to include the factors of the intra-subjects design in \code{factors}, for calculating their main effects or interactions. 

The generic \code{plot} function creates matrices of interaction plots, with the main effects of each factor represented in the diagonal, and the interactions between each pair of factors in the rest of panels.  For multivariate models without intra-subjects design, a new device for each variable will be created. By default it also prints error bars around the means, plus/minus their standard errors.  The size of the error bars can be adjusted by the argument \code{errorbar}.  Currently supported definitions are strings with the pattern \code{ciXX}, where \code{XX} is a number between 01 and 99, standing for the $XX%$ asymptotic confidence intervals of the means.  Alternatively, \code{errorbar} can be a function of the form \code{function(mean,std.err)}, returning a 2-element list with the lower and upper values of the bars.

The adjusted means and error bars of generalized models (fitted with \code{glm} or \code{glmer}) are plotted on the scale of the link function, although the y-axis is labelled on the scale of the response (unless the link function had been forced in the calculation of the means).
  
If the interactions involve many factors, it may be convenient to plot each panel in a different device (with \code{multiple=FALSE}), or select a subset of factors to be plotted with the arguments \code{atx} and \code{traces}.  A panel will be plotted for each pair of factors defined by crossing these arguments; if the crossed factors are the same one, that panel will show its main effect.

By default, the interaction plots show the names of the factors on the labels of their x-axes (only for the bottom row of panels, if there is more than one). Those labels can be overriden by the argument \code{xlab}, which should be a character vector with as many strings as the number of factors of the model (or the number of factors indicated in \cod{atx}). The string passed as \code{ylab} will be used as the label of the y-axes on the left column of panels (no label is printed by default). The default main titles of the plots are the names of the dependent variables (e.g. "adjusted mean"), but they can be also replaced by a string passed in the argument \code{main}, which will be used for all the variables if there is more than one.
}
\value{
\code{interactionMeans} returns an object of class \code{"interactionMeans"}, that contains a data frame with the factor interactions and the means or slopes of the model adjusted to them, and some attributes used for plotting.
}
\author{
Helios De Rosario-Martinez, \email{helios.derosario@gmail.com}
}
\note{
The purpose of the \code{plot} method is similar to the function \code{\link{interaction.plot}}, but it uses the lower-level function \code{\link{matplot}}, so the aspect of the plots is different.
}
\seealso{
\code{\link{testFactors}}, \code{\link{interactionMeans}}.
}
\examples{
# Interaction between two factors
# See ?Adler for a description of the data set

mod.adler <- lm(rating ~ instruction * expectation, data=Adler)
(means.adler <- interactionMeans(mod.adler))
plot(means.adler, abbrev.levels=TRUE)


# Effect of factors on the slopes of the model
# See ?SLID for a description of the data set

SLID$logwages <- log2(SLID$wages)
mod.slid <- lm(logwages ~ education + age * (sex  * language), data=SLID)
(slopes.slid <- interactionMeans(mod.slid, slope="age"))
plot(slopes.slid)


# Include intra-subjects factors
# See ?OBrienKaiser for a description of the data set

mod.ok <- lm(cbind(pre.1, pre.2, pre.3, pre.4, pre.5, 
                     post.1, post.2, post.3, post.4, post.5, 
                     fup.1, fup.2, fup.3, fup.4, fup.5) ~  treatment*gender, 
                data=OBrienKaiser)

# Intra-subjects data:
phase <- factor(rep(c("pretest", "posttest", "followup"), each=5))
hour <- ordered(rep(1:5, 3))
idata <- data.frame(phase, hour)

# Calculate all interactions, but plot only the interactions between
# hour (represented in the x-axis) and the other factors (in traces)

means.ok <- interactionMeans(mod.ok, idata=idata)
plot(means.ok, atx="hour", traces=c("gender","treatment","phase"))
}
