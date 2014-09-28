interactionMeans <- function(model, factors=names(xlevels), slope=NULL, ...){
	# Levels in existing factors
	xlevels <-  getXLevels(model)
	dots <- list(...)
	# Include within-subjects factors, if they are defined
	if ("idata" %in% names(dots)) xlevels <- c(xlevels, lapply(dots$idata, levels))
	# Check the factors specified in the arguments
	specified.factors <- match(factors,names(xlevels))
	if (any(is.na(specified.factors))) warning("Some factors are not in the model and will be ignored")
	xlevels <- xlevels[sort(specified.factors)]
	# Create diagonal matrices sized as the number of factor levels
	nlev <- sapply(xlevels,length)
	fdiags <- lapply(nlev,diag)
	# Argument test.formula for testFactors
	if (is.null(slope)){
		terms.formula <- ~1
		value.label <- "adjusted mean"
		term.label <- "(Intercept)"
		slope.term <- NULL
	}else{
		# Check predictors specified in the arguments
		model.variables <- getPredictors(model)
		numeric.predictors <- model.variables[!(model.variables %in% names(xlevels))]
		specified.numeric <- match(slope,numeric.predictors)
		if (any(is.na(specified.numeric))) warning("Some covariates are not in the model and will be ignored.")
		slope <- numeric.predictors[sort(specified.numeric)]
		slope.term <- term.label <- value.label <- paste(slope,collapse=":")
		terms.formula <- as.formula(paste("~0 +",slope.term))
	}
	# Create data frame with values
	tf <- testFactors(model,fdiags,terms.formula=terms.formula,lht=FALSE,...)
	interactions.table <- tf$terms[[1]]$adjusted.values
	interactions.dataframe <- if (is.null(factors)) as.data.frame(matrix(,nrow=1,ncol=0)) else expand.grid(xlevels)
	# Define what term is represented in the data frame
	# (and redefine if it is the link function in a glm)
	attr(interactions.dataframe,"term") <- term.label
	fam <- getFamily(model)
	if (!is.null(fam)){
		attr(interactions.dataframe,"family") <- fam
		if (attr(tf,"means")=="link" && is.null(slope.term)){
			attr(interactions.dataframe,"term") <- "(Link)"
			value.label <- "adjusted link"
		}
	}
	# The calculated interactions are in just one variable (after possible response transformation)
	nf <- length(xlevels)
	if (length(interactions.table)==nrow(interactions.dataframe)){
		interactions.dataframe[nf+1] <- as.numeric(interactions.table)
		names(interactions.dataframe)[nf+1] <- value.label
	}else{
		# The calculated interactions are in several variables
		if (nrow(interactions.table)==nrow(interactions.dataframe)){
			rownames(interactions.table)<-NULL
			if (!is.null(slope.term)) colnames(interactions.table) <- paste(value.label,colnames(interactions.table),sep=".")
			interactions.dataframe <- cbind(interactions.dataframe,interactions.table)
		}else stop("Incompatible dimensions of the intra-subjects design")
	}
	# Assign more attributes and class for further methods on this object
	attr(interactions.dataframe,"factors") <- names(interactions.dataframe)[1:nf]
	attr(interactions.dataframe,"values") <- names(interactions.dataframe)[(nf+1):ncol(interactions.dataframe)]
	class(interactions.dataframe) <- c("interactionMeans","data.frame")
	return(interactions.dataframe)
}

plot.interactionMeans <- function(x, atx=attr(x,"factors"), traces=atx, multiple=TRUE, y.equal=FALSE, legend=TRUE, legend.margin=0.2, cex.legend=1, abbrev.levels=FALSE, type="b", pch=0:6, ...){
	# Check consistency between x and atx, traces
	if (!("interactionMeans" %in% class(x))) stop("The first argument must be an interactionMeans object.")
	valid.atx <- (atx %in% attr(x,"factors"))
	if (!all(valid.atx)){
		warning("Some values of atx are not factors of x and will be ignored.")
		atx <- atx[valid.atx]
	}
	valid.traces <- (traces %in% attr(x,"factors"))
	if (!all(valid.traces)){
		warning("Some values of traces are not factors of x and will be ignored.")
		traces <- traces[valid.traces]
	}
	# Abbreviate factors, if required
	if (abbrev.levels){
		specified.factors <- unique(c(atx,traces))
		if (is.logical(abbrev.levels)){
			x[specified.factors] <- lapply(specified.factors, function(f) as.factor(abbreviate(x[[f]])))
		}else x[specified.factors] <- lapply(specified.factors, function(f) as.factor(abbreviate(x[[f]],minlength=abbrev.levels)))
	}
	# Get values of link function if specified (for glm)
	fam <- NULL
	if (transform <- (!is.null(fam <- attr(x,"family")) && attr(x,"term")=="(Intercept)")){
		yy <- attr(x,"values")
		x[[yy]] <- fam$linkfun(x[[yy]])
	}
	# Calculate and apply margin-plot regions ratio
	marginRatio <- function(n,prop) n*prop/(1-prop)
	xax.margin <- 0.25
	# Loop over the numeric columns
	for (y in attr(x,"values")){
		# List of data values for plots,
		# setting legend to FALSE if are not traces
		if (is.null(traces)){
			legend <- FALSE
			nr <- 1
			nc <- length(atx)
			atx.pattern <- atx
			plotdata <- lapply(atx,function(margin) tapply(x[,y],x[margin],mean))
		}else{
			nr <- length(traces)
			nc <- length(atx)
			atx.pattern <- rep(atx,nr)
			traces.pattern <- rep(traces,each=nc)
			if (all(atx.pattern==traces.pattern)) legend <- FALSE
			plotdata <- mapply(
				function(xf,lf){
					margins <- unique(c(xf,lf))
					tapply(x[,y],x[margins],mean)
					},
				atx.pattern,traces.pattern,USE.NAMES=FALSE,SIMPLIFY=FALSE)
		}
		# Re-transform data if suitable (glm)
		# if (transform) plotdata <- lapply(plotdata,fam$linkinv)
		# Define figures
		dots <- list(...)
		# Parameters for legend
		defpar <- formals(matplot)
		col <- if ("col" %in% names(dots)) dots$col else eval(defpar$col)
		lty <- if ("lty" %in% names(dots)) dots$lty else eval(defpar$lty)
		lwd <- if ("lwd" %in% names(dots)) dots$lwd else eval(defpar$lwd)
		if (multiple){
			# Create device with multiple figures
			if (length(y)>1L) dev.new()
			par(oma=c(1,4,4,2),mar=rep(0,4))
			# Parameters for x label and title
			cex.lab <- if("cex.lab" %in% names(dots)) dots$cex.lab else 1
			col.lab <- if("col.lab" %in% names(dots)) dots$cex.lab else par("col")
			font.lab <- if("font.lab" %in% names(dots)) dots$font.lab else par("font")
			cex.main <- if("cex.main" %in% names(dots)) dots$cex.lab else 1
			col.main <- if("col.main" %in% names(dots)) dots$cex.lab else par("col")
			font.main <- if("font.main" %in% names(dots)) dots$font.lab else par("font")
			if (legend){
				layout(matrix(1:((nr+1)*(nc+1)),nrow=nr+1,byrow=TRUE),
					c(rep(1,nc),marginRatio(nc,legend.margin)),
					c(rep(1,nr),xax.margin))
			}else{
				layout(matrix(1:((nr+1)*nc),nrow=nr+1,byrow=TRUE),
					c(rep(1,nc)),
					c(rep(1,nr),xax.margin))
			}
			# Get y limits to set common values
			ylim.all <- sapply(plotdata,range)
			f <- 0L
			# Start plotting by rows
			for (row in 1:nr){
				# y limits for the row
				ylim <- if (y.equal) range(ylim.all) else range(ylim.all[,f+(1:nc)])
				# Redefine the labels of y-axes if suitable (glm)
				if (transform){
					ylabels <- pretty(fam$linkinv(ylim))
					at <- fam$linkfun(ylabels)
					yaxis <- list(at=at,labels=as.character(ylabels))
				}else yaxis <- list(at=pretty(ylim),labels=TRUE)
				# do not draw legend until a figure with traces is found in the row
				legend.labels <- NULL
				for(column in 1:nc){
					# Increase plot counter, get and plot data without axes
					f <- f+1
					means <- plotdata[[f]]
					if (is.matrix(means)) legend.labels <- colnames(means) else means <- as.matrix(means)
					matplot(means,type=type,pch=pch,
						axes=FALSE,xlab="",ylab="",
						xlim=c(0.5,nrow(means)+0.5),ylim=ylim,...)
					box()
					# Draw x axis, with labels if it is the last row
					axis(1,at=1:nrow(means),labels=if (row==nr) rownames(means) else FALSE,...)
					# Draw y axis, with labels if it is the first column
					axis(2,at=yaxis$at,labels=if (column==1) yaxis$labels else FALSE,...)
				}
				# If there are legends, jump to the last column and add it (if required)
				if (legend){
					plot.new()
					if (!is.null(legend.labels)) legend("right",legend.labels,title=traces[row],
						pch=pch,cex=cex.legend,col=col,lty=lty,lwd=lwd)
				}
			}
			# Add x labels in the regions of the last row
			for (column in 1:nc){
				plot.new()
				text(0.5,0.1,atx[column],pos=3,cex=cex.lab,col=col.lab,font=font.lab)
			}
			mtext(y,outer=TRUE,line=1,cex=cex.main,col=col.main,font=font.main)
		}else{
			for (f in 1:length(plotdata)){
				means <- plotdata[[f]]
				if (f > 1L) dev.new()
				ylim <- if (y.equal) range(ylim.all) else range(means)
				if (transform){
					ylabels <- pretty(fam$linkinv(ylim))
					at <- fam$linkfun(ylabels)
					yaxis <- list(at=at,labels=as.character(ylabels))
				}else yaxis <- list(at=pretty(ylim),labels=TRUE)
				# Draw with legend if there are multiple traces
				if (legend && is.matrix(means)){
					margins <- par("mar")
					oma <- mar <- rep(0,4)
					oma[c(2,4)] <- margins[c(2,4)]
					mar[c(1,3)] <- margins[c(1,3)]
					par(oma=oma,mar=mar)
					layout(matrix(1:2,1),c(1,marginRatio(1,legend.margin)),1)
					matplot(means,type=type,pch=pch,
						xaxt="n",yaxt="n",xlab=atx.pattern[f],ylab="",
						xlim=c(0.5,nrow(means)+0.5),ylim=ylim,main=y,...)
					axis(1,at=1:nrow(means),labels=rownames(means),...)
					axis(2,at=yaxis$at,labels=yaxis$labels,...)
					plot.new()
					legend("right",colnames(means),title=traces.pattern[f],
						pch=pch,cex=cex.legend,col=col,lty=lty,lwd=lwd)
				}else{
					means <- as.matrix(means)
					matplot(means,type=type,pch=pch,
						xaxt="n",yaxt="n",xlab=atx.pattern[f],ylab="",
						xlim=c(0.5,nrow(means)+0.5),ylim=ylim,main=y,...)
					axis(1,at=1:nrow(means),labels=rownames(means),...)
					axis(2,at=yaxis$at,labels=yaxis$labels,...)
				}
			}
		}
	}
}

