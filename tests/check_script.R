library(car)

measure <- function(gender,treatment,age,weight,phase,hour){
	# Means at age=35, weight=75
	table.means <- rbind(c(90,110,90,90,110,90),
		c(90,120,90,90,120,90),
		c(90,110,90,75,95,75),
		c(80,120,90,75,105,75))
	# Slopes on age at age=35, weight=75
	table.age <- rbind(c(1,1,1,1,1,1),
		c(1.3,1.3,1.45,1.3,1.3,1.45),
		c(1,1,1,0.5,0.5,0.5),
		c(1.3,1.3,1.45,0.5,0.5,0.5))
	# Slopes on age at age=35, weight=75
	table.weight <- rbind(c(0.8,0.8,0.8,0.8,0.8,0.8),
		c(1,1,1,1,1,1),
		c(0.8,0.8,0.8,0.8,0.8,0.5),
		c(1,1,1,0.8,0.8,0.5))
	table.age.weight <- rbind(c(0.2,0.2,0.2,0.2,0.2,0.2),
		c(0.2,0.2,0.2,0.2,0.2,0.2),
		c(0.2,0.2,0.2,0,0,0),
		c(0.2,0.2,0.2,0,0,0))
	rownames(table.means) <- rownames(table.age) <- rownames(table.weight) <- rownames(table.age.weight) <- c("F.control","M.control","F.X","M.X")
	colnames(table.means) <- colnames(table.age) <- colnames(table.weight) <- colnames(table.age.weight) <- c("pre.1","pre.2","pre.3","post.1","post.2","post.3")
	intercept <- table.means[paste(gender,treatment,sep="."),paste(phase,hour,sep=".")]
	slope.age <- table.age[paste(gender,treatment,sep="."),paste(phase,hour,sep=".")]
	slope.weight <- table.weight[paste(gender,treatment,sep="."),paste(phase,hour,sep=".")]
	slope.age.weight <- table.age.weight[paste(gender,treatment,sep="."),paste(phase,hour,sep=".")]
	return(intercept + slope.age*(age-35) + slope.weight*(weight-75) + slope.age.weight*(age-35)*(weight-75))
}

# Create dataframe of predictors
dataframe <- expand.grid(gender=c("F","M"),treatment=c("control","X"))
n=10
dataframe <- dataframe[rep(1:4,each=n),]
set.seed(60249)
dataframe$age <- rnorm(n*4,35,5)
set.seed(1901)
dataframe$weight <- rnorm(n*4,75,7)

# Response
Y <- matrix(nrow=4*n,ncol=6,dimnames=list(NULL,c("pre.1","pre.2","pre.3","post.1","post.2","post.3")))
for (r in 1:(4*n)){
Y[r,1] <- with(dataframe, measure(gender[r],treatment[r],age[r],weight[r],"pre",1))
Y[r,2] <- with(dataframe, measure(gender[r],treatment[r],age[r],weight[r],"pre",2))
Y[r,3] <- with(dataframe, measure(gender[r],treatment[r],age[r],weight[r],"pre",3))
Y[r,4] <- with(dataframe, measure(gender[r],treatment[r],age[r],weight[r],"post",1))
Y[r,5] <- with(dataframe, measure(gender[r],treatment[r],age[r],weight[r],"post",2))
Y[r,6] <- with(dataframe, measure(gender[r],treatment[r],age[r],weight[r],"post",3))
}
# Response with error
set.seed(2890)
Y <- Y + matrix(rnorm(n*4*6,sd=5),ncol=6,)

# Plan of tests
# 1. Model with only intercept (Y~1)
# 2. Model with 1 factor (Y~gender)
# 3. Model with 1 covariate (Y~age)
# 4. Model with 2 factors (Y~gender*treatment)
# 5. Model with 2 covariates (Y~age*weight)
# 6. Full model (Y~gender*treatment*age*weight)
# 7-12. Same as 1-6 without intercept
# 13. 6 in glm (family="poisson")
# 14. 6 with all responses, without transformation
# 15. 14 with response transformations
# 16. 6 with various terms (~age*gender)
# 17. 15 with various terms (~age*hour)
# 18. 16 with inherit.contrasts=TRUE
# 12, 13, 16, 17 tested (a) with "levels" set to fixed values (default),
#  (b) without "levels" or "covariates",
#  (c) with "levels" set to fixed values and "covariates" set to c(35,75)
#  (d) with "levels" seet to fixed values and "covariates" set to 0

# Formulae
formulas <- vector("list",17)
formulas[[1]] <- Y[,6]~1
formulas[[2]] <- Y[,6]~gender
formulas[[3]] <- Y[,6]~age
formulas[[4]] <- Y[,6]~gender*treatment
formulas[[5]] <- Y[,6]~age*weight
formulas[[6]] <- Y[,6]~gender*treatment*age*weight
formulas[[7]] <- Y[,6]~0
formulas[[8]] <- Y[,6]~gender-1
formulas[[9]] <- Y[,6]~age-1
formulas[[10]] <- Y[,6]~gender*treatment-1
formulas[[11]] <- Y[,6]~age*weight-1
formulas[[12]] <- Y[,6]~gender*treatment*age*weight-1
formulas[[13]] <- round(Y[,6])~gender*treatment*age*weight
formulas[[14]] <- Y~gender*treatment*age*weight
formulas[[15]] <- Y~gender*treatment*age*weight
formulas[[16]] <- Y[,6]~gender*treatment*age*weight
formulas[[17]] <- Y~gender*treatment*age*weight
formulas[[18]] <- Y[,6]~gender*treatment*age*weight

# Models
models <- lapply(formulas,"lm",data=dataframe)
models[[13]] <- glm(formulas[[13]],data=dataframe,family="poisson")

# Fixed values of levels
levelslm <- list(treatment=c("X","control"))
levelsmlm <- list(treatment="control",phase="post",hour=c("1","3"))
# Frame for response transformation
idata=expand.grid(hour=factor(1:3),phase=c("pre","post"))

#1
(x <- testFactors(models[[1]]))
summary(x)
#2
(x <- testFactors(models[[2]]))
summary(x)
#3
(x <- testFactors(models[[3]]))
summary(x)
#4
(x <- testFactors(models[[4]],levelslm))
summary(x)
#5
(x <- testFactors(models[[5]]))
summary(x)
#6
(x <- testFactors(models[[6]],levelslm))
summary(x)
#7
(x <- testFactors(models[[7]]))
summary(x)
#8
(x <- testFactors(models[[8]]))
summary(x)
#9
(x <- testFactors(models[[9]]))
summary(x)
#10
(x <- testFactors(models[[10]],levelslm))
summary(x)
#11
(x <- testFactors(models[[11]]))
summary(x)
#12
(x <- testFactors(models[[12]],levelslm))
summary(x)
#13
(x <- testFactors(models[[13]],levelslm))
summary(x)
#14
(x <- testFactors(models[[14]],levelslm))
summary(x)
#15
(x <- testFactors(models[[15]],levelsmlm,idata=idata,idesign=~phase*hour))
summary(x)
#16
(x <- testFactors(models[[16]],levelslm,terms.formula=~age*gender))
summary(x)
#17
(x <- testFactors(models[[17]],levelsmlm,idata=idata,idesign=~phase*hour,terms.formula=~age*hour))
summary(x)
#18
(x <- testFactors(models[[18]],levelslm,terms.formula=~age*gender,inherit.contrasts=TRUE))
summary(x)

#12.b
(x <- testFactors(models[[12]]))
summary(x)
#13.b
(x <- testFactors(models[[13]]))
summary(x)
#16.b
(x <- testFactors(models[[16]],terms.formula=~age*gender))
summary(x)
#17.b
(x <- testFactors(models[[15]],idata=idata,idesign=~phase*hour,terms.formula=~age*hour))
summary(x)
#12.c
(x <- testFactors(models[[12]],levelslm,covariates=c(weight=75,age=35)))
summary(x)
#13.c
(x <- testFactors(models[[13]],levelslm,covariates=c(weight=75,age=35)))
summary(x)
#16.c
(x <- testFactors(models[[16]],levelslm,terms.formula=~age*gender,covariates=c(weight=75,age=35)))
summary(x)
#17.c
(x <- testFactors(models[[15]],levelsmlm,idata=idata,idesign=~phase*hour,terms.formula=~age*hour,covariates=c(weight=75,age=35)))
summary(x)
#12.d
(x <- testFactors(models[[12]],levelslm,covariates=0))
summary(x)
#13.d
(x <- testFactors(models[[13]],levelslm,covariates=0))
summary(x)
#16.d
(x <- testFactors(models[[16]],levelslm,terms.formula=~age*gender,covariates=0))
summary(x)
#17.d
(x <- testFactors(models[[15]],levelsmlm,idata=idata,idesign=~phase*hour,terms.formula=~age*hour,covariates=0))
summary(x)

# lme and mer objects
Snijders <- nlme::bdf[c("langPOST",   # Outcomes
    "pupilNR", "IQ.ver.cen", "ses", "sex",       # Student-related variables
    "schoolNR", "schoolSES", "avg.IQ.ver.cen")]  # School-related variables
Snijders$sex <- factor(Snijders$sex, labels=c("F","M"))
names(Snijders) <-
    c("score","student","IQ","SES","sex","school","avgSES","avgIQ")

# Plan of tests
# 1. Model with only intercept
# 2. Model with 1 factor
# 3. Model with 1 covariate
# 4. Model with 2 covariates
# 5. Full model
# 6-9. Same as 2-5 without intercept
# 10. 5 in glm (family="poisson")

library(nlme)
# Models
models <- vector("list",9)
models[[1]] <- lme(score~1,data=Snijders,random=~1|school)
models[[2]] <- lme(score~sex,data=Snijders,random=~1|school)
models[[3]] <- lme(score~IQ,data=Snijders,random=~IQ|school)
models[[4]] <- lme(score~SES,data=Snijders,random=~1|school)
models[[5]] <- lme(score~IQ*SES*sex,data=Snijders,random=~IQ|school)
models[[6]] <- lme(score~0+sex,data=Snijders,random=~1|school)
models[[7]] <- lme(score~0+IQ,data=Snijders,random=~IQ|school)
models[[8]] <- lme(score~0+SES,data=Snijders,random=~1|school)
models[[9]] <- lme(score~0+IQ*SES*sex,data=Snijders,random=~IQ|school)

# Fixed values of levels
levelslme <- list(sex=c("F","M"))

#1
(x <- testFactors(models[[1]]))
summary(x)
#2
(x <- testFactors(models[[2]],levelslme))
summary(x)
#3
(x <- testFactors(models[[3]]))
summary(x)
#4
(x <- testFactors(models[[4]]))
summary(x)
#5
(x <- testFactors(models[[5]],levelslme))
summary(x)
#6
(x <- testFactors(models[[6]],levelslme))
summary(x)
#7
(x <- testFactors(models[[7]]))
summary(x)
#8
(x <- testFactors(models[[8]]))
summary(x)
#9
(x <- testFactors(models[[9]],levelslme))
summary(x)
#5.b
(x <- testFactors(models[[5]],terms.formula=~sex*IQ))
summary(x)
#5.c
(x <- testFactors(models[[5]],levelslme,covariates=c(IQ=3,SES=20)))
summary(x)
#5.d
(x <- testFactors(models[[5]],levelslme,covariates=0))
summary(x)


library(lme4)
# Models
models <- vector("list",10)
models[[1]] <- lmer(score~1+(1|school),data=Snijders)
models[[2]] <- lmer(score~sex+(1|school),data=Snijders)
models[[3]] <- lmer(score~IQ+(IQ|school),data=Snijders)
models[[4]] <- lmer(score~SES+(1|school),data=Snijders)
models[[5]] <- lmer(score~IQ*SES*sex+(IQ|school),data=Snijders)
models[[6]] <- lmer(score~0+sex+(1|school),data=Snijders)
models[[7]] <- lmer(score~0+IQ+(IQ|school),data=Snijders)
models[[8]] <- lmer(score~0+SES+(1|school),data=Snijders)
models[[9]] <- lmer(score~0+IQ*SES*sex+(IQ|school),data=Snijders)
models[[10]] <- glmer(round(score)~IQ*SES*sex+(1|school),data=Snijders,family="poisson")

# Fixed values of levels
levelslme <- list(sex=c("F","M"))

#1
(x <- testFactors(models[[1]]))
summary(x)
#2
(x <- testFactors(models[[2]],levelslme))
summary(x)
#3
(x <- testFactors(models[[3]]))
summary(x)
#4
(x <- testFactors(models[[4]]))
summary(x)
#5
(x <- testFactors(models[[5]],levelslme))
summary(x)
#6
(x <- testFactors(models[[6]],levelslme))
summary(x)
#7
(x <- testFactors(models[[7]]))
summary(x)
#8
(x <- testFactors(models[[8]]))
summary(x)
#9
(x <- testFactors(models[[9]],levelslme))
summary(x)
#10
(x <- testFactors(models[[10]],levelslme))
summary(x)
#5.b
(x <- testFactors(models[[5]],terms.formula=~sex*IQ))
summary(x)
#5.c
(x <- testFactors(models[[5]],levelslme,covariates=c(IQ=3,SES=20)))
summary(x)
#5.d
(x <- testFactors(models[[5]],levelslme,covariates=0))
summary(x)
#10.b
(x <- testFactors(models[[10]],terms.formula=~sex*IQ))
summary(x)
#10.c
(x <- testFactors(models[[10]],levelslme,covariates=c(IQ=3,SES=20)))
summary(x)
#10.d
(x <- testFactors(models[[10]],levelslme,covariates=0))
summary(x)

## Steve Kohler's example
jeff <- read.table("behaviorHB.txt", header=T)
for(j in c("Trial","Tank","Obs")) jeff[,j]<- factor(jeff[,j])
## start with a model that ignores random effects and omits the 3-way interaction
lm1 <- lm(normMoves~Heron+Bass+Day+Heron:Bass+Heron:Day+Bass:Day, data=jeff)
(Moves.means <- interactionMeans(lm1, factors=list("Heron","Bass")))
## custom contrasts in phia
noHeron.vs.others <- list(Heron=c(2,-1,-1))
noBass.vs.others <- list(Bass=c(2,-1,-1))
Honce.vs.Htwice <- list(Heron=c(0,1,-1))
Bonce.vx.Btwice <- list(Bass=c(0,1,-1))
noBass.vs.Bonce <- list(Bass=c(1,-1,0))
## the following gives the 3 "partial interactions" comparing the no-Heron
## treatment to the others for each level of Bass
testInteractions(lm1,custom=noHeron.vs.others, fixed="Bass", adjustment="none")
## the following gives the 3 individual contrasts ("partial interactions")
## comparing the Bass control to Bass present for each level of Heron
testInteractions(lm1,custom=noBass.vs.others, fixed="Heron", adjustment="none")
## the following are "product interactions" 
testInteractions(lm1,custom=c(noHeron.vs.others,noBass.vs.Bonce), adjustment="none")
testInteractions(lm1,custom=c(noHeron.vs.others,Bonce.vx.Btwice), adjustment="none")
testInteractions(lm1,custom=c(noBass.vs.others,Honce.vs.Htwice), adjustment="none")
## examine a model that includes random effects
lmm1 <- lmer(normMoves~Heron+Bass+Day+Heron:Bass+Heron:Day+Bass:Day+(1|Trial)+(1|Tank:Trial), data=jeff)
## the following gives the 3 "partial interactions" comparing the no-Heron
## treatment to the others for each level of Bass
testInteractions(lmm1,custom=noHeron.vs.others, fixed="Bass", adjustment="none")
## the following gives the 3 individual contrasts ("partial interactions")
## comparing the Bass control to Bass present for each level of Heron
testInteractions(lmm1,custom=noBass.vs.others, fixed="Heron", adjustment="none")
## the following are "product interactions" 
testInteractions(lmm1,custom=c(noHeron.vs.others,noBass.vs.Bonce), adjustment="none")
testInteractions(lmm1,custom=c(noHeron.vs.others,Bonce.vx.Btwice), adjustment="none")
testInteractions(lmm1,custom=c(noBass.vs.others,Honce.vs.Htwice), adjustment="none")
## examine a glm that does not include random effects
## the model assumes that Moves values are Poisson distributed
## an offset is included to account for loss of fish within a trial
glm1 <- glm(Moves~Heron+Bass+Day+Heron:Bass+Heron:Day+Bass:Day, offset=log(Fish), family="poisson", data=jeff)
## look at the cell means
(Moves.means <- interactionMeans(glm1, factors=list("Heron","Bass")))
## the following gives the 3 "partial interactions" comparing the no-Heron
## treatment to the others for each level of Bass
testInteractions(glm1,custom=noHeron.vs.others, fixed="Bass", adjustment="none")
## examine glmm with random effects, and assumes Moves values are Poisson distributed
## an additional random effect for each observation is included to account for overdispersion
## log(Fish) is included as an offset to account for loss of fish within a trial
glmm1 <- glmer(Moves~Heron+Bass+Day+Heron:Bass+Heron:Day+Bass:Day+(1|Trial)+(1|Tank:Trial)+(1|Obs), offset=log(Fish), family="poisson", data=jeff)
## look at the cell means
(Moves.means <- interactionMeans(glmm1, factors=list("Heron","Bass")))
## the following gives the 3 "partial interactions" comparing the no-Heron
## treatment to the others for each level of Bass
testInteractions(glmm1,custom=noHeron.vs.others, fixed="Bass", adjustment="none")

