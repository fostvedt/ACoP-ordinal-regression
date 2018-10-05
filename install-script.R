#####################################
# Checking to see if packages are installed
# then installing packages which are not installed
#####################################

packages <- c("ggplot2", # plotting
              "GGAlly", # pairwise plotting
              "gridExtra", # plotting grid.arrange() function
              "dplyr",  # data manipulation
              "reshape2", # data manipulation
              "magrittr", # data manipulation and pipe operator
              "MASS",  # polr() function
              "ordinal", # clm() and other ordinal regression functions
              "rms", # another package that can do ordinal modeling 
              "VGAM", # another package that can do ordinal modeling 
              "PResiduals", # Probability based residuals (SBS residuals)
              "sure", # surrogate residuals
              "generalhoslem", # goodness-of-fit tests
              "devtools", # github sourcing functions
              "foreign", 
              "Hmisc", 
              "plotROC", # ROC curves functions
              "pROC",  # ROC curve functions
              "boot") # logit(), inv.logit()  functions

if (length(setdiff(packages, rownames(installed.packages()))) > 0) {
  install.packages(setdiff(packages, rownames(installed.packages())))  
}

# load all the packages listed above
lapply(packages,FUN = function(x){
  do.call("library",list(x))
  }
)


sessionInfo()

dat <- read.csv("https://raw.githubusercontent.com/fostvedt/ACoP-ordinal-regression/master/AEs-ordinal-data-ver3.csv",header=T)

# converting DV to a factor since the model
# functions need it to be class factor.
dat1 <- dat %>% mutate(
  DV=factor(DV),
  EXP = CMAXSS,
  SEX = factor(SEX,levels=c(1,2), labels=c("Male","Female")),
  SMOK =  factor(SMOK,levels=c(1,2,3), labels=c("Current","Former","Never"))
)


# testing that the models from the ordinal and MASS package work

mod.clm <- clm(DV~SEX+BIO+CMAXSS,data=dat1)
summary(mod.clm)
nominal_test(mod.clm)
scale_test(mod.clm)

# testing goodness of fit tests
mod.polr <- polr(DV~SEX+BIO+CMAXSS,data=dat1)
summary(mod.polr)
# there should be a warning message about expected cell counts
pulkrob.deviance(mod.polr,"SEX")
logitgof(dat1$DV,fitted(mod.polr),g=6,ord=TRUE)

#should have no warning message
lipsitz.test(mod.polr,g=6)

# getting residuals
dat1$sres <- resids(mod.polr)
dat1$pres <- presid(mod.polr)

# using ggplot to plot the residual diagnostics
autoplot(dat1$sres, what = "covariate", x = dat1$EXP, xlab = "Cmax Steady-State")






