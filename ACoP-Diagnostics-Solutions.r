################################################
# ACOP 2018 Ordinal Logistic Regression Introduction      
# Hands On solutions
# Luke Fostvedt, PhD
# email: Luke.Fostvedt@pfizer.com
# Prepared October 2018
# creative commons license
################################################
#loading libraries
library(dplyr)
library(ggplot2)
library(gridExtra)
library(MASS) 
library(reshape2)
library(ordinal)
library(sure)
library(rms) # for orm function
library(VGAM) # for vgam and vglm functions
library(PResiduals)
library(generalhoslem)

sessionInfo()

# R version 3.4.1 (2017-06-30)
# Platform: x86_64-w64-mingw32/x64 (64-bit)
# Running under: Windows >= 8 x64 (build 9200)

#------------------------------------------------------------------------
#Load data file
#------------------------------------------------------------------------
#Source Data


##########################################
# Read in the Dataset (if not already read in)
##########################################
filename<-"AEs-ordinal-data-ver3.csv"
dat <- read.csv(filename,na.strings=".",stringsAsFactors=F, header=T)

#################################
# convert Sex and DV to factor

d<-dat %>% 
  mutate(DOSE>0)%>%
  mutate(SEX=factor(SEX,levels=c(1,2),labels=c("Male","Female")),
         DV = factor(DV))

# create new variable for exposure
d$EXP<-d$CMAXSS
################################

#----------------------------------------------------------------

full <- d %>%
  dplyr::select(DV,SEX,SMOK,WT,AGE,BIO,DOSE,BIO2,EXP)

# alternative way to write the line above
#full <- d[,which(colnames(d) %in% c("DV","SEX","SMOK","WT","AGE","BIO","DOSE","BIO2","EXP"))]
dout <- full

# using these models
final.polr <- polr(DV ~ EXP+SEX+BIO, data=full)
final.clm <- clm(DV ~ EXP+SEX+BIO, data=full, method="logit")



###############################################################
#
### ------------------    Solutions   ----------------------###
#
###############################################################


set.seed(131313)
sres <- resids(final.polr)
dout$sres = sres
# Residual-vs-covariate plot and Q-Q plot
p1 <- autoplot(sres, what = "covariate", x = dout$EXP, xlab = "Cmax Steady-State")
p2 <- autoplot(sres, what = "covariate", x = dout$AGE, xlab = "Age")
p3 <- autoplot(sres, what = "covariate", x = dout$WT, xlab = "Weight")
p4 <- autoplot(sres, what = "covariate", x = dout$BIO, xlab = "Biomarker")
p5 <- autoplot(sres, what = "covariate", x = dout$BIO2, xlab = "Biomarker 2")
p6 <- autoplot(sres, what = "covariate", x = factor(dout$SMOK), xlab = "SMOKE")
p7 <- autoplot(sres, what = "covariate", x = factor(dout$DOSE), xlab = "DOSE")
p8 <- autoplot(sres, what = "covariate", x = factor(dout$SEX), xlab = "Sex")
p9 <- autoplot(sres, what = "qq", distribution = qnorm,nsim = 100)
grid.arrange(p1, p2, p3,p4, p5,p6,p7,p8,p9, nrow = 3) # Figure 2

##################################################
#
#
# recreate the SBS residual plots with the 
# the final model we developed earlier
#
#
##################################################
dout$pres.final <- presid(final.polr)


g1a <- dout %>% 
  ggplot(aes(x=EXP,y=pres.final)) + geom_point()+
  theme_bw()+
  labs(list(y="Sign Based Statistics",x="Steady-State Cmax"))+ 
  stat_smooth(se=F)

g1b <- dout %>% 
  ggplot(aes(x=AGE,y=pres.final)) + geom_point()+
  theme_bw()+
  labs(list(y="Sign Based Statistics",x="Age (years)"))+ 
  stat_smooth(se=F)

g1c <- dout %>% 
  ggplot(aes(x=WT,y=pres.final)) + geom_point()+
  theme_bw()+
  labs(list(y="Sign Based Statistics",x="Weight (kg)"))+ 
  stat_smooth(se=F)

g1d <- dout %>% 
  ggplot(aes(x=BIO,y=pres.final)) + geom_point()+
  theme_bw()+
  labs(list(y="Sign Based Statistics",x="Biomarker 1"))+ 
  stat_smooth(se=F)

g1e <- dout %>% 
  ggplot(aes(x=BIO2,y=pres.final)) + geom_point()+
  theme_bw()+
  labs(list(y="Sign Based Statistics",x="Biomarker 2"))+ 
  stat_smooth(se=F)

g2 <- dout %>% 
  ggplot(aes(x=SEX,y=pres.final,fill=SEX)) + 
  geom_boxplot()+
  theme_bw()+
  labs(list(y="Sign Based Statistics",x="Sex"))+ 
  guides(fill=FALSE)

g3 <- dout %>% 
  mutate(DOSE=factor(DOSE)) %>%
  ggplot(aes(x=DOSE,y=pres.final,fill=DOSE)) + 
  geom_boxplot()+
  theme_bw()+
  labs(list(y="Sign Based Statistics",x="Dose"))+ 
  guides(fill=FALSE)

g4 <- dout %>% 
  mutate(SMOK=factor(SMOK)) %>%
  ggplot(aes(x=SMOK,y=pres.final,fill=SMOK)) + 
  geom_boxplot()+
  theme_bw()+
  labs(list(y="Sign Based Statistics",x="Smoking Status"))+ 
  guides(fill=FALSE)

g5 <- dout %>% ggplot(aes(sample=pres.final)) + 
  stat_qq(distribution = stats::qunif, dparams = list(min=-1,max=1))+ 
  geom_abline(intercept=0,slope=1) +
  theme_bw()

grid.arrange(g1a,g1b,g1c,g1d,g1e, g2, g3,g4, g5, nrow = 3) # Figure 2

##################################################
# Goodness-of-fit tests with the final model
##################################################
final.polr <- polr(DV ~ EXP+SEX+BIO, data=full)
final.clm <- clm(DV ~ EXP+SEX+BIO, data=full, method="logit")

pulkrob.chisq(final.polr,"SEX")
pulkrob.deviance(final.polr,"SEX")
logitgof(full$DV,fitted(final.polr),g=10,ord=TRUE)
lipsitz.test(final.polr,g=10)
lipsitz.test(final.polr,g=6)


##################################################
#
# Test the Proportional odds Assumption
#
##################################################

nominal_test(final.clm)
scale_test(final.clm)

##################################################
#
# Test link function
# goodness-of-fit tests from sure package
#
##################################################

# Fit models with various link functions to the simulated data
fit.probit <- update(final.polr, method = "probit")
fit.logistic <- update(final.polr, method = "logistic") # correct link
fit.loglog <- update(final.polr, method = "loglog") 
fit.cloglog <- update(final.polr, method = "cloglog")

# Construct Q-Q plots of the surrogate residuals for each model
set.seed(1056) # for reproducibility
p1 <- autoplot(fit.probit, nsim = 100, what = "qq")
p2 <- autoplot(fit.logistic, nsim = 100, what = "qq")
p3 <- autoplot(fit.loglog, nsim = 100, what = "qq")
p4 <- autoplot(fit.cloglog, nsim = 100, what = "qq")

grid.arrange(p1, p2, p3, p4, ncol = 2) # bottom left plot is correct model


#Testing the link function using the Anderson darling test
par(mfrow = c(2, 2), mar = c(2, 4, 2, 2) + 0.1)
set.seed(8491) # for reproducibility
plot(gof(fit.probit, nsim = 100, test = "ks"), main = "Probit")
plot(gof(fit.logistic, nsim = 100, test="ks"), main = "Logit")
plot(gof(fit.loglog, nsim = 100, test = "ks"), main = "Log-log")
plot(gof(fit.cloglog, nsim = 100, test ="ks"), main = "c-log-log")


#noW make this plot using the kolmogorov smirnov test

par(mfrow = c(2, 2), mar = c(2, 4, 2, 2) + 0.1)
set.seed(8491) # for reproducibility
plot(gof(fit.probit, nsim = 100, test = "ks"), main = "Probit")
plot(gof(fit.logistic, nsim = 100, test = "ks"), main = "Logit")
plot(gof(fit.loglog, nsim = 100, test = "ks"), main = "Log-log")
plot(gof(fit.cloglog, nsim = 100, test = "ks"), main = "c-log-log")

#noW make this plot using the Cramer von Mises test

par(mfrow = c(2, 2), mar = c(2, 4, 2, 2) + 0.1)
set.seed(8491) # for reproducibility
plot(gof(fit.probit, nsim = 100, test =  "cvm"), main = "Probit")
plot(gof(fit.logistic, nsim = 100, test= "cvm"), main = "Logit")
plot(gof(fit.loglog, nsim = 100, test =  "cvm"), main = "Log-log")
plot(gof(fit.cloglog, nsim = 100, test = "cvm"), main = "c-log-log")


##################################################
#
# Evaluate Proportional odds using 
# individual logistic regressions
#
##################################################
#need to add plot of point estimates with CIs
# make it specific to our dataset
df4.glms <- dout %>% 
  mutate(DV = as.numeric(as.character(DV))) %>%
  mutate(
    DV0 = factor(1*(DV<=0)),
    DV1 = factor(1*(DV<=1)),
    DV2 = factor(1*(DV<=2)),
    DV3 = factor(1*(DV<=3))
  )


fit.prop0 <- glm(DV0~EXP+SEX+BIO,data=df4.glms,family=binomial("logit"))
fit.prop1 <- glm(DV1~EXP+SEX+BIO,data=df4.glms,family=binomial("logit"))
fit.prop2 <- glm(DV2~EXP+SEX+BIO,data=df4.glms,family=binomial("logit"))
fit.prop3 <- glm(DV3~EXP+SEX+BIO,data=df4.glms,family=binomial("logit"))


conest0 <- as.data.frame(confint(fit.prop0))
conest0$Model = "DV0"
conest0$Est = coef(fit.prop0)
conest0$Variables = row.names(conest0)

conest1 <- as.data.frame(confint(fit.prop1))
conest1$Model = "DV1"
conest1$Est = coef(fit.prop1)
conest1$Variables = row.names(conest1)

conest2 <- as.data.frame(confint(fit.prop2))
conest2$Model = "DV2"
conest2$Est = coef(fit.prop2)
conest2$Variables = row.names(conest2)

conest3 <- as.data.frame(confint(fit.prop3))
conest3$Model = "DV3"
conest3$Est = coef(fit.prop3)
conest3$Variables = row.names(conest3)


conest <- rbind(conest0,conest1,conest2,conest3)
colnames(conest)[1:2] <- c("lower","upper")

conest %>% 
  mutate(Variables=factor(Variables,labels=c("Intercept","Steady-State Cmax","Sex (Female)","Biomarker")),
         Model = factor(Model,labels=paste0("Prob(DV <= ",0:3,")"))) %>%
  ggplot(aes(x=Model,y=Est))+
  geom_linerange(aes(ymin=lower,ymax=upper),size=2)+
  geom_point(color="red",size=4)+
  facet_grid(~Variables,scales="free_x")+
  theme_bw()+
  coord_flip()+
  labs(list(x="Model",y="Coefficient Estimates"))




##################################################
##################################################
##################################################
##################################################
##################################################
##################################################
##################################################
##################################################
##################################################
##################################################
##################################################
##################################################
##################################################
##################################################
##################################################




