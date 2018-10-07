################################################
# ACOP 2018 HO Solutions                       #
#                                              #
################################################
rm(list=ls())

#loading libraries
library(dplyr)
library(ggplot2)
library(gridExtra)
library(GGally)
library(foreign)
library(MASS) 
library(Hmisc)
library(reshape2)
library(ordinal)
library(generalhoslem)
library(plotROC)
library(pROC)


sessionInfo()



######################################################################################
#
#  Load Image
#
######################################################################################

load(file="ExampleOLR_BS_Pred.Rdata")

### ------------- What do we have ------------- ###
objects()
# [1] "AE.name"   "digit.fun" "fit.final" "full" 

# AE.name is the text name for plotting, tables, other cool stuff.
# digit.fun is a fun script for cleaning up the number of digits in table output.
# fit.final is the output model previously used
# full is the data set used in fit.final

### ------------- end of what we have ------------- ###



# to be consistent with plotting etc.
EXP.name <- "CmaxSS"
size<-theme(axis.title.x=element_text(size=20),
            axis.text.x=element_text(size=20),
            axis.title.y=element_text(size=20),
            axis.text.y=element_text(size=20),
            legend.text=element_text(size=20),
            legend.title=element_text(size=20),
            plot.title=element_text(size=20),
            strip.text.x=element_text(size=20),
            strip.text.y=element_text(size=20))


######################################################################################
#
# setup output directory
#
######################################################################################

# add output directory [organization can be helpful] 
origDir <- getwd()
dir.create(paste(origDir,"/Output",sep=""))
outDir <- paste(origDir,"/Output/",sep="")



#####################################################################################
#####################################################################################
#
### ------------- Begin:  Hands on question #1 ------------- ###
#
#####################################################################################

# 1. Perform a bootstrap of your ordinal logistic regression model.  
#    For this model you can arbitrarily use the continuous covariate 
#    BIO2 instead of EXP.


### new model with BIO2
new.model <- clm(DV ~ SEX +BIO2, data = full)
summary(new.model)

#####################################################################################
#
# Begin Bootstrap 
#
####################################################################################
set.seed(2018)  #now you can reproduce your bs if needed

#random samples; easy call
# dataset[sample(nrow(dataset),replace=T),]  #random sample of dataframe with replacement

# input needed
nBS <- 1000  # number of boostraps

# prepare data collection
coef.out <- matrix(nrow=nBS, ncol=length(coef(new.model)))
llkhood.out <- vector(length=nBS)
aic.out <- vector(length=nBS)

# quick loop
for(i in 1:nBS) {
  bsData <- full[sample(nrow(full),replace=T),]  #random sample of dataframe with replacement
  bsMod <- clm(DV ~ SEX + BIO2, data=bsData)
  coef.out[i,] <- coef(bsMod)
  llkhood.out[i] <- logLik(bsMod)
  aic.out[i] <- AIC(bsMod)
}

# bring back to data.frame for easy processing [yes this could also be done upfront]
coef.out <- as.data.frame(coef.out)
names(coef.out) <- names(coef(new.model))

# what quantiles to pull
quantiles <- c(0.05,0.5,0.95)
# conglom <- apply(coef.out,2,quantile,quantiles)
conglom <- cbind(apply(coef.out,2,quantile,quantiles),quantile(llkhood.out,quantiles),quantile(aic.out,quantiles))

origObs <- c(coef(new.model),logLik(new.model),AIC(new.model))
ObsBS <- data.frame(t(rbind(c(coef(new.model),logLik(new.model),AIC(new.model)),conglom)))
names(ObsBS) <- c("Model", "Bootstrap 5%", "Bootstrap 50%", "Bootstrap 95%")
ObsBS$Parameters <- c(names(coef(new.model)),"LogLikelihood","AIC")

# write out 'pretty' output file
BSout <- cbind(ObsBS$Parameters, apply(ObsBS[,1:4],2,digit.fun,3))
write.csv(BSout, paste(outDir,"BootstrapOutput.csv",sep=""),quote = F, row.names = F)

#####################################################################################
#
# End Bootstrap 
#
####################################################################################


# 1. Perform a bootstrap of your ordinal logistic regression model.  
#    For this model you can arbitrarily use the continuous covariate 
#    BIO2 instead of EXP.
#
#    a. How do the bootstrap 90% intervals compare with standard error output?
#
#    b. What are the range of AIC and log likelihood values.


summary(new.model); BSout

# EXTRA CREDIT:  Look for influential subjects


#####################################################################################
#
# Begin Case deletion (Jackknife) EXTRA CREDIT
#
####################################################################################

# use a dataset with ID
fullDat <- full
fullDat$ID <- 1:nrow(fullDat)

# input needed
nID <- nrow(fullDat)  # number of individuals

# prepare data collection
coef.out <- matrix(nrow=nID, ncol=length(coef(new.model)))
llkhood.aic.out <- data.frame(ID=fullDat$ID, 
                              LL=rep(NA, nrow(fullDat)), 
                              aic=rep(NA, nrow(fullDat)))


# quick loop
for(i in 1:nID) {
  jkData <- fullDat[-i,]  #drop an individual each time
  jkMod <- clm(DV ~ SEX + BIO2, data=jkData)
  coef.out[i,] <- coef(jkMod)
  llkhood.aic.out[i,2] <- logLik(jkMod)
  llkhood.aic.out[i,3]  <- AIC(jkMod)
}

head(llkhood.aic.out)
pLL <- ggplot(llkhood.aic.out, aes(x=ID, y=LL)) + geom_line() + geom_point() + size
pLL
dev.print(file=(paste(outDir,AE.name,"_caseDel_LL.png",sep="")), device=png, width=1200 ,height=800)

pAIC <- ggplot(llkhood.aic.out, aes(x=ID, y=aic)) + geom_line() + geom_point() + ylab("AIC") + size
pAIC
dev.print(file=(paste(outDir,AE.name,"_caseDel_AIC.png",sep="")), device=png, width=1200 ,height=800)

#####################################################################################
#
# End Case deletion (Jackknife)  EXTRA CREDIT
#
####################################################################################


#####################################################################################
#
### ------------- END:  Hands on question #1 ------------- ###
#
#####################################################################################
#####################################################################################















#####################################################################################
#####################################################################################
#
### ------------- Begin:  Hands on question #2 ------------- ###
#
#####################################################################################



# 2. Explore preciction objects.  What is the change in predicted 
#      probabilities if exposure is dropped by 10 units?  What is 
#      the odds ratio for having a grade 2 or below AE with this 
#      decreased exposure?
#
#      a. Review the predicted probability across all grades.
#      b. Review the predicted probability for grade 2 or below.
#      c. Calculate odds ratio.
#
#   
#   Extra credit:  bootstrap the odds ratio.  What is the 95% interval?




######################################################################################
#
# Begin: prediction intervals all grades
#
######################################################################################

# what is model object
cvfinal.pred<-clm(DV ~ EXP +SEX, data=full)    # not needed, but done for modularity




###----------------------Prediction exposure example begin-------------------------###

# create new data frame
newdat1 <- data.frame(
  EXP = seq(from=min(full$EXP), to =max(full$EXP), length.out = 100),
  SEX = as.factor(rep(1,100)))

# remember: need to make sure that our datasets are factor for categoricals
newdat <- cbind(newdat1, predict(cvfinal.pred, newdat1, se.fit=TRUE, interval=TRUE))  #default prob
names(newdat)

# notice each Grade fit as output
# > names(newdat)
# [1] "EXP"      "SEX"      "fit.0"    "fit.1"    "fit.2"    "fit.3"    "fit.4"    "se.fit.0"
# [9] "se.fit.1" "se.fit.2" "se.fit.3" "se.fit.4" "lwr.0"    "lwr.1"    "lwr.2"    "lwr.3"   
# [17] "lwr.4"    "upr.0"    "upr.1"    "upr.2"    "upr.3"    "upr.4"     

# some clean up
newdat$ID <- 1:nrow(newdat)
lnewdata <- melt(newdat, measure.vars = paste("fit",0:4,sep="."), 
                 id.vars = c("ID","EXP"))

lnewdatb <- melt(newdat, measure.vars = paste("upr",0:4,sep="."), 
                 id.vars = c("ID","EXP"))

lnewdatc <- melt(newdat, measure.vars = paste("lwr",0:4,sep="."), 
                 id.vars = c("ID","EXP"))

newdatbind <- cbind(lnewdata,upr=lnewdatb$value,lwr=lnewdatc$value)

head(newdatbind)   #oooh, looks good

names(newdatbind)[names(newdatbind)=="variable"]<-"GRADE"
names(newdatbind)[names(newdatbind)=="value"]<-"Probability"

#########################################################################
# predicted probability plot

p.all.grades<-ggplot(newdatbind, aes(x = (EXP), y = Probability)) + 
  geom_ribbon(aes(ymin = lwr,ymax = upr, fill = as.factor(GRADE)), alpha = 0.2) + 
  geom_line(aes(colour =as.factor(GRADE)), size = 1)+
  ylab("Predicted probability")+
  xlab(EXP.name)+
  theme_bw() +
  scale_fill_discrete(name="Grade",
                      breaks=paste("fit",0:4,sep="."),
                      labels=c("0","1", "2", "3","4"))+
  scale_colour_discrete(name="Grade",
                        breaks=paste("fit",0:4,sep="."),
                        labels=c("0","1", "2","3","4"))+
  ggtitle(" ")+
  size
p.all.grades
dev.print(file=(paste(outDir,AE.name,"_clm_predprob_finalmodelEXP.png",sep="")), device=png, width=1200 ,height=800)
###########################################################################

######################################################################################
#
# End: prediction intervals all grades
#
######################################################################################








######################################################################################
#
# Begin: prediction intervals grade 2 and below
#
######################################################################################

newdatLT3 <- newdat
newdatLT3$fitLT3 <- newdatLT3$fit.0+newdatLT3$fit.1+newdatLT3$fit.2
newdatLT3$fitGE3 <- newdatLT3$fit.3+newdatLT3$fit.4
newdatLT3$uprLT3 <- newdatLT3$upr.0+newdatLT3$upr.1+newdatLT3$upr.2
newdatLT3$uprGE3 <- newdatLT3$upr.3+newdatLT3$upr.4
newdatLT3$lwrLT3 <- newdatLT3$lwr.0+newdatLT3$lwr.1+newdatLT3$lwr.2
newdatLT3$lwrGE3 <- newdatLT3$lwr.3+newdatLT3$lwr.4

lnewdataLT <- melt(newdatLT3, measure.vars = c("fitLT3","fitGE3"), 
                 id.vars = c("ID","EXP","SEX"))

lnewdatbLT <- melt(newdatLT3, measure.vars = c("uprLT3","uprGE3"), 
                 id.vars = c("ID","EXP"))

lnewdatcLT <- melt(newdatLT3, measure.vars = c("lwrLT3","lwrGE3"), 
                 id.vars = c("ID","EXP"))

newdatbindLT <- cbind(lnewdataLT,upr=lnewdatbLT$value,lwr=lnewdatcLT$value)

head(newdatbindLT)

names(newdatbindLT)[names(newdatbindLT)=="variable"]<-"GRADE"
names(newdatbindLT)[names(newdatbindLT)=="value"]<-"Probability"

# probability maxes out at 100%
newdatbindLT$upr[newdatbindLT$upr>1] <- 1

#########################################################################
# predicted probability plot

pLT3<-ggplot(newdatbindLT, aes(x = (EXP), y = Probability)) + 
  geom_ribbon(aes(ymin = lwr,ymax = upr, fill = as.factor(GRADE)), alpha = 0.2) + 
  geom_line(aes(colour =as.factor(GRADE)), size = 1)+
  ylab("Predicted probability")+
  xlab(EXP.name)+
  theme_bw() +
  scale_fill_discrete(name="Grade",
                      breaks=c("fitLT3","fitGE3"),
                      labels=c("LT3","GE3"))+
  scale_colour_discrete(name="Grade",
                        breaks=c("fitLT3","fitGE3"),
                        labels=c("LT3","GE3"))+
  ggtitle(" ")+
  size 
pLT3
dev.print(file=(paste(outDir,AE.name,"_clm_predprob_Grade3_M_F.png",sep="")), device=png, width=1200 ,height=800)
###########################################################################


######################################################################################
#
# End: prediction intervals grade 2 and below
#
######################################################################################






######################################################################################
#
# Begin: Odds ratio for drop in 10 units of exposure
#
######################################################################################

###########################################################################
### create male, female and 30 or 60
newdat3 <- data.frame(
  EXP = c(30,30,40,40),
  SEX = as.factor(rep(1:2,2)))

#need to make sure that our datasets are factor for categoricals

newdat <- cbind(newdat3, predict(cvfinal.pred, newdat3, 
                                 se.fit=TRUE, interval=TRUE))
# names(newdat)

newdat$pLT3 <- newdat$fit.0 + newdat$fit.1 + newdat$fit.2
newdat$oddLT3 <- newdat$pLT3/(1-newdat$pLT3)

# With drop in 10 units of exposure, what is OR?
OR.drop10 <- newdat$oddLT3[newdat$EXP==30&newdat$SEX==1]/
  newdat$oddLT3[newdat$EXP==40&newdat$SEX==1]
OR.drop10


######################################################################################
#
# End: Odds ratio for drop in 10 units of exposure
#
######################################################################################




######################################################################################
#
# Begin: Odds ratio for drop in 10 units of exposure  EXTRA CREDIT BS
#
######################################################################################
# input needed
# seed was set earlier, or can be reset now

nBS <- 1000  # number of boostraps

# prepare data collection
coef.out <- matrix(nrow=nBS, ncol=length(coef(fit.final)))
llkhood.out <- vector(length=nBS)
aic.out <- vector(length=nBS)
or.drop10.out <- vector(length = nBS)
# or.30.out <- vector(length = nBS)

# quick loop
for(i in 1:nBS) {
  bsData <- full[sample(nrow(full),replace=T),]  #random sample of dataframe with replacement
  bsMod <- clm(DV ~ EXP + SEX, data=bsData)
  coef.out[i,] <- coef(bsMod)
  llkhood.out[i] <- logLik(bsMod)
  aic.out[i] <- AIC(bsMod)
  predNewDat <- cbind(newdat3, predict(bsMod, newdat3, se.fit=TRUE, interval=TRUE))
  predNewDat$pLT3 <- predNewDat$fit.0 + predNewDat$fit.1 + predNewDat$fit.2
  predNewDat$oddLT3 <- predNewDat$pLT3/(1-predNewDat$pLT3)
  or.drop10.out[i] <- predNewDat$oddLT3[predNewDat$SEX==1&predNewDat$EXP==30]/predNewDat$oddLT3[predNewDat$SEX==1&predNewDat$EXP==40]
    }

# bring back 
# what quantiles to pull
quantiles <- c(0.05,0.5,0.95)
orDrop10BS <- quantile(or.drop10.out,quantiles)
OR.drop10; orDrop10BS

# > OR.drop10; orDrop10BS
# [1] 1.717816
# 5%      50%      95% 
# 1.375812 1.726070 2.190727 

######################################################################################
#
# End: Odds ratio for drop in 10 units of exposure  EXTRA CREDIT BS
#
######################################################################################


#####################################################################################
#
### ------------- End:  Hands on question #2 ------------- ###
#
#####################################################################################
#####################################################################################




















#####################################################################################
#####################################################################################
#
### ------------- Begin:  Hands on question #3 ------------- ###
#
#####################################################################################

# 3. Explore the predictive nature of the full model using cross-validation.
#    a. What does the true vs predicted matrix ('Confusion Matrix') look like?  
#    b. Generate the ROC curve for Grade 0 prediction.



#####################################################################################
#
# Begin: Cross-Validation:  Confusion matrix...
#
####################################################################################

# # Cross validation via rough code
# look at full model
cvpred.mod <- clm(DV ~ EXP + SEX + BIO, data=full)
summary(cvpred.mod)


# parameters for cross validation
nCross <- 10
folds <- cut(seq(1,nrow(full)),breaks=nCross,labels=F)

# capture output
df <- full  # grab the previous data
df$ID <- 1:nrow(df)  #label 
result <- list()
modelFiles <- list()
CVcoef <- data.frame(matrix(nrow=length(coef(cvpred.mod)), ncol=nCross))

for(i in 1:nCross){
  testIndexes <- which(folds==i,arr.ind = T)
  testData <- df[testIndexes,] 
  names(testData)[8] <- "obsDV"
  trainData <- df[-testIndexes,]
  fin.model  <- clm(DV ~ EXP + SEX + BIO, data=trainData)
  modelFiles[[i]] <- fin.model
  CVcoef[,i] <- coef(fin.model)
  predRes <- cbind(testData,predict(fin.model, testData, se.fit = TRUE, interval = TRUE, type="prob"),
                   predict(fin.model, testData, type="class"))
  predRes$nObsDV <- as.numeric(as.character(predRes$obsDV))
  predRes$nPredDV <- as.numeric(as.character(predRes$fit))
  result[[i]] <- predRes
}

# are the CV training models similar to final model?  Coudl be issue if there are highly influential observations
CVcoef$Final <- coef(cvpred.mod)
CVcoef$mean <- apply(CVcoef,1,mean,na.rm=T)
CVcoef$SD <- apply(CVcoef[,1:nCross],1,sd,na.rm=T)
CVcoef

revCVcoef <- CVcoef
row.names(revCVcoef) <- names(coef(cvpred.mod))
revCVcoef[,c("Final","mean","SD")]
# cross validation parameters line up with final fitted model, can use the CV for validation

tCVcoef <- as.data.frame(t(CVcoef[,1:nCross]))
# class(tCVcoef);str(tCVcoef)
names(tCVcoef) <- paste("parameter",1:7,sep="")

testCV <- ggplot(tCVcoef,aes(x = parameter1)) + geom_density() + geom_vline(xintercept = coef(cvpred.mod)[1],size=3)+ size
testCV
dev.print(file=(paste(outDir,"crossValPar1.png",sep="")), device=png, width=1200 ,height=800)

########################################################################
# ok to go to the results
########################################################################
# add combo of model outputs to confirm no big discrepancies
CVout <- dplyr::bind_rows(result)
dim(CVout)


########################################################################################
# Looking at all of the cross validation predictions
# matrix for all output
nGrades <- length(unique(CVout$nObsDV))

fullConfMat <- matrix(nrow=nGrades,ncol=nGrades)
# fullConfMat <- matrix(nrow=length(unique(CVout$nObsDV)),ncol=length(unique(CVout$nObsDV)))

for(i in 1:nGrades) {
  for(j in 1:nGrades) {
    fullConfMat[i,j] <- sum(CVout$nObsDV==(j-1)&CVout$nPredDV==(i-1))    
  }
}
fullConfMat
fullConfMat <- data.frame(fullConfMat)
fullConfMat$Totals <- apply(fullConfMat,1,sum)
fCM <- rbind(fullConfMat,apply(fullConfMat,2,sum))
names(fCM) <- c(paste("True Grade ",0:4,sep=""), "Totals")
row.names(fCM) <- c(paste("Pred Grade ",0:4,sep=""), "Totals")

# write out in pretty format with true positive rate
Sensitivity <- 100*(diag(as.matrix(fCM))/fCM[nrow(fCM),])  # true positive rate
SensOut <- paste(digit.fun(Sensitivity,3),"%", sep="")  #clean up
fCM.out <- rbind(fCM,SensOut)
row.names(fCM.out) <- c(paste("Pred Grade ",0:4,sep=""), "Totals","True Positive Rate")

fCM.out #check, yes looks ok
write.csv(fCM.out,paste(outDir,"fullConfMatrix.csv",sep=""),quote=F,row.names = T)
########################################################################################


####################################################################################
# Grade level ROC

# Grade 0
TrueObs <- ifelse(CVout$nObsDV==0,1,0)
fitPred <- CVout$fit.0  # all probs grade 0
ROCdata <- data.frame(obs = TrueObs, pred = fitPred)

ROCout <- ggplot(ROCdata, aes(d=obs, m=pred)) + geom_roc(labels = F, pointsize = 0)+ 
  geom_abline(intercept = 0, slope = 1, linetype=2) +
  labs(title = "Grade 0", x = "1 - Specificity", y = "Sensitivity")+ size

ROCout 
# if you want the AUC for ROC
auc(ROCdata$obs,ROCdata$pred)

dev.print(file=(paste(outDir,"ROCgrade0.png",sep="")), device=png, width=1200 ,height=800)
####################################################################################


#####################################################################################
#
### ------------- Begin:  Hands on question #3 ------------- ###
#
#####################################################################################
#####################################################################################



#########################  --- end ---  #########################