################################################
# ACOP 2018 Ordinal Logistic Regression Introduction      
# Hands On 
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


###############################################################
###############################################################
#
### ------------------  HANDS ON WORK ----------------------###
#
###############################################################
###############################################################

# using these models
final.polr <- polr(DV ~ EXP+SEX+BIO, data=full)
final.clm <- clm(DV ~ EXP+SEX+BIO, data=full, method="logit")



############################################################
#
### ------------- Hands on question #1a ------------- ###
#
#  Recreate the Residual plots that we made for the base model
#  Using Surrogate residuals
#
#############################################################





##################################
# Which covariate, if any, still shows a relationship with the DV 








############################################################
#
### ------------- Hands on question #1b ------------- ###
#
#  Recreate the Residual plots that we made for the base model
#  Using SBS residuals
#
#############################################################






##################################
# Which covariate, if any, still shows a relationship with the DV 



#########################################################
#
### ------------- Hands on question #2 ------------- ###
#
#  Compute the goodness-of-fit tests to look for evidence 
#  of lack of fit
#
#############################################################





##################################
# Is there evidence of lack of fit?



#########################################################
#
#    ------------- Hands on question #3 ------------- ###
#
#  Test For violation of the proportional odds assumption
#
#############################################################



##################################
# Is there evidence of a violation of the 
# proportional odds assumption


#########################################################
#
#    ------------- Hands on question #4 ------------- ###
#
#  Evaluate the choice of link function
#
#############################################################


##################################
# Is there evidence that the wrong link function was selected





#########################################################
#
#    ------------- Hands on question #5 ------------- ###
#
# Evaluate Proportional odds using 
# individual logistic regressions
#
#############################################################






