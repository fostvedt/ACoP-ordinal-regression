#' ---     
#' title: "Modeling Ordinal Data: Model Building Hands On"   
#' author: DJNickens   
#' date: 29 Sept 2018   
#' output: pdf_document   
#' ---    

#' # Preparation for Analysis   

rm(list=ls())

#' ## Libraries
library(MASS)       #' polr() function , stepAIC() function     
library(ordinal)    #' clm() function   
library(dplyr)      #' data transformation functions   
library(boot)       #' logit(), inv.logit()  functions
library(ggplot2)    #' plotting functions      
library(GGally)     #' scatterplot matrices   



#' ## Functions   
stuq <- function(x) { sort(unique(x)) }
indicator <- function(condition) { ifelse(condition,1,0) }
mean_2 <- function(x) { round(mean(x,na.rm=T),2) }
median_2 <- function(x) { round(median(x,na.rm=T),2) }
min_2 <- function(x) { round(min(x,na.rm=T),2) }
max_2 <- function(x) { round(max(x,na.rm=T),2) }
sum_2 <- function(x) { sum(x,na.rm=T) }
nobs_2 <- function(x) { sum(!is.na(x)) }

AICr <- function(minModel,model) { 
           # function to compute relative likelihood   
           AICr <- exp((as.numeric(as.character(minModel$info$AIC))-
                          as.numeric(as.character(model$info$AIC)))/2)
           return(AICr) 
}


#' ## Data   
#' wd <- "C:/...";   setwd(wd)   
getwd() 

dsn <- "AEs-ordinal-data-ver3.csv"
d <- read.csv(dsn, as.is=T)
d$AEGrade <- as.factor(d$DV)
d$Sex <- factor(d$SEX,levels=c(1,2), labels=c("Male","Female"))
d$Smoker <- factor(d$SMOK,levels=c(1,2,3), labels=c("Current","Former","Never"))
d$Dose <- as.factor(d$DOSE)

head(d)

summary(d)


#' # Question 1: 

#'  
#' **Create at least 1 graph (a) to explore relationships between covariates and**   
#' **(b) between covariates and the response.**  
#'   


#' ## Observed Covariate-to-Covariate Relationships    

#' ### Continuous Variables
gg <- ggpairs(d[,c(2,3,5,6,8,9,10)],lower=list(continuous='smooth_loess'), 
              diag=list(continuous='density'), 
              axisLabels = 'show')
ggsave("Cov_Cov_02.png",gg,height=9,width=12)   
#' ![Figure 1](Cov_Cov_02.png)   
#' **Figure 1**.  *Covariate Relationships - Continuous.*   
#'    


#' ### Catergorcal  
gg <- ggpairs(data=d,columns=c(3,5:6,9,10,13), title = "By Sex",
              upper = list(continuous = "cor"),
              diag = list(continuous= "density"),
              lower = list(continuous = "smooth_loess", mapping=aes(colour = Sex)),
              axisLabels="show")
ggsave("Cov_Cov_10.png",gg,height=9,width=12)
#' ![Figure 2](Cov_Cov_10.png)   
#' **Figure 2**.  *Covariate Relationships - By Sex.*   
#'   


gg <- ggpairs(data=d,columns=c(3,5:6,9,10,14), title = "By Smoker",
              upper = list(continuous = "cor"),
              diag = list(continuous= "density"),
              lower = list(continuous = "smooth_loess", mapping=aes(colour = Smoker)),
              axisLabels="show")
ggsave("Cov_Cov_12.png",gg,height=9,width=12)
#' ![Figure 3](Cov_Cov_12.png)   
#' **Figure 3**.  *Covariate Relationships - By Smoker.*   
#'   


#' ## Observed Covariate-to-Response Relationships  

#' ### Continuous   

#' Frequency & cumulative sum by DOSE   
d1 <- d %>% group_by(DOSE,DV) %>% summarize(N.AEGrade=nobs_2(DV)) %>% 
            mutate(cFreq = cumsum(N.AEGrade)) %>% 
            group_by(DOSE) %>% mutate(N.Tot = sum_2(N.AEGrade))


d1$AEGrade <- as.factor(d1$DV)
d1$p <- d1$N.AEGrade/d1$N.Tot
d1$cp <- d1$cFreq/d1$N.Tot
d1$Dose <- as.factor(d1$DOSE)

gg <- ggplot(data=d1, aes(x=AEGrade, y=cp, colour=Dose, group=Dose)) +
  geom_point() +
  geom_line() +
  theme(axis.text.x  = element_text(angle=0, colour="black", vjust=0, size=20),
        axis.text.y  = element_text(angle=0, colour="black", vjust=0, size=20),
        axis.title.x = element_text(size=20),
        axis.title.y = element_text(size=20),
        legend.position ="bottom",
        legend.title=element_text(colour="black", size=18, face="bold"),
        legend.text = element_text(colour="black", size=18, face="bold")) +
  xlab("AE Grade") + 
  ylab("Cumulative Probability")
ggsave("Cov_Resp_1_02.png",gg,height=9,width=12)
#' ![Figure 4](Cov_Resp_1_02.png)   
#' **Figure 4**.  *Cumulative Probablity Plot - AE Grade vs Dose.*   
#'  
 

gg <- ggplot(data=d, aes(x=AEGrade, y=DOSE, colour=AEGrade)) + 
  geom_boxplot(fill="white", alpha=0.5) +
  # geom_point(pch=0, size=4) +
  geom_jitter(pch=1, size=5, width = 0.05) +
  theme(axis.text.x  = element_text(angle=0, colour="black", vjust=0, size=20),
        axis.text.y  = element_text(angle=0, colour="black", vjust=0, size=20),
        axis.title.x = element_text(size=20),
        axis.title.y = element_text(size=20),
        legend.position ="none") + 
  xlab("AEGrade") + 
  ylab("Dose (mg)")
ggsave("Cov_Resp_1_04.png",gg,height=9,width=12)
#' ![Figure 5](Cov_Resp_1_04.png)     
#' **Figure 5**.  *AE Grade vs Dose.*   
#'  


#' Frequency & cumulative sum by AGE   

d2 <- d %>% mutate(q20 = quantile(AGE, prob=0.2), 
                   q40 = quantile(AGE, prob=0.4),
                   q60 = quantile(AGE, prob=0.6),
                   q80 = quantile(AGE, prob=0.8))

d2$AGEgn <- 1
d2$AGEgn[d2$AGE>d2$q20 & d2$AGE<=d2$q40] <- 2
d2$AGEgn[d2$AGE>d2$q40 & d2$AGE<=d2$q60] <- 3
d2$AGEgn[d2$AGE>d2$q60 & d2$AGE<=d2$q80] <- 4
d2$AGEgn[d2$AGE>d2$q8] <- 5

d2 <- d2 %>% group_by(AGEgn) %>% mutate(AGEgmin = min_2(AGE), AGEgm = median_2(AGE), 
                                        AGEgmax = max_2(AGE))
d2$AGE_Group <- paste(d2$AGEgmin,d2$AGEgmax,sep=" - ")

d3 <- d2 %>% group_by(AGE_Group,DV) %>% summarize(N.AEGrade=nobs_2(DV)) %>% 
             mutate(cFreq = cumsum(N.AEGrade)) %>% 
             group_by(AGE_Group) %>% mutate(N.Tot = sum_2(N.AEGrade))

d3$AEGrade <- as.factor(d3$DV)
d3$p <- d3$N.AEGrade/d3$N.Tot
d3$cp <- d3$cFreq/d3$N.Tot
d3$Age <- as.factor(d3$AGE_Group)

gg <- ggplot(data=d3, aes(x=AEGrade, y=cp, colour=Age, group=Age)) +
  geom_point() +
  geom_line() +
  theme(axis.text.x  = element_text(angle=0, colour="black", vjust=0, size=20),
        axis.text.y  = element_text(angle=0, colour="black", vjust=0, size=20),
        axis.title.x = element_text(size=20),
        axis.title.y = element_text(size=20),
        legend.position ="bottom",
        legend.title=element_text(colour="black", size=18, face="bold"),
        legend.text = element_text(colour="black", size=18, face="bold")) +
  xlab("AE Grade") + 
  ylab("Cumulative Probability")
ggsave("Cov_Resp_1_08.png",gg,height=9,width=12)
#' ![Figure 6](Cov_Resp_1_08.png)   
#' **Figure 6**.  *Cumulative Probablity Plot - AE Grade vs Age.*   
#'  


gg <- ggplot(data=d, aes(x=AEGrade, y=AGE, colour=AEGrade)) + 
  geom_boxplot(fill="white", alpha=0.5) +
  # geom_point(pch=0, size=4) +
  geom_jitter(pch=1, size=5, width = 0.05) +
  theme(axis.text.x  = element_text(angle=0, colour="black", vjust=0, size=20),
        axis.text.y  = element_text(angle=0, colour="black", vjust=0, size=20),
        axis.title.x = element_text(size=20),
        axis.title.y = element_text(size=20),
        legend.position ="nonem") +
  xlab("AEGrade") + 
  ylab("Age (yrs)")
ggsave("Cov_Resp_1_10.png",gg,height=9,width=12)
#' ![Figure 7](Cov_Resp_1_10.png)   
#' **Figure 7**.  *AE Grade vs Age.*   
#'  


#' ### Categorical   

#' Frequency & cumulative sum by SEX   
d2 <- d %>% group_by(SEX,DV) %>% summarize(N.AEGrade=length(DV)) %>% 
            mutate(cFreq = cumsum(N.AEGrade)) %>% 
            group_by(SEX) %>% mutate(N.Tot = sum(N.AEGrade))

d2$AEGrade <- as.factor(d2$DV)
d2$p <- d2$N.AEGrade/d2$N.Tot
d2$cp <- d2$cFreq/d2$N.Tot
d2$Sex <- factor(d2$SEX,levels=c(1,2), labels=c("Male","Female"))

gg <- ggplot(data=d2, aes(x=AEGrade, y=cp, colour=Sex, group=Sex)) +
  geom_point() +
  geom_line() +
  theme(axis.text.x  = element_text(angle=0, colour="black", vjust=0, size=20),
        axis.text.y  = element_text(angle=0, colour="black", vjust=0, size=20),
        axis.title.x = element_text(size=20),
        axis.title.y = element_text(size=20),
        legend.position ="bottom",
        legend.title=element_text(colour="black", size=18, face="bold"),
        legend.text = element_text(colour="black", size=18, face="bold")) +
  xlab("AE Grade") + 
  ylab("Cumulative Probability")
ggsave("Cov_Resp_2_01.png",gg,height=9,width=12)
#' ![Figure 8](Cov_Resp_2_01.png)   
#' **Figure 8**.  *Cumulative Probablity Plot - AE Grade vs Sex.*   
#'  


gg <- ggplot(data=d, aes(x=Sex, y=DV, colour=AEGrade)) + 
  geom_point() +
  # geom_point(pch=0, size=4) +
  geom_jitter(pch=1, size=5, width = 0.05) +
  theme(axis.text.x  = element_text(angle=0, colour="black", vjust=0, size=20),
        axis.text.y  = element_text(angle=0, colour="black", vjust=0, size=20),
        axis.title.x = element_text(size=20),
        axis.title.y = element_text(size=20),
        legend.position ="none") +
  xlab("Sex") + 
  ylab("AE Grade")
ggsave("Cov_Resp_2_03.png",gg,height=9,width=12)
#' ![Figure 9](Cov_Resp_2_03.png)   
#' **Figure 9**.  *AE Grade vs Sex.*   
#'  


#' Frequency & cumulative sum by SMOKER   

d3 <- d %>% group_by(SMOK,DV) %>% summarize(N.AEGrade=length(DV)) %>% 
            mutate(cFreq = cumsum(N.AEGrade)) %>% 
            group_by(SMOK) %>% mutate(N.Tot = sum(N.AEGrade))

d3$AEGrade <- as.factor(d3$DV)
d3$p <- d3$N.AEGrade/d3$N.Tot
d3$cp <- d3$cFreq/d3$N.Tot
d3$Smoker <- factor(d3$SMOK,levels=c(1,2,3), labels=c("Current","Former","Never"))


gg <- ggplot(data=d3, aes(x=AEGrade, y=cp, colour=Smoker, group=Smoker)) +
  geom_point() +
  geom_line() +
  theme(axis.text.x  = element_text(angle=0, colour="black", vjust=0, size=20),
        axis.text.y  = element_text(angle=0, colour="black", vjust=0, size=20),
        axis.title.x = element_text(size=20),
        axis.title.y = element_text(size=20),
        legend.position ="bottom",
        legend.title=element_text(colour="black", size=18, face="bold"),
        legend.text = element_text(colour="black", size=18, face="bold")) +
  xlab("AE Grade") + 
  ylab("Cumulative Probability")
ggsave("Cov_Resp_2_04.png",gg,height=9,width=12)
#' ![Figure 10](Cov_Resp_2_04.png)   
#' **Figure 10**.  *Cumulative Probablity Plot - AE Grade vs Smoker.*   
#'  


gg <- ggplot(data=d, aes(x=Smoker, y=DV, colour=AEGrade)) + 
  geom_point() +
  # geom_point(pch=0, size=4) +
  geom_jitter(pch=1, size=5, width = 0.05) +
  theme(axis.text.x  = element_text(angle=0, colour="black", vjust=0, size=20),
        axis.text.y  = element_text(angle=0, colour="black", vjust=0, size=20),
        axis.title.x = element_text(size=20),
        axis.title.y = element_text(size=20),
        legend.position ="none") +
  xlab("Smoker") + 
  ylab("AE Grade")
ggsave("Cov_Resp_2_06.png",gg,height=9,width=12)
#' ![Figure 11](Cov_Resp_2_06.png)   
#' **Figure 11**.  *AE Grade vs Smoker.*   
#'  


#' # Question 2:

#' **Do a scatter plot for the 3 exposure measures.**  
#' **Which measure would you choose for further covariate model building?**
#'    
  
gg <- ggpairs(d[,c(2,8,9)],lower=list(continuous='smooth_loess'), 
              diag=list(continuous='density'), 
              axisLabels = 'show')
ggsave("Cov_Cov_04.png",gg,height=9,width=12) 
#' ![Figure 12](Cov_Cov_04.png)   
#' **Figure 12**.  *Covariate Relationships - Exposure.*   
#'   


#' # Question 3   

#' **With CMAXSS as the exposure measure in the base model, use stepAIC() to do a complete**    
#' **forward selection followed by a complete backward selection to select a final model.**   
#'    


#' ## Models   
#'  

#' Exposure = CMAXSS   
d$EXP <- d$CMAXSS


#' Null Model   
mod0 <- clm(AEGrade ~ 1 , data = d, link = "logit", threshold="flex", Hess=T)
summary(mod0, digits = 3)

inv.logit(mod0$coef[1:4])


#' Base Model   
mod1 <- clm(AEGrade~ EXP , data = d, link = "logit", threshold="flex", Hess=T)
summary(mod1, digits = 3)

exp(mod1$beta)
inv.logit(mod1$coef[1:4])


#' Full Model   
mod2 <- clm(AEGrade ~ EXP + BIO + SEX + AGE + WT + SMOK + BIO2 , data = d, 
            link = "logit", threshold="flex", Hess=T)
summary(mod2)

exp(mod2$beta)
inv.logit(mod2$coef[1:4])


#' ## Forward & Backward Selection Separately    

#' Forward Selection: Start with Base model   
stepAIC(mod1, scope=list(lower=~EXP, upper=~EXP + Sex + AGE + WT + Smoker + BIO + BIO2), 
        direction="forward", trace=1, k=2)


#' Model from forward selection   
mod3 <- clm(AEGrade ~ EXP + BIO + Sex, data = d, link = "logit", threshold="flex", Hess=T)
summary(mod3)


#' Backward Selection: Start with final forward model   
summary(stepAIC(mod3, scope=list(upper=~EXP + BIO + Sex), direction="backward", trace=1, k=2))


#'   
#' **Model 3 is the Final Model**   
#'   

#' # Question 4:   

#' **Comparison of Models Using Relative Likelihood**  
#'       

#' Base model vs Final model   
AICr(mod3,mod1)


#' Full model vs Final model   
AICr(mod3,mod2)

#' # BONUS Question:

#' **Using clm() with exposure, is there a way to use AIC to help decide which**    
#' **exposure measure to use for the AE grades data?**   
#'    

#' Compare AIC and relative likelihood for a base model for each exposure measure.   

#' DOSE as exposure   
mod4 <- clm(AEGrade ~ DOSE, data = d, link = "logit", threshold="flex", Hess=T)
summary(mod4)


#' CAVGSS as exposure   
mod5 <- clm(AEGrade ~ CAVGSS, data = d, link = "logit", threshold="flex", Hess=T)
summary(mod5)


#' CMAXSS as exposure   
mod6 <- clm(AEGrade ~ CMAXSS, data = d, link = "logit", threshold="flex", Hess=T)
summary(mod6)


#' Relative Likelihood, CAVGSS vs DOSE   
AICr(mod5,mod4)
     

#' Relative Likelihood, CMAXSS vs DOSE   
AICr(mod6,mod4)


#' Relative Likelihood, CMAXSS vs CAVGSS      
AICr(mod6,mod5)


#' ***fini***   
#'    
