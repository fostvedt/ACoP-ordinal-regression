################################################
# ACOP 2018 log reg analysis Introduction      #
#                                              #
################################################
#setwd("H:/My Documents/blabla")

#loading libraries
library(dplyr)
library(ggplot2)
library(gridExtra)
library(MASS) 
library(ordinal)
library(generalhoslem)
sessionInfo()

# R version 3.4.1 (2017-06-30)
# Platform: x86_64-w64-mingw32/x64 (64-bit)
# Running under: Windows >= 8 x64 (build 9200)
# 
# Matrix products: default
# 
# locale:
#   [1] LC_COLLATE=English_United States.1252  LC_CTYPE=English_United States.1252   
# [3] LC_MONETARY=English_United States.1252 LC_NUMERIC=C                          
# [5] LC_TIME=English_United States.1252    
# 
# attached base packages:
#   [1] stats     graphics  grDevices utils     datasets  methods   base     
# 
# other attached packages:
#   [1] generalhoslem_1.3.1 reshape_0.8.7       ordinal_2015.6-28   reshape2_1.4.2      Formula_1.2-2      
# [6] survival_2.41-3     lattice_0.20-35     MASS_7.3-47         foreign_0.8-69      GGally_1.3.2       
# [11] gridExtra_2.3       ggplot2_2.2.1       dplyr_0.7.4        
# 
# loaded via a namespace (and not attached):
#   [1] Rcpp_0.12.14        compiler_3.4.1      RColorBrewer_1.1-2  plyr_1.8.4          bindr_0.1          
# [6] base64enc_0.1-3     tools_3.4.1         tibble_1.3.4        gtable_0.2.0        ucminf_1.1-4       
# [11] pkgconfig_2.0.1     rlang_0.1.4         Matrix_1.2-10       yaml_2.1.14         bindrcpp_0.2       
# [16] stringr_1.2.0       grid_3.4.1          glue_1.2.0          R6_2.2.2            latticeExtra_0.6-28
# [21] magrittr_1.5        scales_0.5.0        splines_3.4.1       assertthat_0.2.0    colorspace_1.3-2   
# [26] stringi_1.1.6       acepack_1.4.1       lazyeval_0.2.1      munsell_0.4.3

################setting for graphics 

size<-theme(axis.title.x=element_text(size=20), 
            axis.text.x=element_text(size=20), 
            axis.title.y=element_text(size=20),
            axis.text.y=element_text(size=20), 
            legend.text=element_text(size=20),
            legend.title=element_text(size=20),
            plot.title=element_text(size=20),
            strip.text.x=element_text(size=20),
            strip.text.y=element_text(size=20))


#------------------------------------------------------------------------
#Load data file
#------------------------------------------------------------------------
#Source Data
filename<-"AEs-ordinal-data-ver3.csv"

dat <- read.csv(filename,na.strings=".",stringsAsFactors=F, header=T)

####################################################################################################
######################DATA EXPLORATION##############################################################
####################################################################################################

names(dat)

# [1] "ID"     "DOSE"   "BIO"    "SEX"    "AGE"    "WT"     "SMOK"   "CMAXSS" "CAVGSS" "BIO2"   "DV"  


summary(dat)

# ID              DOSE            BIO               SEX             AGE              WT        
# Min.   :  1.00   Min.   :  0.0   Min.   : 0.9467   Min.   :1.000   Min.   :19.00   Min.   : 41.50  
# 1st Qu.: 50.75   1st Qu.: 10.0   1st Qu.:12.0209   1st Qu.:2.000   1st Qu.:48.00   1st Qu.: 62.00  
# Median :100.50   Median : 30.0   Median :16.1634   Median :2.000   Median :56.00   Median : 71.35  
# Mean   :100.50   Mean   : 40.9   Mean   :16.4975   Mean   :1.785   Mean   :53.65   Mean   : 73.26  
# 3rd Qu.:150.25   3rd Qu.: 50.0   3rd Qu.:20.8966   3rd Qu.:2.000   3rd Qu.:61.00   3rd Qu.: 82.12  
# Max.   :200.00   Max.   :100.0   Max.   :33.1072   Max.   :2.000   Max.   :73.00   Max.   :128.40  
# SMOK           CMAXSS           CAVGSS            BIO2              DV      
# Min.   :1.000   Min.   : 0.000   Min.   : 0.000   Min.   : 9.199   Min.   :0.00  
# 1st Qu.:1.000   1st Qu.: 7.124   1st Qu.: 4.748   1st Qu.:30.721   1st Qu.:1.00  
# Median :1.000   Median :14.991   Median :10.334   Median :39.101   Median :2.00  
# Mean   :1.375   Mean   :15.096   Mean   : 9.347   Mean   :40.725   Mean   :1.71  
# 3rd Qu.:2.000   3rd Qu.:20.696   3rd Qu.:12.790   3rd Qu.:50.189   3rd Qu.:2.00  
# Max.   :3.000   Max.   :63.131   Max.   :21.403   Max.   :76.248   Max.   :4.00  


###NAME THE ADVERSE EVENT HERE###
AE.name<-"RENAL TOXICITY"

plot1<-ggplot(dat, aes(x = DV, y = CMAXSS, colour=as.factor(DV))) +
  geom_boxplot(size=0.75,outlier.shape = NA)+
  geom_jitter(aes(colour=as.factor(DV)),size=2,alpha=0.6)+
  facet_grid(SEX~DOSE)+
  theme_bw() +
  size+
  ylab("CMAXSS")+
  scale_colour_discrete(name="Grades")+
  xlab(paste(AE.name, "Grade"))
plot1  

dev.print(file=(paste(AE.name,"_boxplot1.png")), device=png, width=1200 ,height=800)


plot2<-ggplot(dat, aes(x = DV, y = CMAXSS, colour=as.factor(DV))) +
  geom_boxplot(size=0.75,outlier.shape = NA)+
  geom_jitter(aes(colour=as.factor(DV)),size=2,alpha=0.6)+
  theme_bw() +
  facet_grid(.~SEX)+
  size+
  ylab("CMAXSS")+
  scale_colour_discrete(name="Grades")+
  xlab(paste(AE.name, "Grade"))
plot2  

dev.print(file=(paste(AE.name,"_boxplot2.png")), device=png, width=1200 ,height=800)

# For exposure-response analysis, remove placebo group
d<-dat%>%subset(DOSE>0)%>%
  mutate(lCMAXSS=log(CMAXSS+0.0001),lCAVGSS=log(CAVGSS+0.0001),lWT=log(WT),
         lAGE=log(AGE),lBIO=log(BIO),lBIO2=log(BIO2),lDOSE=log(DOSE))

#Convert categorical variables to factors
catvar<-c("DV","SEX","SMOK")



d[catvar]<-lapply(d[catvar],as.factor)

#distribution of variables

p1<-ggplot(data=d, aes(d$WT)) + 
  geom_histogram(colour="black", fill="blue")+
  xlab("Body Weight (kg)")+size

p2<-ggplot(data=d, aes(d$lWT)) + 
  geom_histogram(colour="black", fill="blue")+
  xlab("Logarithm of Body Weight (kg)")+size

p3<-ggplot(data=d, aes(d$AGE)) + 
  geom_histogram(colour="black", fill="blue")+
  xlab("Age (years)")+size

p4<-ggplot(data=d, aes(d$lAGE)) + 
  geom_histogram(colour="black", fill="blue")+
  xlab("Logarithm of Age (years)")+size

p5<-ggplot(data=d, aes(d$BIO)) + 
  geom_histogram(colour="black", fill="blue")+
  xlab("BIO")+size

p6<-ggplot(data=d, aes(d$lBIO)) + 
  geom_histogram(colour="black", fill="blue")+
  xlab("Logarithm of BIO")+size

p7<-ggplot(data=d, aes(d$BIO2)) + 
  geom_histogram(colour="black", fill="blue")+
  xlab("BIO2")+size

p8<-ggplot(data=d, aes(d$lBIO2)) + 
  geom_histogram(colour="black", fill="blue")+
  xlab("Logarithm of BIO2")+size

grid.arrange(p1,p2,p3,p4,p5,p6,p7,p8, ncol=4)

dev.print(file="distributioncov.png", device=png, width=1200,height=800)



e1<-ggplot(data=d, aes(d$CMAXSS)) + 
  geom_histogram(colour="black", fill="blue")+
  xlab("Cmaxss (ng/mL)")+size

e2<-ggplot(data=d, aes(d$lCMAXSS)) + 
  geom_histogram(colour="black", fill="blue")+
  xlab("Logarithm of Cmaxss (ng/mL)")+size

e3<-ggplot(data=d, aes(d$CAVGSS)) + 
  geom_histogram(colour="black", fill="blue")+
  xlab("Cavgss (ng/mL)")+size

e4<-ggplot(data=d, aes(d$lCAVGSS)) + 
  geom_histogram(colour="black", fill="blue")+
  xlab("Logarithm of Cavgss (ng/mL)")+size



grid.arrange(e1,e2,e3,e4, ncol=2)

dev.print(file="distributionexp.png", device=png, width=1200,height=800)

#################################

table<-t(data.frame(xtabs(~DV, data=d)))

table
# [,1] [,2] [,3] [,4] [,5]
# DV   "0"  "1"  "2"  "3"  "4" 
# Freq "20" "45" "64" "14" "18"

write.csv(table,file=(paste(AE.name,"_grades.csv")),quote=F,row.names=F)


head(d)
dim(d) 
names(d)
str(d)


d$EXP<-d$CMAXSS
EXP.name<-expression(paste("Cmax at steady state (",mu,"g/L)"))



#summary of exp variable
asc<-function(x){as.numeric(as.character(x))}


n1 = function(x){
  (length(x))
}

n2= function(x){
  (sum(is.na(x)))
}


#summary by exposure
sum<-d%>%
  group_by(DV)%>%
  summarise(EXPmean=mean(EXP,na.rm=TRUE),EXPsd=sd(EXP,na.rm=TRUE),
            EXPmed=median(EXP,na.rm=TRUE),EXPmax=max(EXP,na.rm=TRUE),
            EXPmin=min(EXP,na.rm=TRUE),nEXP=n1(EXP),mEXP=n2(EXP))%>%
  ungroup()%>%
  mutate (EXPmean=sprintf("%.2f",asc(EXPmean)),EXPsd=sprintf("%.2f",asc(EXPsd)),
          EXPmed=sprintf("%.2f",asc(EXPmed)),EXPmax=sprintf("%.2f",asc(EXPmax)),
          EXPmin=sprintf("%.2f",asc(EXPmin)))%>%
  mutate(EXPmsd=paste(EXPmean,"(",EXPsd,")"),EXPrange=paste0("(",EXPmin,"-",EXPmax,")"),
         EXPmrange=paste(EXPmed,EXPrange))


names(sum)
sum<-sum[,c(1,7,8,9,11)]
sum

write.csv(sum,file=(paste(AE.name,"_pkbygrade.csv")),quote=F,row.names=F)

#----------------------------------------------------------------


full <- d[,which(colnames(d) %in% c("DV","SEX","SMOK",
                                    "WT","AGE",
                                    "BIO","DOSE","BIO2",
                                    "EXP"))]




fit.full<-clm(DV ~ EXP + AGE + SEX  + WT + BIO+ BIO2+ SMOK+DOSE, data=full)
summary(fit.full)

# formula: DV ~ EXP + AGE + SEX + WT + BIO + BIO2 + SMOK + DOSE
# data:    full
# 
# link  threshold nobs logLik  AIC    niter max.grad cond.H 
# logit flexible  200  -265.45 556.90 6(0)  2.96e-12 4.6e+06
# 
# Coefficients:
#          Estimate Std. Error z value Pr(>|z|)  
# EXP    0.0628150  0.0270750   2.320   0.0203 *
# AGE   -0.0001035  0.0127925  -0.008   0.9935  
# SEX2  -0.7710394  0.3690184  -2.089   0.0367 *
# WT     0.0136824  0.0099213   1.379   0.1679  
# BIO   -0.0495548  0.0304226  -1.629   0.1033  
# BIO2  -0.0118454  0.0142718  -0.830   0.4065  
# SMOK2 -0.1034933  0.3840416  -0.269   0.7876  
# SMOK3  0.3053872  0.4348363   0.702   0.4825  
# DOSE  -0.0030147  0.0082649  -0.365   0.7153  
# ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Threshold coefficients:
#   Estimate Std. Error z value
# 0|1  -2.0887     1.2585  -1.660
# 1|2  -0.4185     1.2492  -0.335
# 2|3   1.6894     1.2550   1.346
# 3|4   2.4011     1.2662   1.896


nullmodel<-clm(DV ~ 1, data=full)

fit.full$info
#residual deviance:

2*(asc(fit.full$info$logLik)-asc(nullmodel$info$logLik))

#table

#Make table with pvalues
ctab <- data.frame(coef(summary(fit.full)))
names(ctab)

#Calucalte 95% CI
lower<-round(ctab$Estimate - (1.96*ctab$Std..Error), digits=4)
upper<-round(ctab$Estimate + (1.96*ctab$Std..Error), digits=4)
ci<-cbind(paste("(",lower,"-",upper,")"))
#Rbind coefficient table  with CI
temp1<-cbind(ctab,ci,lower,upper)
temp<-temp1[c(1,5,3,4)]
temp2<-temp1[c(1,6,7)]
#Round numbers to 4 digits
temp$Estimate<-sprintf("%.4f",round(temp$Estimate,digits=4))
temp$z.value<-sprintf("%.4f",round(temp$z.value,digits=4))
temp$Pr...z..<-sprintf("%.4f",round(temp$Pr...z..,digits=4))
temp$Pr...z..[temp$Pr...z..=="0.0000"]<-"<0.0001"



## odds ratios and 95% CI

OR<-exp(temp2)
OR$Estimate<-sprintf("%.4f",round(OR$Estimate,digits=4))
OR$lower<-sprintf("%.4f",round(OR$lower,digits=4))
OR$upper<-sprintf("%.4f",round(OR$upper,digits=4))
OR$ci<-paste0("(",OR$lower,"-",OR$upper,")")
OR<-OR[c(5:13),c(1,4)]
write.csv(OR,file="OR.csv",quote=F,row.names=F)
newrow <- c("Odds ratio","CI")
OR<-rbind(newrow,OR)


OR[,3]<-" "
OR[,4]<-" "
names(OR)[3]<-"z.value"
names(OR)[4]<-"Pr...z.."

temp<-rbind(temp,OR)

table.gof<-fit.full$info
table.gof<-table.gof[,c(4:8)]


table.gof.names<-data.frame(t(c("Loglik","AIC","Iter(Convergence)",
                                "Gradient","Cond.number")))

table.gof<-table.gof%>%
  mutate(max.grad=asc(max.grad),cond.H=asc(cond.H))%>%
  mutate(max.grad= ifelse(max.grad<=0.001,"<0.001",max.grad))%>%
  mutate(cond.H= sprintf("%.1f",round(cond.H, digits=1)))

names(table.gof)<-c("cname",names(temp))
names(table.gof.names)<-c("cname",names(temp))


#Make names
cname<-c("Intercept-Grade 0 to 1","Intercept-Grade 1 to 2","Intercept-Grade 2 to 3","Intercept-Grade 3 to 4",
         paste0("Cmaxss", "(","$\\mu$","g/L)"),"Age (yr)","Female","Body Weight","biomarker#1","Biomarker#2",
         "Former Smoker","Current smoker","Dose",  " ","Cmaxss","Age (yr)",
         "Female","Body Weight","biomarker#1","Biomarker#2",
         "Former Smoker","Current smoker","Dose")



final0<-cbind(cname,temp)

final<-rbind(final0,table.gof.names,table.gof)
final

write.csv(final, file=(paste(AE.name,"_clm_finalmodel.csv")), row.names=F,quote=F)




#---------------------------------------------------------------------

polr.full<-polr(DV ~ EXP + AGE + SEX  + WT + BIO+ BIO2+ SMOK+DOSE, data=full)

summary(polr.full)


# Call:
#   polr(formula = DV ~ EXP + AGE + SEX + WT + BIO + BIO2 + SMOK + 
#          DOSE, data = full)
# 
# Coefficients:
#   Value Std. Error t value
# EXP    0.094321    0.03143  3.0015
# AGE   -0.007961    0.01428 -0.5575
# SEX2  -0.571077    0.42537 -1.3426
# WT     0.024622    0.01113  2.2125
# BIO   -0.043773    0.03467 -1.2627
# BIO2  -0.021726    0.01602 -1.3558
# SMOK2 -0.130335    0.44334 -0.2940
# SMOK3  0.154638    0.49213  0.3142
# DOSE  -0.005816    0.00852 -0.6826
# 
# Intercepts:
#   Value   Std. Error t value
# 0|1 -1.4367  1.3602    -1.0563
# 1|2  0.3427  1.3493     0.2540
# 2|3  2.4918  1.3643     1.8264
# 3|4  3.2883  1.3814     2.3804
# 
# Residual Deviance: 421.9603 
# AIC: 447.9603 


ctable<-coef(summary(polr.full))
p<-pnorm(abs(ctable[,"t value"]), lower.tail=FALSE)*2
ctable<-cbind(ctable,"p value"=p)
ctable
#           Value  Std. Error      t value    p value
# EXP    0.094321057 0.03142504  3.0014622 0.002686864
# AGE   -0.007960918 0.01428021 -0.5574789 0.577200261
# SEX2  -0.571077094 0.42536540 -1.3425565 0.179415615
# WT     0.024621690 0.01112831  2.2125273 0.026930251
# BIO   -0.043772507 0.03466686 -1.2626615 0.206710842
# BIO2  -0.021726165 0.01602430 -1.3558259 0.175154556
# SMOK2 -0.130335107 0.44334329 -0.2939824 0.768771384
# SMOK3  0.154637772 0.49212892  0.3142221 0.753352377
# DOSE  -0.005816034 0.00852039 -0.6826018 0.494858493
# 0|1   -1.436744794 1.36021623 -1.0562621 0.290848505
# 1|2    0.342698285 1.34928772  0.2539846 0.799507465
# 2|3    2.491759513 1.36432992  1.8263614 0.067795841
# 3|4    3.288313722 1.38141688  2.3803920 0.017294229

polr.null<-polr(DV ~ 1, data=full)

deviance(polr.null)-deviance(polr.full)

d<-d%>%
  mutate(sexlabel=ifelse(SEX==1,"Male","Female"))
plot1<-ggplot(d, aes(x = DV, y = EXP, colour=DV)) +
  geom_boxplot(size=0.75,outlier.shape = NA)+
  geom_jitter(aes(colour=DV),size=2,alpha=0.6)+
  facet_grid(.~sexlabel)+
  theme_bw() +
  size+
  ylab(EXP.name)+
  scale_colour_discrete(name="Grades")+
  xlab(paste(AE.name, "Grade"))
plot1  

dev.print(file=(paste(AE.name,"_boxplotsbysex.png")), device=png, width=1200 ,height=800)





#imagine "best final model": the relationships found statistically significant after cov analysis 
#are   EXP and SEX

final<-clm(DV ~ EXP +SEX, data=full)
summary(final)

# formula: DV ~ EXP + SEX
# data:    full
# 
# link  threshold nobs logLik  AIC    niter max.grad cond.H 
# logit flexible  161  -219.54 451.07 5(0)  9.37e-07 1.7e+04
# 
# Coefficients:
#       Estimate Std. Error z value Pr(>|z|)    
# EXP   0.06861    0.01721   3.986 6.73e-05 ***
# SEX2 -1.00939    0.36489  -2.766  0.00567 ** 
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Threshold coefficients:
#   Estimate Std. Error z value
# 0|1 -1.66067    0.47354  -3.507
# 1|2  0.01558    0.44618   0.035
# 2|3  2.00717    0.47683   4.209
# 3|4  2.76511    0.50654   5.459

##############################################################################################
#calculate the cumulative probability
###############################################################################################

ce<-data.frame(final$coefficients)
ce

#      final.coefficients
# 0|1         -1.66067263
# 1|2          0.01558217
# 2|3          2.00717233
# 3|4          2.76510853
# EXP          0.06861060
# SEX2        -1.00939054



newdata <-  data.frame(
  EXP = full$EXP,
  SEX=ifelse(full$SEX==1,0,1),
  DV=full$DV)



newdata0<-newdata%>%
  mutate(f0=ce[1,1]-(ce[5,1]*EXP+ce[6,1]*SEX), 
         f1=ce[2,1]-(ce[5,1]*EXP+ce[6,1]*SEX),
         f2=ce[3,1]-(ce[5,1]*EXP+ce[6,1]*SEX),
         f3=ce[4,1]-(ce[5,1]*EXP+ce[6,1]*SEX))%>%
  mutate(c0=exp(f0)/(1+exp(f0)),c1=exp(f1)/(1+exp(f1)),c2=exp(f2)/(1+exp(f2)),
         c3=exp(f3)/(1+exp(f3)),c4=1)%>%
  mutate(prob0=c0,prob1=c1-c0,prob2=c2-c1,
         prob3=c3-c2,prob4=1-c3)%>%
  mutate(sexlabel=ifelse(SEX==0,"Male","Female"))%>%
  mutate(latent=ifelse(DV==0,f0,ifelse(DV==1,f1,ifelse(DV==2,f2,
                                                       ifelse(DV==3,f3,NA)))))%>%
  mutate(cutpoint=ifelse(DV==0,ce[1,1],ifelse(DV==1,ce[2,1],ifelse(DV==2,ce[3,1],
                                                                   ifelse(DV==3,ce[4,1],NA)))))


#latent variable################

latent<-ggplot(newdata0%>%subset(DV!=4), aes(x=EXP, y=latent)) + 
  geom_line(aes(colour =as.factor(DV)), size = 1)+
  geom_hline(aes(yintercept=cutpoint ,colour =as.factor(DV)), size = 1,lty=2)+
  ylab("Latent variable")+
  facet_grid(.~sexlabel)+
  xlab(EXP.name)+
  theme_bw() +
  scale_colour_discrete(name="Cumulative Grade",
                        labels=c("Grade 0","Grade \u2264 1","Grade \u2264 2","Grade \u2264 3"))+
  ggtitle("Latent Variable")+
  size
latent


dev.print(file=(paste(AE.name,"_latentvariable.png")), device=png, width=1200 ,height=800)

histo1<-ggplot(data=newdata0%>%subset(DV!=4), aes(latent,fill=sexlabel)) + 
  geom_histogram(position="identity", alpha=0.5,binwidth = 0.5)+
  xlab("Latent Variable")+
  facet_grid(.~sexlabel)+
  geom_vline(aes(xintercept=cutpoint ,colour =as.factor(DV)), size = 1,lty=2)+
  scale_colour_discrete(name="Cumulative Grade",
                        labels=c("Grade 0","Grade \u2264 1","Grade \u2264 2","Grade \u2264 3","Grade \u2264 4"))+
  scale_fill_discrete(guide=FALSE)+
  theme_bw() +
  size
histo1
dev.print(file=(paste(AE.name,"_latentvariablehisto.png")), device=png, width=1200 ,height=800)



histo<-ggplot(data=newdata0%>%subset(DV!=4), aes(latent,fill=as.factor(DV))) + 
  geom_histogram(position="identity", alpha=0.5,binwidth = 0.5)+
  xlab("Latent Variable")+
  facet_grid(.~sexlabel)+
  geom_vline(aes(xintercept=cutpoint ,colour =as.factor(DV)), size = 1,lty=2)+
  scale_fill_discrete(name="Grade",
                      labels=c("Grade 0","Grade \u2264 1","Grade \u2264 2","Grade \u2264 3","Grade \u2264 4"))+
  scale_colour_discrete(guide=FALSE)+
  theme_bw() +
  size
histo
dev.print(file=(paste(AE.name,"_latentvariablehisto.png")), device=png, width=1200 ,height=800)

###CUMULATIVE PROB########################

prob<-ggplot(newdata0, aes()) + 
  geom_line(aes(EXP, c0, colour="coral4")) +
  geom_line(aes(EXP, c1, colour="darkviolet")) +  
  geom_line(aes(EXP, c2, colour="gold3"))+
  geom_line(aes(EXP, c3, colour="olivedrab1")) +  
  geom_line(aes(EXP, c4, colour="plum4"))+
  ylab("Cumulative probability")+
  facet_grid(.~sexlabel)+
  xlab(EXP.name)+
  theme_bw() +
  scale_colour_discrete(name="Probabilities",
                        labels=c("Grade 0","Grade \u2264 1","Grade \u2264 2","Grade \u2264 3","Grade \u2264 4"))+
  ggtitle("Cumulative Probabilities")+
  size
prob
dev.print(file=(paste(AE.name,"_cumprob.png")), device=png, width=1200 ,height=800)

#####################################################################################
#Category Probabilities
####################################################################################




probcat<-ggplot(newdata0, aes())+ 
  geom_line(aes(EXP, prob0, colour="coral4")) +
  geom_line(aes(EXP, prob1, colour="darkviolet")) +  
  geom_line(aes(EXP, prob2, colour="gold3"))+
  geom_line(aes(EXP, prob3, colour="olivedrab1")) +  
  geom_line(aes(EXP, prob4, colour="plum4"))+
  ylab("Predicted probability")+
  facet_grid(.~sexlabel)+
  xlab(EXP.name)+
  theme_bw() +
  scale_colour_discrete(name="Probabilities",
                        labels=c("Grade 0","Grade 1","Grade 2","Grade 3","Grade 4"))+
  ggtitle("Probabilities")+
  size

probcat +
  
  dev.print(file=(paste(AE.name,"_probygrade.png")), device=png, width=1200 ,height=800)



