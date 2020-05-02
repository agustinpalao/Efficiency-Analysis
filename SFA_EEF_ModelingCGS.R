########################################################################################
##############S T O C H A S T I C  F R O N T I E R  A N A L Y S I S ###################
################### M O D E L   S E T U P  ##############################

#SET THE WORKING DIRECTORY
# vAIO Laptop
setwd("C:/Users/Agustin/Dropbox/Dissertation/Data/RWD")
# Home Desktop
setwd("C:/Users/casa/Dropbox/Dissertation/Data/RWD")
# School Desktop
setwd("C:/Users/axp121731/Dropbox/Dissertation/Data/RWD")
require(foreign)
require(dplyr)
require(plyr)
require(frontier)

### 1.- Call the Data
    Dat<- readRDS("SFA_Bean.rds")
    Dat<- readRDS("SFA_Corn.rds")
    Dat<- readRDS("SFA_Sorghum.rds")
    Dat<- readRDS("SFA_Wheat.rds")
    ###Put again panel data format
    library(plm)
    pDat <- plm.data( Dat, c("mun", "year") )
    pdim(pDat)
    punbalancedness(pDat)
### 1.1 Load the SFA production functions for each crop
    load("C:/Users/axp121731/Dropbox/Dissertation/Data/RWD/EEF_Bean_SFA.RData")
    load("C:/Users/axp121731/Dropbox/Dissertation/Data/RWD/EEF_Corn_SFA.RData")
    load("C:/Users/axp121731/Dropbox/Dissertation/Data/RWD/EEF_Sorghum_SFA.RData")
    load("C:/Users/casa/Dropbox/Dissertation/Data/RWD/EEF_Wheat_SFA.RData")
    
### 2.- EEF - SFA Time-invariant Estimation of Cobb-Douglas Production Function 
    # A) Without technological change
    EEF_SFA_TI_Ntch<-sfa(log(PROD)~log(TOTL)+ PCTGIRR+ log(LAB) | WK + 
                         CI+ CB +FI -1 ,data = pDat) 
    
    summary(EEF_SFA_TI_Ntch, extraPar = TRUE)
    summary(EEF_SFA_TI_Ntch)
    
    # Look at the efficiencies and put them in the dataset
    
    length(efficiencies(ECF_SFA_TI_Ntch))
    
    pDat$eff_ECF_SFA_TI_Ntch <-   efficiencies( ECF_SFA_TI_Ntch, asInData = TRUE )
    
    
   
    # B) With technological change
    EEF_SFA_TI_tch<-sfa(log(PROD)~log(TOTL)+ PCTGIRR+ log(LAB)+ mYear | WK +
                          CI + CB + FI -1, data = pDat)

    summary(EEF_SFA_TI_tch)
    
   
    
    
  #Comparing the two models: In the CD production frontier that accounts for technological change,
   
    lrtest(ECF_SFA_TI_Ntch, ECF_SFA_TI_tch)
    
  # The results showed that there is technological change through time involved 
 
    
   # Comparing to OLS model. Likelihood ratio tests show that OLS models are clearly rejected in favor of
   # of the corresponding stochastic frontier models
    lrtest(ECF_SFA_TI_Ntch)
    lrtest(ECF_SFA_TI_tch)
    
  # Look at the efficiencies and put them in the dataset
    
    length(efficiencies(EEF_SFA_TI_tch))
    
    pDat$eff_EEF_SFA_TI_tch <- efficiencies( EEF_SFA_TI_tch, asInData = TRUE )
    
### 3.- ECF - SFA Time-variant Estimation of Cobb-Douglas Production Function 
    # A) Without technological change
    
    EEF_SFA_TV_Ntch<-sfa(log(PROD)~log(TOTL)+ PCTGIRR+ log(LAB) | WK +
                           CI + CB + FI + mYear -1, data = pDat) 
    
    summary(EEF_SFA_TV_Ntch)
    
    # Look at the efficiencies and put them in the dataset
    
    length(efficiencies(EEF_SFA_TV_Ntch))
    
    pDat$eff_EEF_SFA_TV_Ntch <- efficiencies( EEF_SFA_TV_Ntch, asInData = TRUE )
    
    
    # B) With technological change
    
    EEF_SFA_TV_tch<-sfa(log(PROD)~log(TOTL)+ PCTGIRR+ log(LAB)+ mYear | WK +
                          CI + CB + FI + mYear -1, data = pDat)
    
    summary(EEF_SFA_TV_tch)
    
    #Comparing the two models
    lrtest(ECF_SFA_TV_tch, ECF_SFA_TV_Ntch)
    
    #So there is no technological progress: 
    
    # The results showed that there is no technological progress, but there is variation
    # in the efficiency levels through time. The positive sign in the heta term (time) indicates
    # that efficiency is increasing over time, is significant.
    # Comparing the time variant versus time invariant models:  
    lrtest(ECF_SFA_TI_Ntch, ECF_SFA_TV_Ntch)
    lrtest(ECF_SFA_TI_tch, ECF_SFA_TV_tch)
    lrtest(ECF_SFA_TI_Ntch, ECF_SFA_TV_tch)
    lrtest(ECF_SFA_TV_tch, ECF_SFA_TV_Ntch)
    lrtest(ECF_SFA_TV_Ntch, ECF_SFA_TI_tch)
    #This confirm that "eta" is siginificant, that is the effect of time in efficiencies is significant.
    #And the presence of technological progress is not clear. 
    
    
    
    # to confirm a separate effect of efficiencies,
    # look at some correlation between efficiencies and technological change
    round( cov2cor( vcov( ECF_SFA_TV_tch ) ), 2 )
    
    # Check if frontier model is suitable
    lrtest(ECF_SFA_TV_tch)
    
    # Look at the efficiencies and put them in the dataset
    dim(efficiencies(EEF_SFA_TV_tch))
    pDat$eff_EEF_TV_tch <- efficiencies( EEF_SFA_TV_tch, asInData = TRUE )   
    pDat$eff_EEF_TV_Ntch <- efficiencies( EEF_SFA_TV_Ntch, asInData = TRUE )
    
### Create summary side-by-side table for all regressions
    
    install.packages("stargazer")
    library(stargazer)
    install.packages("broom")
    library(broom)
    
    tidy.frontier <- function(x, conf.int = FALSE, conf.level = .95,
                              exponentiate = FALSE, quick = FALSE, ...)
    {
      broom:::tidy.lm(x, conf.int = conf.int, conf.level = conf.level,
                      exponentiate = exponentiate, quick = quick, ...)
    }
    
    Test<-coef( ECF_SFA_TI_Ntch, which = "mle", extraPar = TRUE )
    
    
    Test<-tidy.frontier(ECF_SFA_TI_Ntch, conf.int = TRUE)
    Test1<-broom:::tidy.lm(ECF_SFA_TI_Ntch)
    Test2<-broom:::tidy.lm(ECF_SFA_TI_tch)
    
    
    
    linear.1 <- lm(rating ~ complaints + privileges + learning + raises + critical, data=attitude)
    linear.2 <- lm(rating ~ complaints + privileges + learning, data=attitude)
    stargazer(linear.1, linear.2, title="Regression Results", align=TRUE)
    
    
    stargazer(ECF_LM_TI_Ntch, ECF_LM_TV_tch, ECF_LM_TI_tch, 
              ECF_LM_TV_Ntch, title="SFA results", align=TRUE)
    
    stargazer(Test1, Test2, align=TRUE)
    
### 10.- Using Efficiency Effects Frontier (EEF) to broke down inefficiency factors
  
    
#### CITING FRONTIER                   
    citation(package = "frontier")
    
    
    #Calculating estimated variances of efficiency term u and noise
    #term v 
    gamma_var <- function(SFA) {
      gamma<-unname(coef(SFA)["gamma"])
      sigmaSq <- unname(coef(SFA)["sigmaSq"])
      sigmaSqU <- gamma*sigmaSq
      VarU<- sigmaSqU * (1 - (2 *dnorm(max((coef(SFA)["mu"]),0)))^2)
      VarV<- sigmaSqV<-(1-gamma)* sigmaSq
      out<-VarU/(VarU+VarV)
      return(out)
    }
    
gamma_var(EEF_SFA_TI_Ntch)


##### Creating a graph with time varying efficiencies
eff_bean <- summary(summ_Bean)
eff_corn <- summary(summ_Corn)
eff_sorghum <- summary(summ_Sorghum)
eff_wheat <- summary(summ_Wheat)
Eff<-data.frame(eff_bean$efficYearMeans, eff_corn$efficYearMeans, eff_sorghum$efficYearMeans,
                eff_wheat$efficYearMeans)

colnames(Eff)<-c("Bean", "Corn", "Sorghum", "Wheat")

EffTS<-ts(Eff, start = 2004, frequency = 1)


ts.plot(EffTS, gpars = list(col=c("black","green", 
                                "red", "blue"), ylim=c(0.35, 0.85),pch=c(1,2,3,4),
                            type="b",xaxt="n", lty=4, lwd=2, frame.plot=FALSE, xlab="Year", ylab="Efficiency",
                            main="EEF Efficiency Time Series"))
legend("topright",c("Bean", "Corn", "Sorghum", "Wheat"), 
       col=c("black","green","red", "blue"), pch = c(1,2, 3,4), lty = 4,
       bty="n")
axis(1, at = seq(2004, 2013, by = 1), las=2)
grid (NA,NULL, lty = 6, col = "cornsilk2")



Dat$eff<-efficiencies(ECF_SFA_TV_tch)

# 1 year's worth of dates:
d <- strptime(1:365, format="%j")

# some simulated data
x <- rlnorm(365)

# combine them into a DF 
bpbean <- data.frame(Year=pDat$year, Efficiency=pDat$eff_EEF_TV_tch)
bpcorn <- data.frame(Year=pDat$year, Efficiency=pDat$eff_EEF_SFA_TI_tch)
bpsorghum <- data.frame(Year=pDat$year, Efficiency=pDat$eff_EEF_SFA_TI_tch)
bpwheat <- data.frame(Year=pDat$year, Efficiency=pDat$eff_EEF_SFA_TV_Ntch)

# plot the data, note that x-axis is in dates:
plot(dx)

# now generate a grouping factor. how about months:
dx$month <- format(dx$date, format="%B")

# box and whisker plot for data *grouped* by month
graph<-boxplot(Efficiency ~ Year, data=bpbean, las=3)


set.seed(1)
xz = zoo(ts(rnorm(20), frequency = 4, start = c(1959, 2)))
yz = zoo(ts(rnorm(20), frequency = 4, start = c(1959, 2)))
# Basic approach
plot(xz)
lines(yz, col = "red")
# Panels
plot.zoo(cbind(xz, yz))
# Overplotted
plot.zoo(cbind(xz, yz), 
         plot.type = "single", 
         col = c("red", "blue"))

library(ggplot2)
means <- aggregate(Efficiency ~  Year, bpbean, mean)
theme_update(plot.title = element_text(hjust = 0.5, size = 12))
ggplot(bpbean, aes(x=Year, y=Efficiency)) + geom_boxplot() +
  stat_summary(fun.y=mean, colour="darkred", geom="point", 
               shape=18, size=3,show.legend = FALSE) + 
  #geom_text(data = means, size=2, aes(label = Efficiency, y = Efficiency -0.08))+
  ggtitle("Beans \n (N=1,855, min = 0.003, max = 0.953)")


##### Extract the efficiencies for chapter 7 #####
### Load the SFA production functions for each crop
library(plm)
#Bean
Dat<- readRDS("pSFA_Bean.rds")
pDat <- plm.data( Dat, c("mun", "year") )
load("C:/Users/axp121731/Dropbox/Dissertation/Data/RWD/EEF_Bean_SFA.RData")
# the selected model is technological change and time variant efficiencies
length(efficiencies(EEF_SFA_TV_tch))
pDat$eff_EEF_SFA_TV_tch <-   efficiencies( EEF_SFA_TV_tch, asInData = TRUE )
EEF_bean_eff <- pDat[,c(1,2,3,30)]
colnames(EEF_bean_eff) <- c("ID_Mun", "Year", "Municipality", "Bean_Eff")

#Corn
Dat<- readRDS("SFA_Corn.rds")
pDat <- plm.data( Dat, c("mun", "year") )
load("C:/Users/axp121731/Dropbox/Dissertation/Data/RWD/EEF_Corn_SFA.RData")
# the selected model is technological change and time invariant efficiencies
length(efficiencies(EEF_SFA_TI_tch))
pDat$eff_EEF_SFA_TI_tch <-   efficiencies( EEF_SFA_TI_tch, asInData = TRUE )
EEF_corn_eff <- pDat[,c(1,2,3,30)]
colnames(EEF_corn_eff) <- c("ID_Mun", "Year", "Municipality", "Corn_Eff")

#Sorghum
Dat<- readRDS("SFA_Sorghum.rds")
pDat <- plm.data( Dat, c("mun", "year") )
load("C:/Users/axp121731/Dropbox/Dissertation/Data/RWD/EEF_Sorghum_SFA.RData")
# the selected model is technological change and time invariant efficiencies
length(efficiencies(EEF_SFA_TI_tch))
pDat$eff_EEF_SFA_TV_tch <-   efficiencies( EEF_SFA_TV_tch, asInData = TRUE )
EEF_sorghum_eff <- pDat[,c(1,2,3,30)]
colnames(EEF_sorghum_eff) <- c("ID_Mun", "Year", "Municipality", "Sorghum_Eff")

#Wheat
Dat<- readRDS("SFA_Wheat.rds")
pDat <- plm.data( Dat, c("mun", "year") )
load("C:/Users/axp121731/Dropbox/Dissertation/Data/RWD/EEF_Wheat_SFA.RData")
# the selected model is no technological change and time variant efficiencies
length(efficiencies(EEF_SFA_TV_Ntch))
pDat$eff_EEF_SFA_TV_Ntch <-   efficiencies( EEF_SFA_TV_Ntch, asInData = TRUE )
EEF_wheat_eff <- pDat[,c(1,2,3,30)]
colnames(EEF_wheat_eff) <- c("ID_Mun", "Year", "Municipality", "Wheat_Eff")


#joining all efficiencies
EEF_eff<- list(EEF_bean_eff, EEF_corn_eff, EEF_sorghum_eff, EEF_wheat_eff) %>%
  Reduce(function(dtf1,dtf2) full_join(dtf1,dtf2,by=c("ID_Mun","Year", "Municipality")), .)

EEF_eff["Avg_eff"]<-rowMeans(EEF_eff[,4:7], na.rm = TRUE)

write.csv(EEF_bean_eff,"C:/Users/axp121731/Dropbox/Dissertation/Data/RWD/shpR/Eff_EEF_bean.csv",row.names = FALSE)
write.csv(EEF_corn_eff,"C:/Users/axp121731/Dropbox/Dissertation/Data/RWD/shpR/Eff_EEF_corn.csv",row.names = FALSE)
write.csv(EEF_sorghum_eff,"C:/Users/axp121731/Dropbox/Dissertation/Data/RWD/shpR/Eff_EEF_sorghum.csv",row.names = FALSE)
write.csv(EEF_wheat_eff,"C:/Users/axp121731/Dropbox/Dissertation/Data/RWD/shpR/Eff_EEF_wheat.csv",row.names = FALSE)
write.csv(EEF_eff,"C:/Users/axp121731/Dropbox/Dissertation/Data/RWD/shpR/Eff_EEF_all.csv",row.names = FALSE)

