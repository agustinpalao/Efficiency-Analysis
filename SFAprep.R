########################################################################################
##############S T O C H A S T I C  F R O N T I E R  A N A L Y S I S ###################
################### D A T A   P R E P A R A T I O N ##############################

require(foreign)
require(dplyr)
require(plyr)


#Next command is for installing how to read xls files in r
#devtools::install_github("hadley/readxl")


#setting up Guarantees and SIAP Database to work with

CGS_Sorghum<- readRDS("CGS_Sorghum.rds")
CGS_Wheat<- readRDS("CGS_Wheat.rds")
CGS_Bean<- readRDS("CGS_Bean.rds")
CGS_Corn<- readRDS("CGS_Corn.rds")
Maize <- readRDS("Corn.rds")
Bean <- readRDS("Frijol.rds")
Wheat <- readRDS("Wheat.rds")
Sorghum <- readRDS("Sorghum.rds")


##First from SIAP create the Dependent Variable: TONNES OF PRODUCE PER MUNICIPALITY-YEAR

  SFA_Maize<- ddply(Maize, .(ID_Mun, Municipio, AñoAgricola), summarize, Prod=sum(Produccion))
  SFA_Bean<- ddply(Bean, .(ID_Mun, Municipio, AñoAgricola), summarize, Prod=sum(Produccion))
  SFA_Wheat<- ddply(Wheat, .(ID_Mun, Municipio, AñoAgricola), summarize, Prod=sum(Produccion))
  SFA_Sorghum<- ddply(Sorghum, .(ID_Mun, Municipio, AñoAgricola), summarize, Prod=sum(Produccion))
  
  
  ### Start creating Independent variables: (1) PLANTED AREA IN HECTARES: 1.1 DRY-LAND, 1.2 IRRIGATED-LAND  
    ### CORN 
  
    General_land<-ddply(Maize, .(ID_Mun, Municipio, AñoAgricola, Modalidad), summarize, Area=sum(Sembrada))
    DryLand<-subset(General_land, General_land$Modalidad=="TEMPORAL")
    Irrigation<-subset(General_land, General_land$Modalidad=="RIEGO")
  
    SFA_Maize<-merge(SFA_Maize, DryLand, by=c("ID_Mun","Municipio","AñoAgricola" ),all.x = TRUE)
    SFA_Maize<-merge(SFA_Maize, Irrigation, by=c("ID_Mun","Municipio","AñoAgricola" ),all.x = TRUE)
    SFA_Maize$Modalidad.x<-NULL
    SFA_Maize$Modalidad.y<-NULL
    names(SFA_Maize)[names(SFA_Maize) == 'Area.x'] <- 'DryLand'
    names(SFA_Maize)[names(SFA_Maize) == 'Area.y'] <- 'Irrigation'
    SFA_Maize[is.na(SFA_Maize)]<-0
    rm(DryLand,General_land,Irrigation,Maize)
    SFA_Maize$TotLand<-SFA_Maize$DryLand+SFA_Maize$Irrigation
    SFA_Maize$PctgIrr<-SFA_Maize$Irrigation/SFA_Maize$TotLand
    
    #### BEAN
    General_land<-ddply(Bean, .(ID_Mun, Municipio, AñoAgricola, Modalidad), summarize, Area=sum(Sembrada))
    DryLand<-subset(General_land, General_land$Modalidad=="TEMPORAL")
    Irrigation<-subset(General_land, General_land$Modalidad=="RIEGO")
    
    SFA_Bean<-merge(SFA_Bean, DryLand, by=c("ID_Mun","Municipio","AñoAgricola" ),all.x = TRUE)
    SFA_Bean<-merge(SFA_Bean, Irrigation, by=c("ID_Mun","Municipio","AñoAgricola" ),all.x = TRUE)
    SFA_Bean$Modalidad.x<-NULL
    SFA_Bean$Modalidad.y<-NULL
    names(SFA_Bean)[names(SFA_Bean) == 'Area.x'] <- 'DryLand'
    names(SFA_Bean)[names(SFA_Bean) == 'Area.y'] <- 'Irrigation'
    SFA_Bean[is.na(SFA_Bean)]<-0
    rm(DryLand,General_land,Irrigation,Bean)
    SFA_Bean$TotLand<-SFA_Bean$DryLand+SFA_Bean$Irrigation
    SFA_Bean$PctgIrr<-SFA_Bean$Irrigation/SFA_Bean$TotLand
    
    ### WHEAT
    General_land<-ddply(Wheat, .(ID_Mun, Municipio, AñoAgricola, Modalidad), summarize, Area=sum(Sembrada))
    DryLand<-subset(General_land, General_land$Modalidad=="TEMPORAL")
    Irrigation<-subset(General_land, General_land$Modalidad=="RIEGO")
    
    SFA_Wheat<-merge(SFA_Wheat, DryLand, by=c("ID_Mun","Municipio","AñoAgricola" ),all.x = TRUE)
    SFA_Wheat<-merge(SFA_Wheat, Irrigation, by=c("ID_Mun","Municipio","AñoAgricola" ),all.x = TRUE)
    SFA_Wheat$Modalidad.x<-NULL
    SFA_Wheat$Modalidad.y<-NULL
    names(SFA_Wheat)[names(SFA_Wheat) == 'Area.x'] <- 'DryLand'
    names(SFA_Wheat)[names(SFA_Wheat) == 'Area.y'] <- 'Irrigation'
    SFA_Wheat[is.na(SFA_Wheat)]<-0
    rm(DryLand,General_land,Irrigation,Wheat)
    SFA_Wheat$TotLand<-SFA_Wheat$DryLand+SFA_Wheat$Irrigation
    SFA_Wheat$PctgIrr<-SFA_Wheat$Irrigation/SFA_Wheat$TotLand
    
    ### SORGHUM
    General_land<-ddply(Sorghum, .(ID_Mun, Municipio, AñoAgricola, Modalidad), summarize, Area=sum(Sembrada))
    DryLand<-subset(General_land, General_land$Modalidad=="TEMPORAL")
    Irrigation<-subset(General_land, General_land$Modalidad=="RIEGO")
    
    SFA_Sorghum<-merge(SFA_Sorghum, DryLand, by=c("ID_Mun","Municipio","AñoAgricola" ),all.x = TRUE)
    SFA_Sorghum<-merge(SFA_Sorghum, Irrigation, by=c("ID_Mun","Municipio","AñoAgricola" ),all.x = TRUE)
    SFA_Sorghum$Modalidad.x<-NULL
    SFA_Sorghum$Modalidad.y<-NULL
    names(SFA_Sorghum)[names(SFA_Sorghum) == 'Area.x'] <- 'DryLand'
    names(SFA_Sorghum)[names(SFA_Sorghum) == 'Area.y'] <- 'Irrigation'
    SFA_Sorghum[is.na(SFA_Sorghum)]<-0
    rm(DryLand,General_land,Irrigation,Sorghum)
    SFA_Sorghum$TotLand<-SFA_Sorghum$DryLand+SFA_Sorghum$Irrigation
    SFA_Sorghum$PctgIrr<-SFA_Sorghum$Irrigation/SFA_Sorghum$TotLand
    
    
    
       
###   MERGING SIAP WITH GUARANTEE DATABASE
  
  SFA_Maize<-merge(SFA_Maize, CGS_Corn, by.x = c("ID_Mun","Municipio","AñoAgricola" ),
                   by.y = c("ID_Mun", "Mun", "Year"),all.x = TRUE)
  SFA_Bean<-merge(SFA_Bean, CGS_Bean, by.x = c("ID_Mun","Municipio","AñoAgricola" ),
                   by.y = c("ID_Mun", "Mun", "Year"),all.x = TRUE)
  SFA_Wheat<-merge(SFA_Wheat, CGS_Wheat, by.x = c("ID_Mun","Municipio","AñoAgricola" ),
                   by.y = c("ID_Mun", "Mun", "Year"),all.x = TRUE)
  SFA_Sorghum<-merge(SFA_Sorghum, CGS_Sorghum, by.x = c("ID_Mun","Municipio","AñoAgricola" ),
                   by.y = c("ID_Mun", "Mun", "Year"),all.x = TRUE)
  
  
  
  rm(CGS_Sorghum, CGS_Wheat, CGS_Bean, CGS_Corn)
  
 
  
  #Labor

  Labor<-read.csv("labor_mun_final.csv")
  Labor<-reshape(Labor, varying = list(names(Labor)[2:11]), v.names = "Workers",
                    idvar = c("ID_Mun"), timevar="year", times=2004:2013,  direction = "long")
  #Now make workforce proportions based on production levels for each type of produce
  #Agriculture percentages: Corn: 13.1, Bean: 9.1, Wheat: 2, Sorghum: 2.7.
  
  Labor$LabCorn<-(Labor$Workers*.131)#*250
  Labor$LabBean<-(Labor$Workers*0.091)#*250
  Labor$LabWheat<-(Labor$Workers*0.02)#*250
  Labor$LabSorghum<-(Labor$Workers*0.027)#*250
  
  #based on the example of Coelli, I am going to use man-days of labor. The number of labor hours
  #per week is about 38 hours, close to the english week of labor (Monday-Friday). Based on this
  #I will use 250 of labor days per year.
  
  #Add Labor to SFA database
  SFA_Maize<-merge(SFA_Maize, Labor[ , c("ID_Mun","year", "LabCorn")], 
                    by.x = c("ID_Mun","AñoAgricola"), by.y = c("ID_Mun", "year"), all.x=TRUE)
  SFA_Bean<-merge(SFA_Bean, Labor[ , c("ID_Mun","year", "LabBean")], 
                   by.x = c("ID_Mun","AñoAgricola"), by.y = c("ID_Mun", "year"), all.x=TRUE)
  SFA_Wheat<-merge(SFA_Wheat, Labor[ , c("ID_Mun","year", "LabWheat")], 
                   by.x = c("ID_Mun","AñoAgricola"), by.y = c("ID_Mun", "year"), all.x=TRUE)
  SFA_Sorghum<-merge(SFA_Sorghum, Labor[ , c("ID_Mun","year", "LabSorghum")], 
                   by.x = c("ID_Mun","AñoAgricola"), by.y = c("ID_Mun", "year"), all.x=TRUE)
  
  #Put zeroes in all na
  
  SFA_Maize[9:13][is.na(SFA_Maize[9:13])]<-0
  which(is.na(SFA_Maize), arr.ind=TRUE)#Checking for Nas
  SFA_Maize<-SFA_Maize[complete.cases(SFA_Maize),]
  
  SFA_Bean[9:13][is.na(SFA_Bean[9:13])]<-0
  which(is.na(SFA_Bean), arr.ind=TRUE)#Checking for Nas
  SFA_Bean<-SFA_Bean[complete.cases(SFA_Bean),]
  
  SFA_Wheat[9:13][is.na(SFA_Wheat[9:13])]<-0
  which(is.na(SFA_Wheat), arr.ind=TRUE)#Checking for Nas
  SFA_Wheat<-SFA_Wheat[complete.cases(SFA_Wheat),]
  
  SFA_Sorghum[9:13][is.na(SFA_Sorghum[9:13])]<-0
  which(is.na(SFA_Sorghum), arr.ind=TRUE)#Checking for Nas
  SFA_Sorghum<-SFA_Sorghum[complete.cases(SFA_Sorghum),]
  
  rm(Labor)
  
  #Prepare data to merge in just one dataframe
  SFA_Bean$Produce<-"Bean"
  names(SFA_Bean)[names(SFA_Bean) == 'LabBean'] <- 'Labor'
  SFA_Maize$Produce<-"Corn"
  names(SFA_Maize)[names(SFA_Maize) == 'LabCorn'] <- 'Labor'
  SFA_Sorghum$Produce<-"Sorghum"
  names(SFA_Sorghum)[names(SFA_Sorghum) == 'LabSorghum'] <- 'Labor'
  SFA_Wheat$Produce<-"Wheat"
  names(SFA_Wheat)[names(SFA_Wheat) == 'LabWheat'] <- 'Labor'
  
  #Create single DataFrame
  
  Dat<-rbind(SFA_Bean,SFA_Maize,SFA_Sorghum,SFA_Wheat)
  
  
  ####Add additional variables, money values in millions..

    Dat <- setNames(Dat, c("mun","year", "muname","PROD", "DRYL","IRRIGL","TOTL","PCTGIRR","WK", "CI",
                           "CB","CHAIN" ,"FI","LAB","CROP"))
    Dat$CHAIN<-NULL
    
    ### Money values in millions
    Dat$WK<-Dat$WK/1000000
    Dat$CI<-Dat$CI/1000000
    Dat$CB<-Dat$CB/1000000
    Dat$TK<-Dat$WK+Dat$CI+Dat$CB
    Dat$P_L<-Dat$PROD/Dat$LAB  #Production per Labor
    Dat$P_TL<-Dat$PROD/Dat$TOTL #Production per Total cultivated Land
    
### Arrange columns to have a better understanding of the data
    
    Dat<-Dat[c(2,1,3,14,4,5,6,8,7,17,13,16,15,9,10,11,12)]
    Dat$CROP<-as.factor(Dat$CROP)
    
    ### 1.5.- Create dummy variables for the variables that have zeros in CGS and FIs
    ### Note: This is only necessary when you use only the ECF specification
    
    # for (i in 1:length(Dat$mun)) {
    #   ifelse(Dat[i,9]>0,(Dat$Dwk[i]<-1),(Dat$Dwk[i]<-0))
    #   
    #   ifelse(Dat[i,10]>0,(Dat$Dci[i]<-1),(Dat$Dci[i]<-0))
    #   
    #   ifelse(Dat[i,11]>0,(Dat$Dcb[i]<-1),(Dat$Dcb[i]<-0))
    #   
    #   ifelse(Dat[i,15]>0,(Dat$DTK[i]<-1),(Dat$DTK[i]<-0))
    #   
    #   #ifelse(Dat[i,12]>0,(Dat$Dfi[i]<-1),(Dat$Dfi[i]<-0))
    # }
    
  
  
  rm(SFA_Sorghum, SFA_Wheat, SFA_Bean, SFA_Maize)
  
  
  
  
  #################################################################
  ####    P A R T  T W O   ######## DATA ANALYSIS and CONSISTENCY
  ##################################################################
  ### 1.- Work separately for each crop. Subset each and perform statistics
  Corn<-subset(Dat, Dat$CROP=='Corn')
  Bean<-subset(Dat, Dat$CROP=='Bean')
  Wheat<-subset(Dat, Dat$CROP=='Wheat')
  Sorghum<-subset(Dat, Dat$CROP=='Sorghum')
  ### 2.- Perform basic summary statistics to check data consistency
  
  # require(psych)
  # describe(Bean[,5:17])
  # summary(Bean[,5:17], digits = max(3, getOption("digits")-3))
  # describe(Corn[,5:17])
  # summary(Corn[,5:17], digits = max(3, getOption("digits")-3))
  # describe(Sorghum[,5:17])
  # summary(Sorghum[,5:17], digits = max(3, getOption("digits")-3))
  # describe(Wheat[,5:17])
  # summary(Wheat[,5:17], digits = max(3, getOption("digits")-3))

  ### There are some rows with zero production. Not good although it is plausible.
  ### Total Land that were affected by droughts or other climate conditions may produce
  ### zero harvested areas. 
  ### Labor with zero values is not meaningful for the analysis
  ### For the sake of the analysis we'll remove those values
  
  # remzeros = function(Dat)
  # {
  #   Dat<-subset(Dat, PROD>0 & TOTL>0 & LAB>0)
  #   return(Dat)
  # }
  # out<-lapply(out, remzeros)
  
  Bean<-subset(Bean, PROD>0 & TOTL>0 & LAB>0)
  Corn<-subset(Corn, PROD>0 & TOTL>0 & LAB>0)
  Sorghum<-subset(Sorghum, PROD>0 & TOTL>0 & LAB>0)
  Wheat<-subset(Wheat, PROD>0 & TOTL>0 & LAB>0)
  
   summary(Bean[,c(10,12)], digits = max(3, getOption("digits")-3))
  # 
   summary(Corn[,c(10,12)], digits = max(3, getOption("digits")-3))
  # 
   summary(Sorghum[,c(10,12)], digits = max(3, getOption("digits")-3))
  # 
   summary(Wheat[,c(10,12)], digits = max(3, getOption("digits")-3))
   
   library(plm)
   pBean <- plm.data( Bean, c("mun", "year") )
   pdim(pBean)
   punbalancedness(pBean)
   
   pCorn <- plm.data( Corn, c("mun", "year") )
   pdim(pCorn)
   punbalancedness(pCorn)
   
   pSorghum <- plm.data( Sorghum, c("mun", "year") )
   pdim(pSorghum)
   punbalancedness(pSorghum)
   
   pWheat <- plm.data( Wheat, c("mun", "year") )
   pdim(pWheat)
   punbalancedness(pWheat)
   
  
  ### 3.- Outliers treatment   

    ### Analyze data to see statistical Outliers. Run a function called Outlier KD
  ### data outside quartiles      
  
  outlierKD <- function(dt, var) {
    var_name <- eval(substitute(var),eval(dt))
    na1 <- sum(is.na(var_name))
    m1 <- mean(var_name, na.rm = T)
    par(mfrow=c(2, 2), oma=c(0,0,3,0))
    boxplot(var_name, main="With outliers")
    hist(var_name, main="With outliers", xlab=NA, ylab=NA)
    outlier <- boxplot.stats(var_name)$out
    mo <- mean(outlier)
    var_name <- ifelse(var_name %in% outlier, NA, var_name)
    boxplot(var_name, main="Without outliers")
    hist(var_name, main="Without outliers", xlab=NA, ylab=NA)
    title("Outlier Check", outer=TRUE)
    na2 <- sum(is.na(var_name))
    cat("Outliers identified:", na2 - na1, "n")
    cat("Propotion (%) of outliers:", round((na2 - na1) / sum(!is.na(var_name))*100, 1), "n")
    cat("Mean of the outliers:", round(mo, 2), "n")
    m2 <- mean(var_name, na.rm = T)
    cat("Mean without removing outliers:", round(m1, 2), "n")
    cat("Mean if we remove outliers:", round(m2, 2), "n")
    response <- readline(prompt="Do you want to remove outliers and to replace with NA? [yes/no]: ")
    if(response == "y" | response == "yes"){
      dt[as.character(substitute(var))] <- invisible(var_name)
      assign(as.character(as.list(match.call())$dt), dt, envir = .GlobalEnv)
      cat("Outliers successfully removed", "n")
      return(invisible(dt))
    } else{
      cat("Nothing changed", "n")
      return(invisible(var_name))
    }
  }
  
  #Run the function based on logical proportions of Tons per man-day labor.
  #I decided to use number of workers instead of man-days for Labor

  ######## BEAN ################# After running different orders, outlier labor then land is best
    outlierKD(Bean, P_L)
    Bean<-Bean[complete.cases(Bean),]
    summary(Bean)
    #Then analyze production per hectares
    outlierKD(Bean, P_TL)
    Bean<-Bean[complete.cases(Bean),]
    summary(Bean[,c(10,12)])
    #The numbers for production per hectares looks good.
    
    #run this just to check balancedness
    library(plm)
    pBean <- plm.data( Bean, c("mun", "year") )
    pdim(pBean)
    punbalancedness(pBean)
    # I chose first labor and then land
    
    
    ######## CORN #################After running different orders, outlier land then labor is best
    outlierKD(Corn, P_TL)
    Corn<-Corn[complete.cases(Corn),]
    #outlierKD(Corn, P_L)
    #Corn<-Corn[complete.cases(Corn),]
    summary(Bean[,c(10,12)])
    pCorn <- plm.data( Corn, c("mun", "year") )
    pdim(pCorn)
    punbalancedness(pCorn)

  # I chose first land and then labor
    
    ######## SORGHUM ###############After running different orders, outlier land then labor is best   
    outlierKD(Sorghum, P_TL)
    Sorghum<-Sorghum[complete.cases(Sorghum),]
    #outlierKD(Sorghum, P_L)
    #Sorghum<-Sorghum[complete.cases(Sorghum),]
    pSorghum <- plm.data( Sorghum, c("mun", "year") )
    pdim(pSorghum)
    punbalancedness(pSorghum)
    
    summary(Sorghum)
    # I chose first land and then labor
    
    
    ######## WHEAT #################After running different orders, outlier land then labor is indifferent
    outlierKD(Wheat, P_L)
    Wheat<-Wheat[complete.cases(Wheat),]
    # outlierKD(Wheat, P_TL)
    # Wheat<-Wheat[complete.cases(Wheat),]
    pWheat <- plm.data( Wheat, c("mun", "year") )
    pdim(pWheat)
    punbalancedness(pWheat)
    
    summary(Wheat)
  #### same result.
    
#########   PART THREE   #############
    #########  Do the statistics summary for the required variables  #############
    
    library(pastecs)
    BeanStat<-as.data.frame(t(stat.desc(Bean[,5:17])))
    BeanStat[2:3]<-list(NULL)
    BeanStat[4:5]<-list(NULL)
    BeanStat[6:8]<-list(NULL)
    BeanStat <- format(round(BeanStat,digits = 2), big.mark=",")
    BeanStat<-BeanStat[c(1,2,3,5,7,9,10,11,12,13),]
    
    CornStat<-as.data.frame(t(stat.desc(Corn[,5:17])))
    CornStat[2:3]<-list(NULL)
    CornStat[4:5]<-list(NULL)
    CornStat[6:8]<-list(NULL)
    CornStat <- format(round(CornStat,digits = 2), big.mark=",")
    CornStat<-CornStat[c(1,2,3,5,7,9,10,11,12,13),]
    
    
    SorghumStat<-as.data.frame(t(stat.desc(Sorghum[,5:17])))
    SorghumStat[2:3]<-list(NULL)
    SorghumStat[4:5]<-list(NULL)
    SorghumStat[6:8]<-list(NULL)
    SorghumStat <- format(round(SorghumStat,digits = 2), big.mark=",")
    SorghumStat<-SorghumStat[c(1,2,3,5,7,9,10,11,12,13),]
    
    
    WheatStat<-as.data.frame(t(stat.desc(Wheat[,5:17])))
    WheatStat[2:3]<-list(NULL)
    WheatStat[4:5]<-list(NULL)
    WheatStat[6:8]<-list(NULL)
    WheatStat <- format(round(WheatStat,digits = 2), big.mark=",")
    WheatStat<-WheatStat[c(1,2,3,5,7,9,10,11,12,13),]
    
    GeneralStat<-rbind(BeanStat, CornStat, SorghumStat, WheatStat)
    
    ##### Export to Latex ###########  OR
    require(xtable)
    xtable(GeneralStat)
    
  
    ###### PART FOUR  ############################
    #######  ADDITIONAL USEFUL VARIABLES FOR PANEL DATA SETS ########  
    ## 1.- Mean-scaled quantities
    ###    In some model specifications we are going to use Mean-scaling quantities.
    ###   We create new variable with mean-scaled input and output quantities:
    Mean_sc<-function(produce,...){
      produce["prod"]<-produce$PROD/mean(produce$PROD)
      produce["dryl"]<-produce$DRYL/mean(produce$DRYL)
      produce["irrigl"]<-produce$IRRIGL/mean(produce$IRRIGL)
      produce["totl"]<-produce$TOTL/mean(produce$TOTL)
      produce["wk"]<-produce$WK/mean(produce$WK)
      produce["ci"]<-produce$CI/mean(produce$CI)
      produce["cb"]<-produce$CB/mean(produce$CB)
      produce["fi"]<-produce$FI/mean(produce$FI)
      produce["lab"]<-produce$LAB/mean(produce$LAB)
      produce["tk"]<-produce$TK/mean(produce$TK)
      return(invisible(produce))
    }####  Mean-scaled Variables are in lower case   ########################
    
    
    
    ### 2.- Mean-adjusting the Time Trend
    ###     In some models it is an advantage to have time trend variable that is zero at the 
    ###     sample mean. First we need to convert real years into consecutive numbers
    Ttrend<-function(produce2){
      values <- data.frame(Yearcon=c(1:10),year=c(2004:2013))
      produce2 <- merge(produce2,values, by="year", all.x = TRUE)
      produce2["mYear"]<-produce2$Yearcon-mean(produce2$Yearcon) 
      return(invisible(produce2))
    }
    
    
    Lst<-list(Bean,pBean, Corn, pCorn, Sorghum, pSorghum, Wheat, pWheat)
    
    temp<-lapply(Lst, Mean_sc)
    Beanf<-temp[[1]]
    pBeanf<-temp[[2]]
    Cornf<-temp[[3]]
    pCornf<-temp[[4]]
    Sorghumf<-temp[[5]]
    pSorghumf<-temp[[6]]
    Wheatf<-temp[[7]]
    pWheatf<-temp[[8]]
    
    Lst2<-list(Beanf,pBeanf, Cornf, pCornf, Sorghumf, pSorghumf, Wheatf, pWheatf)
    temp<-lapply(Lst2, Ttrend)
    Beanf<-temp[[1]]
    pBeanf<-temp[[2]]
    Cornf<-temp[[3]]
    pCornf<-temp[[4]]
    Sorghumf<-temp[[5]]
    pSorghumf<-temp[[6]]
    Wheatf<-temp[[7]]
    pWheatf<-temp[[8]]
    
    
    
    ##Some tests about the mean scaled vars  ######
    
    #colMeans(Dat[,c("prod", "dryl", "irrigl", "wk", "ci", "cb", "fi", "lab", "tk")])
    #log(colMeans(Dat[,c("prod", "dryl", "irrigl", "wk", "ci", "cb", "fi", "lab", "tk")]))
    
    
    
    
    ### 5.- We will use logarithmic inputs and outputs in Cobb-Douglas and probably Translog
    ###     specifications. We create variables with logarithmic (mean-scaled) input and 
    ###     output quantities:
    # Dat$lProd<- log(Dat$prod)
    # Dat$lDryl<- log(Dat$dryl)
    # Dat$lIrrigl<-log(Dat$irrigl)
    # Dat$lTotl<-log(Dat$totl)
    # Dat$lWk<- log(Dat$wk)
    # Dat$lCi<- log(Dat$ci)
    # Dat$lCb<- log(Dat$cb)
    # Dat$lFi<- log(Dat$fi)
    # Dat$lLab<- log(Dat$lab)
    # Dat$ltk<- log(Dat$tk)
    
    saveRDS(Cornf, file="SFA_Corn.rds")
    saveRDS(Beanf, file="SFA_Bean.rds")
    saveRDS(Wheatf, file="SFA_Wheat.rds")
    saveRDS(Sorghumf, file="SFA_Sorghum.rds")
    
    
    saveRDS(pCornf, file="pSFA_Corn.rds")
    saveRDS(pBeanf, file="pSFA_Bean.rds")
    saveRDS(pWheatf, file="pSFA_Wheat.rds")
    saveRDS(pSorghumf, file="pSFA_Sorghum.rds")
    
    
   
###### PART FOUR  ############################
#######  ADDITIONAL USEFUL VARIABLES AND PANEL DATA STRUCTURE  ########    

    
    #####Update Dat #########
    Dat<-rbind(Bean,Corn,Sorghum,Wheat)
    
   
     
  ### 1.- In some model specifications we are going to use Mean-scaling quantities.
  ###     We create new variable with mean-scaled input and output quantities: 
  Dat$prod<-Dat$PROD/mean(Dat$PROD)
  Dat$dryl<-Dat$DRYL/mean(Dat$DRYL)
  Dat$irrigl<-Dat$IRRIGL/mean(Dat$IRRIGL)
  Dat$totl<-Dat$TOTL/mean(Dat$TOTL)
  Dat$wk<-Dat$WK/mean(Dat$WK)
  Dat$ci<-Dat$CI/mean(Dat$CI)
  Dat$cb<-Dat$CB/mean(Dat$CB)
  Dat$fi<-Dat$FI/mean(Dat$FI)
  Dat$lab<-Dat$LAB/mean(Dat$LAB)
  Dat$tk<-Dat$TK/mean(Dat$TK)
  
  ####  Mean-scaled Variables are in lower case   ########################
  ##Some tests about the mean scaled vars  ######
  
   colMeans(Dat[,c("prod", "dryl", "irrigl", "wk", "ci", "cb", "fi", "lab", "tk")])
   log(colMeans(Dat[,c("prod", "dryl", "irrigl", "wk", "ci", "cb", "fi", "lab", "tk")]))
  
  
  
  
  ### 5.- We will use logarithmic inputs and outputs in Cobb-Douglas and probably Translog
  ###     specifications. We create variables with logarithmic (mean-scaled) input and 
  ###     output quantities:
  # Dat$lProd<- log(Dat$prod)
  # Dat$lDryl<- log(Dat$dryl)
  # Dat$lIrrigl<-log(Dat$irrigl)
  # Dat$lTotl<-log(Dat$totl)
  # Dat$lWk<- log(Dat$wk)
  # Dat$lCi<- log(Dat$ci)
  # Dat$lCb<- log(Dat$cb)
  # Dat$lFi<- log(Dat$fi)
  # Dat$lLab<- log(Dat$lab)
  # Dat$ltk<- log(Dat$tk)
  
  
### 6.- Mean-adjusting the Time Trend
  ###     In some models it is an advantage to have time trend variable that is zero at the 
  ###     sample mean. First we need to convert real years into consecutive numbers
  
  values <- data.frame(Yearcon=c(1:10),year=c(2004:2013))
  Dat <- merge(Dat,values, by="year", all.x = TRUE)
  rm(values)
  Dat$mYear<-Dat$Yearcon-mean(Dat$Yearcon)    
  
  
  l.df <- setNames(lapply(ls(pattern="p"), function(x) get(x)), ls(pattern = "p"))
  
  PrepDat2 = function(Dat_ok)
  {  
  return(Dat_ok)
    }
    
    out2<-lapply(l.df, PrepDat2)
    
    SFA_Bean<-out2$SFA_Bean
    SFA_Maize<-out2$SFA_Maize
    SFA_Sorghum<-out2$SFA_Sorghum
    SFA_Wheat<-out2$SFA_Wheat
    
  
  #### NOTE: I will have some problems with cells that have zeros, convert logs in inf
  
  
  ### 7.- Specifying Panel Structure
  library("plm")
  pSFA_Bean<-plm.data(SFA_Bean,c("mun","Yearcon"))
  pSFA_Bean$yearcon <- as.numeric( pSFA_Bean$Yearcon )
  
  pSFA_Maize<-plm.data(SFA_Maize,c("mun","Yearcon"))
  pSFA_Maize$yearcon <- as.numeric( pSFA_Maize$Yearcon )
  
  pSFA_Sorghum<-plm.data(SFA_Sorghum,c("mun","Yearcon"))
  pSFA_Sorghum$yearcon <- as.numeric( pSFA_Sorghum$Yearcon )
  
  pSFA_Wheat<-plm.data(SFA_Wheat,c("mun","Yearcon"))
  pSFA_Wheat$yearcon <- as.numeric( pSFA_Wheat$Yearcon )
  
  
  rm(l.df, out, out2)
  
  # pDat_outlier<-pdata.frame(Dat_outlier, index=c("mun","year"), drop.index=TRUE, row.names=TRUE)
  # head(pDat_outlier)
  # head(attr(pDat_outlier, "index"))
  # head(as.matrix(pDat_outlier$PROD))
  

  saveRDS(SFA_Maize, file="SFA_Corn.rds")
  saveRDS(SFA_Bean, file="SFA_Bean.rds")
  saveRDS(SFA_Wheat, file="SFA_Wheat.rds")
  saveRDS(SFA_Sorghum, file="SFA_Sorghum.rds")
  saveRDS(pSFA_Maize, file="SFA_Corn.rds")
  saveRDS(pSFA_Bean, file="SFA_Bean.rds")
  saveRDS(pSFA_Wheat, file="SFA_Wheat.rds")
  saveRDS(pSFA_Sorghum, file="SFA_Sorghum.rds")
  

  
  
  
 
  
