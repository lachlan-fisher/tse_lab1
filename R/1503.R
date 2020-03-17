
# PREPARE ENVIRONMENT -----------------------------------------------------

  library(tidyverse)
  library(here)
  library(ggplot2)
  library(scales)
  
# LOAD DATA ---------------------------------------------------------------

  dat <- read.table(file=here::here("data", "m-dec12910.txt"))

  
# TIDY DATA ---------------------------------------------------------------

  dat <- read.table(file=here::here("data", "m-dec12910.txt"), header=TRUE)
  yrs <- substr(as.numeric(dat$date), start=1, stop=4)
  mths <- substr(as.numeric(dat$date), start=5, stop=6)
  days <- substr(as.numeric(dat$date), start=7, stop=8)
  dates <- as.Date.character(paste(yrs,'-',mths,'-',days, sep=''))
  colnames(dat)<-c('date','dec1','dec2','dec9','dec10')
  dat <- data.frame(dates, dat$dec1, dat$dec2, dat$dec9, dat$dec10)
  colnames(dat)<-c('date','dec1','dec2','dec9','dec10')
  
  
# 1) PLOT RETURN SERIES FOR THE FOUR PORTFOLIOS ---------------------------
  
  #plotting decile 1
  ggplot(data=dat)+
    geom_point(mapping=aes(x=date,y=dec1))+
    labs(title='Decile 1 Returns',x='Date',y="Return Percentage")+
    scale_y_continuous(labels=scales::percent)
  
  #plotting decile 2
  ggplot(data=dat)+
    geom_point(mapping=aes(x=date,y=dec2))+
    labs(title='Decile 2 Returns',x='Date',y="Return Percentage")+
    scale_y_continuous(labels=scales::percent)
  
  #plotting decile 9
  ggplot(data=dat)+
    geom_point(mapping=aes(x=date,y=dec9))+
    labs(title='Decile 9 Returns',x='Date',y="Return Percentage")+
    scale_y_continuous(labels=scales::percent)
  
  #plotting decile 10
  ggplot(data=dat)+
    geom_point(mapping=aes(x=date,y=dec10))+
    labs(title='Decile 10 Returns',x='Date',y="Return Percentage")+
    scale_y_continuous(labels=scales::percent)
  

# 2)  CORRELATIONS BETWEEN DECILE RETURNS ---------------------------------

  #Correlation between Decile 1 and Decile 9
  cor(dat$dec1,dat$dec9)
  
  #Correlation between Decile 1 and Decile 10
  cor(dat$dec1,dat$dec10)
  
  #Correlation between Decile 2 and Decile 9
  cor(dat$dec2,dat$dec9)
  
  #Correlation between Decile 2 and Decile 10
  cor(dat$dec2,dat$dec10)
  
  
# 3) PLOTTING DECILES INVESTMENT SLOPES -----------------------------------

  #Finding return on dollar invested into Decile 1
  dold1<-1*(1+dat$dec1)
  
  #Finding return on dollar invested into Decile 2
  dold2<-1*(1+dat$dec2)
  
  #Finding return on dollar invested into Decile 9
  dold9<-1*(1+dat$dec9)
  
  #Finding return on dollar invested into Decile 10
  dold10<-1*(1+dat$dec10)
  
  #Plot returns
  ggplot(data=dat)+
    geom_line(mapping=aes(x=dates,y=dold1),colour="blue")+
    geom_line(mapping=aes(x=dates,y=dold2),colour="red")+
    geom_line(mapping=aes(x=dates,y=dold9),colour="green")+
    geom_line(mapping=aes(x=dates,y=dold10),colour="orange")+
    labs(title="Returns on Dollar Portfolio, 1967-2010",y="Returns",x="Date")+
    scale_y_continuous(labels = dollar)


# 4) PLOTTING ACF RETURNS -------------------------------------------------

  #Decile 1 ACF
  acfd1 <- acf(dat$dec1, lag=30, plot=FALSE)
  plot(acfd1, main="Decile 1 ACF with Maximum Lag of 30")
  
  #Decile 2 ACF
  acfd2 <- acf(dat$dec2, lag=30, plot=FALSE)
  plot(acfd2, main="Decile 2 ACF with Maximum Lag of 30")
  
  #Decile 9 ACF
  acfd9 <- acf(dat$dec9, lag=30, plot=FALSE)
  plot(acfd9, main="Decile 9 ACF with Maximum Lag of 30")
  
  #Decile 10 ACF
  acfd10 <- acf(dat$dec10, lag=30 ,plot=FALSE) 
  plot(acfd10, main="Decile 10 ACF with Maximum Lag of 30")

  
# 5) AUTOCORRELATION TEST -------------------------------------------------

  #1) VALUE OF RHO 5
  rho_5 <- acf(dat$dec1, lag=5, plot=FALSE)
  rho_5$acf[5]
  
  #2) PRECISE VALUE OF T-RATIO RHO 5
  tt=rho_5$acf[5]*sqrt(516)
  
  #3) APPROXIMATE VALUE OF T-RATIO RHO 5
  tt
  
  #4) HYPOTHESIS TEST
  # H_0: p_5=0
  # H_A: p_5/=0
  # tt:=0.8864176, t_crit=1.96, as tt<t_crit p_5 is significantly different
  # from zero.
      