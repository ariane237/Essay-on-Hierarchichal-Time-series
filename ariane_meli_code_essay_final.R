##############################Code for Hierachical time series methods#####################################

#Change the working directory
setwd("/home/aimsstudent05")

#load the data into R
mu= read.csv("/home/aimsstudent05/Downloads//Data/fire_month_per_municipality_final.csv", header = T)
biome = read.csv("/home/aimsstudent05/Downloads//Data/Fires_Month_Biomes.csv", header = T)
attach(mu)

#load useful packages for hierarchical forecasting
library(fpp2)
library(forecast)
library(hts)
library(tseries)

#load library for boxplot and Excel files
library(readxl)
library(ggplot2)
library(tidyr)
library(gridExtra)

#create times series of data
muts = ts(mu, frequency = 12, start = c(2011,1))

biomets = ts(biome, frequency = 12, start = c(2011,1))

#starting by removing the column containing the dates
mutsM= muts[,3:5573]
biometsM= biomets[,2:7]

#plots of times series of 10 municipalities
plot.ts(mutsM[, 35:44], main="Fire per month in 10 municipalities", xlab= "time", col="blue")
plot.ts(biometsM, main="Fire per month in each biome", xlab= "time", col='blue')

#define the hierarchical time series 
#A : Malta-Atlantica : 3082, B: Amazonia : 558, B : Caatinga : 1064, C : Pampa : 92, D: Pantamal : 16, E : Cerrado : 759

y <- hts(mutsM, nodes=list(6 , c(3082, 558, 1064, 92, 16, 759)))

#plot of the two first levels
y %>% aggts(levels=0:1) %>% autoplot(facet=TRUE) + xlab("Year") + ylab("Number of fires") + ggtitle("Fire in brazil") 
+ geom_line(color="turquoise4", lwd = 1) +theme_bw() + 
  theme(plot.title = element_text(hjust=0.5, size=16, face="bold"), 
  axis.title.x = element_text(color = "grey20", size = 16, angle = 0, hjust = .5, vjust = 0, face = "plain"),
                                                                                                                                                                                   axis.title.y = element_text(color = "grey20", size = 16, hjust = .5, face = "plain"), axis.text.x = element_text(color = "grey20", size = 8, angle = 0, hjust = .5, vjust = .5, face = "plain"),
                                                                                                                                                                                   axis.text.y = element_text(color = "grey20", size = 8, angle = 0, hjust = 1, vjust = 0, face = "plain"))


#performance of the models using training and test sets approach

fmes= c("ets", "arima")
mes = c( "tdgsf", "tdgsa", "bu", "tdfp", "mo", "com")
for( m in mes)
{for(mf in fmes)
{ 
  datay1<- window(y, start=c(2011, 1), end = c(2019,12))
  testy1 <- window(y, start=c(2020,1), end = c(2020,12))
  if(m == "mo"){
    foretd <- forecast(datay1, h=12, level=1,  method=m , fmethod = mf)
  } else {
    foretd <- forecast(datay1, h=12, method=m , fmethod = mf)
  }
  h = accuracy(foretd , testy1)
  write.table(t(h), paste('h_',m, '_', mf, '_', '.csv'))

}
  
}


### Generate tables of Mase for each forecasting horizon with base forecast ARIMA 
mess = c( "tdgsf", "tdgsa" )
mf = "arima"
# LEVEL 0
level0_arima = data.frame()

for (m in mess) {
  row = c(m)
  for (i in c(0:11)) {
    df = read.csv(paste('h_',m, '_', mf, '_', toString(i), '.csv'), sep = ' ')
    df$MASE[is.infinite(df$MASE)] = NA
    row <- append(row, mean(df$MASE[1], na.rm = TRUE))
  }
  level0_arima = rbind(level0_arima, t(row))
}

# LEVEL 1
level1_arima = data.frame()

for (m in mess) {
  row = c(m)
  for (i in c(0:11)) {
    df = read.csv(paste('h_',m, '_', mf, '_', toString(i), '.csv'), sep = ' ')
    df$MASE[is.infinite(df$MASE)] = NA
    row <- append(row, mean(df$MASE[2:7], na.rm = TRUE))
  }
  level1_arima = rbind(level1_arima, t(row))
}
head(level1_arima)

# LEVEL 2

level2_arima = data.frame()

for (m in mess) {
  row = c(m)
  for (i in c(0:11)) {
    df = read.csv(paste('h_',m, '_', mf, '_', toString(i), '.csv'), sep = ' ')
    df$MASE[is.infinite(df$MASE)] = NA
    row <- append(row, mean(df$MASE[8:length(df$MASE)], na.rm = TRUE))
  }
  level2_arima = rbind(level2_arima, t(row))
}
head(level1_arima)

ARIMA_table = rbind(level0_arima, level1_arima, level2_arima)
head(ARIMA_table)

### ETS Generate tables of Mase for each forecasting horizon with base forecast
mess = c( "tdgsf", "tdgsa", "bu", "mo")
mf = "ets"
# LEVEL 0
level0_ets = data.frame()

for (m in mess) {
  row = c(m)
  for (i in c(0:11)) {
    df = read.csv(paste('h_',m, '_', mf, '_', toString(i), '.csv'), sep = ' ')
    df$MASE[is.infinite(df$MASE)] = NA
    row <- append(row, mean(df$MASE[1], na.rm = TRUE))
  }
  level0_ets = rbind(level0_ets, t(row))
}

# LEVEL 1
level1_ets = data.frame()

for (m in mess) {
  row = c(m)
  for (i in c(0:11)) {
    df = read.csv(paste('h_',m, '_', mf, '_', toString(i), '.csv'), sep = ' ')
    df$MASE[is.infinite(df$MASE)] = NA
    row <- append(row, mean(df$MASE[2:7], na.rm = TRUE))
  }
  level1_ets = rbind(level1_ets, t(row))
}

# LEVEL 2

level2_ets = data.frame()

for (m in mess) {
  row = c(m)
  for (i in c(0:11)) {
    df = read.csv(paste('h_',m, '_', mf, '_', toString(i), '.csv'), sep = ' ')
    df$MASE[is.infinite(df$MASE)] = NA
    row <- append(row, mean(df$MASE[8:length(df$MASE)], na.rm = TRUE))
  }
  level2_ets = rbind(level2_ets, t(row))
}

ETS_table = rbind(level0_ets, level1_ets, level2_ets)


write.table(ETS_table, 'ETS_table.csv')

write.table(ARIMA_table, 'ARIMA_table.csv')








#BOXplot for comparison

BM <- read_excel("/home/aimsstudent05/Downloads/Base_donnees/BMArimaExponentielle.xlsx")


BoxBiomes<-gather(BM, key="Paramètres", value="Valeur",
                  BU, GSF, GSA, FP, MO, COM)

p1<-ggplot(BoxBiomes, aes(Paramètres, Valeur,fill=Methods))+geom_boxplot()
p2<-p1+labs(x=NULL, y=NULL, title=NULL)+theme_light()+theme(legend.position = "bottom")
p3<-p2+stat_summary(fun=mean, geom="point", shape=18,size=2.5, col="red",
                    position=position_dodge(width=0.75))
graph<-p3+facet_grid(~Level, margins = TRUE)
graph



#Performance of model using time series cross-validation



#step one for all the methods

fmes= c("ets", "arima")
mes = c( "tdgsf", "tdgsa", "bu", "tdfp", "mo", "com")
for( m in mes)
{for(mf in fmes)
{ for (i in c(0:0)){
  datay1<- window(y, start=c(2011, 1), end = c(2019,12))
  testy1 <- window(y, start=c(2020,i+1), end = c(2020,i+1))
  if(m == "mo"){
    foretd <- forecast(datay1, h=1, method=m , level=1, fmethod = mf)
  } else {
    foretd <- forecast(datay1, h=1, method=m , fmethod = mf)
  }
  h = accuracy(foretd , testy1)
  write.table(t(h), paste('h_',m, '_', mf, '_', toString(i), '.csv'))
}
  
}
  
}



#step2 to 12 for all the methods

fmes= c("ets", "arima")
mes = c("tdgsf", "tdgsa", "bu", "tdfp", "mo", "com")
for( m in mes)
{for(mf in fmes)
{ for (i in c(1:11)){
  datay1<- window(y, start=c(2011, 1), end = c(2020,i))
  testy1 <- window(y, start=c(2020,i+1), end = c(2020,i+1))
  if(m == "com"){
    foretd <- forecast(datay1, h=1, method=m , level=1, fmethod = mf)
  } else {
    foretd <- forecast(datay1, h=1, method=m , fmethod = mf)
  }
  h = accuracy(foretd , testy1)
  write.table(t(h), paste('h_',m, '_', mf, '_', toString(i), '.csv'))
}
  
}
  
}

### Generate tables of Mase for each forecasting horizon with base forecast ARIMA 


mess = c( "tdgsf", "tdgsa", "bu", "tdfp", "mo", "com")
mf = "arima"
# LEVEL 0
level0_arima = data.frame()

for (m in mess) {
  row = c(m)
  for (i in c(0:11)) {
    df = read.csv(paste('h_',m, '_', mf, '_', toString(i), '.csv'), sep = ' ')
    df$MASE[is.infinite(df$MASE)] = NA
    row <- append(row, mean(df$MASE[1], na.rm = TRUE))
  }
  level0_arima = rbind(level0_arima, t(row))
}

# LEVEL 1
level1_arima = data.frame()

for (m in mess) {
  row = c(m)
  for (i in c(0:11)) {
    df = read.csv(paste('h_',m, '_', mf, '_', toString(i), '.csv'), sep = ' ')
    df$MASE[is.infinite(df$MASE)] = NA
    row <- append(row, mean(df$MASE[2:7], na.rm = TRUE))
  }
  level1_arima = rbind(level1_arima, t(row))
}
head(level1_arima)

# LEVEL 2

level2_arima = data.frame()

for (m in mess) {
  row = c(m)
  for (i in c(0:11)) {
    df = read.csv(paste('h_',m, '_', mf, '_', toString(i), '.csv'), sep = ' ')
    df$MASE[is.infinite(df$MASE)] = NA
    row <- append(row, mean(df$MASE[8:length(df$MASE)], na.rm = TRUE))
  }
  level2_arima = rbind(level2_arima, t(row))
}
head(level1_arima)

ARIMA_table = rbind(level0_arima, level1_arima, level2_arima)
head(ARIMA_table)

###  Generate tables of Mase for each forecasting horizon with base forecast ETS 
mess = c( "tdgsf", "tdgsa")
mf = "ets"
# LEVEL 0
level0_ets = data.frame()

for (m in mess) {
  row = c(m)
  for (i in c(0:11)) {
    df = read.csv(paste('h_',m, '_', mf, '_', toString(i), '.csv'), sep = ' ')
    df$MASE[is.infinite(df$MASE)] = NA
    row <- append(row, mean(df$MASE[1], na.rm = TRUE))
  }
  level0_ets = rbind(level0_ets, t(row))
}

# LEVEL 1
level1_ets = data.frame()

for (m in mess) {
  row = c(m)
  for (i in c(0:11)) {
    df = read.csv(paste('h_',m, '_', mf, '_', toString(i), '.csv'), sep = ' ')
    df$MASE[is.infinite(df$MASE)] = NA
    row <- append(row, mean(df$MASE[2:7], na.rm = TRUE))
  }
  level1_ets = rbind(level1_ets, t(row))
}

# LEVEL 2

level2_ets = data.frame()

for (m in mess) {
  row = c(m)
  for (i in c(1:11)) {
    df = read.csv(paste('h_',m, '_', mf, '_', toString(i), '.csv'), sep = ' ')
    df$MASE[is.infinite(df$MASE)] = NA
    row <- append(row, mean(df$MASE[8:length(df$MASE)], na.rm = TRUE))
  }
  level2_ets = rbind(level2_ets, t(row))
}

ETS_table = rbind(level0_ets, level1_ets, level2_ets)





#####################################two level hierarchy#########################################
#create times series of data
biomets = ts(biome, frequency = 12, start = c(2011,1))

#starting by removing the column containing the dates
biometsM= biomets[,2:7]

#plots of times series 
plot.ts(biometsM, main="Fire per month in each biome", xlab= "time", col='blue')

#define the hierarchical time series 
#A : Malta-Atlantica : 1, B: Amazonia : 1, B : Caatinga : 1, C :1 : 1, D: Pantamal : 1, E : Cerrado : 1.

y <- hts(biometsM, nodes=list( c(6)))

#plot of the two first levels
y %>% aggts(levels=0:1) %>% autoplot(facet=TRUE) + xlab("Year") + ylab("Number of fires") + ggtitle("Fire in brazil")

#performance of the models 

fmes= c("ets", "arima")
mes = c( "tdgsf", "tdgsa", "bu", "tdfp", "com")
for( m in mes)
{for(mf in fmes)
{ 
  datay1<- window(y, start=c(2011, 1), end = c(2019,12))
  testy1 <- window(y, start=c(2020,1), end = c(2020,12))
  
  foretd <- forecast(datay1, h=12, method=m , fmethod = mf)
  
  hb = accuracy(foretd , testy1)
  write.table(t(hb), paste('hb_',m, '_', mf, '_', '.csv'))
}
  
  
}


##Plot of the reconciled forecast versus the test_data
#data was import from python

reconcilled_data = c(37242.7109375, 38009.140625, 34866.01171875, 14713.015625, 
                     13063.7939453125, 3893.458251953125, 3341.1806640625,
                     3082.37841796875,
                     3451.781494140625,
                     5038.03857421875,
                     8899.6845703125,
                     15350.9736328125,
                     28886.521484375,
                     44388.02734375,
                     33058.4375,
                     17258.251953125,
                     7191.01904296875,
                     2235.36083984375,
                     1190.597412109375,
                     1216.833740234375,
                     2088.313232421875,
                     4281.48583984375,
                     5249.46337890625,
                     19022.529296875,
                     36353.625,
                     44968.3828125,
                     42206.203125,
                     21448.11328125)

test_data= c(22773.99885642, 42250.99865121, 19567.99917287, 13013.99920733,
             5113.00006841,  4030.00007206,  2864.9999222,   5213.00000783,
             2842.00003238,  2963.00003394,  7257.99999896, 13393.99961881,
             51934.99843189, 53234.00093888, 25612.99914728, 20585.0003107,
             7700.00041566,  2866.00010339,  2656.99993055,  3879.99989556,
             4117.00006214,  4002.00007833,  7109.00000366, 15803.99944231,
             50694.00162189, 69328.99910342, 41467.9987405,  13463.00009034)


plot(c(1:length(test_data)), test_data, type="l", col="blue", xlab = "Draw up date.index", ylab = "Number of spot fire per month", lwd=2.0)
lines(c(1:length(test_data)), reconcilled_data, col="red", lwd=2.0)
title("Plot of LSTM model of the Top level")
legend("topleft", legend=c("Test Data", "Reconcilled Data"),
       col=c("blue", "red"), lty = 1:2, cex=0.8)



