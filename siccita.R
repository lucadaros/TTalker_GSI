# scarico i dati di entrambi i siti sperimentali e unisco le tabelle aggiungendo
# una colonna che identifica i due siti sperimentali, Ultimo e Carezza
# confronto tra abeti in condizioni diverse
# devtools::install_github("EnricoTomelleri/ttalkR")

library(ttalkR)
library(lubridate)
library(dplyr)

setwd("C:/Users/LucDaRos/OneDrive - Scientific Network South Tyrol/AR_Tomelleri/siccita")

## TTCLOUD 91 CAREZZA

ttScrape("C0200091", 900) ##numero di giorni

ttBattery(mydata_4B, mydata_4D, "none")
car_batt <- df_ttBattery

ttGranier(mydata_4D, "none")
car_gran <- df_ttGranier

ttRH(mydata_4D, "none")
car_RH <- df_ttRH 
  
ttStWC(mydata_4D, "spruce", "none")
car_StWC <- df_ttStWC

ttTair(mydata_4D, "none") 
car_Tair <- df_ttTair

ttGrowth(mydata_4D, "none")
car_Growth <- df_ttGrowth
  
ttLight(mydata_49, lat=46.45, lon=11.25, 860, "none")
car_Light <- df_ttLight

# innerjoin of all carezza tables, NO light as the data are not complete
carezza1 <- inner_join(car_batt,car_gran, by = c("Timestamp", "TT_ID"))
carezza2 <- inner_join(carezza1,car_RH, by = c("Timestamp", "TT_ID"))
carezza3 <- inner_join(carezza2,car_StWC, by = c("Timestamp", "TT_ID"))
carezza4 <- inner_join(carezza3,car_Growth, by = c("Timestamp", "TT_ID"))
carezza5 <- inner_join(carezza4,car_Tair, by = c("Timestamp", "TT_ID"))

#add a column that identifies the site

carezza5$site <- "carezza"

## TTCLOUD 92 ULTIMO
ttScrape("C0200092", 900) ##numero di giorni

ttBattery(mydata_4B, mydata_4D, "none")
ult_batt <- df_ttBattery

ttGranier(mydata_4D, "none")
ult_gran <- df_ttGranier

ttRH(mydata_4D, "none")
ult_RH <- df_ttRH 

ttStWC(mydata_4D, "spruce", "none")
ult_StWC <- df_ttStWC

ttTair(mydata_4D, "none") 
ult_Tair <- df_ttTair
  
ttGrowth(mydata_4D, "none")
ult_Growth <- df_ttGrowth

ttLight(mydata_49, lat=46.45, lon=11.25, 860, "none")
ult_Light <- df_ttLight

# innerjoin of all carezza tables, NO light as the data are not complete
ultimo1 <- inner_join(ult_batt,ult_gran, by = c("Timestamp", "TT_ID"))
ultimo2 <- inner_join(ultimo1,ult_RH, by = c("Timestamp", "TT_ID"))
ultimo3 <- inner_join(ultimo2,ult_StWC, by = c("Timestamp", "TT_ID"))
ultimo4 <- inner_join(ultimo3,ult_Growth, by = c("Timestamp", "TT_ID"))
ultimo5 <- inner_join(ultimo4,ult_Tair, by = c("Timestamp", "TT_ID"))

#add a column that identifies the site

ultimo5$site <- "ultimo"

#unire i due dataframe per timestamp
joint <- rbind(ultimo5, carezza5)

joint$TT_ID <- as.factor(joint$TT_ID)
joint$site <- as.factor(joint$site)

joint$ymd<-as.Date(joint$Timestamp, format="%Y%m%d") # seleziono anno mese giorno per avere dati orari raggruppati a livello giornaliero

#se crasha
saveRDS(joint, file="joint.RData")
joint <- readRDS("joint.RData")

class(joint$Timestamp)

## con paper di Jolly et al 2005 individuo i giorni di stress Temmin
TMMin<- -2
TMMax<- 5

iTmin <- joint%>%group_by(site, ymd, TT_ID)%>%summarise(Tmin=min(Tair))%>%ungroup()%>%
  mutate(iTmin = ifelse(Tmin<=TMMin, 0,ifelse(Tmin>=TMMax, 1, ((Tmin-TMMin)/(TMMax-TMMin)))))%>% #jolly et al 2005
  select(site,ymd,TT_ID, iTmin)
## VPD
# a volte il minutaggio è sballato e risulta alle ore 09:00:01 quindi 
#filtro timestamp per avere solo ymdh, rimuovo minuti e secondi così da poter raggruppare
joint$ymdh<-round_date(joint$Timestamp, "minute") # arrotondo il minutaggio

# formula VPD presa qui https://betterorganix.com/blog/what-is-how-to-calculate-vapour-pressure-deficit/
VPDmin <- 900
VPDmax <- 4100

iVPD <- joint%>%group_by(site, ymdh, TT_ID)%>%dplyr::filter(RH <= 10)%>%
  mutate(VPsat = (610.7*10^((7.5*Tair)/(237.3+Tair))))%>% #assumo che la Temp della foglia sua = a Tair
  mutate(VPair = (610.7*10^((7.5*Tair)/(237.3+Tair)))*(RH/10))%>% #RH diviso 10 perchè è gia diviso 10 nel pacchetto TTalkR 
  mutate(VPD = VPsat-VPair)%>%
  mutate(iVPD = ifelse(VPD<=VPDmin, 1,ifelse(VPD>=VPDmax, 0, (1-(VPD-VPDmin)/(VPDmax-VPDmin)))))%>% #jolly et al 2005
  ungroup()%>%
  group_by(site, ymd, TT_ID)%>%
  summarise(iVPD=min(iVPD))%>% #SELEZIONO IL VALORE MINIMO DI FITNESS DELLA GIORNATA
  select(site,ymd,TT_ID, iVPD)

# Fotoperiodo
PHOTOmin <- 10
PHOTOmax <- 11

# Below is a function in R that will estimate photoperiod based on inputs of latitude and day of the year.  
# The function below is based on the equations provided by Keisling (1982).
# https://pablorosasanderson.wordpress.com/2017/02/23/r-a-function-for-estimating-photoperiod-based-on-latitude-and-day-of-the-year/

photoperiod<-function(latitude, day_of_year) {
  #This angle can be changed depending on the plants ability to
  #detect light 
  angle_below_sun = 6
  #zenith distance of the sun:
  zenith= 90 + angle_below_sun
  
  #function to calculate secant
  secant=function(x) {1/cos(x)}
  
  #R works with radians so it is necessary to convert
  #radian to degrees when using trig functions in the equations
  #that follow
  radian=pi/180
  
  sun_mean_anomaly=0.985600*day_of_year-3.251
  lambda=sun_mean_anomaly+1.916*sin(sun_mean_anomaly*radian)+
    0.020*sin(2*sun_mean_anomaly*radian)+282.565
  
  #Declination of the sun:
  sun_declination= asin(0.39779*sin(lambda*radian))/radian
  
  #calculate daylength 
  photoperiod = (2/15) * acos(cos(zenith*radian)*
                                secant(latitude*radian)*secant(sun_declination*radian)-
                                tan(latitude*radian)*tan(sun_declination*radian))
  #convert back
  photoperiod=photoperiod/radian
  return(photoperiod) 
}

joint$lat <-  46.45 #latitudine dei siti sperimentali   
joint$dayofyear <- yday(joint$Timestamp)

iPHOTO <- joint%>%group_by(site, ymd, TT_ID)%>%mutate(PHOTO=photoperiod(lat, dayofyear))%>%ungroup()%>%
  mutate(iPHOTO = ifelse(PHOTO<=PHOTOmin, 0,ifelse(PHOTO>=PHOTOmax, 1, ((PHOTO-PHOTOmin)/(PHOTOmax-PHOTOmin)))))%>%
  select(site,ymd,TT_ID, iPHOTO)

str(iPHOTO)

# collego le tre tabelle con dati per TTalker giornalieri, con fit da 0 a 1 dei tre valori 
# per la formula del Growing Season Index (GSI) secondo jolly et al 2005

GSI_raw <- inner_join(iTmin,iVPD, by = c("site", "ymd", "TT_ID"))
GSI_raw2 <- inner_join(GSI_raw,iPHOTO, by = c("site", "ymd", "TT_ID"))

GSI <- GSI_raw2 %>% 
  dplyr::filter(ymd<="2021-12-31" & ymd>="2021-01-01")%>%#riduco il dataset a un solo anno TEST
  mutate(GSI=(iTmin*iVPD*iPHOTO))%>%unique()%>%
  group_by(site, ymd)%>% mutate(medianGSI=median(GSI))%>%
  # faccio mediana per sito sperimentale
  # ottengo valore di GSI ho un valore per i 20 dati giornalieri
  tidyr::gather(4:6, key="key", val="val")%>%
  ungroup()%>%
  group_by(site, ymd, key)%>% mutate(mediankey=mean(val))%>%
  select(site, ymd, GSI, medianGSI , key, mediankey)%>%unique()
  
# ROLLING AVERAGE
library(zoo) # moving averages        
GSI_roll <- GSI %>% ungroup()%>%
  group_by(site, key)%>%
  dplyr::mutate(roll_3 = zoo::rollmean(mediankey, k = 3, fill = NA),
                roll_5 = zoo::rollmean(mediankey, k = 5, fill = NA),
                roll_7 = zoo::rollmean(mediankey, k = 7, fill = NA),
                roll_12 = zoo::rollmean(mediankey, k = 12, fill = NA),
                roll_21 = zoo::rollmean(mediankey, k = 21, fill = NA))%>%
  dplyr::mutate(rollGSI_21 = zoo::rollmean(medianGSI, k = 3, fill = NA))
                
# plotto come pagina 625 di Jolly et al 2005
library(ggplot2)

ggplot(GSI, aes(x=ymd, y=mediankey, fill=key))+ geom_area(position = 'identity') + 
  facet_grid(site~key)+theme_bw()+ggtitle("GSI_median_value_each_day")

# plotto con ROLLIN AVERAGE 3gg
ggplot(GSI_roll, aes(x=ymd, y=roll_3, fill=key))+ geom_area(position = 'identity') + 
  facet_grid(site~key)+theme_bw()+ggtitle("GSI_rolling_avg_3days")

# plotto con ROLLIN AVERAGE 7gg
ggplot(GSI_roll, aes(x=ymd, y=roll_7, fill=key))+ geom_area(position = 'identity') + 
  facet_grid(site~key)+theme_bw()+ggtitle("GSI_rolling_avg_7days")

# plotto con ROLLIN AVERAGE 12gg
ggplot(GSI_roll, aes(x=ymd, y=roll_12, fill=key))+ geom_area(position = 'identity') + 
  facet_grid(site~key)+theme_bw()+ggtitle("GSI_rolling_avg_12days")

# plotto con ROLLIN AVERAGE 21gg
ggplot(GSI_roll, aes(x=ymd, y=roll_21, fill=key))+ geom_area(position = 'identity') + 
  facet_grid(site~key)+theme_bw()+ggtitle("GSI_rolling_avg_21days")

# plotto con ROLLIN AVERAGE 21gg position identity all parameters
ggplot(GSI_roll, aes(x=ymd, y=roll_21, fill=key))+ geom_area(position = 'identity', alpha=0.55) + 
  facet_grid(~site)+theme_bw()+ggtitle("GSI_rolling_avg_21days") + geom_line(aes(y=rollGSI_21))

# plotto con ROLLIN AVERAGE 21gg position identity GSI
ggplot(GSI_roll, aes(x=ymd, y=medianGSI ))+ geom_area(position = 'identity') + 
  facet_grid(~site)+theme_bw()+ggtitle("GSI_rolling_avg_21days")

