
# author = "Besognet Thomas"
# date = " 23/05/23" 
# project = "Korpela et al. Proceedings B Replication work"
 # name = "Data management"
  
# packages 
install.packages("dplyr")
library(dplyr)
install.packages("epiR")
library(epiR)
install.packages("epiDisplay")
library(epiDisplay)

# original data importation 
tab <- read.table("../data/Korpela et al PRSL 2014 data.txt", sep="\t", header=T, dec=",") 


#-------------------------------------------------------------------------------------------------------------------------------------------


# we will modify some types of variables 
summary(tab)
tab$Vole.Spring <- as.numeric(tab$Vole.Spring)
tab$Vole.Autumn  <- as.numeric(tab$Vole.Autumn )
tab$Small.mustelid   <- as.numeric(tab$Small.mustelid  )
tab$Generalist.predator <- as.numeric(tab$Generalist.predator)
tab$Avian.predator   <- as.numeric(tab$Avian.predator  )

# we add longitude,latitude,region and a number for each site 

new_columns<-data.frame(matrix("center",891,4))
names(new_columns)<-c("longitude","latitude","zone","site_ordo")
new_columns

# we add the 4 columns
data<-cbind(tab,new_columns) 


# we recodee some of the names : encoding problems 
data$site[data$site == "pallasjSrvi"] <- "pallasjarvi"
data$site[data$site == "luumSki"] <- "luumaki"
data$site[data$site == "sodankylS"] <- "sodankyla"
data$site[data$site == "tohmajSrvi"] <- "tohmajarvi"
data$site[data$site == "ShtSri"] <- "ahtari"

# we can complete our new columns with data from google map for longitude and latitude 

data$latitude[data$site == "hauho"] <- 61.17193795784571
data$longitude[data$site == "hauho"] <- 24.563394493033037
data$site_ordo[data$site == "hauho"] <- 1

data$latitude[data$site == "heinola"] <-61.214453199709325
data$longitude[data$site == "heinola"] <- 26.037397307513178
data$site_ordo[data$site == "heinola"] <- 2

data$latitude[data$site == "ilmajoki"] <-62.73458686562898
data$longitude[data$site == "ilmajoki"] <-22.571161498822388
data$site_ordo[data$site == "ilmajoki"] <-3

data$latitude[data$site == "inari"] <-68.90626454694164
data$longitude[data$site == "inari"] <-27.029059540189902
data$site_ordo[data$site == "inari"] <-4

data$latitude[data$site == "kannus"] <-63.900363717827794
data$longitude[data$site == "kannus"] <-23.91672375111349
data$site_ordo[data$site == "kannus"] <-5

data$latitude[data$site == "karvia"] <-62.13303041685207
data$longitude[data$site == "karvia"] <-22.557948316998463
data$site_ordo[data$site == "karvia"] <-6

data$latitude[data$site == "kauhava"] <-63.101274042399794
data$longitude[data$site == "kauhava"] <-23.066095162688693
data$site_ordo[data$site == "kauhava"] <-7

data$latitude[data$site == "keuruu"] <- 62.255975595413716
data$longitude[data$site == "keuruu"] <-24.70813090058065
data$site_ordo[data$site == "keuruu"] <-8

data$latitude[data$site == "kevo"] <-69.75796858891722
data$longitude[data$site == "kevo"] <-27.017867829916767
data$site_ordo[data$site == "kevo"] <-9

data$latitude[data$site == "kolari"] <-67.33307354129319
data$longitude[data$site == "kolari"] <-23.78922926685231
data$site_ordo[data$site == "kolari"] <-10

data$latitude[data$site == "koli"] <-63.099708032798354 
data$longitude[data$site == "koli"] <-29.799828217696344
data$site_ordo[data$site == "koli"] <-11

data$latitude[data$site == "korpilahti"] <-62.02113340809769
data$longitude[data$site == "korpilahti"] <-25.57088035606078
data$site_ordo[data$site == "korpilahti"] <-12

data$latitude[data$site == "kuhmo"] <-64.12916547749539
data$longitude[data$site == "kuhmo"] <-29.520157048770965
data$site_ordo[data$site == "kuhmo"] <-13

data$latitude[data$site == "kuusamo"] <-65.96390928667391
data$longitude[data$site == "kuusamo"] <-29.18813824043734
data$site_ordo[data$site == "kuusamo"] <-14

data$latitude[data$site == "lapua"] <-62.971549988769816
data$longitude[data$site == "lapua"] <-23.001866567258098
data$site_ordo[data$site == "lapua"] <-15

data$latitude[data$site == "loppi"] <-60.71740508584855
data$longitude[data$site == "loppi"] <-24.4419450073272
data$site_ordo[data$site == "loppi"] <-16

data$latitude[data$site == "luumaki"] <-62.84002966915132
data$longitude[data$site == "luumaki"] <-28.912952390107723 
data$site_ordo[data$site == "luumaki"] <-17

data$latitude[data$site == "mikkeli"] <-61.68822602204782
data$longitude[data$site == "mikkeli"] <- 27.280515834064634
data$site_ordo[data$site == "mikkeli"] <- 18

data$latitude[data$site == "muhos"] <-64.80781659742189
data$longitude[data$site == "muhos"] <-25.99507459495535
data$site_ordo[data$site == "muhos"] <-19

data$latitude[data$site == "paimio"] <-60.456839159016766
data$longitude[data$site == "paimio"] <-22.687592015181714
data$site_ordo[data$site == "paimio"] <-20

data$latitude[data$site == "pallasjarvi"] <-68.02374980280263
data$longitude[data$site == "pallasjarvi"] <-24.216057473592983
data$site_ordo[data$site == "pallasjarvi"] <-21

data$latitude[data$site == "punkaharju"] <-61.755865889437295
data$longitude[data$site == "punkaharju"] <-29.39343524663757
data$site_ordo[data$site == "punkaharju"] <-22

data$latitude[data$site == "puolanka"] <-64.86725497125404
data$longitude[data$site == "puolanka"] <-27.67597545656743
data$site_ordo[data$site == "puolanka"] <-23

data$latitude[data$site == "ranua"] <-65.93231545042872
data$longitude[data$site == "ranua"] <-26.51879737018491
data$site_ordo[data$site == "ranua"] <-24

data$latitude[data$site == "rovaniemi"] <-66.50596024185901
data$longitude[data$site == "rovaniemi"] <-25.732290381304118
data$site_ordo[data$site == "rovaniemi"] <-25

data$latitude[data$site == "sodankyla"] <-67.41593233203037
data$longitude[data$site == "sodankyla"] <-26.589851899156837
data$site_ordo[data$site == "sodankyla"] <-26

data$latitude[data$site == "sotkamo"] <-64.13047524722737
data$longitude[data$site == "sotkamo"] <-28.38965115103982
data$site_ordo[data$site == "sotkamo"] <-27

data$latitude[data$site == "suonenjoki"] <-62.62590656797375
data$longitude[data$site == "suonenjoki"] <-27.122380689250058
data$site_ordo[data$site == "suonenjoki"] <-28

data$latitude[data$site == "tohmajarvi"] <-62.2232043951199
data$longitude[data$site == "tohmajarvi"] <-30.334194724668734
data$site_ordo[data$site == "tohmajarvi"] <-29

data$latitude[data$site == "vammala"] <-61.34328755967948
data$longitude[data$site == "vammala"] <-22.912847306989974
data$site_ordo[data$site == "vammala"] <-30

data$latitude[data$site == "viitasaari"] <-63.07323466048183
data$longitude[data$site == "viitasaari"] <-25.859865824469
data$site_ordo[data$site == "viitasaari"] <-31

data$latitude[data$site == "virolahti"] <-60.582795219758474
data$longitude[data$site == "virolahti"] <-27.70665301681641
data$site_ordo[data$site == "virolahti"] <-32

data$latitude[data$site == "ahtari"] <-62.55004929863743
data$longitude[data$site == "ahtari"] <-24.066963604038467
data$site_ordo[data$site == "ahtari"] <-33
  
  
# we complete with regions from criteria of the Korpela study 
data$zone[data$latitude > 66 ] <- "north"
data$zone[data$latitude < 64 & data$longitude<25] <- "west"
data$zone[data$longitude >29.2 & data$latitude <66 ] <- "east"

# we will modify some types of variables 
data$latitude <- as.numeric(data$latitude)
data$longitude <- as.numeric(data$longitude)
data$zone<- as.factor(data$zone)
data$site_ordo <- as.numeric(data$site_ordo)

# we can suppress some years that aren't in  the models 

data <- filter(data,year<2012,year>1988)

# checks

summary(data)

tab1(data$zone)

# we can register our clean data 

write.csv (data, "../data/data_Korpela.csv", row.names = T, quote = F)
