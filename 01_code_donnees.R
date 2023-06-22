
# author = "Besognet Thomas"
# date = " 23/05/23" 
# project = "Estimation d'int�ractions entre esp�ces � partir de s�ries temporelles"
 # name = "Data management"
  
# packages 
install.packages("dplyr")
library(dplyr)
install.packages("epiR")
library(epiR)
install.packages("epiDisplay")
library(epiDisplay)

# original data importation 
tableau <- read.table("../Donnees/Korpela et al PRSL 2014 data.txt", sep="\t", header=T, dec=",") 


#-------------------------------------------------------------------------------------------------------------------------------------------


# we will modify some types of variables 
summary(tableau)
tableau$Vole.Spring <- as.numeric(tableau$Vole.Spring)
tableau$Vole.Autumn  <- as.numeric(tableau$Vole.Autumn )
tableau$Small.mustelid   <- as.numeric(tableau$Small.mustelid  )
tableau$Generalist.predator <- as.numeric(tableau$Generalist.predator)
tableau$Avian.predator   <- as.numeric(tableau$Avian.predator  )

# we add longitude,latitude,region and a number for each site 

new_colonnes<-data.frame(matrix("centre",891,4))
names(new_colonnes)<-c("longitude","latitude","cardinalite","site_ordo")
new_colonnes

# we add the 4 columns
donnees<-cbind(tableau,new_colonnes) 


# we recodee some of the names : encoding problems 
donnees$site[donnees$site == "pallasjSrvi"] <- "pallasjarvi"
donnees$site[donnees$site == "luumSki"] <- "luumaki"
donnees$site[donnees$site == "sodankylS"] <- "sodankyla"
donnees$site[donnees$site == "tohmajSrvi"] <- "tohmajarvi"
donnees$site[donnees$site == "ShtSri"] <- "ahtari"

# we can complete our new columns with data from google map for longitude and latitude 

donnees$latitude[donnees$site == "hauho"] <- 61.17193795784571
donnees$longitude[donnees$site == "hauho"] <- 24.563394493033037
donnees$site_ordo[donnees$site == "hauho"] <- 1

donnees$latitude[donnees$site == "heinola"] <-61.214453199709325
donnees$longitude[donnees$site == "heinola"] <- 26.037397307513178
donnees$site_ordo[donnees$site == "heinola"] <- 2

donnees$latitude[donnees$site == "ilmajoki"] <-62.73458686562898
donnees$longitude[donnees$site == "ilmajoki"] <-22.571161498822388
donnees$site_ordo[donnees$site == "ilmajoki"] <-3

donnees$latitude[donnees$site == "inari"] <-68.90626454694164
donnees$longitude[donnees$site == "inari"] <-27.029059540189902
donnees$site_ordo[donnees$site == "inari"] <-4

donnees$latitude[donnees$site == "kannus"] <-63.900363717827794
donnees$longitude[donnees$site == "kannus"] <-23.91672375111349
donnees$site_ordo[donnees$site == "kannus"] <-5

donnees$latitude[donnees$site == "karvia"] <-62.13303041685207
donnees$longitude[donnees$site == "karvia"] <-22.557948316998463
donnees$site_ordo[donnees$site == "karvia"] <-6

donnees$latitude[donnees$site == "kauhava"] <-63.101274042399794
donnees$longitude[donnees$site == "kauhava"] <-23.066095162688693
donnees$site_ordo[donnees$site == "kauhava"] <-7

donnees$latitude[donnees$site == "keuruu"] <- 62.255975595413716
donnees$longitude[donnees$site == "keuruu"] <-24.70813090058065
donnees$site_ordo[donnees$site == "keuruu"] <-8

donnees$latitude[donnees$site == "kevo"] <-69.75796858891722
donnees$longitude[donnees$site == "kevo"] <-27.017867829916767
donnees$site_ordo[donnees$site == "kevo"] <-9

donnees$latitude[donnees$site == "kolari"] <-67.33307354129319
donnees$longitude[donnees$site == "kolari"] <-23.78922926685231
donnees$site_ordo[donnees$site == "kolari"] <-10

donnees$latitude[donnees$site == "koli"] <-63.099708032798354 
donnees$longitude[donnees$site == "koli"] <-29.799828217696344
donnees$site_ordo[donnees$site == "koli"] <-11

donnees$latitude[donnees$site == "korpilahti"] <-62.02113340809769
donnees$longitude[donnees$site == "korpilahti"] <-25.57088035606078
donnees$site_ordo[donnees$site == "korpilahti"] <-12

donnees$latitude[donnees$site == "kuhmo"] <-64.12916547749539
donnees$longitude[donnees$site == "kuhmo"] <-29.520157048770965
donnees$site_ordo[donnees$site == "kuhmo"] <-13

donnees$latitude[donnees$site == "kuusamo"] <-65.96390928667391
donnees$longitude[donnees$site == "kuusamo"] <-29.18813824043734
donnees$site_ordo[donnees$site == "kuusamo"] <-14

donnees$latitude[donnees$site == "lapua"] <-62.971549988769816
donnees$longitude[donnees$site == "lapua"] <-23.001866567258098
donnees$site_ordo[donnees$site == "lapua"] <-15

donnees$latitude[donnees$site == "loppi"] <-60.71740508584855
donnees$longitude[donnees$site == "loppi"] <-24.4419450073272
donnees$site_ordo[donnees$site == "loppi"] <-16

donnees$latitude[donnees$site == "luumaki"] <-62.84002966915132
donnees$longitude[donnees$site == "luumaki"] <-28.912952390107723 
donnees$site_ordo[donnees$site == "luumaki"] <-17

donnees$latitude[donnees$site == "mikkeli"] <-61.68822602204782
donnees$longitude[donnees$site == "mikkeli"] <- 27.280515834064634
donnees$site_ordo[donnees$site == "mikkeli"] <- 18

donnees$latitude[donnees$site == "muhos"] <-64.80781659742189
donnees$longitude[donnees$site == "muhos"] <-25.99507459495535
donnees$site_ordo[donnees$site == "muhos"] <-19

donnees$latitude[donnees$site == "paimio"] <-60.456839159016766
donnees$longitude[donnees$site == "paimio"] <-22.687592015181714
donnees$site_ordo[donnees$site == "paimio"] <-20

donnees$latitude[donnees$site == "pallasjarvi"] <-68.02374980280263
donnees$longitude[donnees$site == "pallasjarvi"] <-24.216057473592983
donnees$site_ordo[donnees$site == "pallasjarvi"] <-21

donnees$latitude[donnees$site == "punkaharju"] <-61.755865889437295
donnees$longitude[donnees$site == "punkaharju"] <-29.39343524663757
donnees$site_ordo[donnees$site == "punkaharju"] <-22

donnees$latitude[donnees$site == "puolanka"] <-64.86725497125404
donnees$longitude[donnees$site == "puolanka"] <-27.67597545656743
donnees$site_ordo[donnees$site == "puolanka"] <-23

donnees$latitude[donnees$site == "ranua"] <-65.93231545042872
donnees$longitude[donnees$site == "ranua"] <-26.51879737018491
donnees$site_ordo[donnees$site == "ranua"] <-24

donnees$latitude[donnees$site == "rovaniemi"] <-66.50596024185901
donnees$longitude[donnees$site == "rovaniemi"] <-25.732290381304118
donnees$site_ordo[donnees$site == "rovaniemi"] <-25

donnees$latitude[donnees$site == "sodankyla"] <-67.41593233203037
donnees$longitude[donnees$site == "sodankyla"] <-26.589851899156837
donnees$site_ordo[donnees$site == "sodankyla"] <-26

donnees$latitude[donnees$site == "sotkamo"] <-64.13047524722737
donnees$longitude[donnees$site == "sotkamo"] <-28.38965115103982
donnees$site_ordo[donnees$site == "sotkamo"] <-27

donnees$latitude[donnees$site == "suonenjoki"] <-62.62590656797375
donnees$longitude[donnees$site == "suonenjoki"] <-27.122380689250058
donnees$site_ordo[donnees$site == "suonenjoki"] <-28

donnees$latitude[donnees$site == "tohmajarvi"] <-62.2232043951199
donnees$longitude[donnees$site == "tohmajarvi"] <-30.334194724668734
donnees$site_ordo[donnees$site == "tohmajarvi"] <-29

donnees$latitude[donnees$site == "vammala"] <-61.34328755967948
donnees$longitude[donnees$site == "vammala"] <-22.912847306989974
donnees$site_ordo[donnees$site == "vammala"] <-30

donnees$latitude[donnees$site == "viitasaari"] <-63.07323466048183
donnees$longitude[donnees$site == "viitasaari"] <-25.859865824469
donnees$site_ordo[donnees$site == "viitasaari"] <-31

donnees$latitude[donnees$site == "virolahti"] <-60.582795219758474
donnees$longitude[donnees$site == "virolahti"] <-27.70665301681641
donnees$site_ordo[donnees$site == "virolahti"] <-32

donnees$latitude[donnees$site == "ahtari"] <-62.55004929863743
donnees$longitude[donnees$site == "ahtari"] <-24.066963604038467
donnees$site_ordo[donnees$site == "ahtari"] <-33*
  
  
# we complet with regions from criteria of the Korpela study 
donnees$cardinalite[donnees$latitude >66 ] <-"nord"
donnees$cardinalite[donnees$latitude < 64 & donnees$longitude<25] <- "sud_ouest"
donnees$cardinalite[donnees$longitude >29.2 & donnees$latitude <66 ] <- "est"

# we will modify some types of variables 
donnees$latitude <- as.numeric(donnees$latitude)
donnees$longitude <- as.numeric(donnees$longitude)
donnees$cardinalite<- as.factor(tableau$cardinalite)
donnees$site_ordo <- as.numeric(donnees$site_ordo)

# we can supress some years that aren't in  the models 

donnees <- filter(donnees,year<2012,year>1988)

# checks

summary(donnees)

tab1(donnees$cardinalite)

# we can register our clean data 

write.csv (donnees, "donnees_completees.csv", row.names = T, quote = F)

# we will recreate the map 