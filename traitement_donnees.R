# Besognet Thomas, 03/04/23 , Stage : Traitement des données 

# packages 
install.packages("ggplot2")                                       
library(ggplot2)
install.packages("dplyr")
library(dplyr)
install.packages("epiR")
library(epiR)
install.packages("epiDisplay")
library(epiDisplay)

# importation des données 
setwd("C:\\Users\\thoma\\OneDrive\\Documents\\ETUDES\\SP S2\\STG102_Stage\\Statistiques")
tableau <- read.table("donnees.txt", sep="\t", header=T, dec=",")

# modificationdes données 
summary(tableau)
tableau$Vole.Spring <- as.numeric(tableau$Vole.Spring)
tableau$Vole.Autumn  <- as.numeric(tableau$Vole.Autumn )
tableau$Small.mustelid   <- as.numeric(tableau$Small.mustelid  )
tableau$Generalist.predator <- as.numeric(tableau$Generalist.predator)
tableau$Avian.predator   <- as.numeric(tableau$Avian.predator  )

# rajouter : longitudes, latitudes et cardinalité 

new_colonnes<-data.frame(matrix("centre",891,3))
names(new_colonnes)<-c("longitude","latitude","cardinalite")
new_colonnes

donnees<-cbind(tableau,new_colonnes)


# recodage : attention 
donnees$site[donnees$site == "pallasjSrvi"] <- "pallasjarvi"
donnees$site[donnees$site == "luumSki"] <- "luumaki"
donnees$site[donnees$site == "sodankylS"] <- "sodankyla"
donnees$site[donnees$site == "tohmajSrvi"] <- "tohmajarvi"
donnees$site[donnees$site == "ShtSri"] <- "ahtari"


  donnees$latitude[donnees$site == "hauho"] <- 61.17193795784571
  donnees$longitude[donnees$site == "hauho"] <- 24.563394493033037

  donnees$latitude[donnees$site == "heinola"] <-61.214453199709325
  donnees$longitude[donnees$site == "heinola"] <- 26.037397307513178
  
  donnees$latitude[donnees$site == "ilmajoki"] <-62.73458686562898
  donnees$longitude[donnees$site == "ilmajoki"] <-22.571161498822388
  
  donnees$latitude[donnees$site == "inari"] <-68.90626454694164
  donnees$longitude[donnees$site == "inari"] <-27.029059540189902

  donnees$latitude[donnees$site == "kannus"] <-63.900363717827794
  donnees$longitude[donnees$site == "kannus"] <-23.91672375111349
  
  donnees$latitude[donnees$site == "karvia"] <-62.13303041685207
  donnees$longitude[donnees$site == "karvia"] <-22.557948316998463
  
  donnees$latitude[donnees$site == "kauhava"] <-63.101274042399794
  donnees$longitude[donnees$site == "kauhava"] <-23.066095162688693
  
  donnees$latitude[donnees$site == "keuruu"] <- 62.255975595413716
  donnees$longitude[donnees$site == "keuruu"] <-24.70813090058065
  
  donnees$latitude[donnees$site == "kevo"] <-69.75796858891722
  donnees$longitude[donnees$site == "kevo"] <-27.017867829916767
  
  donnees$latitude[donnees$site == "kolari"] <-67.33307354129319
  donnees$longitude[donnees$site == "kolari"] <-23.78922926685231
  
  donnees$latitude[donnees$site == "koli"] <-63.099708032798354 
  donnees$longitude[donnees$site == "koli"] <-29.799828217696344
  
  donnees$latitude[donnees$site == "korpilahti"] <-62.02113340809769
  donnees$longitude[donnees$site == "korpilahti"] <-25.57088035606078
  
  donnees$latitude[donnees$site == "kuhmo"] <-64.12916547749539
  donnees$longitude[donnees$site == "kuhmo"] <-29.520157048770965
  
  donnees$latitude[donnees$site == "kuusamo"] <-65.96390928667391
  donnees$longitude[donnees$site == "kuusamo"] <-29.18813824043734
  
  donnees$latitude[donnees$site == "lapua"] <-62.971549988769816
  donnees$longitude[donnees$site == "lapua"] <-23.001866567258098
  
  donnees$latitude[donnees$site == "loppi"] <-60.71740508584855
  donnees$longitude[donnees$site == "loppi"] <-24.4419450073272
  
  donnees$latitude[donnees$site == "luumaki"] <-62.84002966915132
  donnees$longitude[donnees$site == "luumaki"] <-28.912952390107723 
  
  donnees$latitude[donnees$site == "mikkeli"] <-61.68822602204782
  donnees$longitude[donnees$site == "mikkeli"] <- 27.280515834064634
  
  donnees$latitude[donnees$site == "muhos"] <-64.80781659742189
  donnees$longitude[donnees$site == "muhos"] <-25.99507459495535
  
  donnees$latitude[donnees$site == "paimio"] <-60.456839159016766
  donnees$longitude[donnees$site == "paimio"] <-22.687592015181714
  
  donnees$latitude[donnees$site == "pallasjarvi"] <-68.02374980280263
  donnees$longitude[donnees$site == "pallasjarvi"] <-24.216057473592983
  
  donnees$latitude[donnees$site == "punkaharju"] <-61.755865889437295
  donnees$longitude[donnees$site == "punkaharju"] <-29.39343524663757
  
  donnees$latitude[donnees$site == "puolanka"] <-64.86725497125404
  donnees$longitude[donnees$site == "puolanka"] <-27.67597545656743
  
  donnees$latitude[donnees$site == "ranua"] <-65.93231545042872
  donnees$longitude[donnees$site == "ranua"] <-26.51879737018491
  
  donnees$latitude[donnees$site == "rovaniemi"] <-66.50596024185901
  donnees$longitude[donnees$site == "rovaniemi"] <-25.732290381304118
  
  donnees$latitude[donnees$site == "sodankyla"] <-67.41593233203037
  donnees$longitude[donnees$site == "sodankyla"] <-26.589851899156837
  
  donnees$latitude[donnees$site == "sotkamo"] <-64.13047524722737
  donnees$longitude[donnees$site == "sotkamo"] <-28.38965115103982
  
  donnees$latitude[donnees$site == "suonenjoki"] <-62.62590656797375
  donnees$longitude[donnees$site == "suonenjoki"] <-27.122380689250058
  
  donnees$latitude[donnees$site == "tohmajarvi"] <-62.2232043951199
  donnees$longitude[donnees$site == "tohmajarvi"] <-30.334194724668734
  
  donnees$latitude[donnees$site == "vammala"] <-61.34328755967948
  donnees$longitude[donnees$site == "vammala"] <-22.912847306989974
  
  donnees$latitude[donnees$site == "viitasaari"] <-63.07323466048183
  donnees$longitude[donnees$site == "viitasaari"] <-25.859865824469
  
  donnees$latitude[donnees$site == "virolahti"] <-60.582795219758474
  donnees$longitude[donnees$site == "virolahti"] <-27.70665301681641
  
  donnees$latitude[donnees$site == "ahtari"] <-62.55004929863743
  donnees$longitude[donnees$site == "ahtari"] <-24.066963604038467
    
  donnees$cardinalite[donnees$latitude >66 ] <-"nord"
  donnees$cardinalite[donnees$latitude < 64 & donnees$longitude<25] <- "sud_ouest"
  donnees$cardinalite[donnees$longitude >29.2 & donnees$latitude <66 ] <- "est"
  
  donnees$latitude <- as.numeric(donnees$latitude)
  donnees$longitude <- as.numeric(donnees$longitude)
  tableau$cardinalite<- as.factor(tableau$cardinalite)
  
  summary(donnees)
  
  tab1(donnees$cardinalite)
  
  # tentative de heatmap 
  
  donnees2 <- donnees %>% filter(!is.na(Vole.Spring))
  
  donnees2$groupes<-cut(donnees2$Vole.Spring,
                     breaks = c(-4,-2,-1,0,1,2,4))
  
  graph2 <- ggplot(donnees2, aes(x=year, y=site,fill=groupes)) +
    geom_tile()+
    scale_fill_manual(breaks = levels(donnees2$groupes),
                      values = c("#660000","#FF0000","#FF6600","#FFFF00","#00FF00","#006600"))
  graph2
  
  donnees3 <- donnees %>% filter(!is.na(Vole.Autumn))
  
  donnees3$groupes<-cut(donnees3$Vole.Autumn,
                        breaks = c(-4,-2,-1,0,1,2,4))
  
  graph3 <- ggplot(donnees3, aes(x=year, y=site,fill=groupes)) +
    geom_tile()+
    scale_fill_manual(breaks = levels(donnees3$groupes),
                      values = c("#660000","#FF0000","#FF6600","#FFFF00","#00FF00","#006600"))
  graph3
  
# Création du nouveau fichier de données 
  
# creation de "data" qui va stocker les nouvelles valeurs calculées pour chaque cardinalité 
  
camp_1 <- c(1:23) 
camp_2 <- c(1:23)
pred_1 <- c(1:23)
pred_2 <- c(1:23)
pred_3 <- c(1:23)
  
data <- data.frame(c1=camp_1,c2=camp_2,p1=pred_1,p2=pred_2,p3=pred_3)

# le nord 
  
donnees_nord <- filter (donnees,cardinalite=="nord",year<2012,year>1988)
for (i in 1:23){
  donnees_nord_annees <- filter(donnees_nord,year==1988+i)
  data[i,1]<- mean(donnees_nord_annees$Vole.Spring,na.rm=T)
  data[i,2]<- mean(donnees_nord_annees$Vole.Autumn,na.rm=T)
  data[i,3]<- mean(donnees_nord_annees$Small.mustelid,na.rm=T)
  data[i,4]<- mean(donnees_nord_annees$Generalist.predator,na.rm=T)
  data[i,5]<- mean(donnees_nord_annees$Avian.predator,na.rm=T)
  }
write.csv (data, "nord.csv", row.names = T, quote = F)

# l'est

donnees_est <- filter (donnees,cardinalite=="est",year<2012,year>1988)
for (i in 1:23){
  donnees_est_annees <- filter(donnees_nord,year==1988+i)
  data[i,1]<- mean(donnees_est_annees$Vole.Spring,na.rm=T)
  data[i,2]<- mean(donnees_est_annees$Vole.Autumn,na.rm=T)
  data[i,3]<- mean(donnees_est_annees$Small.mustelid,na.rm=T)
  data[i,4]<- mean(donnees_est_annees$Generalist.predator,na.rm=T)
  data[i,5]<- mean(donnees_est_annees$Avian.predator,na.rm=T)
}
write.csv (data, "est.csv", row.names = T, quote = F)

# l'ouest

donnees_ouest <- filter (donnees,cardinalite=="sud_ouest",year<2012,year>1988)
for (i in 1:23){
  donnees_ouest_annees <- filter(donnees_ouest,year==1988+i)
  data[i,1]<- mean(donnees_ouest_annees$Vole.Spring,na.rm=T)
  data[i,2]<- mean(donnees_ouest_annees$Vole.Autumn,na.rm=T)
  data[i,3]<- mean(donnees_ouest_annees$Small.mustelid,na.rm=T)
  data[i,4]<- mean(donnees_ouest_annees$Generalist.predator,na.rm=T)
  data[i,5]<- mean(donnees_ouest_annees$Avian.predator,na.rm=T)
}
write.csv (data, "ouest.csv", row.names = T, quote = F)

# le sud  

donnees_sud <- filter (donnees,cardinalite=="centre",year<2012,year>1988)
for (i in 1:23){
  donnees_sud_annees <- filter(donnees_sud,year==1988+i)
  data[i,1]<- mean(donnees_sud_annees$Vole.Spring,na.rm=T)
  data[i,2]<- mean(donnees_sud_annees$Vole.Autumn,na.rm=T)
  data[i,3]<- mean(donnees_sud_annees$Small.mustelid,na.rm=T)
  data[i,4]<- mean(donnees_sud_annees$Generalist.predator,na.rm=T)
  data[i,5]<- mean(donnees_sud_annees$Avian.predator,na.rm=T)
}
write.csv (data, "sud.csv", row.names = T, quote = F)
  
  
