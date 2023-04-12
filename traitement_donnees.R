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
install.packages("forcats")
library(forcats)

# I Traitement des données 

# importation des données 
setwd("C:\\Users\\thoma\\OneDrive\\Documents\\ETUDES\\SP S2\\STG102_Stage\\Statistiques") #working directory
tableau <- read.table("donnees.txt", sep="\t", header=T, dec=",") # ouverture des données de base 

# modificationdes données : les types de variable 
summary(tableau)
tableau$Vole.Spring <- as.numeric(tableau$Vole.Spring)
tableau$Vole.Autumn  <- as.numeric(tableau$Vole.Autumn )
tableau$Small.mustelid   <- as.numeric(tableau$Small.mustelid  )
tableau$Generalist.predator <- as.numeric(tableau$Generalist.predator)
tableau$Avian.predator   <- as.numeric(tableau$Avian.predator  )

# rajouter : longitudes, latitudes, cardinalité et une variable qualitiative ordinale pour les sites 

new_colonnes<-data.frame(matrix("centre",891,4,4))
names(new_colonnes)<-c("longitude","latitude","cardinalite","site_ordo")
new_colonnes

donnees<-cbind(tableau,new_colonnes) # on rajoute les 4 colonnes 


# recodage des noms des sites qui se sont mal importés 
donnees$site[donnees$site == "pallasjSrvi"] <- "pallasjarvi"
donnees$site[donnees$site == "luumSki"] <- "luumaki"
donnees$site[donnees$site == "sodankylS"] <- "sodankyla"
donnees$site[donnees$site == "tohmajSrvi"] <- "tohmajarvi"
donnees$site[donnees$site == "ShtSri"] <- "ahtari"

# remplissage des 4 colonnes 

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
  donnees$site_ordo[donnees$site == "ahtari"] <-33
    
  donnees$cardinalite[donnees$latitude >66 ] <-"nord"
  donnees$cardinalite[donnees$latitude < 64 & donnees$longitude<25] <- "sud_ouest"
  donnees$cardinalite[donnees$longitude >29.2 & donnees$latitude <66 ] <- "est"
  
  donnees$latitude <- as.numeric(donnees$latitude)
  donnees$longitude <- as.numeric(donnees$longitude)
  donnees$cardinalite<- as.factor(tableau$cardinalite)
  donnees$site_ordo <- as.numeric(donnees$site_ordo)
  
# on retire les années qui ne sont pas étudiées par la suite 
  
  donnees <- filter(donnees,year<2012,year>1988)
  
# on vérifie que tout est bon 
  
  summary(donnees)
  
  tab1(donnees$cardinalite)
  
# on enregistre dans un nouveau fichier de donnéess pour ne pas avor besoin de tout réexecuter si besoin 
  
write.csv (donnees, "donnees_completees.csv", row.names = T, quote = F)
  
#  Facultatif : heatmap illustratives 
  
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
  
# II Influence of predators on vole population growth rates 
  
# Ouverture des données avec directement toutes les modifications précédentes 
  
setwd("C:\\Users\\thoma\\OneDrive\\Documents\\ETUDES\\SP S2\\STG102_Stage\\Statistiques") #working directory 
donnees <- read.csv("donnees_completees.csv", sep=",", header=T, dec=".")

# selection aléatoire de l'échantillon 

# rajouter de quoi faire l'opération 50 fois ici !!!

idx <- sample(seq(1,2), size = nrow(donnees), replace = TRUE, prob = c(0.5,0.5))
training_set <- donnees[idx == 1,]
test_set <- donnees[idx == 2,]
  
# creation de "data" qui va stocker les nouvelles valeurs calculées pour chaque cardinalité à partir du training set
  
camp_1 <- c(1:23) 
camp_2 <- c(1:23)
pred_1 <- c(1:23)
pred_2 <- c(1:23)
pred_3 <- c(1:23)
  
data <- data.frame(c1=camp_1,c2=camp_2,p1=pred_1,p2=pred_2,p3=pred_3)

# le nord 
  
donnees_nord <- filter (training_set,cardinalite=="nord")
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

donnees_est <- filter (training_set,cardinalite=="est")
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

donnees_ouest <- filter (training_set,cardinalite=="sud_ouest")
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

donnees_sud <- filter (training_set,cardinalite=="centre")
for (i in 1:23){
  donnees_sud_annees <- filter(donnees_sud,year==1988+i)
  data[i,1]<- mean(donnees_sud_annees$Vole.Spring,na.rm=T)
  data[i,2]<- mean(donnees_sud_annees$Vole.Autumn,na.rm=T)
  data[i,3]<- mean(donnees_sud_annees$Small.mustelid,na.rm=T)
  data[i,4]<- mean(donnees_sud_annees$Generalist.predator,na.rm=T)
  data[i,5]<- mean(donnees_sud_annees$Avian.predator,na.rm=T)
}
write.csv (data, "sud.csv", row.names = T, quote = F)

# copie collage à la main des valeurs dans le fichier donnees_2 , automatiser ? 

# importation des nouvelles données et transformation 

donnees_2 <- read.csv("donnees_2.csv", sep=";", header=T, dec=".")

for(i in which(is.na(donnees_2$vole_spring))){
  if(!is.na(donnees_2$vole_spring[i-1]) & !is.na(donnees_2$vole_spring[i+1])){
    donnees_2$vole_spring[i] <- (donnees_2$vole_spring[i-1]+donnees_2$vole_spring[i+1])/2
  }
  else{
    print(c("erreur en ", i))
  }
}

for(i in which(is.na(donnees_2$vole_autumn))){
  if(!is.na(donnees_2$vole_autumn[i-1]) & !is.na(donnees_2$vole_autumn[i+1])){
    donnees_2$vole_autumn[i] <- (donnees_2$vole_autumn[i-1]+donnees_2$vole_autumn[i+1])/2
  }
  else{
    print(c("erreur en ", i))
  }
}

for(i in which(is.na(donnees_2$small_mustelid))){
  if(!is.na(donnees_2$small_mustelid[i-1]) & !is.na(donnees_2$small_mustelid[i+1])){
    donnees_2$small_mustelid[i] <- (donnees_2$small_mustelid[i-1]+donnees_2$small_mustelid[i+1])/2
  }
  else{
    print(c("erreur en ", i))
  }
}

for(i in which(is.na(donnees_2$generalist_predator))){
  if(!is.na(donnees_2$generalist_predator[i-1]) & !is.na(donnees_2$generalist_predator[i+1])){
    donnees_2$generalist_predator[i] <- (donnees_2$generalist_predator[i-1]+donnees_2$generalist_predator[i+1])/2
  }
  else{
    print(c("erreur en ", i))
  }
}

for(i in which(is.na(donnees_2$avian_predator))){
  if(!is.na(donnees_2$avian_predator[i-1]) & !is.na(donnees_2$avian_predator[i+1])){
    donnees_2$avian_predator[i] <- (donnees_2$avian_predator[i-1]+donnees_2$avian_predator[i+1])/2
  }
  else{
    print(c("erreur en ", i))
  }
}


write.csv (donnees_2, "donnees2_completees.csv", row.names = T, quote = F)

# ouverture des données propres 

donnees_2 <- read.csv("donnees2_completees.csv", sep=",", header=T, dec=".")

# Les estimations de modèles 20 modéles à tester (5 espéces X 4 cardinalités) 

donnees_2_north <- filter (donnees_2,area=="north ") # attention à l'espace...

model_voleS_north <- lm(vole_spring[i] ~ vole_autumn[i-1]+vole_autumn[i-2]+small_mustelid[i]+generalist_predator[i]+avian_predator[i-1], data=donnees_2_north)

#  Calculs de variance et varaince expliquées par chaque espéce pour la prédiction de chaque espèce 

variance_voleS_north <- 
# Pour chacun des 20 modèles, il faut donc faire un graphique avec les variances expliquées 

# Simulations pour le futur et dépendances de densité ( fit avec le test set)

  
  
