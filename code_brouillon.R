# Besognet Thomas, 17/04/23 , Stage : Brouillon 

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

#  Heatmap illustratives 

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

# Codes faux mais dont certains éléments pourraient être utiles 

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
