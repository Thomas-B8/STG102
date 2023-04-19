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

# Tentative numéro 2 estimation des paramètres  ( avec un seul estimateur par localisation )

# on va créer nos series chronologiques à plusieurs variables ( 1 variable par localisation=colonne, temps en ligne)

At <- data.frame(matrix("NA",23,33))
St <- data.frame(matrix("NA",23,33))
P1t <- data.frame(matrix("NA",23,33))
P2t <- data.frame(matrix("NA",23,33))
P3t <- data.frame(matrix("NA",23,33))

# on les remplit des valeurs de nos données 

for (i in 1:33){ # on parcourt les localisations 
  for(j in 1:23){ # on parcourt les années 
    At[j,i] <- donnees[i+j,4]
    St[j,i] <- donnees[i+j,5]
    P1t[j,i] <- donnees[i+j,6]
    P2t[j,i] <- donnees[i+j,7]
    P3t[j,i] <- donnees[i+j,8]
    
  }
}

# on crée le décalage temporel 

At_ <- At[-c(1,2),] #present 
St_ <- St[-c(1,2),] #present 
P1t_ <- P1t[-c(1,2),] #present 
P2t_ <- P2t[-c(1,2),] #present 
P3t_ <- P3t[-c(1,2),] #present 
At_1 <- At[-c(1,23),] #passé1
St_1 <- St[-c(1,23),] #passé1
P3t_1 <- P3t[-c(1,23),] #passé1
At_2 <- At[-c(22,23),] #passé2

# on estime les modèles avec les matrices 

modele1 <- lm(formula = St_ ~ At_1 + At_2 + P1t_ + P2t_ + P3t_1 ,offset = At_1 ,na.action=na.omit)
modele2 <- lm(formula = At_ ~ St_ + St_1 + P1t_ + P2t_ + P3t_ ,offset = St_,na.action=na.omit)

# ne fonctionne pas en multidimensionnel ... 

# Tentative avce les 50 training set 

estimateurs_tableau_training_set<-data.frame(matrix("NA",33,13))
names(estimateurs_tableau_training_set)<-c("site","a1","b1","c1","d1","f1","e1","a2","b2","c2","d2","e2","f2")

# Tentative numéro 1 ( avec autant d'estimations des paramètres que de lieux)

for (i in vect){
  donnees_site <-  filter(donnees,site_ordo==i) # à chaque passage dans la boucle, on ne garde qu'une localisation 
  a1_moy=0;a2_moy=0;b1_moy=0;b2_moy=0;c1_moy=0;c2_moy=0;d1_moy=0;d2_moy=0;e1_moy=0;e2_moy=0;f1_moy=0;f2_moy=0
  # creation du training set et du test set 
  for (k in 1:50){
    idx <- sample(seq(1,2), size = nrow(donnees_site), replace = TRUE, prob = c(0.5,0.5))
    training_set <- donnees_site[idx == 1,]
    test_set <- donnees_site[idx == 2,]
    # creation des vecteurs 
    Present <-filter(training_set,year>1990)# on ne prend pas les 2 premières années 
    At    <-Present$Vole.Autumn
    St    <-Present$Vole.Spring
    P1t   <-Present$Small.mustelid 
    P2t   <-Present$Generalist.predator
    P3t   <-Present$Avian.predator
    Passe_1 <-filter(training_set,year>1989,year<2011) # on ne prend ni la première ni la dernière année 
    At_1    <-Passe_1$Vole.Autumn
    St_1    <-Passe_1$Vole.Spring
    P3t_1   <-Passe_1$Avian.predator
    Passe_2 <-filter(training_set,year<2010) # on ne prend pas les 2 dernières années  
    At_2    <-Passe_2$Vole.Autumn
    # on passe aux modèles  
    modele1 <- lm(formula = St ~ At_1 + At_2 + P1t + P2t + P3t_1 , data = donnees_site,offset = At_1 )
    modele2 <- lm(formula = At ~ St + St_1 + P1t + P2t + P3t , data = donnees_site,offset = St   )
    # moyennage des estimateurs 
    a1_moy <- a1_moy + modele1$coefficients[1]
    b1_moy <- b1_moy + modele1$coefficients[2]
    c1_moy <- c1_moy + modele1$coefficients[3]
    d1_moy <- d1_moy + modele1$coefficients[4]
    e1_moy <- e1_moy + modele1$coefficients[5]
    f1_moy <- f1_moy + modele1$coefficients[6]
    a2_moy <- a2_moy + modele2$coefficients[1]
    b2_moy <- b2_moy + modele2$coefficients[2]
    c2_moy <- c2_moy + modele2$coefficients[3]
    d2_moy <- d2_moy + modele2$coefficients[4]
    e2_moy <- e2_moy + modele2$coefficients[5]
    f2_moy <- f2_moy + modele2$coefficients[6] } # on arrête ici la boucle "k", car après on va compléter le tableau avec les moyennes 
  # remplissage du tableau 
  estimateurs_tableau_training_set[i,1] <- i
  estimateurs_tableau_training_set[i,2] <- a1_moy/50
  estimateurs_tableau_training_set[i,3] <- b1_moy/50
  estimateurs_tableau_training_set[i,4] <- c1_moy/50
  estimateurs_tableau_training_set[i,5] <- d1_moy/50
  estimateurs_tableau_training_set[i,6] <- e1_moy/50
  estimateurs_tableau_training_set[i,7] <- f1_moy/50
  estimateurs_tableau_training_set[i,8] <- a2_moy/50
  estimateurs_tableau_training_set[i,9] <- b2_moy/50
  estimateurs_tableau_training_set[i,10] <- c2_moy/50
  estimateurs_tableau_training_set[i,11] <- d2_oy/50
  estimateurs_tableau_training_set[i,12] <- e2_moy/50
  estimateurs_tableau_training_set[i,13] <- f2_mpy/50 }

estimateurs_tableau_training_set$a1 <- as.numeric(estimateurs_tableau_training_set$a1)
estimateurs_tableau_training_set$b1 <- as.numeric(estimateurs_tableau_training_set$b1)
estimateurs_tableau_training_set$c1 <- as.numeric(estimateurs_tableau_training_set$c1)
estimateurs_tableau_training_set$d1 <- as.numeric(estimateurs_tableau_training_set$d1)
estimateurs_tableau_training_set$e1 <- as.numeric(estimateurs_tableau_training_set$e1)
estimateurs_tableau_training_set$f1 <- as.numeric(estimateurs_tableau_training_set$f1)
estimateurs_tableau_training_set$a2 <- as.numeric(estimateurs_tableau_training_set$a2)
estimateurs_tableau_training_set$b2 <- as.numeric(estimateurs_tableau_training_set$b2)
estimateurs_tableau_training_set$c2 <- as.numeric(estimateurs_tableau_training_set$c2)
estimateurs_tableau_training_set$d2 <- as.numeric(estimateurs_tableau_training_set$d2)
estimateurs_tableau_training_set$e2 <- as.numeric(estimateurs_tableau_training_set$e2)
estimateurs_tableau_training_set$f2 <- as.numeric(estimateurs_tableau_training_set$f2)



# Tentative numéro 2 ( avec un seul estimateur par localisation )

# ...


# mauvais calcul : variance entre les années à localisation fixée ( au lieu de l'inverse )

# creation d'un tableau qui va contenir les variances des 2 taux d'évolution  expliquées et les varaincess expliquées par les paramètres  

tableau_var <- data.frame(matrix("NA",33,13))
names(tableau_var)<-c("site","VarY1","dir1","del1","P11","P21","P31","VarY2","dir2","del2","P12","P22","P32")

vect21 <- c(1:3,5:17,19,20,21,23,24,25,27,28,29,32) # les localisations sans estimateurs = "NA" dans pour le modèle 1
vect22 <- c(1:3,5:13,15,16,17,19:25,27,28,29,32)   # les localisations sans estimateurs = "NA" dans pour le modèle 2

# on reprend nos sélections de vecteurs, mais cette fois ci sur l'ensemble du jeu de données et non pas par localisations 
Present <-filter(donnees,year>1990)# on ne prend pas les 2 premières années 
At    <-Present$Vole.Autumn
St    <-Present$Vole.Spring
P1t   <-Present$Small.mustelid 
P2t   <-Present$Generalist.predator
P3t   <-Present$Avian.predator
Passe_1 <-filter(donnees,year>1989,year<2011) # on ne prend ni la première ni la dernière année 
At_1    <-Passe_1$Vole.Autumn
St_1    <-Passe_1$Vole.Spring
P3t_1   <-Passe_1$Avian.predator
Passe_2 <-filter(donnees,year<2010) # on ne prend pas les 2 dernières années  
At_2    <-Passe_2$Vole.Autumn


for (i in vect21){
  b <- estimateurs_tableau_full_set[i,3]
  c <- estimateurs_tableau_full_set[i,4]
  d <- estimateurs_tableau_full_set[i,5]
  e <- estimateurs_tableau_full_set[i,6]
  f <- estimateurs_tableau_full_set[i,7]
  tableau_var[i,1] <- i
  DD1 <- b*(b*var(At_1,na.rm = TRUE)+c*cov(At_1,At_2,use="pairwise.complete.obs")+d*cov(At_1,P1t,use="pairwise.complete.obs")+e*cov(At_1,P2t,use="pairwise.complete.obs")+f*cov(At_1,P3t_1,use="pairwise.complete.obs"))
  DL1 <- c*(c*var(At_2,na.rm = TRUE)+b*cov(At_1,At_2,use="pairwise.complete.obs")+d*cov(At_2,P1t,use="pairwise.complete.obs")+e*cov(At_2,P2t,use="pairwise.complete.obs")+f*cov(At_2,P3t_1,use="pairwise.complete.obs"))
  P11 <- d*(d*var(P1t,na.rm = TRUE)+b*cov(At_1,P1t,use="pairwise.complete.obs")+c*cov(At_2,P1t,use="pairwise.complete.obs")+e*cov(P1t,P2t,use="pairwise.complete.obs")+f*cov(P1t,P3t_1,use="pairwise.complete.obs"))
  P21 <- e*(e*var(P2t,na.rm = TRUE)+b*cov(At_1,P2t,use="pairwise.complete.obs")+c*cov(At_2,P2t,use="pairwise.complete.obs")+d*cov(P1t,P2t,use="pairwise.complete.obs")+f*cov(P2t,P3t_1,use="pairwise.complete.obs"))
  P31 <- f*(f*var(P3t_1,na.rm = TRUE)+b*cov(At_1,P3t_1,use="pairwise.complete.obs")+c*cov(At_2,P3t_1,use="pairwise.complete.obs")+d*cov(P1t,P3t_1,use="pairwise.complete.obs")+e*cov(P2t,P3t_1,use="pairwise.complete.obs"))
  tableau_var[i,2] <- DD1 + DL1 + P11 + P21 + P31 
  tableau_var[i,2] <- as.numeric(tableau_var[i,2])
  tableau_var[i,3] <- DD1/tableau_var[i,2]
  tableau_var[i,4] <- DL1/tableau_var[i,2]
  tableau_var[i,5] <- P11/tableau_var[i,2]
  tableau_var[i,6] <- P21/tableau_var[i,2]
  tableau_var[i,7] <- P31/tableau_var[i,2]
}

for (i in vect22){
  b <- estimateurs_tableau_full_set[i,9]
  c <- estimateurs_tableau_full_set[i,10]
  d <- estimateurs_tableau_full_set[i,11]
  e <- estimateurs_tableau_full_set[i,12]
  f <- estimateurs_tableau_full_set[i,13]
  DD2 <- b*(b*var(St,na.rm = TRUE)+c*cov(St,St_1,use="pairwise.complete.obs")+d*cov(St,P1t,use="pairwise.complete.obs")+e*cov(St,P2t,use="pairwise.complete.obs")+f*cov(St,P3t,use="pairwise.complete.obs"))
  DL2 <- c*(c*var(St_1,na.rm = TRUE)+b*cov(St,St_1,use="pairwise.complete.obs")+d*cov(St_1,P1t,use="pairwise.complete.obs")+e*cov(St_1,P2t,use="pairwise.complete.obs")+f*cov(St_1,P3t,use="pairwise.complete.obs"))
  P12 <- d*(d*var(P1t,na.rm = TRUE)+b*cov(St,P1t,use="pairwise.complete.obs")+c*cov(St_1,P1t,use="pairwise.complete.obs")+e*cov(P1t,P2t,use="pairwise.complete.obs")+f*cov(P1t,P3t,use="pairwise.complete.obs"))
  P22 <- e*(e*var(P2t,na.rm = TRUE)+b*cov(St,P2t,use="pairwise.complete.obs")+c*cov(St_1,P2t,use="pairwise.complete.obs")+d*cov(P1t,P2t,use="pairwise.complete.obs")+f*cov(P2t,P3t,use="pairwise.complete.obs"))
  P32 <- f*(f*var(P3t,na.rm = TRUE)+b*cov(St,P3t,use="pairwise.complete.obs")+c*cov(St_1,P3t,use="pairwise.complete.obs")+d*cov(P1t,P3t,use="pairwise.complete.obs")+e*cov(P2t,P3t,use="pairwise.complete.obs"))
  tableau_var[i,8] <- DD2 + DL2 + P12 + P22 + P32 
  tableau_var[i,8] <- as.numeric(tableau_var[i,8])
  tableau_var[i,9] <-  DD2/tableau_var[i,8]
  tableau_var[i,10] <- DL2/tableau_var[i,8]
  tableau_var[i,11] <- P12/tableau_var[i,8]
  tableau_var[i,12] <- P22/tableau_var[i,8]
  tableau_var[i,13] <- P32/tableau_var[i,8]
}

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
