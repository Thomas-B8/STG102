# Besognet Thomas, 17/04/23 , Stage : Modèles 

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

# II Influence of predators on vole population growth rates 

# Ouverture des données avec directement toutes les modifications précédentes 

setwd("C:\\Users\\thoma\\OneDrive\\Documents\\ETUDES\\SP S2\\STG102_Stage\\Statistiques") #working directory 
donnees <- read.csv("donnees_completees.csv", sep=",", header=T, dec=".")

# Cration des tableaux qui vont stocker les estimateurs ( un avec moyenne des 50 training set, l'autre avec les données complètes)

estimateurs_tableau_training_set<-data.frame(matrix("NA",33,13))
names(estimateurs_tableau_training_set)<-c("site","a1","b1","c1","d1","f1","e1","a2","b2","c2","d2","e2","f2")

estimateurs_tableau_full_set<-data.frame(matrix("NA",33,13))
names(estimateurs_tableau_full_set)<-c("site","a1","b1","c1","d1","f1","e1","a2","b2","c2","d2","e2","f2")

# Avec le Full set 

vect <-c(1:3,5:33) # trop de NA dans le site numéro 4 , cela crée une erreur 

for (i in vect){
  donnees_site <-  filter(donnees,site_ordo==i) # à chaque passage dans la boucle, on ne garde qu'une localisation 
  # creation des vecteurs 
  Present <-filter(donnees_site,year>1990)# on ne prend pas les 2 premières années 
  At    <-Present$Vole.Autumn
  St    <-Present$Vole.Spring
  P1t   <-Present$Small.mustelid 
  P2t   <-Present$Generalist.predator
  P3t   <-Present$Avian.predator
  Passe_1 <-filter(donnees_site,year>1989,year<2011) # on ne prend ni la première ni la dernière année 
  At_1    <-Passe_1$Vole.Autumn
  St_1    <-Passe_1$Vole.Spring
  P3t_1   <-Passe_1$Avian.predator
  Passe_2 <-filter(donnees_site,year<2010) # on ne prend pas les 2 dernières années  
  At_2    <-Passe_2$Vole.Autumn
  # on passe aux modèles  
  modele1 <- lm(formula = St ~ At_1 + At_2 + P1t + P2t + P3t_1 , data = donnees_site,offset = At_1 ,na.action=na.omit)
  modele2 <- lm(formula = At ~ St + St_1 + P1t + P2t + P3t , data = donnees_site,offset = St,na.action=na.omit)
  # remplissage du tableau 
  estimateurs_tableau_full_set[i,1] <- i
  for (j in 2:7){
    estimateurs_tableau_full_set[i,j] <- modele1$coefficients[j-1]
    estimateurs_tableau_full_set[i,j+6] <- modele2$coefficients[j-1]}}

write.csv (estimateurs_tableau_full_set, "estiateurs_full_set.csv", row.names = T, quote = F)
# Avec les 50 training set 

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

estimateurs_tableau_full_set$a1 <- as.numeric(estimateurs_tableau_full_set$a1)
estimateurs_tableau_full_set$b1 <- as.numeric(estimateurs_tableau_full_set$b1)
estimateurs_tableau_full_set$c1 <- as.numeric(estimateurs_tableau_full_set$c1)
estimateurs_tableau_full_set$d1 <- as.numeric(estimateurs_tableau_full_set$d1)
estimateurs_tableau_full_set$e1 <- as.numeric(estimateurs_tableau_full_set$e1)
estimateurs_tableau_full_set$f1 <- as.numeric(estimateurs_tableau_full_set$f1)
estimateurs_tableau_full_set$a2 <- as.numeric(estimateurs_tableau_full_set$a2)
estimateurs_tableau_full_set$b2 <- as.numeric(estimateurs_tableau_full_set$b2)
estimateurs_tableau_full_set$c2 <- as.numeric(estimateurs_tableau_full_set$c2)
estimateurs_tableau_full_set$d2 <- as.numeric(estimateurs_tableau_full_set$d2)
estimateurs_tableau_full_set$e2 <- as.numeric(estimateurs_tableau_full_set$e2)
estimateurs_tableau_full_set$f2 <- as.numeric(estimateurs_tableau_full_set$f2)

# Calculs des variances et variances expliquées 

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





