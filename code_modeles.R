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

# Influence of predators on vole population growth rates 

# Ouverture des données propres

donnees <- read.csv("donnees_propres_korpela.csv", sep=",", header=T, dec=".")

# I Estimation des paramètres du modèle 

# Cration des tableaux qui vont stocker les estimateurs ( un avec moyenne des 50 training set, l'autre avec les données complètes)

# Pour la tentative 1

estimateurs_tableau_training_set<-data.frame(matrix("NA",33,13))
names(estimateurs_tableau_training_set)<-c("site","a1","b1","c1","d1","f1","e1","a2","b2","c2","d2","e2","f2")

estimateurs_tableau_full_set<-data.frame(matrix("NA",33,13))
names(estimateurs_tableau_full_set)<-c("site","a1","b1","c1","d1","f1","e1","a2","b2","c2","d2","e2","f2")


# Avec le Full set 

# Tentative numéro 1 ( avec autant d'estimations des paramètres que de lieux puis moyennage 

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

# On va maintenant moyenner les estimateurs pour obtenir une estimation moyenne de chaque paramètre 

estimateurs_tableau_full_set1 <- estimateurs_tableau_full_set[-c(4,18,22,26,30,31,33),]
estimateurs_tableau_full_set2 <- estimateurs_tableau_full_set[-c(4,14,18,26,30,31,33),]

a1=0
b1=0
c1=0
d1=0
e1=0
f1=0

a2=0
b2=0
c2=0
d2=0
e2=0
f2=0

for (i in 1:26){
  a1 <- a1+estimateurs_tableau_full_set1[i,2]
  b1 <- b1+estimateurs_tableau_full_set1[i,3]
  c1 <- c1+estimateurs_tableau_full_set1[i,4]
  d1 <- d1+estimateurs_tableau_full_set1[i,5]
  e1 <- e1+estimateurs_tableau_full_set1[i,6]
  f1 <- f1+estimateurs_tableau_full_set1[i,7]
  a2 <- a2+estimateurs_tableau_full_set2[i,8]
  b2 <- b2+estimateurs_tableau_full_set2[i,9]
  c2 <- c2+estimateurs_tableau_full_set2[i,10]
  d2 <- d2+estimateurs_tableau_full_set2[i,11]
  e2 <- e2+estimateurs_tableau_full_set2[i,12]
  f2 <- f2+estimateurs_tableau_full_set2[i,13]
}



a1 <- a1/26
b1 <- b1/26
c1 <- c1/26
d1 <- d1/26
e1 <- e1/26
f1 <- f1/26

a2 <- a2/26
b2 <- b2/26
c2 <- c2/26
d2 <- d2/26
e2 <- e2/26
f2 <- f2/26

tableau_parametres <- data.frame("Na",2,7)
names(tableau_parametres)<-c("modele","a","b","c","d","e","f")
tableau_parametres[1,1] <- "modele1"
tableau_parametres[2,1] <- "modele2"
tableau_parametres[1,2] <- a1
tableau_parametres[1,3] <- b1
tableau_parametres[1,4] <- c1
tableau_parametres[1,5] <- d1
tableau_parametres[1,6] <- e1
tableau_parametres[1,7] <- f1
tableau_parametres[2,2] <- a2
tableau_parametres[2,3] <- b2
tableau_parametres[2,4] <- c2
tableau_parametres[2,5] <- d2
tableau_parametres[2,6] <- e2 
tableau_parametres[2,7] <- f2


write.csv (tableau_parametres, "parametres_modeles.csv", row.names = T, quote = F)

# Tentative numéro 2 ( avec un seul estimateur par localisation )

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

# Avec les 50 training set 

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

# II Calculs des variances et variances expliquées 

# variance entre les localisations à temps fixé pour chaque cardinalité 

# pour le nord 

# on commence par créer le tableau qui va stocker les variances et les variances expliquées

tableau_var_nord <- data.frame(matrix("NA",21,13))
names(tableau_var_nord)<-c("annee","VarY1","dir1","del1","P11","P21","P31","VarY2","dir2","del2","P12","P22","P32")

# on récupère les données pour le nord 

donnees_nord <- filter(donnees,cardinalite=="nord")
donnees_nord <- filter(donnees_nord,X>100)

Atn <- data.frame(matrix("NA",23,5))
Stn <- data.frame(matrix("NA",23,5))
P1tn <- data.frame(matrix("NA",23,5))
P2tn <- data.frame(matrix("NA",23,5))
P3tn <- data.frame(matrix("NA",23,5))

# on les remplit des valeurs de nos données 

for (i in 1:5){ # on parcourt les localisations 
  for(j in 1:23){ # on parcourt les années 
    Atn[j,i] <- donnees_nord[i+j,4]
    Stn[j,i] <- donnees_nord[i+j,5]
    P1tn[j,i] <- donnees_nord[i+j,6]
    P2tn[j,i] <- donnees_nord[i+j,7]
    P3tn[j,i] <- donnees_nord[i+j,8]
    
  }
}

# on crée le décalage temporel 

Atn_ <- Atn[-c(1,2),] #present 
Stn_ <- Stn[-c(1,2),] #present 
P1tn_ <- P1tn[-c(1,2),] #present 
P2tn_ <- P2tn[-c(1,2),] #present 
P3tn_ <- P3tn[-c(1,2),] #present 
Atn_1 <- Atn[-c(1,23),] #passé1
Stn_1 <- Stn[-c(1,23),] #passé1
P3tn_1 <- P3tn[-c(1,23),] #passé1
Atn_2 <- Atn[-c(22,23),] #passé2

for (i in 1:21){
  
  At <- Atn_[i,]
  At_1 <- Atn_1[i,]
  At_2 <- Atn_2[i,]
  St <- Stn_[i,]
  St_1 <- Stn_1[i,]
  P1t <- P1tn_[i,]
  P2t <- P2tn_[i,]
  P3t <- P3tn_[i,]
  P3t_1 <- P3tn_1[i,]
  
  At <- as.numeric(At)
  At_1 <- as.numeric(At_1)
  At_2 <- as.numeric(At_2)
  St <-  as.numeric(St)
  St_1 <- as.numeric(St_1)
  P1t <- as.numeric(P1t)
  P2t <- as.numeric(P2t)
  P3t_1 <- as.numeric(P3t_1)
  
  tableau_var_nord[i,1] <-1990 + i
  
  DD1 <- b1*(b1*var(At_1,na.rm = TRUE)+c1*cov(At_1,At_2,use="pairwise.complete.obs")+d1*cov(At_1,P1t,use="pairwise.complete.obs")+e1*cov(At_1,P2t,use="pairwise.complete.obs")+f1*cov(At_1,P3t_1,use="pairwise.complete.obs"))
  DL1 <- c1*(c1*var(At_2,na.rm = TRUE)+b1*cov(At_1,At_2,use="pairwise.complete.obs")+d1*cov(At_2,P1t,use="pairwise.complete.obs")+e1*cov(At_2,P2t,use="pairwise.complete.obs")+f1*cov(At_2,P3t_1,use="pairwise.complete.obs"))
  P11 <- d1*(d1*var(P1t,na.rm = TRUE)+b1*cov(At_1,P1t,use="pairwise.complete.obs")+c1*cov(At_2,P1t,use="pairwise.complete.obs")+e1*cov(P1t,P2t,use="pairwise.complete.obs")+f1*cov(P1t,P3t_1,use="pairwise.complete.obs"))
  P21 <- e1*(e1*var(P2t,na.rm = TRUE)+b1*cov(At_1,P2t,use="pairwise.complete.obs")+c1*cov(At_2,P2t,use="pairwise.complete.obs")+d1*cov(P1t,P2t,use="pairwise.complete.obs")+f1*cov(P2t,P3t_1,use="pairwise.complete.obs"))
  P31 <- f1*(f1*var(P3t_1,na.rm = TRUE)+b1*cov(At_1,P3t_1,use="pairwise.complete.obs")+c1*cov(At_2,P3t_1,use="pairwise.complete.obs")+d1*cov(P1t,P3t_1,use="pairwise.complete.obs")+e1*cov(P2t,P3t_1,use="pairwise.complete.obs"))
  
  tableau_var_nord[i,2] <- DD1 + DL1 + P11 + P21 + P31 
  tableau_var_nord[i,2] <- as.numeric(tableau_var_nord[i,2])
  tableau_var_nord[i,3] <- DD1/tableau_var_nord[i,2]
  tableau_var_nord[i,4] <- DL1/tableau_var_nord[i,2]
  tableau_var_nord[i,5] <- P11/tableau_var_nord[i,2]
  tableau_var_nord[i,6] <- P21/tableau_var_nord[i,2]
  tableau_var_nord[i,7] <- P31/tableau_var_nord[i,2]
  
  DD2 <- b2*(b2*var(St,na.rm = TRUE)+c*cov(St,St_1,use="pairwise.complete.obs")+d2*cov(St,P1t,use="pairwise.complete.obs")+e2*cov(St,P2t,use="pairwise.complete.obs")+f2*cov(St,P3t,use="pairwise.complete.obs"))
  DL2 <- c2*(c2*var(St_1,na.rm = TRUE)+b*cov(St,St_1,use="pairwise.complete.obs")+d2*cov(St_1,P1t,use="pairwise.complete.obs")+e2*cov(St_1,P2t,use="pairwise.complete.obs")+f2*cov(St_1,P3t,use="pairwise.complete.obs"))
  P12 <- d2*(d2*var(P1t,na.rm = TRUE)+b*cov(St,P1t,use="pairwise.complete.obs")+c2*cov(St_1,P1t,use="pairwise.complete.obs")+e2*cov(P1t,P2t,use="pairwise.complete.obs")+f2*cov(P1t,P3t,use="pairwise.complete.obs"))
  P22 <- e2*(e2*var(P2t,na.rm = TRUE)+b*cov(St,P2t,use="pairwise.complete.obs")+c2*cov(St_1,P2t,use="pairwise.complete.obs")+d2*cov(P1t,P2t,use="pairwise.complete.obs")+f2*cov(P2t,P3t,use="pairwise.complete.obs"))
  P32 <- f2*(f2*var(P3t,na.rm = TRUE)+b*cov(St,P3t,use="pairwise.complete.obs")+c2*cov(St_1,P3t,use="pairwise.complete.obs")+d2*cov(P1t,P3t,use="pairwise.complete.obs")+e2*cov(P2t,P3t,use="pairwise.complete.obs"))
  
  tableau_var_nord[i,8] <- DD2 + DL2 + P12 + P22 + P32 
  tableau_var_nord[i,8] <- as.numeric(tableau_var_nord[i,8])
  tableau_var_nord[i,9] <-  DD2/tableau_var_nord[i,8]
  tableau_var_nord[i,10] <- DL2/tableau_var_nord[i,8]
  tableau_var_nord[i,11] <- P12/tableau_var_nord[i,8]
  tableau_var_nord[i,12] <- P22/tableau_var_nord[i,8]
  tableau_var_nord[i,13] <- P32/tableau_var_nord[i,8]
}
  

#





