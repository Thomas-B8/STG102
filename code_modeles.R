# Besognet Thomas, 17/04/23 , Stage : Modèles 

# packages 
install.packages("dplyr")
library(dplyr)

# Influence of predators on vole population growth rates 

# Ouverture des données propres

donnees <- read.csv("donnees_propres_korpela.csv", sep=",", header=T, dec=".")


#------------------------------------------------------------------------------------------------------------------------------------------



# I Estimation des paramètres du modèle 

# Avec le Full set ( version avec les trainings set en brouillon )

# Cration du  tableau qui va stocker les estimateurs 

estimateurs_tableau_full_set<-data.frame(matrix("NA",33,13))
names(estimateurs_tableau_full_set)<-c("site","a1","b1","c1","d1","f1","e1","a2","b2","c2","d2","e2","f2")

# Tentative numéro 1 ( avec autant d'estimations des paramètres que de lieux puis moyennage ) ( Tentative 2 en brouillon ) 

vect <-c(1:3,5:33) # trop de NA dans le site numéro 4 , cela crée une erreur : on enlève cette localisation  

# creation de la boucle pour remplir le tableau 

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

# on retire quand il y a des NA dans chaucun des modèles 

estimateurs_tableau_full_set1 <- estimateurs_tableau_full_set[-c(4,18,22,26,30,31,33),] # modèle 1
estimateurs_tableau_full_set2 <- estimateurs_tableau_full_set[-c(4,14,18,26,30,31,33),] # modèle 2

# initialisation des paramètres 

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

# somme pour le calcul des moyennes 

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

# division pour le calcul des moyennes 

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

# remplissage d'un plus petit tableau avec les estimateurs "généaraux"

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

# enregistrement du tableau 

write.csv (tableau_parametres, "parametres_modeles.csv", row.names = T, quote = F)


#------------------------------------------------------------------------------------------------------------------------------------------


# II Calculs des variances et variances expliquées 

# on ouvre le vecteur contenant les paramètres et on remet les valeurs dans des variables bien nommées 

parametres <- read.csv("parametres_modeles.csv", sep=",", header=T, dec=".",stringsAsFactors=FALSE)

a1 <- parametres[1,3]
b1 <- parametres[1,4]
c1 <- parametres[1,5]
d1 <- parametres[1,6]
e1 <- parametres[1,7]
f1 <- parametres[1,8]
a2 <- parametres[2,3]
b2 <- parametres[2,4]
c2 <- parametres[2,5]
d2 <- parametres[2,6]
e2 <- parametres[2,7]
f2 <- parametres[2,8]


# variance entre les localisations à temps fixé pour chaque cardinalité 

# A) pour le nord 

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

# boucle pour remplir le tableau 

for (i in 1:21){ # les i parcourent les années 
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
  P3t <- as.numeric(P3t)
  P3t_1 <- as.numeric(P3t_1)
  
  tableau_var_nord[i,1] <-1990 + i
  
  DD1 <- b1*(b1*var(At_1,na.rm = TRUE)+c1*cov(At_1,At_2,use="pairwise.complete.obs")+d1*cov(At_1,P1t,use="pairwise.complete.obs")+e1*cov(At_1,P2t,use="pairwise.complete.obs")+f1*cov(At_1,P3t_1,use="pairwise.complete.obs"))
  DL1 <- c1*(c1*var(At_2,na.rm = TRUE)+b1*cov(At_1,At_2,use="pairwise.complete.obs")+d1*cov(At_2,P1t,use="pairwise.complete.obs")+e1*cov(At_2,P2t,use="pairwise.complete.obs")+f1*cov(At_2,P3t_1,use="pairwise.complete.obs"))
  P11 <- d1*(d1*var(P1t,na.rm = TRUE)+b1*cov(At_1,P1t,use="pairwise.complete.obs")+c1*cov(At_2,P1t,use="pairwise.complete.obs")+e1*cov(P1t,P2t,use="pairwise.complete.obs")+f1*cov(P1t,P3t_1,use="pairwise.complete.obs"))
  P21 <- e1*(e1*var(P2t,na.rm = TRUE)+b1*cov(At_1,P2t,use="pairwise.complete.obs")+c1*cov(At_2,P2t,use="pairwise.complete.obs")+d1*cov(P1t,P2t,use="pairwise.complete.obs")+f1*cov(P2t,P3t_1,use="pairwise.complete.obs"))
  P31 <- f1*(f1*var(P3t_1,na.rm = TRUE)+b1*cov(At_1,P3t_1,use="pairwise.complete.obs")+c1*cov(At_2,P3t_1,use="pairwise.complete.obs")+d1*cov(P1t,P3t_1,use="pairwise.complete.obs")+e1*cov(P2t,P3t_1,use="pairwise.complete.obs"))
  
  tableau_var_nord[i,2] <- var(St-At_1,na.rm = TRUE) #variance réélle 
  tableau_var_nord[i,3] <- DD1/var(St-At_1,na.rm = TRUE) # variances expliquees
  tableau_var_nord[i,4] <- DL1/var(St-At_1,na.rm = TRUE)
  tableau_var_nord[i,5] <- P11/var(St-At_1,na.rm = TRUE)
  tableau_var_nord[i,6] <- P21/var(St-At_1,na.rm = TRUE)
  tableau_var_nord[i,7] <- P31/var(St-At_1,na.rm = TRUE)
  
  DD2 <- b2*(b2*var(St,na.rm = TRUE)+c2*cov(St,St_1,use="pairwise.complete.obs")+d2*cov(St,P1t,use="pairwise.complete.obs")+e2*cov(St,P2t,use="pairwise.complete.obs")+f2*cov(St,P3t,use="pairwise.complete.obs"))
  DL2 <- c2*(c2*var(St_1,na.rm = TRUE)+b2*cov(St,St_1,use="pairwise.complete.obs")+d2*cov(St_1,P1t,use="pairwise.complete.obs")+e2*cov(St_1,P2t,use="pairwise.complete.obs")+f2*cov(St_1,P3t,use="pairwise.complete.obs"))
  P12 <- d2*(d2*var(P1t,na.rm = TRUE)+b2*cov(St,P1t,use="pairwise.complete.obs")+c2*cov(St_1,P1t,use="pairwise.complete.obs")+e2*cov(P1t,P2t,use="pairwise.complete.obs")+f2*cov(P1t,P3t,use="pairwise.complete.obs"))
  P22 <- e2*(e2*var(P2t,na.rm = TRUE)+b2*cov(St,P2t,use="pairwise.complete.obs")+c2*cov(St_1,P2t,use="pairwise.complete.obs")+d2*cov(P1t,P2t,use="pairwise.complete.obs")+f2*cov(P2t,P3t,use="pairwise.complete.obs"))
  P32 <- f2*(f2*var(P3t,na.rm = TRUE)+b2*cov(St,P3t,use="pairwise.complete.obs")+c2*cov(St_1,P3t,use="pairwise.complete.obs")+d2*cov(P1t,P3t,use="pairwise.complete.obs")+e2*cov(P2t,P3t,use="pairwise.complete.obs"))
  
  tableau_var_nord[i,8] <- var(At-St,na.rm = TRUE) # variance réélle 
  tableau_var_nord[i,9] <-  DD2/var(At-St,na.rm = TRUE) # variances expliquées
  tableau_var_nord[i,10] <- DL2/var(At-St,na.rm = TRUE) 
  tableau_var_nord[i,11] <- P12/var(At-St,na.rm = TRUE)
  tableau_var_nord[i,12] <- P22/var(At-St,na.rm = TRUE)
  tableau_var_nord[i,13] <- P32/var(At-St,na.rm = TRUE)
}

# enregistrement des resultats 

write.csv (tableau_var_nord, "variances_expliquees_nord.csv", row.names = T, quote = F) 

# B) pour l'ouest 

# on commence par créer le tableau qui va stocker les variances et les variances expliquées

tableau_var_ouest <- data.frame(matrix("NA",21,13))
names(tableau_var_ouest)<-c("annee","VarY1","dir1","del1","P11","P21","P31","VarY2","dir2","del2","P12","P22","P32")

# on récupère les données pour l'ouest

donnees_ouest <- filter(donnees,cardinalite=="sud_ouest")

Atn <- data.frame(matrix("NA",23,11))
Stn <- data.frame(matrix("NA",23,11))
P1tn <- data.frame(matrix("NA",23,11))
P2tn <- data.frame(matrix("NA",23,11))
P3tn <- data.frame(matrix("NA",23,11))

# on les remplit des valeurs de nos données 

for (i in 1:11){ # on parcourt les localisations 
  for(j in 1:23){ # on parcourt les années 
    Atn[j,i] <- donnees_ouest[i+j,4]
    Stn[j,i] <- donnees_ouest[i+j,5]
    P1tn[j,i] <- donnees_ouest[i+j,6]
    P2tn[j,i] <- donnees_ouest[i+j,7]
    P3tn[j,i] <- donnees_ouest[i+j,8]
    
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

# boucle pour remplir le tableau 

for (i in 1:21){ # les i parcourent les années 
  
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
  P3t <- as.numeric(P3t)
  P3t_1 <- as.numeric(P3t_1)
  
  tableau_var_ouest[i,1] <-1990 + i
  
  DD1 <- b1*(b1*var(At_1,na.rm = TRUE)+c1*cov(At_1,At_2,use="pairwise.complete.obs")+d1*cov(At_1,P1t,use="pairwise.complete.obs")+e1*cov(At_1,P2t,use="pairwise.complete.obs")+f1*cov(At_1,P3t_1,use="pairwise.complete.obs"))
  DL1 <- c1*(c1*var(At_2,na.rm = TRUE)+b1*cov(At_1,At_2,use="pairwise.complete.obs")+d1*cov(At_2,P1t,use="pairwise.complete.obs")+e1*cov(At_2,P2t,use="pairwise.complete.obs")+f1*cov(At_2,P3t_1,use="pairwise.complete.obs"))
  P11 <- d1*(d1*var(P1t,na.rm = TRUE)+b1*cov(At_1,P1t,use="pairwise.complete.obs")+c1*cov(At_2,P1t,use="pairwise.complete.obs")+e1*cov(P1t,P2t,use="pairwise.complete.obs")+f1*cov(P1t,P3t_1,use="pairwise.complete.obs"))
  P21 <- e1*(e1*var(P2t,na.rm = TRUE)+b1*cov(At_1,P2t,use="pairwise.complete.obs")+c1*cov(At_2,P2t,use="pairwise.complete.obs")+d1*cov(P1t,P2t,use="pairwise.complete.obs")+f1*cov(P2t,P3t_1,use="pairwise.complete.obs"))
  P31 <- f1*(f1*var(P3t_1,na.rm = TRUE)+b1*cov(At_1,P3t_1,use="pairwise.complete.obs")+c1*cov(At_2,P3t_1,use="pairwise.complete.obs")+d1*cov(P1t,P3t_1,use="pairwise.complete.obs")+e1*cov(P2t,P3t_1,use="pairwise.complete.obs"))
  
  tableau_var_ouest[i,2] <- var(St-At_1,na.rm = TRUE) # variance réélle 
  tableau_var_ouest[i,3] <- DD1/var(St-At_1,na.rm = TRUE) # variances explqiuées 
  tableau_var_ouest[i,4] <- DL1/var(St-At_1,na.rm = TRUE)
  tableau_var_ouest[i,5] <- P11/var(St-At_1,na.rm = TRUE)
  tableau_var_ouest[i,6] <- P21/var(St-At_1,na.rm = TRUE)
  tableau_var_ouest[i,7] <- P31/var(St-At_1,na.rm = TRUE)
  
  DD2 <- b2*(b2*var(St,na.rm = TRUE)+c2*cov(St,St_1,use="pairwise.complete.obs")+d2*cov(St,P1t,use="pairwise.complete.obs")+e2*cov(St,P2t,use="pairwise.complete.obs")+f2*cov(St,P3t,use="pairwise.complete.obs"))
  DL2 <- c2*(c2*var(St_1,na.rm = TRUE)+b2*cov(St,St_1,use="pairwise.complete.obs")+d2*cov(St_1,P1t,use="pairwise.complete.obs")+e2*cov(St_1,P2t,use="pairwise.complete.obs")+f2*cov(St_1,P3t,use="pairwise.complete.obs"))
  P12 <- d2*(d2*var(P1t,na.rm = TRUE)+b2*cov(St,P1t,use="pairwise.complete.obs")+c2*cov(St_1,P1t,use="pairwise.complete.obs")+e2*cov(P1t,P2t,use="pairwise.complete.obs")+f2*cov(P1t,P3t,use="pairwise.complete.obs"))
  P22 <- e2*(e2*var(P2t,na.rm = TRUE)+b2*cov(St,P2t,use="pairwise.complete.obs")+c2*cov(St_1,P2t,use="pairwise.complete.obs")+d2*cov(P1t,P2t,use="pairwise.complete.obs")+f2*cov(P2t,P3t,use="pairwise.complete.obs"))
  P32 <- f2*(f2*var(P3t,na.rm = TRUE)+b2*cov(St,P3t,use="pairwise.complete.obs")+c2*cov(St_1,P3t,use="pairwise.complete.obs")+d2*cov(P1t,P3t,use="pairwise.complete.obs")+e2*cov(P2t,P3t,use="pairwise.complete.obs"))
  
  tableau_var_ouest[i,8] <- var(At-St,na.rm = TRUE) # variance réélle 
  tableau_var_ouest[i,9] <-  DD2/var(At-St,na.rm = TRUE) # variance expliquées 
  tableau_var_ouest[i,10] <- DL2/var(At-St,na.rm = TRUE) 
  tableau_var_ouest[i,11] <- P12/var(At-St,na.rm = TRUE)
  tableau_var_ouest[i,12] <- P22/var(At-St,na.rm = TRUE)
  tableau_var_ouest[i,13] <- P32/var(At-St,na.rm = TRUE)
}

# enregistrement des resultats 

write.csv (tableau_var_ouest, "variances_expliquees_ouest.csv", row.names = T, quote = F) 

# C) pour l'est

# on commence par créer le tableau qui va stocker les variances et les variances expliquées

tableau_var_est <- data.frame(matrix("NA",21,13))
names(tableau_var_est)<-c("annee","VarY1","dir1","del1","P11","P21","P31","VarY2","dir2","del2","P12","P22","P32")

# on récupère les données pour l'ouest

donnees_est <- filter(donnees,cardinalite=="est")

Atn <- data.frame(matrix("NA",23,4))
Stn <- data.frame(matrix("NA",23,4))
P1tn <- data.frame(matrix("NA",23,4))
P2tn <- data.frame(matrix("NA",23,4))
P3tn <- data.frame(matrix("NA",23,4))

# on les remplit des valeurs de nos données 

for (i in 1:4){ # on parcourt les localisations 
  for(j in 1:23){ # on parcourt les années 
    Atn[j,i] <- donnees_est[i+j,4]
    Stn[j,i] <- donnees_est[i+j,5]
    P1tn[j,i] <- donnees_est[i+j,6]
    P2tn[j,i] <- donnees_est[i+j,7]
    P3tn[j,i] <- donnees_est[i+j,8]
    
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

# boucle pour remplir le tableau 

for (i in 1:21){ # les i parcourent les années 
  
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
  P3t <- as.numeric(P3t)
  P3t_1 <- as.numeric(P3t_1)
  
  tableau_var_est[i,1] <-1990 + i
  
  DD1 <- b1*(b1*var(At_1,na.rm = TRUE)+c1*cov(At_1,At_2,use="pairwise.complete.obs")+d1*cov(At_1,P1t,use="pairwise.complete.obs")+e1*cov(At_1,P2t,use="pairwise.complete.obs")+f1*cov(At_1,P3t_1,use="pairwise.complete.obs"))
  DL1 <- c1*(c1*var(At_2,na.rm = TRUE)+b1*cov(At_1,At_2,use="pairwise.complete.obs")+d1*cov(At_2,P1t,use="pairwise.complete.obs")+e1*cov(At_2,P2t,use="pairwise.complete.obs")+f1*cov(At_2,P3t_1,use="pairwise.complete.obs"))
  P11 <- d1*(d1*var(P1t,na.rm = TRUE)+b1*cov(At_1,P1t,use="pairwise.complete.obs")+c1*cov(At_2,P1t,use="pairwise.complete.obs")+e1*cov(P1t,P2t,use="pairwise.complete.obs")+f1*cov(P1t,P3t_1,use="pairwise.complete.obs"))
  P21 <- e1*(e1*var(P2t,na.rm = TRUE)+b1*cov(At_1,P2t,use="pairwise.complete.obs")+c1*cov(At_2,P2t,use="pairwise.complete.obs")+d1*cov(P1t,P2t,use="pairwise.complete.obs")+f1*cov(P2t,P3t_1,use="pairwise.complete.obs"))
  P31 <- f1*(f1*var(P3t_1,na.rm = TRUE)+b1*cov(At_1,P3t_1,use="pairwise.complete.obs")+c1*cov(At_2,P3t_1,use="pairwise.complete.obs")+d1*cov(P1t,P3t_1,use="pairwise.complete.obs")+e1*cov(P2t,P3t_1,use="pairwise.complete.obs"))
  
  tableau_var_est[i,2] <- var(St-At_1,na.rm = TRUE) # variance réélle  
  tableau_var_est[i,3] <- DD1/var(St-At_1,na.rm = TRUE) # variances expliquées 
  tableau_var_est[i,4] <- DL1/var(St-At_1,na.rm = TRUE)
  tableau_var_est[i,5] <- P11/var(St-At_1,na.rm = TRUE)
  tableau_var_est[i,6] <- P21/var(St-At_1,na.rm = TRUE)
  tableau_var_est[i,7] <- P31/var(St-At_1,na.rm = TRUE)
  
  DD2 <- b2*(b2*var(St,na.rm = TRUE)+c2*cov(St,St_1,use="pairwise.complete.obs")+d2*cov(St,P1t,use="pairwise.complete.obs")+e2*cov(St,P2t,use="pairwise.complete.obs")+f2*cov(St,P3t,use="pairwise.complete.obs"))
  DL2 <- c2*(c2*var(St_1,na.rm = TRUE)+b2*cov(St,St_1,use="pairwise.complete.obs")+d2*cov(St_1,P1t,use="pairwise.complete.obs")+e2*cov(St_1,P2t,use="pairwise.complete.obs")+f2*cov(St_1,P3t,use="pairwise.complete.obs"))
  P12 <- d2*(d2*var(P1t,na.rm = TRUE)+b2*cov(St,P1t,use="pairwise.complete.obs")+c2*cov(St_1,P1t,use="pairwise.complete.obs")+e2*cov(P1t,P2t,use="pairwise.complete.obs")+f2*cov(P1t,P3t,use="pairwise.complete.obs"))
  P22 <- e2*(e2*var(P2t,na.rm = TRUE)+b2*cov(St,P2t,use="pairwise.complete.obs")+c2*cov(St_1,P2t,use="pairwise.complete.obs")+d2*cov(P1t,P2t,use="pairwise.complete.obs")+f2*cov(P2t,P3t,use="pairwise.complete.obs"))
  P32 <- f2*(f2*var(P3t,na.rm = TRUE)+b2*cov(St,P3t,use="pairwise.complete.obs")+c2*cov(St_1,P3t,use="pairwise.complete.obs")+d2*cov(P1t,P3t,use="pairwise.complete.obs")+e2*cov(P2t,P3t,use="pairwise.complete.obs"))
  
  tableau_var_est[i,8] <- var(At-St,na.rm = TRUE) # variance réélle 
  tableau_var_est[i,9] <-  DD2/var(At-St,na.rm = TRUE) # variances expliquées 
  tableau_var_est[i,10] <- DL2/var(At-St,na.rm = TRUE) 
  tableau_var_est[i,11] <- P12/var(At-St,na.rm = TRUE)
  tableau_var_est[i,12] <- P22/var(At-St,na.rm = TRUE)
  tableau_var_est[i,13] <- P32/var(At-St,na.rm = TRUE)
}

# enregistrement des resultats 

write.csv (tableau_var_est, "variances_expliquees_est.csv", row.names = T, quote = F) 


#----------------------------------------------------------------------------------------------------------------------------------------------

# simulations sur 1000 ans 

# creation du tableau qui va stocker les resultats
simulations <-data.frame(matric(0,24,6)) 
names(simulations) <- c("scenario","region","direct density dependance","delayed density dependance","s-index","seasonality")

# on ouvre le vecteur contenant les paramètres et on remet les valeurs dans des variables bien nommées 

parametres <- read.csv("parametres_modeles.csv", sep=",", header=T, dec=".",stringsAsFactors=FALSE)

a1 <- parametres[1,3]
b1 <- parametres[1,4]
c1 <- parametres[1,5]
d1 <- parametres[1,6]
e1 <- parametres[1,7]
f1 <- parametres[1,8]
a2 <- parametres[2,3]
b2 <- parametres[2,4]
c2 <- parametres[2,5]
d2 <- parametres[2,6]
e2 <- parametres[2,7]
f2 <- parametres[2,8]

# On va maintenant parcourir les différents scénarios et les différentes régions pour compléter notre tableau 

# Scénario numéro 1 : modèle complet 

# supressions 
# pas de supressions dans ce modèle 




# scénario numéro 2: sans les petits mustélidés 

# supressions ( pour tout le scénario numéro 2 )
P1 <-min(na.omit(donnees$Small.mustelid))

# nord 
# initialisations des autres variables : on commence à 1989 car on veut aussi modéliser les anciennes valeurs ( donc fin en 2989 )

Present <- filter(donnees,cardinalite=="nord",year==1990)
Retard <- filter(donnees,cardinalite=="nord",year==1989)
At <- mean(na.omit(Present$Vole.Autumn))
At_1 <- mean(na.omit(Retard$Vole.Autumn))
St <- mean(na.omit(Present$Vole.Spring))
St_1 <- mean(na.omit(Retard$Vole.Spring))
P2t <- mean(na.omit(Present$Generalist.predator))
P3t <-mean(na.omit(Present$Avian.predator))
saisonalite <- 0

# creation des vecteurs de stockage des valeurs 
Vole <- data.frame(matrix(0,2000,1))
Yt <- data.frame(0,2000,1)
tableau_density_dependance <- data.frame(0,40,2)
tableau_s_index <- data.frame(0,40,1)
tableau_saisonalite <- data.frame(0,40,1)

# boucle sur 1000 ans , stocker les 1000 valeurs dans un tableau 

for (i in 1:1000){
  # on va calculer chacun de nos indices de densité dans l'ordre chronologique de l'année
  # attention : les indices ne sont pas les même que dans la forumle car ils évoluent en cours de route ( vérification faite sur papier )
  P2t <- a3 + P2t +b3*At +c3*St +d3*At_1 # hiver 
  St_1 <- St
  St <- a1 + (b1+1)*At +c1*At_1 +d1*P1 +e1*P2t + f1*P3t # printemps 
  P3t <- a3 + P3t +b3*St+c3*At+d3*St_1 # été 
  At_1 <- At 
  At <- a2 + (b2+1)*St + c2*St_1 +d2*P1 +e2*P2t +f2*P3t # automne 
  # Stockage des valeurs de densités pour les campagnols 
  Vole[1,2*i-1] <- St
  Vole[1,2*i] <- At
  # on calcules les "growth rates" et la saisonnalité  qui seront utiles pour la suite 
  Y1 <- St-At_1
  Yt[1,2*i-1] <- Y1
  Y2 <- At-St
  Yt[1,2*i] <- Y2
  saisonalite <- saisonalite+(Y1-Y2)
  # tous les 25 ans, on passe une section 
  if (i %%25 ==0){
  # on calcule direct density dependance et delayed density dependance 
  Direct <- lm(formula = Yt ~ Vole, na.action=na.omit)
  Delayed <- lm(formula = Yt ~ Vole_1, na.action=na.omit)
  tableau_density_dependance[i,1] <- summary(Direct)$r.squared
  tableau_density_dependance[i,2] <- summary(Delayed)$r.squared
  # s index 
  s_index <- Var(Vole)
  tableau_s_index[i,1] <- s_index
  # saisonnalité 
  saisonalite_moyenne <- saisonalite/25
  tableau_saisonalite[i,1] <- saisonalite_moyenne
  saisonalite <- 0
  }
}

# enregristrement des résultats en vu d'une représentation graphique 

simulations[4,1] <- 2
simulations[4,2] <- "north"
simulations[4,3] <- mean(tableau_density_dependance[,1])
simulations[4,4]<- mean(tableau_density_dependance[,2])
simulations[4,5] <- mean(tableau_s_index)
simulations[4,6] <- mean(tableau_saisonalite)





# ouest
# initialisations des autres variables avec celles de 2011 ( 2010 pour t-2 ? )

# est
# initialisations des autres variables avec celles de 2011 ( 2010 pour t-2 ? )



# scénrio numéro 3: avec uniquement les petits mustélidés 

# supressions 
P2 <-min(na.omit(donnees$Generalist.predator))
P3 <-min(na.omit(donnees$Avian.predator))



# scénario numéro 4: sans les prédateurs généralistes 

# supressions 
P2 <-min(na.omit(donnees$Generalist.predator))



# scénario numéro 5: avec uniquement les prédateurs généralistes 

# supressions 
P1 <-min(na.omit(donnees$Small.mustelid))
P3 <-min(na.omit(donnees$Avian.predator))



# scénario numéro 6 : sans les prédateurs aériens 

# supressions 
P3 <-min(na.omit(donnees$Avian.predator))



# scnério numéro 7: avec uniquement les prédateurs aériens 

# supressions 
P1 <-min(na.omit(donnees$Small.mustelid))
P2 <-min(na.omit(donnees$Generalist.predator))



# scénario numéro 8: sans aucun prédateurs 

# supressions 
P1 <-min(na.omit(donnees$Small.mustelid))
P2 <-min(na.omit(donnees$Generalist.predator))
P3 <-min(na.omit(donnees$Avian.predator))

# FIN : on enregistre notre tableau avec toutes les valeurs 

write.csv (simulations, "simulations.csv", row.names = T, quote = F) 













