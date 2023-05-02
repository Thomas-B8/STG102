# Besognet Thomas, 17/04/23 , Stage : Modèles 
# Version 1 : les estimateurs sont estimés sur le dataset entier , puis les variances et covariances sont calculées par régions 

# packages 
install.packages("dplyr")
library(dplyr)

# Influence of predators on vole population growth rates 

# Ouverture des données propres

donnees <- read.csv("donnees_propres_korpela.csv", sep=",", header=T, dec=".")


#------------------------------------------------------------------------------------------------------------------------------------------



# I Estimation des paramètres du modèle 

# Avec le Full set ( version avec les trainings set en brouillon )


# Tentative numéro 3 estimation des paramètres  ( avec un seul estimateur par modèle ) 

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

modele1 <- lm(formula = St ~ At_1 + At_2 + P1t + P2t + P3t_1 ,offset = At_1 )
modele2 <- lm(formula = At ~ St + St_1 + P1t + P2t + P3t ,offset = St   )

summary(modele1)
summary(modele2)

tableau_parametres2 <- data.frame(matrix(0,2,7))
names(tableau_parametres2)<-c("modele","a","b","c","d","e","f")

tableau_parametres2[1,1] <- 1
tableau_parametres2[2,1] <- 2
tableau_parametres2[1,2] <- modele1$coefficients[1]
tableau_parametres2[1,3] <- modele1$coefficients[2]
tableau_parametres2[1,4] <- modele1$coefficients[3]
tableau_parametres2[1,5] <- modele1$coefficients[4]
tableau_parametres2[1,6] <- modele1$coefficients[5]
tableau_parametres2[1,7] <- modele1$coefficients[6]
tableau_parametres2[2,2] <- modele2$coefficients[1]
tableau_parametres2[2,3] <- modele2$coefficients[2]
tableau_parametres2[2,4] <- modele2$coefficients[3]
tableau_parametres2[2,5] <- modele2$coefficients[4]
tableau_parametres2[2,6] <- modele2$coefficients[5]
tableau_parametres2[2,7] <- modele2$coefficients[6]

# enregistrement du tableau 

write.csv (tableau_parametres2, "parametres_modeles2.csv", row.names = T, quote = F)


#------------------------------------------------------------------------------------------------------------------------------------------


# II Calculs des variances et variances expliquées 

# on ouvre le vecteur contenant les paramètres et on remet les valeurs dans des variables bien nommées 

parametres <- read.csv("parametres_modeles2.csv", sep=",", header=T, dec=".",stringsAsFactors=FALSE)

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
donnees_nord <- filter(donnees_nord,X>100) # problème avec inari 

Atn <- data.frame(matrix("NA",23,5))
Stn <- data.frame(matrix("NA",23,5))
P1tn <- data.frame(matrix("NA",23,5))
P2tn <- data.frame(matrix("NA",23,5))
P3tn <- data.frame(matrix("NA",23,5))

# on les remplit des valeurs de nos données , une ligne par année et une colonne par localisation 

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

# Corrections à apporter : refaire le code avec les paramètres dépendants de chaque zone + importation également de ceux pour les prédateurs 


# creation du tableau qui va stocker les resultats
simulations <-data.frame(matric(0,24,10)) 
names(simulations) <- c("scenario","region","direct density dependance","delayed density dependance","s-index","min_s","max_s","seasonality","min_n","max_n")

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

#_______________________________________________________________________________________________________________________________

# Scénario numéro 1 : modèle complet 

# supressions 
# pas de supressions dans ce modèle 

# 1.1 nord 
# initialisations des autres variables : on commence à 1991 car on veut aussi modéliser les anciennes valeurs ( donc fin en 2991 )

Present <- filter(donnees,cardinalite=="nord",year==1990)
Retard <- filter(donnees,cardinalite=="nord",year==1989)
At <- mean(na.omit(Present$Vole.Autumn))
At_1 <- mean(na.omit(Retard$Vole.Autumn))
St <- mean(na.omit(Present$Vole.Spring))
St_1 <- mean(na.omit(Retard$Vole.Spring))
P1t <- mean(na.omit(Present$Small.mustelid))
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
  P1t <- a3 + P1t +b3*At +c3*St + d3*At_1 # hiver 
  P2t <- a3 + P2t +b3*At +c3*St +d3*At_1 
  St_1 <- St
  St <- a1 + (b1+1)*At +c1*At_1 +d1*P1t +e1*P2t + f1*P3t # printemps 
  P3t <- a3 + P3t +b3*St+c3*At+d3*St_1 # été 
  At_1 <- At 
  At <- a2 + (b2+1)*St + c2*St_1 +d2*P1t +e2*P2t +f2*P3t # automne 
  # Stockage des valeurs de densités pour les campagnols 
  Vole[1,2*i-1] <- St
  Vole[1,2*i] <- At
  Vole_1[1,2*i-1] <- St_1
  Vole_1[1,2*i] <- At_1
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

simulations[1,1] <- 1
simulations[1,2] <- "north"
simulations[1,3] <- mean(tableau_density_dependance[,1])
simulations[1,4]<- mean(tableau_density_dependance[,2])
simulations[1,5] <- mean(tableau_s_index)
simulations[1,6]<- mean(tableau_s_index)-1.96*(var(tableau_s_index)/sqrt(40)) #IC 95% ( bas) 
simulations[1,7]<- mean(tableau_s_index)+1.96*(var(tableau_s_index)/sqrt(40)) #IC 95% ( haut )
simulations[1,8] <- mean(tableau_saisonalite)
simulations[1,9]<- mean(tableau_saisonalite)-1.96*(var(tableau_saisonalite)/sqrt(40)) #IC 95% ( bas) 
simulations[1,10]<- mean(tableau_saisonalite)+1.96*(var(tableau_saisonalite)/sqrt(40)) #IC 95% ( haut ) 

# 1.2 ouest
# initialisations des autres variables : on commence à 1991 car on veut aussi modéliser les anciennes valeurs ( donc fin en 2991 )

Present <- filter(donnees,cardinalite=="sud_ouest",year==1990)
Retard <- filter(donnees,cardinalite=="sud_ouest",year==1989)
At <- mean(na.omit(Present$Vole.Autumn))
At_1 <- mean(na.omit(Retard$Vole.Autumn))
St <- mean(na.omit(Present$Vole.Spring))
St_1 <- mean(na.omit(Retard$Vole.Spring))
P1t <- mean(na.omit(Present$Small.mustelid))
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
  P1t <- a3 + P1t +b3*At +c3*St + d3*At_1 # hiver 
  P2t <- a3 + P2t +b3*At +c3*St +d3*At_1 # hiver 
  St_1 <- St
  St <- a1 + (b1+1)*At +c1*At_1 +d1*P1t +e1*P2t + f1*P3t # printemps 
  P3t <- a3 + P3t +b3*St+c3*At+d3*St_1 # été 
  At_1 <- At 
  At <- a2 + (b2+1)*St + c2*St_1 +d2*P1t +e2*P2t +f2*P3t # automne 
  # Stockage des valeurs de densités pour les campagnols 
  Vole[1,2*i-1] <- St
  Vole[1,2*i] <- At
  Vole_1[1,2*i-1] <- St_1
  Vole_1[1,2*i] <- At_1
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

simulations[2,1] <- 1
simulations[2,2] <- "west"
simulations[2,3] <- mean(tableau_density_dependance[,1])
simulations[2,4]<- mean(tableau_density_dependance[,2])
simulations[2,5] <- mean(tableau_s_index)
simulations[2,6]<- mean(tableau_s_index)-1.96*(var(tableau_s_index)/sqrt(40)) #IC 95% ( bas) 
simulations[2,7]<- mean(tableau_s_index)+1.96*(var(tableau_s_index)/sqrt(40)) #IC 95% ( haut )
simulations[2,8] <- mean(tableau_saisonalite)
simulations[2,9]<- mean(tableau_saisonalite)-1.96*(var(tableau_saisonalite)/sqrt(40)) #IC 95% ( bas) 
simulations[2,10]<- mean(tableau_saisonalite)+1.96*(var(tableau_saisonalite)/sqrt(40)) #IC 95% ( haut ) 

# 1.3 est
# initialisations des autres variables : on commence à 1991 car on veut aussi modéliser les anciennes valeurs ( donc fin en 2991 )

Present <- filter(donnees,cardinalite=="est",year==1990)
Retard <- filter(donnees,cardinalite=="est",year==1989)
At <- mean(na.omit(Present$Vole.Autumn))
At_1 <- mean(na.omit(Retard$Vole.Autumn))
St <- mean(na.omit(Present$Vole.Spring))
St_1 <- mean(na.omit(Retard$Vole.Spring))
P1t <- mean(na.omit(Present$Small.mustelid))
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
  P1t <- a3 + P1t +b3*At +c3*St + d3*At_1 # hiver 
  P2t <- a3 + P2t +b3*At +c3*St +d3*At_1  
  St_1 <- St
  St <- a1 + (b1+1)*At +c1*At_1 +d1*P1t +e1*P2t + f1*P3t # printemps 
  P3t <- a3 + P3t +b3*St+c3*At+d3*St_1 # été 
  At_1 <- At 
  At <- a2 + (b2+1)*St + c2*St_1 +d2*P1t +e2*P2t +f2*P3t # automne 
  # Stockage des valeurs de densités pour les campagnols 
  Vole[1,2*i-1] <- St
  Vole[1,2*i] <- At
  Vole_1[1,2*i-1] <- St_1
  Vole_1[1,2*i] <- At_1
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

simulations[3,1] <- 1
simulations[3,2] <- "east"
simulations[3,3] <- mean(tableau_density_dependance[,1])
simulations[3,4]<- mean(tableau_density_dependance[,2])
simulations[3,5] <- mean(tableau_s_index)
simulations[3,6]<- mean(tableau_s_index)-1.96*(var(tableau_s_index)/sqrt(40)) #IC 95% ( bas) 
simulations[3,7]<- mean(tableau_s_index)+1.96*(var(tableau_s_index)/sqrt(40)) #IC 95% ( haut )
simulations[3,8] <- mean(tableau_saisonalite)
simulations[3,9]<- mean(tableau_saisonalite)-1.96*(var(tableau_saisonalite)/sqrt(40)) #IC 95% ( bas) 
simulations[3,10]<- mean(tableau_saisonalite)+1.96*(var(tableau_saisonalite)/sqrt(40)) #IC 95% ( haut ) 


#_________________________________________________________________________________________________________________________________


# scénario numéro 2: sans les petits mustélidés 

# supressions ( pour tout le scénario numéro 2 )
P1 <-min(na.omit(donnees$Small.mustelid))

# 2.1 nord 
# initialisations des autres variables : on commence à 1991 car on veut aussi modéliser les anciennes valeurs ( donc fin en 2991 )

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
  Vole_1[1,2*i-1] <- St_1
  Vole_1[1,2*i] <- At_1
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
simulations[4,6]<- mean(tableau_s_index)-1.96*(var(tableau_s_index)/sqrt(40)) #IC 95% ( bas) 
simulations[4,7]<- mean(tableau_s_index)+1.96*(var(tableau_s_index)/sqrt(40)) #IC 95% ( haut )
simulations[4,8] <- mean(tableau_saisonalite)
simulations[4,9]<- mean(tableau_saisonalite)-1.96*(var(tableau_saisonalite)/sqrt(40)) #IC 95% ( bas) 
simulations[4,10]<- mean(tableau_saisonalite)+1.96*(var(tableau_saisonalite)/sqrt(40)) #IC 95% ( haut ) 

# 2.2 ouest
# initialisations des autres variables : on commence à 1989 car on veut aussi modéliser les anciennes valeurs ( donc fin en 2989 )

Present <- filter(donnees,cardinalite=="sud_ouest",year==1990)
Retard <- filter(donnees,cardinalite=="sud_ouest",year==1989)
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
  Vole_1[1,2*i-1] <- St_1
  Vole_1[1,2*i] <- At_1
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

simulations[5,1] <- 2
simulations[5,2] <- "west"
simulations[5,3] <- mean(tableau_density_dependance[,1])
simulations[5,4]<- mean(tableau_density_dependance[,2])
simulations[5,5] <- mean(tableau_s_index)
simulations[5,6]<- mean(tableau_s_index)-1.96*(var(tableau_s_index)/sqrt(40)) #IC 95% ( bas) 
simulations[5,7]<- mean(tableau_s_index)+1.96*(var(tableau_s_index)/sqrt(40)) #IC 95% ( haut )
simulations[5,8] <- mean(tableau_saisonalite)
simulations[5,9]<- mean(tableau_saisonalite)-1.96*(var(tableau_saisonalite)/sqrt(40)) #IC 95% ( bas) 
simulations[5,10]<- mean(tableau_saisonalite)+1.96*(var(tableau_saisonalite)/sqrt(40)) #IC 95% ( haut )

# 2.3 est
# initialisations des autres variables : on commence à 1989 car on veut aussi modéliser les anciennes valeurs ( donc fin en 2989 )

Present <- filter(donnees,cardinalite=="est",year==1990)
Retard <- filter(donnees,cardinalite=="est",year==1989)
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
  Vole_1[1,2*i-1] <- St_1
  Vole_1[1,2*i] <- At_1
  # on calcules les "growth rates" et la saisonnalité  qui seront utiles pour la suite 
  Y1 <- St-At_1
  Yt[1,2*i-1] <- Y1
  Y2 <- At-St
  Yt[1,2*i] <- Y2
  saisonalite <- saisonalite+(Y1-Y2)
  # tous les 25 ans, on passe une section 
  if (i %%25 ==0){
    # on calcule direct density dependance et delayed density dependance 
    Density_dependance <- lm(formula = Yt ~ Vole+Vole_1, na.action=na.omit)
    tableau_density_dependance[i,1] <- summary(Density_dependance)$coefficient[1]
    tableau_density_dependance[i,2] <- summary(Density_dependance)$coefficient[2]
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

simulations[6,1] <- 2
simulations[6,2] <- "east"
simulations[6,3] <- mean(tableau_density_dependance[,1])
simulations[6,4]<- mean(tableau_density_dependance[,2])
simulations[6,5] <- mean(tableau_s_index)
simulations[6,6]<- mean(tableau_s_index)-1.96*(var(tableau_s_index)/sqrt(40)) #IC 95% ( bas) 
simulations[6,7]<- mean(tableau_s_index)+1.96*(var(tableau_s_index)/sqrt(40)) #IC 95% ( haut )
simulations[6,8] <- mean(tableau_saisonalite)
simulations[6,9]<- mean(tableau_saisonalite)-1.96*(var(tableau_saisonalite)/sqrt(40)) #IC 95% ( bas) 
simulations[6,10]<- mean(tableau_saisonalite)+1.96*(var(tableau_saisonalite)/sqrt(40)) #IC 95% ( haut )

#_______________________________________________________________________________________________________________________________________________

# scénario numéro 3: avec uniquement les petits mustélidés 

# supressions 
P2 <-min(na.omit(donnees$Generalist.predator))
P3 <-min(na.omit(donnees$Avian.predator))

# 3.1 nord
# initialisations des autres variables : on commence à 1991 car on veut aussi modéliser les anciennes valeurs ( donc fin en 2991 )

Present <- filter(donnees,cardinalite=="nord",year==1990)
Retard <- filter(donnees,cardinalite=="nord",year==1989)
At <- mean(na.omit(Present$Vole.Autumn))
At_1 <- mean(na.omit(Retard$Vole.Autumn))
St <- mean(na.omit(Present$Vole.Spring))
St_1 <- mean(na.omit(Retard$Vole.Spring))
P1t <- mean(na.omit(Present$Small.mustelid))
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
  P1t <- a3 + P2t +b3*At +c3*St +d3*At_1 # hiver
  St_1 <- St
  St <- a1 + (b1+1)*At +c1*At_1 +d1*P1t +e1*P2 + f1*P3 # printemps 
  At_1 <- At 
  At <- a2 + (b2+1)*St + c2*St_1 +d2*P1t +e2*P2 +f2*P3 # automne 
  # Stockage des valeurs de densités pour les campagnols 
  Vole[1,2*i-1] <- St
  Vole[1,2*i] <- At
  Vole_1[1,2*i-1] <- St_1
  Vole_1[1,2*i] <- At_1
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

simulations[7,1] <- 3
simulations[7,2] <- "north"
simulations[7,3] <- mean(tableau_density_dependance[,1])
simulations[7,4]<- mean(tableau_density_dependance[,2])
simulations[7,5] <- mean(tableau_s_index)
simulations[7,6]<- mean(tableau_s_index)-1.96*(var(tableau_s_index)/sqrt(40)) #IC 95% ( bas) 
simulations[7,7]<- mean(tableau_s_index)+1.96*(var(tableau_s_index)/sqrt(40)) #IC 95% ( haut )
simulations[7,8] <- mean(tableau_saisonalite)
simulations[7,9]<- mean(tableau_saisonalite)-1.96*(var(tableau_saisonalite)/sqrt(40)) #IC 95% ( bas) 
simulations[7,10]<- mean(tableau_saisonalite)+1.96*(var(tableau_saisonalite)/sqrt(40)) #IC 95% ( haut ) 

# 3.2 ouest
# initialisations des autres variables : on commence à 1991 car on veut aussi modéliser les anciennes valeurs ( donc fin en 2991 )

Present <- filter(donnees,cardinalite=="sud_ouest",year==1990)
Retard <- filter(donnees,cardinalite=="sud_ouest",year==1989)
At <- mean(na.omit(Present$Vole.Autumn))
At_1 <- mean(na.omit(Retard$Vole.Autumn))
St <- mean(na.omit(Present$Vole.Spring))
St_1 <- mean(na.omit(Retard$Vole.Spring))
P1t <- mean(na.omit(Present$Small.mustelid))
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
  P1t <- a3 + P2t +b3*At +c3*St +d3*At_1 # hiver
  St_1 <- St
  St <- a1 + (b1+1)*At +c1*At_1 +d1*P1t +e1*P2 + f1*P3 # printemps 
  At_1 <- At 
  At <- a2 + (b2+1)*St + c2*St_1 +d2*P1t +e2*P2 +f2*P3 # automne 
  # Stockage des valeurs de densités pour les campagnols 
  Vole[1,2*i-1] <- St
  Vole[1,2*i] <- At
  Vole_1[1,2*i-1] <- St_1
  Vole_1[1,2*i] <- At_1
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

simulations[8,1] <- 3
simulations[8,2] <- "west"
simulations[8,3] <- mean(tableau_density_dependance[,1])
simulations[8,4]<- mean(tableau_density_dependance[,2])
simulations[8,5] <- mean(tableau_s_index)
simulations[8,6]<- mean(tableau_s_index)-1.96*(var(tableau_s_index)/sqrt(40)) #IC 95% ( bas) 
simulations[8,7]<- mean(tableau_s_index)+1.96*(var(tableau_s_index)/sqrt(40)) #IC 95% ( haut )
simulations[8,8] <- mean(tableau_saisonalite)
simulations[8,9]<- mean(tableau_saisonalite)-1.96*(var(tableau_saisonalite)/sqrt(40)) #IC 95% ( bas) 
simulations[8,10]<- mean(tableau_saisonalite)+1.96*(var(tableau_saisonalite)/sqrt(40)) #IC 95% ( haut ) 

# 3.3 est
# initialisations des autres variables : on commence à 1991 car on veut aussi modéliser les anciennes valeurs ( donc fin en 2991 )

Present <- filter(donnees,cardinalite=="est",year==1990)
Retard <- filter(donnees,cardinalite=="est",year==1989)
At <- mean(na.omit(Present$Vole.Autumn))
At_1 <- mean(na.omit(Retard$Vole.Autumn))
St <- mean(na.omit(Present$Vole.Spring))
St_1 <- mean(na.omit(Retard$Vole.Spring))
P1t <- mean(na.omit(Present$Small.mustelid))
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
  P1t <- a3 + P2t +b3*At +c3*St +d3*At_1 # hiver
  St_1 <- St
  St <- a1 + (b1+1)*At +c1*At_1 +d1*P1t +e1*P2 + f1*P3 # printemps 
  At_1 <- At 
  At <- a2 + (b2+1)*St + c2*St_1 +d2*P1t +e2*P2 +f2*P3 # automne 
  # Stockage des valeurs de densités pour les campagnols 
  Vole[1,2*i-1] <- St
  Vole[1,2*i] <- At
  Vole_1[1,2*i-1] <- St_1
  Vole_1[1,2*i] <- At_1
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

simulations[9,1] <- 3
simulations[9,2] <- "east"
simulations[9,3] <- mean(tableau_density_dependance[,1])
simulations[9,4]<- mean(tableau_density_dependance[,2])
simulations[9,5] <- mean(tableau_s_index)
simulations[9,6]<- mean(tableau_s_index)-1.96*(var(tableau_s_index)/sqrt(40)) #IC 95% ( bas) 
simulations[9,7]<- mean(tableau_s_index)+1.96*(var(tableau_s_index)/sqrt(40)) #IC 95% ( haut )
simulations[9,8] <- mean(tableau_saisonalite)
simulations[9,9]<- mean(tableau_saisonalite)-1.96*(var(tableau_saisonalite)/sqrt(40)) #IC 95% ( bas) 
simulations[9,10]<- mean(tableau_saisonalite)+1.96*(var(tableau_saisonalite)/sqrt(40)) #IC 95% ( haut ) 

#____________________________________________________________________________________________________________________________________

# scénario numéro 4: sans les prédateurs généralistes 

# supressions 
P2 <-min(na.omit(donnees$Generalist.predator))

# 4.1 ord 
# initialisations des autres variables : on commence à 1991 car on veut aussi modéliser les anciennes valeurs ( donc fin en 2991 )

Present <- filter(donnees,cardinalite=="nord",year==1990)
Retard <- filter(donnees,cardinalite=="nord",year==1989)
At <- mean(na.omit(Present$Vole.Autumn))
At_1 <- mean(na.omit(Retard$Vole.Autumn))
St <- mean(na.omit(Present$Vole.Spring))
St_1 <- mean(na.omit(Retard$Vole.Spring))
P1t <- mean(na.omit(Present$Small.mustelid))
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
  P1t <- a3 + P1t +b3*At +c3*St + d3*At_1 # hiver 
  St_1 <- St
  St <- a1 + (b1+1)*At +c1*At_1 +d1*P1t +e1*P2 + f1*P3t # printemps 
  P3t <- a3 + P3t +b3*St+c3*At+d3*St_1 # été 
  At_1 <- At 
  At <- a2 + (b2+1)*St + c2*St_1 +d2*P1t +e2*P2 +f2*P3t # automne 
  # Stockage des valeurs de densités pour les campagnols 
  Vole[1,2*i-1] <- St
  Vole[1,2*i] <- At
  Vole_1[1,2*i-1] <- St_1
  Vole_1[1,2*i] <- At_1
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

simulations[10,1] <- 4
simulations[10,2] <- "north"
simulations[10,3] <- mean(tableau_density_dependance[,1])
simulations[10,4]<- mean(tableau_density_dependance[,2])
simulations[10,5] <- mean(tableau_s_index)
simulations[10,6]<- mean(tableau_s_index)-1.96*(var(tableau_s_index)/sqrt(40)) #IC 95% ( bas) 
simulations[10,7]<- mean(tableau_s_index)+1.96*(var(tableau_s_index)/sqrt(40)) #IC 95% ( haut )
simulations[10,8] <- mean(tableau_saisonalite)
simulations[10,9]<- mean(tableau_saisonalite)-1.96*(var(tableau_saisonalite)/sqrt(40)) #IC 95% ( bas) 
simulations[10,10]<- mean(tableau_saisonalite)+1.96*(var(tableau_saisonalite)/sqrt(40)) #IC 95% ( haut ) 

# 4.2 ouest
# initialisations des autres variables : on commence à 1991 car on veut aussi modéliser les anciennes valeurs ( donc fin en 2991 )

Present <- filter(donnees,cardinalite=="sud_ouest",year==1990)
Retard <- filter(donnees,cardinalite=="sud_ouest",year==1989)
At <- mean(na.omit(Present$Vole.Autumn))
At_1 <- mean(na.omit(Retard$Vole.Autumn))
St <- mean(na.omit(Present$Vole.Spring))
St_1 <- mean(na.omit(Retard$Vole.Spring))
P1t <- mean(na.omit(Present$Small.mustelid))
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
  P1t <- a3 + P1t +b3*At +c3*St + d3*At_1 # hiver 
  St_1 <- St
  St <- a1 + (b1+1)*At +c1*At_1 +d1*P1t +e1*P2 + f1*P3t # printemps 
  P3t <- a3 + P3t +b3*St+c3*At+d3*St_1 # été 
  At_1 <- At 
  At <- a2 + (b2+1)*St + c2*St_1 +d2*P1t +e2*P2 +f2*P3t # automne 
  # Stockage des valeurs de densités pour les campagnols 
  Vole[1,2*i-1] <- St
  Vole[1,2*i] <- At
  Vole_1[1,2*i-1] <- St_1
  Vole_1[1,2*i] <- At_1
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

simulations[11,1] <- 4
simulations[11,2] <- "west"
simulations[11,3] <- mean(tableau_density_dependance[,1])
simulations[11,4]<- mean(tableau_density_dependance[,2])
simulations[11,5] <- mean(tableau_s_index)
simulations[11,6]<- mean(tableau_s_index)-1.96*(var(tableau_s_index)/sqrt(40)) #IC 95% ( bas) 
simulations[11,7]<- mean(tableau_s_index)+1.96*(var(tableau_s_index)/sqrt(40)) #IC 95% ( haut )
simulations[11,8] <- mean(tableau_saisonalite)
simulations[11,9]<- mean(tableau_saisonalite)-1.96*(var(tableau_saisonalite)/sqrt(40)) #IC 95% ( bas) 
simulations[11,10]<- mean(tableau_saisonalite)+1.96*(var(tableau_saisonalite)/sqrt(40)) #IC 95% ( haut ) 

# 4.3 est
# initialisations des autres variables : on commence à 1991 car on veut aussi modéliser les anciennes valeurs ( donc fin en 2991 )

Present <- filter(donnees,cardinalite=="est",year==1990)
Retard <- filter(donnees,cardinalite=="est",year==1989)
At <- mean(na.omit(Present$Vole.Autumn))
At_1 <- mean(na.omit(Retard$Vole.Autumn))
St <- mean(na.omit(Present$Vole.Spring))
St_1 <- mean(na.omit(Retard$Vole.Spring))
P1t <- mean(na.omit(Present$Small.mustelid))
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
  P1t <- a3 + P1t +b3*At +c3*St + d3*At_1 # hiver 
  St_1 <- St
  St <- a1 + (b1+1)*At +c1*At_1 +d1*P1t +e1*P2 + f1*P3t # printemps 
  P3t <- a3 + P3t +b3*St+c3*At+d3*St_1 # été 
  At_1 <- At 
  At <- a2 + (b2+1)*St + c2*St_1 +d2*P1t +e2*P2 +f2*P3t # automne 
  # Stockage des valeurs de densités pour les campagnols 
  Vole[1,2*i-1] <- St
  Vole[1,2*i] <- At
  Vole_1[1,2*i-1] <- St_1
  Vole_1[1,2*i] <- At_1
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

simulations[12,1] <- 4
simulations[12,2] <- "east"
simulations[12,3] <- mean(tableau_density_dependance[,1])
simulations[12,4]<- mean(tableau_density_dependance[,2])
simulations[12,5] <- mean(tableau_s_index)
simulations[12,6]<- mean(tableau_s_index)-1.96*(var(tableau_s_index)/sqrt(40)) #IC 95% ( bas) 
simulations[12,7]<- mean(tableau_s_index)+1.96*(var(tableau_s_index)/sqrt(40)) #IC 95% ( haut )
simulations[12,8] <- mean(tableau_saisonalite)
simulations[12,9]<- mean(tableau_saisonalite)-1.96*(var(tableau_saisonalite)/sqrt(40)) #IC 95% ( bas) 
simulations[12,10]<- mean(tableau_saisonalite)+1.96*(var(tableau_saisonalite)/sqrt(40)) #IC 95% ( haut ) 

#________________________________________________________________________________________________________________________________

# scénario numéro 5: avec uniquement les prédateurs généralistes 

# supressions 
P1 <-min(na.omit(donnees$Small.mustelid))
P3 <-min(na.omit(donnees$Avian.predator))

# 5.1 nord 
# initialisations des autres variables : on commence à 1991 car on veut aussi modéliser les anciennes valeurs ( donc fin en 2991 )

Present <- filter(donnees,cardinalite=="nord",year==1990)
Retard <- filter(donnees,cardinalite=="nord",year==1989)
At <- mean(na.omit(Present$Vole.Autumn))
At_1 <- mean(na.omit(Retard$Vole.Autumn))
St <- mean(na.omit(Present$Vole.Spring))
St_1 <- mean(na.omit(Retard$Vole.Spring))
P2t <- mean(na.omit(Present$Generalist.predator))
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
  P2t <- a3 + P2t +b3*At +c3*St +d3*At_1 
  St_1 <- St
  St <- a1 + (b1+1)*At +c1*At_1 +d1*P1 +e1*P2t + f1*P3 # printemps 
  At_1 <- At 
  At <- a2 + (b2+1)*St + c2*St_1 +d2*P1 +e2*P2t +f2*P3 # automne 
  # Stockage des valeurs de densités pour les campagnols 
  Vole[1,2*i-1] <- St
  Vole[1,2*i] <- At
  Vole_1[1,2*i-1] <- St_1
  Vole_1[1,2*i] <- At_1
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

simulations[13,1] <- 5
simulations[13,2] <- "north"
simulations[13,3] <- mean(tableau_density_dependance[,1])
simulations[13,4]<- mean(tableau_density_dependance[,2])
simulations[13,5] <- mean(tableau_s_index)
simulations[13,6]<- mean(tableau_s_index)-1.96*(var(tableau_s_index)/sqrt(40)) #IC 95% ( bas) 
simulations[13,7]<- mean(tableau_s_index)+1.96*(var(tableau_s_index)/sqrt(40)) #IC 95% ( haut )
simulations[13,8] <- mean(tableau_saisonalite)
simulations[13,9]<- mean(tableau_saisonalite)-1.96*(var(tableau_saisonalite)/sqrt(40)) #IC 95% ( bas) 
simulations[13,10]<- mean(tableau_saisonalite)+1.96*(var(tableau_saisonalite)/sqrt(40)) #IC 95% ( haut ) 

# 5.2 uest
# initialisations des autres variables : on commence à 1991 car on veut aussi modéliser les anciennes valeurs ( donc fin en 2991 )

Present <- filter(donnees,cardinalite=="sud_ouest",year==1990)
Retard <- filter(donnees,cardinalite=="sud_ouest",year==1989)
At <- mean(na.omit(Present$Vole.Autumn))
At_1 <- mean(na.omit(Retard$Vole.Autumn))
St <- mean(na.omit(Present$Vole.Spring))
St_1 <- mean(na.omit(Retard$Vole.Spring))
P2t <- mean(na.omit(Present$Generalist.predator))
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
  P2t <- a3 + P2t +b3*At +c3*St +d3*At_1 
  St_1 <- St
  St <- a1 + (b1+1)*At +c1*At_1 +d1*P1 +e1*P2t + f1*P3 # printemps 
  At_1 <- At 
  At <- a2 + (b2+1)*St + c2*St_1 +d2*P1 +e2*P2t +f2*P3 # automne 
  # Stockage des valeurs de densités pour les campagnols 
  Vole[1,2*i-1] <- St
  Vole[1,2*i] <- At
  Vole_1[1,2*i-1] <- St_1
  Vole_1[1,2*i] <- At_1
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

simulations[14,1] <- 5
simulations[14,2] <- "west"
simulations[14,3] <- mean(tableau_density_dependance[,1])
simulations[14,4]<- mean(tableau_density_dependance[,2])
simulations[14,5] <- mean(tableau_s_index)
simulations[14,6]<- mean(tableau_s_index)-1.96*(var(tableau_s_index)/sqrt(40)) #IC 95% ( bas) 
simulations[14,7]<- mean(tableau_s_index)+1.96*(var(tableau_s_index)/sqrt(40)) #IC 95% ( haut )
simulations[14,8] <- mean(tableau_saisonalite)
simulations[14,9]<- mean(tableau_saisonalite)-1.96*(var(tableau_saisonalite)/sqrt(40)) #IC 95% ( bas) 
simulations[14,10]<- mean(tableau_saisonalite)+1.96*(var(tableau_saisonalite)/sqrt(40)) #IC 95% ( haut ) 

# 5.3 est
# initialisations des autres variables : on commence à 1991 car on veut aussi modéliser les anciennes valeurs ( donc fin en 2991 )

Present <- filter(donnees,cardinalite=="est",year==1990)
Retard <- filter(donnees,cardinalite=="est",year==1989)
At <- mean(na.omit(Present$Vole.Autumn))
At_1 <- mean(na.omit(Retard$Vole.Autumn))
St <- mean(na.omit(Present$Vole.Spring))
St_1 <- mean(na.omit(Retard$Vole.Spring))
P2t <- mean(na.omit(Present$Generalist.predator))
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
  P2t <- a3 + P2t +b3*At +c3*St +d3*At_1 
  St_1 <- St
  St <- a1 + (b1+1)*At +c1*At_1 +d1*P1 +e1*P2t + f1*P3 # printemps 
  At_1 <- At 
  At <- a2 + (b2+1)*St + c2*St_1 +d2*P1 +e2*P2t +f2*P3 # automne 
  # Stockage des valeurs de densités pour les campagnols 
  Vole[1,2*i-1] <- St
  Vole[1,2*i] <- At
  Vole_1[1,2*i-1] <- St_1
  Vole_1[1,2*i] <- At_1
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

simulations[15,1] <- 5
simulations[15,2] <- "east"
simulations[15,3] <- mean(tableau_density_dependance[,1])
simulations[15,4]<- mean(tableau_density_dependance[,2])
simulations[15,5] <- mean(tableau_s_index)
simulations[15,6]<- mean(tableau_s_index)-1.96*(var(tableau_s_index)/sqrt(40)) #IC 95% ( bas) 
simulations[15,7]<- mean(tableau_s_index)+1.96*(var(tableau_s_index)/sqrt(40)) #IC 95% ( haut )
simulations[15,8] <- mean(tableau_saisonalite)
simulations[15,9]<- mean(tableau_saisonalite)-1.96*(var(tableau_saisonalite)/sqrt(40)) #IC 95% ( bas) 
simulations[15,10]<- mean(tableau_saisonalite)+1.96*(var(tableau_saisonalite)/sqrt(40)) #IC 95% ( haut ) 

#____________________________________________________________________________________________________________________________

# scénario numéro 6 : sans les prédateurs aériens 

# supressions 
P3 <-min(na.omit(donnees$Avian.predator))

# 6.1 nord 
# initialisations des autres variables : on commence à 1991 car on veut aussi modéliser les anciennes valeurs ( donc fin en 2991 )

Present <- filter(donnees,cardinalite=="nord",year==1990)
Retard <- filter(donnees,cardinalite=="nord",year==1989)
At <- mean(na.omit(Present$Vole.Autumn))
At_1 <- mean(na.omit(Retard$Vole.Autumn))
St <- mean(na.omit(Present$Vole.Spring))
St_1 <- mean(na.omit(Retard$Vole.Spring))
P1t <- mean(na.omit(Present$Small.mustelid))
P2t <- mean(na.omit(Present$Generalist.predator))
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
  P1t <- a3 + P1t +b3*At +c3*St + d3*At_1 # hiver 
  P2t <- a3 + P2t +b3*At +c3*St +d3*At_1 
  St_1 <- St
  St <- a1 + (b1+1)*At +c1*At_1 +d1*P1t +e1*P2t + f1*P3 # printemps 
  At_1 <- At 
  At <- a2 + (b2+1)*St + c2*St_1 +d2*P1t +e2*P2t +f2*P3 # automne 
  # Stockage des valeurs de densités pour les campagnols 
  Vole[1,2*i-1] <- St
  Vole[1,2*i] <- At
  Vole_1[1,2*i-1] <- St_1
  Vole_1[1,2*i] <- At_1
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

simulations[16,1] <- 6
simulations[16,2] <- "north"
simulations[16,3] <- mean(tableau_density_dependance[,1])
simulations[16,4]<- mean(tableau_density_dependance[,2])
simulations[16,5] <- mean(tableau_s_index)
simulations[16,6]<- mean(tableau_s_index)-1.96*(var(tableau_s_index)/sqrt(40)) #IC 95% ( bas) 
simulations[16,7]<- mean(tableau_s_index)+1.96*(var(tableau_s_index)/sqrt(40)) #IC 95% ( haut )
simulations[16,8] <- mean(tableau_saisonalite)
simulations[16,9]<- mean(tableau_saisonalite)-1.96*(var(tableau_saisonalite)/sqrt(40)) #IC 95% ( bas) 
simulations[16,10]<- mean(tableau_saisonalite)+1.96*(var(tableau_saisonalite)/sqrt(40)) #IC 95% ( haut ) 

# 6.2 ouest
# initialisations des autres variables : on commence à 1991 car on veut aussi modéliser les anciennes valeurs ( donc fin en 2991 )

Present <- filter(donnees,cardinalite=="sud_ouest",year==1990)
Retard <- filter(donnees,cardinalite=="sud_ouest",year==1989)
At <- mean(na.omit(Present$Vole.Autumn))
At_1 <- mean(na.omit(Retard$Vole.Autumn))
St <- mean(na.omit(Present$Vole.Spring))
St_1 <- mean(na.omit(Retard$Vole.Spring))
P1t <- mean(na.omit(Present$Small.mustelid))
P2t <- mean(na.omit(Present$Generalist.predator))
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
  P1t <- a3 + P1t +b3*At +c3*St + d3*At_1 # hiver 
  P2t <- a3 + P2t +b3*At +c3*St +d3*At_1 
  St_1 <- St
  St <- a1 + (b1+1)*At +c1*At_1 +d1*P1t +e1*P2t + f1*P3 # printemps 
  At_1 <- At 
  At <- a2 + (b2+1)*St + c2*St_1 +d2*P1t +e2*P2t +f2*P3 # automne 
  # Stockage des valeurs de densités pour les campagnols 
  Vole[1,2*i-1] <- St
  Vole[1,2*i] <- At
  Vole_1[1,2*i-1] <- St_1
  Vole_1[1,2*i] <- At_1
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

simulations[17,1] <- 6
simulations[17,2] <- "west"
simulations[17,3] <- mean(tableau_density_dependance[,1])
simulations[17,4]<- mean(tableau_density_dependance[,2])
simulations[17,5] <- mean(tableau_s_index)
simulations[17,6]<- mean(tableau_s_index)-1.96*(var(tableau_s_index)/sqrt(40)) #IC 95% ( bas) 
simulations[17,7]<- mean(tableau_s_index)+1.96*(var(tableau_s_index)/sqrt(40)) #IC 95% ( haut )
simulations[17,8] <- mean(tableau_saisonalite)
simulations[17,9]<- mean(tableau_saisonalite)-1.96*(var(tableau_saisonalite)/sqrt(40)) #IC 95% ( bas) 
simulations[17,10]<- mean(tableau_saisonalite)+1.96*(var(tableau_saisonalite)/sqrt(40)) #IC 95% ( haut ) 

# 6.3 est
# initialisations des autres variables : on commence à 1991 car on veut aussi modéliser les anciennes valeurs ( donc fin en 2991 )

Present <- filter(donnees,cardinalite=="est",year==1990)
Retard <- filter(donnees,cardinalite=="est",year==1989)
At <- mean(na.omit(Present$Vole.Autumn))
At_1 <- mean(na.omit(Retard$Vole.Autumn))
St <- mean(na.omit(Present$Vole.Spring))
St_1 <- mean(na.omit(Retard$Vole.Spring))
P1t <- mean(na.omit(Present$Small.mustelid))
P2t <- mean(na.omit(Present$Generalist.predator))
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
  P1t <- a3 + P1t +b3*At +c3*St + d3*At_1 # hiver 
  P2t <- a3 + P2t +b3*At +c3*St +d3*At_1 
  St_1 <- St
  St <- a1 + (b1+1)*At +c1*At_1 +d1*P1t +e1*P2t + f1*P3 # printemps 
  At_1 <- At 
  At <- a2 + (b2+1)*St + c2*St_1 +d2*P1t +e2*P2t +f2*P3 # automne 
  # Stockage des valeurs de densités pour les campagnols 
  Vole[1,2*i-1] <- St
  Vole[1,2*i] <- At
  Vole_1[1,2*i-1] <- St_1
  Vole_1[1,2*i] <- At_1
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

simulations[18,1] <- 6
simulations[18,2] <- "east"
simulations[18,3] <- mean(tableau_density_dependance[,1])
simulations[18,4]<- mean(tableau_density_dependance[,2])
simulations[18,5] <- mean(tableau_s_index)
simulations[18,6]<- mean(tableau_s_index)-1.96*(var(tableau_s_index)/sqrt(40)) #IC 95% ( bas) 
simulations[18,7]<- mean(tableau_s_index)+1.96*(var(tableau_s_index)/sqrt(40)) #IC 95% ( haut )
simulations[18,8] <- mean(tableau_saisonalite)
simulations[18,9]<- mean(tableau_saisonalite)-1.96*(var(tableau_saisonalite)/sqrt(40)) #IC 95% ( bas) 
simulations[18,10]<- mean(tableau_saisonalite)+1.96*(var(tableau_saisonalite)/sqrt(40)) #IC 95% ( haut ) 

#________________________________________________________________________________________________________________________________


# scénario numéro 7: avec uniquement les prédateurs aériens 

# supressions 
P1 <-min(na.omit(donnees$Small.mustelid))
P2 <-min(na.omit(donnees$Generalist.predator))

# 7.1 nord 
# initialisations des autres variables : on commence à 1991 car on veut aussi modéliser les anciennes valeurs ( donc fin en 2991 )

Present <- filter(donnees,cardinalite=="nord",year==1990)
Retard <- filter(donnees,cardinalite=="nord",year==1989)
At <- mean(na.omit(Present$Vole.Autumn))
At_1 <- mean(na.omit(Retard$Vole.Autumn))
St <- mean(na.omit(Present$Vole.Spring))
St_1 <- mean(na.omit(Retard$Vole.Spring))
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
  St_1 <- St
  St <- a1 + (b1+1)*At +c1*At_1 +d1*P1 +e1*P2 + f1*P3t # printemps 
  P3t <- a3 + P3t +b3*St+c3*At+d3*St_1 # été 
  At_1 <- At 
  At <- a2 + (b2+1)*St + c2*St_1 +d2*P1 +e2*P2 +f2*P3t # automne 
  # Stockage des valeurs de densités pour les campagnols 
  Vole[1,2*i-1] <- St
  Vole[1,2*i] <- At
  Vole_1[1,2*i-1] <- St_1
  Vole_1[1,2*i] <- At_1
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

simulations[19,1] <- 7
simulations[19,2] <- "north"
simulations[19,3] <- mean(tableau_density_dependance[,1])
simulations[19,4]<- mean(tableau_density_dependance[,2])
simulations[19,5] <- mean(tableau_s_index)
simulations[19,6]<- mean(tableau_s_index)-1.96*(var(tableau_s_index)/sqrt(40)) #IC 95% ( bas) 
simulations[19,7]<- mean(tableau_s_index)+1.96*(var(tableau_s_index)/sqrt(40)) #IC 95% ( haut )
simulations[19,8] <- mean(tableau_saisonalite)
simulations[19,9]<- mean(tableau_saisonalite)-1.96*(var(tableau_saisonalite)/sqrt(40)) #IC 95% ( bas) 
simulations[19,10]<- mean(tableau_saisonalite)+1.96*(var(tableau_saisonalite)/sqrt(40)) #IC 95% ( haut ) 

# 7.2 ouest
# initialisations des autres variables : on commence à 1991 car on veut aussi modéliser les anciennes valeurs ( donc fin en 2991 )

Present <- filter(donnees,cardinalite=="sud_ouest",year==1990)
Retard <- filter(donnees,cardinalite=="sud_ouest",year==1989)
At <- mean(na.omit(Present$Vole.Autumn))
At_1 <- mean(na.omit(Retard$Vole.Autumn))
St <- mean(na.omit(Present$Vole.Spring))
St_1 <- mean(na.omit(Retard$Vole.Spring))
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
  St_1 <- St
  St <- a1 + (b1+1)*At +c1*At_1 +d1*P1 +e1*P2 + f1*P3t # printemps 
  P3t <- a3 + P3t +b3*St+c3*At+d3*St_1 # été 
  At_1 <- At 
  At <- a2 + (b2+1)*St + c2*St_1 +d2*P1 +e2*P2 +f2*P3t # automne 
  # Stockage des valeurs de densités pour les campagnols 
  Vole[1,2*i-1] <- St
  Vole[1,2*i] <- At
  Vole_1[1,2*i-1] <- St_1
  Vole_1[1,2*i] <- At_1
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

simulations[20,1] <- 7
simulations[20,2] <- "west"
simulations[20,3] <- mean(tableau_density_dependance[,1])
simulations[20,4]<- mean(tableau_density_dependance[,2])
simulations[20,5] <- mean(tableau_s_index)
simulations[20,6]<- mean(tableau_s_index)-1.96*(var(tableau_s_index)/sqrt(40)) #IC 95% ( bas) 
simulations[20,7]<- mean(tableau_s_index)+1.96*(var(tableau_s_index)/sqrt(40)) #IC 95% ( haut )
simulations[20,8] <- mean(tableau_saisonalite)
simulations[20,9]<- mean(tableau_saisonalite)-1.96*(var(tableau_saisonalite)/sqrt(40)) #IC 95% ( bas) 
simulations[20,10]<- mean(tableau_saisonalite)+1.96*(var(tableau_saisonalite)/sqrt(40)) #IC 95% ( haut ) 

# 7.3 est
# initialisations des autres variables : on commence à 1991 car on veut aussi modéliser les anciennes valeurs ( donc fin en 2991 )

Present <- filter(donnees,cardinalite=="est",year==1990)
Retard <- filter(donnees,cardinalite=="est",year==1989)
At <- mean(na.omit(Present$Vole.Autumn))
At_1 <- mean(na.omit(Retard$Vole.Autumn))
St <- mean(na.omit(Present$Vole.Spring))
St_1 <- mean(na.omit(Retard$Vole.Spring))
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
  St_1 <- St
  St <- a1 + (b1+1)*At +c1*At_1 +d1*P1 +e1*P2 + f1*P3t # printemps 
  P3t <- a3 + P3t +b3*St+c3*At+d3*St_1 # été 
  At_1 <- At 
  At <- a2 + (b2+1)*St + c2*St_1 +d2*P1 +e2*P2 +f2*P3t # automne 
  # Stockage des valeurs de densités pour les campagnols 
  Vole[1,2*i-1] <- St
  Vole[1,2*i] <- At
  Vole_1[1,2*i-1] <- St_1
  Vole_1[1,2*i] <- At_1
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

simulations[21,1] <- 7
simulations[21,2] <- "east"
simulations[21,3] <- mean(tableau_density_dependance[,1])
simulations[21,4]<- mean(tableau_density_dependance[,2])
simulations[21,5] <- mean(tableau_s_index)
simulations[21,6]<- mean(tableau_s_index)-1.96*(var(tableau_s_index)/sqrt(40)) #IC 95% ( bas) 
simulations[21,7]<- mean(tableau_s_index)+1.96*(var(tableau_s_index)/sqrt(40)) #IC 95% ( haut )
simulations[21,8] <- mean(tableau_saisonalite)
simulations[21,9]<- mean(tableau_saisonalite)-1.96*(var(tableau_saisonalite)/sqrt(40)) #IC 95% ( bas) 
simulations[21,10]<- mean(tableau_saisonalite)+1.96*(var(tableau_saisonalite)/sqrt(40)) #IC 95% ( haut )

#_____________________________________________________________________________________________________________________________


# scénario numéro 8: sans aucun prédateurs 

# supressions 
P1 <-min(na.omit(donnees$Small.mustelid))
P2 <-min(na.omit(donnees$Generalist.predator))
P3 <-min(na.omit(donnees$Avian.predator))

# 8.1 nord 
# initialisations des autres variables : on commence à 1991 car on veut aussi modéliser les anciennes valeurs ( donc fin en 2991 )

Present <- filter(donnees,cardinalite=="nord",year==1990)
Retard <- filter(donnees,cardinalite=="nord",year==1989)
At <- mean(na.omit(Present$Vole.Autumn))
At_1 <- mean(na.omit(Retard$Vole.Autumn))
St <- mean(na.omit(Present$Vole.Spring))
St_1 <- mean(na.omit(Retard$Vole.Spring))
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
  St_1 <- St
  St <- a1 + (b1+1)*At +c1*At_1 +d1*P1 +e1*P2 + f1*P3 # printemps 
  At_1 <- At 
  At <- a2 + (b2+1)*St + c2*St_1 +d2*P1 +e2*P2 +f2*P3 # automne 
  # Stockage des valeurs de densités pour les campagnols 
  Vole[1,2*i-1] <- St
  Vole[1,2*i] <- At
  Vole_1[1,2*i-1] <- St_1
  Vole_1[1,2*i] <- At_1
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

simulations[22,1] <- 8
simulations[22,2] <- "north"
simulations[22,3] <- mean(tableau_density_dependance[,1])
simulations[22,4]<- mean(tableau_density_dependance[,2])
simulations[22,5] <- mean(tableau_s_index)
simulations[22,6]<- mean(tableau_s_index)-1.96*(var(tableau_s_index)/sqrt(40)) #IC 95% ( bas) 
simulations[22,7]<- mean(tableau_s_index)+1.96*(var(tableau_s_index)/sqrt(40)) #IC 95% ( haut )
simulations[22,8] <- mean(tableau_saisonalite)
simulations[22,9]<- mean(tableau_saisonalite)-1.96*(var(tableau_saisonalite)/sqrt(40)) #IC 95% ( bas) 
simulations[22,10]<- mean(tableau_saisonalite)+1.96*(var(tableau_saisonalite)/sqrt(40)) #IC 95% ( haut ) 

# 8.2 ouest
# initialisations des autres variables : on commence à 1991 car on veut aussi modéliser les anciennes valeurs ( donc fin en 2991 )

Present <- filter(donnees,cardinalite=="sud_ouest",year==1990)
Retard <- filter(donnees,cardinalite=="sud_ouest",year==1989)
At <- mean(na.omit(Present$Vole.Autumn))
At_1 <- mean(na.omit(Retard$Vole.Autumn))
St <- mean(na.omit(Present$Vole.Spring))
St_1 <- mean(na.omit(Retard$Vole.Spring))
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
  St_1 <- St
  St <- a1 + (b1+1)*At +c1*At_1 +d1*P1 +e1*P2 + f1*P3 # printemps 
  At_1 <- At 
  At <- a2 + (b2+1)*St + c2*St_1 +d2*P1 +e2*P2 +f2*P3 # automne 
  # Stockage des valeurs de densités pour les campagnols 
  Vole[1,2*i-1] <- St
  Vole[1,2*i] <- At
  Vole_1[1,2*i-1] <- St_1
  Vole_1[1,2*i] <- At_1
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

simulations[23,1] <- 8
simulations[23,2] <- "west"
simulations[23,3] <- mean(tableau_density_dependance[,1])
simulations[23,4]<- mean(tableau_density_dependance[,2])
simulations[23,5] <- mean(tableau_s_index)
simulations[23,6]<- mean(tableau_s_index)-1.96*(var(tableau_s_index)/sqrt(40)) #IC 95% ( bas) 
simulations[23,7]<- mean(tableau_s_index)+1.96*(var(tableau_s_index)/sqrt(40)) #IC 95% ( haut )
simulations[23,8] <- mean(tableau_saisonalite)
simulations[23,9]<- mean(tableau_saisonalite)-1.96*(var(tableau_saisonalite)/sqrt(40)) #IC 95% ( bas) 
simulations[23,10]<- mean(tableau_saisonalite)+1.96*(var(tableau_saisonalite)/sqrt(40)) #IC 95% ( haut ) 

# 8.3 est
# initialisations des autres variables : on commence à 1991 car on veut aussi modéliser les anciennes valeurs ( donc fin en 2991 )

Present <- filter(donnees,cardinalite=="est",year==1990)
Retard <- filter(donnees,cardinalite=="est",year==1989)
At <- mean(na.omit(Present$Vole.Autumn))
At_1 <- mean(na.omit(Retard$Vole.Autumn))
St <- mean(na.omit(Present$Vole.Spring))
St_1 <- mean(na.omit(Retard$Vole.Spring))
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
  St_1 <- St
  St <- a1 + (b1+1)*At +c1*At_1 +d1*P1 +e1*P2 + f1*P3 # printemps 
  At_1 <- At 
  At <- a2 + (b2+1)*St + c2*St_1 +d2*P1 +e2*P2 +f2*P3 # automne 
  # Stockage des valeurs de densités pour les campagnols 
  Vole[1,2*i-1] <- St
  Vole[1,2*i] <- At
  Vole_1[1,2*i-1] <- St_1
  Vole_1[1,2*i] <- At_1
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

simulations[24,1] <- 8
simulations[24,2] <- "east"
simulations[24,3] <- mean(tableau_density_dependance[,1])
simulations[24,4]<- mean(tableau_density_dependance[,2])
simulations[24,5] <- mean(tableau_s_index)
simulations[24,6]<- mean(tableau_s_index)-1.96*(var(tableau_s_index)/sqrt(40)) #IC 95% ( bas) 
simulations[24,7]<- mean(tableau_s_index)+1.96*(var(tableau_s_index)/sqrt(40)) #IC 95% ( haut )
simulations[24,8] <- mean(tableau_saisonalite)
simulations[24,9]<- mean(tableau_saisonalite)-1.96*(var(tableau_saisonalite)/sqrt(40)) #IC 95% ( bas) 
simulations[24,10]<- mean(tableau_saisonalite)+1.96*(var(tableau_saisonalite)/sqrt(40)) #IC 95% ( haut ) 



# FIN : on enregistre notre tableau avec toutes les valeurs 

write.csv (simulations, "simulations.csv", row.names = T, quote = F) 













