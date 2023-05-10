# Besognet Thomas, 28/04/23 , Stage : Modèles 
# Version 2 : les estimateurs sont estimés par région , puis les variances et covariances sont calculées sur le dataset entier 

# packages 
install.packages("dplyr")
library(dplyr)

# Influence of predators on vole population growth rates 

# Ouverture des données propres

donnees <- read.csv("donnees_propres_korpela.csv", sep=",", header=T, dec=".")


#------------------------------------------------------------------------------------------------------------------------------------------



# I Estimation des paramètres du modèle 

# Avec le Full set ( version avec les trainings set en brouillon )

# Tentative numéro 4 estimation des paramètres  ( 1 seul estimateur par cobinaison modele x region )

# Creation des tableaux de stockage des valeurs 

tableau_parametres_regions <- data.frame(matrix(0,6,9))
names(tableau_parametres_regions)<-c("region","modele","a","b","c","d","e","f","sigma")

tableau_parametres_regions_predateurs <- data.frame(matrix(0,9,7))
names(tableau_parametres_regions)<-c("region","modele","a","b","c","d","sigma")

# A) pour le nord 

donnees_nord <- filter(donnees,cardinalite=="nord")

Present <-filter(donnees_nord,year>1990)# on ne prend pas les 2 premières années 
At    <-Present$Vole.Autumn
St    <-Present$Vole.Spring
P1t   <-Present$Small.mustelid 
P2t   <-Present$Generalist.predator
P3t   <-Present$Avian.predator
Passe_1 <-filter(donnees_nord,year>1989,year<2011) # on ne prend ni la première ni la dernière année 
At_1    <-Passe_1$Vole.Autumn
St_1    <-Passe_1$Vole.Spring
P1t_1   <-Passe_1$Small.mustelid
P2t_1   <-Passe_1$Generalist.predator
P3t_1   <-Passe_1$Avian.predator
Passe_2 <-filter(donnees_nord,year<2010) # on ne prend pas les 2 dernières années  
At_2    <-Passe_2$Vole.Autumn

modele1 <- lm(formula = St ~ At_1 + At_2 + P1t + P2t + P3t_1 ,offset = At_1 )
modele2 <- lm(formula = At ~ St + St_1 + P1t + P2t + P3t ,offset = St   )
modele3 <- lm(formula = P1t ~ At_1 + St_1 + At_2 ,offset = P1t_1 )
modele4 <- lm(formula = P2t ~ At_1 + St_1 + At_2 ,offset = P2t_1 )
modele5 <- lm(formula = P3t ~ St + At_1 + St_1 ,offset = P3t_1 )

summary(modele1)
summary(modele2)
summary(modele3)
summary(modele4)
summary(modele5)

tableau_parametres_regions[1,1] <- "nord"
tableau_parametres_regions[2,1] <- "nord"
tableau_parametres_regions[1,2] <- 1
tableau_parametres_regions[2,2] <- 2
tableau_parametres_regions[1,3] <- modele1$coefficients[1]
tableau_parametres_regions[1,4] <- modele1$coefficients[2]
tableau_parametres_regions[1,5] <- modele1$coefficients[3]
tableau_parametres_regions[1,6] <- modele1$coefficients[4]
tableau_parametres_regions[1,7] <- modele1$coefficients[5]
tableau_parametres_regions[1,8] <- modele1$coefficients[6]
tableau_parametres_regions[1,9] <- summary.lm(modele1)$sigma
tableau_parametres_regions[2,3] <- modele2$coefficients[1]
tableau_parametres_regions[2,4] <- modele2$coefficients[2]
tableau_parametres_regions[2,5] <- modele2$coefficients[3]
tableau_parametres_regions[2,6] <- modele2$coefficients[4]
tableau_parametres_regions[2,7] <- modele2$coefficients[5]
tableau_parametres_regions[2,8] <- modele2$coefficients[6]
tableau_parametres_regions[2,9] <- summary.lm(modele2)$sigma

tableau_parametres_regions_predateurs[1,1] <- "nord"
tableau_parametres_regions_predateurs[2,1] <- "nord"
tableau_parametres_regions_predateurs[3,1] <- "nord"
tableau_parametres_regions_predateurs[1,2] <- 3
tableau_parametres_regions_predateurs[2,2] <- 4
tableau_parametres_regions_predateurs[3,2] <- 5
tableau_parametres_regions_predateurs[1,3] <- modele3$coefficients[1]
tableau_parametres_regions_predateurs[1,4] <- modele3$coefficients[2]
tableau_parametres_regions_predateurs[1,5] <- modele3$coefficients[3]
tableau_parametres_regions_predateurs[1,6] <- modele3$coefficients[4]
tableau_parametres_regions_predateurs[1,7] <- summary.lm(modele3)$sigma
tableau_parametres_regions_predateurs[2,3] <- modele4$coefficients[1]
tableau_parametres_regions_predateurs[2,4] <- modele4$coefficients[2]
tableau_parametres_regions_predateurs[2,5] <- modele4$coefficients[3]
tableau_parametres_regions_predateurs[2,6] <- modele4$coefficients[4]
tableau_parametres_regions_predateurs[2,7] <- summary.lm(modele4)$sigma
tableau_parametres_regions_predateurs[3,3] <- modele5$coefficients[1]
tableau_parametres_regions_predateurs[3,4] <- modele5$coefficients[2]
tableau_parametres_regions_predateurs[3,5] <- modele5$coefficients[3]
tableau_parametres_regions_predateurs[3,6] <- modele5$coefficients[4]
tableau_parametres_regions_predateurs[3,7] <- summary.lm(modele5)$sigma



# B) Pour l'est 

donnees_est <- filter(donnees,cardinalite=="est")

Present <-filter(donnees_est,year>1990)# on ne prend pas les 2 premières années 
At    <-Present$Vole.Autumn
St    <-Present$Vole.Spring
P1t   <-Present$Small.mustelid 
P2t   <-Present$Generalist.predator
P3t   <-Present$Avian.predator
Passe_1 <-filter(donnees_est,year>1989,year<2011) # on ne prend ni la première ni la dernière année 
At_1    <-Passe_1$Vole.Autumn
St_1    <-Passe_1$Vole.Spring
P1t_1   <-Passe_1$Small.mustelid
P2t_1   <-Passe_1$Generalist.predator
P3t_1   <-Passe_1$Avian.predator
Passe_2 <-filter(donnees_est,year<2010) # on ne prend pas les 2 dernières années  
At_2    <-Passe_2$Vole.Autumn

modele1 <- lm(formula = St ~ At_1 + At_2 + P1t + P2t + P3t_1 ,offset = At_1 )
modele2 <- lm(formula = At ~ St + St_1 + P1t + P2t + P3t ,offset = St   )
modele3 <- lm(formula = P1t ~ At_1 + St_1 + At_2 ,offset = P1t_1 )
modele4 <- lm(formula = P2t ~ At_1 + St_1 + At_2 ,offset = P2t_1 )
modele5 <- lm(formula = P3t ~ St + At_1 + St_1 ,offset = P3t_1 )

summary(modele1)
summary(modele2)
summary(modele3)
summary(modele4)
summary(modele5)

tableau_parametres_regions[3,1] <- "est"
tableau_parametres_regions[4,1] <- "est"
tableau_parametres_regions[3,2] <- 1
tableau_parametres_regions[4,2] <- 2
tableau_parametres_regions[3,3] <- modele1$coefficients[1]
tableau_parametres_regions[3,4] <- modele1$coefficients[2]
tableau_parametres_regions[3,5] <- modele1$coefficients[3]
tableau_parametres_regions[3,6] <- modele1$coefficients[4]
tableau_parametres_regions[3,7] <- modele1$coefficients[5]
tableau_parametres_regions[3,8] <- modele1$coefficients[6]
tableau_parametres_regions[3,9] <- summary.lm(modele1)$sigma
tableau_parametres_regions[4,3] <- modele2$coefficients[1]
tableau_parametres_regions[4,4] <- modele2$coefficients[2]
tableau_parametres_regions[4,5] <- modele2$coefficients[3]
tableau_parametres_regions[4,6] <- modele2$coefficients[4]
tableau_parametres_regions[4,7] <- modele2$coefficients[5]
tableau_parametres_regions[4,8] <- modele2$coefficients[6]
tableau_parametres_regions[4,9] <- summary.lm(modele2)$sigma

tableau_parametres_regions_predateurs[4,1] <- "est"
tableau_parametres_regions_predateurs[5,1] <- "est"
tableau_parametres_regions_predateurs[6,1] <- "est"
tableau_parametres_regions_predateurs[4,2] <- 3
tableau_parametres_regions_predateurs[5,2] <- 4
tableau_parametres_regions_predateurs[6,2] <- 5
tableau_parametres_regions_predateurs[4,3] <- modele3$coefficients[1]
tableau_parametres_regions_predateurs[4,4] <- modele3$coefficients[2]
tableau_parametres_regions_predateurs[4,5] <- modele3$coefficients[3]
tableau_parametres_regions_predateurs[4,6] <- modele3$coefficients[4]
tableau_parametres_regions_predateurs[4,7] <- summary.lm(modele3)$sigma
tableau_parametres_regions_predateurs[5,3] <- modele4$coefficients[1]
tableau_parametres_regions_predateurs[5,4] <- modele4$coefficients[2]
tableau_parametres_regions_predateurs[5,5] <- modele4$coefficients[3]
tableau_parametres_regions_predateurs[5,6] <- modele4$coefficients[4]
tableau_parametres_regions_predateurs[5,7] <- summary.lm(modele4)$sigma
tableau_parametres_regions_predateurs[6,3] <- modele5$coefficients[1]
tableau_parametres_regions_predateurs[6,4] <- modele5$coefficients[2]
tableau_parametres_regions_predateurs[6,5] <- modele5$coefficients[3]
tableau_parametres_regions_predateurs[6,6] <- modele5$coefficients[4]
tableau_parametres_regions_predateurs[6,7] <- summary.lm(modele5)$sigma

# C) Pour l'ouest

donnees_ouest <- filter(donnees,cardinalite=="sud_ouest")

Present <-filter(donnees_ouest,year>1990)# on ne prend pas les 2 premières années 
At    <-Present$Vole.Autumn
St    <-Present$Vole.Spring
P1t   <-Present$Small.mustelid 
P2t   <-Present$Generalist.predator
P3t   <-Present$Avian.predator
Passe_1 <-filter(donnees_ouest,year>1989,year<2011) # on ne prend ni la première ni la dernière année 
At_1    <-Passe_1$Vole.Autumn
St_1    <-Passe_1$Vole.Spring
P1t_1   <-Passe_1$Small.mustelid
P2t_1   <-Passe_1$Generalist.predator
P3t_1   <-Passe_1$Avian.predator
Passe_2 <-filter(donnees_ouest,year<2010) # on ne prend pas les 2 dernières années  
At_2    <-Passe_2$Vole.Autumn

modele1 <- lm(formula = St ~ At_1 + At_2 + P1t + P2t + P3t_1 ,offset = At_1 )
modele2 <- lm(formula = At ~ St + St_1 + P1t + P2t + P3t ,offset = St   )
modele3 <- lm(formula = P1t ~ At_1 + St_1 + At_2 ,offset = P1t_1 )
modele4 <- lm(formula = P2t ~ At_1 + St_1 + At_2 ,offset = P2t_1 )
modele5 <- lm(formula = P3t ~ St + At_1 + St_1 ,offset = P3t_1 )

summary(modele1)
summary(modele2)
summary(modele3)
summary(modele4)
summary(modele5)

tableau_parametres_regions[5,1] <- "ouest"
tableau_parametres_regions[6,1] <- "ouest"
tableau_parametres_regions[5,2] <- 1
tableau_parametres_regions[6,2] <- 2
tableau_parametres_regions[5,3] <- modele1$coefficients[1]
tableau_parametres_regions[5,4] <- modele1$coefficients[2]
tableau_parametres_regions[5,5] <- modele1$coefficients[3]
tableau_parametres_regions[5,6] <- modele1$coefficients[4]
tableau_parametres_regions[5,7] <- modele1$coefficients[5]
tableau_parametres_regions[5,8] <- modele1$coefficients[6]
tableau_parametres_regions[5,9] <- summary.lm(modele1)$sigma
tableau_parametres_regions[6,3] <- modele2$coefficients[1]
tableau_parametres_regions[6,4] <- modele2$coefficients[2]
tableau_parametres_regions[6,5] <- modele2$coefficients[3]
tableau_parametres_regions[6,6] <- modele2$coefficients[4]
tableau_parametres_regions[6,7] <- modele2$coefficients[5]
tableau_parametres_regions[6,8] <- modele2$coefficients[6]
tableau_parametres_regions[6,9] <- summary.lm(modele2)$sigma

tableau_parametres_regions_predateurs[7,1] <- "ouest"
tableau_parametres_regions_predateurs[8,1] <- "ouest"
tableau_parametres_regions_predateurs[9,1] <- "ouest"
tableau_parametres_regions_predateurs[7,2] <- 3
tableau_parametres_regions_predateurs[8,2] <- 4
tableau_parametres_regions_predateurs[9,2] <- 5
tableau_parametres_regions_predateurs[7,3] <- modele3$coefficients[1]
tableau_parametres_regions_predateurs[7,4] <- modele3$coefficients[2]
tableau_parametres_regions_predateurs[7,5] <- modele3$coefficients[3]
tableau_parametres_regions_predateurs[7,6] <- modele3$coefficients[4]
tableau_parametres_regions_predateurs[7,7] <- summary.lm(modele3)$sigma
tableau_parametres_regions_predateurs[8,3] <- modele4$coefficients[1]
tableau_parametres_regions_predateurs[8,4] <- modele4$coefficients[2]
tableau_parametres_regions_predateurs[8,5] <- modele4$coefficients[3]
tableau_parametres_regions_predateurs[8,6] <- modele4$coefficients[4]
tableau_parametres_regions_predateurs[8,7] <- summary.lm(modele4)$sigma
tableau_parametres_regions_predateurs[9,3] <- modele5$coefficients[1]
tableau_parametres_regions_predateurs[9,4] <- modele5$coefficients[2]
tableau_parametres_regions_predateurs[9,5] <- modele5$coefficients[3]
tableau_parametres_regions_predateurs[9,6] <- modele5$coefficients[4]
tableau_parametres_regions_predateurs[9,7] <- summary.lm(modele5)$sigma


# enregistrement du tableau 

write.csv (tableau_parametres_regions, "parametres_modeles_region.csv", row.names = T, quote = F)
write.csv (tableau_parametres_regions_predateurs, "parametres_modeles_region_predateurs.csv", row.names = T, quote = F)

#------------------------------------------------------------------------------------------------------------------------------------------


# II Calculs des variances et variances expliquées pour les campagnols 

# on ouvre le vecteur contenant les paramètres 

parametres <- read.csv("parametres_modeles_region.csv", sep=",", header=T, dec=".",stringsAsFactors=FALSE)

# on va créer nos vecteurs pour calculer les variances et covariances 

Stn <- data.frame(matrix(0,23,33))
Atn <- data.frame(matrix(0,23,33))
P1tn <- data.frame(matrix(0,23,33))
P2tn <- data.frame(matrix(0,23,33))
P3tn <- data.frame(matrix(0,23,33))

# on les remplit des valeurs de nos données , une ligne par année et une colonne par localisation 

for (i in 0:32){ # on parcourt les localisations 
  for(j in 1:23){ # on parcourt les années 
    Stn[j,i+1] <- donnees[23*i+j,4]
    Atn[j,i+1] <- donnees[23*i+j,5]
    P1tn[j,i+1] <- donnees[23*i+j,6]
    P2tn[j,i+1] <- donnees[23*i+j,7]
    P3tn[j,i+1] <- donnees[23*i+j,8]
    
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

# A) pour le nord 

# on va créer le tableau qui va stocker les variances et les variances expliquées

tableau_var_nord <- data.frame(matrix(0,21,13))
names(tableau_var_nord)<-c("annee","VarY1","dir1","del1","P11","P21","P31","VarY2","dir2","del2","P12","P22","P32")


# les paramètres prennent les valeurs de celles estimées pour le nord 

a1 <- parametres[1,4]
b1 <- parametres[1,5]
c1 <- parametres[1,6]
d1 <- parametres[1,7]
e1 <- parametres[1,8]
f1 <- parametres[1,9]
sigma_carre_1 <- parametres[1,10]^2

a2 <- parametres[2,4]
b2 <- parametres[2,5]
c2 <- parametres[2,6]
d2 <- parametres[2,7]
e2 <- parametres[2,8]
f2 <- parametres[2,9]
sigma_carre_2 <- parametres[2,10]^2


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
  
  tableau_var_nord[i,2] <- DD1+DL1+P11+P21+P31+sigma_carre_1
  tableau_var_nord[i,3] <- DD1/tableau_var_nord[i,2] # variances expliquees
  tableau_var_nord[i,4] <- DL1/tableau_var_nord[i,2]
  tableau_var_nord[i,5] <- P11/tableau_var_nord[i,2]
  tableau_var_nord[i,6] <- P21/tableau_var_nord[i,2]
  tableau_var_nord[i,7] <- P31/tableau_var_nord[i,2]
  
  DD2 <- b2*(b2*var(St,na.rm = TRUE)+c2*cov(St,St_1,use="pairwise.complete.obs")+d2*cov(St,P1t,use="pairwise.complete.obs")+e2*cov(St,P2t,use="pairwise.complete.obs")+f2*cov(St,P3t,use="pairwise.complete.obs"))
  DL2 <- c2*(c2*var(St_1,na.rm = TRUE)+b2*cov(St,St_1,use="pairwise.complete.obs")+d2*cov(St_1,P1t,use="pairwise.complete.obs")+e2*cov(St_1,P2t,use="pairwise.complete.obs")+f2*cov(St_1,P3t,use="pairwise.complete.obs"))
  P12 <- d2*(d2*var(P1t,na.rm = TRUE)+b2*cov(St,P1t,use="pairwise.complete.obs")+c2*cov(St_1,P1t,use="pairwise.complete.obs")+e2*cov(P1t,P2t,use="pairwise.complete.obs")+f2*cov(P1t,P3t,use="pairwise.complete.obs"))
  P22 <- e2*(e2*var(P2t,na.rm = TRUE)+b2*cov(St,P2t,use="pairwise.complete.obs")+c2*cov(St_1,P2t,use="pairwise.complete.obs")+d2*cov(P1t,P2t,use="pairwise.complete.obs")+f2*cov(P2t,P3t,use="pairwise.complete.obs"))
  P32 <- f2*(f2*var(P3t,na.rm = TRUE)+b2*cov(St,P3t,use="pairwise.complete.obs")+c2*cov(St_1,P3t,use="pairwise.complete.obs")+d2*cov(P1t,P3t,use="pairwise.complete.obs")+e2*cov(P2t,P3t,use="pairwise.complete.obs"))
  
  tableau_var_nord[i,8] <-  DD2+DL2+P12+P22+P32+sigma_carre_2 
  tableau_var_nord[i,9] <-  DD2/tableau_var_nord[i,8]# variances expliquées
  tableau_var_nord[i,10] <- DL2/tableau_var_nord[i,8]
  tableau_var_nord[i,11] <- P12/tableau_var_nord[i,8]
  tableau_var_nord[i,12] <- P22/tableau_var_nord[i,8]
  tableau_var_nord[i,13] <- P32/tableau_var_nord[i,8]
}

# enregistrement des resultats 

write.csv (tableau_var_nord, "variances_expliquees_nord2.csv", row.names = T, quote = F) 

# B) pour l'est

# on va créer le tableau qui va stocker les variances et les variances expliquées

tableau_var_est <- data.frame(matrix(0,21,13))
names(tableau_var_est)<-c("annee","VarY1","dir1","del1","P11","P21","P31","VarY2","dir2","del2","P12","P22","P32")


# les paramètres prennent les valeurs de celles estimées pour le nord 

a1 <- parametres[3,4]
b1 <- parametres[3,5]
c1 <- parametres[3,6]
d1 <- parametres[3,7]
e1 <- parametres[3,8]
f1 <- parametres[3,9]
sigma_carre_1 <- parametres[3,10]^2

a2 <- parametres[4,4]
b2 <- parametres[4,5]
c2 <- parametres[4,6]
d2 <- parametres[4,7]
e2 <- parametres[4,8]
f2 <- parametres[4,9]
sigma_carre_2 <- parametres[4,10]^2


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
  
  tableau_var_est[i,2] <- DD1+DL1+P11+P21+P31+sigma_carre_1 
  tableau_var_est[i,3] <- DD1/tableau_var_est[i,2] # variances expliquees
  tableau_var_est[i,4] <- DL1/tableau_var_est[i,2]
  tableau_var_est[i,5] <- P11/tableau_var_est[i,2]
  tableau_var_est[i,6] <- P21/tableau_var_est[i,2]
  tableau_var_est[i,7] <- P31/tableau_var_est[i,2]
  
  DD2 <- b2*(b2*var(St,na.rm = TRUE)+c2*cov(St,St_1,use="pairwise.complete.obs")+d2*cov(St,P1t,use="pairwise.complete.obs")+e2*cov(St,P2t,use="pairwise.complete.obs")+f2*cov(St,P3t,use="pairwise.complete.obs"))
  DL2 <- c2*(c2*var(St_1,na.rm = TRUE)+b2*cov(St,St_1,use="pairwise.complete.obs")+d2*cov(St_1,P1t,use="pairwise.complete.obs")+e2*cov(St_1,P2t,use="pairwise.complete.obs")+f2*cov(St_1,P3t,use="pairwise.complete.obs"))
  P12 <- d2*(d2*var(P1t,na.rm = TRUE)+b2*cov(St,P1t,use="pairwise.complete.obs")+c2*cov(St_1,P1t,use="pairwise.complete.obs")+e2*cov(P1t,P2t,use="pairwise.complete.obs")+f2*cov(P1t,P3t,use="pairwise.complete.obs"))
  P22 <- e2*(e2*var(P2t,na.rm = TRUE)+b2*cov(St,P2t,use="pairwise.complete.obs")+c2*cov(St_1,P2t,use="pairwise.complete.obs")+d2*cov(P1t,P2t,use="pairwise.complete.obs")+f2*cov(P2t,P3t,use="pairwise.complete.obs"))
  P32 <- f2*(f2*var(P3t,na.rm = TRUE)+b2*cov(St,P3t,use="pairwise.complete.obs")+c2*cov(St_1,P3t,use="pairwise.complete.obs")+d2*cov(P1t,P3t,use="pairwise.complete.obs")+e2*cov(P2t,P3t,use="pairwise.complete.obs"))
  
  tableau_var_est[i,8] <-  DD2+DL2+P12+P22+P32+sigma_carre_2  
  tableau_var_est[i,9] <-  DD2/tableau_var_est[i,8] # variances expliquées
  tableau_var_est[i,10] <- DL2/tableau_var_est[i,8]
  tableau_var_est[i,11] <- P12/tableau_var_est[i,8]
  tableau_var_est[i,12] <- P22/tableau_var_est[i,8]
  tableau_var_est[i,13] <- P32/tableau_var_est[i,8]
}

# enregistrement des resultats 

write.csv (tableau_var_est, "variances_expliquees_est2.csv", row.names = T, quote = F) 

# C) pour l'ouest 

# on va créer le tableau qui va stocker les variances et les variances expliquées

tableau_var_ouest <- data.frame(matrix(0,21,13))
names(tableau_var_ouest)<-c("annee","VarY1","dir1","del1","P11","P21","P31","VarY2","dir2","del2","P12","P22","P32")


# les paramètres prennent les valeurs de celles estimées pour le nord 

a1 <- parametres[5,4]
b1 <- parametres[5,5]
c1 <- parametres[5,6]
d1 <- parametres[5,7]
e1 <- parametres[5,8]
f1 <- parametres[5,9]
sigma_carre_1 <- parametres[5,10]^2

a2 <- parametres[6,4]
b2 <- parametres[6,5]
c2 <- parametres[6,6]
d2 <- parametres[6,7]
e2 <- parametres[6,8]
f2 <- parametres[6,9]
sigma_carre_2 <- parametres[6,10]^2


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
  
  tableau_var_ouest[i,2] <- DD1+DL1+P11+P21+P31+sigma_carre_1 
  tableau_var_ouest[i,3] <- DD1/tableau_var_ouest[i,2] # variances expliquees
  tableau_var_ouest[i,4] <- DL1/tableau_var_ouest[i,2]
  tableau_var_ouest[i,5] <- P11/tableau_var_ouest[i,2]
  tableau_var_ouest[i,6] <- P21/tableau_var_ouest[i,2]
  tableau_var_ouest[i,7] <- P31/tableau_var_ouest[i,2]
  
  DD2 <- b2*(b2*var(St,na.rm = TRUE)+c2*cov(St,St_1,use="pairwise.complete.obs")+d2*cov(St,P1t,use="pairwise.complete.obs")+e2*cov(St,P2t,use="pairwise.complete.obs")+f2*cov(St,P3t,use="pairwise.complete.obs"))
  DL2 <- c2*(c2*var(St_1,na.rm = TRUE)+b2*cov(St,St_1,use="pairwise.complete.obs")+d2*cov(St_1,P1t,use="pairwise.complete.obs")+e2*cov(St_1,P2t,use="pairwise.complete.obs")+f2*cov(St_1,P3t,use="pairwise.complete.obs"))
  P12 <- d2*(d2*var(P1t,na.rm = TRUE)+b2*cov(St,P1t,use="pairwise.complete.obs")+c2*cov(St_1,P1t,use="pairwise.complete.obs")+e2*cov(P1t,P2t,use="pairwise.complete.obs")+f2*cov(P1t,P3t,use="pairwise.complete.obs"))
  P22 <- e2*(e2*var(P2t,na.rm = TRUE)+b2*cov(St,P2t,use="pairwise.complete.obs")+c2*cov(St_1,P2t,use="pairwise.complete.obs")+d2*cov(P1t,P2t,use="pairwise.complete.obs")+f2*cov(P2t,P3t,use="pairwise.complete.obs"))
  P32 <- f2*(f2*var(P3t,na.rm = TRUE)+b2*cov(St,P3t,use="pairwise.complete.obs")+c2*cov(St_1,P3t,use="pairwise.complete.obs")+d2*cov(P1t,P3t,use="pairwise.complete.obs")+e2*cov(P2t,P3t,use="pairwise.complete.obs"))
  
  tableau_var_ouest[i,8] <-  DD2+DL2+P12+P22+P32+sigma_carre_2
  tableau_var_ouest[i,9] <-  DD2/tableau_var_ouest[i,8] # variances expliquées
  tableau_var_ouest[i,10] <- DL2/tableau_var_ouest[i,8]
  tableau_var_ouest[i,11] <- P12/tableau_var_ouest[i,8]
  tableau_var_ouest[i,12] <- P22/tableau_var_ouest[i,8]
  tableau_var_ouest[i,13] <- P32/tableau_var_ouest[i,8]
}

# enregistrement des resultats 

write.csv (tableau_var_ouest, "variances_expliquees_ouest2.csv", row.names = T, quote = F) 

#-----------------------------------------------------------------------------------------------------------------------------------------------------------------------

# III Calculs des variances et variances expliquées pour les prédateurs 

# on ouvre le vecteur contenant les paramètres 

parametres <- read.csv("parametres_modeles_region_predateurs.csv", sep=",", header=T, dec=".",stringsAsFactors=FALSE)

# on va créer nos vecteurs pour calculer les variances et covariances 

Stn <- data.frame(matrix(0,23,33))
Atn <- data.frame(matrix(0,23,33))

# on les remplit des valeurs de nos données , une ligne par année et une colonne par localisation 

for (i in 0:32){ # on parcourt les localisations 
  for(j in 1:23){ # on parcourt les années 
    Stn[j,i+1] <- donnees[23*i+j,4]
    Atn[j,i+1] <- donnees[23*i+j,5]
    
  }
}

# on crée le décalage temporel 

Atn_ <- Atn[-c(1,2),] #present 
Atn_1 <- Atn[-c(1,23),] #passé1
Atn_2 <- Atn[-c(22,23),] #passé2
Stn_ <- Stn[-c(1,2),] #present 
Stn_1 <- Stn[-c(1,23),] #passé1

# A) pour le nord 

# on va créer le tableau qui va stocker les variances et les variances expliquées

tableau_var_nord_pred <- data.frame(matrix(0,21,13))
names(tableau_var_nord_pred)<-c("annee","VarY3","a_1_3","s_1_3","a_2_3","VarY4","a_1_4","s_1_4","a_2_4","VarY5","s_5","a_1_5","s_1_5")


# les paramètres prennent les valeurs de celles estimées pour le nord 

a3 <- parametres[1,4]
b3 <- parametres[1,5]
c3 <- parametres[1,6]
d3 <- parametres[1,7]
sigma_carre_3 <- parametres[1,8]^2

a4 <- parametres[2,4]
b4 <- parametres[2,5]
c4 <- parametres[2,6]
d4 <- parametres[2,7]
sigma_carre_4 <- parametres[2,8]^2

a5 <- parametres[3,4]
b5 <- parametres[3,5]
c5 <- parametres[3,6]
d5 <- parametres[3,7]
sigma_carre_5 <- parametres[3,8]^2


# boucle pour remplir le tableau 

for (i in 1:21){ # les i parcourent les années 
  At <- Atn_[i,]
  At_1 <- Atn_1[i,]
  At_2 <- Atn_2[i,]
  St <- Stn_[i,]
  St_1 <- Stn_1[i,]
  
  At <- as.numeric(At)
  At_1 <- as.numeric(At_1)
  At_2 <- as.numeric(At_2)
  St <-  as.numeric(St)
  St_1 <- as.numeric(St_1)
  
  tableau_var_nord_pred[i,1] <-1990 + i
  
  V13 <- b3*(b3*var(At_1,na.rm = TRUE)+c3*cov(At_1,St_1,use="pairwise.complete.obs")+d3*cov(At_1,At_2,use="pairwise.complete.obs"))
  V23 <- c3*(c3*var(St_1,na.rm = TRUE)+b3*cov(At_1,St_1,use="pairwise.complete.obs")+d3*cov(At_2,St_1,use="pairwise.complete.obs"))
  V33 <- d3*(d3*var(At_2,na.rm = TRUE)+b3*cov(At_1,At_2,use="pairwise.complete.obs")+c3*cov(At_2,St_1,use="pairwise.complete.obs"))
  
  tableau_var_nord_pred[i,2] <- V13+V23+V33+sigma_carre_3
  tableau_var_nord_pred[i,3] <- V13/tableau_var_nord_pred[i,2] 
  tableau_var_nord_pred[i,4] <- V23/tableau_var_nord_pred[i,2]
  tableau_var_nord_pred[i,5] <- V33/tableau_var_nord_pred[i,2]
  
  V14 <- b4*(b4*var(At_1,na.rm = TRUE)+c4*cov(At_1,St_1,use="pairwise.complete.obs")+d4*cov(At_1,At_2,use="pairwise.complete.obs"))
  V24 <- c4*(c4*var(St_1,na.rm = TRUE)+b4*cov(At_1,St_1,use="pairwise.complete.obs")+d4*cov(At_2,St_1,use="pairwise.complete.obs"))
  V34 <- d4*(d4*var(At_2,na.rm = TRUE)+b4*cov(At_1,At_2,use="pairwise.complete.obs")+c4*cov(At_2,St_1,use="pairwise.complete.obs"))
  
  tableau_var_nord_pred[i,6] <- V14+V24+V34+sigma_carre_4
  tableau_var_nord_pred[i,7] <- V14/tableau_var_nord_pred[i,6] 
  tableau_var_nord_pred[i,8] <- V24/tableau_var_nord_pred[i,6]
  tableau_var_nord_pred[i,9] <- V34/tableau_var_nord_pred[i,6]
  
  V15 <- b5*(b5*var(St,na.rm = TRUE)+c5*cov(St,At_1,use="pairwise.complete.obs")+d5*cov(St,St_1,use="pairwise.complete.obs"))
  V25 <- c5*(c5*var(At_1,na.rm = TRUE)+b5*cov(St,At_1,use="pairwise.complete.obs")+d5*cov(St_1,At_1,use="pairwise.complete.obs"))
  V35 <- d5*(d5*var(St_1,na.rm = TRUE)+b5*cov(St,St_1,use="pairwise.complete.obs")+c5*cov(St_1,At_1,use="pairwise.complete.obs"))
  
  tableau_var_nord_pred[i,10] <- V15+V25+V35+sigma_carre_5
  tableau_var_nord_pred[i,11] <- V15/tableau_var_nord_pred[i,10] 
  tableau_var_nord_pred[i,12] <- V25/tableau_var_nord_pred[i,10]
  tableau_var_nord_pred[i,13] <- V35/tableau_var_nord_pred[i,10]

}

# enregistrement des resultats 

write.csv (tableau_var_nord_pred, "variances_expliquees_nord_pred.csv", row.names = T, quote = F) 

# B) pour l'est

# on va créer le tableau qui va stocker les variances et les variances expliquées

tableau_var_est_pred <- data.frame(matrix(0,21,13))
names(tableau_var_est_pred)<-c("annee","VarY3","a_1_3","s_1_3","a_2_3","VarY4","a_1_4","s_1_4","a_2_4","VarY5","s_5","a_1_5","s_1_5")


# les paramètres prennent les valeurs de celles estimées pour le nord 

a3 <- parametres[4,4]
b3 <- parametres[4,5]
c3 <- parametres[4,6]
d3 <- parametres[4,7]
sigma_carre_3 <- parametres[4,8]^2

a4 <- parametres[5,4]
b4 <- parametres[5,5]
c4 <- parametres[5,6]
d4 <- parametres[5,7]
sigma_carre_4 <- parametres[5,8]^2

a5 <- parametres[6,4]
b5 <- parametres[6,5]
c5 <- parametres[6,6]
d5 <- parametres[6,7]
sigma_carre_5 <- parametres[6,8]^2


# boucle pour remplir le tableau 

for (i in 1:21){ # les i parcourent les années 
  At <- Atn_[i,]
  At_1 <- Atn_1[i,]
  At_2 <- Atn_2[i,]
  St <- Stn_[i,]
  St_1 <- Stn_1[i,]
  
  At <- as.numeric(At)
  At_1 <- as.numeric(At_1)
  At_2 <- as.numeric(At_2)
  St <-  as.numeric(St)
  St_1 <- as.numeric(St_1)
  
  tableau_var_est_pred[i,1] <-1990 + i
  
  V13 <- b3*(b3*var(At_1,na.rm = TRUE)+c3*cov(At_1,St_1,use="pairwise.complete.obs")+d3*cov(At_1,At_2,use="pairwise.complete.obs"))
  V23 <- c3*(c3*var(St_1,na.rm = TRUE)+b3*cov(At_1,St_1,use="pairwise.complete.obs")+d3*cov(At_2,St_1,use="pairwise.complete.obs"))
  V33 <- d3*(d3*var(At_2,na.rm = TRUE)+b3*cov(At_1,At_2,use="pairwise.complete.obs")+c3*cov(At_2,St_1,use="pairwise.complete.obs"))
  
  tableau_var_est_pred[i,2] <- V13+V23+V33+sigma_carre_3
  tableau_var_est_pred[i,3] <- V13/tableau_var_est_pred[i,2] 
  tableau_var_est_pred[i,4] <- V23/tableau_var_est_pred[i,2]
  tableau_var_est_pred[i,5] <- V33/tableau_var_est_pred[i,2]
  
  V14 <- b4*(b4*var(At_1,na.rm = TRUE)+c4*cov(At_1,St_1,use="pairwise.complete.obs")+d4*cov(At_1,At_2,use="pairwise.complete.obs"))
  V24 <- c4*(c4*var(St_1,na.rm = TRUE)+b4*cov(At_1,St_1,use="pairwise.complete.obs")+d4*cov(At_2,St_1,use="pairwise.complete.obs"))
  V34 <- d4*(d4*var(At_2,na.rm = TRUE)+b4*cov(At_1,At_2,use="pairwise.complete.obs")+c4*cov(At_2,St_1,use="pairwise.complete.obs"))
  
  tableau_var_est_pred[i,6] <- V14+V24+V34+sigma_carre_4
  tableau_var_est_pred[i,7] <- V14/tableau_var_est_pred[i,6] 
  tableau_var_est_pred[i,8] <- V24/tableau_var_est_pred[i,6]
  tableau_var_est_pred[i,9] <- V34/tableau_var_est_pred[i,6]
  
  V15 <- b5*(b5*var(St,na.rm = TRUE)+c5*cov(St,At_1,use="pairwise.complete.obs")+d5*cov(St,St_1,use="pairwise.complete.obs"))
  V25 <- c5*(c5*var(At_1,na.rm = TRUE)+b5*cov(St,At_1,use="pairwise.complete.obs")+d5*cov(St_1,At_1,use="pairwise.complete.obs"))
  V35 <- d5*(d5*var(St_1,na.rm = TRUE)+b5*cov(St,St_1,use="pairwise.complete.obs")+c5*cov(St_1,At_1,use="pairwise.complete.obs"))
  
  tableau_var_est_pred[i,10] <- V15+V25+V35+sigma_carre_5
  tableau_var_est_pred[i,11] <- V15/tableau_var_est_pred[i,10] 
  tableau_var_est_pred[i,12] <- V25/tableau_var_est_pred[i,10]
  tableau_var_est_pred[i,13] <- V35/tableau_var_est_pred[i,10]
  
}

# enregistrement des resultats 

write.csv (tableau_var_est_pred, "variances_expliquees_est_pred.csv", row.names = T, quote = F) 

# C) pour l'ouest

# on va créer le tableau qui va stocker les variances et les variances expliquées

tableau_var_ouest_pred <- data.frame(matrix(0,21,13))
names(tableau_var_ouest_pred)<-c("annee","VarY3","a_1_3","s_1_3","a_2_3","VarY4","a_1_4","s_1_4","a_2_4","VarY5","s_5","a_1_5","s_1_5")


# les paramètres prennent les valeurs de celles estimées pour le nord 

a3 <- parametres[7,4]
b3 <- parametres[7,5]
c3 <- parametres[7,6]
d3 <- parametres[7,7]
sigma_carre_3 <- parametres[7,8]^2

a4 <- parametres[8,4]
b4 <- parametres[8,5]
c4 <- parametres[8,6]
d4 <- parametres[8,7]
sigma_carre_4 <- parametres[8,8]^2

a5 <- parametres[9,4]
b5 <- parametres[9,5]
c5 <- parametres[9,6]
d5 <- parametres[9,7]
sigma_carre_5 <- parametres[9,8]^2


# boucle pour remplir le tableau 

for (i in 1:21){ # les i parcourent les années 
  At <- Atn_[i,]
  At_1 <- Atn_1[i,]
  At_2 <- Atn_2[i,]
  St <- Stn_[i,]
  St_1 <- Stn_1[i,]
  
  At <- as.numeric(At)
  At_1 <- as.numeric(At_1)
  At_2 <- as.numeric(At_2)
  St <-  as.numeric(St)
  St_1 <- as.numeric(St_1)
  
  tableau_var_ouest_pred[i,1] <-1990 + i
  
  V13 <- b3*(b3*var(At_1,na.rm = TRUE)+c3*cov(At_1,St_1,use="pairwise.complete.obs")+d3*cov(At_1,At_2,use="pairwise.complete.obs"))
  V23 <- c3*(c3*var(St_1,na.rm = TRUE)+b3*cov(At_1,St_1,use="pairwise.complete.obs")+d3*cov(At_2,St_1,use="pairwise.complete.obs"))
  V33 <- d3*(d3*var(At_2,na.rm = TRUE)+b3*cov(At_1,At_2,use="pairwise.complete.obs")+c3*cov(At_2,St_1,use="pairwise.complete.obs"))
  
  tableau_var_ouest_pred[i,2] <- V13+V23+V33+sigma_carre_3
  tableau_var_ouest_pred[i,3] <- V13/tableau_var_ouest_pred[i,2] 
  tableau_var_ouest_pred[i,4] <- V23/tableau_var_ouest_pred[i,2]
  tableau_var_ouest_pred[i,5] <- V33/tableau_var_ouest_pred[i,2]
  
  V14 <- b4*(b4*var(At_1,na.rm = TRUE)+c4*cov(At_1,St_1,use="pairwise.complete.obs")+d4*cov(At_1,At_2,use="pairwise.complete.obs"))
  V24 <- c4*(c4*var(St_1,na.rm = TRUE)+b4*cov(At_1,St_1,use="pairwise.complete.obs")+d4*cov(At_2,St_1,use="pairwise.complete.obs"))
  V34 <- d4*(d4*var(At_2,na.rm = TRUE)+b4*cov(At_1,At_2,use="pairwise.complete.obs")+c4*cov(At_2,St_1,use="pairwise.complete.obs"))
  
  tableau_var_ouest_pred[i,6] <- V14+V24+V34+sigma_carre_4
  tableau_var_ouest_pred[i,7] <- V14/tableau_var_ouest_pred[i,6] 
  tableau_var_ouest_pred[i,8] <- V24/tableau_var_ouest_pred[i,6]
  tableau_var_ouest_pred[i,9] <- V34/tableau_var_ouest_pred[i,6]
  
  V15 <- b5*(b5*var(St,na.rm = TRUE)+c5*cov(St,At_1,use="pairwise.complete.obs")+d5*cov(St,St_1,use="pairwise.complete.obs"))
  V25 <- c5*(c5*var(At_1,na.rm = TRUE)+b5*cov(St,At_1,use="pairwise.complete.obs")+d5*cov(St_1,At_1,use="pairwise.complete.obs"))
  V35 <- d5*(d5*var(St_1,na.rm = TRUE)+b5*cov(St,St_1,use="pairwise.complete.obs")+c5*cov(St_1,At_1,use="pairwise.complete.obs"))
  
  tableau_var_ouest_pred[i,10] <- V15+V25+V35+sigma_carre_5
  tableau_var_ouest_pred[i,11] <- V15/tableau_var_ouest_pred[i,10] 
  tableau_var_ouest_pred[i,12] <- V25/tableau_var_ouest_pred[i,10]
  tableau_var_ouest_pred[i,13] <- V35/tableau_var_ouest_pred[i,10]
  
}

# enregistrement des resultats 

write.csv (tableau_var_ouest_pred, "variances_expliquees_ouest_pred.csv", row.names = T, quote = F) 



