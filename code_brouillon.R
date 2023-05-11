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

# lecture des données 

donnees <- read.csv("donnees_propres_korpela.csv", sep=",", header=T, dec=".")

# Tentative numéro 1 ( avec autant d'estimations des paramètres que de lieux puis moyennage )  

# Cration du  tableau qui va stocker les estimateurs 

estimateurs_tableau_full_set<-data.frame(matrix("NA",33,13))
names(estimateurs_tableau_full_set)<-c("site","a1","b1","c1","d1","f1","e1","a2","b2","c2","d2","e2","f2")

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

# A) pour le nord 

# on commence par créer le tableau qui va stocker les variances et les variances expliquées

tableau_var_nord <- data.frame(matrix("NA",21,13))
names(tableau_var_nord)<-c("annee","VarY1","dir1","del1","P11","P21","P31","VarY2","dir2","del2","P12","P22","P32")

# on récupère les données pour le nord 

donnees_nord <- filter(donnees,cardinalite=="nord")

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
  
  
# simulations 
  
  # east 
  
  a1e <- parametres[3,4]
b1e <- parametres[3,5]
c1e <- parametres[3,6]
d1e <- parametres[3,7]
e1e <- parametres[3,8]
f1e <- parametres[3,9]
sigma1e <- parametres[3,10]

a2e <- parametres[4,4]
b2e <- parametres[4,5]
c2e <- parametres[4,6]
d2e <- parametres[4,7]
e2e <- parametres[4,8]
f2e <- parametres[4,9]
sigma2e <- parametres[4,10]

a3e <- parametres_pred[4,4]
b3e <- parametres_pred[4,5]
c3e <- parametres_pred[4,6]
d3e <- parametres_pred[4,7]
sigma3e <- parametres_pred[4,8]

a4e <- parametres_pred[5,4]
b4e <- parametres_pred[5,5]
c4e <- parametres_pred[5,6]
d4e <- parametres_pred[5,7]
sigma4e <- parametres_pred[5,8]

a5e <- parametres_pred[6,4]
b5e <- parametres_pred[6,5]
c5e <- parametres_pred[6,6]
d5e <- parametres_pred[6,7]
sigma5e <- parametres_pred[6,8]

# west 

a1o <- parametres[5,4]
b1o <- parametres[5,5]
c1o <- parametres[5,6]
d1o <- parametres[5,7]
e1o <- parametres[5,8]
f1o <- parametres[5,9]
sigma1o <- parametres[5,10]

a2o <- parametres[6,4]
b2o <- parametres[6,5]
c2o <- parametres[6,6]
d2o <- parametres[6,7]
e2o <- parametres[6,8]
f2o <- parametres[6,9]
sigma2o <- parametres[6,10]

a3o <- parametres_pred[7,4]
b3o <- parametres_pred[7,5]
c3o <- parametres_pred[7,6]
d3o <- parametres_pred[7,7]
sigma3o <- parametres_pred[7,8]

a4o <- parametres_pred[8,4]
b4o <- parametres_pred[8,5]
c4o <- parametres_pred[8,6]
d4o <- parametres_pred[8,7]
sigma4o <- parametres_pred[8,8]

a5o <- parametres_pred[9,4]
b5o <- parametres_pred[9,5]
c5o <- parametres_pred[9,6]
d5o <- parametres_pred[9,7]
sigma5o <- parametres_pred[9,8]
