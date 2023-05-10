# Besognet Thomas, 10/05/23 , Stage : Simulations 

# packages 
install.packages("dplyr")
library(dplyr)

# Opening data 

donnees <- read.csv("donnees_propres_korpela.csv", sep=",", header=T, dec=".")

# Simulations 

# We create the data frame with data that we will produce 

simulations <-data.frame(matrix(0,24,10)) 
names(simulations) <- c("scenario","region","direct density dependance","delayed density dependance","s-index","min_s","max_s","seasonality","min_n","max_n")

Vole <- data.frame(matrix(0,2000,24)) 
names(Vole) <- c("1n","1o","1e","2n","2o","2e","3n","3o","3e","4n","4o","4e","5n","5o","5e","6n","6o","6e","7n","7o","7e","8n","8o","8e")
Vole_1 <- data.frame(matrix(0,2000,24)) 
names(Vole_1) <- c("1n","1o","1e","2n","2o","2e","3n","3o","3e","4n","4o","4e","5n","5o","5e","6n","6o","6e","7n","7o","7e","8n","8o","8e")
Yt <- data.frame(matrix(0,2000,24)) 
names(Yt) <- c("1n","1o","1e","2n","2o","2e","3n","3o","3e","4n","4o","4e","5n","5o","5e","6n","6o","6e","7n","7o","7e","8n","8o","8e")

# We open the parameters estimated previously and create variable to stock them

parametres <- read.csv("parametres_modeles_region.csv", sep=",", header=T, dec=".",stringsAsFactors=FALSE)
parametres_pred <- read.csv("parametres_modeles_region_predateurs.csv", sep=",", header=T, dec=".",stringsAsFactors=FALSE)

# north 

a1n <- parametres[1,4]
b1n <- parametres[1,5]
c1n <- parametres[1,6]
d1n <- parametres[1,7]
e1n <- parametres[1,8]
f1n <- parametres[1,9]
sigma1n <- parametres[1,10]

a2n <- parametres[2,4]
b2n <- parametres[2,5]
c2n <- parametres[2,6]
d2n <- parametres[2,7]
e2n <- parametres[2,8]
f2n <- parametres[2,9]
sigma2n <- parametres[2,10]

a3n <- parametres_pred[1,4]
b3n <- parametres_pred[1,5]
c3n <- parametres_pred[1,6]
d3n <- parametres_pred[1,7]
sigma3n <- parametres_pred[1,8]

a4n <- parametres_pred[2,4]
b4n <- parametres_pred[2,5]
c4n <- parametres_pred[2,6]
d4n <- parametres_pred[2,7]
sigma4n <- parametres_pred[2,8]

a5n <- parametres_pred[3,4]
b5n <- parametres_pred[3,5]
c5n <- parametres_pred[3,6]
d5n <- parametres_pred[3,7]
sigma5n <- parametres_pred[3,8]


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

# Now we can do the simulations for each region (3) and each scenario (8) , ie 24 simulations 

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
j <- 0

# creation des vecteurs de stockage des valeurs 
tableau_density_dependance <- data.frame(matrix(0,40,2))
tableau_s_index <- data.frame(matrix(0,40,1))
tableau_saisonalite <- data.frame(matrix(0,40,1))


# boucle sur 1000 ans , stocker les 1000 valeurs dans un tableau 

for (i in 1:1000){
  # on va calculer chacun de nos indices de densité dans l'ordre chronologique de l'année
  # attention : les indices ne sont pas les même que dans la forumle car ils évoluent en cours de route ( vérification faite sur papier )
  P1t <- a3n + P1t +b3n*At +c3n*St + d3n*At_1 + sigma3n # hiver 
  P2t <- a4n + P2t +b4n*At +c4n*St +d4n*At_1 +sigma4n
  St_1 <- St
  St <- a1n + (b1n+1)*At +c1n*At_1 +d1n*P1t +e1n*P2t + f1n*P3t +sigma1n # printemps 
  P3t <- a5n + P3t +b5n*St+c5n*At+d5n*St_1 +sigma5n # été 
  At_1 <- At 
  At <- a2n + (b2n+1)*St + c2n*St_1 +d2n*P1t +e2n*P2t +f2n*P3t + sigma2n # automne 
  # Stockage des valeurs de densités pour les campagnols 
  Vole[2*i-1,1] <- St
  Vole[2*i,1] <- At
  Vole_1[2*i-1,1] <- St_1
  Vole_1[2*i,1] <- At_1
  # on calcules les "growth rates" et la saisonnalité  qui seront utiles pour la suite 
  Y1 <- St-At_1
  Yt[2*i-1,1] <- Y1
  Y2 <- At-St
  Yt[2*i,1] <- Y2
  saisonalite <- saisonalite+(Y1-Y2)
  # tous les 25 ans, on passe une section 
  if (i %%25 ==0){
    j<- j+1
    # on calcule direct density dependance et delayed density dependance 
    Vole_section <- Vole[25*(j-1)+1:25*j,1]
    Vole_1_section <- Vole_1[25*(j-1)+1:25*j,1]
    Yt_section <- Yt[25*(j-1)+1:25*j,1]
    Density_dependance <- lm(formula = Yt_section ~ Vole_section+Vole_1_section, na.action=na.omit)
    tableau_density_dependance[j,1] <- summary(Density_dependance)$coefficient[1]
    tableau_density_dependance[j,2] <- summary(Density_dependance)$coefficient[2]
    # s index 
    s_index <- sqrt(var(Vole_section))
    tableau_s_index[j,1] <- s_index
    # saisonnalité 
    saisonalite_moyenne <- saisonalite/25
    tableau_saisonalite[j,1] <- saisonalite_moyenne
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

write.csv (Vole, "Vole_simulations.csv", row.names = T, quote = F) 


































# attention : corriger à partir d'ici 

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
    Density_dependance <- lm(formula = Yt ~ Vole+Vole_1, na.action=na.omit)
    tableau_density_dependance[i,1] <- summary(Density_dependance)$coefficient[1]
    tableau_density_dependance[i,2] <- summary(Density_dependance)$coefficient[2]
    # s index 
    s_index <- sqrt(Var(Vole))
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
    Density_dependance <- lm(formula = Yt ~ Vole+Vole_1, na.action=na.omit)
    tableau_density_dependance[i,1] <- summary(Density_dependance)$coefficient[1]
    tableau_density_dependance[i,2] <- summary(Density_dependance)$coefficient[2]
    # s index 
    s_index <- sqrt(Var(Vole))
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
    Density_dependance <- lm(formula = Yt ~ Vole+Vole_1, na.action=na.omit)
    tableau_density_dependance[i,1] <- summary(Density_dependance)$coefficient[1]
    tableau_density_dependance[i,2] <- summary(Density_dependance)$coefficient[2]
  # s index 
  s_index <- sqrt(Var(Vole))
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
    Density_dependance <- lm(formula = Yt ~ Vole+Vole_1, na.action=na.omit)
    tableau_density_dependance[i,1] <- summary(Density_dependance)$coefficient[1]
    tableau_density_dependance[i,2] <- summary(Density_dependance)$coefficient[2]
    # s index 
    s_index <- sqrt(Var(Vole))
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
    s_index <- sqrt(Var(Vole))
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
    Density_dependance <- lm(formula = Yt ~ Vole+Vole_1, na.action=na.omit)
    tableau_density_dependance[i,1] <- summary(Density_dependance)$coefficient[1]
    tableau_density_dependance[i,2] <- summary(Density_dependance)$coefficient[2]
    # s index 
    s_index <- sqrt(Var(Vole))
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
    Density_dependance <- lm(formula = Yt ~ Vole+Vole_1, na.action=na.omit)
    tableau_density_dependance[i,1] <- summary(Density_dependance)$coefficient[1]
    tableau_density_dependance[i,2] <- summary(Density_dependance)$coefficient[2]
    # s index 
    s_index <- sqrt(Var(Vole))
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
    Density_dependance <- lm(formula = Yt ~ Vole+Vole_1, na.action=na.omit)
    tableau_density_dependance[i,1] <- summary(Density_dependance)$coefficient[1]
    tableau_density_dependance[i,2] <- summary(Density_dependance)$coefficient[2]
    # s index 
    s_index <- sqrt(Var(Vole))
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
    Density_dependance <- lm(formula = Yt ~ Vole+Vole_1, na.action=na.omit)
    tableau_density_dependance[i,1] <- summary(Density_dependance)$coefficient[1]
    tableau_density_dependance[i,2] <- summary(Density_dependance)$coefficient[2]
    # s index 
    s_index <- sqrt(Var(Vole))
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
    Density_dependance <- lm(formula = Yt ~ Vole+Vole_1, na.action=na.omit)
    tableau_density_dependance[i,1] <- summary(Density_dependance)$coefficient[1]
    tableau_density_dependance[i,2] <- summary(Density_dependance)$coefficient[2]
    # s index 
    s_index <- sqrt(Var(Vole))
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
    Density_dependance <- lm(formula = Yt ~ Vole+Vole_1, na.action=na.omit)
    tableau_density_dependance[i,1] <- summary(Density_dependance)$coefficient[1]
    tableau_density_dependance[i,2] <- summary(Density_dependance)$coefficient[2]
    # s index 
    s_index <- sqrt(Var(Vole))
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
    Density_dependance <- lm(formula = Yt ~ Vole+Vole_1, na.action=na.omit)
    tableau_density_dependance[i,1] <- summary(Density_dependance)$coefficient[1]
    tableau_density_dependance[i,2] <- summary(Density_dependance)$coefficient[2]
    # s index 
    s_index <- sqrt(Var(Vole))
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
    Density_dependance <- lm(formula = Yt ~ Vole+Vole_1, na.action=na.omit)
    tableau_density_dependance[i,1] <- summary(Density_dependance)$coefficient[1]
    tableau_density_dependance[i,2] <- summary(Density_dependance)$coefficient[2]
    # s index 
    s_index <- sqrt(Var(Vole))
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
    Density_dependance <- lm(formula = Yt ~ Vole+Vole_1, na.action=na.omit)
    tableau_density_dependance[i,1] <- summary(Density_dependance)$coefficient[1]
    tableau_density_dependance[i,2] <- summary(Density_dependance)$coefficient[2]
    # s index 
    s_index <- sqrt(Var(Vole))
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
    Density_dependance <- lm(formula = Yt ~ Vole+Vole_1, na.action=na.omit)
    tableau_density_dependance[i,1] <- summary(Density_dependance)$coefficient[1]
    tableau_density_dependance[i,2] <- summary(Density_dependance)$coefficient[2]
    # s index 
    s_index <- sqrt(Var(Vole))
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
    Density_dependance <- lm(formula = Yt ~ Vole+Vole_1, na.action=na.omit)
    tableau_density_dependance[i,1] <- summary(Density_dependance)$coefficient[1]
    tableau_density_dependance[i,2] <- summary(Density_dependance)$coefficient[2]
    # s index 
    s_index <- sqrt(Var(Vole))
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
    Density_dependance <- lm(formula = Yt ~ Vole+Vole_1, na.action=na.omit)
    tableau_density_dependance[i,1] <- summary(Density_dependance)$coefficient[1]
    tableau_density_dependance[i,2] <- summary(Density_dependance)$coefficient[2]
    # s index 
    s_index <- sqrt(Var(Vole))
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
    Density_dependance <- lm(formula = Yt ~ Vole+Vole_1, na.action=na.omit)
    tableau_density_dependance[i,1] <- summary(Density_dependance)$coefficient[1]
    tableau_density_dependance[i,2] <- summary(Density_dependance)$coefficient[2]
    # s index 
    s_index <- sqrt(Var(Vole))
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
    Density_dependance <- lm(formula = Yt ~ Vole+Vole_1, na.action=na.omit)
    tableau_density_dependance[i,1] <- summary(Density_dependance)$coefficient[1]
    tableau_density_dependance[i,2] <- summary(Density_dependance)$coefficient[2]
    # s index 
    s_index <- sqrt(Var(Vole))
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
    Density_dependance <- lm(formula = Yt ~ Vole+Vole_1, na.action=na.omit)
    tableau_density_dependance[i,1] <- summary(Density_dependance)$coefficient[1]
    tableau_density_dependance[i,2] <- summary(Density_dependance)$coefficient[2]
    # s index 
    s_index <- sqrt(Var(Vole))
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
    Density_dependance <- lm(formula = Yt ~ Vole+Vole_1, na.action=na.omit)
    tableau_density_dependance[i,1] <- summary(Density_dependance)$coefficient[1]
    tableau_density_dependance[i,2] <- summary(Density_dependance)$coefficient[2]
    # s index 
    s_index <- sqrt(Var(Vole))
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
    Density_dependance <- lm(formula = Yt ~ Vole+Vole_1, na.action=na.omit)
    tableau_density_dependance[i,1] <- summary(Density_dependance)$coefficient[1]
    tableau_density_dependance[i,2] <- summary(Density_dependance)$coefficient[2]
    # s index 
    s_index <- sqrt(Var(Vole))
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
    Density_dependance <- lm(formula = Yt ~ Vole+Vole_1, na.action=na.omit)
    tableau_density_dependance[i,1] <- summary(Density_dependance)$coefficient[1]
    tableau_density_dependance[i,2] <- summary(Density_dependance)$coefficient[2]
    # s index 
    s_index <- sqrt(Var(Vole))
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













