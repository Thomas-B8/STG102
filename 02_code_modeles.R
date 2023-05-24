
# author = "Besognet Thomas"
# date = " 23/05/23" 
# project = "Estimation d'intéractions entre espèces à partir de séries temporelles"
# name = "Models"

# packages 
install.packages("dplyr")
library(dplyr)

# We open clean data 
donnees <- read.csv("donnees_propres_korpela.csv", sep=",", header=T, dec=".")


#------------------------------------------------------------------------------------------------------------------------------------------


# I Parameter's estimation 

# We create two data frames, the first one for models 1 et 2 (vole) , the second for models 3,4 and 5 (Predators)
tableau_parametres_regions <- data.frame(matrix(0,6,9))
names(tableau_parametres_regions)<-c("region","modele","a","b","c","d","e","f","sigma")

tableau_parametres_regions_predateurs <- data.frame(matrix(0,9,7))
names(tableau_parametres_regions_predateurs)<-c("region","modele","a","b","c","d","sigma")


# parameters are estimated by regions, so we have to filter the data frame 
for (region in 1:3){
  if (region==1){
    donnees_region <- filter(donnees,cardinalite=="nord")
    card <- "north"
  }
  if (region==2){
    donnees_region <- filter(donnees,cardinalite=="est")
    card <- "east"
  }
  if (region==3){
    donnees_region <- filter(donnees,cardinalite=="sud_ouest")
    card <- "west"
  }
  
# then, we create vectors for each variables and each temporal laps 
  Present <-filter(donnees_region,year>1990)# on ne prend pas les 2 premières années 
  At    <-Present$Vole.Autumn
  St    <-Present$Vole.Spring
  P1t   <-Present$Small.mustelid 
  P2t   <-Present$Generalist.predator
  P3t   <-Present$Avian.predator
  Passe_1 <-filter(donnees_region,year>1989,year<2011) # on ne prend ni la première ni la dernière année 
  At_1    <-Passe_1$Vole.Autumn
  St_1    <-Passe_1$Vole.Spring
  P1t_1   <-Passe_1$Small.mustelid
  P2t_1   <-Passe_1$Generalist.predator
  P3t_1   <-Passe_1$Avian.predator
  Passe_2 <-filter(donnees_region,year<2010) # on ne prend pas les 2 dernières années  
  At_2    <-Passe_2$Vole.Autumn
  
# we can now estimate our five models 
  modele1 <- lm(formula = St ~ At_1 + At_2 + P1t + P2t + P3t_1 ,offset = At_1 )
  modele2 <- lm(formula = At ~ St + St_1 + P1t + P2t + P3t ,offset = St   )
  modele3 <- lm(formula = P1t ~ At_1 + St_1 + At_2 ,offset = P1t_1 )
  modele4 <- lm(formula = P2t ~ At_1 + St_1 + At_2 ,offset = P2t_1 )
  modele5 <- lm(formula = P3t ~ St + At_1 + St_1 ,offset = P3t_1 )
  
# and complete our data frames 
  # 1) the region's name
  tableau_parametres_regions[2*(region-1)+1,1] <- card
  tableau_parametres_regions[2*(region-1)+2,1] <- card 
  
  tableau_parametres_regions_predateurs[3*(region-1)+1,1] <- card
  tableau_parametres_regions_predateurs[3*(region-1)+2,1] <- card
  tableau_parametres_regions_predateurs[3*(region-1)+3,1] <- card 
  
  
  # 2) the model's name
  tableau_parametres_regions[2*(region-1)+1,2] <- 1
  tableau_parametres_regions[2*(region-1)+2,2] <- 2
  
  tableau_parametres_regions_predateurs[3*(region-1)+1,2] <- 3
  tableau_parametres_regions_predateurs[3*(region-1)+2,2] <- 4
  tableau_parametres_regions_predateurs[3*(region-1)+3,2] <- 5 
  
  # 3.1) parameters for models 1 and 2 
  for (j in 3:8) {
     
    
    tableau_parametres_regions[2*(region-1)+1,j] <- modele1$coefficients[j-2]
    tableau_parametres_regions[2*(region-1)+2,j] <- modele2$coefficients[j-2]
    
  }
  
  # 3.2) parameters for models 3,4 and 5  
  for (j in 3:6) {
    
    tableau_parametres_regions_predateurs[3*(region-1)+1,j] <- modele3$coefficients[j-2]
    tableau_parametres_regions_predateurs[3*(region-1)+2,j] <- modele4$coefficients[j-2]
    tableau_parametres_regions_predateurs[3*(region-1)+3,j] <- modele5$coefficients[j-2]
    
  }
  
  # 4) error's variance 
  tableau_parametres_regions[2*(region-1)+1,9] <- summary.lm(modele1)$sigma
  tableau_parametres_regions[2*(region-1)+2,9] <- summary.lm(modele2)$sigma
  
  tableau_parametres_regions_predateurs[3*(region-1)+1,7] <- summary.lm(modele3)$sigma
  tableau_parametres_regions_predateurs[3*(region-1)+2,7] <- summary.lm(modele4)$sigma
  tableau_parametres_regions_predateurs[3*(region-1)+3,7] <- summary.lm(modele5)$sigma

# we can end the region's loop 
}

# we can register our data frame in csv 
write.csv (tableau_parametres_regions, "parameters_vole.csv", row.names = T, quote = F)
write.csv (tableau_parametres_regions_predateurs, "parameters_predators.csv", row.names = T, quote = F)

#------------------------------------------------------------------------------------------------------------------------------------------

# II Variance and proportion of variance explained for Vole's models 

# we open our parameters 
parametres <- read.csv("parameters_vole.csv", sep=",", header=T, dec=".",stringsAsFactors=FALSE)

# we create our data frame for our results 
tableau_var<- data.frame(matrix(0,21,37))
names(tableau_var)<-c("annee","VarY1n","dir1n","del1n","P11n","P21n","P31n","VarY2n","dir2n","del2n","P12n","P22n","P32n",
                           "VarY1e","dir1e","del1e","P11e","P21e","P31e","VarY2e","dir2e","del2e","P12e","P22e","P32e",
                           "VarY1w","dir1w","del1w","P11w","P21w","P31w","VarY2w","dir2w","del2w","P12w","P22w","P32w")
                           


# 1) Creation of density's vectors for calculated variance in the whole data set 
# we create our five vectors 
Stn <- data.frame(matrix(0,23,33))
Atn <- data.frame(matrix(0,23,33))
P1tn <- data.frame(matrix(0,23,33))
P2tn <- data.frame(matrix(0,23,33))
P3tn <- data.frame(matrix(0,23,33))

# we can fill them, one line per year and one localisation per column 
for (i in 0:32){ 
  
  for(j in 1:23){ 
    
    Stn[j,i+1] <- donnees[23*i+j,4]
    Atn[j,i+1] <- donnees[23*i+j,5]
    P1tn[j,i+1] <- donnees[23*i+j,6]
    P2tn[j,i+1] <- donnees[23*i+j,7]
    P3tn[j,i+1] <- donnees[23*i+j,8]
    
  }
  
}

# we can create our temporal gap 
# no gap 
Atn_ <- Atn[-c(1,2),]  
Stn_ <- Stn[-c(1,2),] 
P1tn_ <- P1tn[-c(1,2),] 
P2tn_ <- P2tn[-c(1,2),] 
P3tn_ <- P3tn[-c(1,2),] 

# 1 year gap 
Atn_1 <- Atn[-c(1,23),] 
Stn_1 <- Stn[-c(1,23),] 
P3tn_1 <- P3tn[-c(1,23),]

# 2 years gap 
Atn_2 <- Atn[-c(22,23),] 


# 2) Creation of  our loop for regions 
for (region in 1:3){
  
  # we put our estimators in variables 
  a1 <- parametres[2*(region-1)+1,4]
  b1 <- parametres[2*(region-1)+1,5]
  c1 <- parametres[2*(region-1)+1,6]
  d1 <- parametres[2*(region-1)+1,7]
  e1 <- parametres[2*(region-1)+1,8]
  f1 <- parametres[2*(region-1)+1,9]
  sigma_carre_1 <- parametres[2*(region-1)+1,10]^2
  
  a2 <- parametres[2*(region-1)+2,4]
  b2 <- parametres[2*(region-1)+2,5]
  c2 <- parametres[2*(region-1)+2,6]
  d2 <- parametres[2*(region-1)+2,7]
  e2 <- parametres[2*(region-1)+2,8]
  f2 <- parametres[2*(region-1)+2,9]
  sigma_carre_2 <- parametres[2*(region-1)+2,10]^2
  
# we can create a loop to run through the years 
  for (i in 1:21){ 
    
  # our vectors keep only the data of year i
    At <- Atn_[i,]
    At_1 <- Atn_1[i,]
    At_2 <- Atn_2[i,]
    St <- Stn_[i,]
    St_1 <- Stn_1[i,]
    P1t <- P1tn_[i,]
    P2t <- P2tn_[i,]
    P3t <- P3tn_[i,]
    P3t_1 <- P3tn_1[i,]
    
    # we have to keep them on numeric format 
    At <- as.numeric(At)
    At_1 <- as.numeric(At_1)
    At_2 <- as.numeric(At_2)
    St <-  as.numeric(St)
    St_1 <- as.numeric(St_1)
    P1t <- as.numeric(P1t)
    P2t <- as.numeric(P2t)
    P3t <- as.numeric(P3t)
    P3t_1 <- as.numeric(P3t_1)
    
    # we add the year in the fors column 
    tableau_var[i,1] <-1990 + i
    
    # variance explained by each variables for model 1 
    DD1 <- b1*(b1*var(At_1,na.rm = TRUE)+c1*cov(At_1,At_2,use="pairwise.complete.obs")+d1*cov(At_1,P1t,use="pairwise.complete.obs")+e1*cov(At_1,P2t,use="pairwise.complete.obs")+f1*cov(At_1,P3t_1,use="pairwise.complete.obs"))
    DL1 <- c1*(c1*var(At_2,na.rm = TRUE)+b1*cov(At_1,At_2,use="pairwise.complete.obs")+d1*cov(At_2,P1t,use="pairwise.complete.obs")+e1*cov(At_2,P2t,use="pairwise.complete.obs")+f1*cov(At_2,P3t_1,use="pairwise.complete.obs"))
    P11 <- d1*(d1*var(P1t,na.rm = TRUE)+b1*cov(At_1,P1t,use="pairwise.complete.obs")+c1*cov(At_2,P1t,use="pairwise.complete.obs")+e1*cov(P1t,P2t,use="pairwise.complete.obs")+f1*cov(P1t,P3t_1,use="pairwise.complete.obs"))
    P21 <- e1*(e1*var(P2t,na.rm = TRUE)+b1*cov(At_1,P2t,use="pairwise.complete.obs")+c1*cov(At_2,P2t,use="pairwise.complete.obs")+d1*cov(P1t,P2t,use="pairwise.complete.obs")+f1*cov(P2t,P3t_1,use="pairwise.complete.obs"))
    P31 <- f1*(f1*var(P3t_1,na.rm = TRUE)+b1*cov(At_1,P3t_1,use="pairwise.complete.obs")+c1*cov(At_2,P3t_1,use="pairwise.complete.obs")+d1*cov(P1t,P3t_1,use="pairwise.complete.obs")+e1*cov(P2t,P3t_1,use="pairwise.complete.obs"))
    
    # total variance for model 1
    tableau_var[i,12*(region-1)+2] <- DD1+DL1+P11+P21+P31+sigma_carre_1
    
    # proportion of variance explained for model 1
    tableau_var[i,12*(region-1)+3] <- DD1/tableau_var[i,12*(region-1)+2] 
    tableau_var[i,12*(region-1)+4] <- DL1/tableau_var[i,12*(region-1)+2]
    tableau_var[i,12*(region-1)+5] <- P11/tableau_var[i,12*(region-1)+2]
    tableau_var[i,12*(region-1)+6] <- P21/tableau_var[i,12*(region-1)+2]
    tableau_var[i,12*(region-1)+7] <- P31/tableau_var[i,12*(region-1)+2]
    
    # variance explained by each variables for model 2
    DD2 <- b2*(b2*var(St,na.rm = TRUE)+c2*cov(St,St_1,use="pairwise.complete.obs")+d2*cov(St,P1t,use="pairwise.complete.obs")+e2*cov(St,P2t,use="pairwise.complete.obs")+f2*cov(St,P3t,use="pairwise.complete.obs"))
    DL2 <- c2*(c2*var(St_1,na.rm = TRUE)+b2*cov(St,St_1,use="pairwise.complete.obs")+d2*cov(St_1,P1t,use="pairwise.complete.obs")+e2*cov(St_1,P2t,use="pairwise.complete.obs")+f2*cov(St_1,P3t,use="pairwise.complete.obs"))
    P12 <- d2*(d2*var(P1t,na.rm = TRUE)+b2*cov(St,P1t,use="pairwise.complete.obs")+c2*cov(St_1,P1t,use="pairwise.complete.obs")+e2*cov(P1t,P2t,use="pairwise.complete.obs")+f2*cov(P1t,P3t,use="pairwise.complete.obs"))
    P22 <- e2*(e2*var(P2t,na.rm = TRUE)+b2*cov(St,P2t,use="pairwise.complete.obs")+c2*cov(St_1,P2t,use="pairwise.complete.obs")+d2*cov(P1t,P2t,use="pairwise.complete.obs")+f2*cov(P2t,P3t,use="pairwise.complete.obs"))
    P32 <- f2*(f2*var(P3t,na.rm = TRUE)+b2*cov(St,P3t,use="pairwise.complete.obs")+c2*cov(St_1,P3t,use="pairwise.complete.obs")+d2*cov(P1t,P3t,use="pairwise.complete.obs")+e2*cov(P2t,P3t,use="pairwise.complete.obs"))
    
    # total variance for model 2
    tableau_var[i,12*(region-1)+8] <-  DD2+DL2+P12+P22+P32+sigma_carre_2 
    
    # proportion of variance explained for model 2
    tableau_var[i,12*(region-1)+9] <-  DD2/tableau_var[i,12*(region-1)+8]
    tableau_var[i,12*(region-1)+10] <- DL2/tableau_var[i,12*(region-1)+8]
    tableau_var[i,12*(region-1)+11] <- P12/tableau_var[i,12*(region-1)+8]
    tableau_var[i,12*(region-1)+12] <- P22/tableau_var[i,12*(region-1)+8]
    tableau_var[i,12*(region-1)+13] <- P32/tableau_var[i,12*(region-1)+8]
    
  
  # end of the year's loop
  }
  # end of the region's loop 
}

# we can register our results  
write.csv (tableau_var, "variances_explained_vole.csv", row.names = T, quote = F) 


#-----------------------------------------------------------------------------------------------------------------------------------------------------------------------

# III Variance and proportion of variance explained for predator's models 

# we open our parameters  

parametres <- read.csv("parameters_predators.csv", sep=",", header=T, dec=".",stringsAsFactors=FALSE)

# we create our data frame for our results 
tableau_var_pred <- data.frame(matrix(0,21,37))
names(tableau_var_pred)<-c("annee","VarY3n","a_1_3n","s_1_3n","a_2_3n","VarY4n","a_1_4n","s_1_4n","a_2_4n","VarY5n","s_5n","a_1_5n","s_1_5n",
                                "VarY3e","a_1_3e","s_1_3e","a_2_3e","VarY4e","a_1_4e","s_1_4e","a_2_4e","VarY5e","s_5e","a_1_5e","s_1_5e",
                                "VarY3w","a_1_3w","s_1_3w","a_2_3w","VarY4w","a_1_4w","s_1_4w","a_2_4w","VarY5w","s_5w","a_1_5w","s_1_5w")

# 1) Creation of density's vectors for calculated variance in the whole data set 
# we create our two vectors 
Stn <- data.frame(matrix(0,23,33))
Atn <- data.frame(matrix(0,23,33))

# we can fill them, one line per year and one localisation per column 
for (i in 0:32){ 
  
  for(j in 1:23){ 
     
    Stn[j,i+1] <- donnees[23*i+j,4]
    Atn[j,i+1] <- donnees[23*i+j,5]
    
  }
  
}

# we can create our temporal gap 
# no gap 
Atn_ <- Atn[-c(1,2),] 
Stn_ <- Stn[-c(1,2),] 

# one year gap 
Atn_1 <- Atn[-c(1,23),] 
Stn_1 <- Stn[-c(1,23),] 

# two years gap 
Atn_2 <- Atn[-c(22,23),] 

# 2) Creation of  our loop for regions 
for (region in 1:3){
  
# we put our estimators in variables
a3 <- parametres[3*(region-1)+1,4]
b3 <- parametres[3*(region-1)+1,5]
c3 <- parametres[3*(region-1)+1,6]
d3 <- parametres[3*(region-1)+1,7]
sigma_carre_3 <- parametres[3*(region-1)+1,8]^2

a4 <- parametres[3*(region-1)+2,4]
b4 <- parametres[3*(region-1)+2,5]
c4 <- parametres[3*(region-1)+2,6]
d4 <- parametres[3*(region-1)+2,7]
sigma_carre_4 <- parametres[3*(region-1)+2,8]^2

a5 <- parametres[3*(region-1)+3,4]
b5 <- parametres[3*(region-1)+3,5]
c5 <- parametres[3*(region-1)+3,6]
d5 <- parametres[3*(region-1)+3,7]
sigma_carre_5 <- parametres[3*(region-1)+3,8]^2


# we can create a loop to run through the years 
for (i in 1:21){ 
  
  # our vectors only keep the current (i) year data 
  At <- Atn_[i,]
  At_1 <- Atn_1[i,]
  At_2 <- Atn_2[i,]
  St <- Stn_[i,]
  St_1 <- Stn_1[i,]
  
  # we have to keep the numeric format 
  At <- as.numeric(At)
  At_1 <- as.numeric(At_1)
  At_2 <- as.numeric(At_2)
  St <-  as.numeric(St)
  St_1 <- as.numeric(St_1)
  
  # we register the year in  the data frame's first column 
  tableau_var_pred[i,1] <-1990 + i
  
  # variance explained by each variables for model 3
  V13 <- b3*(b3*var(At_1,na.rm = TRUE)+c3*cov(At_1,St_1,use="pairwise.complete.obs")+d3*cov(At_1,At_2,use="pairwise.complete.obs"))
  V23 <- c3*(c3*var(St_1,na.rm = TRUE)+b3*cov(At_1,St_1,use="pairwise.complete.obs")+d3*cov(At_2,St_1,use="pairwise.complete.obs"))
  V33 <- d3*(d3*var(At_2,na.rm = TRUE)+b3*cov(At_1,At_2,use="pairwise.complete.obs")+c3*cov(At_2,St_1,use="pairwise.complete.obs"))
  
  # total variance for model 3
  tableau_var_pred[i,12*(region-1)+2] <- V13+V23+V33+sigma_carre_3
  
  # proportion of variance explained for model 3
  tableau_var_pred[i,12*(region-1)+3] <- V13/tableau_var_pred[i,12*(region-1)+2] 
  tableau_var_pred[i,12*(region-1)+4] <- V23/tableau_var_pred[i,12*(region-1)+2]
  tableau_var_pred[i,12*(region-1)+5] <- V33/tableau_var_pred[i,12*(region-1)+2]
  
  # variance explained by each variables for model 4
  V14 <- b4*(b4*var(At_1,na.rm = TRUE)+c4*cov(At_1,St_1,use="pairwise.complete.obs")+d4*cov(At_1,At_2,use="pairwise.complete.obs"))
  V24 <- c4*(c4*var(St_1,na.rm = TRUE)+b4*cov(At_1,St_1,use="pairwise.complete.obs")+d4*cov(At_2,St_1,use="pairwise.complete.obs"))
  V34 <- d4*(d4*var(At_2,na.rm = TRUE)+b4*cov(At_1,At_2,use="pairwise.complete.obs")+c4*cov(At_2,St_1,use="pairwise.complete.obs"))
  
  # total variance for model 4
  tableau_var_pred[i,12*(region-1)+6] <- V14+V24+V34+sigma_carre_4
  
  # proportion of variance explained for model 4
  tableau_var_pred[i,12*(region-1)+7] <- V14/tableau_var_pred[i,12*(region-1)+6] 
  tableau_var_pred[i,12*(region-1)+8] <- V24/tableau_var_pred[i,12*(region-1)+6]
  tableau_var_pred[i,12*(region-1)+9] <- V34/tableau_var_pred[i,12*(region-1)+6]
  
  # variance explained by each variables for model 5
  V15 <- b5*(b5*var(St,na.rm = TRUE)+c5*cov(St,At_1,use="pairwise.complete.obs")+d5*cov(St,St_1,use="pairwise.complete.obs"))
  V25 <- c5*(c5*var(At_1,na.rm = TRUE)+b5*cov(St,At_1,use="pairwise.complete.obs")+d5*cov(St_1,At_1,use="pairwise.complete.obs"))
  V35 <- d5*(d5*var(St_1,na.rm = TRUE)+b5*cov(St,St_1,use="pairwise.complete.obs")+c5*cov(St_1,At_1,use="pairwise.complete.obs"))
  
  # total variance for model 5
  tableau_var_pred[i,12*(region-1)+10] <- V15+V25+V35+sigma_carre_5
  
  # proportion of variance explained for model 5
  tableau_var_pred[i,12*(region-1)+11] <- V15/tableau_var_pred[i,12*(region-1)+10] 
  tableau_var_pred[i,12*(region-1)+12] <- V25/tableau_var_pred[i,12*(region-1)+10]
  tableau_var_pred[i,12*(region-1)+13] <- V35/tableau_var_pred[i,12*(region-1)+10]
  
  # end of the year's loop
   }

# end of the region's loop
}

# we register our results 
write.csv (tableau_var_pred, "variances_explained_predators.csv", row.names = T, quote = F) 

