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
names(Vole) <- c("1n","1e","1o","2n","2e","2o","3n","3e","3o","4n","4e","4o","5n","5e","5o","6n","6e","6o","7n","7e","7o","8n","8e","8o")
Vole_1 <- data.frame(matrix(0,2000,24)) 
names(Vole_1) <- c("1n","1e","1o","2n","2e","2o","3n","3e","3o","4n","4e","4o","5n","5e","5o","6n","6e","6o","7n","7e","7o","8n","8e","8o")
Yt <- data.frame(matrix(0,2000,24)) 
names(Yt) <- c("1n","1e","1o","2n","2e","2o","3n","3e","3o","4n","4e","4o","5n","5e","5o","6n","6e","6o","7n","7e","7o","8n","8e","8o")

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


# Now we can do the simulations for each region (3) and each scenario (8) , ie 24 simulations 

#_______________________________________________________________________________________________________________________________


# First, we will create a code only for the first simulation 


# initialisation 

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

# We create vectors to store our data 

tableau_density_dependance <- data.frame(matrix(0,40,2))
tableau_s_index <- data.frame(matrix(0,40,1))
tableau_saisonalite <- data.frame(matrix(0,40,1))


# 1000 years loop 

for (i in 1:1000){
  # Chronologicaly calculated 
  P1t <- a3n + P1t +b3n*At +c3n*St + d3n*At_1 + rnorm(1,0,sigma3n) # winter 
  P2t <- a4n + P2t +b4n*At +c4n*St +d4n*At_1 + rnorm(1,0,sigma4n)
  St_1 <- St # spring
  St <- a1n + (b1n+1)*At +c1n*At_1 +d1n*P1t +e1n*P2t + f1n*P3t+ rnorm(1,0,sigma1n)  
  P3t <- a5n + P3t +b5n*St+c5n*At+d5n*St_1 + rnorm(1,0,sigma5n) # summer 
  At_1 <- At 
  At <- a2n + (b2n+1)*St + c2n*St_1 +d2n*P1t +e2n*P2t +f2n*P3t+ rnorm(1,0,sigma2n) # autumn
  # Store the density 
  Vole[2*i-1,1] <- St
  Vole[2*i,1] <- At
  Vole_1[2*i-1,1] <- St_1
  Vole_1[2*i,1] <- At_1
  # Calculate growth rate 
  Y1 <- St-At_1
  Yt[2*i-1,1] <- Y1
  Y2 <- At-St
  Yt[2*i,1] <- Y2
  saisonalite <- saisonalite+(Y1-Y2)
  # each 25 years, we have a section 
  if (i %%25 ==0){
    j<- j+1
    # Calculate indicators 
    # Density dependance 
    Vole_section <- Vole[25*(j-1)+1:25*j,1]
    Vole_1_section <- Vole_1[25*(j-1)+1:25*j,1]
    Yt_section <- Yt[25*(j-1)+1:25*j,1]
    Density_dependance <- lm(formula = Yt_section ~ Vole_section+Vole_1_section, na.action=na.omit)
    tableau_density_dependance[j,1] <- summary(Density_dependance)$coefficient[1]
    tableau_density_dependance[j,2] <- summary(Density_dependance)$coefficient[2]
    # s index 
    s_index <- sqrt(var(Vole_section))
    tableau_s_index[j,1] <- s_index
    # saisonnality 
    saisonalite_moyenne <- saisonalite/25
    tableau_saisonalite[j,1] <- saisonalite_moyenne
    saisonalite <- 0
  }
}

# Register results 

simulations[1,1] <- 1
simulations[1,2] <- 1
simulations[1,3] <- mean(tableau_density_dependance[,1])
simulations[1,4]<- mean(tableau_density_dependance[,2])
simulations[1,5] <- mean(tableau_s_index[,1])
simulations[1,6]<- mean(tableau_s_index[,1])-1.96*(var(tableau_s_index[,1])/sqrt(40)) #IC 95% ( bas) 
simulations[1,7]<- mean(tableau_s_index[,1])+1.96*(var(tableau_s_index[,1])/sqrt(40)) #IC 95% ( haut )
simulations[1,8] <- mean(tableau_saisonalite[,1])
simulations[1,9]<- mean(tableau_saisonalite[,1])-1.96*(var(tableau_saisonalite[,1])/sqrt(40)) #IC 95% ( bas) 
simulations[1,10]<- mean(tableau_saisonalite[,1])+1.96*(var(tableau_saisonalite[,1])/sqrt(40)) #IC 95% ( haut ) 

write.csv (Vole, "Vole_simulations1.csv", row.names = T, quote = F) 
write.csv (simulations, "simulations1.csv",row.names=T,quote=F)



# Now we have the loop with all scenarios and all regions 

for (scenario in 1:8){ 
  for (region in 1:3){
    
    # for each region, we have to select data and estimators for this region 
    
    if (region==1){
      Present <- filter(donnees,cardinalite=="nord",year==1990)
      Retard <- filter(donnees,cardinalite=="nord",year==1989)
      
      a1 <- parametres[1,4]
      b1 <- parametres[1,5]
      c1 <- parametres[1,6]
      d1 <- parametres[1,7]
      e1 <- parametres[1,8]
      f1 <- parametres[1,9]
      sigma1 <- parametres[1,10]
      
      a2 <- parametres[2,4]
      b2 <- parametres[2,5]
      c2 <- parametres[2,6]
      d2 <- parametres[2,7]
      e2 <- parametres[2,8]
      f2 <- parametres[2,9]
      sigma2 <- parametres[2,10]
      
      a3 <- parametres_pred[1,4]
      b3 <- parametres_pred[1,5]
      c3 <- parametres_pred[1,6]
      d3 <- parametres_pred[1,7]
      sigma3 <- parametres_pred[1,8]
      
      a4 <- parametres_pred[2,4]
      b4 <- parametres_pred[2,5]
      c4 <- parametres_pred[2,6]
      d4 <- parametres_pred[2,7]
      sigma4 <- parametres_pred[2,8]
      
      a5 <- parametres_pred[3,4]
      b5 <- parametres_pred[3,5]
      c5 <- parametres_pred[3,6]
      d5<- parametres_pred[3,7]
      sigma5 <- parametres_pred[3,8]
    }
    if (region==2){
      Present <- filter(donnees,cardinalite=="est",year==1990)
      Retard <- filter(donnees,cardinalite=="est",year==1989)
      
      a1 <- parametres[3,4]
      b1 <- parametres[3,5]
      c1 <- parametres[3,6]
      d1 <- parametres[3,7]
      e1 <- parametres[3,8]
      f1 <- parametres[3,9]
      sigma1 <- parametres[3,10]
      
      a2 <- parametres[4,4]
      b2 <- parametres[4,5]
      c2 <- parametres[4,6]
      d2 <- parametres[4,7]
      e2 <- parametres[4,8]
      f2 <- parametres[4,9]
      sigma2 <- parametres[4,10]
      
      a3 <- parametres_pred[4,4]
      b3 <- parametres_pred[4,5]
      c3 <- parametres_pred[4,6]
      d3 <- parametres_pred[4,7]
      sigma3 <- parametres_pred[4,8]
      
      a4 <- parametres_pred[5,4]
      b4 <- parametres_pred[5,5]
      c4 <- parametres_pred[5,6]
      d4 <- parametres_pred[5,7]
      sigma4 <- parametres_pred[5,8]
      
      a5 <- parametres_pred[6,4]
      b5 <- parametres_pred[6,5]
      c5 <- parametres_pred[6,6]
      d5 <- parametres_pred[6,7]
      sigma5 <- parametres_pred[6,8]
    }
    if (region==3){
      Present <- filter(donnees,cardinalite=="sud_ouest",year==1990)
      Retard <- filter(donnees,cardinalite=="sud_ouest",year==1989)
      
      a1 <- parametres[5,4]
      b1 <- parametres[5,5]
      c1 <- parametres[5,6]
      d1 <- parametres[5,7]
      e1 <- parametres[5,8]
      f1 <- parametres[5,9]
      sigma1 <- parametres[5,10]
      
      a2 <- parametres[6,4]
      b2 <- parametres[6,5]
      c2 <- parametres[6,6]
      d2 <- parametres[6,7]
      e2 <- parametres[6,8]
      f2 <- parametres[6,9]
      sigma2 <- parametres[6,10]
      
      a3 <- parametres_pred[7,4]
      b3 <- parametres_pred[7,5]
      c3 <- parametres_pred[7,6]
      d3 <- parametres_pred[7,7]
      sigma3 <- parametres_pred[7,8]
      
      a4 <- parametres_pred[8,4]
      b4 <- parametres_pred[8,5]
      c4 <- parametres_pred[8,6]
      d4 <- parametres_pred[8,7]
      sigma4 <- parametres_pred[8,8]
      
      a5 <- parametres_pred[9,4]
      b5 <- parametres_pred[9,5]
      c5 <- parametres_pred[9,6]
      d5 <- parametres_pred[9,7]
      sigma5 <- parametres_pred[9,8]
      
    }
    
# we initialize Vole density
    
At <- mean(na.omit(Present$Vole.Autumn))
At_1 <- mean(na.omit(Retard$Vole.Autumn))
St <- mean(na.omit(Present$Vole.Spring))
St_1 <- mean(na.omit(Retard$Vole.Spring)) 

# we initialize predators density depending of the scenario 


if (scenario ==2 ||scenario ==5 ||scenario ==7 ||scenario ==8 ){
  P1t  <-min(na.omit(donnees$Small.mustelid))
}
else {
  P1t <- mean(na.omit(Present$Small.mustelid))
}

if (scenario ==3 ||scenario ==4 ||scenario ==7 ||scenario ==8 ){
  P2t <-min(na.omit(Present$Generalist.predator))
}
else {
  P2t <- mean(na.omit(Present$Generalist.predator))
}    
  
if (scenario ==3 ||scenario ==5 ||scenario ==6 ||scenario ==8 ){
    P3t <- min(na.omit(Present$Avian.predator))
}
else {
  P3t <-mean(na.omit(Present$Avian.predator))
}  

 # We create vectors to store our data and initializing some variables to 0 
tableau_density_dependance <- data.frame(matrix(0,40,2))
tableau_s_index <- data.frame(matrix(0,40,1))
tableau_saisonalite <- data.frame(matrix(0,40,1))
saisonalite <- 0
j <- 0 #number of section 

# 1000 years loop 

for (i in 1:1000){
  # Chronologically calculated 
  # Predators are calculated only if there are taken in the model 
  # Winter 
  if (scenario ==1 ||scenario ==3 ||scenario ==4 ||scenario ==6 ){
  P1t <- a3 + P1t +b3*At +c3*St + d3*At_1 + rnorm(1,0,sigma3) 
  }
  if (scenario ==1 ||scenario ==2 ||scenario ==5 ||scenario ==6 ){
  P2t <- a4 + P2t +b4*At +c4*St +d4*At_1 + rnorm(1,0,sigma4)
  }
  # Spring
  St_1 <- St
  St <- a1 + (b1+1)*At +c1*At_1 +d1*P1t +e1*P2t + f1*P3t+ rnorm(1,0,sigma1) 
  # Summer 
  if (scenario ==1 ||scenario ==2 ||scenario ==4 ||scenario ==7 ){
  P3t <- a5 + P3t +b5*St +c5*At +d5*St_1 + rnorm(1,0,sigma5) # été 
  }
  # Autumn 
  At_1 <- At 
  At <- a2 + (b2+1)*St + c2*St_1 +d2*P1t +e2*P2t +f2*P3t+ rnorm(1,0,sigma2) # automne 
  # Vole density Storage 
  Vole[2*i-1,(3*(scenario-1)+region)] <- St
  Vole[2*i,(3*(scenario-1)+region)] <- At
  Vole_1[2*i-1,(3*(scenario-1)+region)] <- St_1
  Vole_1[2*i,(3*(scenario-1)+region)] <- At_1
  # Growth rate and seasonality 
  Y1 <- St-At_1
  Yt[2*i-1,(3*(scenario-1)+region)] <- Y1
  Y2 <- At-St
  Yt[2*i,(3*(scenario-1)+region)] <- Y2
  saisonalite <- saisonalite+(Y1-Y2)
  # Each 25 years, we have a section 
  if (i %%25 ==0){
    j<- j+1
    # We can calculate our indicators , first we have to select data for our section 
    Vole_section <- Vole[25*(j-1)+1:25*j,(3*(scenario-1)+region)]
    Vole_1_section <- Vole_1[25*(j-1)+1:25*j,(3*(scenario-1)+region)]
    Yt_section <- Yt[25*(j-1)+1:25*j,(3*(scenario-1)+region)]
    # Density dependance 
    Density_dependance <- lm(formula = Yt_section ~ Vole_section+Vole_1_section, na.action=na.omit)
    tableau_density_dependance[j,1] <- summary(Density_dependance)$coefficient[1]
    tableau_density_dependance[j,2] <- summary(Density_dependance)$coefficient[2]
    # s index 
    tableau_s_index[j,1] <- sqrt(var(Vole_section))
    # saisonnality 
    tableau_saisonalite[j,1] <- saisonalite/25
    saisonalite <- 0
  }
}

# we complete our table 

simulations[(3*(scenario-1)+region),1] <- scenario
simulations[(3*(scenario-1)+region),2] <- region
simulations[(3*(scenario-1)+region),3] <- mean(tableau_density_dependance[,1])
simulations[(3*(scenario-1)+region),4]<- mean(tableau_density_dependance[,2])
simulations[(3*(scenario-1)+region),5] <- mean(tableau_s_index[,1])
simulations[(3*(scenario-1)+region),6]<- mean(tableau_s_index[,1])-1.96*(var(tableau_s_index[,1])/sqrt(40)) #IC 95% ( bas) 
simulations[(3*(scenario-1)+region),7]<- mean(tableau_s_index[,1])+1.96*(var(tableau_s_index[,1])/sqrt(40)) #IC 95% ( haut )
simulations[(3*(scenario-1)+region),8] <- mean(tableau_saisonalite[,1])
simulations[(3*(scenario-1)+region),9]<- mean(tableau_saisonalite[,1])-1.96*(var(tableau_saisonalite[,1])/sqrt(40)) #IC 95% ( bas) 
simulations[(3*(scenario-1)+region),10]<- mean(tableau_saisonalite[,1])+1.96*(var(tableau_saisonalite[,1])/sqrt(40)) #IC 95% ( haut )



  }
}

# Register results 

write.csv (Vole, "Vole_simulations.csv", row.names = T, quote = F) 
write.csv (simulations, "simulations.csv",row.names=T,quote=F)



































