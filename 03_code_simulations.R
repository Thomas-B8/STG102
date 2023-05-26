
# author = "Besognet Thomas"
# date = " 24/05/23" 
# project = "Estimation d'intéractions entre espèces à partir de séries temporelles"
# name = "Simulations"

# packages 
install.packages("dplyr")
library(dplyr)

# Opening data 
donnees <- read.csv("donnees_propres_korpela.csv", sep=",", header=T, dec=".")

#-------------------------------------------------------------------------------------------------------------------------------------------------------

# We create the data frame with data that we will produce 

# simulation's indicators 
simulations <-data.frame(matrix(0,24,10)) 
names(simulations) <- c("scenario","region","direct density dependance","delayed density dependance","s-index","min_s","max_s","seasonality","min_n","max_n")

# vole density 
Vole <- data.frame(matrix(0,2000,24)) 
names(Vole) <- c("1n","1e","1o","2n","2e","2o","3n","3e","3o","4n","4e","4o","5n","5e","5o","6n","6e","6o","7n","7e","7o","8n","8e","8o")

# vole density in autumn with a 2 years gap 
Vole_autumn_2 <- data.frame(matrix(0,1000,24)) 
names(Vole_autumn_2) <- c("1n","1e","1o","2n","2e","2o","3n","3e","3o","4n","4e","4o","5n","5e","5o","6n","6e","6o","7n","7e","7o","8n","8e","8o")

# vole density in autumn with a 1 years gap 
Vole_autumn_1 <- data.frame(matrix(0,1000,24)) 
names(Vole_autumn_1) <- c("1n","1e","1o","2n","2e","2o","3n","3e","3o","4n","4e","4o","5n","5e","5o","6n","6e","6o","7n","7e","7o","8n","8e","8o")

# vole density in spring with a 1 years gap 
Vole_spring_1 <- data.frame(matrix(0,1000,24)) 
names(Vole_spring_1) <- c("1n","1e","1o","2n","2e","2o","3n","3e","3o","4n","4e","4o","5n","5e","5o","6n","6e","6o","7n","7e","7o","8n","8e","8o")

# vole density in spring without  years gap 
Vole_spring <- data.frame(matrix(0,1000,24)) 
names(Vole_spring) <- c("1n","1e","1o","2n","2e","2o","3n","3e","3o","4n","4e","4o","5n","5e","5o","6n","6e","6o","7n","7e","7o","8n","8e","8o")

# vole gowth rate in winter 
Yt_1 <- data.frame(matrix(0,1000,24)) 
names(Yt_1) <- c("1n","1e","1o","2n","2e","2o","3n","3e","3o","4n","4e","4o","5n","5e","5o","6n","6e","6o","7n","7e","7o","8n","8e","8o")

# vole gowth rate in summer 
Yt_2 <- data.frame(matrix(0,1000,24)) 
names(Yt_2) <- c("1n","1e","1o","2n","2e","2o","3n","3e","3o","4n","4e","4o","5n","5e","5o","6n","6e","6o","7n","7e","7o","8n","8e","8o")


# We open the parameters estimated previously and create variable to stock them
parametres <- read.csv("parameters_vole.csv", sep=",", header=T, dec=".",stringsAsFactors=FALSE)
parametres_pred <- read.csv("parameters_predators.csv", sep=",", header=T, dec=".",stringsAsFactors=FALSE)


# Now we can do the simulations for each region (3) and each scenario (8) , ie 24 simulations 
for (scenario in 1:8){ 
  
  for (region in 1:3){
    
    # for each region, we have to select data and estimators for this region 
    if (region==1){
      
      Present <- filter(donnees,cardinalite=="nord",year==2011)
      Retard <- filter(donnees,cardinalite=="nord",year==2010)
      
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
      
      Present <- filter(donnees,cardinalite=="est",year==2011)
      Retard <- filter(donnees,cardinalite=="est",year==2010)
      
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
      Present <- filter(donnees,cardinalite=="sud_ouest",year==2011)
      Retard <- filter(donnees,cardinalite=="sud_ouest",year==2010)
      
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
  
  P2t <-min(na.omit(donnees$Generalist.predator))
  
}

else {
  
  P2t <- mean(na.omit(Present$Generalist.predator))
  
}    
  
if (scenario ==3 ||scenario ==5 ||scenario ==6 ||scenario ==8 ){
  
    P3t <- min(na.omit(donnees$Avian.predator))
    
}

else {
  
  P3t <-mean(na.omit(Present$Avian.predator))
  
}  

# We create vectors to store our data for each section of 25 years and initializing some variables to 0 
tableau_density_dependance <- data.frame(matrix(0,40,4))
tableau_s_index <- data.frame(matrix(0,40,1))
tableau_saisonalite <- data.frame(matrix(0,40,1))
saisonalite <- 0
j <- 0 

# we can begin our 1000 years loop 
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
  St_2 <- St_1
  St_1 <- St
  St <- a1 + (b1+1)*At +c1*At_1 +d1*P1t +e1*P2t + f1*P3t+ rnorm(1,0,sigma1) 
  # Summer 
  if (scenario ==1 ||scenario ==2 ||scenario ==4 ||scenario ==7 ){
  P3t <- a5 + P3t +b5*St +c5*At +d5*St_1 + rnorm(1,0,sigma5) 
  }
  # Autumn 
  At_2 <- At_1
  At_1 <- At 
  At <- a2 + (b2+1)*St + c2*St_1 +d2*P1t +e2*P2t +f2*P3t+ rnorm(1,0,sigma2)  
  # Vole density Storage 
  Vole_autumn_2[i,(3*(scenario-1)+region)] <- At_2
  Vole_autumn_1[i,(3*(scenario-1)+region)] <- At_1
  Vole_spring_1[i,(3*(scenario-1)+region)] <- St_1
  Vole_spring[2*i,(3*(scenario-1)+region)] <- St
  Vole[2*i-1,(3*(scenario-1)+region)] <- St
  Vole[2*i,(3*(scenario-1)+region)] <- At
  # Growth rate and seasonality 
  Yt_1[i,(3*(scenario-1)+region)] <- St-At_1
  Yt_2[i,(3*(scenario-1)+region)] <- At-St
  saisonalite <- saisonalite + (At-St)-(St-At_1)
  # Each 25 years, we have a section 
  if (i %%25 ==0){
    j<- j+1
    # We can calculate our indicators , first we have to select data for our section 
    Vole_section <- Vole[25*(j-1)+1:50*j,(3*(scenario-1)+region)]
    Vole_autumn_2_section <- Vole_autumn_2[25*(j-1)+1:25*j,(3*(scenario-1)+region)]
    Vole_autumn_1_section <- Vole_autumn_1[25*(j-1)+1:25*j,(3*(scenario-1)+region)]
    Vole_spring_1_section <- Vole_spring_1[25*(j-1)+1:25*j,(3*(scenario-1)+region)]
    Vole_spring_section <- Vole_spring[25*(j-1)+1:25*j,(3*(scenario-1)+region)]
    Yt_1_section <- Yt_1[25*(j-1)+1:25*j,(3*(scenario-1)+region)]
    Yt_2_section <- Yt_2[25*(j-1)+1:25*j,(3*(scenario-1)+region)]
    # Density dependance 
    Density_dependance_1 <- lm(formula = Yt_1_section ~ Vole_autumn_1_section +  Vole_autumn_2_section , na.action=na.omit)
    Density_dependance_2 <- lm(formula = Yt_2_section ~ Vole_spring_section +  Vole_spring_1_section , na.action=na.omit)
    tableau_density_dependance[j,1] <- Density_dependance_1$coefficient[2]
    tableau_density_dependance[j,2] <- Density_dependance_1$coefficient[3]
    tableau_density_dependance[j,3] <- Density_dependance_2$coefficient[2]
    tableau_density_dependance[j,4] <- Density_dependance_2$coefficient[3]
    # s index 
    # s index 
    tableau_s_index[j,1] <- sqrt(var(Vole_section))
    # saisonnality 
    tableau_saisonalite[j,1] <- saisonalite/25
    saisonalite <- 0
  
  # end of the section's calculs  
  }
  
# end of the 1000 years loop 
}

# calcul : from seasonal density dependance to annual density dependancd 
B1 <- mean(tableau_density_dependance[,1])
C1 <- mean(tableau_density_dependance[,2])
B2 <- mean(tableau_density_dependance[,3])
C2 <- mean(tableau_density_dependance[,4])
DR <- B1 + B2 + B2*B1 - 0.20 
DL <- C1 + C2 + C2*B1 + B2*C1 

# we complete our data table 
simulations[(3*(scenario-1)+region),1] <- scenario
simulations[(3*(scenario-1)+region),2] <- region
simulations[(3*(scenario-1)+region),3] <- DR
simulations[(3*(scenario-1)+region),4] <- DL
simulations[(3*(scenario-1)+region),5] <- mean(tableau_s_index[,1])
simulations[(3*(scenario-1)+region),6] <- mean(tableau_s_index[,1])-1.96*(var(tableau_s_index[,1])/sqrt(40)) 
simulations[(3*(scenario-1)+region),7] <- mean(tableau_s_index[,1])+1.96*(var(tableau_s_index[,1])/sqrt(40)) 
simulations[(3*(scenario-1)+region),8] <- mean(tableau_saisonalite[,1])
simulations[(3*(scenario-1)+region),9] <- mean(tableau_saisonalite[,1])-1.96*(var(tableau_saisonalite[,1])/sqrt(40))  
simulations[(3*(scenario-1)+region),10] <- mean(tableau_saisonalite[,1])+1.96*(var(tableau_saisonalite[,1])/sqrt(40)) 

# end of the region's loop
  }
  
  # end of the scenario's loop 
}

# we register our results 
write.csv (Vole, "vole_density_simulations.csv", row.names = T, quote = F) 
write.csv (simulations, "simulations_indicators.csv",row.names=T,quote=F)



































