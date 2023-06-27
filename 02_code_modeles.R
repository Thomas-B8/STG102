
# author = "Besognet Thomas"
# date = " 23/05/23" 
# project = "Korpela et al. Proceedings B Replication work""
# name = "Models"

# packages 
install.packages("dplyr")
library(dplyr)

# We open clean data 
data <- read.csv("../data/data_Korpela.csv", sep=",", header=T, dec=".")


#------------------------------------------------------------------------------------------------------------------------------------------


# I Parameter's estimation 

# We create two data frames, the first one for models 1 et 2 (vole) , the second for models 3,4 and 5 (Predators)
estimators_vole <- data.frame(matrix(0,6,9))
names(estimators_vole)<-c("zone","model","a","b","c","d","e","f","sigma")

estimators_predators <- data.frame(matrix(0,9,7))
names(estimators_predators)<-c("zone","model","a","b","c","d","sigma")


# parameters are estimated by zones, so we have to filter the data frame 
for (zone in 1:3){
  if (zone==1){
    data_zone <- filter(data,zone=="north")
    region <- "north"
  }
  if (zone==2){
    data_zone <- filter(data,zone=="east")
    region <- "east"
  }
  if (zone==3){
    data_zone <- filter(data,zone=="west")
    region <- "west"
  }
  
# then, we create vectors for each variables and each temporal laps 
  Present <-filter(data_zone,year>1990)
  At    <-Present$Vole.Autumn
  St    <-Present$Vole.Spring
  P1t   <-Present$Small.mustelid 
  P2t   <-Present$Generalist.predator
  P3t   <-Present$Avian.predator
  Past_1 <-filter(data_zone,year>1989,year<2011) 
  At_1    <-Past_1$Vole.Autumn
  St_1    <-Past_1$Vole.Spring
  P1t_1   <-Past_1$Small.mustelid
  P2t_1   <-Past_1$Generalist.predator
  P3t_1   <-Past_1$Avian.predator
  Past_2 <-filter(data_zone,year<2010)  
  At_2    <-Past_2$Vole.Autumn
  
# we can now estimate our five models 
  model1 <- lm(formula = St ~ At_1 + At_2 + P1t + P2t + P3t_1 ,offset = At_1 )
  model2 <- lm(formula = At ~ St + St_1 + P1t + P2t + P3t ,offset = St   )
  model3 <- lm(formula = P1t ~ At_1 + St_1 + At_2 ,offset = P1t_1 )
  model4 <- lm(formula = P2t ~ At_1 + St_1 + At_2 ,offset = P2t_1 )
  model5 <- lm(formula = P3t ~ St + At_1 + St_1 ,offset = P3t_1 )
  
# and complete our data frames 
  # 1) the zone's name
  estimators_vole[2*(zone-1)+1,1] <- region
  estimators_vole[2*(zone-1)+2,1] <- region
  
  estimators_predators[3*(zone-1)+1,1] <- region
  estimators_predators[3*(zone-1)+2,1] <- region
  estimators_predators[3*(zone-1)+3,1] <- region
  
  
  # 2) the model's name
  estimators_vole[2*(zone-1)+1,2] <- 1
  estimators_vole[2*(zone-1)+2,2] <- 2
  
  estimators_predators[3*(zone-1)+1,2] <- 3
  estimators_predators[3*(zone-1)+2,2] <- 4
  estimators_predators[3*(zone-1)+3,2] <- 5 
  
  # 3.1) parameters for models 1 and 2 
  for (j in 3:8) {
     
    
    estimators_vole[2*(zone-1)+1,j] <- model1$coefficients[j-2]
    estimators_vole[2*(zone-1)+2,j] <- model2$coefficients[j-2]
    
  }
  
  # 3.2) parameters for models 3,4 and 5  
  for (j in 3:6) {
    
    estimators_predators[3*(zone-1)+1,j] <- model3$coefficients[j-2]
    estimators_predators[3*(zone-1)+2,j] <- model4$coefficients[j-2]
    estimators_predators[3*(zone-1)+3,j] <- model5$coefficients[j-2]
    
  }
  
  # 4) error's variance 
  estimators_vole[2*(zone-1)+1,9] <- summary.lm(model1)$sigma
  estimators_vole[2*(zone-1)+2,9] <- summary.lm(model2)$sigma
  
  estimators_predators[3*(zone-1)+1,7] <- summary.lm(model3)$sigma
  estimators_predators[3*(zone-1)+2,7] <- summary.lm(model4)$sigma
  estimators_predators[3*(zone-1)+3,7] <- summary.lm(model5)$sigma

# we can end the zone's loop 
}

# we can register our data frame in csv 
write.csv (estimators_vole, "../data/parameters_vole.csv", row.names = T, quote = F)
write.csv (estimators_predators, "../data/parameters_predators.csv", row.names = T, quote = F)

#------------------------------------------------------------------------------------------------------------------------------------------

# II Variance and proportion of variance explained for Vole's models 

# we open our parameters 
parameters <- read.csv("../data/parameters_vole.csv", sep=",", header=T, dec=".",stringsAsFactors=FALSE)

# we create our data frame for our results 
table_var<- data.frame(matrix(0,21,37))
names(table_var)<-c("year","VarY1n","dir1n","del1n","P11n","P21n","P31n","VarY2n","dir2n","del2n","P12n","P22n","P32n",
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
    
    Stn[j,i+1] <- data[23*i+j,4]
    Atn[j,i+1] <- data[23*i+j,5]
    P1tn[j,i+1] <- data[23*i+j,6]
    P2tn[j,i+1] <- data[23*i+j,7]
    P3tn[j,i+1] <- data[23*i+j,8]
    
  }
  
}

# missing values 
for (i2 in 0:32){
  
  # if the first data of a column is a NA, we replace it with the column's mean 
  if (is.na(Stn[1,i2+1])){
    Stn[1,i2+1] <- mean(Stn[,i2+1],na.rm=TRUE)
  }
  
  if (is.na(Atn[1,i2+1])){
    Atn[1,i2+1] <- mean(Atn[,i2+1],na.rm=TRUE)
  }
  
  if (is.na(P1tn[1,i2+1])){
    P1tn[1,i2+1] <- mean(P1tn[,i2+1],na.rm=TRUE)
  }
  
  if (is.na(P2tn[1,i2+1])){
    P2tn[1,i2+1] <- mean(P2tn[,i2+1],na.rm=TRUE)
  }
  
  if (is.na(P3tn[1,i2+1])){
    P3tn[1,i2+1] <- mean(P3tn[,i2+1],na.rm=TRUE)
  }
  
  
  for(j2 in 2:23){ 
    
    # then, if there are NA for other years, we use the "last observation carry forward"
    if (is.na(Stn[j2,i2+1])){
      Stn[j2,i2+1] <- Stn[j2-1,i2+1]
    }
    if (is.na(Atn[j2,i2+1])){
      Atn[j2,i2+1] <- Atn[j2-1,i2+1]
    }
    if (is.na(P1tn[j2,i2+1])){
      P1tn[j2,i2+1] <- P1tn[j2-1,i2+1]
    }
    if (is.na(P2tn[j2,i2+1])){
      P2tn[j2,i2+1] <- P2tn[j2-1,i2+1]
    }
    if (is.na(P3tn[j2,i2+1])){
      P3tn[j2,i2+1] <- P3tn[j2-1,i2+1]
    }
    
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


# 2) Creation of  our loop for zones 
for (zone in 1:3){
  
  # we put our estimators in variables 
  a1 <- parameters[2*(zone-1)+1,4]
  b1 <- parameters[2*(zone-1)+1,5]
  c1 <- parameters[2*(zone-1)+1,6]
  d1 <- parameters[2*(zone-1)+1,7]
  e1 <- parameters[2*(zone-1)+1,8]
  f1 <- parameters[2*(zone-1)+1,9]
  sigma_carre_1 <- parameters[2*(zone-1)+1,10]^2
  
  a2 <- parameters[2*(zone-1)+2,4]
  b2 <- parameters[2*(zone-1)+2,5]
  c2 <- parameters[2*(zone-1)+2,6]
  d2 <- parameters[2*(zone-1)+2,7]
  e2 <- parameters[2*(zone-1)+2,8]
  f2 <- parameters[2*(zone-1)+2,9]
  sigma_carre_2 <- parameters[2*(zone-1)+2,10]^2
  
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
    table_var[i,1] <-1990 + i
    
    # variance explained by each variables for model 1 
    DD1 <- b1*(b1*var(At_1)+c1*cov(At_1,At_2)+d1*cov(At_1,P1t)+e1*cov(At_1,P2t)+f1*cov(At_1,P3t_1))
    DL1 <- c1*(c1*var(At_2)+b1*cov(At_1,At_2)+d1*cov(At_2,P1t)+e1*cov(At_2,P2t)+f1*cov(At_2,P3t_1))
    P11 <- d1*(d1*var(P1t)+b1*cov(At_1,P1t)+c1*cov(At_2,P1t)+e1*cov(P1t,P2t)+f1*cov(P1t,P3t_1))
    P21 <- e1*(e1*var(P2t)+b1*cov(At_1,P2t)+c1*cov(At_2,P2t)+d1*cov(P1t,P2t)+f1*cov(P2t,P3t_1))
    P31 <- f1*(f1*var(P3t_1)+b1*cov(At_1,P3t_1)+c1*cov(At_2,P3t_1)+d1*cov(P1t,P3t_1)+e1*cov(P2t,P3t_1))
    
    # total variance for model 1
    table_var[i,12*(zone-1)+2] <- DD1+DL1+P11+P21+P31+sigma_carre_1
    
    # proportion of variance explained for model 1
    table_var[i,12*(zone-1)+3] <- DD1/table_var[i,12*(zone-1)+2] 
    table_var[i,12*(zone-1)+4] <- DL1/table_var[i,12*(zone-1)+2]
    table_var[i,12*(zone-1)+5] <- P11/table_var[i,12*(zone-1)+2]
    table_var[i,12*(zone-1)+6] <- P21/table_var[i,12*(zone-1)+2]
    table_var[i,12*(zone-1)+7] <- P31/table_var[i,12*(zone-1)+2]
    
    # variance explained by each variables for model 2
    DD2 <- b2*(b2*var(St)+c2*cov(St,St_1)+d2*cov(St,P1t)+e2*cov(St,P2t)+f2*cov(St,P3t))
    DL2 <- c2*(c2*var(St_1)+b2*cov(St,St_1)+d2*cov(St_1,P1t)+e2*cov(St_1,P2t)+f2*cov(St_1,P3t))
    P12 <- d2*(d2*var(P1t)+b2*cov(St,P1t)+c2*cov(St_1,P1t)+e2*cov(P1t,P2t)+f2*cov(P1t,P3t))
    P22 <- e2*(e2*var(P2t)+b2*cov(St,P2t)+c2*cov(St_1,P2t)+d2*cov(P1t,P2t)+f2*cov(P2t,P3t))
    P32 <- f2*(f2*var(P3t)+b2*cov(St,P3t)+c2*cov(St_1,P3t)+d2*cov(P1t,P3t)+e2*cov(P2t,P3t))
    
    # total variance for model 2
    table_var[i,12*(zone-1)+8] <-  DD2+DL2+P12+P22+P32+sigma_carre_2 
    
    # proportion of variance explained for model 2
    table_var[i,12*(zone-1)+9] <-  DD2/table_var[i,12*(zone-1)+8]
    table_var[i,12*(zone-1)+10] <- DL2/table_var[i,12*(zone-1)+8]
    table_var[i,12*(zone-1)+11] <- P12/table_var[i,12*(zone-1)+8]
    table_var[i,12*(zone-1)+12] <- P22/table_var[i,12*(zone-1)+8]
    table_var[i,12*(zone-1)+13] <- P32/table_var[i,12*(zone-1)+8]
    
  
  # end of the year's loop
  }
  # end of the zone's loop 
}

# we can register our results  
write.csv (table_var, "../data/variances_explained_vole.csv", row.names = T, quote = F) 


#-----------------------------------------------------------------------------------------------------------------------------------------------------------------------

# III Variance and proportion of variance explained for predator's models 

# we open our parameters  

parameters <- read.csv("../data/parameters_predators.csv", sep=",", header=T, dec=".",stringsAsFactors=FALSE)

# we create our data frame for our results 
table_var_pred <- data.frame(matrix(0,21,37))
names(table_var_pred)<-c("year","VarY3n","a_1_3n","s_1_3n","a_2_3n","VarY4n","a_1_4n","s_1_4n","a_2_4n","VarY5n","s_5n","a_1_5n","s_1_5n",
                                "VarY3e","a_1_3e","s_1_3e","a_2_3e","VarY4e","a_1_4e","s_1_4e","a_2_4e","VarY5e","s_5e","a_1_5e","s_1_5e",
                                "VarY3w","a_1_3w","s_1_3w","a_2_3w","VarY4w","a_1_4w","s_1_4w","a_2_4w","VarY5w","s_5w","a_1_5w","s_1_5w")

# 1) Creation of density's vectors for calculated variance in the whole data set 
# we create our two vectors 
Stn <- data.frame(matrix(0,23,33))
Atn <- data.frame(matrix(0,23,33))

# we can fill them, one line per year and one localisation per column 
for (i in 0:32){ 
  
  for(j in 1:23){ 
     
    Stn[j,i+1] <- data[23*i+j,4]
    Atn[j,i+1] <- data[23*i+j,5]
    
  }
  
}

# missing values 
for (i2 in 0:32){
  
  # if the first data of a column is a NA, we replace it with the column's mean 
  if (is.na(Stn[1,i2+1])){
    Stn[1,i2+1] <- mean(Stn[,i2+1],na.rm=TRUE)
  }
  
  if (is.na(Atn[1,i2+1])){
    Atn[1,i2+1] <- mean(Atn[,i2+1],na.rm=TRUE)
  }
  
  
  for(j2 in 2:23){ 
    
    # then, if there are NA for other years, we use the "last observation carry forward"
    if (is.na(Stn[j2,i2+1])){
      Stn[j2,i2+1] <- Stn[j2-1,i2+1]
    }
    if (is.na(Atn[j2,i2+1])){
      Atn[j2,i2+1] <- Atn[j2-1,i2+1]
    }
    
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

# 2) Creation of  our loop for zones 
for (zone in 1:3){
  
# we put our estimators in variables
a3 <- parameters[3*(zone-1)+1,4]
b3 <- parameters[3*(zone-1)+1,5]
c3 <- parameters[3*(zone-1)+1,6]
d3 <- parameters[3*(zone-1)+1,7]
sigma_carre_3 <- parameters[3*(zone-1)+1,8]^2

a4 <- parameters[3*(zone-1)+2,4]
b4 <- parameters[3*(zone-1)+2,5]
c4 <- parameters[3*(zone-1)+2,6]
d4 <- parameters[3*(zone-1)+2,7]
sigma_carre_4 <- parameters[3*(zone-1)+2,8]^2

a5 <- parameters[3*(zone-1)+3,4]
b5 <- parameters[3*(zone-1)+3,5]
c5 <- parameters[3*(zone-1)+3,6]
d5 <- parameters[3*(zone-1)+3,7]
sigma_carre_5 <- parameters[3*(zone-1)+3,8]^2


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
  table_var_pred[i,1] <-1990 + i
  
  # variance explained by each variables for model 3
  V13 <- b3*(b3*var(At_1)+c3*cov(At_1,St_1)+d3*cov(At_1,At_2))
  V23 <- c3*(c3*var(St_1)+b3*cov(At_1,St_1)+d3*cov(At_2,St_1))
  V33 <- d3*(d3*var(At_2)+b3*cov(At_1,At_2)+c3*cov(At_2,St_1))
  
  # total variance for model 3
  table_var_pred[i,12*(zone-1)+2] <- V13+V23+V33+sigma_carre_3
  
  # proportion of variance explained for model 3
  table_var_pred[i,12*(zone-1)+3] <- V13/table_var_pred[i,12*(zone-1)+2] 
  table_var_pred[i,12*(zone-1)+4] <- V23/table_var_pred[i,12*(zone-1)+2]
  table_var_pred[i,12*(zone-1)+5] <- V33/table_var_pred[i,12*(zone-1)+2]
  
  # variance explained by each variables for model 4
  V14 <- b4*(b4*var(At_1)+c4*cov(At_1,St_1)+d4*cov(At_1,At_2))
  V24 <- c4*(c4*var(St_1)+b4*cov(At_1,St_1)+d4*cov(At_2,St_1))
  V34 <- d4*(d4*var(At_2)+b4*cov(At_1,At_2)+c4*cov(At_2,St_1))
  
  # total variance for model 4
  table_var_pred[i,12*(zone-1)+6] <- V14+V24+V34+sigma_carre_4
  
  # proportion of variance explained for model 4
  table_var_pred[i,12*(zone-1)+7] <- V14/table_var_pred[i,12*(zone-1)+6] 
  table_var_pred[i,12*(zone-1)+8] <- V24/table_var_pred[i,12*(zone-1)+6]
  table_var_pred[i,12*(zone-1)+9] <- V34/table_var_pred[i,12*(zone-1)+6]
  
  # variance explained by each variables for model 5
  V15 <- b5*(b5*var(St)+c5*cov(St,At_1)+d5*cov(St,St_1))
  V25 <- c5*(c5*var(At_1)+b5*cov(St,At_1)+d5*cov(St_1,At_1))
  V35 <- d5*(d5*var(St_1)+b5*cov(St,St_1)+c5*cov(St_1,At_1))
  
  # total variance for model 5
  table_var_pred[i,12*(zone-1)+10] <- V15+V25+V35+sigma_carre_5
  
  # proportion of variance explained for model 5
  table_var_pred[i,12*(zone-1)+11] <- V15/table_var_pred[i,12*(zone-1)+10] 
  table_var_pred[i,12*(zone-1)+12] <- V25/table_var_pred[i,12*(zone-1)+10]
  table_var_pred[i,12*(zone-1)+13] <- V35/table_var_pred[i,12*(zone-1)+10]
  
  # end of the year's loop
   }

# end of the zone's loop
}

# we register our results 
write.csv (table_var_pred, "../data/variances_explained_predators.csv", row.names = T, quote = F) 

