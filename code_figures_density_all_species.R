# Besognet Thomas, 17/05/23 , Stage : Figures et tableaux : densités moyennes par région des différentes espéces 

# packages 
install.packages("ggplot2")                                       
library(ggplot2)
install.packages("dplyr")
library(dplyr)
install.packages("cowplot")
library(cowplot)
install.packages("ggplotify")
library(ggplotify)

# Figures et tableaux 

# Densités moyennes des différentes espéces par régions 

#---------------------------------------------------------------------------------------------------------------------------------------------------------------------

# Importation des données

donnees <- read.csv("donnees_propres_korpela.csv", sep=",", header=T, dec=".")

# A : le nord 

# A.1 Récupération des données 

donnees_nord <- filter(donnees,cardinalite=="nord")

Vole_n <- data.frame(matrix(0,46,2))
Must_n <- data.frame(matrix(0,23,2))
Gene_n <- data.frame(matrix(0,23,2))
Aeri_n <- data.frame(matrix(0,23,2))

for (i in 1:23){

Time_n <- filter(donnees_nord,year==1988+i) 

Vole_n[2*i-1,1] <- mean(na.omit(Time_n$Vole.Spring))
Vole_n[2*i,1] <- mean(na.omit(Time_n$Vole.Autumn))
Must_n[i,1] <- mean(na.omit(Time_n$Small.mustelid))
Gene_n[i,1] <- mean(na.omit(Time_n$Generalist.predator))
Aeri_n[i,1] <-mean(na.omit(Time_n$Avian.predator)) 

Vole_n[2*i-1,2] <- 1988 +i +0.25 # car printemps
Vole_n[2*i,2]  <- 1988 +i +0.75 # car automne 
Must_n[i,2] <- 1988+i
Gene_n[i,2] <- 1988+i
Aeri_n[i,2] <- 1988+i +0.5 # car été 

}

# A.2 les graphiques

# les campagnols 

vole_graph_n <- ggplot(data=Vole_n,aes(x=Vole_n[,2],y=Vole_n[,1]))+geom_line(color="darkgreen",size=1.5) +
 ggtitle("Vole_north") +
  xlab("years") + ylab("log density") + theme_classic()

vole_graph_n

# les petits mustélidés 

must_graph_n <- ggplot(data=Must_n,aes(x=Must_n[,2],y=Must_n[,1]))+geom_line(color="darkred",size=1.5) +
  ggtitle("Small_mustelids_north") +
  xlab("years") + ylab("log density") + theme_classic()

must_graph_n

# les prédateurs généralistes 

gene_graph_n <- ggplot(data=Gene_n,aes(x=Gene_n[,2],y=Gene_n[,1]))+geom_line(color="orange",size=1.5) +
  ggtitle("Fox_and_pine_marten_north") +
  xlab("years") + ylab("log density") + theme_classic()

gene_graph_n

# les prédateurs aériens 

aeri_graph_n <- ggplot(data=Aeri_n,aes(x=Aeri_n[,2],y=Aeri_n[,1]))+geom_line(color="darkblue",size=1.5) +
  ggtitle("Avian_predators_north") +
  xlab("years") + ylab("log density") + theme_classic()

aeri_graph_n

# B : l'est

# B.1 Récupération des données 

donnees_est <- filter(donnees,cardinalite=="est")

Vole_e <- data.frame(matrix(0,46,2))
Must_e <- data.frame(matrix(0,23,2))
Gene_e <- data.frame(matrix(0,23,2))
Aeri_e <- data.frame(matrix(0,23,2))

for (i in 1:23){
  
  Time_e <- filter(donnees_est,year==1988+i) 
  
  Vole_e[2*i-1,1] <- mean(na.omit(Time_e$Vole.Spring))
  Vole_e[2*i,1] <- mean(na.omit(Time_e$Vole.Autumn))
  Must_e[i,1] <- mean(na.omit(Time_e$Small.mustelid))
  Gene_e[i,1] <- mean(na.omit(Time_e$Generalist.predator))
  Aeri_e[i,1] <-mean(na.omit(Time_e$Avian.predator)) 
  
  Vole_e[2*i-1,2] <- 1988 +i +0.25 # car printemps
  Vole_e[2*i,2]  <- 1988 +i +0.75 # car automne 
  Must_e[i,2] <- 1988+i
  Gene_e[i,2] <- 1988+i
  Aeri_e[i,2] <- 1988+i +0.5 # car été 
  
}

# B.2 les graphiques

# les campagnols 

vole_graph_e <- ggplot(data=Vole_e,aes(x=Vole_e[,2],y=Vole_e[,1]))+geom_line(color="darkgreen",size=1.5) +
  ggtitle("Vole_east") +
  xlab("years") + ylab("log density") + theme_classic()

vole_graph_e

# les petits mustélidés 

must_graph_e <- ggplot(data=Must_e,aes(x=Must_e[,2],y=Must_e[,1]))+geom_line(color="darkred",size=1.5) +
  ggtitle("Small_mustelids_east") +
  xlab("years") + ylab("log density") + theme_classic()

must_graph_e

# les prédateurs généralistes 

gene_graph_e <- ggplot(data=Gene_e,aes(x=Gene_e[,2],y=Gene_e[,1]))+geom_line(color="orange",size=1.5) +
  ggtitle("Fox_and_pine_marten_east") +
  xlab("years") + ylab("log density") + theme_classic()

gene_graph_e

# les prédateurs aériens 

aeri_graph_e <- ggplot(data=Aeri_e,aes(x=Aeri_e[,2],y=Aeri_e[,1]))+geom_line(color="darkblue",size=1.5) +
  ggtitle("Avian_predators_east") +
  xlab("years") + ylab("log density") + theme_classic()

aeri_graph_e

# C : l'ouest

# C.1 Récupération des données 

donnees_ouest <- filter(donnees,cardinalite=="sud_ouest")

Vole_o <- data.frame(matrix(0,46,2))
Must_o <- data.frame(matrix(0,23,2))
Gene_o <- data.frame(matrix(0,23,2))
Aeri_o <- data.frame(matrix(0,23,2))

for (i in 1:23){
  
  Time_o <- filter(donnees_ouest,year==1988+i) 
  
  Vole_o[2*i-1,1] <- mean(na.omit(Time_o$Vole.Spring))
  Vole_o[2*i,1] <- mean(na.omit(Time_o$Vole.Autumn))
  Must_o[i,1] <- mean(na.omit(Time_o$Small.mustelid))
  Gene_o[i,1] <- mean(na.omit(Time_o$Generalist.predator))
  Aeri_o[i,1] <-mean(na.omit(Time_o$Avian.predator)) 
  
  Vole_o[2*i-1,2] <- 1988 +i +0.25 # car printemps
  Vole_o[2*i,2]  <- 1988 +i +0.75 # car automne 
  Must_o[i,2] <- 1988+i
  Gene_o[i,2] <- 1988+i
  Aeri_o[i,2] <- 1988+i +0.5 # car été 
  
}

# C.2 les graphiques

# les campagnols 

vole_graph_o <- ggplot(data=Vole_o,aes(x=Vole_o[,2],y=Vole_o[,1]))+geom_line(color="darkgreen",size=1.5) +
  ggtitle("Vole_west") +
  xlab("years") + ylab("log density") + theme_classic()

vole_graph_o

# les petits mustélidés 

must_graph_o <- ggplot(data=Must_o,aes(x=Must_o[,2],y=Must_o[,1]))+geom_line(color="darkred",size=1.5) +
  ggtitle("Small_mustelids_west") +
  xlab("years") + ylab("log density") + theme_classic()

must_graph_o

# les prédateurs généralistes 

gene_graph_o <- ggplot(data=Gene_o,aes(x=Gene_o[,2],y=Gene_o[,1]))+geom_line(color="orange",size=1.5) +
  ggtitle("Fox_and_pine_marten_west") +
  xlab("years") + ylab("log density") + theme_classic()

gene_graph_o

# les prédateurs aériens 

aeri_graph_o <- ggplot(data=Aeri_o,aes(x=Aeri_o[,2],y=Aeri_o[,1]))+geom_line(color="darkblue",size=1.5) +
  ggtitle("Avian_predators_west") +
  xlab("years") + ylab("log density") + theme_classic()

aeri_graph_o

# faire apparaitre tous les graphiques sur la même figure et l'enregistrer 

plot_grid(vole_graph_n,vole_graph_o,vole_graph_e,must_graph_n,must_graph_o,must_graph_e,gene_graph_n,gene_graph_o,gene_graph_e,aeri_graph_n,aeri_graph_o,aeri_graph_e, labels=c("1", "2","3","4","5","6","7","8","9","10","11","12"), ncol = 3, nrow = 4) 

