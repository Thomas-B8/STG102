# Besognet Thomas, 17/04/23 , Stage : Figures et tableaux 

# packages 
install.packages("ggplot2")                                       
library(ggplot2)
install.packages("dplyr")
library(dplyr)

# Figures et tableaux 

#---------------------------------------------------------------------------------------------------------------------------------------------------------------------

# Introduction : Présentation brute des données 

# Importation des données

donnees <- read.csv("donnees_propres_korpela.csv", sep=",", header=T, dec=".")

# A : le nord 

# A.1 Récupération des données 

donnees_nord <- filter(donnees,cardinalite=="nord")

Vole <- data.frame(matrix(0,46,2))
Must <- data.frame(matrix(0,23,2))
Gene <- data.frame(matrix(0,23,2))
Aeri <- data.frame(matrix(0,23,2))

for (i in 1:23){

Time <- filter(donnees_nord,year==1988+i) 

Vole[2*i-1,1] <- mean(na.omit(Time$Vole.Spring))
Vole[2*i,1] <- mean(na.omit(Time$Vole.Autumn))
Must[i,1] <- mean(na.omit(Time$Small.mustelid))
Gene[i,1] <- mean(na.omit(Time$Generalist.predator))
Aeri[i,1] <-mean(na.omit(Time$Avian.predator)) 

Vole[2*i-1,2] <- 1988 +i +0.25 # car printemps
Vole[2*i,2]  <- 1988 +i +0.75 # car automne 
Must[i,2] <- 1988+i
Gene[i,2] <- 1988+i
Aeri[i,2] <- 1988+i +0.5 # car été 

}

# A.2 les graphiques

# les campagnols 

vole_graph_n <- ggplot(data=Vole,aes(x=Vole[,2],y=Vole[,1]))+geom_line(color="darkgreen",size=1.5) +
 ggtitle("Densité de campagnols dans le nord") +
  xlab("année") + ylab("log densité ") + theme_classic()

vole_graph_n

# les petits mustélidés 

must_graph_n <- ggplot(data=Must,aes(x=Must[,2],y=Must[,1]))+geom_line(color="darkred",size=1.5) +
  ggtitle("Densité de petits mustélidés dans le nord") +
  xlab("année") + ylab("log densité ") + theme_classic()

must_graph_n

# les prédateurs généralistes 

gene_graph_n <- ggplot(data=Gene,aes(x=Gene[,2],y=Gene[,1]))+geom_line(color="orange",size=1.5) +
  ggtitle("Densité de prédateurs généralistes dans le nord") +
  xlab("année") + ylab("log densité ") + theme_classic()

gene_graph_n

# les prédateurs aériens 

must_aeri_n <- ggplot(data=Must,aes(x=Aeri[,2],y=Aeri[,1]))+geom_line(color="darkblue",size=1.5) +
  ggtitle("Densité de prédateurs aériens dans le nord") +
  xlab("année") + ylab("log densité ") + theme_classic()

aeri_graph_n

# B : l'est

# B.1 Récupération des données 

donnees_est <- filter(donnees,cardinalite=="est")

Vole <- data.frame(matrix(0,46,2))
Must <- data.frame(matrix(0,23,2))
Gene <- data.frame(matrix(0,23,2))
Aeri <- data.frame(matrix(0,23,2))

for (i in 1:23){
  
  Time <- filter(donnees_est,year==1988+i) 
  
  Vole[2*i-1,1] <- mean(na.omit(Time$Vole.Spring))
  Vole[2*i,1] <- mean(na.omit(Time$Vole.Autumn))
  Must[i,1] <- mean(na.omit(Time$Small.mustelid))
  Gene[i,1] <- mean(na.omit(Time$Generalist.predator))
  Aeri[i,1] <-mean(na.omit(Time$Avian.predator)) 
  
  Vole[2*i-1,2] <- 1988 +i +0.25 # car printemps
  Vole[2*i,2]  <- 1988 +i +0.75 # car automne 
  Must[i,2] <- 1988+i
  Gene[i,2] <- 1988+i
  Aeri[i,2] <- 1988+i +0.5 # car été 
  
}

# B.2 les graphiques

# les campagnols 

vole_graph_e <- ggplot(data=Vole,aes(x=Vole[,2],y=Vole[,1]))+geom_line(color="darkgreen",size=1.5) +
  ggtitle("Densité de campagnols dans l'est") +
  xlab("année") + ylab("log densité ") + theme_classic()

vole_graph_e

# les petits mustélidés 

must_graph_e <- ggplot(data=Must,aes(x=Must[,2],y=Must[,1]))+geom_line(color="darkred",size=1.5) +
  ggtitle("Densité de petits mustélidés dans l'est") +
  xlab("année") + ylab("log densité ") + theme_classic()

must_graph_e

# les prédateurs généralistes 

gene_graph_e <- ggplot(data=Gene,aes(x=Gene[,2],y=Gene[,1]))+geom_line(color="orange",size=1.5) +
  ggtitle("Densité de prédateurs généralistes dans l'est") +
  xlab("année") + ylab("log densité ") + theme_classic()

gene_graph_e

# les prédateurs aériens 

must_aeri_e <- ggplot(data=Must,aes(x=Aeri[,2],y=Aeri[,1]))+geom_line(color="darkblue",size=1.5) +
  ggtitle("Densité de prédateurs aériens dans l'est") +
  xlab("année") + ylab("log densité ") + theme_classic()

aeri_graph_e

# C : l'ouest

# C.1 Récupération des données 

donnees_ouest <- filter(donnees,cardinalite=="sud_ouest")

Vole <- data.frame(matrix(0,46,2))
Must <- data.frame(matrix(0,23,2))
Gene <- data.frame(matrix(0,23,2))
Aeri <- data.frame(matrix(0,23,2))

for (i in 1:23){
  
  Time <- filter(donnees_ouest,year==1988+i) 
  
  Vole[2*i-1,1] <- mean(na.omit(Time$Vole.Spring))
  Vole[2*i,1] <- mean(na.omit(Time$Vole.Autumn))
  Must[i,1] <- mean(na.omit(Time$Small.mustelid))
  Gene[i,1] <- mean(na.omit(Time$Generalist.predator))
  Aeri[i,1] <-mean(na.omit(Time$Avian.predator)) 
  
  Vole[2*i-1,2] <- 1988 +i +0.25 # car printemps
  Vole[2*i,2]  <- 1988 +i +0.75 # car automne 
  Must[i,2] <- 1988+i
  Gene[i,2] <- 1988+i
  Aeri[i,2] <- 1988+i +0.5 # car été 
  
}

# C.2 les graphiques

# les campagnols 

vole_graph_o <- ggplot(data=Vole,aes(x=Vole[,2],y=Vole[,1]))+geom_line(color="darkgreen",size=1.5) +
  ggtitle("Densité de campagnols dans l'ouest") +
  xlab("année") + ylab("log densité ") + theme_classic()

vole_graph_o

# les petits mustélidés 

must_graph_o <- ggplot(data=Must,aes(x=Must[,2],y=Must[,1]))+geom_line(color="darkred",size=1.5) +
  ggtitle("Densité de petits mustélidés dans l'ouest") +
  xlab("année") + ylab("log densité ") + theme_classic()

must_graph_o

# les prédateurs généralistes 

gene_graph_o <- ggplot(data=Gene,aes(x=Gene[,2],y=Gene[,1]))+geom_line(color="orange",size=1.5) +
  ggtitle("Densité de prédateurs généralistes dans l'ouest") +
  xlab("année") + ylab("log densité ") + theme_classic()

gene_graph_o

# les prédateurs aériens 

must_aeri_o <- ggplot(data=Must,aes(x=Aeri[,2],y=Aeri[,1]))+geom_line(color="darkblue",size=1.5) +
  ggtitle("Densité de prédateurs aériens dans l'ouest") +
  xlab("année") + ylab("log densité ") + theme_classic()

aeri_graph_o

#----------------------------------------------------------------------------------------------------------------------------------

# I Réplication de la figure 2

# Importation des tableaux de variance expliquées 

nord <- read.csv("variances_expliquees_nord2.csv", sep=",", header=T, dec=".", stringsAsFactors=FALSE)
est <- read.csv("variances_expliquees_est2.csv", sep=",", header=T, dec=".", stringsAsFactors=FALSE)
ouest <- read.csv("variances_expliquees_ouest2.csv", sep=",", header=T, dec=".", stringsAsFactors=FALSE)

# Creation des graphiques avec ggplot2 

# A.1 modele 1 pour le nord 

vect1n <- c(1:105)
a <-0

for (j in 1:5){
  for (i in 1:21){
    a <- a+1
    vect1n[a]<- nord[i,3+j]
  }
}

df1_nord <-  data.frame(variable=rep(c("5_direct DD", "4_delayed DD", "3_small mustelids","2_generalists","1_avian predator"), each=21),
                              annee=rep(c(1991,1992,1993,1994,1995,1996,1997,1998,1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011),5),
                              variances=vect1n)

graph1_nord <- ggplot(data=df1_nord, aes(x=annee, y=variances, fill=variable)) +
  geom_bar(stat="identity") + scale_fill_manual(values=c("#6699CC", "#FFFF33", "#FF0033", "#333333" , "#000000")) +
  ggtitle("proportion de la variance du taux de croissance des campagnols en HIVER expliquée selon différentes variables au NORD de la Finlande ") +
  xlab("année") + ylab("proportion de la variance expliquée") + theme_classic()

graph1_nord

# A.2 modele 2 pour le nord 

vect2n <- c(1:105)
a <-0

for (j in 1:5){
  for (i in 1:21){
    a <- a+1
    vect2n[a]<- nord[i,9+j]
  }
}

df2_nord <-  data.frame(variable=rep(c("5_direct DD", "4_delayed DD", "3_small mustelids","2_generalists","1_avian predator"), each=21),
                        annee=rep(c(1991,1992,1993,1994,1995,1996,1997,1998,1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011),5),
                        variances=vect2n)

graph2_nord <- ggplot(data=df2_nord, aes(x=annee, y=variances, fill=variable)) +
  geom_bar(stat="identity") + scale_fill_manual(values=c("#6699CC", "#FFFF33", "#FF0033", "#333333" , "#000000")) +
  ggtitle("proportion de la variance du taux de croissance des campagnols en ETE expliquée selon différentes variables au NORD de la Finlande ") +
  xlab("année") + ylab("proportion de la variance expliquée") + theme_classic()

graph2_nord


# B.1 modele 1 pour l'ouest

vect1w <- c(1:105)
a <-0

for (j in 1:5){
  for (i in 1:21){
    a <- a+1
    vect1w[a]<- ouest[i,3+j]
  }
}

df1_ouest <-  data.frame(variable=rep(c("5_direct DD", "4_delayed DD", "3_small mustelids","2_generalists","1_avian predator"), each=21),
                        annee=rep(c(1991,1992,1993,1994,1995,1996,1997,1998,1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011),5),
                        variances=vect1w)

graph1_ouest <- ggplot(data=df1_ouest, aes(x=annee, y=variances, fill=variable)) +
  geom_bar(stat="identity") + scale_fill_manual(values=c("#6699CC", "#FFFF33", "#FF0033", "#333333" , "#000000")) +
  ggtitle("proportion de la variance du taux de croissance des campagnols en HIVER expliquée selon différentes variables à l'OUEST de la Finlande ") +
  xlab("année") + ylab("proportion de la variance expliquée") + theme_classic()

graph1_ouest

# B.2 modele 2 pour l'ouest

vect2w <- c(1:105)
a <-0

for (j in 1:5){
  for (i in 1:21){
    a <- a+1
    vect2w[a]<- ouest[i,9+j]
  }
}

df2_ouest <-  data.frame(variable=rep(c("5_direct DD", "4_delayed DD", "3_small mustelids","2_generalists","1_avian predator"), each=21),
                        annee=rep(c(1991,1992,1993,1994,1995,1996,1997,1998,1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011),5),
                        variances=vect2w)

graph2_ouest <- ggplot(data=df2_ouest, aes(x=annee, y=variances, fill=variable)) +
  geom_bar(stat="identity") + scale_fill_manual(values=c("#6699CC", "#FFFF33", "#FF0033", "#333333" , "#000000")) +
  ggtitle("proportion de la variance du taux de croissance des campagnols en ETE expliquée selon différentes variables à l'OUEST de la Finlande ") +
  xlab("année") + ylab("proportion de la variance expliquée") + theme_classic()

graph2_ouest


# C.1 modele 1 pour l'est

vect1e <- c(1:105)
a <-0

for (j in 1:5){
  for (i in 1:21){
    a <- a+1
    vect1e[a]<- est[i,3+j]
  }
}

df1_est <-  data.frame(variable=rep(c("5_direct DD", "4_delayed DD", "3_small mustelids","2_generalists","1_avian predator"), each=21),
                        annee=rep(c(1991,1992,1993,1994,1995,1996,1997,1998,1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011),5),
                        variances=vect1e)

graph1_est <- ggplot(data=df1_est, aes(x=annee, y=variances, fill=variable)) +
  geom_bar(stat="identity") + scale_fill_manual(values=c("#6699CC", "#FFFF33", "#FF0033", "#333333" , "#000000")) +
  ggtitle("proportion de la variance du taux de croissance des campagnols en HIVER expliquée selon différentes variables à l'EST de la Finlande ") +
  xlab("année") + ylab("proportion de la variance expliquée") + theme_classic()

graph1_est

# C.2 modele 2 pour l'est

vect2e <- c(1:105)
a <-0

for (j in 1:5){
  for (i in 1:21){
    a <- a+1
    vect2e[a]<- est[i,9+j]
  }
}

df2_est <-  data.frame(variable=rep(c("5_direct DD", "4_delayed DD", "3_small mustelids","2_generalists","1_avian predator"), each=21),
                        annee=rep(c(1991,1992,1993,1994,1995,1996,1997,1998,1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011),5),
                        variances=vect2e)

graph2_est <- ggplot(data=df2_est, aes(x=annee, y=variances, fill=variable)) +
  geom_bar(stat="identity") + scale_fill_manual(values=c("#6699CC", "#FFFF33", "#FF0033", "#333333" , "#000000")) +
  ggtitle("proportion de la variance du taux de croissance des campagnols en ETE expliquée selon différentes variables à l'EST de la Finlande ") +
  xlab("année") + ylab("proportion de la variance expliquée") + theme_classic()

graph2_est

#-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

# II Réplication de la figure 3

#-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

# III Réplication de la figure 4

# les données ne sont pas encore disponibles, donc je vais créer un tableua fictif pour travailler dessus 

simulations <- read.csv("fausses_donnees_figure4.csv",sep=",", header=T, dec=".", stringsAsFactors=FALSE)
simulations$scenario <- as.factor(simulations$scenario)

# A) density dependance 

# A.1 le nord 

simulations_nord <- filter(simulations,region==1)

ggplot(simulations_nord) +
  geom_point(aes(x = DRD,y = DLD,color = scenario, size = 0.5))+
  scale_color_manual(values=c('black','pink','red',"yellow","orange","darkgreen","darkblue","purple"))+
  ggtitle("dependance de densités au NORD de la Finlande sous différents scénarios ") +
  xlab("direct density dependance ") + ylab("delayed density dependance")+ theme_classic()

# A.2 le sud ouest 

simulations_ouest <- filter(simulations,region==2)

ggplot(simulations_ouest) +
  geom_point(aes(x = DRD,y = DLD,color = scenario, size = 0.5))+
  scale_color_manual(values=c('black','pink','red',"yellow","orange","darkgreen","darkblue","purple"))+
  ggtitle("dependance de densités à l'OUEST de la Finlande sous différents scénarios ") +
  xlab("direct density dependance ") + ylab("delayed density dependance")+ theme_classic()

# A.3 l'est

simulations_est <- filter(simulations,region==3)

ggplot(simulations_est) +
  geom_point(aes(x = DRD,y = DLD,color = scenario, size = 0.5))+
  scale_color_manual(values=c('black','pink','red',"yellow","orange","darkgreen","darkblue","purple"))+
  ggtitle("dependance de densités à l'EST de la Finlande sous différents scénarios ") +
  xlab("direct density dependance ") + ylab("delayed density dependance")+ theme_classic()

# B) s-index

# B.1 le nord 

ggplot(simulations_nord, aes(x=scenario, y=s_index)) +
  geom_segment( aes(x=1, xend=scenario, y=0, yend=s_index),linetype="blank") +
  geom_point(aes(x = scenario,y = s_index,color = scenario, size = 0.5))+
  geom_errorbar(aes(ymin = min_s, ymax = max_s), colour="black", width=0.1 ) +
  scale_color_manual(values=c('black','pink','red',"yellow","orange","darkgreen","darkblue","purple"))+
  ggtitle("le s-index des prédictions de densité de campagnols au NORD de la Finlande sous différents scénarios ") +
  xlab("scenario") + ylab("s-index")+ theme_classic()

# B.2 le sud ouest

ggplot(simulations_ouest, aes(x=scenario, y=s_index)) +
  geom_segment( aes(x=1, xend=scenario, y=0, yend=s_index),linetype="blank") +
  geom_point(aes(x = scenario,y = s_index,color = scenario, size = 0.5))+
  geom_errorbar(aes(ymin = min_s, ymax = max_s), colour="black", width=0.1 ) +
  scale_color_manual(values=c('black','pink','red',"yellow","orange","darkgreen","darkblue","purple"))+
  ggtitle("le s-index des prédictions de densité de campagnols à l'OUEST de la Finlande sous différents scénarios ") +
  xlab("scenario") + ylab("s-index")+ theme_classic()

# B.3 l'est  

ggplot(simulations_est, aes(x=scenario, y=s_index)) +
  geom_segment( aes(x=1, xend=scenario, y=0, yend=s_index),linetype="blank") +
  geom_point(aes(x = scenario,y = s_index,color = scenario, size = 0.5))+
  geom_errorbar(aes(ymin = min_s, ymax = max_s), colour="black", width=0.1 ) +
  scale_color_manual(values=c('black','pink','red',"yellow","orange","darkgreen","darkblue","purple"))+
  ggtitle("le s-index des prédictions de densité de campagnols à l'EST de la Finlande sous différents scénarios ") +
  xlab("scenario") + ylab("s-index")+ theme_classic()

# C) saisonnalite

# C.1 le nord 

ggplot(simulations_nord, aes(x=scenario, y=seasonality)) +
  geom_segment( aes(x=1, xend=scenario, y=0, yend=seasonality),linetype="blank") +
  geom_point(aes(x = scenario,y = seasonality,color = scenario, size = 0.5))+
  geom_errorbar(aes(ymin = min_n, ymax = max_n), colour="black", width=0.1 ) +
  scale_color_manual(values=c('black','pink','red',"yellow","orange","darkgreen","darkblue","purple"))+
  ggtitle("la saisonnalite des prédictions de densité de campagnols au NORD de la Finlande sous différents scénarios ") +
  xlab("scenario") + ylab("saisonnalite")+ theme_classic()

# C.2 le sud ouest

ggplot(simulations_ouest, aes(x=scenario, y=seasonality)) +
  geom_segment( aes(x=1, xend=scenario, y=0, yend=seasonality),linetype="blank") +
  geom_point(aes(x = scenario,y = seasonality,color = scenario, size = 0.5))+
  geom_errorbar(aes(ymin = min_n, ymax = max_n), colour="black", width=0.1 ) +
  scale_color_manual(values=c('black','pink','red',"yellow","orange","darkgreen","darkblue","purple"))+
  ggtitle("la saisonnalite des prédictions de densité de campagnols à l'OUEST de la Finlande sous différents scénarios ") +
  xlab("scenario") + ylab("saisonnalite")+ theme_classic()

# C.3 l'est  

ggplot(simulations_est, aes(x=scenario, y=seasonality)) +
  geom_segment( aes(x=1, xend=scenario, y=0, yend=seasonality),linetype="blank") +
  geom_point(aes(x = scenario,y = seasonality,color = scenario, size = 0.5))+
  geom_errorbar(aes(ymin = min_n, ymax = max_n), colour="black", width=0.1 ) +
  scale_color_manual(values=c('black','pink','red',"yellow","orange","darkgreen","darkblue","purple"))+
  ggtitle("la saisonnalite des prédictions de densité de campagnols à l'EST de la Finlande sous différents scénarios ") +
  xlab("scenario") + ylab("saisonnalite")+ theme_classic()





