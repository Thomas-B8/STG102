# Besognet Thomas, 17/04/23 , Stage : Figures et tableaux 

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

#---------------------------------------------------------------------------------------------------------------------------------------------------------------------

# Introduction : Présentation brute des données 

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

# faire apparaitre tous les graphiques sur la même figure 

plot_grid(vole_graph_n,vole_graph_o,vole_graph_e,must_graph_n,must_graph_o,must_graph_e,gene_graph_n,gene_graph_o,gene_graph_e,aeri_graph_n,aeri_graph_o,aeri_graph_e, labels=c("1", "2","3","4","5","6","7","8","9","10","11","12"), ncol = 3, nrow = 4) 

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

df1_nord <-  data.frame(variable=rep(c("5_direct DD", "4_delayed DD", "3_small_mustelids","2_generalists","1_avian_predators"), each=21),
                              annee=rep(c(1991,1992,1993,1994,1995,1996,1997,1998,1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011),5),
                              variances=vect1n)

graph1_nord <- ggplot(data=df1_nord, aes(x=annee, y=variances, fill=variable)) +
  geom_bar(stat="identity") + scale_fill_manual(values=c("#6699CC", "#FFFF33", "#FF0033", "gray30" , "gray0")) +
  ggtitle("winter-north") +
  xlab("years") + ylab("proportion of variance explained") + theme_classic()

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

df2_nord <-  data.frame(variable=rep(c("5_direct DD", "4_delayed DD", "3_small_mustelids","2_generalists","1_avian_predators"), each=21),
                        annee=rep(c(1991,1992,1993,1994,1995,1996,1997,1998,1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011),5),
                        variances=vect2n)

graph2_nord <- ggplot(data=df2_nord, aes(x=annee, y=variances, fill=variable)) +
  geom_bar(stat="identity") + scale_fill_manual(values=c("#6699CC", "#FFFF33", "#FF0033", "gray30" , "gray0")) +
  ggtitle("summer_north") +
  xlab("years") + ylab("proportion of variance explained") + theme_classic()

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

df1_ouest <-  data.frame(variable=rep(c("5_direct DD", "4_delayed DD", "3_small_mustelids","2_generalists","1_avian_predators"), each=21),
                        annee=rep(c(1991,1992,1993,1994,1995,1996,1997,1998,1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011),5),
                        variances=vect1w)

graph1_ouest <- ggplot(data=df1_ouest, aes(x=annee, y=variances, fill=variable)) +
  geom_bar(stat="identity") + scale_fill_manual(values=c("#6699CC", "#FFFF33", "#FF0033", "gray30" , "gray0")) +
  ggtitle("winter-west") +
  xlab("years") + ylab("proportion of variance explained") + theme_classic()

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

df2_ouest <-  data.frame(variable=rep(c("5_direct DD", "4_delayed DD", "3_small_mustelids","2_generalists","1_avian_predators"), each=21),
                        annee=rep(c(1991,1992,1993,1994,1995,1996,1997,1998,1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011),5),
                        variances=vect2w)

graph2_ouest <- ggplot(data=df2_ouest, aes(x=annee, y=variances, fill=variable)) +
  geom_bar(stat="identity") + scale_fill_manual(values=c("#6699CC", "#FFFF33", "#FF0033", "gray30" , "gray0")) +
  ggtitle("summer-west") +
  xlab("years") + ylab("proportion of variance explained") + theme_classic()

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
  geom_bar(stat="identity") + scale_fill_manual(values=c("#6699CC", "#FFFF33", "#FF0033", "gray30" , "gray0")) +
  ggtitle("winter-east") +
  xlab("years") + ylab("proportion of variance explained") + theme_classic()

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
  geom_bar(stat="identity") + scale_fill_manual(values=c("#6699CC", "#FFFF33", "#FF0033", "gray30" , "gray0")) +
  ggtitle("summer-east") +
  xlab("years") + ylab("proportion of variance explained") + theme_classic()

graph2_est

# tous les graphiques résumés 

plot_grid(graph1_nord,graph1_ouest,graph1_est,graph2_nord,graph2_ouest,graph2_est, labels=c("1", "2","3","4","5","6"), ncol = 3, nrow = 2)

#-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

# II Réplication de la figure 3

# I Réplication de la figure 2

# Importation des tableaux de variance expliquées 

nord <- read.csv("variances_expliquees_pred_nord2.csv", sep=",", header=T, dec=".", stringsAsFactors=FALSE)
est <- read.csv("variances_expliquees_pred_est2.csv", sep=",", header=T, dec=".", stringsAsFactors=FALSE)
ouest <- read.csv("variances_expliquees_pred_ouest2.csv", sep=",", header=T, dec=".", stringsAsFactors=FALSE)

# Creation des graphiques avec ggplot2 

# A.1 modele 3 pour le nord 

vect3n <- c(1:63)
a <-0

for (j in 1:3){
  for (i in 1:21){
    a <- a+1
    vect3n[a]<- nord[i,3+j]
  }
}

df3_nord <-  data.frame(variable=rep(c("3_autumn_t-1", "2_spring_t-1", "1_autumn_t-2"), each=21),
                        annee=rep(c(1991,1992,1993,1994,1995,1996,1997,1998,1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011),3),
                        variances=vect3n)

graph3_nord <- ggplot(data=df3_nord, aes(x=annee, y=variances, fill=variable)) +
  geom_bar(stat="identity") + scale_fill_manual(values=c("gray60", "gray30", "gray0")) +
  ggtitle("small_mustelids_north") +
  xlab("years") + ylab("proportion of variance explained") + theme_classic()

graph3_nord

# A.2 modele 4 pour le nord 

vect4n <- c(1:63)
a <-0

for (j in 1:3){
  for (i in 1:21){
    a <- a+1
    vect4n[a]<- nord[i,7+j]
  }
}

df4_nord <-  data.frame(variable=rep(c("3_autumn_t-1", "2_spring_t-1", "1_autumn_t-2"), each=21),
                        annee=rep(c(1991,1992,1993,1994,1995,1996,1997,1998,1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011),3),
                        variances=vect4n)

graph4_nord <- ggplot(data=df4_nord, aes(x=annee, y=variances, fill=variable)) +
  geom_bar(stat="identity") + scale_fill_manual(values=c("gray60", "gray30", "gray0")) +
  ggtitle("fox_and_pine_marten_north") +
  xlab("years") + ylab("proportion of variance explained") + theme_classic()

graph4_nord

# A.3 modele 5 pour le nord 

vect5n <- c(1:63)
a <-0

for (j in 1:3){
  for (i in 1:21){
    a <- a+1
    vect5n[a]<- nord[i,11+j]
  }
}

df5_nord <-  data.frame(variable=rep(c("3_spring_t", "2_autumn_t-1", "1_spring_t-1"), each=21),
                        annee=rep(c(1991,1992,1993,1994,1995,1996,1997,1998,1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011),3),
                        variances=vect5n)

graph5_nord <- ggplot(data=df5_nord, aes(x=annee, y=variances, fill=variable)) +
  geom_bar(stat="identity") + scale_fill_manual(values=c("gray60", "gray30", "gray0")) +
  ggtitle("avian_predators_north") +
  xlab("years") + ylab("proportion of variance explained") + theme_classic()

graph5_nord

# B.1 modele 3 pour l'ouest 

vect3o <- c(1:63)
a <-0

for (j in 1:3){
  for (i in 1:21){
    a <- a+1
    vect3o[a]<- ouest[i,3+j]
  }
}

df3_ouest <-  data.frame(variable=rep(c("3_autumn_t-1", "2_spring_t-1", "1_autumn_t-2"), each=21),
                        annee=rep(c(1991,1992,1993,1994,1995,1996,1997,1998,1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011),3),
                        variances=vect3o)

graph3_ouest <- ggplot(data=df3_ouest, aes(x=annee, y=variances, fill=variable)) +
  geom_bar(stat="identity") + scale_fill_manual(values=c("gray60", "gray30", "gray0")) +
  ggtitle("small_mustelids_west") +
  xlab("years") + ylab("proportion of variance explained") + theme_classic()

graph3_ouest

# B.2 modele 4 pour l'ouest 

vect4o <- c(1:63)
a <-0

for (j in 1:3){
  for (i in 1:21){
    a <- a+1
    vect4o[a]<- ouest[i,7+j]
  }
}

df4_ouest <-  data.frame(variable=rep(c("3_autumn_t-1", "2_spring_t-1", "1_autumn_t-2"), each=21),
                        annee=rep(c(1991,1992,1993,1994,1995,1996,1997,1998,1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011),3),
                        variances=vect4o)

graph4_ouest <- ggplot(data=df4_ouest, aes(x=annee, y=variances, fill=variable)) +
  geom_bar(stat="identity") + scale_fill_manual(values=c("gray60", "gray30", "gray0")) +
  ggtitle("fox_and_pine_marten_west") +
  xlab("years") + ylab("proportion of variance explained") + theme_classic()

graph4_ouest

# B.3 modele 5 pour l'ouest

vect5o <- c(1:63)
a <-0

for (j in 1:3){
  for (i in 1:21){
    a <- a+1
    vect5o[a]<- ouest[i,11+j]
  }
}

df5_ouest <-  data.frame(variable=rep(c("3_spring_t", "2_autumn_t-1", "1_spring_t-1"), each=21),
                        annee=rep(c(1991,1992,1993,1994,1995,1996,1997,1998,1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011),3),
                        variances=vect5o)

graph5_ouest <- ggplot(data=df5_ouest, aes(x=annee, y=variances, fill=variable)) +
  geom_bar(stat="identity") + scale_fill_manual(values=c("gray60", "gray30", "gray0")) +
  ggtitle("avian_predators_west") +
  xlab("years") + ylab("proportion of variance explained") + theme_classic()

graph5_ouest

# C.1 modele 3 pour l'est

vect3e <- c(1:63)
a <-0

for (j in 1:3){
  for (i in 1:21){
    a <- a+1
    vect3e[a]<- est[i,3+j]
  }
}

df3_est <-  data.frame(variable=rep(c("3_autumn_t-1", "2_spring_t-1", "1_autumn_t-2"), each=21),
                        annee=rep(c(1991,1992,1993,1994,1995,1996,1997,1998,1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011),3),
                        variances=vect3e)

graph3_est <- ggplot(data=df3_est, aes(x=annee, y=variances, fill=variable)) +
  geom_bar(stat="identity") + scale_fill_manual(values=c("gray60", "gray30", "gray0")) +
  ggtitle("small_mustelids_east") +
  xlab("years") + ylab("proportion of variance explained") + theme_classic()

graph3_est

# C.2 modele 4 pour l'est

vect4e <- c(1:63)
a <-0

for (j in 1:3){
  for (i in 1:21){
    a <- a+1
    vect4e[a]<- nord[i,7+j]
  }
}

df4_est <-  data.frame(variable=rep(c("3_autumn_t-1", "2_spring_t-1", "1_autumn_t-2"), each=21),
                        annee=rep(c(1991,1992,1993,1994,1995,1996,1997,1998,1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011),3),
                        variances=vect4e)

graph4_est <- ggplot(data=df4_est, aes(x=annee, y=variances, fill=variable)) +
  geom_bar(stat="identity") + scale_fill_manual(values=c("gray60", "gray30", "gray0")) +
  ggtitle("fox_and_pine_marten_east") +
  xlab("years") + ylab("proportion of variance explained") + theme_classic()

graph4_est

# C.3 modele 5 pour l'est

vect5e <- c(1:63)
a <-0

for (j in 1:3){
  for (i in 1:21){
    a <- a+1
    vect5e[a]<- nord[i,11+j]
  }
}

df5_est <-  data.frame(variable=rep(c("3_spring_t", "2_autumn_t-1", "1_spring_t-1"), each=21),
                        annee=rep(c(1991,1992,1993,1994,1995,1996,1997,1998,1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011),3),
                        variances=vect5e)

graph5_est <- ggplot(data=df5_est, aes(x=annee, y=variances, fill=variable)) +
  geom_bar(stat="identity") + scale_fill_manual(values=c("gray60", "gray30", "gray0")) +
  ggtitle("avian_predators_east") +
  xlab("years") + ylab("proportion of variance explained") + theme_classic()

graph5_est

# tous les graphiques résumés 

plot_grid(graph3_nord,graph3_ouest,graph3_est,graph4_nord,graph4_ouest,graph4_est,graph5_nord,graph5_ouest,graph5_est, labels=c("1", "2","3","4","5","6","7","8","9"), ncol = 3, nrow = 3)
#-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

# III Réplication de la figure 4

# les données ne sont pas encore disponibles, donc je vais créer un tableua fictif pour travailler dessus 

simulations <- read.csv("simulations.csv",sep=",", header=T, dec=".", stringsAsFactors=FALSE)
simulations$scenario <- as.factor(simulations$scenario)

# A) density dependance 

# A.1 le nord 

simulations_nord <- filter(simulations,region==1)

ggplot(simulations_nord) +
  geom_point(aes(x = direct.density.dependance,y = delayed.density.dependance,color = scenario, size = 0.5))+
  scale_color_manual(values=c('black','pink','red',"yellow","orange","darkgreen","darkblue","purple"))+
  ggtitle("dependance de densités au NORD de la Finlande sous différents scénarios ") +
  xlab("direct density dependance ") + ylab("delayed density dependance")+ theme_classic()

# triangle ? 
abline(a = -1, b = 0, lty="dashed")
abline(a = 1, b = -1, lty="dashed")
polygon(x = phi1[6:46], y = 1-abs(phi1[6:46]), col="gray")
lines(phi1,-phi1^2/4)

# A.2 le sud ouest 

simulations_ouest <- filter(simulations,region==3)

ggplot(simulations_ouest) +
  geom_point(aes(x =direct.density.dependance,y =delayed.density.dependance,color = scenario, size = 0.5))+
  scale_color_manual(values=c('black','pink','red',"yellow","orange","darkgreen","darkblue","purple"))+
  ggtitle("dependance de densités à l'OUEST de la Finlande sous différents scénarios ") +
  xlab("direct density dependance ") + ylab("delayed density dependance")+ theme_classic()

# A.3 l'est

simulations_est <- filter(simulations,region==2)

ggplot(simulations_est) +
  geom_point(aes(x =direct.density.dependance,y =delayed.density.dependance,color = scenario, size = 0.5))+
  scale_color_manual(values=c('black','pink','red',"yellow","orange","darkgreen","darkblue","purple"))+
  ggtitle("dependance de densités à l'EST de la Finlande sous différents scénarios ") +
  xlab("direct density dependance ") + ylab("delayed density dependance")+ theme_classic()

# B) s-index

# B.1 le nord 

ggplot(simulations_nord, aes(x=scenario, y=s.index)) +
  geom_segment( aes(x=1, xend=scenario, y=0, yend=s.index),linetype="blank") +
  geom_point(aes(x = scenario,y = s.index,color = scenario, size = 0.5))+
  geom_errorbar(aes(ymin = min_s, ymax = max_s), colour="black", width=0.1 ) +
  scale_color_manual(values=c('black','pink','red',"yellow","orange","darkgreen","darkblue","purple"))+
  ggtitle("le s-index des prédictions de densité de campagnols au NORD de la Finlande sous différents scénarios ") +
  xlab("scenario") + ylab("s-index")+ theme_classic()

# B.2 le sud ouest

ggplot(simulations_ouest, aes(x=scenario, y=s.index)) +
  geom_segment( aes(x=1, xend=scenario, y=0, yend=s.index),linetype="blank") +
  geom_point(aes(x = scenario,y = s.index,color = scenario, size = 0.5))+
  geom_errorbar(aes(ymin = min_s, ymax = max_s), colour="black", width=0.1 ) +
  scale_color_manual(values=c('black','pink','red',"yellow","orange","darkgreen","darkblue","purple"))+
  ggtitle("le s-index des prédictions de densité de campagnols à l'OUEST de la Finlande sous différents scénarios ") +
  xlab("scenario") + ylab("s-index")+ theme_classic()

# B.3 l'est  

ggplot(simulations_est, aes(x=scenario, y=s.index)) +
  geom_segment( aes(x=1, xend=scenario, y=0, yend=s.index),linetype="blank") +
  geom_point(aes(x = scenario,y = s.index,color = scenario, size = 0.5))+
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


#-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

# IV Les simulations : densités de campagnols 

Vole <- read.csv("Vole_simulations.csv",sep=",", header=T, dec=".", stringsAsFactors=FALSE)

# S1 - North 

s1n_data <- Vole[1:100,1:2]

s1n_graph <- ggplot(data=s1n_data,aes(x=s1n_data[,1],y=s1n_data[,2]))+geom_line(color="black",size=1) +
  ggtitle("Vole_s1_north") +
  xlab("time") + ylab("Vole index") + theme_classic()

s1n_graph

a1 <- acf(s1n_data[,2])
a1_data <- with(a1, data.frame(lag, acf))
a1_acf <- ggplot(data=a1_data, mapping=aes(x=lag, y=acf)) +
  geom_bar(stat = "identity", position = "identity")+ theme_classic()
a1_acf

# S2 - North 

s2n_data <- Vole[1:100,c(1,5)]

s2n_graph <- ggplot(data=s2n_data,aes(x=s2n_data[,1],y=s2n_data[,2]))+geom_line(color="darkmagenta",size=1) +
  ggtitle("Vole_s2_north") +
  xlab("time") + ylab("Vole index") + theme_classic()

s2n_graph

a2 <- acf(s2n_data[,2])
a2_data <- with(a2, data.frame(lag, acf))
a2_acf <- ggplot(data=a2_data, mapping=aes(x=lag, y=acf)) +
  geom_bar(stat = "identity", position = "identity")+ theme_classic()
a2_acf

# S3 - North 

s3n_data <- Vole[1:100,c(1,8)]

s3n_graph <- ggplot(data=s3n_data,aes(x=s3n_data[,1],y=s3n_data[,2]))+geom_line(color="firebrick4",size=1) +
  ggtitle("Vole_s3_north") +
  xlab("time") + ylab("Vole index") + theme_classic()

s3n_graph

a3 <- acf(s3n_data[,2])
a3_data <- with(a3, data.frame(lag, acf))
a3_acf <- ggplot(data=a3_data, mapping=aes(x=lag, y=acf)) +
  geom_bar(stat = "identity", position = "identity")+ theme_classic()
a3_acf

# S4 - North 

s4n_data <- Vole[1:100,c(1,11)]

s4n_graph <- ggplot(data=s4n_data,aes(x=s4n_data[,1],y=s4n_data[,2]))+geom_line(color="goldenrod1",size=1) +
  ggtitle("Vole_s4_north") +
  xlab("time") + ylab("Vole index") + theme_classic()

s4n_graph

a4 <- acf(s4n_data[,2])
a4_data <- with(a4, data.frame(lag, acf))
a4_acf <- ggplot(data=a4_data, mapping=aes(x=lag, y=acf)) +
  geom_bar(stat = "identity", position = "identity")+ theme_classic()
a4_acf

# S5 - North 

s5n_data <- Vole[1:100,c(1,14)]

s5n_graph <- ggplot(data=s5n_data,aes(x=s5n_data[,1],y=s5n_data[,2]))+geom_line(color="orange2",size=1) +
  ggtitle("Vole_s5_north") +
  xlab("time") + ylab("Vole index") + theme_classic()

s5n_graph

a5 <- acf(s5n_data[,2])
a5_data <- with(a5, data.frame(lag, acf))
a5_acf <- ggplot(data=a5_data, mapping=aes(x=lag, y=acf)) +
  geom_bar(stat = "identity", position = "identity")+ theme_classic()
a5_acf

# S6 - North 

s6n_data <- Vole[1:100,c(1,17)]

s6n_graph <- ggplot(data=s6n_data,aes(x=s6n_data[,1],y=s6n_data[,2]))+geom_line(color="lightsteelblue4",size=1) +
  ggtitle("Vole_s6_north") +
  xlab("time") + ylab("Vole index") + theme_classic()

s6n_graph

a6 <- acf(s6n_data[,2])
a6_data <- with(a6, data.frame(lag, acf))
a6_acf <- ggplot(data=a6_data, mapping=aes(x=lag, y=acf)) +
  geom_bar(stat = "identity", position = "identity")+ theme_classic()
a6_acf

# S7 - North 

s7n_data <- Vole[1:100,c(1,20)]

s7n_graph <- ggplot(data=s7n_data,aes(x=s7n_data[,1],y=s7n_data[,2]))+geom_line(color="darkblue",size=1) +
  ggtitle("Vole_s7_north") +
  xlab("time") + ylab("Vole index") + theme_classic()

s7n_graph

a7 <- acf(s7n_data[,2])
a7_data <- with(a7, data.frame(lag, acf))
a7_acf <- ggplot(data=a7_data, mapping=aes(x=lag, y=acf)) +
  geom_bar(stat = "identity", position = "identity")+ theme_classic()
a7_acf

# S8 - North 

s8n_data <- Vole[1:100,c(1,23)]

s8n_graph <- ggplot(data=s8n_data,aes(x=s8n_data[,1],y=s8n_data[,2]))+geom_line(color="forestgreen",size=1) +
  ggtitle("Vole_s8_north") +
  xlab("time") + ylab("Vole index") + theme_classic()

s8n_graph

a8 <- acf(s8n_data[,2])
a8_data <- with(a8, data.frame(lag, acf))
a8_acf <- ggplot(data=a8_data, mapping=aes(x=lag, y=acf)) +
  geom_bar(stat = "identity", position = "identity")+ theme_classic()
a8_acf

# big graph north 

plot_grid(s1n_graph,a1_acf,s2n_graph,a2_acf,s3n_graph,a3_acf,s4n_graph,a4_acf,s5n_graph,a5_acf,s6n_graph,a6_acf,s7n_graph,a7_acf,s8n_graph,a8_acf, labels=c("1", "2","3","4","5","6","7","8","9","10","11","12","13","14","15","16"), ncol = 2, nrow = 8)

# S1 - East

s1e_data <- Vole[1:100,c(1,3)]

s1e_graph <- ggplot(data=s1e_data,aes(x=s1e_data[,1],y=s1e_data[,2]))+geom_line(color="black",size=1) +
  ggtitle("Vole_s1_east") +
  xlab("time") + ylab("Vole index") + theme_classic()

s1e_graph

a1 <- acf(s1e_data[,2])
a1_data <- with(a1, data.frame(lag, acf))
a1_acf <- ggplot(data=a1_data, mapping=aes(x=lag, y=acf)) +
  geom_bar(stat = "identity", position = "identity")+ theme_classic()
a1_acf

# S2 - East 

s2e_data <- Vole[1:100,c(1,6)]

s2e_graph <- ggplot(data=s2e_data,aes(x=s2e_data[,1],y=s2e_data[,2]))+geom_line(color="darkmagenta",size=1) +
  ggtitle("Vole_s2_east") +
  xlab("time") + ylab("Vole index") + theme_classic()

s2e_graph

a2 <- acf(s2e_data[,2])
a2_data <- with(a2, data.frame(lag, acf))
a2_acf <- ggplot(data=a2_data, mapping=aes(x=lag, y=acf)) +
  geom_bar(stat = "identity", position = "identity")+ theme_classic()
a2_acf

# S3 - East

s3e_data <- Vole[1:100,c(1,9)]

s3e_graph <- ggplot(data=s3e_data,aes(x=s3e_data[,1],y=s3e_data[,2]))+geom_line(color="firebrick4",size=1) +
  ggtitle("Vole_s3_east") +
  xlab("time") + ylab("Vole index") + theme_classic()

s3e_graph

a3 <- acf(s3e_data[,2])
a3_data <- with(a3, data.frame(lag, acf))
a3_acf <- ggplot(data=a3_data, mapping=aes(x=lag, y=acf)) +
  geom_bar(stat = "identity", position = "identity")+ theme_classic()
a3_acf

# S4 - East

s4e_data <- Vole[1:100,c(1,12)]

s4e_graph <- ggplot(data=s4e_data,aes(x=s4e_data[,1],y=s4e_data[,2]))+geom_line(color="goldenrod1",size=1) +
  ggtitle("Vole_s4_east") +
  xlab("time") + ylab("Vole index") + theme_classic()

s4e_graph

a4 <- acf(s4e_data[,2])
a4_data <- with(a4, data.frame(lag, acf))
a4_acf <- ggplot(data=a4_data, mapping=aes(x=lag, y=acf)) +
  geom_bar(stat = "identity", position = "identity")+ theme_classic()
a4_acf

# S5 - East

s5e_data <- Vole[1:100,c(1,15)]

s5e_graph <- ggplot(data=s5e_data,aes(x=s5e_data[,1],y=s5e_data[,2]))+geom_line(color="orange2",size=1) +
  ggtitle("Vole_s5_east") +
  xlab("time") + ylab("Vole index") + theme_classic()

s5e_graph

a5 <- acf(s5e_data[,2])
a5_data <- with(a5, data.frame(lag, acf))
a5_acf <- ggplot(data=a5_data, mapping=aes(x=lag, y=acf)) +
  geom_bar(stat = "identity", position = "identity")+ theme_classic()
a5_acf

# S6 - East 

s6e_data <- Vole[1:100,c(1,18)]

s6e_graph <- ggplot(data=s6e_data,aes(x=s6e_data[,1],y=s6e_data[,2]))+geom_line(color="lightsteelblue4",size=1) +
  ggtitle("Vole_s6_east") +
  xlab("time") + ylab("Vole index") + theme_classic()

s6e_graph

a6 <- acf(s6e_data[,2])
a6_data <- with(a6, data.frame(lag, acf))
a6_acf <- ggplot(data=a6_data, mapping=aes(x=lag, y=acf)) +
  geom_bar(stat = "identity", position = "identity")+ theme_classic()
a6_acf

# S7 - East

s7e_data <- Vole[1:100,c(1,21)]

s7e_graph <- ggplot(data=s7e_data,aes(x=s7e_data[,1],y=s7e_data[,2]))+geom_line(color="darkblue",size=1) +
  ggtitle("Vole_s7_east") +
  xlab("time") + ylab("Vole index") + theme_classic()

s7e_graph

a7 <- acf(s7e_data[,2])
a7_data <- with(a7, data.frame(lag, acf))
a7_acf <- ggplot(data=a7_data, mapping=aes(x=lag, y=acf)) +
  geom_bar(stat = "identity", position = "identity")+ theme_classic()
a7_acf

# S8 - East

s8e_data <- Vole[1:100,c(1,24)]

s8e_graph <- ggplot(data=s8e_data,aes(x=s8e_data[,1],y=s8e_data[,2]))+geom_line(color="forestgreen",size=1) +
  ggtitle("Vole_s8_east") +
  xlab("time") + ylab("Vole index") + theme_classic()

s8e_graph

a8 <- acf(s8e_data[,2])
a8_data <- with(a8, data.frame(lag, acf))
a8_acf <- ggplot(data=a8_data, mapping=aes(x=lag, y=acf)) +
  geom_bar(stat = "identity", position = "identity")+ theme_classic()
a8_acf

# big graph east 

plot_grid(s1e_graph,a1_acf,s2e_graph,a2_acf,s3e_graph,a3_acf,s4e_graph,a4_acf,s5e_graph,a5_acf,s6e_graph,a6_acf,s7e_graph,a7_acf,s8e_graph,a8_acf, labels=c("1", "2","3","4","5","6","7","8","9","10","11","12","13","14","15","16"), ncol = 2, nrow = 8)


# S1 - West

s1o_data <- Vole[1:100,1:4]

s1o_graph <- ggplot(data=s1o_data,aes(x=s1o_data[,1],y=s1o_data[,2]))+geom_line(color="black",size=1) +
  ggtitle("Vole_s1_west") +
  xlab("time") + ylab("Vole index") + theme_classic()

s1o_graph

a1 <- acf(s1o_data[,2])
a1_data <- with(a1, data.frame(lag, acf))
a1_acf <- ggplot(data=a1_data, mapping=aes(x=lag, y=acf)) +
  geom_bar(stat = "identity", position = "identity")+ theme_classic()
a1_acf

# S2 - West 

s2o_data <- Vole[1:100,c(1,7)]

s2o_graph <- ggplot(data=s2o_data,aes(x=s2o_data[,1],y=s2o_data[,2]))+geom_line(color="darkmagenta",size=1) +
  ggtitle("Vole_s2_west") +
  xlab("time") + ylab("Vole index") + theme_classic()

s2o_graph

a2 <- acf(s2o_data[,2])
a2_data <- with(a2, data.frame(lag, acf))
a2_acf <- ggplot(data=a2_data, mapping=aes(x=lag, y=acf)) +
  geom_bar(stat = "identity", position = "identity")+ theme_classic()
a2_acf

# S3 - West

s3o_data <- Vole[1:100,c(1,10)]

s3o_graph <- ggplot(data=s3o_data,aes(x=s3o_data[,1],y=s3o_data[,2]))+geom_line(color="firebrick4",size=1) +
  ggtitle("Vole_s3_west") +
  xlab("time") + ylab("Vole index") + theme_classic()

s3o_graph

a3 <- acf(s3o_data[,2])
a3_data <- with(a3, data.frame(lag, acf))
a3_acf <- ggplot(data=a3_data, mapping=aes(x=lag, y=acf)) +
  geom_bar(stat = "identity", position = "identity")+ theme_classic()
a3_acf

# S4 - West

s4o_data <- Vole[1:100,c(1,13)]

s4o_graph <- ggplot(data=s4o_data,aes(x=s4o_data[,1],y=s4o_data[,2]))+geom_line(color="goldenrod1",size=1) +
  ggtitle("Vole_s4_west") +
  xlab("time") + ylab("Vole index") + theme_classic()

s4o_graph

a4 <- acf(s4o_data[,2])
a4_data <- with(a4, data.frame(lag, acf))
a4_acf <- ggplot(data=a4_data, mapping=aes(x=lag, y=acf)) +
  geom_bar(stat = "identity", position = "identity")+ theme_classic()
a4_acf

# S5 - West

s5o_data <- Vole[1:100,c(1,16)]

s5o_graph <- ggplot(data=s5o_data,aes(x=s5o_data[,1],y=s5o_data[,2]))+geom_line(color="orange2",size=1) +
  ggtitle("Vole_s5_west") +
  xlab("time") + ylab("Vole index") + theme_classic()

s5o_graph

a5 <- acf(s5o_data[,2])
a5_data <- with(a5, data.frame(lag, acf))
a5_acf <- ggplot(data=a5_data, mapping=aes(x=lag, y=acf)) +
  geom_bar(stat = "identity", position = "identity")+ theme_classic()
a5_acf

# S6 - West

s6o_data <- Vole[1:100,c(1,19)]

s6o_graph <- ggplot(data=s6o_data,aes(x=s6o_data[,1],y=s6o_data[,2]))+geom_line(color="lightsteelblue4",size=1) +
  ggtitle("Vole_s6_west") +
  xlab("time") + ylab("Vole index") + theme_classic()

s6n_graph

a6 <- acf(s6o_data[,2])
a6_data <- with(a6, data.frame(lag, acf))
a6_acf <- ggplot(data=a6_data, mapping=aes(x=lag, y=acf)) +
  geom_bar(stat = "identity", position = "identity")+ theme_classic()
a6_acf

# S7 - West

s7o_data <- Vole[1:100,c(1,22)]

s7o_graph <- ggplot(data=s7o_data,aes(x=s7o_data[,1],y=s7o_data[,2]))+geom_line(color="darkblue",size=1) +
  ggtitle("Vole_s7_west") +
  xlab("time") + ylab("Vole index") + theme_classic()

s7o_graph

a7 <- acf(s7o_data[,2])
a7_data <- with(a7, data.frame(lag, acf))
a7_acf <- ggplot(data=a7_data, mapping=aes(x=lag, y=acf)) +
  geom_bar(stat = "identity", position = "identity")+ theme_classic()
a7_acf

# S8 - West

s8o_data <- Vole[1:100,c(1,25)]

s8o_graph <- ggplot(data=s8o_data,aes(x=s8o_data[,1],y=s8o_data[,2]))+geom_line(color="forestgreen",size=1) +
  ggtitle("Vole_s8_west") +
  xlab("time") + ylab("Vole index") + theme_classic()

s8o_graph

a8 <- acf(s8o_data[,2])
a8_data <- with(a8, data.frame(lag, acf))
a8_acf <- ggplot(data=a8_data, mapping=aes(x=lag, y=acf)) +
  geom_bar(stat = "identity", position = "identity")+ theme_classic()
a8_acf

# big graph west

plot_grid(s1o_graph,a1_acf,s2o_graph,a2_acf,s3o_graph,a3_acf,s4o_graph,a4_acf,s5o_graph,a5_acf,s6o_graph,a6_acf,s7o_graph,a7_acf,s8o_graph,a8_acf, labels=c("1", "2","3","4","5","6","7","8","9","10","11","12","13","14","15","16"), ncol = 2, nrow = 8)


