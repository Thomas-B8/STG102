# Besognet Thomas, 17/04/23 , Stage : Figures et tableaux 

# packages 
install.packages("ggplot2")                                       
library(ggplot2)

# Figures et tableaux 

# I Spatio-temporal variation in predator-vole interactions

# Importation des tableaux de variance expliquées 

nord <- read.csv("variances_expliquees_nord.csv", sep=",", header=T, dec=".", stringsAsFactors=FALSE)
est <- read.csv("variances_expliquees_est.csv", sep=",", header=T, dec=".", stringsAsFactors=FALSE)
ouest <- read.csv("variances_expliquees_ouest.csv", sep=",", header=T, dec=".", stringsAsFactors=FALSE)

# Creation des graphiques avec ggplot2 

# I modele 1 pour le nord 

vect1n <- c(1:105)
a <-0

for (i in 1:21){
  for (j in 1:5){
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

# II modele 2 pour le nord 

vect2n <- c(1:105)
a <-0

for (i in 1:21){
  for (j in 1:5){
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


# III modele 1 pour l'ouest

vect1w <- c(1:105)
a <-0

for (i in 1:21){
  for (j in 1:5){
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

# IV modele 2 pour l'ouest

vect2w <- c(1:105)
a <-0

for (i in 1:21){
  for (j in 1:5){
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


# V modele 1 pour l'est

vect1e <- c(1:105)
a <-0

for (i in 1:21){
  for (j in 1:5){
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

# VI modele 2 pour l'est

vect2e <- c(1:105)
a <-0

for (i in 1:21){
  for (j in 1:5){
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
