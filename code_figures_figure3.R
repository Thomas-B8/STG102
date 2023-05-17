# Besognet Thomas, 17/05/23 , Stage : Figures et tableaux : Réplication de la figure 3 des modèles : les variances et variances expliquées des prédateurs 

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

# Réplication de la figure 3

#---------------------------------------------------------------------------------------------------------------------------------------------------------------------

# Importation des données

nord <- read.csv("variances_expliquees_nord_pred.csv", sep=",", header=T, dec=".", stringsAsFactors=FALSE)
est <- read.csv("variances_expliquees_est_pred.csv", sep=",", header=T, dec=".", stringsAsFactors=FALSE)
ouest <- read.csv("variances_expliquees_ouest_pred.csv", sep=",", header=T, dec=".", stringsAsFactors=FALSE)

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