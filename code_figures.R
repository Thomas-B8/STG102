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

vect <- c(1:105)
a <-0

for (i in 1:21){
  for (j in 1:5){
    a <- a+1
    vect[a]<- nord[i,3+j]
  }
}

df1_nord <-  data.frame(variable=rep(c("direct DD", "delayed DD", "small mustelids","generalists","avian predator"), each=21),
                              annee=rep(c(1991,1992,1993,1994,1995,1996,1997,1998,1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011),5),
                              len=vect)

ggplot(data=df1_nord, aes(x=annee, y=len, fill=variable)) +
  geom_bar(stat="identity")
