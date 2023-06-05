
# author = "Besognet Thomas"
# date = " 24/05/23" 
# project = "Estimation d'intéractions entre espèces à partir de séries temporelles"
# name =  "figure 2 replication "

# packages 
install.packages("ggplot2")                                       
library(ggplot2)
install.packages("dplyr")
library(dplyr)
install.packages("cowplot")
library(cowplot)
install.packages("ggplotify")
library(ggplotify)

# Graphics 

#---------------------------------------------------------------------------------------------------------------------------------------------------------------------

# data importation 
data <- read.csv("../Donnees/variances_explained_vole.csv", sep=",", header=T, dec=".", stringsAsFactors=FALSE)

# data selection by region 
nord <- data[,c(1:14)]
est <- data[,c(1,2,15:26)]
ouest <-data[,c(1,2,27:38)]

# A.1 modele 1 north 

vect1n <- c(1:105)
a <-0

for (j in 1:5){
  for (i in 1:21){
    a <- a+1
    vect1n[a]<- nord[i,3+j]
  }
}

df1_nord <-  data.frame(variable=rep(c("5 direct density dependance ", "4 delayed density dependance", "3 small mustelids","2 generalists predators","1 avian predators"), each=21),
                        annee=rep(c(1991,1992,1993,1994,1995,1996,1997,1998,1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011),5),
                        variances=vect1n)

graph1_nord <- ggplot(data=df1_nord, aes(x=annee, y=variances, fill=variable)) +
  geom_bar(stat="identity") + scale_fill_manual(values=c("#6699CC", "#FFFF33", "#FF0033", "gray30" , "gray0")) +
  ggtitle("winter north") +
  xlab("years") + ylab("proportion of variance explained") + theme_classic()+ 
  theme(plot.title = element_text(size=18, face="bold",hjust = 0.5),
        axis.title.x = element_text( size=18, face="bold"),
        axis.title.y = element_text( size=14, face="bold"),
        axis.text.x = element_text(face="bold", size=12,color="black") ,
        axis.text.y = element_text(face="bold", size=12,color="black"),
        legend.background = element_rect(fill="grey70",linewidth=1, linetype="solid", colour ="black"),
        legend.text = element_text(colour="black", size=15, face="bold"),
        legend.title = element_text(colour="black", size=15, face="bold"))

graph1_nord

# A.2 modele 2 north 

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
  ggtitle("summer north") +
  xlab("years") + ylab("proportion of variance explained") + theme_classic()+ 
  theme(plot.title = element_text(size=18, face="bold",hjust = 0.5),
        axis.title.x = element_text( size=18, face="bold"),
        axis.title.y = element_text( size=14, face="bold"),
        axis.text.x = element_text(face="bold", size=12,color="black") ,
        axis.text.y = element_text(face="bold", size=12,color="black"),
        legend.position='none')

graph2_nord


# B.1 modele 1 west 

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
  ggtitle("winter west") +
  xlab("years") + ylab("proportion of variance explained") + theme_classic()+ 
  theme(plot.title = element_text(size=18, face="bold",hjust = 0.5),
        axis.title.x = element_text( size=18, face="bold"),
        axis.title.y = element_text( size=14, face="bold"),
        axis.text.x = element_text(face="bold", size=12,color="black") ,
        axis.text.y = element_text(face="bold", size=12,color="black"),
        legend.position='none')

graph1_ouest

# B.2 modele 2 west 

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
  ggtitle("summer west") +
  xlab("years") + ylab("proportion of variance explained") + theme_classic()+ 
  theme(plot.title = element_text(size=18, face="bold",hjust = 0.5),
        axis.title.x = element_text( size=18, face="bold"),
        axis.title.y = element_text( size=14, face="bold"),
        axis.text.x = element_text(face="bold", size=12,color="black") ,
        axis.text.y = element_text(face="bold", size=12,color="black"),
        legend.position='none')

graph2_ouest


# C.1 modele 1 east 

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
  ggtitle("winter east") +
  xlab("years") + ylab("proportion of variance explained") + theme_classic()+ 
  theme(plot.title = element_text(size=18, face="bold",hjust = 0.5),
        axis.title.x = element_text( size=18, face="bold"),
        axis.title.y = element_text( size=14, face="bold"),
        axis.text.x = element_text(face="bold", size=12,color="black") ,
        axis.text.y = element_text(face="bold", size=12,color="black"),
        legend.position='none')

graph1_est

# C.2 modele 2 east 

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
  ggtitle("summer east") +
  xlab("years") + ylab("proportion of variance explained") + theme_classic()+ 
  theme(plot.title = element_text(size=18, face="bold",hjust = 0.5),
        axis.title.x = element_text( size=18, face="bold"),
        axis.title.y = element_text( size=14, face="bold"),
        axis.text.x = element_text(face="bold", size=12,color="black") ,
        axis.text.y = element_text(face="bold", size=12,color="black"),
        legend.position='none')

graph2_est

legend<- get_legend(graph1_nord + theme(legend.position = 'right'))


# Global figure and register 

A <- plot_grid(graph1_nord+ theme(legend.position = 'none'),graph1_ouest,graph1_est,legend,graph2_nord,graph2_ouest,graph2_est, labels=c("1", "2","3","","4","5","6"), ncol = 4, nrow = 2)

# Opening the graphic device 
pdf("../Figures/Graph2_figure2.pdf",  
    width = 14, height = 8, 
    bg = "white",         
    colormodel = "cmyk")

# Creating a plot
plot(A)

# Closing the graphical device
dev.off() 

