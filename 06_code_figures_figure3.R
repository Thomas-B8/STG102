
# author = "Besognet Thomas"
# date = " 24/05/23" 
# project = "Estimation d'int?ractions entre esp?ces ? partir de s?ries temporelles"
# name =  "figure 3 replication "

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
data <- read.csv("../Donnees/variances_explained_predators.csv", sep=",", header=T, dec=".", stringsAsFactors=FALSE)

# data selection by region 
nord <- data[,c(1:14)]
est <- data[,c(1,2,15:26)]
ouest <-data[,c(1,2,27:38)]

# A.1 modele 3 north 

vect3n <- c(1:63)
a <-0

for (j in 1:3){
  for (i in 1:21){
    a <- a+1
    vect3n[a]<- nord[i,3+j]
  }
}

df3_nord <-  data.frame(variable=rep(c("3 vole autumn (t-1)", "2 vole spring (t-1)", "1 vole autumn (t-2)"), each=21),
                        annee=rep(c(1991,1992,1993,1994,1995,1996,1997,1998,1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011),3),
                        variances=vect3n)

graph3_nord <- ggplot(data=df3_nord, aes(x=annee, y=variances, fill=variable)) +
  geom_bar(stat="identity") + scale_fill_manual(values=c("gray60", "gray30", "gray0")) +
  ggtitle("small mustelids north") +
  xlab("years") + ylab("proportion of variance explained") + theme_classic()+ 
  theme(plot.title = element_text(size=18, face="bold",hjust = 0.5),
        axis.title.x = element_text( size=18, face="bold"),
        axis.title.y = element_text( size=6, face="bold"),
        axis.text.x = element_text(face="bold", size=12,color="black") ,
        axis.text.y = element_text(face="bold", size=12,color="black"),
        legend.background = element_rect(fill="grey70",linewidth=1, linetype="solid", colour ="black"),
        legend.text = element_text(colour="black", size=15, face="bold"),
        legend.title = element_text(colour="black", size=15, face="bold"))

graph3_nord

# A.2 modele 4 north 

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
  ggtitle("fox and pine marten north") +
  xlab("years") + ylab("proportion of variance explained") + theme_classic()+ 
  theme(plot.title = element_text(size=18, face="bold",hjust = 0.5),
        axis.title.x = element_text( size=18, face="bold"),
        axis.title.y = element_text( size=6, face="bold"),
        axis.text.x = element_text(face="bold", size=12,color="black") ,
        axis.text.y = element_text(face="bold", size=12,color="black"),
        legend.position='none')

graph4_nord

# A.3 modele 5 north 

vect5n <- c(1:63)
a <-0

for (j in 1:3){
  for (i in 1:21){
    a <- a+1
    vect5n[a]<- nord[i,11+j]
  }
}

df5_nord <-  data.frame(variable=rep(c("3 vole spring t", "2 vole autumn (t-1)", "1 vole spring (t-1)"), each=21),
                        annee=rep(c(1991,1992,1993,1994,1995,1996,1997,1998,1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011),3),
                        variances=vect5n)

graph5_nord <- ggplot(data=df5_nord, aes(x=annee, y=variances, fill=variable)) +
  geom_bar(stat="identity") + scale_fill_manual(values=c("gray60", "gray30", "gray0")) +
  ggtitle("avian predators north") +
  xlab("years") + ylab("proportion of variance explained") + theme_classic()+ 
  theme(plot.title = element_text(size=18, face="bold",hjust = 0.5),
        axis.title.x = element_text( size=18, face="bold"),
        axis.title.y = element_text( size=6, face="bold"),
        axis.text.x = element_text(face="bold", size=12,color="black") ,
        axis.text.y = element_text(face="bold", size=12,color="black"),
        legend.background = element_rect(fill="grey70",linewidth=1, linetype="solid", colour ="black"),
        legend.text = element_text(colour="black", size=15, face="bold"),
        legend.title = element_text(colour="black", size=15, face="bold"))


graph5_nord

# B.1 modele 3 west 

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
  ggtitle("small mustelids west") +
  xlab("years") + ylab("proportion of variance explained") + theme_classic()+ 
  theme(plot.title = element_text(size=18, face="bold",hjust = 0.5),
        axis.title.x = element_text( size=18, face="bold"),
        axis.title.y = element_text( size=6, face="bold"),
        axis.text.x = element_text(face="bold", size=12,color="black") ,
        axis.text.y = element_text(face="bold", size=12,color="black"),
        legend.position='none')

graph3_ouest

# B.2 modele 4 west 

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
  ggtitle("fox and pine marten west") +
  xlab("years") + ylab("proportion of variance explained") + theme_classic()+ 
  theme(plot.title = element_text(size=18, face="bold",hjust = 0.5),
        axis.title.x = element_text( size=18, face="bold"),
        axis.title.y = element_text( size=6, face="bold"),
        axis.text.x = element_text(face="bold", size=12,color="black") ,
        axis.text.y = element_text(face="bold", size=12,color="black"),
        legend.position='none')

graph4_ouest

# B.3 modele 5 west 

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
  ggtitle("avian predators west") +
  xlab("years") + ylab("proportion of variance explained") + theme_classic()+ 
  theme(plot.title = element_text(size=18, face="bold",hjust = 0.5),
        axis.title.x = element_text( size=18, face="bold"),
        axis.title.y = element_text( size=6, face="bold"),
        axis.text.x = element_text(face="bold", size=12,color="black") ,
        axis.text.y = element_text(face="bold", size=12,color="black"),
        legend.position='none')

graph5_ouest

# C.1 modele 3 east 

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
  ggtitle("small mustelids east") +
  xlab("years") + ylab("proportion of variance explained") + theme_classic()+ 
  theme(plot.title = element_text(size=18, face="bold",hjust = 0.5),
        axis.title.x = element_text( size=18, face="bold"),
        axis.title.y = element_text( size=6, face="bold"),
        axis.text.x = element_text(face="bold", size=12,color="black") ,
        axis.text.y = element_text(face="bold", size=12,color="black"),
        legend.position='none')

graph3_est

# C.2 modele 4 east 

vect4e <- c(1:63)
a <-0

for (j in 1:3){
  for (i in 1:21){
    a <- a+1
    vect4e[a]<- est[i,7+j]
  }
}

df4_est <-  data.frame(variable=rep(c("3_autumn_t-1", "2_spring_t-1", "1_autumn_t-2"), each=21),
                       annee=rep(c(1991,1992,1993,1994,1995,1996,1997,1998,1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011),3),
                       variances=vect4e)

graph4_est <- ggplot(data=df4_est, aes(x=annee, y=variances, fill=variable)) +
  geom_bar(stat="identity") + scale_fill_manual(values=c("gray60", "gray30", "gray0")) +
  ggtitle("fox and pine marten east") +
  xlab("years") + ylab("proportion of variance explained") + theme_classic()+ 
  theme(plot.title = element_text(size=18, face="bold",hjust = 0.5),
        axis.title.x = element_text( size=18, face="bold"),
        axis.title.y = element_text( size=6, face="bold"),
        axis.text.x = element_text(face="bold", size=12,color="black") ,
        axis.text.y = element_text(face="bold", size=12,color="black"),
        legend.position='none')

graph4_est

# C.3 modele 5 east 

vect5e <- c(1:63)
a <-0

for (j in 1:3){
  for (i in 1:21){
    a <- a+1
    vect5e[a]<- est[i,11+j]
  }
}

df5_est <-  data.frame(variable=rep(c("3_spring_t", "2_autumn_t-1", "1_spring_t-1"), each=21),
                       annee=rep(c(1991,1992,1993,1994,1995,1996,1997,1998,1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011),3),
                       variances=vect5e)

graph5_est <- ggplot(data=df5_est, aes(x=annee, y=variances, fill=variable)) +
  geom_bar(stat="identity") + scale_fill_manual(values=c("gray60", "gray30", "gray0")) +
  ggtitle("avian predators east") +
  xlab("years") + ylab("proportion of variance explained") + theme_classic()+ 
  theme(plot.title = element_text(size=18, face="bold",hjust = 0.5),
        axis.title.x = element_text( size=18, face="bold"),
        axis.title.y = element_text( size=6, face="bold"),
        axis.text.x = element_text(face="bold", size=12,color="black") ,
        axis.text.y = element_text(face="bold", size=12,color="black"),
        legend.position='none')

graph5_est

legend1<- get_legend(graph3_nord + theme(legend.position = 'right'))
legend2<- get_legend(graph5_nord + theme(legend.position = 'right'))

# Global figure and register 

A <- plot_grid(graph3_nord + theme(legend.position = 'none'),graph3_ouest,graph3_est,legend1,graph4_nord,graph4_ouest,graph4_est,legend1,graph5_nord+ theme(legend.position = 'none'),graph5_ouest,graph5_est,legend2, labels=c("1", "2","3","legende","4","5","6","legende","7","8","9","legende"), ncol = 4, nrow = 3)

A
# Opening the graphic device 
pdf("../Figures/Graph3_figure3.pdf",  
    width = 10, height = 8, 
    bg = "white",         
    colormodel = "cmyk")

# Creating a plot
plot(A)

# Closing the graphical device
dev.off() 

