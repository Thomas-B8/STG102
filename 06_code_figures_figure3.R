
# author = "Besognet Thomas"
# date = " 24/05/23" 
# project = "Korpela et al. Proceedings B Replication work""
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
data <- read.csv("../data/variances_explained_predators.csv", sep=",", header=T, dec=".", stringsAsFactors=FALSE)

# data selection by region 
north <- data[,c(1:14)]
east <- data[,c(1,2,15:26)]
west <-data[,c(1,2,27:38)]

# A.1 modele 3 north 

vect3n <- c(1:63)
a <-0

for (j in 1:3){
  for (i in 1:21){
    a <- a+1
    vect3n[a]<- north[i,3+j]
  }
}

df3_north <-  data.frame(variable=rep(c("3 vole autumn (t-1)", "2 vole spring (t-1)", "1 vole autumn (t-2)"), each=21),
                        year=rep(c(1991,1992,1993,1994,1995,1996,1997,1998,1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011),3),
                        variances=vect3n)

graph3_north <- ggplot(data=df3_north, aes(x=year, y=variances, fill=variable)) +
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

graph3_north

# A.2 modele 4 north 

vect4n <- c(1:63)
a <-0

for (j in 1:3){
  for (i in 1:21){
    a <- a+1
    vect4n[a]<- north[i,7+j]
  }
}

df4_north <-  data.frame(variable=rep(c("3_autumn_t-1", "2_spring_t-1", "1_autumn_t-2"), each=21),
                        year=rep(c(1991,1992,1993,1994,1995,1996,1997,1998,1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011),3),
                        variances=vect4n)

graph4_north <- ggplot(data=df4_north, aes(x=year, y=variances, fill=variable)) +
  geom_bar(stat="identity") + scale_fill_manual(values=c("gray60", "gray30", "gray0")) +
  ggtitle("fox and pine marten north") +
  xlab("years") + ylab("proportion of variance explained") + theme_classic()+ 
  theme(plot.title = element_text(size=18, face="bold",hjust = 0.5),
        axis.title.x = element_text( size=18, face="bold"),
        axis.title.y = element_text( size=6, face="bold"),
        axis.text.x = element_text(face="bold", size=12,color="black") ,
        axis.text.y = element_text(face="bold", size=12,color="black"),
        legend.position='none')

graph4_north

# A.3 modele 5 north 

vect5n <- c(1:63)
a <-0

for (j in 1:3){
  for (i in 1:21){
    a <- a+1
    vect5n[a]<- north[i,11+j]
  }
}

df5_north <-  data.frame(variable=rep(c("3 vole spring t", "2 vole autumn (t-1)", "1 vole spring (t-1)"), each=21),
                        year=rep(c(1991,1992,1993,1994,1995,1996,1997,1998,1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011),3),
                        variances=vect5n)

graph5_north <- ggplot(data=df5_north, aes(x=year, y=variances, fill=variable)) +
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


graph5_north

# B.1 modele 3 west 

vect3o <- c(1:63)
a <-0

for (j in 1:3){
  for (i in 1:21){
    a <- a+1
    vect3o[a]<- west[i,3+j]
  }
}

df3_west <-  data.frame(variable=rep(c("3_autumn_t-1", "2_spring_t-1", "1_autumn_t-2"), each=21),
                         year=rep(c(1991,1992,1993,1994,1995,1996,1997,1998,1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011),3),
                         variances=vect3o)

graph3_west <- ggplot(data=df3_west, aes(x=year, y=variances, fill=variable)) +
  geom_bar(stat="identity") + scale_fill_manual(values=c("gray60", "gray30", "gray0")) +
  ggtitle("small mustelids west") +
  xlab("years") + ylab("proportion of variance explained") + theme_classic()+ 
  theme(plot.title = element_text(size=18, face="bold",hjust = 0.5),
        axis.title.x = element_text( size=18, face="bold"),
        axis.title.y = element_text( size=6, face="bold"),
        axis.text.x = element_text(face="bold", size=12,color="black") ,
        axis.text.y = element_text(face="bold", size=12,color="black"),
        legend.position='none')

graph3_west

# B.2 modele 4 west 

vect4o <- c(1:63)
a <-0

for (j in 1:3){
  for (i in 1:21){
    a <- a+1
    vect4o[a]<- west[i,7+j]
  }
}

df4_west <-  data.frame(variable=rep(c("3_autumn_t-1", "2_spring_t-1", "1_autumn_t-2"), each=21),
                         year=rep(c(1991,1992,1993,1994,1995,1996,1997,1998,1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011),3),
                         variances=vect4o)

graph4_west <- ggplot(data=df4_west, aes(x=year, y=variances, fill=variable)) +
  geom_bar(stat="identity") + scale_fill_manual(values=c("gray60", "gray30", "gray0")) +
  ggtitle("fox and pine marten west") +
  xlab("years") + ylab("proportion of variance explained") + theme_classic()+ 
  theme(plot.title = element_text(size=18, face="bold",hjust = 0.5),
        axis.title.x = element_text( size=18, face="bold"),
        axis.title.y = element_text( size=6, face="bold"),
        axis.text.x = element_text(face="bold", size=12,color="black") ,
        axis.text.y = element_text(face="bold", size=12,color="black"),
        legend.position='none')

graph4_west

# B.3 modele 5 west 

vect5o <- c(1:63)
a <-0

for (j in 1:3){
  for (i in 1:21){
    a <- a+1
    vect5o[a]<- west[i,11+j]
  }
}

df5_west <-  data.frame(variable=rep(c("3_spring_t", "2_autumn_t-1", "1_spring_t-1"), each=21),
                         year=rep(c(1991,1992,1993,1994,1995,1996,1997,1998,1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011),3),
                         variances=vect5o)

graph5_west <- ggplot(data=df5_west, aes(x=year, y=variances, fill=variable)) +
  geom_bar(stat="identity") + scale_fill_manual(values=c("gray60", "gray30", "gray0")) +
  ggtitle("avian predators west") +
  xlab("years") + ylab("proportion of variance explained") + theme_classic()+ 
  theme(plot.title = element_text(size=18, face="bold",hjust = 0.5),
        axis.title.x = element_text( size=18, face="bold"),
        axis.title.y = element_text( size=6, face="bold"),
        axis.text.x = element_text(face="bold", size=12,color="black") ,
        axis.text.y = element_text(face="bold", size=12,color="black"),
        legend.position='none')

graph5_west

# C.1 modele 3 east 

vect3e <- c(1:63)
a <-0

for (j in 1:3){
  for (i in 1:21){
    a <- a+1
    vect3e[a]<- east[i,3+j]
  }
}

df3_east <-  data.frame(variable=rep(c("3_autumn_t-1", "2_spring_t-1", "1_autumn_t-2"), each=21),
                       year=rep(c(1991,1992,1993,1994,1995,1996,1997,1998,1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011),3),
                       variances=vect3e)

graph3_east <- ggplot(data=df3_east, aes(x=year, y=variances, fill=variable)) +
  geom_bar(stat="identity") + scale_fill_manual(values=c("gray60", "gray30", "gray0")) +
  ggtitle("small mustelids east") +
  xlab("years") + ylab("proportion of variance explained") + theme_classic()+ 
  theme(plot.title = element_text(size=18, face="bold",hjust = 0.5),
        axis.title.x = element_text( size=18, face="bold"),
        axis.title.y = element_text( size=6, face="bold"),
        axis.text.x = element_text(face="bold", size=12,color="black") ,
        axis.text.y = element_text(face="bold", size=12,color="black"),
        legend.position='none')

graph3_east

# C.2 modele 4 east 

vect4e <- c(1:63)
a <-0

for (j in 1:3){
  for (i in 1:21){
    a <- a+1
    vect4e[a]<- east[i,7+j]
  }
}

df4_east <-  data.frame(variable=rep(c("3_autumn_t-1", "2_spring_t-1", "1_autumn_t-2"), each=21),
                       year=rep(c(1991,1992,1993,1994,1995,1996,1997,1998,1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011),3),
                       variances=vect4e)

graph4_east <- ggplot(data=df4_east, aes(x=year, y=variances, fill=variable)) +
  geom_bar(stat="identity") + scale_fill_manual(values=c("gray60", "gray30", "gray0")) +
  ggtitle("fox and pine marten east") +
  xlab("years") + ylab("proportion of variance explained") + theme_classic()+ 
  theme(plot.title = element_text(size=18, face="bold",hjust = 0.5),
        axis.title.x = element_text( size=18, face="bold"),
        axis.title.y = element_text( size=6, face="bold"),
        axis.text.x = element_text(face="bold", size=12,color="black") ,
        axis.text.y = element_text(face="bold", size=12,color="black"),
        legend.position='none')

graph4_east

# C.3 modele 5 east 

vect5e <- c(1:63)
a <-0

for (j in 1:3){
  for (i in 1:21){
    a <- a+1
    vect5e[a]<- east[i,11+j]
  }
}

df5_east <-  data.frame(variable=rep(c("3_spring_t", "2_autumn_t-1", "1_spring_t-1"), each=21),
                       year=rep(c(1991,1992,1993,1994,1995,1996,1997,1998,1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011),3),
                       variances=vect5e)

graph5_east <- ggplot(data=df5_east, aes(x=year, y=variances, fill=variable)) +
  geom_bar(stat="identity") + scale_fill_manual(values=c("gray60", "gray30", "gray0")) +
  ggtitle("avian predators east") +
  xlab("years") + ylab("proportion of variance explained") + theme_classic()+ 
  theme(plot.title = element_text(size=18, face="bold",hjust = 0.5),
        axis.title.x = element_text( size=18, face="bold"),
        axis.title.y = element_text( size=6, face="bold"),
        axis.text.x = element_text(face="bold", size=12,color="black") ,
        axis.text.y = element_text(face="bold", size=12,color="black"),
        legend.position='none')

graph5_east

legend1<- get_legend(graph3_north + theme(legend.position = 'right'))
legend2<- get_legend(graph5_north + theme(legend.position = 'right'))

# Global figure and register 

A <- plot_grid(graph3_north + theme(legend.position = 'none'),graph3_west,graph3_east,legend1,graph4_north,graph4_west,graph4_east,legend1,graph5_north+ theme(legend.position = 'none'),graph5_west,graph5_east,legend2, labels=c("1", "2","3","legende","4","5","6","legende","7","8","9","legende"), ncol = 4, nrow = 3)

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

