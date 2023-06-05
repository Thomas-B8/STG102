
# author = "Besognet Thomas"
# date = " 24/05/23" 
# project = "Estimation d'intéractions entre espèces à partir de séries temporelles"
# name =  "mean density for each species "

# packages 
install.packages("ggplot2")                                       
library(ggplot2)
install.packages("dplyr")
library(dplyr)
install.packages("cowplot")
library(cowplot)
install.packages("ggplotify")
library(ggplotify)

# Graphic

#---------------------------------------------------------------------------------------------------------------------------------------------------------------------

# data importation
donnees <- read.csv("../Donnees/donnees_propres_korpela.csv", sep=",", header=T, dec=".")

# A : north 
# A.1 data management 
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

# A.2 graphics 
# vole

vole_graph_n <- ggplot(data=Vole_n,aes(x=Vole_n[,2],y=Vole_n[,1]))+geom_line(color="darkgreen",size=1.5) +
 ggtitle("Vole north") +
  xlab("years") + ylab("log density") + theme_classic()+ 
  theme(plot.title = element_text(size=18, face="bold",hjust = 0.5),
        axis.title.x = element_text( size=18, face="bold"),
        axis.title.y = element_text( size=18, face="bold"),
        axis.text.x = element_text(face="bold", size=12,color="black") ,
        axis.text.y = element_text(face="bold", size=12,color="black"))

vole_graph_n

# small mustelids 

must_graph_n <- ggplot(data=Must_n,aes(x=Must_n[,2],y=Must_n[,1]))+geom_line(color="darkred",size=1.5) +
  ggtitle("Small mustelids north") +
  xlab("years") + ylab("log density") + theme_classic()+ 
  theme(plot.title = element_text(size=18, face="bold",hjust = 0.5),
        axis.title.x = element_text( size=18, face="bold"),
        axis.title.y = element_text( size=18, face="bold"),
        axis.text.x = element_text(face="bold", size=12,color="black") ,
        axis.text.y = element_text(face="bold", size=12,color="black"))

must_graph_n

# red fox and pine marten  

gene_graph_n <- ggplot(data=Gene_n,aes(x=Gene_n[,2],y=Gene_n[,1]))+geom_line(color="orange",size=1.5) +
  ggtitle("Fox and pine marten north") +
  xlab("years") + ylab("log density") + theme_classic()+ 
  theme(plot.title = element_text(size=18, face="bold",hjust = 0.5),
        axis.title.x = element_text( size=18, face="bold"),
        axis.title.y = element_text( size=18, face="bold"),
        axis.text.x = element_text(face="bold", size=12,color="black") ,
        axis.text.y = element_text(face="bold", size=12,color="black"))

gene_graph_n

# avian predators 

aeri_graph_n <- ggplot(data=Aeri_n,aes(x=Aeri_n[,2],y=Aeri_n[,1]))+geom_line(color="darkblue",size=1.5) +
  ggtitle("Avian predators north") +
  xlab("years") + ylab("log density") + theme_classic()+ 
  theme(plot.title = element_text(size=18, face="bold",hjust = 0.5),
        axis.title.x = element_text( size=18, face="bold"),
        axis.title.y = element_text( size=18, face="bold"),
        axis.text.x = element_text(face="bold", size=12,color="black") ,
        axis.text.y = element_text(face="bold", size=12,color="black"))

aeri_graph_n

# B : east 
# B.1 data managment 
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

# B.2 Graphics 
# Vole

vole_graph_e <- ggplot(data=Vole_e,aes(x=Vole_e[,2],y=Vole_e[,1]))+geom_line(color="darkgreen",size=1.5) +
  ggtitle("Vole east") +
  xlab("years") + ylab("log density") + theme_classic()+ 
  theme(plot.title = element_text(size=18, face="bold",hjust = 0.5),
        axis.title.x = element_text( size=18, face="bold"),
        axis.title.y = element_text( size=18, face="bold"),
        axis.text.x = element_text(face="bold", size=12,color="black") ,
        axis.text.y = element_text(face="bold", size=12,color="black"))

vole_graph_e

# small mustelids

must_graph_e <- ggplot(data=Must_e,aes(x=Must_e[,2],y=Must_e[,1]))+geom_line(color="darkred",size=1.5) +
  ggtitle("Small mustelids east") +
  xlab("years") + ylab("log density") + theme_classic()+ 
  theme(plot.title = element_text(size=18, face="bold",hjust = 0.5),
        axis.title.x = element_text( size=18, face="bold"),
        axis.title.y = element_text( size=18, face="bold"),
        axis.text.x = element_text(face="bold", size=12,color="black") ,
        axis.text.y = element_text(face="bold", size=12,color="black"))

must_graph_e

# red fox and pine marten  

gene_graph_e <- ggplot(data=Gene_e,aes(x=Gene_e[,2],y=Gene_e[,1]))+geom_line(color="orange",size=1.5) +
  ggtitle("Fox and pine marten east") +
  xlab("years") + ylab("log density") + theme_classic()+ 
  theme(plot.title = element_text(size=18, face="bold",hjust = 0.5),
        axis.title.x = element_text( size=18, face="bold"),
        axis.title.y = element_text( size=18, face="bold"),
        axis.text.x = element_text(face="bold", size=12,color="black") ,
        axis.text.y = element_text(face="bold", size=12,color="black"))

gene_graph_e

# avian predators 

aeri_graph_e <- ggplot(data=Aeri_e,aes(x=Aeri_e[,2],y=Aeri_e[,1]))+geom_line(color="darkblue",size=1.5) +
  ggtitle("Avian predators east") +
  xlab("years") + ylab("log density") + theme_classic()+ 
  theme(plot.title = element_text(size=18, face="bold",hjust = 0.5),
        axis.title.x = element_text( size=18, face="bold"),
        axis.title.y = element_text( size=18, face="bold"),
        axis.text.x = element_text(face="bold", size=12,color="black") ,
        axis.text.y = element_text(face="bold", size=12,color="black"))

aeri_graph_e

# C : west
# C.1 data managment 

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

# C.2 Graphics 
# Vole

vole_graph_o <- ggplot(data=Vole_o,aes(x=Vole_o[,2],y=Vole_o[,1]))+geom_line(color="darkgreen",size=1.5) +
  ggtitle("Vole west") +
  xlab("years") + ylab("log density") + theme_classic()+ 
  theme(plot.title = element_text(size=18, face="bold",hjust = 0.5),
        axis.title.x = element_text( size=18, face="bold"),
        axis.title.y = element_text( size=18, face="bold"),
        axis.text.x = element_text(face="bold", size=12,color="black") ,
        axis.text.y = element_text(face="bold", size=12,color="black"))

vole_graph_o

# small mustelids

must_graph_o <- ggplot(data=Must_o,aes(x=Must_o[,2],y=Must_o[,1]))+geom_line(color="darkred",size=1.5) +
  ggtitle("Small mustelids west") +
  xlab("years") + ylab("log density") + theme_classic()+ 
  theme(plot.title = element_text(size=18, face="bold",hjust = 0.5),
        axis.title.x = element_text( size=18, face="bold"),
        axis.title.y = element_text( size=18, face="bold"),
        axis.text.x = element_text(face="bold", size=12,color="black") ,
        axis.text.y = element_text(face="bold", size=12,color="black"))

must_graph_o

# red fox and pine marten 

gene_graph_o <- ggplot(data=Gene_o,aes(x=Gene_o[,2],y=Gene_o[,1]))+geom_line(color="orange",size=1.5) +
  ggtitle("Fox and pine marten west") +
  xlab("years") + ylab("log density") + theme_classic()+ 
  theme(plot.title = element_text(size=18, face="bold",hjust = 0.5),
        axis.title.x = element_text( size=18, face="bold"),
        axis.title.y = element_text( size=18, face="bold"),
        axis.text.x = element_text(face="bold", size=12,color="black") ,
        axis.text.y = element_text(face="bold", size=12,color="black"))

gene_graph_o

# avian predators 

aeri_graph_o <- ggplot(data=Aeri_o,aes(x=Aeri_o[,2],y=Aeri_o[,1]))+geom_line(color="darkblue",size=1.5) +
  ggtitle("Avian predators west") +
  xlab("years") + ylab("log density") + theme_classic()+ 
  theme(plot.title = element_text(size=18, face="bold",hjust = 0.5),
        axis.title.x = element_text( size=18, face="bold"),
        axis.title.y = element_text( size=18, face="bold"),
        axis.text.x = element_text(face="bold", size=12,color="black") ,
        axis.text.y = element_text(face="bold", size=12,color="black"))

aeri_graph_o

# Final figure and register 

A <- plot_grid(vole_graph_n,vole_graph_o,vole_graph_e,must_graph_n,must_graph_o,must_graph_e,gene_graph_n,gene_graph_o,gene_graph_e,aeri_graph_n,aeri_graph_o,aeri_graph_e, labels=c("1", "2","3","4","5","6","7","8","9","10","11","12"), ncol = 3, nrow = 4) 

# Opening the graphic device 
pdf("../Figures/Graph1_averages_density2.pdf",  
    width = 14, height = 8, 
    bg = "white",         
    colormodel = "cmyk")

# Creating a plot
plot(A)

# Closing the graphical device
dev.off() 
