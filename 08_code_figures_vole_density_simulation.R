
# author = "Besognet Thomas"
# date = " 24/05/23" 
# project = "Korpela et al. Proceedings B Replication work"
# name =  "Vole density simulations" 

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
Vole <- read.csv("../data/vole_density_simulations.csv",sep=",", header=T, dec=".", stringsAsFactors=FALSE)

# S1 - North 

s1n_data <- Vole[1:100,1:2]

s1n_graph <- ggplot(data=s1n_data,aes(x=s1n_data[,1],y=s1n_data[,2]))+geom_line(color="black",size=1) +
  ggtitle("Vole s1 north") +
  xlab("time") + ylab("Vole index") + theme_classic()+ 
  theme(plot.title = element_text(size=18, face="bold",hjust = 0.5),
        axis.title.x = element_text( size=18, face="bold"),
        axis.title.y = element_text( size=18, face="bold"),
        axis.text.x = element_text(face="bold", size=12,color="black") ,
        axis.text.y = element_text(face="bold", size=12,color="black"))

s1n_graph

a1 <- acf(s1n_data[,2])
a1_data <- with(a1, data.frame(lag, acf))
a1_acf <- ggplot(data=a1_data, mapping=aes(x=lag, y=acf)) +
  geom_bar(stat = "identity", position = "identity")+ theme_classic()+ 
  theme(plot.title = element_text(size=18, face="bold",hjust = 0.5),
        axis.title.x = element_text( size=18, face="bold"),
        axis.title.y = element_text( size=18, face="bold"),
        axis.text.x = element_text(face="bold", size=12,color="black") ,
        axis.text.y = element_text(face="bold", size=12,color="black"))
a1_acf

# S2 - North 

s2n_data <- Vole[1:100,c(1,5)]

s2n_graph <- ggplot(data=s2n_data,aes(x=s2n_data[,1],y=s2n_data[,2]))+geom_line(color="darkmagenta",size=1) +
  ggtitle("Vole s2 north") +
  xlab("time") + ylab("Vole index") + theme_classic()+ 
  theme(plot.title = element_text(size=18, face="bold",hjust = 0.5),
        axis.title.x = element_text( size=18, face="bold"),
        axis.title.y = element_text( size=18, face="bold"),
        axis.text.x = element_text(face="bold", size=12,color="black") ,
        axis.text.y = element_text(face="bold", size=12,color="black"))

s2n_graph

a2 <- acf(s2n_data[,2])
a2_data <- with(a2, data.frame(lag, acf))
a2_acf <- ggplot(data=a2_data, mapping=aes(x=lag, y=acf)) +
  geom_bar(stat = "identity", position = "identity")+ theme_classic()+ 
  theme(plot.title = element_text(size=18, face="bold",hjust = 0.5),
        axis.title.x = element_text( size=18, face="bold"),
        axis.title.y = element_text( size=18, face="bold"),
        axis.text.x = element_text(face="bold", size=12,color="black") ,
        axis.text.y = element_text(face="bold", size=12,color="black"))
a2_acf

# S3 - North 

s3n_data <- Vole[1:100,c(1,8)]

s3n_graph <- ggplot(data=s3n_data,aes(x=s3n_data[,1],y=s3n_data[,2]))+geom_line(color="red",size=1) +
  ggtitle("Vole s3 north") +
  xlab("time") + ylab("Vole index") + theme_classic()+ 
  theme(plot.title = element_text(size=18, face="bold",hjust = 0.5),
        axis.title.x = element_text( size=18, face="bold"),
        axis.title.y = element_text( size=18, face="bold"),
        axis.text.x = element_text(face="bold", size=12,color="black") ,
        axis.text.y = element_text(face="bold", size=12,color="black"))

s3n_graph

a3 <- acf(s3n_data[,2])
a3_data <- with(a3, data.frame(lag, acf))
a3_acf <- ggplot(data=a3_data, mapping=aes(x=lag, y=acf)) +
  geom_bar(stat = "identity", position = "identity")+ theme_classic()+ 
  theme(plot.title = element_text(size=18, face="bold",hjust = 0.5),
        axis.title.x = element_text( size=18, face="bold"),
        axis.title.y = element_text( size=18, face="bold"),
        axis.text.x = element_text(face="bold", size=12,color="black") ,
        axis.text.y = element_text(face="bold", size=12,color="black"))
a3_acf

# S4 - North 

s4n_data <- Vole[1:100,c(1,11)]

s4n_graph <- ggplot(data=s4n_data,aes(x=s4n_data[,1],y=s4n_data[,2]))+geom_line(color="goldenrod1",size=1) +
  ggtitle("Vole s4 north") +
  xlab("time") + ylab("Vole index") + theme_classic()+ 
  theme(plot.title = element_text(size=18, face="bold",hjust = 0.5),
        axis.title.x = element_text( size=18, face="bold"),
        axis.title.y = element_text( size=18, face="bold"),
        axis.text.x = element_text(face="bold", size=12,color="black") ,
        axis.text.y = element_text(face="bold", size=12,color="black"))

s4n_graph

a4 <- acf(s4n_data[,2])
a4_data <- with(a4, data.frame(lag, acf))
a4_acf <- ggplot(data=a4_data, mapping=aes(x=lag, y=acf)) +
  geom_bar(stat = "identity", position = "identity")+ theme_classic()+ 
  theme(plot.title = element_text(size=18, face="bold",hjust = 0.5),
        axis.title.x = element_text( size=18, face="bold"),
        axis.title.y = element_text( size=18, face="bold"),
        axis.text.x = element_text(face="bold", size=12,color="black") ,
        axis.text.y = element_text(face="bold", size=12,color="black"))
a4_acf

# S5 - North 

s5n_data <- Vole[1:100,c(1,14)]

s5n_graph <- ggplot(data=s5n_data,aes(x=s5n_data[,1],y=s5n_data[,2]))+geom_line(color="darkorange3",size=1) +
  ggtitle("Vole s5 north") +
  xlab("time") + ylab("Vole index") + theme_classic()+ 
  theme(plot.title = element_text(size=18, face="bold",hjust = 0.5),
        axis.title.x = element_text( size=18, face="bold"),
        axis.title.y = element_text( size=18, face="bold"),
        axis.text.x = element_text(face="bold", size=12,color="black") ,
        axis.text.y = element_text(face="bold", size=12,color="black"))

s5n_graph

a5 <- acf(s5n_data[,2])
a5_data <- with(a5, data.frame(lag, acf))
a5_acf <- ggplot(data=a5_data, mapping=aes(x=lag, y=acf)) +
  geom_bar(stat = "identity", position = "identity")+ theme_classic()+ 
  theme(plot.title = element_text(size=18, face="bold",hjust = 0.5),
        axis.title.x = element_text( size=18, face="bold"),
        axis.title.y = element_text( size=18, face="bold"),
        axis.text.x = element_text(face="bold", size=12,color="black") ,
        axis.text.y = element_text(face="bold", size=12,color="black"))
a5_acf

# S6 - North 

s6n_data <- Vole[1:100,c(1,17)]

s6n_graph <- ggplot(data=s6n_data,aes(x=s6n_data[,1],y=s6n_data[,2]))+geom_line(color="lightseagreen",size=1) +
  ggtitle("Vole s6 north") +
  xlab("time") + ylab("Vole index") + theme_classic()+ 
  theme(plot.title = element_text(size=18, face="bold",hjust = 0.5),
        axis.title.x = element_text( size=18, face="bold"),
        axis.title.y = element_text( size=18, face="bold"),
        axis.text.x = element_text(face="bold", size=12,color="black") ,
        axis.text.y = element_text(face="bold", size=12,color="black"))

s6n_graph

a6 <- acf(s6n_data[,2])
a6_data <- with(a6, data.frame(lag, acf))
a6_acf <- ggplot(data=a6_data, mapping=aes(x=lag, y=acf)) +
  geom_bar(stat = "identity", position = "identity")+ theme_classic()+ 
  theme(plot.title = element_text(size=18, face="bold",hjust = 0.5),
        axis.title.x = element_text( size=18, face="bold"),
        axis.title.y = element_text( size=18, face="bold"),
        axis.text.x = element_text(face="bold", size=12,color="black") ,
        axis.text.y = element_text(face="bold", size=12,color="black"))
a6_acf

# S7 - North 

s7n_data <- Vole[1:100,c(1,20)]

s7n_data[,1] <- as.numeric(s7n_data[,1])
s7n_data[,2] <- as.numeric(s7n_data[,2])

s7n_graph <- ggplot(data=s7n_data,aes(x=s7n_data[,1],y=s7n_data[,2],group=1))+geom_line(color="darkblue",size=1) +
  ggtitle("Vole s7 north") +
  xlab("time") + ylab("Vole index") + theme_classic()+ 
  theme(plot.title = element_text(size=18, face="bold",hjust = 0.5),
        axis.title.x = element_text( size=18, face="bold"),
        axis.title.y = element_text( size=18, face="bold"),
        axis.text.x = element_text(face="bold", size=12,color="black") ,
        axis.text.y = element_text(face="bold", size=12,color="black"))

s7n_graph

a7 <- acf(s7n_data[,2])
a7_data <- with(a7, data.frame(lag, acf))
a7_acf <- ggplot(data=a7_data, mapping=aes(x=lag, y=acf)) +
  geom_bar(stat = "identity", position = "identity")+ theme_classic()+ 
  theme(plot.title = element_text(size=18, face="bold",hjust = 0.5),
        axis.title.x = element_text( size=18, face="bold"),
        axis.title.y = element_text( size=18, face="bold"),
        axis.text.x = element_text(face="bold", size=12,color="black") ,
        axis.text.y = element_text(face="bold", size=12,color="black"))
a7_acf

# S8 - North 

s8n_data <- Vole[1:100,c(1,23)]

s8n_graph <- ggplot(data=s8n_data,aes(x=s8n_data[,1],y=s8n_data[,2]))+geom_line(color="grey30",size=1) +
  ggtitle("Vole s8 north") +
  xlab("time") + ylab("Vole index") + theme_classic()+ 
  theme(plot.title = element_text(size=18, face="bold",hjust = 0.5),
        axis.title.x = element_text( size=18, face="bold"),
        axis.title.y = element_text( size=18, face="bold"),
        axis.text.x = element_text(face="bold", size=12,color="black") ,
        axis.text.y = element_text(face="bold", size=12,color="black"))

s8n_graph

a8 <- acf(s8n_data[,2])
a8_data <- with(a8, data.frame(lag, acf))
a8_acf <- ggplot(data=a8_data, mapping=aes(x=lag, y=acf)) +
  geom_bar(stat = "identity", position = "identity")+ theme_classic()+ 
  theme(plot.title = element_text(size=18, face="bold",hjust = 0.5),
        axis.title.x = element_text( size=18, face="bold"),
        axis.title.y = element_text( size=18, face="bold"),
        axis.text.x = element_text(face="bold", size=12,color="black") ,
        axis.text.y = element_text(face="bold", size=12,color="black"))
a8_acf

# Global figure north and register 

A <- plot_grid(s1n_graph,a1_acf,s2n_graph,a2_acf,s3n_graph,a3_acf,s4n_graph,a4_acf,s5n_graph,a5_acf,s6n_graph,a6_acf,s7n_graph,a7_acf,s8n_graph,a8_acf, labels=c("1", "2","3","4","5","6","7","8","9","10","11","12","13","14","15","16"), ncol = 2, nrow = 8)

# Opening the graphic device 
pdf("../Figures/Graph5_1_simulations_north.pdf",  
    width = 8, height = 18, 
    bg = "white",         
    colormodel = "cmyk")

# Creating a plot
plot(A)

# Closing the graphical device
dev.off() 

# S1 - East

s1e_data <- Vole[1:100,c(1,3)]

s1e_graph <- ggplot(data=s1e_data,aes(x=s1e_data[,1],y=s1e_data[,2]))+geom_line(color="black",size=1) +
  ggtitle("Vole s1 east") +
  xlab("time") + ylab("Vole index") + theme_classic()+ 
  theme(plot.title = element_text(size=18, face="bold",hjust = 0.5),
        axis.title.x = element_text( size=18, face="bold"),
        axis.title.y = element_text( size=18, face="bold"),
        axis.text.x = element_text(face="bold", size=12,color="black") ,
        axis.text.y = element_text(face="bold", size=12,color="black"))

s1e_graph

a1 <- acf(s1e_data[,2])
a1_data <- with(a1, data.frame(lag, acf))
a1_acf <- ggplot(data=a1_data, mapping=aes(x=lag, y=acf)) +
  geom_bar(stat = "identity", position = "identity")+ theme_classic()+ 
  theme(plot.title = element_text(size=18, face="bold",hjust = 0.5),
        axis.title.x = element_text( size=18, face="bold"),
        axis.title.y = element_text( size=18, face="bold"),
        axis.text.x = element_text(face="bold", size=12,color="black") ,
        axis.text.y = element_text(face="bold", size=12,color="black"))
a1_acf

# S2 - East 

s2e_data <- Vole[1:100,c(1,6)]

s2e_graph <- ggplot(data=s2e_data,aes(x=s2e_data[,1],y=s2e_data[,2]))+geom_line(color="darkmagenta",size=1) +
  ggtitle("Vole s2 east") +
  xlab("time") + ylab("Vole index") + theme_classic()+ 
  theme(plot.title = element_text(size=18, face="bold",hjust = 0.5),
        axis.title.x = element_text( size=18, face="bold"),
        axis.title.y = element_text( size=18, face="bold"),
        axis.text.x = element_text(face="bold", size=12,color="black") ,
        axis.text.y = element_text(face="bold", size=12,color="black"))

s2e_graph

a2 <- acf(s2e_data[,2])
a2_data <- with(a2, data.frame(lag, acf))
a2_acf <- ggplot(data=a2_data, mapping=aes(x=lag, y=acf)) +
  geom_bar(stat = "identity", position = "identity")+ theme_classic()+ 
  theme(plot.title = element_text(size=18, face="bold",hjust = 0.5),
        axis.title.x = element_text( size=18, face="bold"),
        axis.title.y = element_text( size=18, face="bold"),
        axis.text.x = element_text(face="bold", size=12,color="black") ,
        axis.text.y = element_text(face="bold", size=12,color="black"))
a2_acf

# S3 - East

s3e_data <- Vole[1:100,c(1,9)]

s3e_graph <- ggplot(data=s3e_data,aes(x=s3e_data[,1],y=s3e_data[,2]))+geom_line(color="red",size=1) +
  ggtitle("Vole s3 east") +
  xlab("time") + ylab("Vole index") + theme_classic()+ 
  theme(plot.title = element_text(size=18, face="bold",hjust = 0.5),
        axis.title.x = element_text( size=18, face="bold"),
        axis.title.y = element_text( size=18, face="bold"),
        axis.text.x = element_text(face="bold", size=12,color="black") ,
        axis.text.y = element_text(face="bold", size=12,color="black"))

s3e_graph

a3 <- acf(s3e_data[,2])
a3_data <- with(a3, data.frame(lag, acf))
a3_acf <- ggplot(data=a3_data, mapping=aes(x=lag, y=acf)) +
  geom_bar(stat = "identity", position = "identity")+ theme_classic()+ 
  theme(plot.title = element_text(size=18, face="bold",hjust = 0.5),
        axis.title.x = element_text( size=18, face="bold"),
        axis.title.y = element_text( size=18, face="bold"),
        axis.text.x = element_text(face="bold", size=12,color="black") ,
        axis.text.y = element_text(face="bold", size=12,color="black"))
a3_acf

# S4 - East

s4e_data <- Vole[1:100,c(1,12)]

s4e_graph <- ggplot(data=s4e_data,aes(x=s4e_data[,1],y=s4e_data[,2]))+geom_line(color="goldenrod1",size=1) +
  ggtitle("Vole s4 east") +
  xlab("time") + ylab("Vole index") + theme_classic()+ 
  theme(plot.title = element_text(size=18, face="bold",hjust = 0.5),
        axis.title.x = element_text( size=18, face="bold"),
        axis.title.y = element_text( size=18, face="bold"),
        axis.text.x = element_text(face="bold", size=12,color="black") ,
        axis.text.y = element_text(face="bold", size=12,color="black"))

s4e_graph

a4 <- acf(s4e_data[,2])
a4_data <- with(a4, data.frame(lag, acf))
a4_acf <- ggplot(data=a4_data, mapping=aes(x=lag, y=acf)) +
  geom_bar(stat = "identity", position = "identity")+ theme_classic()+ 
  theme(plot.title = element_text(size=18, face="bold",hjust = 0.5),
        axis.title.x = element_text( size=18, face="bold"),
        axis.title.y = element_text( size=18, face="bold"),
        axis.text.x = element_text(face="bold", size=12,color="black") ,
        axis.text.y = element_text(face="bold", size=12,color="black"))
a4_acf

# S5 - East

s5e_data <- Vole[1:100,c(1,15)]

s5e_data[,1] <- as.numeric(s5e_data[,1])
s5e_data[,2] <- as.numeric(s5e_data[,2])

s5e_graph <- ggplot(data=s5e_data,aes(x=s5e_data[,1],y=s5e_data[,2]))+geom_line(color="darkorange3",size=1) +
  ggtitle("Vole s5 east") +
  xlab("time") + ylab("Vole index") + theme_classic()+ 
  theme(plot.title = element_text(size=18, face="bold",hjust = 0.5),
        axis.title.x = element_text( size=18, face="bold"),
        axis.title.y = element_text( size=18, face="bold"),
        axis.text.x = element_text(face="bold", size=12,color="black") ,
        axis.text.y = element_text(face="bold", size=12,color="black"))

s5e_graph

a5 <- acf(s5e_data[,2])
a5_data <- with(a5, data.frame(lag, acf))
a5_acf <- ggplot(data=a5_data, mapping=aes(x=lag, y=acf)) +
  geom_bar(stat = "identity", position = "identity")+ theme_classic()+ 
  theme(plot.title = element_text(size=18, face="bold",hjust = 0.5),
        axis.title.x = element_text( size=18, face="bold"),
        axis.title.y = element_text( size=18, face="bold"),
        axis.text.x = element_text(face="bold", size=12,color="black") ,
        axis.text.y = element_text(face="bold", size=12,color="black"))
a5_acf

# S6 - East 

s6e_data <- Vole[1:100,c(1,18)]

s6e_graph <- ggplot(data=s6e_data,aes(x=s6e_data[,1],y=s6e_data[,2]))+geom_line(color="lightseagreen",size=1) +
  ggtitle("Vole s6 east") +
  xlab("time") + ylab("Vole index") + theme_classic()+ 
  theme(plot.title = element_text(size=18, face="bold",hjust = 0.5),
        axis.title.x = element_text( size=18, face="bold"),
        axis.title.y = element_text( size=18, face="bold"),
        axis.text.x = element_text(face="bold", size=12,color="black") ,
        axis.text.y = element_text(face="bold", size=12,color="black"))

s6e_graph

a6 <- acf(s6e_data[,2])
a6_data <- with(a6, data.frame(lag, acf))
a6_acf <- ggplot(data=a6_data, mapping=aes(x=lag, y=acf)) +
  geom_bar(stat = "identity", position = "identity")+ theme_classic()+ 
  theme(plot.title = element_text(size=18, face="bold",hjust = 0.5),
        axis.title.x = element_text( size=18, face="bold"),
        axis.title.y = element_text( size=18, face="bold"),
        axis.text.x = element_text(face="bold", size=12,color="black") ,
        axis.text.y = element_text(face="bold", size=12,color="black"))
a6_acf

# S7 - East

s7e_data <- Vole[1:100,c(1,21)]

s7e_graph <- ggplot(data=s7e_data,aes(x=s7e_data[,1],y=s7e_data[,2]))+geom_line(color="darkblue",size=1) +
  ggtitle("Vole s7 east") +
  xlab("time") + ylab("Vole index") + theme_classic()+ 
  theme(plot.title = element_text(size=18, face="bold",hjust = 0.5),
        axis.title.x = element_text( size=18, face="bold"),
        axis.title.y = element_text( size=18, face="bold"),
        axis.text.x = element_text(face="bold", size=12,color="black") ,
        axis.text.y = element_text(face="bold", size=12,color="black"))

s7e_graph

a7 <- acf(s7e_data[,2])
a7_data <- with(a7, data.frame(lag, acf))
a7_acf <- ggplot(data=a7_data, mapping=aes(x=lag, y=acf)) +
  geom_bar(stat = "identity", position = "identity")+ theme_classic()+ 
  theme(plot.title = element_text(size=18, face="bold",hjust = 0.5),
        axis.title.x = element_text( size=18, face="bold"),
        axis.title.y = element_text( size=18, face="bold"),
        axis.text.x = element_text(face="bold", size=12,color="black") ,
        axis.text.y = element_text(face="bold", size=12,color="black"))
a7_acf

# S8 - East

s8e_data <- Vole[1:100,c(1,24)]

s8e_graph <- ggplot(data=s8e_data,aes(x=s8e_data[,1],y=s8e_data[,2]))+geom_line(color="grey30",size=1) +
  ggtitle("Vole s8 east") +
  xlab("time") + ylab("Vole index") + theme_classic()+ 
  theme(plot.title = element_text(size=18, face="bold",hjust = 0.5),
        axis.title.x = element_text( size=18, face="bold"),
        axis.title.y = element_text( size=18, face="bold"),
        axis.text.x = element_text(face="bold", size=12,color="black") ,
        axis.text.y = element_text(face="bold", size=12,color="black"))

s8e_graph

a8 <- acf(s8e_data[,2])
a8_data <- with(a8, data.frame(lag, acf))
a8_acf <- ggplot(data=a8_data, mapping=aes(x=lag, y=acf)) +
  geom_bar(stat = "identity", position = "identity")+ theme_classic()+ 
  theme(plot.title = element_text(size=18, face="bold",hjust = 0.5),
        axis.title.x = element_text( size=18, face="bold"),
        axis.title.y = element_text( size=18, face="bold"),
        axis.text.x = element_text(face="bold", size=12,color="black") ,
        axis.text.y = element_text(face="bold", size=12,color="black"))
a8_acf

# Global figure east and register 

B <- plot_grid(s1e_graph,a1_acf,s2e_graph,a2_acf,s3e_graph,a3_acf,s4e_graph,a4_acf,s5e_graph,a5_acf,s6e_graph,a6_acf,s7e_graph,a7_acf,s8e_graph,a8_acf, labels=c("1", "2","3","4","5","6","7","8","9","10","11","12","13","14","15","16"), ncol = 2, nrow = 8)

# Opening the graphic device 
pdf("../Figures/Graph5_2_simulations_east.pdf",  
    width = 8, height = 18, 
    bg = "white",         
    colormodel = "cmyk")

# Creating a plot
plot(B)

# Closing the graphical device
dev.off() 


# S1 - West

s1o_data <- Vole[1:100,1:4]

s1o_graph <- ggplot(data=s1o_data,aes(x=s1o_data[,1],y=s1o_data[,2]))+geom_line(color="black",size=1) +
  ggtitle("Vole s1 west") +
  xlab("time") + ylab("Vole index") + theme_classic()+ 
  theme(plot.title = element_text(size=18, face="bold",hjust = 0.5),
        axis.title.x = element_text( size=18, face="bold"),
        axis.title.y = element_text( size=18, face="bold"),
        axis.text.x = element_text(face="bold", size=12,color="black") ,
        axis.text.y = element_text(face="bold", size=12,color="black"))

s1o_graph

a1 <- acf(s1o_data[,2])
a1_data <- with(a1, data.frame(lag, acf))
a1_acf <- ggplot(data=a1_data, mapping=aes(x=lag, y=acf)) +
  geom_bar(stat = "identity", position = "identity")+ theme_classic()+ 
  theme(plot.title = element_text(size=18, face="bold",hjust = 0.5),
        axis.title.x = element_text( size=18, face="bold"),
        axis.title.y = element_text( size=18, face="bold"),
        axis.text.x = element_text(face="bold", size=12,color="black") ,
        axis.text.y = element_text(face="bold", size=12,color="black"))
a1_acf

# S2 - West 

s2o_data <- Vole[1:100,c(1,7)]

s2o_graph <- ggplot(data=s2o_data,aes(x=s2o_data[,1],y=s2o_data[,2]))+geom_line(color="darkmagenta",size=1) +
  ggtitle("Vole s2 west") +
  xlab("time") + ylab("Vole index") + theme_classic()+ 
  theme(plot.title = element_text(size=18, face="bold",hjust = 0.5),
        axis.title.x = element_text( size=18, face="bold"),
        axis.title.y = element_text( size=18, face="bold"),
        axis.text.x = element_text(face="bold", size=12,color="black") ,
        axis.text.y = element_text(face="bold", size=12,color="black"))

s2o_graph

a2 <- acf(s2o_data[,2])
a2_data <- with(a2, data.frame(lag, acf))
a2_acf <- ggplot(data=a2_data, mapping=aes(x=lag, y=acf)) +
  geom_bar(stat = "identity", position = "identity")+ theme_classic()+ 
  theme(plot.title = element_text(size=18, face="bold",hjust = 0.5),
        axis.title.x = element_text( size=18, face="bold"),
        axis.title.y = element_text( size=18, face="bold"),
        axis.text.x = element_text(face="bold", size=12,color="black") ,
        axis.text.y = element_text(face="bold", size=12,color="black"))
a2_acf

# S3 - West

s3o_data <- Vole[1:100,c(1,10)]

s3o_graph <- ggplot(data=s3o_data,aes(x=s3o_data[,1],y=s3o_data[,2]))+geom_line(color="red",size=1) +
  ggtitle("Vole s3 west") +
  xlab("time") + ylab("Vole index") + theme_classic()+ 
  theme(plot.title = element_text(size=18, face="bold",hjust = 0.5),
        axis.title.x = element_text( size=18, face="bold"),
        axis.title.y = element_text( size=18, face="bold"),
        axis.text.x = element_text(face="bold", size=12,color="black") ,
        axis.text.y = element_text(face="bold", size=12,color="black"))

s3o_graph

a3 <- acf(s3o_data[,2])
a3_data <- with(a3, data.frame(lag, acf))
a3_acf <- ggplot(data=a3_data, mapping=aes(x=lag, y=acf)) +
  geom_bar(stat = "identity", position = "identity")+ theme_classic()+ 
  theme(plot.title = element_text(size=18, face="bold",hjust = 0.5),
        axis.title.x = element_text( size=18, face="bold"),
        axis.title.y = element_text( size=18, face="bold"),
        axis.text.x = element_text(face="bold", size=12,color="black") ,
        axis.text.y = element_text(face="bold", size=12,color="black"))
a3_acf

# S4 - West

s4o_data <- Vole[1:100,c(1,13)]

s4o_graph <- ggplot(data=s4o_data,aes(x=s4o_data[,1],y=s4o_data[,2]))+geom_line(color="goldenrod1",size=1) +
  ggtitle("Vole s4 west") +
  xlab("time") + ylab("Vole index") + theme_classic()+ 
  theme(plot.title = element_text(size=18, face="bold",hjust = 0.5),
        axis.title.x = element_text( size=18, face="bold"),
        axis.title.y = element_text( size=18, face="bold"),
        axis.text.x = element_text(face="bold", size=12,color="black") ,
        axis.text.y = element_text(face="bold", size=12,color="black"))

s4o_graph

a4 <- acf(s4o_data[,2])
a4_data <- with(a4, data.frame(lag, acf))
a4_acf <- ggplot(data=a4_data, mapping=aes(x=lag, y=acf)) +
  geom_bar(stat = "identity", position = "identity")+ theme_classic()+ 
  theme(plot.title = element_text(size=18, face="bold",hjust = 0.5),
        axis.title.x = element_text( size=18, face="bold"),
        axis.title.y = element_text( size=18, face="bold"),
        axis.text.x = element_text(face="bold", size=12,color="black") ,
        axis.text.y = element_text(face="bold", size=12,color="black"))
a4_acf

# S5 - West

s5o_data <- Vole[1:100,c(1,16)]

s5o_graph <- ggplot(data=s5o_data,aes(x=s5o_data[,1],y=s5o_data[,2]))+geom_line(color="darkorange3",size=1) +
  ggtitle("Vole s5 west") +
  xlab("time") + ylab("Vole index") + theme_classic()+ 
  theme(plot.title = element_text(size=18, face="bold",hjust = 0.5),
        axis.title.x = element_text( size=18, face="bold"),
        axis.title.y = element_text( size=18, face="bold"),
        axis.text.x = element_text(face="bold", size=12,color="black") ,
        axis.text.y = element_text(face="bold", size=12,color="black"))

s5o_graph

a5 <- acf(s5o_data[,2])
a5_data <- with(a5, data.frame(lag, acf))
a5_acf <- ggplot(data=a5_data, mapping=aes(x=lag, y=acf)) +
  geom_bar(stat = "identity", position = "identity")+ theme_classic()+ 
  theme(plot.title = element_text(size=18, face="bold",hjust = 0.5),
        axis.title.x = element_text( size=18, face="bold"),
        axis.title.y = element_text( size=18, face="bold"),
        axis.text.x = element_text(face="bold", size=12,color="black") ,
        axis.text.y = element_text(face="bold", size=12,color="black"))
a5_acf

# S6 - West

s6o_data <- Vole[1:100,c(1,19)]

s6o_graph <- ggplot(data=s6o_data,aes(x=s6o_data[,1],y=s6o_data[,2]))+geom_line(color="lightseagreen",size=1) +
  ggtitle("Vole s6 west") +
  xlab("time") + ylab("Vole index") + theme_classic()+ 
  theme(plot.title = element_text(size=18, face="bold",hjust = 0.5),
        axis.title.x = element_text( size=18, face="bold"),
        axis.title.y = element_text( size=18, face="bold"),
        axis.text.x = element_text(face="bold", size=12,color="black") ,
        axis.text.y = element_text(face="bold", size=12,color="black"))

s6n_graph

a6 <- acf(s6o_data[,2])
a6_data <- with(a6, data.frame(lag, acf))
a6_acf <- ggplot(data=a6_data, mapping=aes(x=lag, y=acf)) +
  geom_bar(stat = "identity", position = "identity")+ theme_classic()+ 
  theme(plot.title = element_text(size=18, face="bold",hjust = 0.5),
        axis.title.x = element_text( size=18, face="bold"),
        axis.title.y = element_text( size=18, face="bold"),
        axis.text.x = element_text(face="bold", size=12,color="black") ,
        axis.text.y = element_text(face="bold", size=12,color="black"))
a6_acf

# S7 - West

s7o_data <- Vole[1:100,c(1,22)]

s7o_graph <- ggplot(data=s7o_data,aes(x=s7o_data[,1],y=s7o_data[,2]))+geom_line(color="darkblue",size=1) +
  ggtitle("Vole s7 west") +
  xlab("time") + ylab("Vole index") + theme_classic()+ 
  theme(plot.title = element_text(size=18, face="bold",hjust = 0.5),
        axis.title.x = element_text( size=18, face="bold"),
        axis.title.y = element_text( size=18, face="bold"),
        axis.text.x = element_text(face="bold", size=12,color="black") ,
        axis.text.y = element_text(face="bold", size=12,color="black"))

s7o_graph

a7 <- acf(s7o_data[,2])
a7_data <- with(a7, data.frame(lag, acf))
a7_acf <- ggplot(data=a7_data, mapping=aes(x=lag, y=acf)) +
  geom_bar(stat = "identity", position = "identity")+ theme_classic()+ 
  theme(plot.title = element_text(size=18, face="bold",hjust = 0.5),
        axis.title.x = element_text( size=18, face="bold"),
        axis.title.y = element_text( size=18, face="bold"),
        axis.text.x = element_text(face="bold", size=12,color="black") ,
        axis.text.y = element_text(face="bold", size=12,color="black"))
a7_acf

# S8 - West

s8o_data <- Vole[1:100,c(1,25)]

s8o_graph <- ggplot(data=s8o_data,aes(x=s8o_data[,1],y=s8o_data[,2]))+geom_line(color="grey30",size=1) +
  ggtitle("Vole s8 west") +
  xlab("time") + ylab("Vole index") + theme_classic()+ 
  theme(plot.title = element_text(size=18, face="bold",hjust = 0.5),
        axis.title.x = element_text( size=18, face="bold"),
        axis.title.y = element_text( size=18, face="bold"),
        axis.text.x = element_text(face="bold", size=12,color="black") ,
        axis.text.y = element_text(face="bold", size=12,color="black"))

s8o_graph

a8 <- acf(s8o_data[,2])
a8_data <- with(a8, data.frame(lag, acf))
a8_acf <- ggplot(data=a8_data, mapping=aes(x=lag, y=acf)) +
  geom_bar(stat = "identity", position = "identity")+ theme_classic()+ 
  theme(plot.title = element_text(size=18, face="bold",hjust = 0.5),
        axis.title.x = element_text( size=18, face="bold"),
        axis.title.y = element_text( size=18, face="bold"),
        axis.text.x = element_text(face="bold", size=12,color="black") ,
        axis.text.y = element_text(face="bold", size=12,color="black"))
a8_acf

# Global figure west 

C <- plot_grid(s1o_graph,a1_acf,s2o_graph,a2_acf,s3o_graph,a3_acf,s4o_graph,a4_acf,s5o_graph,a5_acf,s6o_graph,a6_acf,s7o_graph,a7_acf,s8o_graph,a8_acf, labels=c("1", "2","3","4","5","6","7","8","9","10","11","12","13","14","15","16"), ncol = 2, nrow = 8)

# Opening the graphic device 
pdf("../Figures/Graph5_3_simulations_west.pdf",  
    width = 8, height = 18, 
    bg = "white",         
    colormodel = "cmyk")

# Creating a plot
plot(C)

# Closing the graphical device
dev.off() 
