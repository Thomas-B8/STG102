
# author = "Besognet Thomas"
# date = " 24/05/23" 
# project = "Estimation d'intéractions entre espèces à partir de séries temporelles"
# name =  "Vole density simulations " 

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
Vole <- read.csv("vole_density_simulations.csv",sep=",", header=T, dec=".", stringsAsFactors=FALSE)

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

s7n_data[,1] <- as.numeric(s7n_data[,1])
s7n_data[,2] <- as.numeric(s7n_data[,2])

s7n_graph <- ggplot(data=s7n_data,aes(x=s7n_data[,1],y=s7n_data[,2],group=1))+geom_line(color="blue",size=1) +
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

# Global figure north 

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

s5e_data[,1] <- as.numeric(s5e_data[,1])
s5e_data[,2] <- as.numeric(s5e_data[,2])

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

# Global figure east 

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

# Global figure west 

plot_grid(s1o_graph,a1_acf,s2o_graph,a2_acf,s3o_graph,a3_acf,s4o_graph,a4_acf,s5o_graph,a5_acf,s6o_graph,a6_acf,s7o_graph,a7_acf,s8o_graph,a8_acf, labels=c("1", "2","3","4","5","6","7","8","9","10","11","12","13","14","15","16"), ncol = 2, nrow = 8)

