
# author = "Besognet Thomas"
# date = " 24/05/23" 
# project = "Korpela et al. Proceedings B Replication work"
# name =  "figure 4 replication "

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
simulations <- read.csv("../data/simulations_indicators.csv",sep=",", header=T, dec=".", stringsAsFactors=FALSE)

# data cleaning : we don't want too explosive value for our representations 
simulations$direct.density.dependance[simulations$direct.density.dependance>40 ] <- NA
simulations$delayed.density.dependance[simulations$delayed.density.dependance>40] <- NA
simulations$s.index[simulations$s.index>40] <- NA
simulations$min_s[simulations$min_s>40] <- NA
simulations$max_s[simulations$max_s>40] <- NA
simulations$seasonality[simulations$seasonality>40] <- NA
simulations$min_n[simulations$min_n>40] <- NA
simulations$max_n[simulations$max_n>40] <- NA
simulations$seasonality[simulations$seasonality<(-40)] <- NA
simulations$min_n[simulations$min_n<(-40)] <- NA
simulations$max_n[simulations$max_n<(-40)] <- NA
simulations$delayed.density.dependance[is.na(simulations$direct.density.dependance)] <- NA
simulations$direct.density.dependance[is.na(simulations$delayed.density.dependance)] <- NA

# data cleaning 
simulations$scenario <- as.integer(simulations$scenario)
simulations$direct.density.dependance_p <- simulations$direct.density.dependance+1

simulations_north <- filter(simulations,zone==1)
simulations_east <- filter(simulations,zone==2)
simulations_west <- filter(simulations,zone==3)

# A) density dependance 

# A.1 north 

a1 <- ggplot(simulations_north) +
  geom_point(size=6,aes(x = direct.density.dependance_p,y = delayed.density.dependance,color = factor(scenario)))+
  scale_color_manual(name="scenario",values=c('black','pink','red',"yellow","orange","lightseagreen","darkblue","cornsilk4"))+
  geom_function(fun=function(x) -x^2 / 4, xlim=c(-2, 2))+
  geom_segment(aes(x = -2, y = -1, xend = 0, yend = 1))+
  geom_segment(aes(x = 0, y = 1, xend = 2, yend = -1))+
  geom_segment(aes(x = -2, y = -1, xend = 2, yend = -1))+
  coord_cartesian(xlim = c(-2, 2), ylim = c(-1, 1))+
  ggtitle("density dependance north") +
  xlab("direct density dependance+1") + ylab("delayed density dependance")+ theme_classic() + 
  theme(plot.title = element_text(size=18, face="bold",hjust = 0.5),
    axis.title.x = element_text( size=18, face="bold"),
    axis.title.y = element_text( size=18, face="bold"),
    axis.text.x = element_text(face="bold", size=12,color="black") ,
    axis.text.y = element_text(face="bold", size=12,color="black"),
    legend.background = element_rect(fill="grey70",linewidth=1, linetype="solid", colour ="black"),
    legend.text = element_text(colour="black", size=20, face="bold"),
    legend.title = element_text(colour="black", size=20, face="bold"))
                                    
a1



# A.2 west  

a2 <- ggplot(simulations_west) +
  geom_point(size=6,aes(x =direct.density.dependance_p,y =delayed.density.dependance,color = factor(scenario)))+
  scale_color_manual(name="scenario",values=c('black','pink','red',"yellow","orange","lightseagreen","darkblue","cornsilk4"))+
  geom_function(fun=function(x) -x^2 / 4, xlim=c(-2, 2))+
  geom_segment(aes(x = -2, y = -1, xend = 0, yend = 1))+
  geom_segment(aes(x = 0, y = 1, xend = 2, yend = -1))+
  geom_segment(aes(x = -2, y = -1, xend = 2, yend = -1))+
  coord_cartesian(xlim = c(-2, 2), ylim = c(-1, 1))+
  ggtitle("density dependance west") +
  xlab("direct density dependance+1") + ylab("delayed density dependance")+ theme_classic()+
  theme(plot.title = element_text(size=18, face="bold",hjust = 0.5),
      axis.title.x = element_text( size=18, face="bold"),
      axis.title.y = element_text( size=18, face="bold"),
      axis.text.x = element_text(face="bold", size=12,color="black") ,
      axis.text.y = element_text(face="bold", size=12,color="black"),
      legend.position='none')

a2

# A.3 east 

a3 <- ggplot(simulations_east) +
  geom_point(size=6,aes(x =direct.density.dependance_p,y =delayed.density.dependance,color = factor(scenario)))+
  scale_color_manual(name="scenario",values=c('black','pink','red',"yellow","orange","lightseagreen","darkblue","cornsilk4"))+
  geom_function(fun=function(x) -x^2 / 4, xlim=c(-2, 2))+
  geom_segment(aes(x = -2, y = -1, xend = 0, yend = 1))+
  geom_segment(aes(x = 0, y = 1, xend = 2, yend = -1))+
  geom_segment(aes(x = -2, y = -1, xend = 2, yend = -1))+
  coord_cartesian(xlim = c(-2, 2), ylim = c(-1, 1))+
  ggtitle("density dependance east") +
  xlab("direct density dependance+1") + ylab("delayed density dependance")+ theme_classic()+
  theme(plot.title = element_text(size=18, face="bold",hjust = 0.5),
        axis.title.x = element_text( size=18, face="bold"),
        axis.title.y = element_text( size=18, face="bold"),
        axis.text.x = element_text(face="bold", size=12,color="black") ,
        axis.text.y = element_text(face="bold", size=12,color="black"),
        legend.position='none')

a3

legend_a <- get_legend(a1+ theme(legend.position = 'right'))

# A.4 Global figure and register 

A <- plot_grid(a1+ theme(legend.position = 'none'),a2,a3, legend_a ,labels=c("1", "2","3"), ncol = 4, nrow = 1)

# Opening the graphic device 
pdf("../Figures/Graph4_1_figure4_density.pdf",  
    width = 14, height = 8, 
    bg = "white",         
    colormodel = "cmyk")

# Creating a plot
plot(A)

# Closing the graphical device
dev.off() 

# B) s-index

# B.1 north 

b1 <- ggplot(simulations_north, aes(x = factor(scenario), y=s.index, color=factor(scenario))) +
  geom_errorbar(aes(ymin = min_s, ymax = max_s), colour="black", width=0.4, position=position_dodge(0.1)) +
  geom_point(position=position_dodge(0.1), size=8, shape=20) +
  xlab("scenario") + ylab("s-index")+
  ggtitle("s-index north ") +
  scale_color_manual(name="scenario",values=c('black','pink','red',"yellow","orange","lightseagreen","darkblue","cornsilk4"))+
  theme_classic() +
  theme(plot.title = element_text(size=18, face="bold",hjust = 0.5),
        axis.title.x = element_text( size=18, face="bold"),
        axis.title.y = element_text( size=18, face="bold"),
        axis.text.x = element_text(face="bold", size=12,color="black") ,
        axis.text.y = element_text(face="bold", size=12,color="black"),
        legend.position='none')

b1

# B.2 west 

b2 <- ggplot(simulations_west, aes(x = factor(scenario), y=s.index, color=factor(scenario))) +
  geom_errorbar(aes(ymin = min_s, ymax = max_s), colour="black", width=0.4, position=position_dodge(0.1)) +
  geom_point(position=position_dodge(0.1), size=8, shape=20) +
  xlab("scenario") + ylab("s-index")+
  ggtitle("s-index west ") +
  scale_color_manual(name="scenario",values=c('black','pink','red',"yellow","orange","lightseagreen","darkblue","cornsilk4"))+
  theme_classic() +
  theme(plot.title = element_text(size=18, face="bold",hjust = 0.5),
        axis.title.x = element_text( size=18, face="bold"),
        axis.title.y = element_text( size=18, face="bold"),
        axis.text.x = element_text(face="bold", size=12,color="black") ,
        axis.text.y = element_text(face="bold", size=12,color="black"),
        legend.position='none')

b2

# B.3 east  

b3 <- ggplot(simulations_east, aes(x = factor(scenario), y=s.index, color=factor(scenario))) +
  geom_errorbar(aes(ymin = min_s, ymax = max_s), colour="black", width=0.4, position=position_dodge(0.1)) +
  geom_point(position=position_dodge(0.1), size=8, shape=20) +
  xlab("scenario") + ylab("s-index")+
  ggtitle("s-index east ") +
  scale_color_manual(name="scenario",values=c('black','pink','red',"yellow","orange","lightseagreen","darkblue","cornsilk4"))+
  theme_classic() +
  theme(plot.title = element_text(size=18, face="bold",hjust = 0.5),
        axis.title.x = element_text( size=18, face="bold"),
        axis.title.y = element_text( size=18, face="bold"),
        axis.text.x = element_text(face="bold", size=12,color="black") ,
        axis.text.y = element_text(face="bold", size=12,color="black"),
        legend.position='none')

b3


# B.4 Global figure and register 

B <- plot_grid(b1,b2,b3, labels=c("1", "2","3"), ncol = 3, nrow = 1)

# Opening the graphic device 
pdf("../Figures/Graph4_2_figure4_s_index.pdf",  
    width = 14, height = 8, 
    bg = "white",         
    colormodel = "cmyk")

# Creating a plot
plot(B)

# Closing the graphical device
dev.off() 


# C) seasonality 

# C.1 north 

c1 <- ggplot(simulations_north, aes(x = factor(scenario), y=seasonality, color=factor(scenario))) +
  geom_errorbar(aes(ymin = min_n, ymax = max_n), colour="black", width=0.4, position=position_dodge(0.1)) +
  geom_point(position=position_dodge(0.1), size=8, shape=20) +
  xlab("scenario") + ylab("seasonality")+
  ggtitle("seasonality north ") +
  scale_color_manual(name="scenario",values=c('black','pink','red',"yellow","orange","lightseagreen","darkblue","cornsilk4"))+
  theme_classic() +
  theme(plot.title = element_text(size=18, face="bold",hjust = 0.5),
        axis.title.x = element_text( size=18, face="bold"),
        axis.title.y = element_text( size=18, face="bold"),
        axis.text.x = element_text(face="bold", size=12,color="black") ,
        axis.text.y = element_text(face="bold", size=12,color="black"),
        legend.position='none')

c1

# C.2 west

c2 <- ggplot(simulations_west, aes(x = factor(scenario), y=seasonality, color=factor(scenario))) +
  geom_errorbar(aes(ymin = min_n, ymax = max_n), colour="black", width=0.4, position=position_dodge(0.1)) +
  geom_point(position=position_dodge(0.1), size=8, shape=20) +
  xlab("scenario") + ylab("seasonality")+
  ggtitle("seasonality west ") +
  scale_color_manual(name="scenario",values=c('black','pink','red',"yellow","orange","lightseagreen","darkblue","cornsilk4"))+
  theme_classic()+
  theme(plot.title = element_text(size=18, face="bold",hjust = 0.5),
        axis.title.x = element_text( size=18, face="bold"),
        axis.title.y = element_text( size=18, face="bold"),
        axis.text.x = element_text(face="bold", size=12,color="black") ,
        axis.text.y = element_text(face="bold", size=12,color="black"),
        legend.position='none')

c2

# C.3 east  

c3 <- ggplot(simulations_east, aes(x = factor(scenario), y=seasonality, color=factor(scenario))) +
  geom_errorbar(aes(ymin = min_n, ymax = max_n), colour="black", width=0.4, position=position_dodge(0.1)) +
  geom_point(position=position_dodge(0.1), size=8, shape=20) +
  xlab("scenario") + ylab("seasonality")+
  ggtitle("seasonality east ") +
  scale_color_manual(name="scenario",values=c('black','pink','red',"yellow","orange","lightseagreen","darkblue","cornsilk4"))+
  theme_classic()+
  theme(plot.title = element_text(size=18, face="bold",hjust = 0.5),
        axis.title.x = element_text( size=18, face="bold"),
        axis.title.y = element_text( size=18, face="bold"),
        axis.text.x = element_text(face="bold", size=12,color="black") ,
        axis.text.y = element_text(face="bold", size=12,color="black"),
        legend.position='none')

c3

# C.4 Global figure and register 

C <- plot_grid(c1,c2,c3, labels=c("1", "2","3"), ncol = 3, nrow = 1)

# Opening the graphic device 
pdf("../Figures/Graph4_3_figure4_saisonnality.pdf",  
    width = 14, height = 8, 
    bg = "white",         
    colormodel = "cmyk")

# Creating a plot
plot(C)

# Closing the graphical device
dev.off() 

#---------------------------------------------------------------------