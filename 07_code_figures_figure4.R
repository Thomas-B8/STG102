
# author = "Besognet Thomas"
# date = " 24/05/23" 
# project = "Estimation d'intéractions entre espèces à partir de séries temporelles"
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
simulations <- read.csv("simulations_indicators_without_big_values.csv",sep=",", header=T, dec=".", stringsAsFactors=FALSE)

simulations$scenario <- as.integer(simulations$scenario)
simulations$direct.density.dependance_p <- simulations$direct.density.dependance+1

simulations_nord <- filter(simulations,region==1)
simulations_est <- filter(simulations,region==2)
simulations_ouest <- filter(simulations,region==3)

# A) density dependance 

# A.1 north 

a1 <- ggplot(simulations_nord) +
  geom_point(size=6,aes(x = direct.density.dependance_p,y = delayed.density.dependance,color = factor(scenario)))+
  scale_color_manual(name="scenario",values=c('black','pink','red',"yellow","orange","darkgreen","darkblue","purple"))+
  geom_function(fun=function(x) -x^2 / 4, xlim=c(-2, 2))+
  geom_segment(aes(x = -2, y = -1, xend = 0, yend = 1))+
  geom_segment(aes(x = 0, y = 1, xend = 2, yend = -1))+
  geom_segment(aes(x = -2, y = -1, xend = 2, yend = -1))+
  coord_cartesian(xlim = c(-2, 2), ylim = c(-1, 1))+
  ggtitle("density dependance north") +
  xlab("direct density dependance+1") + ylab("delayed density dependance")+ theme_classic()

a1


# A.2 west  

a2 <- ggplot(simulations_ouest) +
  geom_point(size=6,aes(x =direct.density.dependance_p,y =delayed.density.dependance,color = factor(scenario)))+
  scale_color_manual(name="scenario",values=c('black','pink','red',"yellow","orange","darkgreen","darkblue","purple"))+
  geom_function(fun=function(x) -x^2 / 4, xlim=c(-2, 2))+
  geom_segment(aes(x = -2, y = -1, xend = 0, yend = 1))+
  geom_segment(aes(x = 0, y = 1, xend = 2, yend = -1))+
  geom_segment(aes(x = -2, y = -1, xend = 2, yend = -1))+
  coord_cartesian(xlim = c(-2, 2), ylim = c(-1, 1))+
  ggtitle("density dependance west") +
  xlab("direct density dependance+1") + ylab("delayed density dependance")+ theme_classic()

a2

# A.3 east 

a3 <- ggplot(simulations_est) +
  geom_point(size=6,aes(x =direct.density.dependance_p,y =delayed.density.dependance,color = factor(scenario)))+
  scale_color_manual(name="scenario",values=c('black','pink','red',"yellow","orange","darkgreen","darkblue","purple"))+
  geom_function(fun=function(x) -x^2 / 4, xlim=c(-2, 2))+
  geom_segment(aes(x = -2, y = -1, xend = 0, yend = 1))+
  geom_segment(aes(x = 0, y = 1, xend = 2, yend = -1))+
  geom_segment(aes(x = -2, y = -1, xend = 2, yend = -1))+
  coord_cartesian(xlim = c(-2, 2), ylim = c(-1, 1))+
  ggtitle("density dependance east") +
  xlab("direct density dependance+1") + ylab("delayed density dependance")+ theme_classic()

a3

# A.4 Global figure 

plot_grid(a1,a2,a3, labels=c("1", "2","3"), ncol = 3, nrow = 1)

# B) s-index

# B.1 north 

b1 <- ggplot(simulations_nord, aes(x = factor(scenario), y=s.index, color=factor(scenario))) +
  geom_errorbar(aes(ymin = min_s, ymax = max_s), colour="black", width=0.4, position=position_dodge(0.1)) +
  geom_point(position=position_dodge(0.1), size=8, shape=20) +
  xlab("scenario") + ylab("s-index")+
  ggtitle("s-index north ") +
  scale_color_manual(name="scenario",values=c('black','pink','red',"yellow","orange","darkgreen","darkblue","purple"))+
  theme_classic() 

b1

# B.2 west 

b2 <- ggplot(simulations_ouest, aes(x = factor(scenario), y=s.index, color=factor(scenario))) +
  geom_errorbar(aes(ymin = min_s, ymax = max_s), colour="black", width=0.4, position=position_dodge(0.1)) +
  geom_point(position=position_dodge(0.1), size=8, shape=20) +
  xlab("scenario") + ylab("s-index")+
  ggtitle("s-index west ") +
  scale_color_manual(name="scenario",values=c('black','pink','red',"yellow","orange","darkgreen","darkblue","purple"))+
  theme_classic() 

b2

# B.3 east  

b3 <- ggplot(simulations_est, aes(x = factor(scenario), y=s.index, color=factor(scenario))) +
  geom_errorbar(aes(ymin = min_s, ymax = max_s), colour="black", width=0.4, position=position_dodge(0.1)) +
  geom_point(position=position_dodge(0.1), size=8, shape=20) +
  xlab("scenario") + ylab("s-index")+
  ggtitle("s-index east ") +
  scale_color_manual(name="scenario",values=c('black','pink','red',"yellow","orange","darkgreen","darkblue","purple"))+
  theme_classic() 

b3

# B.4 Global figure  

plot_grid(b1,b2,b3, labels=c("1", "2","3"), ncol = 3, nrow = 1)

# C) seasonality 

# C.1 north 

c1 <- ggplot(simulations_nord, aes(x = factor(scenario), y=seasonality, color=factor(scenario))) +
  geom_errorbar(aes(ymin = min_n, ymax = max_n), colour="black", width=0.4, position=position_dodge(0.1)) +
  geom_point(position=position_dodge(0.1), size=8, shape=20) +
  xlab("scenario") + ylab("seasonality")+
  ggtitle("seasonality north ") +
  scale_color_manual(name="scenario",values=c('black','pink','red',"yellow","orange","darkgreen","darkblue","purple"))+
  theme_classic() 

c1

# C.2 west

c2 <- ggplot(simulations_ouest, aes(x = factor(scenario), y=seasonality, color=factor(scenario))) +
  geom_errorbar(aes(ymin = min_n, ymax = max_n), colour="black", width=0.4, position=position_dodge(0.1)) +
  geom_point(position=position_dodge(0.1), size=8, shape=20) +
  xlab("scenario") + ylab("seasonality")+
  ggtitle("seasonality west ") +
  scale_color_manual(name="scenario",values=c('black','pink','red',"yellow","orange","darkgreen","darkblue","purple"))+
  theme_classic()

c2

# C.3 east  

c3 <- ggplot(simulations_est, aes(x = factor(scenario), y=seasonality, color=factor(scenario))) +
  geom_errorbar(aes(ymin = min_n, ymax = max_n), colour="black", width=0.4, position=position_dodge(0.1)) +
  geom_point(position=position_dodge(0.1), size=8, shape=20) +
  xlab("scenario") + ylab("seasonality")+
  ggtitle("seasonality east ") +
  scale_color_manual(name="scenario",values=c('black','pink','red',"yellow","orange","darkgreen","darkblue","purple"))+
  theme_classic()

c3

# B.4 Global figure 

plot_grid(c1,c2,c3, labels=c("1", "2","3"), ncol = 3, nrow = 1)


#---------------------------------------------------------------------