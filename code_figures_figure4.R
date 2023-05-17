# Besognet Thomas, 17/05/23 , Stage : Figures et tableaux : Réplication de la figure 4 du modèle : density dependance, s-index et  seasonality

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

# Réplication de la figure 4

#---------------------------------------------------------------------------------------------------------------------------------------------------------------------

# Importation des données

simulations <- read.csv("simulations_sansva.csv",sep=",", header=T, dec=".", stringsAsFactors=FALSE)

simulations$scenario <- as.integer(simulations$scenario)
simulations$direct.density.dependance_p <- simulations$direct.density.dependance+1

simulations_nord <- filter(simulations,region==1)
simulations_est <- filter(simulations,region==2)
simulations_ouest <- filter(simulations,region==3)

# A) density dependance 

# A.1 le nord 

a1 <- ggplot(simulations_nord) +
  geom_point(aes(x = direct.density.dependance_p,y = delayed.density.dependance,color = factor(scenario),size=0.5))+
  scale_color_manual(name="scenario",values=c('black','pink','red',"yellow","orange","darkgreen","darkblue","purple"))+
  geom_function(fun=function(x) -x^2 / 4, xlim=c(-2, 2))+
  geom_segment(aes(x = -2, y = -1, xend = 0, yend = 1))+
  geom_segment(aes(x = 0, y = 1, xend = 2, yend = -1))+
  geom_segment(aes(x = -2, y = -1, xend = 2, yend = -1))+
  coord_cartesian(xlim = c(-2, 2), ylim = c(-1, 1))+
  ggtitle("density dependance north") +
  xlab("direct density dependance ") + ylab("delayed density dependance")+ theme_classic()

a1


# A.2 le sud ouest 

a2 <- ggplot(simulations_ouest) +
  geom_point(aes(x =direct.density.dependance_p,y =delayed.density.dependance,color = factor(scenario),size=0.5))+
  scale_color_manual(name="scenario",values=c('black','pink','red',"yellow","orange","darkgreen","darkblue","purple"))+
  geom_function(fun=function(x) -x^2 / 4, xlim=c(-2, 2))+
  geom_segment(aes(x = -2, y = -1, xend = 0, yend = 1))+
  geom_segment(aes(x = 0, y = 1, xend = 2, yend = -1))+
  geom_segment(aes(x = -2, y = -1, xend = 2, yend = -1))+
  coord_cartesian(xlim = c(-2, 2), ylim = c(-1, 1))+
  ggtitle("density dependance west") +
  xlab("direct density dependance ") + ylab("delayed density dependance")+ theme_classic()

a2

# A.3 l'est

a3 <- ggplot(simulations_est) +
  geom_point(aes(x =direct.density.dependance_p,y =delayed.density.dependance,color = factor(scenario),size=0.5))+
  scale_color_manual(name="scenario",values=c('black','pink','red',"yellow","orange","darkgreen","darkblue","purple"))+
  geom_function(fun=function(x) -x^2 / 4, xlim=c(-2, 2))+
  geom_segment(aes(x = -2, y = -1, xend = 0, yend = 1))+
  geom_segment(aes(x = 0, y = 1, xend = 2, yend = -1))+
  geom_segment(aes(x = -2, y = -1, xend = 2, yend = -1))+
  coord_cartesian(xlim = c(-2, 2), ylim = c(-1, 1))+
  ggtitle("density dependance east") +
  xlab("direct density dependance ") + ylab("delayed density dependance")+ theme_classic()

a3

# A.4 Graphique global 

plot_grid(a1,a2,a3, labels=c("1", "2","3"), ncol = 3, nrow = 1)

# B) s-index

# B.1 le nord 

b1 <- ggplot(simulations_nord, aes(x = factor(scenario), y=s.index, color=factor(scenario))) +
  geom_errorbar(aes(ymin = min_s, ymax = max_s), colour="black", width=.1, position=position_dodge(0.1)) +
  geom_point(position=position_dodge(0.1), size=3, shape=15, fill="white") +
  xlab("scenario") + ylab("s-index")+
  ggtitle("s-index north ") +
  scale_color_manual(name="scenario",values=c('black','pink','red',"yellow","orange","darkgreen","darkblue","purple"))+
  theme_classic() 

b1

# B.2 le sud ouest

b2 <- ggplot(simulations_ouest, aes(x = factor(scenario), y=s.index, color=factor(scenario))) +
  geom_errorbar(aes(ymin = min_s, ymax = max_s), colour="black", width=.1, position=position_dodge(0.1)) +
  geom_point(position=position_dodge(0.1), size=3, shape=15, fill="white") +
  xlab("scenario") + ylab("s-index")+
  ggtitle("s-index west ") +
  scale_color_manual(name="scenario",values=c('black','pink','red',"yellow","orange","darkgreen","darkblue","purple"))+
  theme_classic() 

b2

# B.3 l'est  

b3 <- ggplot(simulations_est, aes(x = factor(scenario), y=s.index, color=factor(scenario))) +
  geom_errorbar(aes(ymin = min_s, ymax = max_s), colour="black", width=.1, position=position_dodge(0.1)) +
  geom_point(position=position_dodge(0.1), size=3, shape=15, fill="white") +
  xlab("scenario") + ylab("s-index")+
  ggtitle("s-index east ") +
  scale_color_manual(name="scenario",values=c('black','pink','red',"yellow","orange","darkgreen","darkblue","purple"))+
  theme_classic() 

b3

# B.4 Graphique global 

plot_grid(b1,b2,b3, labels=c("1", "2","3"), ncol = 3, nrow = 1)

# C) saisonnalite

# C.1 le nord 

c1 <- ggplot(simulations_nord, aes(x = factor(scenario), y=seasonality, color=factor(scenario))) +
  geom_errorbar(aes(ymin = min_n, ymax = max_n), colour="black", width=.1, position=position_dodge(0.1)) +
  geom_point(position=position_dodge(0.1), size=3, shape=15, fill="white") +
  xlab("scenario") + ylab("seasonality")+
  ggtitle("seasonality north ") +
  scale_color_manual(name="scenario",values=c('black','pink','red',"yellow","orange","darkgreen","darkblue","purple"))+
  theme_classic() 

c1

# C.2 le sud ouest

c2 <- ggplot(simulations_ouest, aes(x = factor(scenario), y=seasonality, color=factor(scenario))) +
  geom_errorbar(aes(ymin = min_n, ymax = max_n), colour="black", width=.1, position=position_dodge(0.1)) +
  geom_point(position=position_dodge(0.1), size=3, shape=15, fill="white") +
  xlab("scenario") + ylab("seasonality")+
  ggtitle("seasonality west ") +
  scale_color_manual(name="scenario",values=c('black','pink','red',"yellow","orange","darkgreen","darkblue","purple"))+
  theme_classic()

c2

# C.3 l'est  

c3 <- ggplot(simulations_est, aes(x = factor(scenario), y=seasonality, color=factor(scenario))) +
  geom_errorbar(aes(ymin = min_n, ymax = max_n), colour="black", width=.1, position=position_dodge(0.1)) +
  geom_point(position=position_dodge(0.1), size=3, shape=15, fill="white") +
  xlab("scenario") + ylab("seasonality")+
  ggtitle("seasonality east ") +
  scale_color_manual(name="scenario",values=c('black','pink','red',"yellow","orange","darkgreen","darkblue","purple"))+
  theme_classic()

c3

# B.4 Graphique global 

plot_grid(c1,c2,c3, labels=c("1", "2","3"), ncol = 3, nrow = 1)


#---------------------------------------------------------------------