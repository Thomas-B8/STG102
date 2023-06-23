The purpose of this project is the replication of  *Korpela et al. Proceedings B 2014* 

We suppose that the original data file "Korpela et al PRSL 2014 data" is stored in a parallel folder named "data".

8 codes are available

- *code donnees*: in this code we open the original data and we clean them, we also add new variables like longitude and latitude.

- *code modeles*: this code estimate the 15 models in the study: 5 populations (vole in spring, vole in autumn, small mustelids, generalist predators and avian predators) mutiplied by 3 regions (North, East and South-West). Also, in this code, we calculate the proportion of variance explained  for each models.

- *code simulations*: this code compute the 24 simulations : 8 scenarios ( all predators, without small mustelids, only small mustelids, without generalist predators, only generalist predators, without avian predators, only avian predators, without predators) multiplied by 3 regions (North, East and South-West). For each scenario: vole density, direct and delayed density dependance, s-index and seasonality are registered. 

- *code figures density all species*:  attempt the replication of the figure with the mean density for each species and each regions.

- *code figures figure2*:  attempt the replication of figure 2 in *Korpela et al. Proceedings B 2014*, i.e. the proportion of variance explained for the vole growth rate in winter and summer.

- *code figures figure3*:  attempt the replication of figure 3 in *Korpela et al. Proceedings B 2014*, i.e. proportion of variance explained for the three types of predators growth rate.

- *code figures figure4*:  attempt the replication of figure 4 in *Korpela et al. Proceedings B 2014*, with the estimated indicators from the simulations : direct and delayed density dependance, s-index and seasonality.

- *code figures vole density simulation*:  this code present the vole density by region predicted for all 24 simulations for the first 50 years (100 points).