# Besognet Thomas, 07/04/23 , Stage : Modéles 

# packages 

# importation des données 
setwd("C:\\Users\\thoma\\OneDrive\\Documents\\ETUDES\\SP S2\\STG102_Stage\\Statistiques")
donnees_2 <- read.csv("donnees_2.csv", sep=";", header=T, dec=".")

# Les estimations de modèles 

# I : Répartir les données en deux groupes : 50% pour élaborer le modèle, 50% pour le tester 
# 50 répartitions à créer pour éviter d'avoir un effet trop important de l'aléatoire 

# II : Estimer les paramètres des modèles 
# 20 modéles à tester (5 espéces X 4 cardinalités), sachant que chaque modéle est une moyenne 
# 50 modèles avec les répartitions ci dessus 


# III: Calculs de variance et varaince expliquées par chaque espéce pour la prédiction de chaque espèce 
# Pour chacun des 20 modèles, il faut donc faire un graphique avec les variances expliquées 

# IV : Simulations pour le futur et dépendances de densité 
# voir plus tard 

