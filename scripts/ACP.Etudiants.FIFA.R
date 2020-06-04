## Script pour le traitement des données
## rythmanalyse
## graphes à 2 valeurs
## analyse factorielle
## PH03 - P20

## pour afficher l'aide sur une fonction
## ? nomfonction
## exemple ? setwd


###############################################
# 1. Préparation et installation des packages #
###############################################

# ajoutez vos données au fichier contenant tous les enregistrements
# du type 2020.05.12.ALL.GENRE.csv, disponible sur le github
# https://github.com/GamesRythmAnalysis/analyses-et-donnees/tree/master/donnees
# dans ce fichier, ajoutez vos propres données
# en vérifiant bien que les colonnes correspondent !
# en "nettoyant" le nom du jeu, pour qu'il corresponde bien aux enregistrements existants s'il y en a
# pensez à ajouter le genre du jeu, en respectant les catégories déjà existantes, si vous souhaitez faire une analyse par genre

rm(list=ls()) # on vide la mémoire de RStudio

#choix du dossier de travail où se trouve le fichier de données type 2020.05.12.ALL.GENRE.csv
setwd("~/Documents/Projets R/Rythmanalyse") 

# Installation (si besoin) et chargement des packages requis
# au premier lancement, installer les packages
# avec la commande install.packages("nomdupackage")
# ou enlever le # sur la ligne suivante
# install.packages(c("tidyverse","RColorBrewer","FactoMineR","ggrepel"))

library(tidyverse)
library (RColorBrewer)
library (FactoMineR)
library (ggrepel)

###############################
# 2. Récupération des données #
###############################

d <- read.csv ("2020.06.04.ALL.GENRE.csv")

# la commande names (d) affiche l'ensemble des variables
names (d)

# choisissez ensuite les variables que vous souhaitez conserver
# dans l'exemple suivant, on travaille sur des données clavier et souris

d$date <- as.Date(d$date, "%Y.%m.%d") # on ne va conserver que les enregistrements pad dans le nouveau format

d.mini <- d %>%
  filter (P.DuM > 6 & P.NbAppuis > 20) %>% # on ne retient que les enregistrements avant + 6 min et 20 appuis clavier
  select (- contains ("K."), - X) %>% # ET on enlève les données Pad (P.), la 1ère colonne X
  select (- contains ("M.")) %>% # ET on enlève les données Pad (P.), la 1ère colonne X
  select (- contains ("SD")) %>% # ET toutes les indicateurs avec l'écart-type (SD) %>%
  filter (date > "2016-03-24") %>% # ET on enlève les vieux enregistrements PAD
  select (- date) %>% # ET le nom de fichier, la date
  distinct() # ET les doublons

names (d.mini) # ici on peut voir les variables restantes que l'on va utiliser dans l'analyse

############################
# 3. Graphes sur 2 valeurs #
############################

# 3.1 faire des graphiques sur 2 valeurs sur l'ensemble du corpus
#################################################################

monjeu <- c("FIFA14","FIFA20") # le nom exact du jeu étudié

# la fonction ci-dessous fournit le graphe
# avec pour x : le nombre d'appuis / s au clavier (K.NbAppuisS)
# y : la durée moyenne d'appui (K.TpMoyAppui)
# la couleur des points indique le jeu étudié
# pour changer les valeurs du graph en x et y, il suffit de changer les valeurs appelées dans la formule x = ..., y =...
# rappel pour connaître le nom des variables names (d.mini)
# à vous de tester celles qui vous semblent pertinentes
# le bouton Export au-dessus de la fenêtre "Plots" de Rstudio vous permet de sauver l'image

# la documentation de base de ggplot : https://rstudio.com/wp-content/uploads/2015/03/ggplot2-cheatsheet.pdf

ggplot (data = d.mini, aes (x = P.NbAppuisS, y = P.TpMoyAppui, color = titre %in% monjeu)) +
  geom_point(aes (size = 2))

ggplot (data = d.mini, aes (x = P.VitG, y = P.NbAppuisS, color = genre)) +
  geom_point(aes (size = 2))

# 3.2 faire des graphiques sur 2 valeurs en se limitant à un genre défini à l'avance 
####################################################################################

mongenre <- "sport" # le nom exact du genre dans le jeu de données

d.genre <- d.mini %>%
  filter (genre == mongenre)
# si vous vous rendez-compte que certains jeux posent problème, vous pouvez les ôter du graphe sur le même principe
# d.mini <- d.mini %>%
#  filter (titre != "letitreàenlever")


# mon jeu vs les autres
#ggplot (data = d.genre, aes (x = K.NbAppuisS, y = K.TpMoyAppui, color = titre == monjeu)) +
#  geom_point(aes (size = 2))

names (d.mini)

# mon jeu vs les autres : une couleur par titre
ggplot (data = d.genre, aes (x = P.VitG, y = P.NbAppuisS, color = titre,
                             label = nomfichier)) +
  geom_point() +
  geom_label_repel()


##########################
# 4. Analyse factorielle #
##########################

# l'analyse factorielle est une méthode d'analyse et de classification
# nous utilisons le package factominer pour faire une analyse en composantes principales (ACP)
# la documentation : http://factominer.free.fr/factomethods/analyse-en-composantes-principales.html


# Il va d'abord falloir choisir les variables à intégrer à l'analyse
names (d.mini) # est votre ami
# pour une analyse factorielle, on ne traite que des variables quantitatives
# on ne peut pas retenir le nom de la touche la plus fréquente ("Z") par ex., sinon à titre d'illustration
# dans le code ci-dessous d.pca retient les variables de base pour une analyse clavier-souris

d.pca <- d.mini %>%
  select (titre, genre,
          P.NbTouchesFreq,
          P.NbAppuisS,
          P.TpMoyEntreAppuis,
          P.TpMoyAppui,
          P.PropFreq,
          P.VitG,
          P.VitD)

# on ne conserve que les valeurs moyennes pour chaque titre
d.pca <- d.pca %>% 
  group_by(titre, genre) %>%
  summarise_all(mean)
d.pca <- column_to_rownames(d.pca, "titre") 

d.pca$genre <- d.pca$genre == "sport"

res.pca <- PCA(d.pca, scale.unit=TRUE, graph=F, quali.sup = 1)

# on visualise les relations entre les variables
# qui expliquera le positionnement des titres sur le graphe
graph.var (res.pca) 

# on génère le graphe qui va positionner les jeux
# en fonction des covariances entre les indicateurs
plot.PCA (res.pca, axes=c(1, 2), choix="ind", select ="dist 40", cex = 1.1,
          habillage = 1)
