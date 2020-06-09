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

d <- read.csv ("2020.06.08.ALL.GENRE.csv")

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




############################
# 5. Questions spécifiques #
############################

journal <- read_csv("FIFAjournaldebord.csv")

# harmoniser les données 
journal$nomfichier <- paste (journal$nomfichier, ".zip", sep ="") # on ajoute .zip au nom fichier
journal$nomfichier <- toupper (journal$nomfichier) # on passe tout en maj
d.mini$nomfichier <- toupper (d.mini$nomfichier)

# on garde les données communes aux 2 jeux de données
d.journal <- d.mini %>%
  filter (nomfichier %in% journal$nomfichier) %>%
  arrange (nomfichier)
journal <- journal %>%
  filter (nomfichier %in% d.journal$nomfichier) %>%
  arrange(nomfichier)

# on fabrique un nouveau jeu avec les données du journal
journal <- journal %>%
  select (mode,equipe_joueur,equipe_adverse,score)
d.journal <- bind_cols(d.journal,journal)
d.journal$mode <- toupper(d.journal$mode)

# analyse
ggplot (data = d.journal, aes (x = P.VitG, y = P.NbAppuisS, color = P.ToucheFreq, 
                               shape = titre, label = mode )) +
  geom_point(aes (size = 2)) +
  geom_label_repel()

# Comparer les temps d’appuis sur la gachette droite entre FIFA14 et FIFA20 pour voir dans lequel on court le plus
names (d.journal)
ggplot (data = d.journal, aes (x = P.ToucheFreq, y = P.TpMoyAppui, fill = titre)) +
  geom_bar(stat="identity") +
  ggtitle("Durée moyenne d'appui sur A, RB, RT : FIFA 14 (rouge) vs FIFA 20 (bleu)")
  
# Etudier la fréquence des mouvements brusques avec le joystick droit pour étudier le style de jeu (bcp de drible ou non)
ggplot (data = d.journal, aes (x = P.VitD, y = P.VitG, color = mode, 
                               shape = titre)) +
  geom_point(aes (size = 2)) +
  ggtitle("Activité des sticks droit / gauche selon le mode de jeu ")

# Comparer le nombre de passes entre les match en ligne et les match contre l’ordinateur pour voir la différence de style de jeu
# Comparer le nombre d’appuis sur B et le nombre de but par match
# quelle touche pour passes ? revenir aux données brutes

# Etudier les phases défensive en regardant les non-appuies sur A (passe) et les appuis sur X (tacle)
# il faut regarder les graphes individuels pour chaque session

# Etudier l’utilisation du joystick gauche sur une mi-temps (en général une mi-temps dure entre 5 et 6 min) pour observer les déplacements vers le but adverses ou vers son but. 
# Possibilité de mettre en corrélation avec les appuies sur A et X.
# découper le fichier avec le bon timecode pour les mi-temps dans la fonction rythmanalyse 
# sur le modèle : ## ex. Rythmanalyse ("2016.03.24.MT.Sylvio.s1.zip", debut = 5, fin =60, graph = FALSE)
# analyser la sortie graphique
