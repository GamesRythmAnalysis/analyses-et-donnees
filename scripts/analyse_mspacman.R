rm(list=ls()) # on vide la mémoire de RStudio
setwd("~/Documents/Projets R/Rythmanalyse") #choix du dossier où se trouve le script Rythma.4.9.R

# Installation (si besoin) et chargement des packages requis
# Pour installer les packages : install.packages ("tidyverse") ...
library(tidyverse)
library (RColorBrewer)
library (FactoMineR)
library (ggrepel)

#library (knitr)


### visualiser et parcourir les sorties graphiques
## élaborer des questions à poser aux données

# le pattern de touches
# la touche la plus fréquente
# la variabilité des styles individuels
# la comparaison avec les autres jeux de l'arcade

## 0. Préparer les données

# on récupère l'ensemble des données
d <- read_csv("2020.04.07.ALL.GENRE.csv")
names (d[,c(2:ncol(d))])

# données clavier
d.K <- d %>%
  filter (K.DuM > 5, K.NbAppuis > 30) %>%
  select (- contains ("M."), - contains ("P."), - X1) %>% 
  distinct()

# données MsPacMan
MsPacMan <- d.K %>%
  filter (titre == "MsPacMan")

# nombre d'enregistrements
n_distinct (MsPacMan)

# nombre de joueurs 
n_distinct(MsPacMan$usr)


## 1. On parcourt variable par variable

names (MsPacMan)
# Nombre touches utilisées : K.NbTouchesFreq
table (MsPacMan$K.NbTouchesFreq)
ggplot (data = MsPacMan, aes (x = K.NbTouchesFreq)) +
  geom_bar()

outlier <- filter (MsPacMan, K.NbTouchesFreq > 10) # oui, c'est du Dota ou LOL !
MsPacMan <- filter (MsPacMan, K.NbTouchesFreq < 10)

ggplot (data = MsPacMan, aes (x = K.NbTouchesFreq)) +
  geom_bar ()

names (MsPacMan)
# Nombre d'appuis par seconde : K.NbAppuisS
ggplot (data = MsPacMan, aes (x = K.NbAppuisS)) +
  geom_dotplot()

## Q : est-ce qu'il y a lien entre nb appuis / s et skill ou score ? 

names (MsPacMan)
# Entre appui : K.TpMoyEntreAppuis
ggplot (data = MsPacMan, aes (x = K.TpMoyEntreAppuis)) +
  geom_dotplot()

ggplot (data = MsPacMan, aes (x = K.TpSDEntreAppuis)) +
  geom_dotplot()

outliers <- filter (MsPacMan, K.TpSDEntreAppuis > 4) # filtrer ou non ? 
MsPacMan <- filter (MsPacMan, K.TpSDEntreAppuis < 4)

names (MsPacMan)
# Durée moyenne d'appui : K.TpMoyAppui
ggplot (data = MsPacMan, aes (x = K.TpMoyAppui)) +
  geom_dotplot()

## Q : style individuel par appuis plus longs (dans mon cas, quand je chasse un fantôme)
ggplot (data = MsPacMan, aes (x = K.TpMoyAppui)) +
  geom_density(bw = 0.01)

names (MsPacMan)
# Proportion de temps appuyé : K.PropAct
ggplot (data = MsPacMan, aes (x = K.PropAct)) +
  geom_dotplot()

names (MsPacMan)
# Touche la plus fréquente : K.ToucheFreq
table (MsPacMan$K.ToucheFreq)

## Q : extraordinaire ! une répartition à 50/50 droitier, gaucher ? 
## Y a-t-il une signature individuelle ? 

ggplot (MsPacMan, aes (x= K.PropFreq)) +
  geom_dotplot()

## Il y a des directions très faiblement privilégiées, 
## puisque la touche la plus fréquente est entre 25 et 32% des appuis

outliers <- filter (MsPacMan, K.PropFreq < 25) # pas de raison de les éliminer

## 2. Croiser les données 

# Nombre appuis/s // durée moyenne appui

ggplot (data = MsPacMan, aes (x = K.NbAppuisS, y = K.TpMoyAppui)) +
  geom_point() +
  geom_label_repel(aes(label = usr),
                   box.padding = 0.35, point.padding = 0.8, max.iter = 10,
                   alpha = 0.75,
                   segment.color = 'grey50')

# rien ne se dégage : il faut comparer

# mais soyons fous : et si on passe à l'ACP
names (MsPacMan)
dcomp <- MsPacMan %>%
  select (nomfichier,
          K.NbTouchesFreq,
          K.NbAppuisS,
          K.TpMoyEntreAppuis,
          K.TpSDEntreAppuis,
          K.TpMoyAppui,
          K.PropAct,
          K.PropFreq
  )

# On prépare les données pour l'ACP en basculant les usr en rownames
dcomp <- as.data.frame(dcomp) # pas de rownames avec dplyr
rownames (dcomp) <- dcomp$nomfichier
res.pca <- PCA(dcomp[,1:ncol(dcomp)], scale.unit=TRUE, ncp=6, graph=F, quali.sup = 1)
res.pca$var
graph.var (res.pca)

## Résultat de l'analyse
plot(res.pca, choix = c("ind"), label =c("ind"), select = "dist 30",
     title = "MsPacMan", cex = 0.8, invisible = "quali")


dcomp <- MsPacMan %>%
  select (nomfichier,
          K.NbTouchesFreq,
          K.NbAppuisS,
          K.TpMoyEntreAppuis,
          K.PropAct
  )

# On prépare les données pour l'ACP en basculant les usr en rownames
dcomp <- as.data.frame(dcomp) # pas de rownames avec dplyr
rownames (dcomp) <- dcomp$nomfichier
res.pca <- PCA(dcomp[,1:ncol(dcomp)], scale.unit=TRUE, ncp=6, graph=F, quali.sup = 1)
res.pca$var
graph.var (res.pca)

## Résultat de l'analyse
plot(res.pca, choix = c("ind"), label =c("ind"), select = "dist 100",
     title = "MsPacMan", cex = 0.8, invisible = "quali")


## 3. Comparer au sein de l'arcade
d.arcade <- d.K %>%
  filter (genre == "arcade")

d.arcadefilter <- d.K %>%
  filter (genre == "arcade") %>%
  filter (! titre %in% c ("Castlevania","CastlevaniaIV","CastlevaniaSN",
                          "Metroid","MetroidII","SuperMetroid",
                          "ZeldaOcarina","ZeldaTP","ZeldaWindWaker",
                          "Transformice","MetalSlug",
                          "Broforce","SuperHexagon","SuperBreakout",
                          "SpaceWar","Touhou14","OutRun",
                          "Oases","MetroidFusion"))

## Nombre d'enregistrements 
n_distinct(d.arcade)
n_distinct(MsPacMan)

## Nombre de jeux distincts enregistrés
n_distinct(d.arcade$titre)

## Nombre de joueurs
n_distinct(d$usr)
n_distinct(MsPacMan$usr)

## Croiser durée et nombre d'appuis
ggplot (d.arcadefilter, aes (K.NbAppuisS, K.TpMoyAppui)) +
  geom_point(aes(color = titre), size = 10, alpha = 0.8) +
  scale_color_brewer(palette = "Set3") +
  ggtitle ("Arcade : fréquence / durée appui")

ggplot (d.arcadefilter, aes (K.NbAppuisS, K.TpMoyAppui)) +
  geom_point(aes(color = titre, shape = (usr %in% c("MT","mt"))), size = 10, alpha = 0.8) +
  scale_color_brewer(palette = "Set3") +
  ggtitle ("Arcade : fréquence / durée appui")


ggplot (d.arcadefilter, aes (K.NbAppuisS, K.TpMoyAppui)) +
  geom_point(aes(color = titre), size = 10, alpha = 0.8) +
  scale_color_brewer(palette = "Set3") +
  ggtitle ("Arcade : fréquence / durée appui") +
  geom_label_repel(aes(K.NbAppuisS, K.TpMoyAppui, label = usr),
                   box.padding = 0.35, point.padding = 0.8, max.iter = 10,
                   alpha = 0.75,
                   segment.color = 'grey50')
  
  
#PCA

names (d.arcade)

dcomp <- d.arcade %>%
  select (titre,
          K.NbTouchesFreq,
          K.NbAppuisS,
          K.TpMoyEntreAppuis,
          K.PropAct
  )
  
# Au besoin, moyenne pour chaque titre
dcomp <- dcomp %>%
  group_by(titre)%>%
  summarise_all(mean)

# on vire les outliers
dcomp <- filter (dcomp, ! titre %in% c ("SuperBreakout", "Touhou14", "SuperHexagon"))

# On prépare les données pour l'ACP en basculant les titres en rownames
dcomp <- as.data.frame(dcomp) # pas de rownames avec dplyr
rownames (dcomp) <- dcomp$titre
res.pca <- PCA(dcomp[,2:ncol(dcomp)], scale.unit=TRUE, ncp=6, graph=F, quali.sup = 1)
res.pca$var
graph.var (res.pca)

## Résultat de l'analyse
plot(res.pca, choix = c("ind"), label =c("ind"), select = "dist 30",
         title = "arcade", cex = 0.8, invisible = "quali")
# legend (x = -6,y = 3.5, "Frappe Souris", cex = 1, bty = "n",text.col = "red")
# legend (x = 1,y = 3.5, "Frappe Clavier", cex = 1, bty = "n",text.col = "red")
# legend (x = 5,y = 1, "Appui long", cex = 1, bty = "n",text.col = "red")
# legend (x = -5,y = -4.2, "Infra-actionnabilité", cex = 1, bty = "n",text.col = "red")

names (d.arcade)
dcomp <- d.arcade %>%
  select (titre,
        K.NbTouchesFreq,
        K.NbAppuisS,
        #K.TpMoyEntreAppuis,
        K.TpSDEntreAppuis,
        K.TpMoyAppui,
        #K.TpSDAppui,
        K.PropAct,
        K.PropFreq
)

# Au besoin, moyenne pour chaque titre
dcomp <- dcomp %>%
  group_by(titre)%>%
  summarise_all(mean)

# on vire les outliers
dcomp <- filter (dcomp, ! titre %in% c ("SuperBreakout", "Touhou14", "SuperHexagon"))

# On prépare les données pour l'ACP en basculant les titres en rownames
dcomp <- as.data.frame(dcomp) # pas de rownames avec dplyr
rownames (dcomp) <- dcomp$titre
res.pca <- PCA(dcomp[,2:ncol(dcomp)], scale.unit=TRUE, ncp=6, graph=F, quali.sup = 1)
res.pca$var
graph.var (res.pca)

## Résultat de l'analyse
plot(res.pca, choix = c("ind"), label =c("ind"), select = "dist 30",
     title = "arcade", cex = 0.8, invisible = "quali")
