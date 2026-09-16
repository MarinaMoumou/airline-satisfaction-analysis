# El Amri Sara / Moumou Marina

# Installation des packages necessaires :

install.packages('pastecs')
install.packages('dplyr')
install.packages('MASS')
install.packages('ggpubr')
install.packages('Hmisc')
install.packages('rstatix')
install.packages('questionr')
install.packages('plyr')
install.packages('pacman')
install.packages('ggpubr')
install.packages('ggplot2')
install.packages(tidyverse)
install.packages('naniar')


# Chargement des librairies et packages nécessaires : 

library(tidyverse)
library(dplyr)
library(MASS)
library(Hmisc)
library(rstatix)
library('questionr')
library("plyr")
library('pastecs')
library(pacman)
library("ggpubr")
library(ggplot2) 
library(naniar)


################## Définition du répertoire de travail et importation des données ################## : 

getwd()
satisfaction_airline <- read.csv ("data/satisfaction_airline.csv",header = TRUE, na.strings = ".",dec = ".")

View(satisfaction_airline) # Visualisation du jeu de données dans RStudio

s <- satisfaction_airline # Copie de l'ensemble des données dans un nouvel objet "s"
view(s)


################## Exploration du jeu de données ################## :

nrow(s) # Afficher du nombre de lignes/observations contenus dans le jeu de données

ncol(s)         # Pour retourner le nombre de colonnes/variables de la base de données

dim(s)          # Renvoie les dimensions du jeu de données (lignes et colonnes)

str(s)          # Renvoie un descriptif détaillée de la structure de la base 


################## Tri des données ################## : 

# Avant le tri de la base de donnees nous avions 23 variables et 129880 observations
# Selection des observations par échantillonnage aléatoire de 5000 observations du jeu de données :

set.seed(123)
s <- s[sample(nrow(s), 5000), ]

names(s)        # Retourne les noms des variables de la base 


# Suppression des variables inutiles :
s[ ,c('Departure.Arrival.time.convenient','Flight.Distance','Food.and.drink','Gate.location','Departure.Delay.in.Minutes','Arrival.Delay.in.Minutes','Seat.comfort','Inflight.wifi.service')] <-list(NULL)


# Traitement des valeurs manquantes :
missings_summary <- s %>% summarise_all(~sum(is.na(.)))
print(missings_summary)


# Gaphique des valeurs manquantes par variable : 
gg_miss_var(s, show_pct = TRUE) # Aucune valeurs manquantes dans la base de données

dim(s)
str(s)

# Aprés le tri de la base de donnees nous avons 5000 observations et 15 variables


                         ################## Analyse descriptive (Univariée) ################## 


# Statistiques descriptives de base : 


# Variables quantitatives 
summary(s)
describe(s)
stat.desc(s)
max(s$Age) - min(s$Age) #indicateur de dispersion, écart maximal observée entre les ages



# Visualisation des distributions :
# Histogramme de la répartition de l'age dans l'échantillon avec 20 classes :

hist(s$Age, breaks=20, col = "skyblue", 
     main = "Répartition des âges dans l'échantillon", xlab = "Âge", 
     ylab = "Effectif") 



# Boites à moustache des scores (boxplot) : 

boxplot(s$Cleanliness, col = c("yellow"),main = "Distribution du score lié à la propreté dans l'avion ", ylab = "Quantiles")
boxplot(s$Ease.of.Online.booking, col = c("green"),main = "Distribution du score lié à la facilité de reservation en ligne ", ylab = "Quantiles")
boxplot(s$Baggage.handling , col = c("purple"),main = "Distribution du score lié à la manutention des bagages ", ylab = "Quantiles")
boxplot(s$Inflight.entertainment, col = c("pink"),main = "Distribution du score lié au divertissement en vol ", ylab = "Quantiles")
boxplot(s$Online.support, col = c("blue"),main = "Distribution du score lié à la qualité du service en ligne ", ylab = "Quantiles")
boxplot(s$On.board.service, col = c("grey"),main = "Distribution du score lié à la qualité du service à bord ", ylab = "Quantiles")
boxplot(s$Leg.room.service, col = c("red"),main = "Distribution du score lié à l'espacement pour les jambes lors du vol", ylab = "Quantiles")
boxplot(s$Checkin.service, col = c("orange"),main = "Distribution du score lié au service d'enregistrement ", ylab = "Quantiles")
boxplot(s$Online.boarding, col = c("skyblue"),main = "Distribution du score lié à la qualité de l'enregistrement en ligne ", ylab = "Quantiles")


# Variables qualitatives
# Utilisation du "tri à plat" et représentation graphique avec un diagramme en barre (barplot) : 

table(s$Gender) #Il y a 2520 femmes et 2480 hommes

tab_satisfaction <- table(s$satisfaction)  
barplot(sort(tab_satisfaction)) 

tab_Cust <- table(s$Customer.Type)  
barplot(sort(tab_Cust), col = "#AED6F1", main = "Répartition par type de client")

tab_Type_Travel <- table(s$Type.of.Travel)  
barplot(sort(tab_Type_Travel)) 

tab_class <- table(s$Class)  
barplot(sort(tab_class), 
        col = "#d7bde2", 
        main = "Répartition par classe")


# On peut aussi faire des diagrammes de Cleveland avec les variables qualitatives (dotchart) : 

dotchart(as.numeric(tab_Type_Travel), labels = names(tab_Type_Travel), main = "Dotchart: Type de voyage")
dotchart(as.numeric(tab_satisfaction), labels = names(tab_satisfaction), main = "Dotchart: satisfaction")
dotchart(as.numeric(tab_Cust), labels = names(tab_Cust), main = "Dotchart: Type de client")
dotchart(as.numeric(tab_class), labels = names(tab_class), main = "Dotchart: Type de voyage")


                        ################## Analyses bivariées ################## 


# Croisement de deux variables quantitatives


# Transformation de la variable 'satisfaction' en variable binaire 'satisfaction binaire' :
s <- s %>%
  mutate(satisfaction_binaire = if_else(satisfaction == "satisfied", 1, 0))


# Analyser les relations entre deux variables quantitatives en testant les corrélations linéaires (Pearson) :

correlation_pearson = cor(s$satisfaction_binaire, s$Inflight.entertainment, method = "pearson") 
round(correlation_pearson,2) 
correlation_pearson
# Un coefficient de 0.52 indique une corrélation modérée. Le système de divertissement à bord est lié de facon non aléatoire à la satisfaction globale, mesurée de façon binaire.
# Le signe positif indique que lorsque le score lié à la qualité du divertissement en vol augmente, plus les passagers sont satisfaits.


correlation_pearson = cor(s$satisfaction_binaire, s$On.board.service, method = "pearson") 
round(correlation_pearson,2) 
correlation_pearson
# Une valeur de 0.35 est modérée. Cela suggère que plus le score lié à la qualité du service à bord augmente plus les passagers sont satisfaits.
# Un coefficient positif signifie que lorsque la qualité du service à bord s'améliore, la satisfaction globale augmente également.


correlation_pearson = cor(s$satisfaction_binaire, s$Checkin.service, method = "pearson") 
round(correlation_pearson,2) 
correlation_pearson
# Le signe positif indique que lorsque le score associée à la qualité d'enregistrement augmente, plus les passagers sont satisfaits.
# Avec un coefficient autour de 0.29, on parle d'une corrélation plutôt modérée


correlation_pearson = cor(s$satisfaction_binaire, s$Online.support, method = "pearson") 
round(correlation_pearson,2) 
correlation_pearson
# Le coefficient de corrélation d'environ 0.39 indique une relation linéaire positive entre les variables.
# Plus le score lié à la qualité du service en ligne augmente plus les clients sont satisfaits.


correlation_pearson = cor(s$satisfaction_binaire, s$Leg.room.service, method = "pearson") 
round(correlation_pearson,2) 
correlation_pearson
# Le coefficient de corrélation obtenu est d'environ 0.32, indique une relation linéaire positive entre les 2 variables.
# Plus le score lié à la qualité de l'espacement pour les jambes lors du vol augmente pus les passagers sont satisfaits.


correlation_pearson = cor(s$satisfaction_binaire, s$Cleanliness, method = "pearson") 
round(correlation_pearson,2) 
correlation_pearson
# Le coefficient obtenu est d'environ 0.27, indique une relation linéaire positive entre les 2 variables.
# Plus le score lié à la propreté est élevé plus les passagers tendent à etre satisfait.


correlation_pearson = cor(s$satisfaction_binaire, s$Ease.of.Online.booking, method = "pearson") 
round(correlation_pearson,2) 
correlation_pearson
# Le coefficient obtenu est d'environ 0.43 indique une relation linéaire positive entre les 2 variables.
# Plus le score lié à la facilité de reservation en ligne est élevé, plus les passagers sont satisfaits.


correlation_pearson = cor(s$satisfaction_binaire, s$Baggage.handling, method = "pearson") 
round(correlation_pearson,2) 
correlation_pearson
# Le coefficient obtenu est d'environ 0.28 indique une relation linéaire positive entre les 2 variables.
# Plus le score lié à la qualité du système de manutention des baggages est élevé, plus les passagers sont satisfaits.


correlation_pearson = cor(s$satisfaction_binaire, s$Online.boarding, method = "pearson") 
round(correlation_pearson,2) 
correlation_pearson
#  Le coefficient obtenu est d'environ 0.33 ndique une relation linéaire positive entre les 2 variables.
# Plus le score lié à la qualité de l'enregistrement en ligne est élevé, plus les passagers sont satisfaits.


# Matrice de correlation 

# Installation package "ggcorplot" :
install.packages("ggcorrplot")
library(ggcorrplot)

satisfaction_num <- s[sapply(s, is.numeric)]
cor_matrix <- cor(satisfaction_num, use = "pairwise.complete.obs", method = "pearson")


# Sélection des variables numériques pour la matrice de corrélation :
vars_numeriques <- s[, c("Age", "Inflight.entertainment", "Online.support", "Ease.of.Online.booking", 
                         "On.board.service", "Leg.room.service", "Baggage.handling", 
                         "Checkin.service", "Cleanliness", "Online.boarding", "satisfaction_binaire")]

# Matrice de corrélation
cor_matrix <- cor(vars_numeriques, use = "complete.obs", method = "pearson")

# Affichage de la matrice de corrélation avec ggcorrplot
ggcorrplot(cor_matrix,
           method = "square",       # carré ou cercle
           type = "lower",          # triangle inférieur
           lab = TRUE,              # afficher les coefficients
           lab_size = 3,
           colors = c("blue", "white", "red"),
           title = "Matrice de corrélation des variables explicatives",
           ggtheme = ggplot2::theme_minimal())



# Représentations graphiques des liens entres les variables quantitatives (boites à moustaches)

# Remarque : Comme notre variable mesurant la satisfaction est binaire et que les scores vont tous de 1 à 5 cela rend difficile la visualisation de la relation linéaire entre nos variables avec un graphique type nuage de point
# C'est pourquoi nous allons utiliser des graphiques type boxplot :

boxplot(Inflight.entertainment ~ satisfaction_binaire, data = s,
        col = c("purple", "pink"),
        main = "Distribution du divertissement en vol selon la satisfaction",
        xlab = "Satisfaction Binaire", 
        ylab = "Divertissement en vol")

boxplot(On.board.service ~ satisfaction_binaire, data = s,
        col = c("pink", "yellow"),
        main = "Distribution du score lié au service à bord de l'avion selon la satisfaction",
        xlab = "Satisfaction Binaire", 
        ylab = "Score lié au service à bord de l'avion")

boxplot(Checkin.service ~ satisfaction_binaire, data = s,
        col = c("blue", "purple"),
        main = "Distribution du score lié au service d'enregistrement selon la satisfaction",
        xlab = "Satisfaction Binaire", 
        ylab = "Score lié au service d'enregistrement")

boxplot(Online.support ~ satisfaction_binaire, data = s,
        col = c("red", "orange"),
        main = "Distribution du score lié à la qualité du service en ligne selon la satisfaction",
        xlab = "Satisfaction Binaire", 
        ylab = "Score lié à la qualité du service en ligne")

boxplot(Leg.room.service ~ satisfaction_binaire, data = s,
        col = c("pink", "lightgreen"),
        main = "Distribution du score lié à la qualité de l'espacement des jambes lors du vol selon la satisfaction",
        xlab = "Satisfaction Binaire", 
        ylab = "Score lié à la qualité de l'espacement pour les jambes lors du vol")

boxplot(Cleanliness ~ satisfaction_binaire, data = s,
        col = c("lightblue", "yellow"),
        main = "Distribution du score lié à la propreté à bord de l'avion selon la satisfaction",
        xlab = "Satisfaction Binaire", 
        ylab = "Score lié à la propreté dans l'avion")

boxplot(Ease.of.Online.booking ~ satisfaction_binaire, data = s,
        col = c("blue", "lightgreen"),
        main = "Distribution du score lié à la facilité de reservation en ligne selon la satisfaction",
        xlab = "Satisfaction Binaire", 
        ylab = "Score lié à la facilité de reservation en ligne")

boxplot(Baggage.handling ~ satisfaction_binaire, data = s,
        col = c("lightblue", "orange"),
        main = "Distribution du score lié au système de manutention des bagages selon la satisfaction",
        xlab = "Satisfaction Binaire", 
        ylab = "score lié à la manutention des bagages")

boxplot(Online.boarding ~ satisfaction_binaire, data = s,
        col = c("red", "blue"),
        main = "Distribution du score lié à la qualité de l'enregistrement en ligne selon la satisfaction",
        xlab = "Satisfaction Binaire", 
        ylab = "Score lié à la qualité de l'enregistrement en ligne")


# Croisement de deux variables qualitatives 

table(s$satisfaction, s$Gender) #Tableau croisé pour croiser 2 variables qualitatives

tab_sat <- table(s$satisfaction, s$Gender)                           
cprop(tab_sat) #Calcul des pourcentages en colonnes

chisq.test(tab_sat)
# X-squared = 281.67, p-value < 2.2e-16
# Avec une p-valeur strictement inférieur au seuil de 5%, on rejette l'hypothèse d'indépendance des lignes et des colonnes du tableau et les 2 variables sont dépendantes.

chisq.residuals(tab_sat)
# Pour les femmes : 
# dissatisfied : le résidu est de -8.71 (inférieur à -2), ce qui indique une sous-représentation.il y a significativement moins de femmes insatisfaites que ce qui serait attendu sous l'hypothèse d'indépendance.
# satisfied : le résidu est de 8.02 (supérieur à 2), ce qui signale une sur-représentation. Il y a significativement plus de femmes satisfaites que prévu.

# Pour les hommes :
# dissatisfied : le résidu est de 8.78 (supérieur à 2), indiquant une sur-représentation des hommes insatisfaits – c'est-à-dire qu'il y en a significativement plus que prévu.
# satisfied : le résidu est de -8.09 (inférieur à -2), ce qui montre une sous-représentation des hommes satisfaits – c'est-à-dire qu'il y en a significativement moins que prévu.

mosaicplot(tab_sat, las = 3, shade = TRUE) #graph type mosaicplot

table(s$Customer.Type, s$satisfaction) #Tableau croisé pour croiser 2 variables qualitatives

tab_Customerg <- table(s$Customer.Type, s$satisfaction)                           
lprop(tab_Customerg) 

chisq.test(tab_Customerg)

# X-squared = 434.38, p-value < 2.2e-16
# Avec une p-valeur strictement inférieur au seuil de 5%, on rejette l'hypothèse d'indépendance des lignes et des colonnes du tableau et les 2 variables sont dépendantes.

chisq.residuals(tab_Customerg)
# Pour les clients "disloyal Customer" :
# Dissatisfied : 13.89
# Sur-représentation très marquée. Cela signifie que, parmi les clients disloyaux, il y a beaucoup plus d'insatisfaits que prévu.

# Satisfied : -12.80
# Sous-représentation très marquée. Cela signifie qu'il y a beaucoup moins de clients disloyaux satisfaits que ce que l'on attendrait sous l'hypothèse d'indépendance.

# Pour les clients "Loyal Customer" :

# Dissatisfied : -6.54
# Sous-représentation significative. Il y a significativement moins de clients fidèles insatisfaits que prévu.

# Satisfied : 6.03
# Sur-représentation significative. Il y a significativement plus de clients fidèles satisfaits que prévu.

mosaicplot(tab_Customerg, las = 3, shade = TRUE)

table(s$Class, s$Gender) #Tableau croisé pour croiser 2 variables qualitatives

tab_Class <- table(s$Class, s$Gender)                           
cprop(tab_Class) 

chisq.test(tab_Class)
# X-squared = 0.40545, df = 2, p-value = 0.8165
#  Avec une p-valeur strictement inférieur au seuil de 5%, on rejette l'hypothèse d'indépendance des lignes et des colonnes du tableau et les 2 variables sont dépendantes.

chisq.residuals(tab_Class)
#Les valeurs faibles de ces résidus suggèrent qu'il n'y a pas d'association significative entre la classe de voyage et le genre dans ce tableau"

mosaicplot(tab_Class, las = 3, shade = TRUE)


# Pour appliquer le khi2 pour le reste des variables, au préalable une transformation des variables quantitatives en variables qualitatives a été éffectuée 

s$Inflight.entertainment_cat <- cut(s$Inflight.entertainment,
                                               breaks = c(0, 2, 4, 6),
                                               include.lowest = TRUE,
                                               right = FALSE,
                                               labels = c("Faible", "Moyen", "Elevé"))

s$Online.support_cat <- cut(s$Online.support,
                                               breaks = c(1, 2, 4, 6),
                                               include.lowest = TRUE,
                                               right = FALSE,
                                               labels = c("Faible", "Moyen", "Elevé"))

s$Ease.of.Online.booking_cat <- cut(s$Ease.of.Online.booking,
                                       breaks = c(1, 2, 4, 6),
                                       include.lowest = TRUE,
                                       right = FALSE,
                                       labels = c("Faible", "Moyen", "Elevé"))

s$On.board.service_cat <- cut(s$On.board.service,
                                               breaks = c(1, 2, 4, 6),
                                               include.lowest = TRUE,
                                               right = FALSE,
                                               labels = c("Faible", "Moyen", "Elevé"))

s$Leg.room.service_cat <- cut(s$Leg.room.service,
                                         breaks = c(0, 2, 4, 6),
                                         include.lowest = TRUE,
                                         right = FALSE,
                                         labels = c("Faible", "Moyen", "Elevé"))

s$Baggage.handling_cat <- cut(s$Baggage.handling,
                                         breaks = c(1, 2, 4, 6),
                                         include.lowest = TRUE,
                                         right = FALSE,
                                         labels = c("Faible", "Moyen", "Elevé"))

s$Checkin.service_cat <- cut(s$Checkin.service,
                                         breaks = c(1, 2, 4, 6),
                                         include.lowest = TRUE,
                                         right = FALSE,
                                         labels = c("Faible", "Moyen", "Elevé"))

s$Cleanliness_cat <- cut(s$Cleanliness,
                                         breaks = c(1, 2, 4, 6),
                                         include.lowest = TRUE,
                                         right = FALSE,
                                         labels = c("Faible", "Moyen", "Elevé"))

s$Online.boarding_cat <- cut(s$Online.boarding,
                                         breaks = c(1, 2, 4, 6),
                                         include.lowest = TRUE,
                                         right = FALSE,
                                         labels = c("Faible", "Moyen", "Elevé"))

s$Age_cat <- cut(s$Age,
                  breaks = c(7, 18, 35, 60, Inf),
                  labels = c("Enfant", "Adulte", "Mature", "Senior"),
                  right = FALSE,
                  include.lowest = TRUE)


#Test du khi-2 : 

result=chisq.test(s$satisfaction,s$Inflight.entertainment_cat)
result

# statistique de test X-squared = 1935 et P-valeur (< 2.2e-16)
# On rejette l'hypothèse d'indépendance car la p-valeur est strictement inférieur au seuil de risque de 5%
#Il existe une relation significative entre la satisfaction des passagers et le niveau de divertissement en vol au seuil de 5%. Les 2 variables sont dépendantes.

result=chisq.test(s$satisfaction,s$Online.support_cat)
result

# statistique de test X-squared = 906,38 et P-valeur (< 2.2e-16)
# On rejette l'hypothèse d'indépendance car la p-valeur est strictement inférieur au seuil de risque de 5%
#Il existe une relation significative entre la satisfaction des passagers et la qualité du service en ligne au seuil de 5%. Les 2 variables sont dépendantes.

result=chisq.test(s$satisfaction,s$Ease.of.Online.booking_cat)
result

# statistique de test X-squared = 1039,6 et P-valeur (< 2.2e-16)
# On rejette l'hypothèse d'indépendance car la p-valeur est inférieur au seuil de risque de 5%
#Il existe une relation significative entre la satisfaction des passagers et la facilité de réservation en ligne au seuil de 5%. Les 2 variables sont dépendantes.

result=chisq.test(s$satisfaction,s$On.board.service_cat)
result

# statistique de test X-squared = 647,75 et P-valeur (< 2.2e-16)
# On rejette l'hypothèse d'indépendance car la p-valeur est inférieur au seuil de risque de 5%
#Il existe une relation significative entre la satisfaction des passagers et la qualité du service à bord au seuil de 5%. Les 2 variables sont dépendantes.

result=chisq.test(s$satisfaction,s$Leg.room.service_cat)
result

# statistique de test X-squared = 633,3 et P-valeur (< 2.2e-16)
# On rejette l'hypothèse d'indépendance car la p-valeur est inférieur au seuil de risque de 5%
#Il existe une relation significative entre la satisfaction des passagers et l'espacement pour les jambes durant le vol au seuil de 5%. Les 2 variables sont dépendantes.

result=chisq.test(s$satisfaction,s$Baggage.handling_cat)
result

# statistique de test X-squared = 440,58 et P-valeur (< 2.2e-16)
# On rejette l'hypothèse d'indépendance car la p-valeur est inférieur au seuil de risque de 5%
#Il existe une relation significative entre la satisfaction des passagers et le système de manutention des bagages au seuil de 5%. Les 2 variables sont dépendantes.

result=chisq.test(s$satisfaction,s$Checkin.service_cat)
result 

# statistique de test X-squared = 311,37 et P-valeur (< 2.2e-16)
# On rejette l'hypothèse d'indépendance car la p-valeur est inférieur au seuil de risque de 5%
#Il existe une relation significative entre la satisfaction des passagers et la qualité du service d'enregistrement au seuil de 5%. Les 2 variables sont dépendantes.

result=chisq.test(s$satisfaction,s$Cleanliness_cat)
result

# statistique de test X-squared = 415,18 et P-valeur (< 2.2e-16)
# On rejette l'hypothèse d'indépendance car la p-valeur est inférieur au seuil de risque de 5%
#Il existe une relation significative entre la satisfaction des passagers et le niveau de propreté de l'avion au seuil de 5%. Les 2 variables sont dépendantes.

result=chisq.test(s$satisfaction,s$Online.boarding_cat)
result

# statistique de test X-squared = 463,52 et P-valeur (< 2.2e-16)
# On rejette l'hypothèse d'indépendance car la p-valeur est inférieur au seuil de risque de 5%
#Il existe une relation significative entre la satisfaction des passagers et la qualité de l'enregistrement en ligne au seuil de 5%. Les 2 variables sont dépendantes.

result=chisq.test(s$satisfaction,s$Age_cat)
result

# statistique de test X-squared = 153,15 et P-valeur (< 2.2e-16)
# On rejette l'hypothèse d'indépendance car la p-valeur est inférieur au seuil de risque de 5%
#Il existe une relation significative entre la satisfaction des passagers et leur tranche d'age au seuil de 5%. Les 2 variables sont dépendantes.

result=chisq.test(s$satisfaction,s$Type.of.Travel)
result

# statistique de test X-squared = 57.685 et P-valeur (= 3.076e-14)
# On rejette l'hypothèse d'indépendance car la p-valeur est inférieur au seuil de risque de 5%
#Il existe une relation significative entre la satisfaction des passagers et le type de voyage au seuil de 5%. Les 2 variables sont dépendantes.

result=chisq.test(s$satisfaction,s$Class)
result

# statistique de test X-squared = 510,37 et P-valeur (< 2.2e-16)
# On rejette l'hypothèse d'indépendance car la p-valeur est inférieur au seuil de risque de 5%
#Il existe une relation significative entre la satisfaction des passagers et la classe choisi au seuil de 5%. Les 2 variables sont dépendantes.

result=chisq.test(s$satisfaction,s$Gender)
result

# statistique de test X-squared = 281.67 et P-valeur (< 2.2e-16)
# On rejette l'hypothèse d'indépendance car la p-valeur est inférieur au seuil de risque de 5%
#Il existe une relation significative entre la satisfaction des passagers et leur genre au seuil de 5%. Les 2 variables sont dépendantes.

# Croisement entre la satisfaction et la manutention des bagages :
table(s$satisfaction, s$Baggage.handling_cat) #Tableau croisé pour croiser 2 variables qualitatives

tab_bagage <- table(s$Baggage.handling_cat, s$satisfaction)  
chisq.residuals(tab_bagage)

mosaicplot(tab_bagage, las = 3, shade = TRUE)



##################### croisement d'une variable quantitative et d'une variable qualitative : #####################


# Créatiion de deux variables : score physique et digital pour mesurer la satisfaction des passagers

s$Score_Digital <- rowMeans(s[, c("Online.support", "Ease.of.Online.booking", "Online.boarding")], na.rm = TRUE)
#score regroupant les évaluations liées aux services en ligne et digitaux offerts par la compagnie aérienne. 

s$Score_Physique <- rowMeans(s[, c("On.board.service", "Leg.room.service", "Baggage.handling", "Checkin.service", "Cleanliness","Inflight.entertainment")], na.rm = TRUE)
#score qui permet de résumer l'expérience à bord ou physique, c'est-à-dire l'ensemble des services et conditions matérielles qui impactent le confort et la satisfaction pendant le vol.

hist(s$Score_Digital, main = "Histogramme du Score Digital", xlab = "Score Digital")

boxplot(s$Score_Digital, col = c("yellow"),main = "Distribution du score digital ", ylab = "Quantiles")
boxplot(s$Score_Physique, col = c("green"),main = "Distribution du score physique ", ylab = "Quantiles")


# Représentation graphique :
# Graphique de type "boite à moustache" : 

boxplot(s$Score_Digital ~ s$Class) 
boxplot(s$Score_Physique ~ s$Class) 

boxplot(s$Score_Digital ~ s$Gender) 
boxplot(s$Score_Physique ~ s$Gender) 

boxplot(s$Score_Digital ~ s$Customer.Type) 
boxplot(s$Score_Physique ~ s$Customer.Type) 

boxplot(s$Score_Digital ~ s$Type.of.Travel) 
boxplot(s$Score_Physique ~ s$Type.of.Travel) 

boxplot(s$Score_Digital ~ s$satisfaction) 

boxplot(s$Score_Digital ~ s$satisfaction,
        col = c("lightsalmon", "lightgreen"),
        main = "Score Digital selon la satisfaction",
        xlab = "Satisfaction",
        ylab = "Score Digital")

boxplot(s$Score_Physique ~ s$satisfaction,
        col = c("lightyellow", "lightcoral"),
        main = "Score Physique selon la satisfaction",
        xlab = "Satisfaction",
        ylab = "Score Physique")

boxplot(s$Age ~ s$Gender) 
boxplot(s$Age ~ s$satisfaction)

# Calcul d'indicateurs :

# Création de sous population stockées dans 3 tableaux de données par classe de voyage : Business, Eco, et Eco Plus :
satisfaction_Business <- filter(s, Class == "Business")                                       
satisfaction_Eco <- filter(s, Class == "Eco") 
satisfaction_Eco_Plus <- filter(s, Class == "Eco Plus") 

# Calcul de la moyenne du Score Digital pour chaque classe :
mean(satisfaction_Business$Score_Digital)  
mean(satisfaction_Eco$Score_Digital)  
mean(satisfaction_Eco_Plus$Score_Digital)  

# Calcul de la moyenne du Score Physique pour chaque classe :
mean(satisfaction_Business$Score_Physique)  
mean(satisfaction_Eco$Score_Physique)  
mean(satisfaction_Eco_Plus$Score_Physique) 

# On peut aussi utiliser la fonction "tapply" : 
tapply(s$Score_Digital, s$Class, mean)
tapply(s$Score_Physique, s$Class, mean)

# Le test du t de Student :

# On regroupe la variable 'Class' en deux niveaux puis on effectue un t.test :
t.test(Score_Digital ~ Class,
       data = s[s$Class %in% c("Business", "Eco"), ],
       na.rm = TRUE)

# Le résultat du test est significatif, avec un p_valeur inférieure à 2.2e-16 et donc strictement au seuil de 5%, et on peut rejeter l’hypothèse nulle d’égalité des moyennes des deux groupes
# Le t-statistic est de 9.2402 avec environ 4435,4 degrés de liberté, ce qui indique une différence notable entre les deux groupes.
# Les passagers de la classe Business ont, en moyenne, un Score_Digital significativement plus élevé que ceux de la classe Eco

t.test(Score_Physique ~ Class,
       data = s[s$Class %in% c("Business", "Eco"), ],
       na.rm = TRUE)

# Le résultat du test est significatif, avec un p_valeur inférieure à 2.2e-16 et donc strictement au seuil de 5%, et on peut rejeter l’hypothèse nulle d’égalité des moyennes des deux groupes
# es résultats indiquent que les passagers en Business donnent, en moyenne, une évaluation du Score_Physique significativement supérieure à celle des passagers en Eco
# Les résultats indiquent que les passagers en Business donnent, en moyenne, une évaluation du Score_Physique significativement supérieure à celle des passagers en Eco

# Histogramme des trois répartitions suivant le 'Score_Physique' et le 'Score_Digital' :

hist(satisfaction_Business$Score_Digital) #tester l'hypothèse de normalité des variables
hist(satisfaction_Eco$Score_Digital)
hist(satisfaction_Eco_Plus$Score_Digital)
# Les groupes ne se rapproche pas d'une distribution normale 

hist(satisfaction_Business$Score_Physique) #tester l'hypothèse de normalité des variables
hist(satisfaction_Eco$Score_Physique)
hist(satisfaction_Eco_Plus$Score_Physique)

# Les groupes ne se rapproche pas d'une distribution normale 

##################### Test de Shapiro-Wilk  : #####################

shapiro.test(satisfaction_Business$Score_Digital)#On peut tester la normalité à l'aide du test de shapiro wilk et de la fonction shapiro.test
# W = 0.92474, p-value < 2.2e-16
# La p-valeur est très faible et inférieur au seuil de 5%, on rejette l'hypothèse nulle de normalité.
# Les résultats du test indiquent que les scores digitaux pour la classe Business ne sont pas distribués normalement.

shapiro.test(satisfaction_Eco$Score_Digital)
# W = 0.9294, p-value < 2.2e-16
# La p-valeur est très faible et inférieur au seuil de 5%, on rejette l'hypothèse nulle de normalité.
# Les résultats indiquent que les scores digitaux pour la classe Eco ne suivent pas une distribution normale

shapiro.test(satisfaction_Eco_Plus$Score_Digital)
# W = 0.93107, p-value = 4.795e-12
# La p-valeur est très faible et inférieur au seuil de 5%, on rejette l'hypothèse nulle de normalité.
# Les résultats indiquent que les scores digitaux pour la classe Eco Plus ne suivent pas une distribution normale


shapiro.test(satisfaction_Business$Score_Physique)
# W = 0.94658, p-value < 2.2e-16
# La p-valeur est très faible et inférieur au seuil de 5%, on rejette l'hypothèse nulle de normalité.
# Les résultats indiquent que les scores physiques pour la classe Business ne suivent pas une distribution normale

shapiro.test(satisfaction_Eco$Score_Physique)
# W = 0.98636, p-value = 7.043e-14
# La p-valeur est très faible et inférieur au seuil de 5%, on rejette l'hypothèse nulle de normalité.
# Les résultats indiquent que les scores physiques pour la classe Eco ne suivent pas une distribution normale

shapiro.test(satisfaction_Eco_Plus$Score_Physique)
# W = 0.98718, p-value = 0.002366
# La p-valeur est très faible et inférieur au seuil de 5%, on rejette l'hypothèse nulle de normalité.
# Les résultats indiquent que les scores physiques pour la classe Eco plus ne suivent pas une distribution normale

##################### Test des rangs de Wilcoxon  : #####################

# On fait appel à un test non-paramétrique, ne faisant pas d’hypothèses sur les lois de distribution des variables testées :

wilcox.test(Score_Digital ~ Class, 
            data = s[s$Class %in% c("Business", "Eco"), ],
            na.rm = TRUE)

# W = 3052586, p-value < 2.2e-16
# La p-valeur est strictement inférieur au seuil de 5%, on rejette donc l'hypothèse nulle d'égalité des distributions entre les deux groupes.
# Les résultats montrent que la distribution du Score_Digital est significativement différente entre la classe Business et la classe Eco

wilcox.test(Score_Physique ~ Class, 
            data = s[s$Class %in% c("Business", "Eco"), ],
            na.rm = TRUE)

# W = 3488550, p-value < 2.2e-16
# La p-valeur est strictement inférieur au seuil de 5%, on rejette donc l'hypothèse nulle d'égalité des distributions entre les deux groupes.
# Les résultats indiquent qu'il existe une différence statistiquement significative entre les distributions du Score_Physique des passagers en classe Business et ceux en classe Eco











