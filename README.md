# Segmentation-de-clients-et-maintenance-industrielle
Analyse multidimensionnelle avancée pour la segmentation de clients et la maintenance industrielle. Réalisation d’une analyse factorielle et de regroupement pour identifier des segments clés, et estimation de la durée de vie des équipements via des modèles de survie et régression de Cox.

# Question 1 

# A- Faite une analyse factorielle des données contenues dans le fichier TestPersonnalite.csv. Expliquez comment et pourquoi vous avez choisi un certain nombre de facteurs, et n’oubliez pas de leur donner une interprétation claire. Construisez des échelles à partir des facteurs trouvés et n’oubliez pas de vérifier leur validité interne.
library(dplyr)
Attaching package: 'dplyr'
The following objects are masked from 'package:stats':
filter, lag
The following objects are masked from 'package:base':
    intersect, setdiff, setequal, union
library(hecmulti)
library(hecmulti)
# Chargement des données
data <- read.csv("/Users/Nausicaa/Desktop/TestPersonnalite.csv") # Retirer les colonnes inutiles (ID)
data_personality <- data[, -1]
#analyse factorielle exploratoire à l’aide de la méthode des composantes
principales
covdata <- cor(data_personality)
decompo <- eigen(covdata)
# Graphe Scree pour visualiser le nombre de facteurs
eboulis(decompo)

# Le critère des valeurs propres de Kaiser (variances des composantes principales supérieures à 1) et les diagramme d'ébouli indiquent 4 facteurs, # qui conjointement expliquent plus de 90% pourcent de la variance totale des items du questionnaire.
nkaiser <- sum(decompo$values > 1)
var_cumu <- with(decompo,
cumsum(values)/sum(values))
# Analyse factorielle exploratoire avec 4 facteurs
varcp_4 <- factocp(x = data_personality,
                nfact = 4)
print(varcp_4, cutoff = 0.5)

# Interprétation des facteurs : affichage des charges factorielles loadings <- varcp_4$loadings
print(loadings)

# Le critere de Kaiser recommande 4 facteurs mais les 4 premiers sont des cas de Heywood
knitr::kable(
ajustement_factanal(data_personality, factors = 1:5), digits = 2)

# Validation interne : calcul de l'alpha de Cronbach
vars2 <- apply(varcp_4$loadings, 2, function(x){ which(abs(x) > 0.5)}, simplify = FALSE)
alphaCronbach <- sapply(vars2, function(index){ hecmulti::alphaC(data_personality[, index])
})
# les scores alpha de Cronbach sont tous satisfaisants (plus grand que 0.6). Donc, lanalyse factorielle ne soufre pas de problème de validité interne alphaCronbach
alphaCronbach

# Création des échelles
ech_F1 <- rowMeans(data_personality[,c("Talkative","Outgoing","Forgiving", "Thorough", "Helpful_unselfish", "Considerate", "Cooperative", "Emotionally_stable",
"Sophisticated_in_arts", "Easily_distracted",
"Moody", "Curious")])
ech_F2 <- rowMeans(data_personality[,c("Thorough","Efficient","Energetic","Helpful_unse lfish", "Cooperative", "Curious")])
ech_F3 <- rowMeans(data_personality[,c("Efficient","Helpful_unselfish","Cooperative", "Curious")])
ech_F4 <- rowMeans(data_personality[,c("Talkative","Outgoing", "Relaxed", "Emotionally_stable", "Sophisticated_in_arts")])
#Générer les scores factoriels
factor_scores <- as.data.frame(cbind(ech_F1, ech_F2, ech_F3, ech_F4))
# Sauvegarder les scores factoriels
write.csv(factor_scores, "/Users/Nausicaa/Desktop/FactorScores.csv", row.names = FALSE)

# B - Faites une analyse de regroupement des données basée sur les échelles trouvées en a). Donnez une interprétation claire aux segments trouvés.

# Chargement des packages nécessaires 
library(cluster) 
library(ggplot2) 

# Charger les scores factoriels obtenus
file_path_scores <- "/Users/Nausicaa/Desktop/FactorScores.csv" factor_scores <- read.csv(file_path_scores)
# Standardisation des scores factoriels
scaled_scores <- scale(factor_scores)
# 1. Déterminer le nombre de clusters
# Calcul de l'inertie intra-cluster pour différents nombres de clusters wss <- numeric(10) # WSS = Within-Cluster Sum of Squares
for (k in 1:10) {
kmeans_result <- kmeans(scaled_scores, centers = k, nstart = 25)
  wss[k] <- kmeans_result$tot.withinss
}
# 2. Appliquer k-means clustering avec un nombre de clusters choisi (par exemple, k=3)
set.seed(123) # Pour reproductibilité
kmeans_result <- kmeans(scaled_scores, centers = 3, nstart = 25)
# Ajouter les clusters aux données
factor_scores$Cluster <- as.factor(kmeans_result$cluster)
# 3. Visualisation des clusters (premières deux dimensions)
ggplot(factor_scores, aes(x = scaled_scores[, 1], y = scaled_scores[, 2], color = Cluster)) +
geom_point(size = 3) +
labs(title = "Visualisation des Clusters (k-means)",
       x = "Dimension 1",
       y = "Dimension 2") +
  theme_minimal()

# Calcul des moyennes des scores pour chaque cluster
cluster_summary <- aggregate(. ~ Cluster, data = factor_scores, FUN = mean) print(cluster_summary)
1 2 3
Cluster   ech_F1   ech_F2   ech_F3   ech_F4
      1 4.355596 4.134777 4.055054 4.555235
      2 5.788308 5.487065 5.563433 6.265672
      3 7.044326 7.035461 7.098404 7.562766
# Sauvegarde des données avec les clusters
write.csv(factor_scores, "/Users/Nausicaa/Desktop/ClusterResults.csv", row.names = FALSE)

# C - Les individus ayant participé à cet exercice font partie des données du devoir #1. La colonne ID permet de les reconnaître. Réutilisez les données du devoir #1 pour décrire de quelles façons est-ce que les différents individus diffèrent d’un segment à l’autre ?

# Charger les bibliothèques
library(dplyr)
library(ggplot2)
# Charger les fichiers
clustered_data <- read.csv("/Users/Nausicaa/Desktop/ClusterResults.csv") # Contient ID, F1-F4 et Cluster
life_times <- read.csv("/Users/Nausicaa/Desktop/LifeTimes.csv") # Contient ID et caractéristiques des individus

# Extraire les indices communs entre les deux fichiers
common_indices <- min(nrow(clustered_data), nrow(life_times))

# Aligner les deux fichiers en utilisant les premières lignes communes
aligned_clustered_data <- clustered_data[1:common_indices, ] aligned_life_times <- life_times[1:common_indices, ]

# Ajouter les clusters
aligned_life_times$Cluster <- aligned_clustered_data$Cluster
head(aligned_life_times)


# Analyser les différences par Cluster
library(dplyr)
summary_by_cluster <- aligned_life_times %>% group_by(Cluster) %>%
summarise(
Time_Mean = mean(Time, na.rm = TRUE),
Censored_Prop = mean(Censored, na.rm = TRUE), 
    Plan1_Prop = mean(Plan == 1,na.rm = TRUE),
    Plan2_Prop = mean(Plan == 2,na.rm = TRUE),
    Plan3_Prop = mean(Plan == 3,na.rm = TRUE)
    )
    
# Afficher le résumé statistique
print(summary_by_cluster)

# Boxplot pour le temps observé (Time)
ggplot(aligned_life_times, aes(x = as.factor(Cluster), y = Time, fill = as.factor(Cluster))) +
geom_boxplot() +
labs(title = "Comparaison du Temps par Cluster",
       x = "Cluster",
       y = "Temps observé") +
  theme_minimal()

# Diagramme en barres empilées
ggplot(aligned_life_times, aes(x = as.factor(Cluster), fill = as.factor(Plan))) +
geom_bar(position = "fill") +
labs(title = "Répartition des Plans par Cluster",
       x = "Cluster",
       y = "Proportion",
       fill = "Plan") +
  theme_minimal()

# Question 2 
# Question A
# i. Produisez un graphique de la fonction de survie selon les trois plans systématiques de maintenance.

# Charger les données
lifetimes <- read.csv("/Users/Nausicaa/Desktop/LifeTimes.csv")
# Charger les packages nécessaires
library(survival)
install.packages("survminer")
library(survminer)

# Création de l'objet de survie en tenant compte de la censure 
surv_object <- Surv(time = lifetimes$Time, event = 1 - lifetimes$Censored)

# Ajustement des courbes Kaplan-Meier pour les plans de maintenance 
fit <- survfit(surv_object ~ lifetimes$Plan)

# Visualisation des courbes avec le graphique de survie
ggsurvplot(
  fit,
data = lifetimes,
risk.table = TRUE,
pval = TRUE,
conf.int = TRUE,
legend.labs = c("Aucun", "6 mois", "12 mois"), legend.title = "Plan de maintenance",
title = "Fonction de survie selon les plans de maintenance", xlab = "Temps (jours)",
  ylab = "Probabilité de survie"
)

# Comparaison statistique des courbes avec un test log-rank
Comparison <- survdiff(surv_object ~ lifetimes$Plan) print(comparison)

# ii. Pour chacun des trois programmes de maintenance, quelle est la probabilité qu’une pompe soit encore fonctionnelle après 5 ans (pour simplifier, comptez 30 jours par mois, donc 360 jours par an, ou autrement dit, 5 ans = 1800 jours)?

# Créer l'objet de survie
surv_object <- Surv(time = lifetimes$Time, event = 1 - lifetimes$Censored)
# Ajuster les courbes Kaplan-Meier pour chaque plan 
fit <- survfit(surv_object ~ lifetimes$Plan)
# Obtenir la probabilité de survie à 1800 jours pour chaque plan 
time_point <- 1800 
surv_probs <- summary(fit, times = time_point)$surv
# Afficher les résultats
plan_labels <- c("Aucun entretien", "Entretien tous les 6 mois", "Entretien tous les 12 mois")
surv_table <- data.frame(Plan = plan_labels, Survie_5_ans = surv_probs) print(surv_table)

# iii. Calculezlesquartilesdutempsdeviepourlestroisplansdemaintenance.
# iv. Si vous êtes incapables d’obtenir certains quartiles, expliquez dans vos mots pourquoi il est impossible de les obtenir.

# Créer l'objet de survie
surv_object <- Surv(time = lifetimes$Time, event = 1 - lifetimes$Censored)
# Ajuster les courbes Kaplan-Meier pour chaque plan
fit <- survfit(surv_object ~ lifetimes$Plan)
# Calcul des quartiles
quartiles <- lapply(1:3, function(plan) {
# Filtrer les données pour le plan spécifique
subset <- lifetimes[lifetimes$Plan == plan, ]
surv_object_plan <- Surv(time = subset$Time, event = 1 - subset$Censored) fit_plan <- survfit(surv_object_plan ~ 1)
  # Obtenir les quartiles
summary(fit_plan, quantile = c(0.25, 0.5, 0.75))$quantile })
# Afficher les résultats dans un tableau
# Créer une table avec gestion des NA
quartile_table <- data.frame(
Plan = c("Aucun entretien", "Entretien tous les 6 mois", "Entretien tous les 12 mois"),
Q1 = sapply(quartiles, function(x) ifelse(length(x) >= 1, x[1], NA)),
Median = sapply(quartiles, function(x) ifelse(length(x) >= 2, x[2], NA)),
Q3 = sapply(quartiles, function(x) ifelse(length(x) >= 3, x[3], NA)) )
print(quartile_table)

# QUESTION B - À l’aide de la régression de Cox, établissez l’effet sur le risque instantané : i. De la maintenance aux 6 mois en comparaison de l’absence de maintenance systématique. ii. De la maintenance aux 12 mois en comparaison de l’absence de maintenance systématique. iii. De la maintenance aux 6 mois en comparaison de la maintenance systématique aux 6 mois. Dans les trois cas, fournissez un intervalle de confiance pour le cet effet. Est-il significatif au seuil de 5 % ?

# Créer l'objet de survie
surv_object <- Surv(time = lifetimes$Time, event = 1 - lifetimes$Censored)
# Ajuster un modèle de régression de Cox
cox_model <- coxph(surv_object ~ factor(Plan), data = lifetimes)
# Résumé du modèle
summary(cox_model)

# Re-coder la variable Plan pour prendre 12 mois comme référence
lifetimes$Plan_recode <- factor (lifetimes$Plan, levels = c(3,2))

# Ajuster un modèle de régression de Cox avec la variable re-codée
cox_model_recode <- coxph(surv_object ~ Plan_recode, data = lifetimes) 
# Résumé du modèle
summary(cox_model_recode)



