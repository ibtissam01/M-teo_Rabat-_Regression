                ###########################################
                ############___ELGHAZI_SOUFIANE___#########
                ############___LABYADY_IBTISSAM___#########
                ############_____MAASRI_AMINE_____#########
                ###########################################
#************************************************************************************************************************************************

######################################################__PROJET MODELES DE REGRESSION__###########################################################
                
#************************************************************************************************************************************************
#-----------------------------------------------------------------------------------------------------------------------------------------------#
# set the workspace :
                
setwd("C:\\Users\\elgha\\OneDrive\\Bureau\\my data\\ESI\\cours\\cycle_2année\\S4\\Modèles de régression\\Projet\\Script par R")

#**********************************************************_I.Exploration du weather:***********************************************************#
#-----------------------------------------------------------------------------------------------------------------------------------------------#
                
# installer les packages :
#install.packages("Metrics")
#install.packages("car")####--------- pour les analyses du régression
#install.packages("carData")####----- nécessaire pour "car"
#install.packages("questionr")###---- describe
#install.packages("QuantPsyc")# lm.beta
#install.packages("caret")# split the weather
#install.packages("rgl")# 3D plot
#install.packages("Hmisc") ###-------- cor.test& rcorr
                # Hmisc requires theses packages!!
                     #install.packages("lattice")
                     #install.packages("survival")
                     #install.packages("Formula")
                     #install.packages("ggplot2")
#-----------------------------------------------------------------------------------------------------------------------------------------------#
                
# Importer le jeux de donné et l'organiser:

#"Température(C)","Humidité(g/Kg)","Précipitation(mm/day)","Vitesse_Vent(m/s)"               
weather<-read.csv("C:/Users/elgha/OneDrive/Bureau/my data/ESI/cours/cycle_2année/S4/Modèles de régression/Projet/Dataset/Weather_Rabat.csv",sep=";",dec=".",row.names = 4)
names(weather)<- c("Année","Mois","Jour","Température","Précipitation","Humidité","Vitesse_Vent")
#-----------------------------------------------------------------------------------------------------------------------------------------------#
# Afficher les premières lignes
head(weather)
#-----------------------------------------------------------------------------------------------------------------------------------------------#
# Vérifier les valeurs manquantes/nettoyagae de la weather.
#is.na(weather)
which(is.na(weather),arr.ind = TRUE)
#-----------------------------------------------------------------------------------------------------------------------------------------------#
# Afficher la structure du weather et le résumé des variables
str(weather)
#-----------------------------------------------------------------------------------------------------------------------------------------------#
# Afficher la structure du weather et le résumé des variables
summary(weather)
#-----------------------------------------------------------------------------------------------------------------------------------------------#
# Afficher la description des variables
library(questionr)
describe(weather)
#-----------------------------------------------------------------------------------------------------------------------------------------------#
# Visualiser la distibution du variable Température par un histogramme:

library(ggplot2)
histogram_Température <- ggplot(weather, aes(x=Température)) +
  geom_histogram(aes(y=..density..), binwidth = 0.1, fill="blue", alpha=0.5) +
  geom_density(alpha = .2, fill="red") +
  ggtitle("Distibution du Température") +
  xlab("Température") + ylab("Density")

print(histogram_Température)

#-----------------------------------------------------------------------------------------------------------------------------------------------#
# Afficher la matrice de corrélation entre les variables

#                       Algébriquement:

#------Le coefficient de Pearson r 
round(cor(weather[, 4:7], use = "complete.obs", method = "pearson"),2)
#-----------------------------------------------------------------------------------------------------------------------------------------------#
#------corrélation entre deux variables quantitatives avec rcorr
library(lattice)
library(survival)
library(Formula)
library(ggplot2)
library(Hmisc)
rcorr(as.matrix(weather[, 4:7]))
#-----------------------------------------------------------------------------------------------------------------------------------------------#
#------Constuire des intervalles de confiance avec cor.test
cor.test(weather$Température , weather$Humidité )
cor.test(weather$Température , weather$Humidité , alternative = "greater")
#-----------------------------------------------------------------------------------------------------------------------------------------------#
#------Déterminer la variance expliquée
cor(weather[,4:7], use = "complete.obs") ^2
round(cor(weather[,4:7], use = "complete.obs")^2 *100, 2)
#-----------------------------------------------------------------------------------------------------------------------------------------------#
#                       Graphiquement :
pairs(weather[,4:7])
plot(weather$Température, weather$Humidité)

#-----------------------------------------------------------------------------------------------------------------------------------------------#
                                #------------------------------------------#
                                #     Il existe une relation significative #
                                #     entre la température et l'Humidité,  #  
                                #     r = 0.89, p (unilatéral) < .001.     #
                                #------------------------------------------#


#*******************************************_II.Split the weather into training and testing sets.***********************************************#

# Split the weather into 80% training and 20% testing
library(caret)
train_index <- createDataPartition(weather$Température, p = 0.8, list = FALSE)
train <- weather[train_index, ]
test <- weather[-train_index, ]


#**********************************************************_III.Régression linéaire simple******************************************************#

# Choix du modèle:

rls1<- lm(Température ~Humidité ,data=train)
rls2<- lm(Température ~Précipitation ,data=train)
rls3<- lm(Température ~Vitesse_Vent ,data=train)
##  Comparer les trois modèles:
##          ◦ Critère d’information d’Akaike (plus c’est petit, mieux est le modèle)
AIC(rls1,rls2,rls3)
##          ◦ Critère d’information Bayesien (plus c’est petit, mieux est le modèle)
BIC(rls1,rls2,rls3)

#------------->le modèle rls1 est mieux!!

#-----------------------------------------------------------------------------------------------------------------------------------------------#
# Faire une RLS pour prédire la variable Température  en fonction de la variable Humidité 

rls1
#-----------------------------------------------------------------------------------------------------------------------------------------------#
                                          #------------------------------------------------------------#
                                          #Alors: Température  = 3.345 + 1.536 Humidité                #
                                          #------------------------------------------------------------#
#-----------------------------------------------------------------------------------------------------------------------------------------------#
# Afficher la droite de régression
plot(Température ~Humidité ,data = train,main="Regression de Humidité  sur Température ")
abline(rls1,col='red',lwd=3)
#-----------------------------------------------------------------------------------------------------------------------------------------------#
# Afficher les coefficients et les résidus du modèle
coef(rls1)
#residuals(rls1)
#-----------------------------------------------------------------------------------------------------------------------------------------------#
# Afficher le résumé du modèle
summary(rls1)
#-----------------------------------------------------------------------------------------------------------------------------------------------#
# Qualité du modèle                                                                                                                             #

#     Le R² ajusté:                                                                                                                             #
#             On a la valeur de R deux (R²) est 0.7977 est proche de 1 ce que signifie que le model est bon.                                    #
#     Le RSS (Residual Sum of Squares):                                                                                                         #
#             plus le RSS est petit, plus le modèle est bon, et dans notre cas le RSS =1.885 n'est pas assez petit mais pour un degré de 6208 !.#
#-----------------------------------------------------------------------------------------------------------------------------------------------#

#----------------------------------------------------------------------------------#
# on remarque que notre pont est différente à 0(=1.535535)et la valeur significatif#
#  <2e-16 *** est inférieure à 0.001                                               #
#----------------------------------------------------------------------------------#
confint.default(rls1)
#-----------------------------------------------------------------------------------------------------------------------------------------------#
# et on constate que l'intervalle de confiance pour la ponte ne contient pas zero ([1.516446 , 1.554624]) alors c'est bon/il ya une bonne       #
#corrélation entre les deux variables                                                                                                           #
#-----------------------------------------------------------------------------------------------------------------------------------------------#
#-----------------------------------------------------------------------------------------------------------------------------------------------#
# Tester la validité du modèle
#       Le modèle de RLS est considéré comme valide si les résidus sont:

#            1. Indépendants:
#                     ◦ Test de Durbin-Watson:
#                             Tester l’hypothèse nulle H0 = les résidus sont indépendants.
#                             Si la p-value < 0,05 l’hypothèse est rejetée --
#-----------------------------------------------------------------------------------------------------------------------------------------------#
library(car)
library(carData)
durbinWatsonTest(rls1)
#-----------------------------------------------------------------------------------------------------------------------------------------------#
#-------------------------------------------------> H0 est rejeté!!!<---------------------------------------------------------------------------#
#-----------------------------------------------------------------------------------------------------------------------------------------------#
#-----------------------------------------------------------------------------------------------------------------------------------------------#
#           2. Distribués selon une loi normale de moyenne 0                                                                                    #
#                     ◦ Test de Shapiro-Wilk                                                                                                    #
#                              Tester H0 = les résidus suivent une loi normale.                                                                 #
#                              Si la p-value < 0,05 l’hypothèse est rejetée                                                                     #
#-----------------------------------------------------------------------------------------------------------------------------------------------#
residus<- residuals(rls1)
shapiro.test(residus[3:5000])
#-----------------------------------------------------------------------------------------------------------------------------------------------#
#-------------------------------------------------> H0 est rejeté!!!<---------------------------------------------------------------------------#
#-----------------------------------------------------------------------------------------------------------------------------------------------#
#-----------------------------------------------------------------------------------------------------------------------------------------------#
#           3. Distribués de façon homogène (i.e. varianceconstante)                                                                            #
#                     ◦ Test de Breush-Pagan                                                                                                    #
#                             Tester H0 = les résidus sont distribués de façon homogène.                                                        #
#                              Si la p-value < 0,05 l’hypothèse est rejetée                                                                     #
ncvTest(rls1)
#-----------------------------------------------------------------------------------------------------------------------------------------------#
#-------------------------------------------------> H0 est rejeté!!!<---------------------------------------------------------------------------#
#-----------------------------------------------------------------------------------------------------------------------------------------------#

#-----------------------------------------------------------------------------------------------------------------------------------------------#
#__________________________________________alors rls1 n'est pas valide!!!!!!____________________________________________________________________#
#-----------------------------------------------------------------------------------------------------------------------------------------------#

#Critere1:courbe residuels vs valeurs predits par model /Critere2:QQ-plot
plot(rls1)
#-----------------------------------------------------------------------------------------------------------------------------------------------#

#-----------------------------------------------------------------------------------------------------------------------------------------------#
#visualiser les valeurs prédit et les vraies valeurs .
ggplot(data=test, aes(x=predicted_temp_simple, y=test$Température)) + 
  geom_point(color="blue") + 
  geom_smooth(method="lm", se=FALSE, color="red") +
  labs(title="Courbe de prédiction", x="Valeurs prédites", y="Vraies valeurs") +
  theme_minimal()

#**********************************************************_IV.Régression linéaire multiple:****************************************************#

# Faire une RLM pour prédire la variable Température  en fonction des autres  variables 
library(carData)
library(car) 
library(ggplot2)
library(QuantPsyc) 
model_multiple1 = lm(Température~ Humidité + Précipitation  + Vitesse_Vent  , data = train)
model_multiple2 = lm(Température~ Humidité + Vitesse_Vent  , data = train)
model_multiple3 = lm(Température~ Humidité + Précipitation  , data = train)
summary(model_multiple1)
summary(model_multiple2)
summary(model_multiple3)
#-----------------------------------------------------------------------------------------------------------------------------------------------#
##  Comparer les trois modèles:
##          ◦ Critère d’information d’Akaike (plus c’est petit, mieux est le modèle)
AIC(model_multiple1,model_multiple2,model_multiple3)
##          ◦ Critère d’information Bayesien (plus c’est petit, mieux est le modèle)
BIC(model_multiple1,model_multiple2,model_multiple3)

## utilisant extractAIC()
extractAIC(model_multiple1)
extractAIC(model_multiple2)
extractAIC(model_multiple3)

#####------> on conclu que le modele model_multiple1 est mieux parmi ces modeles mais cela ne signifie que c'est le meilleur!!!
#-----------------------------------------------------------------------------------------------------------------------------------------------#

# Tracer le graphe avec la droite de régression multiple
library(ggplot2)
ggplot(train, aes(x = Humidité, y = Température)) +
  geom_point(aes(size = Précipitation, color = Vitesse_Vent)) +
  scale_size_continuous(range = c(1, 10)) +
  scale_color_continuous(low = "blue", high = "red") +
  geom_smooth(method = "lm", formula = y ~ x, se = FALSE, color = "black", 
              linetype = "dashed", aes(x = Humidité, y = Température)) +
  ggtitle("Température en fonction d'Humidité, Précipitation et Vitesse_Vent") +
  xlab("Humidité") + ylab("Température")
#-----------------------------------------------------------------------------------------------------------------------------------------------#

#-----------------------------------------------------------------------------------------------------------------------------------------------#
# test de multicollinéarité

#-----------------------------------------------------------------------------------------------------------------------------------------------#
#La multicollinéarité est une situation dans laquelle deux ou plusieurs variables indépendantes dans un modèle de régression sont fortement     #
#corrélées entre elles, ce qui peut causer des problèmes lors de l'estimation des coefficients de régression.                                   #
#Il existe plusieurs méthodes pour détecter la présence de multicollinéarité dans un modèle de régression :                                     #
#-----------------------------------------------------------------------------------------------------------------------------------------------#

#      1. Matrice de corrélation:
round(cor(weather[, 5:7], use = "complete.obs", method = "pearson"),2) 
#-----------------------------------------------------------------------------------------------------------------------------------------------#
# Une forte corrélation entre deux variables peut être un indicateur de multicollinéarité , mais dans notre cas
# les corrélations entre les variables sont assez faibles.
#-----------------------------------------------------------------------------------------------------------------------------------------------#

#      2.Facteur d'inflation de la variance (VIF):
vif(model_multiple1) # variance inflation factor 
#-----------------------------------------------------------------------------------------------------------------------------------------------#
#Le VIF est un indicateur de la quantité de variance dans un coefficient de régression qui est due à la corrélation avec d'autres variables     #
#indépendantes. Un VIF élevé (par exemple, supérieur à 5) peut indiquer la présence de multicollinéarité                                        #
# Dans notre cas les valeurs de VIF sont proche de 1 (inférieure à 5), ce qui suggère que la multicollinéarité entre les variables indépendantes#
# est faible. Par conséquent, il est peu probable que la multicollinéarité ait un impact significatif sur les résultats de notre modèle         #

#-----------------------------------------------------------------------------------------------------------------------------------------------#
#      3.Analyse en composantes principales (PCA) :
weather_matrix <- as.matrix(weather[,5:7])
weather_pca <- prcomp(weather_matrix, center = TRUE, scale. = TRUE)
summary(weather_pca)
plot(weather_pca, type = "l")
#-----------------------------------------------------------------------------------------------------------------------------------------------#
#La PCA est une méthode pour réduire la dimensionnalité des données en combinant les variables indépendantes en nouvelles variables appelées
#composantes principales. Si une ou plusieurs des premières composantes principales expliquent une grande partie de la variance dans les données
#, cela peut indiquer la présence de multicollinéarité.
#-----------------------------------------------------------------------------------------------------------------------------------------------#


#-----------------------------------------------------------------------------------------------------------------------------------------------#
#La "standard deviation" représente l'écart-type des scores des observations pour cette composante principale. Elle est utilisée pour
#mesurer la dispersion des données autour de la moyenne.

#-----------------------------------------------------------------------------------------------------------------------------------------------#
#La "proportion of variance" indique la proportion de la variance totale des données qui est expliquée par cette composante principale
#. Par exemple, PC1 explique 50,9% de la variance totale des données.

#-----------------------------------------------------------------------------------------------------------------------------------------------#
#La "cumulative proportion" est la somme cumulée des proportions de variance expliquées par les composantes principales précédentes
#. Par exemple, les deux premières composantes principales (PC1 et PC2) expliquent ensemble 82,6% de la variance totale des données.

#-----------------------------------------------------------------------------------------------------------------------------------------------#
#En général, les composantes principales les plus importantes sont celles qui expliquent la plus grande proportion de variance dans les données.
#Dans ce cas, PC1 explique plus de la moitié de la variance, ce qui en fait la composante principale la plus importante. Les résultats 
#de l'analyse PCA peuvent être utilisés pour réduire la dimensionnalité des données, pour mieux comprendre les relations entre les variables
#ou pour visualiser les données dans un espace à deux ou trois dimensions.
#-----------------------------------------------------------------------------------------------------------------------------------------------#
#       4.Analyse de variance (ANOVA) :
anova(model_multiple1)

#-----------------------------------------------------------------------------------------------------------------------------------------------#
#L'ANOVA peut être utilisée pour évaluer la contribution de chaque variable indépendante à la variance expliquée dans le modèle. Si une variable
#a une contribution faible à la variance expliquée, cela peut indiquer la présence de multicollinéarité.
#-----------------------------------------------------------------------------------------------------------------------------------------------#


#-----------------------------------------------------------------------------------------------------------------------------------------------#
#Ces résultats montrent les résultats d'une analyse de variance (ANOVA) appliquée à un modèle de régression linéaire multiple pour prédire
#la température en fonction de trois variables indépendantes : l'humidité, la précipitation et la vitesse du vent. et le modèle des deux 
#variables :l'humidité, la précipitation.

#L'ANOVA évalue si les variables indépendantes ont une influence significative sur la variable dépendante (la température) et fournit des
#informations sur la qualité du modèle. Dans ce cas, les résultats montrent que les trois variables indépendantes sont significativement 
#liées à la température (p < 0,001 pour chacune des trois variables). Cela signifie que chacune de ces variables contribue significativement
#à expliquer la variation de la température.

#Les résultats de l'ANOVA incluent également une évaluation de la qualité globale du modèle. Le F-value, qui mesure le rapport entre la variance
#expliquée par le modèle et la variance non expliquée, est élevé pour chaque variable indépendante, indiquant que le modèle est globalement 
#significatif. Le modèle dans son ensemble explique également une proportion significative de la variance de la température, car les résidus 
#ont une variance résiduelle faible (mean square residual = 3). Enfin, les signif. codes dans le tableau indiquent que toutes les 
#variables indépendantes ont une influence significative sur la température.

#En somme, ces résultats suggèrent que les variables indépendantes ont un effet significatif sur la température et que le modèle 
#de régression linéaire multiple est un ajustement approprié pour les données.
#-----------------------------------------------------------------------------------------------------------------------------------------------#


### Afficher les coefficients du modèle
coef(model_multiple1)


confint.default(model_multiple1)
# et on constate que l'intervalle de confiance pour les pontes ne contient pas zero
# alors c'est bon/il ya une bonne corrélation entre les 
# variables


### Qualité du modèle
##      Le R² ajusté:
#             On a la valeur de R deux (R²) est 0.8029 est proche de 1 et meilleur que le premier modele, ce que signifie que le model est bon.
##      Adjusted R-squared:
#             Adjusted R-squared:  0.8028  

### Tester la validité du modèle
#       Le modèle de model_multiple1 est considéré comme valide si les résidus sont:

###            1. Indépendants:
##                     ◦ Test de Durbin-Watson:
#                             Tester l’hypothèse nulle H0 = les résidus sont indépendants.
#                             Si la p-value < 0,05 l’hypothèse est rejetée --
library("carData")
library("car")
durbinWatsonTest(model_multiple1)

#---------->H0 rejeté!!!! p-value=0.048

###           2. Distribués selon une loi normale de moyenne 0
#                     ◦ Test de Shapiro-Wilk
#                              Tester H0 = les résidus suivent une loi normale. 
#                              Si la p-value < 0,05 l’hypothèse est rejetée
residus<- residuals(model_multiple1)
shapiro.test(residus[3:5000])

#---------->H0 Accepté!!!! p-value < 2.2e-16

###           3. Distribués de façon homogène (i.e. varianceconstante)
#                     ◦ Test de Breush-Pagan
#                             Tester H0 = les résidus sont distribués de façon homogène.
#                              Si la p-value < 0,05 l’hypothèse est rejetée
ncvTest(model_multiple1)

# résumer des résultats:
#Critere1:courbe residuels vs valeurs predits par model /Critere2:QQ-plot
plot(model_multiple1)

#---------->H0 accepté!!!! p = 0.17129
#-----------------------------------------------------------------------------------------------------------------------------------------------#
#__________________________________________alors model_multiple1 est valide!!!!!!____________________________________________________________________#
#-----------------------------------------------------------------------------------------------------------------------------------------------#

# Utilisation du modèle
predicted_temp_multiple<-predict(model_multiple1,newdata=test)
#predicted_temp_multiple

#      ◦ Intervalle de confiance
#                 La vraie valeur de Température  pour ces observations a une
#                 probabilité de 0,95 d’être dans l’intervalle [lwr , upr]
predict(model_multiple1,newdata=test,interval="confidence")
#      ◦ Intervalle de prédiction
#                 L’intervalle [lwr , upr] a une probabilité de 0,95 de
#                 contenir la prédiction de Température  pour une nouvelle
#                 observation 
predict(model_multiple1,newdata=test,interval="prediction")
#-----------------------------------------------------------------------------------------------------------------------------------------------#

#visualiser les valeurs prédit et les vraies valeurs .
ggplot(data=test, aes(x=predicted_temp_multiple, y=test$Température)) + 
  geom_point(color="orange") + 
  geom_smooth(method="lm", se=FALSE, color="green") +
  labs(title="Courbe de prédiction", x="Valeurs prédites", y="Vraies valeurs") +
  theme_minimal()

#**********************************************************_V.Régression Régularisé:************************************************************#

#   ◦ Adapter les données au format exigé par glmnet():
test_vars<-data.matrix(test[,-c(1,2,3)])
x_vars<-data.matrix(train[,-c(1,2,3)])
y<-data.matrix(train[,4])

#   ◦ Préparer la liste des valeurs de lambda à tester
lambdas<-10^seq(2,-2,-0.1)
library(glmnet)
library(Matrix)
# _________________Régression Ridge:

#-----------------------------------------------------------------------------------------------------------------------------------------------#
#nous pouvons réaliser la régression régularisée par Ridge en utilisant la fonction "glmnet" du package "glmnet". Pour la régression Ridge
#, on utilise le paramètre "alpha" égal à 0.
#-----------------------------------------------------------------------------------------------------------------------------------------------#

#   ◦ Trouver la meilleure valeur de lambda
reg_ridge_cv = cv.glmnet(x_vars,y,alpha=0,nlambda=50,family='gaussian',lambda=lambdas)
reg_ridge_cv$lambda.min

#   ◦ appliquer sur la valeur de lambda trouvé:

lambda_opt1<- reg_ridge_cv$lambda.min
reg_ridge = glmnet(x_vars,y,alpha=0,nlambda=50,family='gaussian',lambda=lambda_opt1)
reg_ridge

#-----------------------------------------------------------------------------------------------------------------------------------------------#
# _________________LASSO:

#-----------------------------------------------------------------------------------------------------------------------------------------------#
#De même, nous pouvons réaliser la régression régularisée par LASSO en utilisant la même fonction "glmnet", mais en utilisant 
#le paramètre "alpha" égal à 1.
#-----------------------------------------------------------------------------------------------------------------------------------------------#
#   ◦ Trouver la meilleure valeur de lambda
reg_lasso_cv = cv.glmnet(x_vars,y,alpha=1,nlambda=50,family='gaussian',lambda=lambdas)
reg_lasso_cv$lambda.min

#   ◦ appliquer sur la valeur de lambda trouvé:

lambda_opt2<- reg_lasso_cv$lambda.min
reg_lasso = glmnet(x_vars,y,alpha=1,nlambda=50,family='gaussian',lambda=lambda_opt2)
reg_lasso

#-----------------------------------------------------------------------------------------------------------------------------------------------#
# ____________ElasticNet:

#-----------------------------------------------------------------------------------------------------------------------------------------------#
#Enfin, nous pouvons réaliser la régression régularisée par ElasticNet en utilisant la même fonction "glmnet", mais en utilisant 
#un paramètre "alpha" entre 0 et 1 pour contrôler le mélange de Ridge et LASSO.
#-----------------------------------------------------------------------------------------------------------------------------------------------#
#   ◦ Trouver la meilleure valeur de lambda
reg_elasticnet_cv = cv.glmnet(x_vars,y,alpha=0.5,nlambda=50,family='gaussian',lambda=lambdas)
reg_elasticnet_cv$lambda.min

#   ◦ appliquer sur la valeur de lambda trouvé:

lambda_opt3<- reg_elasticnet_cv$lambda.min
reg_elasticnet = glmnet(x_vars,y,alpha=0.5,nlambda=50,family='gaussian',lambda=lambda_opt3)
reg_elasticnet
#-----------------------------------------------------------------------------------------------------------------------------------------------#
#Une fois les modèles entraînés, nous pouvons évaluer leur performance en utilisant l'ensemble de test. Pour cela, nous pouvons utiliser 
#la fonction "predict" pour prédire les valeurs de la variable cible à partir des variables d'entrée de l'ensemble de test.
#-----------------------------------------------------------------------------------------------------------------------------------------------#
# Prédiction des valeurs avec le modèle Ridge
ridge_pred <- predict(reg_ridge, newx = test_vars)
# Prédiction des valeurs avec le modèle LASSO
lasso_pred <- predict(reg_lasso, newx = test_vars)
# Prédiction des valeurs avec le modèle ElasticNet
elastic_pred <- predict(reg_elasticnet, newx = test_vars)
                        
# Comparaison des trois modèles:
#-----------------------------------------------------------------------------------------------------------------------------------------------#
#Pour évaluer les performances des modèles, nous pouvons utiliser des métriques d'évaluation telles que l'erreur quadratique moyenne (EQM)
#et le coefficient de détermination (R²). Nous allons calculer ces métriques pour les prédictions du modèle Ridge, du modèle LASSO et 
#du modèle ElasticNet sur l'ensemble de test.
#-----------------------------------------------------------------------------------------------------------------------------------------------#
# calcul de l'EQM
mse_ridge <- mean((test$Température - ridge_pred)^2)
mse_lasso <- mean((test$Température - lasso_pred)^2)
mse_elasticnet <- mean((test$Température - elastic_pred)^2)
#-----------------------------------------------------------------------------------------------------------------------------------------------#
# calcul de R²
rsq_ridge <- 1 - mse_ridge/var(test$Température)
rsq_lasso <- 1 - mse_lasso/var(test$Température)
rsq_elasticnet <- 1 - mse_elasticnet/var(test$Température)
#-----------------------------------------------------------------------------------------------------------------------------------------------#
#les erreurs de prédiction
ridge_errors <- ridge_pred - test$Température
lasso_errors <- lasso_pred - test$Température
elasticnet_errors<- elastic_pred - test$Température
#-----------------------------------------------------------------------------------------------------------------------------------------------#
# affichage des métriques d'évaluation/Comparer les erreurs de prédiction pour les trois modèles
mean_ridge_error <- mean(abs(ridge_errors))
mean_lasso_error <- mean(abs(lasso_errors))
mean_elasticnet_error <- mean(abs(elasticnet_errors))
cat("L'erreur de prédiction moyenne pour le modèle de régression ridge est", round(mean_ridge_error, 2), "\n")
cat("L'erreur de prédiction moyenne pour le modèle de régression lasso est", round(mean_lasso_error, 2), "\n")
cat("L'erreur de prédiction moyenne pour le modèle de régression elasticnet est", round(mean_elasticnet_error, 2), "\n")


cat("Ridge: MSE =", round(mse_ridge, 2), "R² =", round(rsq_ridge, 2), "\n")
cat("LASSO: MSE =", round(mse_lasso, 2), "R² =", round(rsq_lasso, 2), "\n")
cat("ElasticNet: MSE =", round(mse_elasticnet, 2), "R² =", round(rsq_elasticnet, 2), "\n")
#-----------------------------------------------------------------------------------------------------------------------------------------------#
# Visualiser les modeles graphiquement:
plot(test$Humidité, test$Température, type="p", col="black")
lines(test$Humidité, ridge_pred, type="l", col="yellow")
lines(test$Humidité, lasso_pred, type="l", col="pink")
lines(test$Humidité, elastic_pred, type="l", col="brown")
#-----------------------------------------------------------------------------------------------------------------------------------------------#
#-------------------> On remarque l'Overffitting dans ces modèles régularisé!!!!!

#**********************************************************_VI.Régression Non linéaire:*********************************************************#
library(Metrics)
#Exponentielle :
model_expo<- nls(Température ~ a * exp(b * Humidité), data = train, start = list(a = 1, b = 0.01))
predictions_expo <- predict(model_expo, newdata = test)
RMSE_expo <- rmse(predictions_expo, test$Température)
R2_expo <- R2(predictions_expo, test$Température)

#Puissance
model_puiss<-nls(Température ~ a * Humidité^b, data = train, start = list(a = 1, b = 0.01))
predictions_puiss <- predict(model_puiss, newdata = test)
RMSE_puiss <- rmse(predictions_puiss, test$Température)
R2_puiss <- R2(predictions_puiss, test$Température)
#Logarithmique
model_log<-nls(Température ~ a * log(Humidité), data = train, start = list(a = 1))
predictions_log <- predict(model_log, newdata = test)
RMSE_log <- rmse(predictions_log, test$Température)
R2_log <- R2(predictions_log, test$Température)

cat("Exponentielle: RMSE =", RMSE_expo, "R² =", R2_expo, "\n")
cat("Puissance: RMSE =", RMSE_puiss, "R² =", R2_puiss, "\n")
cat("Logarithmique: RMSE =", RMSE_log, "R² =", R2_log, "\n")

library(ggplot2)

ggplot(test, aes(x = Humidité, y = Température)) +
  geom_point() +  # Add scatter plot points
  geom_smooth(method="lm",formula=y~x,se=FALSE,color="purple") + # regression lineaire
  geom_smooth(method="nls",formula=y~a*exp(b*x),method.args=list(start=c(a=1,b=0.01)),se=FALSE,color="red") + # regression exponential
  geom_smooth(method="nls",formula=y~a*x^b,method.args=list(start=c(a=1,b=0.01)),se=FALSE,color="blue") + # regression puissance
  geom_smooth(method="nls",formula=y~a*log(x),method.args=list(start=c(a=1)),se=FALSE,color="green") + # regression logarithmique 
  labs(x = "Humidité", y = "Température")

# On remarque que le modele puissance de Humidté qui donne valeur de RMSE minimale et R2 meilleur (model_puiss)

#-----------------------------------------------------------------------------------------------------------------------------------------------#
# Qualité du modèle                                                                                                                             #
summary(model_puiss)
#-----------------------------------------------------------------------------------------------------------------------------------------------#
#visualiser les valeurs prédit et les vraies valeurs .
ggplot(data=test, aes(x=predictions_puiss, y=test$Température)) + 
  geom_point(color="black") + 
  geom_smooth(method="nls", se=FALSE, color="yellow",formula=y~a*x^b, method.args = list(start = list(a = 2.888999, b = 0.813768))) +
  labs(title="Courbe de prédiction en puissance", x="Valeurs prédites", y="Vraies valeurs") +
  theme_minimal()


#**********************************************************_VII.Régression Polynomiale:*********************************************************#

# Ajuster des modèles de régression polynomiale avec différents degrés de polynômes

# Modèle de régression polynomiale d'ordre 2
model2_poly_precip <- lm(Température ~ poly(Précipitation, 2), data = train)
prediction2_poly_precip <- predict(model2_poly_precip, newdata = test)
RMSE2_poly_precip <- sqrt(mean((test$Température - prediction2_poly_precip)^2))

model2_poly_humid <- lm(Température ~ poly(Humidité,2), data = train)
prediction2_poly_humid <- predict(model2_poly_humid, newdata = test)
RMSE2_poly_humid <- sqrt(mean((test$Température - prediction2_poly_humid)^2))

model2_poly_vent <- lm(Température ~ poly(Vitesse_Vent,2), data = train)
prediction2_poly_vent <- predict(model2_poly_vent, newdata = test)
RMSE2_poly_vent <- sqrt(mean((test$Température - prediction2_poly_vent)^2))
# Modèle de régression polynomiale d'ordre 3
model3_poly_precip <- lm(Température ~ poly(Précipitation, 3), data = train)
prediction3_poly_precip <- predict(model3_poly_precip, newdata = test)
RMSE3_poly_precip <- sqrt(mean((test$Température - prediction3_poly_precip)^2))

model3_poly_humid <- lm(Température ~ poly(Humidité,3), data = train)
prediction3_poly_humid <- predict(model3_poly_humid, newdata = test)
RMSE3_poly_humid <- sqrt(mean((test$Température - prediction3_poly_humid)^2))

model3_poly_vent <- lm(Température ~ poly(Vitesse_Vent,3), data = train)
prediction3_poly_vent <- predict(model3_poly_vent, newdata = test)
RMSE3_poly_vent <- sqrt(mean((test$Température - prediction3_poly_vent)^2))

# Modèle de régression polynomiale d'ordre 4
model4_poly_precip <- lm(Température ~ poly(Précipitation,4), data = train)
prediction4_poly_precip <- predict(model4_poly_precip, newdata = test)
RMSE4_poly_precip <- sqrt(mean((test$Température - prediction4_poly_precip)^2))

model4_poly_humid <- lm(Température ~ poly(Humidité,4), data = train)
prediction4_poly_humid <- predict(model4_poly_humid, newdata = test)
RMSE4_poly_humid <- sqrt(mean((test$Température - prediction4_poly_humid)^2))

model4_poly_vent <- lm(Température ~ poly(Vitesse_Vent,4), data = train)
prediction4_poly_vent <- predict(model4_poly_vent, newdata = test)
RMSE4_poly_vent <- sqrt(mean((test$Température - prediction4_poly_vent)^2))

# Comparaison des performances des modèles
data.frame(Degré = c(2, 3, 4), RMSE = c(RMSE2_poly_precip, RMSE3_poly_precip, RMSE4_poly_precip))
data.frame(Degré = c(2, 3, 4), RMSE = c(RMSE2_poly_humid, RMSE3_poly_humid, RMSE4_poly_humid))
data.frame(Degré = c(2, 3, 4), RMSE = c(RMSE2_poly_vent, RMSE3_poly_vent, RMSE4_poly_vent))

# On remarque que le modele polynomiale de degrée 3 de Humidté qui donne valeur de RMSE minimale (model3_poly_humid)
# Afficher le résumé du modèle
#-----------------------------------------------------------------------------------------------------------------------------------------------#
# Qualité du modèle                                                                                                                             #
#     Le R² ajusté:                                                                                                                             #
#             On a la valeur de R deux (R²) est 0.801 est proche de 1 ce que signifie que le model est bon.                                    #
#     Le RSS (Residual Sum of Squares):                                                                                                         #
#             plus le RSS est petit, plus le modèle est bon, et dans notre cas le RSS =1.875  n'est pas assez petit mais pour un degré de 6202 !.#
#-----------------------------------------------------------------------------------------------------------------------------------------------#
 summary(model3_poly_humid)
 coef(model3_poly_humid)
#-----------------------------------------------------------------------------------------------------------------------------------------------#
# Tester la validité du modèle
#       Le modèle de model3_poly_humid est considéré comme valide si les résidus sont:

#            1. Indépendants:
#-----------------------------------------------------------------------------------------------------------------------------------------------#
library(car)
durbinWatsonTest(model3_poly_humid)
#-----------------------------------------------------------------------------------------------------------------------------------------------#
#-------------------------------------------------> H0 est rejeté!!!<---------------------------------------------------------------------------#
#-----------------------------------------------------------------------------------------------------------------------------------------------#
#-----------------------------------------------------------------------------------------------------------------------------------------------#
#           2. Distribués selon une loi normale de moyenne 0                                                                                    #                                                                   #
#-----------------------------------------------------------------------------------------------------------------------------------------------#
residus<- residuals(model3_poly_humid)
shapiro.test(residus[3:5000])
#-----------------------------------------------------------------------------------------------------------------------------------------------#
#-------------------------------------------------> H0 est rejeté!!!<---------------------------------------------------------------------------#
#-----------------------------------------------------------------------------------------------------------------------------------------------#
#-----------------------------------------------------------------------------------------------------------------------------------------------#
#           3. Distribués de façon homogène (i.e. varianceconstante)                                                                            #                                                                 #
ncvTest(model3_poly_humid)
#-----------------------------------------------------------------------------------------------------------------------------------------------#
#-------------------------------------------------> H0 est rejeté!!!<---------------------------------------------------------------------------#
#-----------------------------------------------------------------------------------------------------------------------------------------------#

#-----------------------------------------------------------------------------------------------------------------------------------------------#
#__________________________________________alors model3_poly_humid n'est pas valide!!!!!!____________________________________________________________________#
#-----------------------------------------------------------------------------------------------------------------------------------------------#


#-----------------------------------------------------------------------------------------------------------------------------------------------#
#visualiser les valeurs prédit et les vraies valeurs .
ggplot(data=test, aes(x=prediction3_poly_humid, y=test$Température)) + 
  geom_point(color="blue") + 
  geom_smooth(method="lm", se=FALSE, color="red")  +
  labs(title="Courbe de prédiction polynomiale", x="Valeurs prédites", y="Vraies valeurs") +
  theme_minimal()
#-----------------------------------------------------------------------------------------------------------------------------------------------#


#**********************************************************Conclusion:**************************************************************************#
#-----------------------------------------------------------------------------------------------------------------------------------------------#
# On va ploter tous les modeles qu' on a extrait:

# Tracer la première scatter avec plot()
plot(test$Humidité, test$Température, type="p", col="black")
# Ajouter les autres courbes avec lines()
lines(test$Humidité, predicted_temp_simple, type="l", col="blue")
lines(test$Humidité, predicted_temp_multiple, type="l", col="green")
lines(test$Humidité, prediction3_poly_humid, type="l", col="red")
lines(test$Humidité, predictions_puiss, type="l", col="orange")

#-----------------------------------------------------------------------------------------------------------------------------------------------#
#_________________________ On conclut que le modèle _________model_multiple1__________ est le meilleur dans notre cas!!!!!___________________________________#
#-----------------------------------------------------------------------------------------------------------------------------------------------#


#-----------------------------------------------------------------------------------------------------------------------------------------------#
#Jusqu'à maintenant on a réaliser la Préparation des données , Déterminer le bon modèle , Entraîner et évaluer le modèle ,il nous restre        #
#maintenant le deploiment du modèle                                                                                                             #
#-----------------------------------------------------------------------------------------------------------------------------------------------#

#Alors on va enregistrer le modèle sous format RDS afin de le deployer:
saveRDS(model_multiple1,"Prediction_température.rds")

#**********************************************************_VIII.Validation du modèle:*********************************************************#
 # on a réalise une application shiny pour validation et deploiment du modèle
 # les résultats de modèle sur la dataset de validation sont acceptables
