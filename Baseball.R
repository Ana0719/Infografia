library("Lahman")
library(dplyr)
library(MASS)
library(pROC)
library(caret)
library(car)
library(bestglm)



wildcard_era_teams<-read.csv("C:/Users/Ana/Desktop/Posibles proyectos/baseball.csv")[,-1]
wildcard_era_teams$Playoff<-as.factor(wildcard_era_teams$Playoff)



set.seed(24)  # Para reproducibilidad
trainIndex <- createDataPartition(wildcard_era_teams$Playoff, p = .8, list = FALSE, times = 1)
trainData <- wildcard_era_teams[trainIndex, ]
testData  <- wildcard_era_teams[-trainIndex, ]




##  https://www.mlb.com/glossary/standard-stats
# 1.OPS On Base Plus Slugging: sumatoria de OBP y slugging
# 2.ERA Earned Run Average: exito del pitcher en evitar carreras ganadas
# 3.SF: Sacrifice flies jugador en tecera base y menos de dos outs, entonces se permite anotar la carrera
# 4.SO: Strikeouts
# 5.HA: Bateos permitido por el pitcher
# 6.RA run average rate at which runs are allowed or scored
# 7.SV: saved games: ccurs every time a relief pitcher either records a save or a blown save
# 8.BBA BB Alowed the average rate at which the man takes walks
# 9.DP Doble play: dos outs con una misma jugada
# 10.HRA: Home runs allowed total de home runs que permitio el pitcher
# 11.HR: Home runs
# 12.AB: at bats numero de veces que un jugador  va al plato y consigue un hit o un out
# 13.R: carreras
#14.ER earned run carreras sin el beneficio de un error o pase de bolta, sin mamadas defesivas
#15.X2B Doubles
#16.H hits
#17.SB bases robadas stolen bases
# 18.BB Base on Balls 
# 19.HBP Hit by pitch ocurre cuando un bateador es golpeado por una bola lanzada sin hacer un swing
# 20.SOA Strikeouts by pitchers
# 21.E Error jardinero si falla en convertir un out 
# 22.SHO Shutouts todo el juego sin cambio
#23.OBP: On-base Percentage how frequently a batter reaches base per plate appearance.
#24.Salary Salario
#25.SLG Slugging % represents the total number of bases a player records per at-bat.
#26.X3B Triple 
#27.CG Complete Game pitcher earns a complete game if he pitches the entire game for his team regardless of how long it lasts
#28. BA Batting Average
#29.CS  caught stealing 

glmfit_seleccion <- glm(Playoff ~  OPS+ERA+SF + SO + HA + RA + SV + BBA + DP + HRA + HR + AB + R + ER + X2B + H + SB + BB + HBP + SOA + E + SHO + OBP + Salary + SLG + X3B + CG + BA + CS, 
               data = trainData, family = binomial)%>% 
  stepAIC(direction = "backward",trace = TRUE)
summary(glmfit_seleccion)


# Matriz de confusión


col<-c("OPS","ERA","SO","HA","RA","SV","HR","R","ER","X2B","OBP","Salary","X3B","CG","BA", "Playoff")
df<-wildcard_era_teams[col]
df<-rename(df, y=Playoff)

best.logit <- bestglm(df,
                      IC = "AIC",                 # Information criteria for
                      family=binomial,
                      method = "exhaustive")






glmfit<- glm(Playoff ~ SO+HA+RA+SV+R+OBP+Salary+CG, 
                        data = trainData, family = binomial)
summary(glmfit)
probabilities <- glmfit %>% predict(testData, type = "response")
predicciones <- ifelse(probabilities> 0.5, "1", "0")
predicciones <- factor(predicciones, levels = levels(testData$Playoff))


matrix<-confusionMatrix(predicciones, testData$Playoff)


logit_P = predict(glmfit  , newdata = testData[,-49] ,type = 'response')
logit_P <- ifelse(logit_P > 0.5,1,0)
roc_score<-roc(as.numeric(testData[,49]),logit_P)
plot(roc_score ,main ="ROC curve -- Logistic Regression ")


