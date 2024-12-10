data_acc <- read.csv('Fish.CSV', header=TRUE, sep=",")
data_acc
# afficher les donnes

summary(lm(ï..Weight ~ ., data = data_acc))
op<-par(mfrow = c(1,1))
boxplot(data_acc$Length1,data_acc$Length2,data_acc$Length3,data_acc$Height,data_acc$Width)



# visualiser le jeu de données
boxplot(data_acc$Length1)
which(data_acc$Length1>55)
#[1] 143 144 145

boxplot(data_acc$Length2)
which(data_acc$Length2>59) #[1] 143 144 145


boxplot(data_acc$Length3)
which(data_acc$Length3>65) #[1] 145

boxplot(data_acc$Height) # La Height colonne n'a pas de valeurs aberrantes
boxplot(data_acc$Width)  # La Width colonne n'a pas de valeurs aberrantes


new_data_acc <- data_acc[-c(143,144,145),] 
#supprimer les valeurs aberrantes dans les lignes 143,144,145
#recharger le tableau des statistiques pour vérifier si le modele est toujours bon
new_data_acc
dim(new_data_acc)
dim(data_acc)
#vérifier le sous-trait et le nombre d'observations dans les données

boxplot(new_data_acc$Length1,new_data_acc$Length2,new_data_acc$Length3,new_data_acc$Height,new_data_acc$Width)
# visualiser le jeu de données encore


boxplot(new_data_acc$Length1)
boxplot(new_data_acc$Length2)
boxplot(new_data_acc$Length3)
boxplot(new_data_acc$Height)
boxplot(new_data_acc$Width)
#toutes les valeurs aberrantes ont été supprimées et le nombre est inférieur à 5 % du total des observations de la base de données

view <- lm(ï..Weight ~ ., data = new_data_acc) # charger le modele lineaire
summary(view)# afficher le tableau statistique

#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
#(Intercept) -426.943     25.576 -16.693  < 2e-16 ***
#  Length1      102.558     33.218   3.087  0.00241 ** 
#  Length2      -45.076     34.413  -1.310  0.19224    
#  Length3      -37.148     14.293  -2.599  0.01028 *  
#  Height        36.847      7.254   5.079 1.11e-06 ***
#  Width         52.402     17.113   3.062  0.00261 ** 
#  ---
#  Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

#Residual standard error: 100.7 on 150 degrees of freedom
#Multiple R-squared:  0.9038,	Adjusted R-squared:  0.9006 
#F-statistic:   282 on 5 and 150 DF,  p-value: < 2.2e-16




#AIC
# Choisissez le meilleur modele
step(view, direction = "backward")

# Modele choisi: ï..Weight ~ Length1 + Length3 + Height + Width, data = new_data_acc)
## --> Lineaire Equation: y = -431.98  + 63.64(Length1) - 44.51(Length3) +37.76(Height) + 45.06(Width) 
# AIC=1444.72


#BIC
# Choisissez le meilleur modele
n = length(resid(view))
step(view,direction = "backward",k = log(n))


#AIC=1459.97
#Coefficients:
#(Intercept)      Length1      Length3       Height        Width  
#-431.98        63.64       -44.51        37.76        45.06  

view2 <- lm(ï..Weight ~ Length1 + Length3 + Height + Width, data = new_data_acc) # charger le meilleur modele lineaire
summary(view2)# afficher le tableau statistique


#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
#(Intercept) -431.981     25.345 -17.044  < 2e-16 ***
#  Length1       63.641     14.891   4.274 3.39e-05 ***
#  Length3      -44.511     13.172  -3.379 0.000925 ***
#  Height        37.763      7.238   5.218 5.89e-07 ***
#  Width         45.056     16.207   2.780 0.006126 ** 
#  ---
#  Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

#Residual standard error: 101 on 151 degrees of freedom
#Multiple R-squared:  0.9027,	Adjusted R-squared:  0.9002 
#F-statistic: 350.4 on 4 and 151 DF,  p-value: < 2.2e-16



shapiro.test(residuals(view2))
# Le modele ne suit pas une distribution normale


temp_data <-new_data_acc


