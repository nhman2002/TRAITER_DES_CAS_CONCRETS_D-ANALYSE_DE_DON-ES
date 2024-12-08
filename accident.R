data_acc <- read.csv('tauxaccidents.CSV', header=TRUE, sep=",")

modele_1 <- lm(y_i~.,data=data_acc)
modele_1
summary(modele_1)
# Dans le modèle initial, il semble que les variables n'ont pas de sens

# --- Ameliorer ---
boxplot(data_acc)

boxplot(data_acc$x_i.1)  #--> il a des outliers
outliers_1 <- boxplot.stats(data_acc$x_i.1)$out
which(data_acc$x_i.1 %in% c(outliers_1))            # => row 21

boxplot(data_acc$x_i.2)  #--> il a des outliers
outliers_2 <- boxplot.stats(data_acc$x_i.2)$out
which(data_acc$x_i.2 %in% c(outliers_2))            # => row 1 2 4

boxplot(data_acc$x_i.3)  #--> il n'y a pas de outlier 


boxplot(data_acc$x_i.4)

boxplot(data_acc$x_i.5)  #--> il a des outliers
outliers_5 <- boxplot.stats(data_acc$x_i.5)$out
which(data_acc$x_i.5 %in% c(outliers_5))            # => row 19 31 32 34 37

boxplot(data_acc$x_i.6)  #--> il n'y a pas de outlier

boxplot(data_acc$x_i.7)  #--> il a des outliers
outliers_7 <- boxplot.stats(data_acc$x_i.7)$out
which(data_acc$x_i.7 %in% c(outliers_7))            # => row 1 2 3 4 9

boxplot(data_acc$x_i.8)  #--> il a des outliers
outliers_8 <- boxplot.stats(data_acc$x_i.8)$out
which(data_acc$x_i.8 %in% c(outliers_8))            # => row 6 11 25

boxplot(data_acc$x_i.9)  #--> il a des outliers
outliers_9 <- boxplot.stats(data_acc$x_i.9)$out
which(data_acc$x_i.9 %in% c(outliers_9))            # => row 25 27 29

boxplot(data_acc$x_i.10)  #--> il a 1 outlier
outliers_10 <- boxplot.stats(data_acc$x_i.10)$out
which(data_acc$x_i.10 %in% c(outliers_10))          # => row 1

boxplot(data_acc$x_i.11)  #--> il a des outliers
outliers_11 <- boxplot.stats(data_acc$x_i.11)$out
which(data_acc$x_i.11 %in% c(outliers_11))          # => row 1 2 3 4 5

boxplot(data_acc$x_i.12) #--> il n'y a pas de outlier

boxplot(data_acc$x_i.13)  #--> il n'y a pas de outlier

# => Il y a 17 lignes contenant des valeurs outliers sur un total de 39 observations 
#dans les lignes 1, 2, 3, 4, 5, 6, 9, 11, 19, 21, 25, 27, 29, 31, 32, 34, 37.
# => Le nombre de lignes contenant des valeurs outliers représente 43,59 % du total des observations.

# -> Il n'est pas possible de supprimer toutes les lignes avec des valeurs outliers, car elles représentent plus de 5 % du total des observations

#=> Par conséquent, nous pouvons remplacer les valeurs outliers par la moyenne ou la médiane de la colonne respective pour réduire les valeurs outliers.


temp_data <- data_acc       # faire 1 copie de data_acc

temp_data$x_i.1[which(temp_data$x_i.1 %in% c(outliers_1))] <- median(data_acc$x_i.1,na.rm = TRUE)
temp_data$x_i.2[which(temp_data$x_i.2 %in% c(outliers_2))] <- median(data_acc$x_i.2,na.rm = TRUE)
temp_data$x_i.5[which(temp_data$x_i.5 %in% c(outliers_5))] <- median(data_acc$x_i.5,na.rm = TRUE)
temp_data$x_i.7[which(temp_data$x_i.7 %in% c(outliers_7))] <- median(data_acc$x_i.7,na.rm = TRUE)
temp_data$x_i.8[which(temp_data$x_i.8 %in% c(outliers_8))] <- median(data_acc$x_i.8,na.rm = TRUE)
temp_data$x_i.9[which(temp_data$x_i.9 %in% c(outliers_9))] <- median(data_acc$x_i.9,na.rm = TRUE)
temp_data$x_i.10[which(temp_data$x_i.10 %in% c(outliers_10))] <- median(data_acc$x_i.10,na.rm = TRUE)
temp_data$x_i.11[which(temp_data$x_i.11 %in% c(outliers_11))] <- median(data_acc$x_i.11,na.rm = TRUE)

boxplot(temp_data)

boxplot(temp_data$x_i.2)
outliers_2t <- boxplot.stats(temp_data$x_i.2)$out
which(temp_data$x_i.2 %in% c(outliers_2t))      # => row 3

boxplot(temp_data$x_i.7)
outliers_7t <- boxplot.stats(temp_data$x_i.7)$out
which(temp_data$x_i.7 %in% c(outliers_7t))      # => row 5 16

boxplot(temp_data$x_i.8)    
outliers_8t <- boxplot.stats(temp_data$x_i.8)$out
which(temp_data$x_i.8 %in% c(outliers_8t))  # row 9 10 12 20 26

boxplot(temp_data$x_i.9)
outliers_9t <- boxplot.stats(temp_data$x_i.9)$out
which(temp_data$x_i.9 %in% c(outliers_9t))  # row 6

# Le nombre de valeurs outliers après remplacement par la médiane est encore beaucoup (aux lignes 3 5 6 9 10 12 16 20 26) par rapport au nombre total d'observations
# Donc, remplacer par la médiane n'est toujours pas vraiment bon

# ==> Remplacer par Q3

new_data <- data_acc       # faire 1 copie de data_acc

new_data$x_i.1[which(new_data$x_i.1 %in% c(outliers_1))] <- quantile(data_acc$x_i.1,0.75,na.rm = TRUE)
new_data$x_i.2[which(new_data$x_i.2 %in% c(outliers_2))] <- quantile(data_acc$x_i.2,0.75,na.rm = TRUE)
new_data$x_i.5[which(new_data$x_i.5 %in% c(outliers_5))] <- quantile(data_acc$x_i.5,0.75,na.rm = TRUE)
new_data$x_i.7[which(new_data$x_i.7 %in% c(outliers_7))] <- quantile(data_acc$x_i.7,0.75,na.rm = TRUE)
new_data$x_i.8[which(new_data$x_i.8 %in% c(outliers_8))] <- quantile(data_acc$x_i.8,0.75,na.rm = TRUE)
new_data$x_i.9[which(new_data$x_i.9 %in% c(outliers_9))] <- quantile(data_acc$x_i.9,0.75,na.rm = TRUE)
new_data$x_i.10[which(new_data$x_i.10 %in% c(outliers_10))] <- quantile(data_acc$x_i.10,0.75,na.rm = TRUE)
new_data$x_i.11[which(new_data$x_i.11 %in% c(outliers_11))] <- quantile(data_acc$x_i.11,0.75,na.rm = TRUE)

boxplot(new_data)

# On voit que, lorsque nous le remplaçons par Q3, le nombre de valeurs outliers est bien inférieur

modele_2 <- lm(y_i~.,data=new_data)
summary(modele_2)


# ========== AIC ==========
# Choix du modèle par AIC
step(modele_2)
# Modele Choisi: y_i = x_i.1 + x_i.3 + x_i.4 + x_i.8 + x_i.9 + x_i.12
# Équation linéaire: y_i = 12.08859 - 0.09550(x_i.1) - 0.14805(x_i.3) - 0.11327(x_i.4) + 1.34911(x_i.8) + 0.08108(x_i.9) - 1.27857(x_i.12)

modele_3 <- lm(y_i~x_i.1+x_i.3+x_i.4+x_i.8+x_i.9+x_i.12, data=new_data)
summary(modele_3)
#Coefficients:
#             Estimate Std. Error t value Pr(>|t|)
# (Intercept) 12.08859    2.73669   4.417 0.000107 ***
# x_i.1       -0.09550    0.03149  -3.033 0.004775 ** 
# x_i.3       -0.14805    0.08898  -1.664 0.105883
# x_i.4       -0.11327    0.04255  -2.662 0.012055 *
# x_i.8        1.34911    0.56651   2.381 0.023362 *  
# x_i.9        0.08108    0.04524   1.792 0.082579 .
# x_i.12      -1.27857    0.41928  -3.049 0.004577 **

# Residual standard error: 1.119 on 32 degrees of freedom
# Multiple R-squared:  0.7329,    Adjusted R-squared:  0.6828
# F-statistic: 14.63 on 6 and 32 DF,  p-value: 5.768e-08


# ========== BIC ==========
# Choix du modèle par BIC
n = length(resid(modele_2))
step(modele_2, direction="backward", k=log(n))

modele_4 <- lm(y_i~x_i.1+x_i.4+x_i.8+x_i.9+x_i.12, data=new_data)
summary(modele_4)


# Coefficients:
#             Estimate Std. Error t value Pr(>|t|)
# (Intercept) 11.03540    2.73292   4.038 0.000302 ***
# x_i.1       -0.11421    0.03019  -3.784 0.000619 ***
# x_i.4       -0.11726    0.04361  -2.689 0.011152 *
# x_i.8        1.54184    0.56921   2.709 0.010622 *
# x_i.9        0.09050    0.04608   1.964 0.057988 .
# x_i.12      -1.35353    0.42788  -3.163 0.003341 **
# ---

# Residual standard error: 1.148 on 33 degrees of freedom
# Multiple R-squared:  0.7097,    Adjusted R-squared:  0.6658 
# F-statistic: 16.14 on 5 and 33 DF,  p-value: 4.76e-08


# Residuals:
res_modele_4 <- residuals(modele_4)
shapiro.test(res_modele_4)

# > shapiro.test(res_modele_4)

#         Shapiro-Wilk normality test

# data:  res_modele_4
# W = 0.97982, p-value = 0.6972  =>  Suit de loi normal 
