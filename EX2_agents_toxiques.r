library(car)
library(ggpubr)
data_project<-read.csv(file.choose(), sep = ',', header = TRUE)
data_project

data_project$poison <- as.factor(data_project$poison)
data_project$Traitement <- as.factor(data_project$Traitement)


plot <- ggline(data_project, x = "poison", y = "temps", color = "Traitement",
               add = c("mean_se", "dotplot"),
               palette = c("black", "blue", "red", "green"))
plot
#--------------------------------------------------
anova <- aov(temps ~ poison*Traitement, data = data_project)
anova2 <- aov(temps ~ poison+Traitement, data = data_project)
summary(anova)
summary(anova2)

residus <- residuals(anova)

shapiro.test(residus)

#--------------------------------------------------
summary(anova)
#--------------------------------------------------
bartlett.test(temps ~ interaction(poison,Traitement), data = data_project)
leveneTest(temps ~ poison * Traitement, data = data_project)
#--------------------------------------------------
TukeyHSD(anova2, which = "poison", data = data_project)
plot(TukeyHSD(anova2, which = "poison", data = data_project),las=2, col = "#3924b1")

TukeyHSD(anova2, which = "Traitement", data = data_project)
plot(TukeyHSD(anova2, which = "Traitement", data = data_project),las=2, col = "#3924b1")