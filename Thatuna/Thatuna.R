library(readxl)
library(mlbench)
library(caret)
library(leaps)
library(olsrr)
library(tidyverse)
library(caret)
library(MASS)
library(Hmisc)
library(corrplot)
library(RColorBrewer)
library(gridExtra)


read_xlsx("Palouse.xlsx", sheet = "20cm")[-c(1:10,24:25)]
head(df)
df1 <- read_xlsx("Thatuna.xlsx", sheet = "10cm")[11:21]
df2 <- read_xlsx("Thatuna.xlsx", sheet = "20cm")[11:22]
#custom control parameteres
custom <- trainControl(method = "repeatedcv",
                       repeats = 10,
                       number = 4,
                       verboseIter = F)

is.na(df2) <- !df2 # to change 0s to NA
df2
# Fit the full model 
set.seed(123)
full.model1 <- lm(pH ~., data = df1)
set.seed(123)
full.model2 <- lm(pH ~., data = df2, na.action = na.omit)
# Stepwise regression model
step.model1 <- stepAIC(full.model1, direction = "both", 
                       trace = FALSE, trControl = custom)

step.model2 <- stepAIC(full.model2, direction = "both", 
                       trace = FALSE, trControl = custom)
summary(step.model1)
summary(step.model2)

varImp(step.model1)
varImp(step.model2)

################## corrrelation#############3

df1 <- read_xlsx("Naff.xlsx", sheet = "10c")[11:22]
df2 <- read_xlsx("Naff.xlsx", sheet = "20c")[11:22]
df3 <- read_xlsx("Naff.xlsx", sheet = "30c")[11:22]
df4 <- read_xlsx("Naff.xlsx", sheet = "60c")[11:22]
df5 <- read_xlsx("Naff.xlsx", sheet = "90c")[11:22]
df6 <- read_xlsx("Naff.xlsx", sheet = "150c")[11:22]




# Mark the insignificant coefficients according to the specified p-value significance level
col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))
png(filename = "corrplot3_new.png", width = 12, height = 15, units = "in", res = 600)

par(mfrow = c(3,2))
cor1 <- rcorr (as.matrix(df1))
p_mat1 <- cor1$r
M1 <-cor(df1, method = "spearman")

cor2 <- rcorr (as.matrix(df2))
p_mat2 <- cor2$r
M2 <-cor(df2, method = "spearman")

cor3 <- rcorr (as.matrix(df3))
p_mat3 <- cor3$r
M3 <-cor(df3, method = "spearman")

cor4 <- rcorr (as.matrix(df4))
p_mat4 <- cor4$r
M4 <-cor(df4, method = "spearman")

cor5 <- rcorr (as.matrix(df5))
p_mat5 <- cor5$r
M5 <-cor(df5, method = "spearman")

cor6 <- rcorr (as.matrix(df6))
p_mat6 <- cor6$r
M6 <-cor(df6, method = "spearman")



corrplot(M1, method="color", col=col(200),  
         type="upper", cl.cex = 1.5, #addCoef.col = "black", number.cex=1.5, 
         # Add coefficient of correlation
         tl.col="black", tl.srt=45,tl.cex = 1.5,
         #Text label color and rotation
         p.mat = p_mat1, sig.level = 0.05,
         # hide correlation coefficient on the principal diagonal
          diag=FALSE)

corrplot(M2, method="color", col=col(200),  
         type="upper",  cl.cex = 1.5,#addCoef.col = "black", number.cex=1.5, 
         tl.col="black", tl.srt=45,tl.cex = 1.5,
         p.mat = p_mat2, sig.level = 0.05,
          diag=FALSE)

         
corrplot(M3, method="color", col=col(200),  
         type="upper",  cl.cex = 1.5,#addCoef.col = "black", number.cex=1.5, 
         tl.col="black", tl.srt=45,tl.cex = 1.5,
         p.mat = p_mat3, sig.level = 0.05,
         diag=FALSE)   

corrplot(M4, method="color", col=col(200),  
         type="upper",  cl.cex = 1.5,#addCoef.col = "black", number.cex=1.5, 
         tl.col="black", tl.srt=45,tl.cex = 1.5,
         p.mat = p_mat4, sig.level = 0.05,
         diag=FALSE)  


corrplot(M5, method="color", col=col(200),  
         type="upper",  cl.cex = 1.5, #addCoef.col = "black", number.cex=1.5, 
         tl.col="black", tl.srt=45,tl.cex = 1.5,
         p.mat = p_mat5, sig.level = 0.05,
         diag=FALSE)  

corrplot(M6, method="color", col=col(200),  
         type="upper", cl.cex = 1.5, #addCoef.col = "black", number.cex=1.5, 
         tl.col="black", tl.srt=45, tl.cex = 1.5,
         p.mat = p_mat6, sig.level = 0.05,
         diag=FALSE)   

dev.off() #to create a file




