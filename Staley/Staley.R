library(readxl)
library(mlbench)
library(caret)
library(leaps)
library(olsrr)
library(tidyverse)
library(caret)
library(MASS)
library(Hmisc)

read_xlsx("Palouse.xlsx", sheet = "20cm")[-c(1:10,24:25)]
head(df)
df1 <- read_xlsx("Staley.xlsx", sheet = "150cm")[11:21]
df2 <- read_xlsx("Staley.xlsx", sheet = "150cm")[11:22]
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
#saving the output
capture.output(summary(res), file = "naff10.txt", append = TRUE)
capture.output(coeff, file = "naff20.txt", append = TRUE)
capture.output(varlasso, file = "naff30.txt", append = TRUE)
capture.output(summary(res), file = "naff60.txt", append = TRUE)
capture.output(coeff, file = "naff90.txt", append = TRUE)
capture.output(varlasso, file = "naff150.txt", append = TRUE)
# detecting multicollinearity
dfcor<- subset(df, select = -c(pH)) # subset from oroginal data
multico <- cor(dfcor)
print(multico)
corrplot(multico)

M <-cor(df, method = "spearman")
col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))
corrplot(M, method="color", col=col(200),  
         type="upper", order="hclust", 
         addCoef.col = "black", number.cex=0.7, # Add coefficient of correlation
         tl.col="black", tl.srt=45,
         #Text label color and rotation
         p.mat = p_mat, sig.level = 0.05,
         # hide correlation coefficient on the principal diagonal
         diag=FALSE)
dev.off()


# Mark the insignificant coefficients according to the specified p-value significance level
cor_5 <- rcorr (as.matrix(df))
M <- cor_5$r
M

p_mat <- cor_5$P

M <-cor(df, method = "spearman")
col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))
corrplot(M, method="color", col=col(200),  
         type="upper", order="hclust", 
         addCoef.col = "black", number.cex=0.7, # Add coefficient of correlation
         tl.col="black", tl.srt=45,
         #Text label color and rotation
         p.mat = p_mat, sig.level = 0.05,
         # hide correlation coefficient on the principal diagonal
         diag=FALSE)
dev.off()

