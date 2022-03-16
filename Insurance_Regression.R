## Insurance cost analysisis via regression models

## Librarys
library(readxl)
library(lmtest)
library(readr)
library(palmerpenguins)
library(MASS)
library(car)
library(lmtest)
library(ggfortify)
library(ggplot2)
library(lawstat)
library(lmtest)
library(psych)
library(dplyr)
library(olsrr)
library(reactable)
library(writexl)
library(corrplot)
## Importar dataset

insurance <- read_xlsx("insurance3.xlsx")
View(insurance)

## Data set 

dfi <- subset(insurance, select= c(charges, age, bmi, children, smoke, region1))



## Model

modelo_seguros2 <- lm(formula = charges ~ ., data = dfi)

summary(modelo_seguros2)

## Correlation

corPlot(dfi, cex = 1.2, main = "Matriz de correlación")

## Lineality

ggplot(data=dfi, aes(x=age, y=charges)) +
  geom_line(color="red")+
  ggtitle("Age vs Charges")+
  geom_point()

ggplot(data=dfi, aes(x=smoke, y=charges)) +
  geom_line(color="blue")+
  ggtitle("Smoke vs Charges")+
  geom_point()

ggplot(data=dfi, aes(x=bmi, y=charges)) +
  geom_line(color="green")+
  ggtitle("BMI vs Charges")+
  geom_point()

## QQ plot 

residuos <- summary(modelo_seguros2)$residuals

ggpubr::ggdensity(residuos,  fill = "lightgray", add = "mean",  xlab = "Residuos de la RLM")

ggpubr::ggqqplot(residuos)

## Hiphhotesis tests

shapiro.test(residuos)

nortest::ad.test(residuos)

nortest::cvm.test(residuos)

nortest::pearson.test(residuos)

## Combine residuals with the data frame

Seguros <- bind_cols(dfi,residuos)
dim(Seguros)

# Graphical tests

ggplot(data = Seguros, aes(y = residuos, x = charges)) + geom_point(col = 'blue') + geom_abline(slope = 0)

plot(Seguros, 1)
plot(Seguros, 3)

## VIF

car::vif(modelo_seguros2)

## Influence values

n <- nrow(dfi)
k <- length(modelo_seguros2$coefficients)-1
cv <- 2*sqrt(k/n)

plot(dffits(modelo_seguros2), 
     ylab = "Standardized dfFits", xlab = "Index", 
     main = paste("Standardized DfFits, \n critical value = 2*sqrt(k/n) = +/-", round(cv,1)))


#Critical Value horizontal lines

abline(h = cv, lty = 2)
abline(h = -cv, lty = 2)

plot(modelo_seguros2, 4)
plot(modelo_seguros2, 5)


ols_plot_resid_lev(modelo_seguros2)

## Extreme values

boxplot(dfi$charges)

boxplot(dfi$charges, plot=TRUE)$out

## Save nulls

outliers <- boxplot(dfi$charges, plot=FALSE)$out
print(outliers)

dfi[which(dfi$charges %in% outliers),]

## Delete them

dfi <- dfi[-which(dfi$charges %in% outliers),]

## Boxplot 2

boxplot(dfi$charges)

## Boxplot 2

modelo_seguros3 <- lm(formula = charges ~ ., data = dfi)

residuos <- summary(modelo_seguros3)$residuals

boxplot(residuos)

## Modify residuals to DF

residuos <- as.data.frame(residuos)
dim(residuos)

## Extrreme values 2

Seguros[which(Seguros$residuos %in% outliers),]
View(Seguros)

boxplot(Seguros$...7, main = "Residuos")

## Charges transformation

plot(charges ~ ., data = dfi, col = "grey", pch = 20, cex = 1.5)

hist(dfi$charges)

modelo_seguros4= lm(charges ~., data = dfi)
summary(modelo_seguros4)

plot(charges ~., data = dfi, col = "grey", pch = 20, cex = 1.5,
     main = "Charges by age")
abline(modelo_seguros4, col = "red", lwd = 2)

## QQ plot 

qqnorm(resid(modelo_seguros4), main = "Normal Q-Q Plot", col = "darkgrey")

qqline(resid(modelo_seguros4), col = "dodgerblue", lwd = 2)

## Box cox

charges_model = lm(charges ~ ., data = dfi)

boxcox(charges_model, plotit = TRUE)

## Heteroscedasticity tests

plot(fitted(modelo_seguros4), resid(modelo_seguros4), xlab='Fitted Values', ylab='Residuals')

##  studentized Breusch-Pagan test

bptest(modelo_seguros4)

## New Model

#Let's define the weights
wt <- 1 / lm(abs(modelo_seguros4$residuals) ~ modelo_seguros4$fitted.values)$fitted.values^2

#Perform weighted least squares regression
wls_model <- lm(charges ~., data = dfi, weights=wt)

#View summary of model
summary(wls_model)



