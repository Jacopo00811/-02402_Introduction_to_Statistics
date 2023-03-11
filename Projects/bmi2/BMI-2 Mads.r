
#####################################################################################################################################
### Indlæsning af datamaterialet

## Sæt working directory
setwd("~/Desktop")

## Indlæs data fra bmi2_data.csv
D <- read.table("bmi2_data.csv", header = TRUE, sep = ";")

## Tilføjer log-transformeret BMI til datasættet
D$logbmi <- log(D$bmi)

#####################################################################################################################################
### Sprøgsmål a) 

## Sætter graf-vindue til to grafer ved siden af hinanden
par(mfrow=c(1,2))

## Scatterplots af log-BMI mod de andre variabler
plot(D$logbmi, D$age, xlab = "Log(BMI)", ylab = "Alder", main = "Scatterplot af log(BMI) og alder")
plot(D$logbmi, D$fastfood, xlab = "Log(BMI)", ylab = "fastfood-forbrug", main = "Scatterplot af log(BMI) og fastfood-forbrug")

## Histogram og boxplot af log-BMI for at undersøge fordelingen
hist(D$logbmi, xlab = "Log-BMI", main = "Density histogram af log-BMI")
boxplot(D$logbmi,xlab = "Log-BMI", main = "Boxplot af log-BMI")

## Histogram og boxplot af alder for at undersøge fordelingen
hist(D$age, xlab ="Alder",  main = "Density histogram af alder")
boxplot(D$age, xlab = "Alder", main = "Boxplot af alder")

## Histogram og boxplot af fastfood-forbrug for at undersøge fordelingen
hist(D$fastfood, xlab = "Fastfood-forbrug", main = "Density histogram af fastfood-forbrug")
boxplot(D$fastfood, xlab = 'Fastfood-forbrug', main = "Density histogram af fastfood-forbrug")

## Udregning af stikprøve gennemsnit
mean(D$bmi)
mean(D$age)
mean(D$logbmi)
mean(D$fastfood)

## Udregning af stikprøve standardafvigelse
sd(D$bmi)
sd(D$age)
sd(D$logbmi)
sd(D$fastfood)

## Udregning af 25%- og 75%-fraktil
quantile(D$bmi)
quantile(D$age)
quantile(D$logbmi)
quantile(D$fastfood)

#####################################################################################################################################
### Spørgsmål b) 

## Deldatasæt med de første 840 observationer (til model)
D_model <- subset(D, id <= 840)

## Deldatasæt med de sidste 7 observationer (til validering)
D_test <- subset(D, id >= 841)

#####################################################################################################################################
### Spørgsmål c)

## Estimer multipel lineær regressionsmodel
fit <- lm(logbmi ~ age + fastfood, data = D_model)

## Vis estimerede parametre mm.
summary(fit)

#####################################################################################################################################
### Spørgsmål d)

## Sætter graf-vindue til 2x3 grafer
par(mfrow=c(2,3))

## Observationer mod fittede værdier
plot(fit$fitted.values, D_model$logbmi, xlab = "Fittede værdier", ylab = "log(BMI)", main = "Observationer mod fittede værdier")

## Residualer mod log-BMI
plot(D_model$logbmi, fit$residuals, xlab = "Log-BMI", ylab = "Residualer", main = "Residualer mod log-BMI")

## Residualer mod alder
plot(D_model$age, fit$residuals, xlab = "Alder", ylab = "Residualer", main = "Residualer mod alder")

## Residualer mod fastfood-forbrug
plot(D_model$fastfood, fit$residuals, xlab = "Fastfood-forbrug", ylab = "Residualer", main = "Residualer mod fastfood-forbrug")

## Residualer mod fittede værdier
plot(fit$fitted.values, fit$residuals, xlab = "Fittede værdier", ylab = "Residualer", main = "Residualer mod fittede værdier")

## Normal QQ-plot af residualerne
qqnorm(fit$residuals, ylab = "Residualer", xlab = "Z-scores", main = "Normal QQ-plot af residualerne")
qqline(fit$residuals)

#####################################################################################################################################
### Spørgsmål e)

## Udregning af t_(1-α/2) med 837 frihedsgrader (opgave c)
qt(0.975, 837)

## Konfidensintervaller for modellens koefficienter
confint(fit, level = 0.95)

#####################################################################################################################################
### Spørgsmål f)

## Udregning af p med teststørrelsen 3.5 og DF = 837
2*(1-pt(3.5, df=837))

#####################################################################################################################################
### Spørgsmål g)

## Definere slutmodel)
slut_model <- lm((D_test$logbmi ~ D_test$age + D_test$fastfood))

#####################################################################################################################################
### Spørgsmål h)

## Prædiktioner og 95% prædiktionsintervaller
pred <- predict(slut_model, newdata = D_test, interval = "prediction", level = 0.95)

## Observerede værdier sammen med prædiktioner
cbind(id = D_test$id, logbmi = D_test$logbmi, pred)

#####################################################################################################################################