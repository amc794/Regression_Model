data(mtcars)
str(mtcars)

mtcars$carname <- row.names(mtcars)
mtcars2$carname <- row.names(mtcars2)
mtcars$cyl.f <- as.factor(mtcars$cyl)
mtcars$gear.f <- as.factor(mtcars$gear)
mtcars$carb.f <- as.factor(mtcars$carb)

par(mfrow = c(2,2))
library(ggvis)
library(dplyr)
library(ggplot2)

mtcars %>%
  group_by(factor(am)) %>%
  ggvis(~mpg, ~wt, fill = ~am) %>% 
  layer_points() %>%
  layer_smooths()

mtcars%>%
  ggvis(~qsec, ~mpg) %>%
  layer_points() %>%
  layer_smooths()


model1 <- lm(mpg ~ am, data=mtcars)
summary(model1)

model2 <- lm(mpg~am*wt, data=mtcars)
summary(model2)

model3 <- lm(mpg~am*wt+qsec, data=mtcars)
summary(model3)

           
anova(model1,model2,model3)

plot(model3)


sort(abs(round(dfbetas(model3)[,2],3)))
     
sort(abs(round(hatvalues(model3),3)))  

sort(abs(round(dffits(model3),3)))  


mtcars2<-filter(mtcars, !mtcars$carname %in% c("Chrysler Imperial","Maserati Bora", "Fiat 128"))

model4 <- lm(mpg~am*wt+qsec, data=mtcars2)
summary(model4)

plot(model4)


sort(abs(round(dfbetas(model4)[,2],3)))

sort(abs(round(hatvalues(model4),3)))  

sort(abs(round(dffits(model4),3)))  


mtcars3<-filter(mtcars, !mtcars$carname %in% c("Chrysler Imperial","Maserati Bora"))

model5 <- lm(mpg~am*wt+qsec, data=mtcars3)
summary(model5)

plot(model5)


sort(abs(round(dfbetas(model5)[,2],3)))

sort(abs(round(hatvalues(model5),3)))  

sort(abs(round(dffits(model5),3))) 




##AIC stepwise
summary(model0 <- lm(mpg ~.-carname, data=mtcars))
model_choice <- step(model0)
summary(model_choice)
model_choice$anova

summary(model1 <- lm(mpg ~.-carname, data=mtcars2))
model_choice1 <- step(model1)
summary(model_choice1)
model_choice1$anova


library(ggvis)
library(dplyr)
g = mtcars %>%
  group_by(factor(cyl)) %>%
  ggvis(~mpg, ~wt, fill = ~cyl) %>% 
  layer_points() %>%
  layer_smooths()

mtcars %>% 
  ggvis(~mpg) %>% 
  layer_histograms(width = 3, fill:="red") %>%
  add_axis("x", title="Miles Per Gallon")

str(mtcars)
mtcars %>%
  group_by(factor(am)) %>%
  ggvis(~mpg, ~wt, fill = ~am) %>% 
  layer_points() %>%
  layer_smooths()

mtcars%>%
  ggvis(~qsec, ~mpg) %>%
  layer_points() %>%
  layer_smooths()
hist(mtcars$mpg)
hist(mtcars$qsec)
hist(log(mtcars$wt))

qplot(wt, mpg, data=mtcars, colour=factor(cyl))

qplot(factor(am), wt, data = mtcars, geom=c("boxplot"))

qplot(qsec, mpg, data=mtcars, colour=factor(cyl))

shapiro.test(mtcars$mpg)
shapiro.test(mtcars$wt)
shapiro.test(mtcars$qsec)
qplot(data=mtcars, y=mpg, x=wt, color=factor(cyl))





