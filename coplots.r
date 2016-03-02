data(mtcars)
str(mtcars)
head(mtcars,10)

require(graphics)
pairs(mtcars, main = "mtcars data")
coplot(mpg ~ disp | as.factor(cyl), data = mtcars,
       panel = panel.smooth, rows = 1)

library(car) 
scatterplot(mpg ~ wt | cyl, data=mtcars, 
            xlab="Weight of Car", ylab="Miles Per Gallon", 
            main="Enhanced Scatter Plot", 
            labels=row.names(mtcars)) 

Simple Scatterplot
attach(mtcars)
plot(wt, mpg, main="Scatterplot Example", 
     xlab="Car Weight ", ylab="Miles Per Gallon ", pch=19) 

# Add fit lines
abline(lm(mpg~wt), col="red") # regression line (y~x) 
lines(lowess(wt,mpg), col="blue") # lowess line (x,y) 

# Plot #2: same as above, but add loess smoother in lower and correlation in upper
data(iris)
pairs(~Sepal.Length+Sepal.Width+Petal.Length+Petal.Width, data=iris,
      lower.panel=panel.smooth, upper.panel=panel.cor,
      pch=20, main="Iris Scatterplot Matrix")


##Finally, you can produce a similar plot using ggplot2, with the diagonal showing the kernel density.
# Plot #3: similar plot using ggplot2
# install.packages("ggplot2") ## uncomment to install ggplot2
library(ggplot2)
plotmatrix(with(iris, data.frame(Sepal.Length, Sepal.Width, Petal.Length, Petal.Width)))


