q1 <- function() {
    library(datasets)
    data(iris)
    
    tapply(iris$Sepal.Length, iris$Species, mean)
}

q3 <- function() {
    library(datasets)
    data(mtcars)
    
    sapply(split(mtcars$mpg, mtcars$cyl), mean)
}

q4 <- function() {
    library(datasets)
    data(mtcars)
    
    #what is the absolute difference between the average horsepower of 4-cylinder cars and the average horsepower of 8-cylinder cars?
    
    means <- tapply(mtcars$hp, mtcars$cyl, mean)
    abs(means['4'] - means['8'])
}