RegresionLinealFit <- function(X,Y) {
    # Creamos una matriz de 1
    unos <- matrix(data = 1, nrow = 50, ncol = 1) #El numero nrow depen del dataset usado
    
    # Unimos la matrix X con la matriz de 1
    X <- cbind(unos, X)
    
    X_T <- t(X) # Traspuesta de la matrix X
    B_hat <- solve(X_T %*% X) %*% X_T %*% Y # Aplicamos la funcion 
    
  return(B_hat)
}

library(dplyr)
library(ggplot2)

cars %>%
        select(speed, dist) %>%
        ggplot(aes (x = speed, y = dist))+
            geom_point()

X_list <- cars %>%
        select(speed)

Y_list <- cars %>%
        select(dist)

X_matrix <- matrix(unlist(X_list), ncol = 1, byrow = TRUE)

Y_matrix <- matrix(unlist(Y_list), ncol = 1, byrow = TRUE)

modelo <- RegresionLinealFit(X_matrix, Y_matrix)

modelo_df = as.data.frame(t(modelo), stringsAsFactors = FALSE)

b <- modelo_df %>%
            select(V1)

b <- as.numeric(b)

m <- modelo_df %>%
            select(V2)

m <- as.numeric(x)


#punto_0 <- modelo_df[1] + modelo_df[2] * 2.5

#punto_1 <- modelo_df[1] + modelo_df[2] * 25

df <- as.data.frame(t(p))

ggplot() +
    geom_point(data = cars, aes (x = speed, y = dist)) +
    geom_line(data = NULL, aes(x= c(5,25) , y=c((5 * m) + b, (25 * m) + b)),color='red')

ggplot(data = df, aes(x= c(0, 0 * x + b) , y=c(25, 25 * x + b))) + geom_line()

datamatrix = as.data.frame(t(p), stringsAsFactors = FALSE)

class(datamatrix)

datamatrix[1]

ggplot(DF,aes(Day, Measure)) + 
    geom_point() + 
    stat_summary(fun.y = "mean", geom="line")


geom_line(aes((modelo[1] + (modelo[2] * 2.5)) ,(modelo[1] + modelo[2] * 25),color='red'))
