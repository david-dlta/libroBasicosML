RegresionLinealFit <- function(X,Y) {
    # Creamos una matriz de 1
    unos <- matrix(data = 1, nrow = 50, ncol = 1) #El numero nrow depen del dataset usado
    
    # Unimos la matrix X con la matriz de 1
    X <- cbind(unos, X)
    
    X_T <- t(X) # Traspuesta de la matrix X
    B_hat <- solve(X_T %*% X) %*% X_T %*% Y # Aplicamos la funcion 
    
  return(B_hat)
}

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

punto_0 <- modelo_df %>%
            select(V1)

punto_0 <- as.numeric(punto_0)

punto_1 <- modelo_df %>%
            select(V2)

punto_1 <- as.numeric(punto_1)


#punto_0 <- modelo_df[1] + modelo_df[2] * 2.5

#punto_1 <- modelo_df[1] + modelo_df[2] * 25

modelo_df

typeof(punto_0)

p <- c(punto_0, punto_1)

df <- as.data.frame(t(p))

cars %>%
        select(speed, dist) %>%
        ggplot() +
            geom_point(aes (x = speed, y = dist)) +
            geom_line(data = df, color='red', aes(x= V1 + V2 * 2.5 , y=V1 + V2 * 25, group = 1))

ggplot() + geom_line(color='red', aes(x= punto_0 + punto_1 * 2.5 , y=punto_0 + punto_1 * 25, group = 1))

ggplot(data = df, aes(x= V1 + V2 * 2.5 , y=V1 + V2 * 25, group = 1)) + geom_line()

glimpse(df)

datamatrix = as.data.frame(t(p), stringsAsFactors = FALSE)

class(datamatrix)

datamatrix[1]

ggplot(DF,aes(Day, Measure)) + 
    geom_point() + 
    stat_summary(fun.y = "mean", geom="line")


geom_line(aes((modelo[1] + (modelo[2] * 2.5)) ,(modelo[1] + modelo[2] * 25),color='red'))
