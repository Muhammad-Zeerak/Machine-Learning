###### Solution to a clasification problem, using the K-NN technique #######
#### By: JUAN SEBASTIÁN PELÁEZ VILLA ####
## For the 

### Function that allows to load the data, but first take the first five row and identify ###
### The classes of the features to improve the load in cases that the dataset are big ###
upload_data <- function(){
  
  ruta_datos <- file.choose()
  inicial <- read.table(ruta_datos, nrows=5,header=TRUE,sep=",")
  clases <- sapply(inicial,class)
  data_set <- read.table(ruta_datos, header=TRUE,sep=",",fileEncoding = "latin1",colClasses = clases)
  
}

### This function allows that the every centroid was created in any place.
inicializarCentroides <- function(n){
  return ( matrix(as.integer(runif(n*2, min=0, max=500)),nrow = n,ncol = 2))
}


### This function verify which centroid are close to each value of dataset  
close_centroid <- function(X,centroids,K){
  n <- length(X[,1]) #number of rows
  m <- length(X[1,]) #number of columns
  
  #Agrego cada uno de los centroides en las coordenadas
  all_coords <- rbind(X,centroids)

  #Utilizo el método propio de R para calcular la distancia euclidiana a cada punto
  distancias <- as.matrix(dist(all_coords,method="euclidean"))

  #Solo me interesan los ultimos valores, los que corresponden a los centroides
  l_distancias <- tail(distancias,n=K)
  
  #Como es una matriz simetrica no me interesa los valores después de cero
  l_distancias <- l_distancias[,1:(length(distancias[1,])-K)]
  
  #Obtengo la distancia al centroide más cercano
  distancia_minima <- as.matrix(apply(l_distancias,2,min))
  centroide_cercano <- matrix(0,nrow=n,ncol=1)
  
  #Verifico cual valor es el que tiene el valor más cercano
  for(i in 1:n){
    j <- 1
    while(l_distancias[[j,i]] != distancia_minima[[i,1]]){
      j <- j+1
    }
    centroide_cercano[[i,1]] <- j
  }

	return (centroide_cercano)
}

#This function allow to find what will be the new position of the centroid
compute_centroids <- function(X,centroide_cercano,K){
  n <- length(X[,1]) #number of rows
  m <- length(X[1,]) #number of columns
  centroids <- matrix(0,nrow=K,ncol=m)
  
  #Para cada centroide
  for(i in 1:K){
    sum <- matrix(0,nrow=1,ncol=m)
    numero_cercanos <- 0
    
    #En cada uno de los valores que lo tienen como centroide más cercano
    for(j in 1:n){
      if(i == centroide_cercano[j]){
        sum <- sum + X[j,]
        numero_cercanos <- numero_cercanos + 1
      }
    }
    
    centroids[i,] <- sum/numero_cercanos
  }

  return (centroids)
}

#This function only is to paint the solution
pintar_resultados <- function(X,centroide_cercano){
  n <- length(X[,1]) #Número de filas
  m <- length(X[1,]) #Número de columnas
  
  plot(X,pch =16,cex=1,xlab="Victorias",ylab="Derrotas",type="n")

  for( i in 1:n){
  	color_indice <- centroide_cercano[[i,1]]
  	color_punto <- colores[color_indice]
  	points(X[[i,1]],X[[i,2]],pch = 16,cex=1, col = color_punto)
  }
}

#Se cargan los datos
data_set <- upload_data()

#Se obtiene el tamaño de los datos, así como el número de variables
length_data_set <- length(data_set)
number_features <- as.integer(length_data_set-1)

#Número de centroides
K <- 5 

#Número de iteraciones 
iteraciones <- 10

#Colores para identificar cada centroide
colores <- c(rainbow(K))

#Se cargan todos los datos
X <- data.matrix(data_set[1:length_data_set])

#Se inicializan los centroides en cero
centroids <- matrix(0,nrow = K, ncol = 2)

#Se aleatorizan sus posiciones
centroids <- inicializarCentroides(K)
centroids

plot(X[,1],X[,2],pch =16,cex=1,xlab="Victorias",ylab="Derrotas")
#points(centroids,pch = 15,cex=2, col = colores)

#Se ejecuta el algoritmo K-NN
for (i in 1:iteraciones){
  
  centroide_cercano <-  close_centroid(X,centroids,K)
  centroids <- compute_centroids(X,centroide_cercano,K)
  
}

#Se pintael resultado y los centroides
pintar_resultados(X,centroide_cercano)
points(centroids,pch = 15,cex=2, col = colores)
