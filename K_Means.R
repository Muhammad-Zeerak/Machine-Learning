###### Solution a CLustering problem, using the KMeans technique #######
#### By: JUAN SEBASTIÁN PELÁEZ VILLA ####

### Function that allows to load the data, but first take the first five row and identify ###
### The classes of the features to improve the load in cases that the dataset are big ###
upload_data <- function(){
  
  ruta_datos <- file.choose()
  inicial <- read.table(ruta_datos, nrows=5,header=TRUE,sep=",")
  clases <- sapply(inicial,class)
  data_set <- read.table(ruta_datos, header=TRUE,sep=",",fileEncoding = "latin1",colClasses = clases)
  
}

#Se cargan los datos
dataset <- upload_data()

# Se realiza el Método del Codo "Elbow" para verificar el número adecuado de Clusters
set.seed(6)
wcss = vector()
for (i in 1:10) wcss[i] = sum(kmeans(dataset, i)$withinss)
plot(1:10,
     wcss,
     type = 'b',
     main = paste('The Elbow Method'),
     xlab = 'Número de clusters',
     ylab = 'WCSS')

# Ajustar el Modelo
set.seed(29)
kmeans = kmeans(x = dataset, centers = 4)
y_kmeans = kmeans$cluster

# Visualising the clusters
library(cluster)
clusplot(dataset,
         y_kmeans,
         lines = 0,
         shade = TRUE,
         color = TRUE,
         labels = 2,
         plotchar = FALSE,
         span = TRUE,
         main = paste('Clusters de Ligas'),
         xlab = 'Win',
         ylab = 'Lose')