###### Solution a CLustering problem, using the Hierarchical Clustering technique #######
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

# Se usa el método del dendograma para el número óptimo de Clusters
dendrogram = hclust(d = dist(dataset, method = 'euclidean'), method = 'ward.D')
plot(dendrogram,
     main = paste('Dendrogram'),
     xlab = 'Ligas',
     ylab = 'Euclidean distances')

# Ajustar el Modelo
hc = hclust(d = dist(dataset, method = 'euclidean'), method = 'ward.D')
y_hc = cutree(hc, 4)

# Visualizar los resultados
library(cluster)
clusplot(dataset,
         y_hc,
         lines = 0,
         shade = TRUE,
         color = TRUE,
         labels= 2,
         plotchar = FALSE,
         span = TRUE,
         main = paste('Clusters de Ligas'),
         xlab = 'Win',
         ylab = 'Ligas')
