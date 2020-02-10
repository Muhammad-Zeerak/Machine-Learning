###### Solution a Clustering problem, using Hierarchical Clustering technique #######
#### By: JUAN SEBASTIÁN PELÁEZ VILLA ####
## This Script was based in the method created in the course Machine Learning A-Z

# Importar las bibliotecas
import numpy as np
import matplotlib.pyplot as plt
import pandas as pd

# Importar los datos the dataset
dataset = pd.read_csv('/home/sebas/Escritorio/My_MachineLearnig_Algorithms/Clustering/DataSet.txt')
X = dataset.iloc[:, 0:2].values

# Método del Dendograma para el número óptimo de Clusters
import scipy.cluster.hierarchy as sch
dendrogram = sch.dendrogram(sch.linkage(X, method = 'ward'))
plt.title('Dendrogram')
plt.xlabel('Ligas')
plt.ylabel('Euclidean distances')
plt.show()

# Se ajusta el modelo
from sklearn.cluster import AgglomerativeClustering
hc = AgglomerativeClustering(n_clusters = 4, affinity = 'euclidean', linkage = 'ward')
y_hc = hc.fit_predict(X)

# Visualising the clusters
plt.scatter(X[y_hc == 0, 0], X[y_hc == 0, 1], s = 100, c = 'red', label = 'Cluster 1')
plt.scatter(X[y_hc == 1, 0], X[y_hc == 1, 1], s = 100, c = 'blue', label = 'Cluster 2')
plt.scatter(X[y_hc == 2, 0], X[y_hc == 2, 1], s = 100, c = 'green', label = 'Cluster 3')
plt.scatter(X[y_hc == 3, 0], X[y_hc == 3, 1], s = 100, c = 'cyan', label = 'Cluster 4')
plt.title('Clusters de Ligas')
plt.xlabel('Win')
plt.ylabel('Lose')
plt.legend()
plt.show()