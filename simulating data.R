library(fungible)
library(pheatmap)



c <- monte(
  seed = 123,
  nvar = 4,
  nclus = 5,
  clus.size = c(3, 12, 8, 5, 4),
  eta2 = c(0.7, 0.87, 0.56, 0.9, 0.83),
  cor.list = NULL,
  random.cor = FALSE,
  skew.list = NULL,
  kurt.list = NULL,
  secor = NULL,
  compactness = NULL,
  sortMeans = TRUE
)

d <- as.data.frame(c$data)

names(d)


############
# Visualize clusters
annotation_col=data.frame(
  cluster=substr(colnames(d),1,3)
)

annotation_col

pheatmap(d,show_rownames=FALSE,show_colnames=FALSE,
         annotation_col = annotation_col,
         scale = "none",clustering_method="ward.D2",
         clustering_distance_cols="euclidean") # needs fixing


set.seed(500)
kclu=kmeans(d, centers=5)

kcluster=kclu$cluster
d_kclu <- cbind(d,kcluster)
table(as.factor(d$id), as.factor(kclu$cluster))

library(ggplot2)
ggplot(d_kclu,aes(id,kcluster))+
  geom_count(aes(colour=as.factor(id)))

plot(d_kclu$id, d_kclu$kcluster)

#K-medoids
d <- as.data.frame(c$data)
kmclu=cluster::pam(d,k=5) 
kmcluster=kmclu$cluster
d_kclu <- cbind(d,kcluster,kmcluster)
table(as.factor(d$id), as.factor(kmclu$cluster))
