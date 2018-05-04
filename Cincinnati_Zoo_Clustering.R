##TransFood <- read.csv('http://homepages.uc.edu/~maifg/DataMining/data/food_4_association.csv')
install.packages("gdata")
library(gdata)
Food_by_month <-read.xls("qry_Food_by_Month.xls", header = TRUE)
str(Food_by_month)
ncol(Food_by_month)
View(Food_by_month)

#colnames
colnames(Food_by_month)
#"NickName" "Oct..10"  "Nov..10"  "Dec..10"  "Jan..11"  "Feb..11"  "Mar..11"

#random sampling - 90% train, 10% test
#set.seed(10669218)
#index <- sample(x =nrow(TransFood), size = nrow(TransFood)*0.9)
#TransFood_train <- TransFood[index,] #train
#TransFood_test <- TransFood[-index,] #test

#Ignore the nickname column
Food_by_month_set <- Food_by_month[,2:7]
View(Food_by_month_set)

#scaling
scaled_set <- scale(Food_by_month_set)
View(scaled_set)

#fpc package
install.packages("fpc")
library(fpc)


#K-means cluster
#k=2 clusters
fit1 <- kmeans(scaled_set,2)
table(fit1$cluster)
plotcluster(scaled_set,fit1$cluster)

#k=3 clusters
fit2 <- kmeans(scaled_set,3)
table(fit2$cluster)
plotcluster(scaled_set,fit2$cluster)

#k=4 clusters
fit3 <- kmeans(scaled_set,4)
table(fit3$cluster)
plotcluster(scaled_set,fit3$cluster)


#k=5 clusters
fit4 <- kmeans(scaled_set,5)
table(fit4$cluster)
plotcluster(scaled_set,fit4$cluster)

#elements of 1st cluster in fit1 (k=2)
scaled_set[fit1$cluster == 1,]
aggregate(scaled_set, by = list(fit1$cluster), FUN = mean)
#gives summary stats group wise 
fit1$centers #same working as above

#determine the number of clusters
#within group sse
wss <- (nrow(scaled_set)-1)*sum(apply(scaled_set, 2, var))
#initialises wss
for (i in 2:12) wss[i] <- sum(kmeans(scaled_set,centers=i)$withinss)
plot(1:12, wss, type="b", xlab="Number of Clusters",ylab="Within groups sum of squares")


#prediction strength
prediction.strength(scaled_set, Gmin=2, Gmax=15, M=10,cutoff=0.8)
#arbitary cutoff- 0.8

#silhoutte width
d = dist(scaled_set, method = "euclidean")
result = matrix(nrow = 14, ncol = 3)
for (i in 2:15){
  cluster_result = kmeans(scaled_set, i)
  clusterstat=cluster.stats(d, cluster_result$cluster)
  result[i-1,1]=i
  result[i-1,2]=clusterstat$avg.silwidth
  result[i-1,3]=clusterstat$dunn
}

#silhouette width
plot(result[,c(1,2)], type="l", ylab = 'silhouette width', xlab = 'number of clusters')

#dunn index
plot(result[,c(1,3)], type="l", ylab = 'dunn index', xlab = 'number of clusters')


#Hierarchial Clustering
Food_by_month_dist <- dist(scaled_set)
Food_by_month_hclust <- hclust(Food_by_month_dist, method = "ward.D")
plot(Food_by_month_hclust)

#cut at 2 cluster level
Food_by_month_2clust <- cutree(Food_by_month_hclust,k =2)
rect.hclust(tree = Food_by_month_hclust,k=2 ) #shows the graph

#cut at 3 cluster level
Food_by_month_3clust <- cutree(Food_by_month_hclust,k =3)
rect.hclust(tree = Food_by_month_hclust,k=3 ) #shows the graph

#cut at 4 cluster level
Food_by_month_4clust <- cutree(Food_by_month_hclust,k =4)
rect.hclust(tree = Food_by_month_hclust,k=4 ) #shows the graph

#cut at 5 cluster level
Food_by_month_5clust <- cutree(Food_by_month_hclust,k =5)
rect.hclust(tree = Food_by_month_hclust,k=5 ) #shows the graph

#items in the 3rd group
scaled_set[Food_by_month_3clust==3,]

#plot
plotcluster(scaled_set,Food_by_month_3clust)




