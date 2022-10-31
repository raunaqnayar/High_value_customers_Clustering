library(dplyr)
library(cluster)

sale_info<-read.csv("Ecommerce.csv")

customer_sales<-sale_info%>%group_by(CustomerID)%>%summarise(total=sum(UnitPrice*Quantity))
customer_sales<-na.omit(customer_sales)
head(customer_sales)

#K-means

set.seed(101)

for(i in 2:12){
  temp.kmean<-kmeans(customer_sales$total,centers=i,nstart=10)
  cat('For ',i, ' clusters total SSE ',temp.kmean$tot.withinss,'\n')
  if(i==2){
    sse<-c(temp.kmean$tot.withinss)
  }
  else{
    sse<-c(sse,c(temp.kmean$tot.withinss))
  }
}

plot(2:12,sse,type='l')

K=3;
cust.kmean<-kmeans(customer_sales$total,centers=K,nstart=10)
c_sale_kmean<-customer_sales
clusplot(customer_sales[2],cust.kmean$cluster,color = T,shade=T,labels=0,lines=0)
c_sale_kmean<-cbind(c_sale_kmean,k_mean_cluster=cust.kmean$cluster)
View(c_sale_kmean)
View(customer_sales)
c_sale_kmean%>%filter(k_mean_cluster==2)
c_sale_kmean%>%filter(k_mean_cluster==3)
head(c_sale_kmean%>%filter(k_mean_cluster==1))
cust.kmean$centers

# Hierarchy
clus_h<-dist(customer_sales$total,'euclidian')
fitted_hier<-hclust(clus_h,method='ward')
c_sale_hier<-customer_sales

c_sale_hier<-cbind(c_sale_hier,hier_clus=cutree(fitted_hier,3))
View(c_sale_hier)

plot(fitted_hier)
head(c_sale_hier%>%filter(hier_clus==2))
c_sale_hier%>%filter(hier_clus==3)
head(c_sale_hier%>%filter(hier_clus==1))

#If the number of observations is loaded in one of the clusters, 
#break down that cluster further using the clustering algorithm. 
#[ hint: Here loaded means if any cluster has more number of data points as
#compared to other clusters then split that clusters by increasing the number of clusters
#and observe, compare the results with previous results.]

low_value_cluster<-(c_sale_kmean%>%filter(k_mean_cluster==1))[-3]
set.seed(101)

for(i in 2:12){
  temp.low_val.kmean<-kmeans(low_value_cluster$total,centers=i,nstart=10)
  cat('For ',i, ' clusters total SSE ',temp.low_val.kmean$tot.withinss,'\n')
  if(i==2){
    sse<-c(temp.low_val.kmean$tot.withinss)
  }
  else{
    sse<-c(sse,c(temp.low_val.kmean$tot.withinss))
  }
}

plot(2:12,sse,type='l')

# K=5

low_val_kmeans_group<-kmeans(low_value_cluster$total,centers=5,nstart=10)
low_value_cluster<-cbind(low_value_cluster,k_mean_cluster=low_val_kmeans_group$cluster)
View(low_value_cluster)
table(low_value_cluster$k_mean_cluster)
low_value_cluster%>%filter(k_mean_cluster==4)
low_value_cluster%>%filter(k_mean_cluster==3)
head(low_value_cluster%>%filter(k_mean_cluster==1))
head(low_value_cluster%>%filter(k_mean_cluster==2))
head(low_value_cluster%>%filter(k_mean_cluster==5))
low_val_kmeans_group$centers