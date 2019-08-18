

# Kmeans ------------------------------------------------------------------

kmeans.output<-function(df_dataset_original,k){

#PreProcess Data
df_dataset_original<-na.omit(df_dataset_original)
df_dataset_original<-apply(df_dataset_original,2,as.numeric)
df_dataset<-df_dataset_original[,apply(df_dataset_original, 2, function(x) !any(is.na(x)))]


#Se le colonne sono piu' di due, uso le componenti principali, altrimenti no
if(ncol(df_dataset)>2){
 mat_dataset<-prcomp(df_dataset)$x[,1:2]
}else{
 mat_dataset<-df_dataset
}

model_km<-kmeans(mat_dataset
                 ,centers=k
                 )
df_output<-data.frame(df_dataset_original,Cluster=model_km$cluster)
list_output<-list(df_output,mat_dataset,model_km$cluster,model_km$centers)
names(list_output)<-c("output","input","labels","centers")
return(list_output)
}

kmeans.plot<-function(list_output){
plot(list_output$input,col=list_output$labels,pch=19)
points(list_output$centers,col=1:5,pch=8,cex=3)
return(invisible())
}



# HCLUST ------------------------------------------------------------------

hclust.output<-function(df_dataset_original,linkagefun,distance,k){
  
  #PreProcess Data
  df_dataset_original<-na.omit(df_dataset_original)
  df_dataset_original<-apply(df_dataset_original,2,as.numeric)
  df_dataset<-df_dataset_original[,apply(df_dataset_original, 2, function(x) !any(is.na(x)))]
  
  
  model_hclust<-hclust(dist(df_dataset,method = distance),method = linkagefun)
  df_output<-data.frame(df_dataset_original,Cluster=cutree(model_hclust,k=k))
  list_output<-list(df_output,model_hclust)
  names(list_output)<-c("output","model")
  return(list_output)
}

hclust.plot<-function(list_output){
  plot(list_output$model)
  return(invisible())
}
