# Pre Process Data --------------------------------------------------------
#Remove NA Rows,convert all columns in numeric and remove not numeric columns
#If Normalize == TRUE, then process the Min-Max Normalization
preProcessData<-function(df_dataset,normalize){
  df_dataset<-na.omit(df_dataset)
  df_dataset<-apply(df_dataset,2,as.numeric)
  df_dataset<-df_dataset[,apply(df_dataset, 2, function(x) !any(is.na(x)))]
  if(normalize){
    df_dataset<-apply(df_dataset,2,function(x){(x-min(x))/(max(x)-min(x))})
  }
  return(df_dataset)
}



# KMEANS ------------------------------------------------------------------

#Perform a KMEANS clusterization
kmeans.output<-function(df_dataset_original,k,normalize){
  df_dataset<-preProcessData(df_dataset_original,normalize)
  model_km<-kmeans(df_dataset
                   ,centers=k
  )
  df_output<-data.frame(df_dataset_original,Cluster=model_km$cluster)
  list_output<-list(df_output,df_dataset,model_km$cluster,model_km)
  names(list_output)<-c("output","input","labels","model")
  return(list_output)
}



#Plot KMEANS results cluster, if columns>2, then plot Principal Components
kmeans.plot<-function(list_output){
  if(ncol(list_output$input)>2){
    list_output$input<-prcomp(list_output$input)$x[,1:2]
  }
  plot(list_output$input,col=list_output$labels,pch=19)
  return(invisible())
}

#Summary
kmeans.summary<-function(list_output){
  return(list_output$model)
}



# HCLUST ------------------------------------------------------------------

#Perform a AGNES clusterization
hclust.output<-function(df_dataset_original,linkagefun,distance,k,normalize){
  df_dataset<-preProcessData(df_dataset_original,normalize)
  model_hclust<-hclust(dist(df_dataset,method = distance),method = linkagefun)
  df_output<-data.frame(df_dataset_original,Cluster=cutree(model_hclust,k=k))
  list_output<-list(df_output,model_hclust)
  names(list_output)<-c("output","model")
  return(list_output)
}


#Plot Dendrogram
hclust.plot<-function(list_output){
  plot(list_output$model)
  return(invisible())
}


#Summary
hclust.summary<-function(list_output){
  return(summary(list_output$model))
}


# DBSCAN ------------------------------------------------------------------

#Perform a DBSCAN clusterization
dbscan.output<-function(df_dataset_original,eps,minpts,normalize){
  df_dataset<-preProcessData(df_dataset_original,normalize)
  model_dbscan<-dbscan(df_dataset,eps = eps,minPts = minpts)
  df_output<-data.frame(df_dataset_original,df_dataset,Cluster=model_dbscan$cluster)
  list_output<-list(df_output,df_dataset,model_dbscan$cluster,model_dbscan)
  names(list_output)<-c("output","input","labels","model")
  return(list_output)
}


#Plot DBSCAN results cluster, if columns>2, then plot Principal Components
dbscan.plot<-function(list_output){
  if(ncol(list_output$input)>2){
    list_output$input<-prcomp(list_output$input)$x[,1:2]
  }
  
  #Index Array where noise is present
  a_indnoise<-which(list_output$output$Cluster==0)
  
  plot(list_output$input,col=list_output$labels,pch=19)
  points(list_output$input[a_indnoise,],col="grey",pch=4,cex=1.5)
  return(invisible())
}

#Summary
dbscan.summary<-function(list_output){
  return(list_output$model)
}



# EMMG --------------------------------------------------------------------

#Perform EMMG clusterization
#Try simulations with an increment of +1 about gaussian mixture and then select simulation where log-likelihood is minimum
emmg.output<-function(df_dataset_original,n,normalize){
  df_dataset<-preProcessData(df_dataset_original,normalize)
  model_emmg<-mclustBIC(df_dataset,G=2:n)
  model_emmg_best<-Mclust(df_dataset,x=model_emmg)
  df_output<-data.frame(df_dataset_original,df_dataset,Cluster=model_emmg_best$classification)
  list_output<-list(df_output,df_dataset,model_emmg_best$classification,model_emmg_best)
  names(list_output)<-c("output","input","labels","model")
  return(list_output)
}


#Plot EMMG results cluster, if columns>2, then plot Principal Components
emmg.plot<-function(list_output){
  if(ncol(list_output$input)>2){
    list_output$input<-prcomp(list_output$input)$x[,1:2]
  }
  plot(list_output$input,col=list_output$labels,pch=19)
  return(invisible())
}

#Summary
emmg.summary<-function(list_output){
  return(summary(list_output$model))
}

