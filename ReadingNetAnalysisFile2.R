FilePath<-"H:/GHCSV/Pop/NetAnalyis/Py 31-100/Comem/"
logFileName<-"logSB.csv"
saveFileName<-"Report.csv"
logFile<-paste(FilePath,logFileName,sep="")
logItems<-read.csv(logFile, header=TRUE)


#frame creation
NetDf <-NULL
#rm(NetDf)
NetDf <- data.frame(PrjId=integer(),
                 #Comment=character(),   
                 Nodes=integer(),
                 Edges=integer(),
                 ClusCoef=double(),
                 Dens=double(),
                 Dia=double(),
                 AvgDegAll=double(),
                 AvgDegIn=double(),
                 AvgDegOut=double(),
                 Distance=double(),
                 DegCent=double(),
                 ColCent=double(),
                 BetwCent=double(),
                 EigenCent=double(),
                 ComModul=double(),
                 Assort=double(),
                 Coh=double(),
                 Adh=double(),
                 Recp=double(),
                 stringsAsFactors=FALSE)


library(igraph)

for (row in 1:nrow(logItems)) {
  
  nodeFile<-logItems$Node[row]
  edgeFile<-logItems$Edge[row]
  CommentFile<-logItems$Comment[row]
  if(is.na( CommentFile))
  {
    CommentFile<-''
  }
  
  node<-read.csv(paste(FilePath,nodeFile,sep=""), header=TRUE)
  edge<-read.csv(paste(FilePath,edgeFile,sep=""), header=TRUE)
  
  net <- graph_from_data_frame(d=edge, vertices=node, directed=T) 
  class(net)
  
  #####Just for DevPrj
  #colrs <- c("gold", "tomato")
  
  V(net)$color <- ifelse(V(net)$Type == "USR", "gold", "tomato")
  #E(net)$width <- E(net)$weight/10
  
  #plot(net, edge.arrow.size=.4,vertex.label=NA)
  #png(paste(FilePath,logItems$FileId[row],".png",sep=""))
  png(paste(FilePath, logItems$FileId[row], CommentFile, ".png",sep=""))
  as <- authority_score(net, weights=NA)$vector
  #plot(net, layout=layout_randomly,edge.arrow.size=.4, vertex.label=node$Label, vertex.size=as*30+5)
  plot(net, layout=layout_on_sphere,edge.arrow.size=.5, vertex.label=node$Label, vertex.size=as*20+5)
  dev.off()
  
  NetClustCoef<-transitivity(net)
  NetDens<-edge_density(net, loops=F)
  NetDia<-diameter(net, directed=F, weights=NA)
  NetAvgDegAll<-mean(degree(net, mode="all"))
  NetAvgDegIn<-mean(degree(net, mode="in"))
  NetAvgDegOut<-mean(degree(net, mode="out"))
  NetDistance<-mean_distance(net, directed=T)
  NetDegCent<-centr_degree(net, mode="in", normalized=T)$centralization
  NetCloCent<-centr_clo(net, mode="all", normalized=T)$centralization
  NetBetwCent<-centr_betw(net, directed=T, normalized=T)$centralization
  NetEigenCent<-centr_eigen(net, directed=T, normalized=T)$centralization
  #ceb <- cluster_edge_betweenness(net)
  #NetComponentModularity<-length(ceb)/ceb$vcount
  cfg <- cluster_fast_greedy(as.undirected(net))
  NetComponentModularity<-modularity(cfg)
  NetAssortativityDeg<-assortativity_degree(net, directed=F)
  NetCohesion<-cohesion(net)
  NetAdhesion<-adhesion(net)
  NetRecp<-reciprocity(net)
  #NetLinkPred<-predict_edges(net)
  
  #frame Filling
  
  # newrow = data.frame(PrjId=logItems$FileId[row], Comment=CommentFile, Nodes=nrow(node),Edges=nrow(edge), ClusCoef=NetClustCoef
  #                     ,Dens=NetDens, Dia=NetDia,AvgDegAll=NetAvgDegAll
  #                     ,AvgDegIn=NetAvgDegIn, AvgDegOut=NetAvgDegOut, Distance=NetDistance
  #                     ,DegCent=NetDegCent, ColCent=NetCloCent
  #                     ,BetwCent=NetBetwCent, EigenCent=NetEigenCent, ComModul=NetComponentModularity
  #                     , Assort=NetAssortativityDeg, Coh=NetCohesion,Adh=NetAdhesion, Recp=NetRecp)
  
  newrow = data.frame(PrjId=logItems$FileId[row], Nodes=nrow(node),Edges=nrow(edge), ClusCoef=NetClustCoef
                      ,Dens=NetDens, Dia=NetDia,AvgDegAll=NetAvgDegAll
                      ,AvgDegIn=NetAvgDegIn, AvgDegOut=NetAvgDegOut, Distance=NetDistance
                      ,DegCent=NetDegCent, ColCent=NetCloCent
                      ,BetwCent=NetBetwCent, EigenCent=NetEigenCent, ComModul=NetComponentModularity
                      , Assort=NetAssortativityDeg, Coh=NetCohesion,Adh=NetAdhesion, Recp=NetRecp)
  NetDf <- rbind(NetDf, newrow)
  
}

write.csv(file=paste(FilePath,saveFileName,sep=""), x=NetDf)

