library(data.table)

ggplot(ksamples)+geom_point(aes(meanStat,meanVar,size=N,col=sig))+
  ggrepel::geom_text_repel(data=ksamples[sig==T],cex=2,aes(meanStat,meanVar,label=term))
allclusters<-data.table()

for (X in list.files("~/GitHub_local/doggo/10x_doggo/N=1M_sampling/",pattern = "Avglog2fc",full.names = T)){
  ksamples<-fread(X)
  ksamples[,file:=X]
  
  ksamples[,Cluster:=tstrsplit(file,split="//")[2]]
  ksamples[,Cluster:=tstrsplit(Cluster,split="_")[1]]
  ksamples[,scaled_stat:=scale(meanStat),by=Cluster]
  allclusters=rbindlist(list(ksamples,allclusters))
}

allclusters[,filter:=any(sig==TRUE),by=term]


ggplot(allclusters[filter==T,])+
  geom_tile(aes(term,Cluster,fill=meanStat))+
  scale_fill_gradient2()+
  geom_point(aes(term,Cluster,alpha=sig,cex=-log10(adjPbs)))+
  theme(axis.text.x = element_text(size=6,hjust = 1,angle=90))+
  coord_fixed()

ggplot(allclusters[filter==T,])+
  geom_tile(aes(term,Cluster,fill=scaled_stat))+
  scale_fill_gradient2()+
  geom_point(aes(term,Cluster,alpha=sig,cex=-log10(adjPbs)))+
  theme(axis.text.x = element_text(size=6,hjust = 1,angle=90))+
  coord_fixed()

ggplot(allclusters[filter==T,])+
  geom_tile(aes(term,Cluster,fill=scaled_stat))+
  scale_fill_gradient2()+
  geom_point(aes(term,Cluster,alpha=sig,pch=meanStat<0,cex=-log10(adjPbs)))+
  theme(axis.text.x = element_text(size=6,hjust = 1,angle=90))+
  coord_fixed()

ggplot(allclusters[filter==T,])+
  geom_tile(aes(term,Cluster,fill=meanStat))+
  scale_fill_gradient2()+
  geom_point(aes(term,Cluster,alpha=sig,cex=N),pch=21)+
  theme(axis.text.x = element_text(size=6,hjust = 1,angle=90))+
  coord_fixed()
