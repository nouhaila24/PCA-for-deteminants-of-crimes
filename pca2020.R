library(plotly)
library(dplyr)
library(readr)
newdata1=newPCA%>% 
  full_join(newPCAhomicide,by=c("Time","Country"))%>%
  select(Time,Country,GDP_per_capita,Gini_index,Mortality_rate,Unemployment,Homicide)
library("writexl")
write_xlsx(newdata1,'newdata1.xlsx')
newdata12 <-aggregate(newdata1[, 4:7], list(newdata1$Country), mean,na.rm=TRUE )

newdata123<-newdata1%>%
   group_by(Country)%>%
  mutate(diff = (GDP_per_capita- lag(GDP_per_capita))/GDP_per_capita)

newdat<-aggregate(newdata123$diff, list(newdata123$Country), mean,na.rm=TRUE )
newdata12$GDP_per_capita<-newdat$x

####faire un caractere +or - homicide####
newdata12$hombrk<-cut(newdata12$Homicide, breaks = 22)
print(newdata12$hombrk)
summary(newdata12$hombrk)
newdata12$hmc[newdata12$Homicide<3.09]<-'moins_homicide'
newdata12$hmc[between(newdata12$Homicide,3.09,9.26)]<-'homicide_average'
newdata12$hmc[newdata12$Homicide>9.26]<-'plus_homicide'

####delete NA######
newdata12<-na.omit(newdata12)

####diviser par 100000 pour homicide , par 1000 pour mortalité infantile, 100 pour chomage#####
newdata12<-newdata12%>%
  group_by(Group.1)%>%
  mutate(Homicide = log(Homicide/100000),Mortality_rate=log(Mortality_rate/1000),Unemployment=log(Unemployment/100),Gini_index=log(Gini_index))

###numbre composante à utiliser#####
library(factoextra)
library(FactoMineR)
newdata12.pca1<- PCA(newdata12[,c(2:6)],  graph = FALSE)
newdata12.pca1$eig
fviz_screeplot(newdata12.pca, addlabels = TRUE)

###pca with biplot####
newdata12.pca <- prcomp(newdata12[,c(2:6)], center = TRUE,scale. = TRUE)
library(ggbiplot)
ggbiplot(newdata12.pca)
ggbiplot(newdata12.pca,obs.scale=2,var.scale=2,labels=(newdata12$Group.1),groups =newdata12$hmc)+ggtitle("ACP pour les determinants des crimes")+theme_minimal()
ggbiplot(newdata12.pca,ellipse=TRUE,obs.scale=2,var.scale=2,labels=(newdata12$newdata12),groups =newdata12$hmc)+ggtitle("ACP pour les determinants des crimes")+theme_minimal()
ggbiplot(newdata12.pca,ellipse=TRUE,var.axes=FALSE,obs.scale=2,var.scale=2,labels=(newdata12$Group.1),groups =newdata12$hmc)+ggtitle("ACP pour les determinants des crimes")+theme_minimal()
 

  
