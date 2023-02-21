newset<-read.csv("new_df.csv")

newset$FLAG_OWN_REALTY
a<-newset$AMT_INCOME_TOTAL
summary(a)
b<-newset$AGE
summary(b)

newset$term<-rep(120,167728)
new1<-subset(newset,newset$AMT_INCOME_TOTAL<=112500)
new1$cre_level<-rep(0.08/12,46073)
new2<-subset(newset,newset$AGE<=40&newset$AMT_INCOME_TOTAL>112500)
new2$cre_level<-rep(0.07/12,63239)
new3<-subset(newset,newset$AGE>40&newset$AMT_INCOME_TOTAL>112500)
new3$cre_level<-rep(0.06/12,58416)

library(tidyverse)
#put all data frames into list
df_list <- list(new1, new2, new3)

#merge all data frames in list
newdata<-df_list %>% reduce(full_join)

#monthly loan fomula: E = P x r x ( 1 + r )n / ( ( 1 + r )n - 1 ) 
newdata$monthly_loan<-newdata$AMT_CREDIT_current*newdata$cre_level*((1+newdata$cre_level)**newdata$term)/((1+newdata$cre_level)**newdata$term-1)
newdata$total_loan<-newdata$monthly_loan*120
  
  
  
 
29822+58954+58416



ifelse(newset$AMT_INCOME_TOTAL<112500&newset$AGE<40,)


age income current-credit 

6%
6.2%
Normalized_with_dummies_for_Natalie

newset1<-read.csv("Normalized_with_dummies_for_Natalie.csv")
newset1$AMT_INCOME_TOTAL
  

 #Applying PCA
installpkg("plfm")
library(plfm)

Labels_Data<- labels(election_data_train[1,])
Labels_Data

Data_PCA<- subset(election_data_train, select =  - c(County, State,FIPS, Region,ElectionDate,
                                                     ElectionType,TotalVote,
                                                     
                                                     
                                                     Clinton,Obama,Obama_wins,Obama_margin_percent,Obama_margin))
Data_PCA
pca.electionData<- prcomp(Data_PCA, scale=TRUE)

ncol(Data_PCA)
ncol(election_data_train)
pca.electionData


##Importance of components
summary(pca.electionData)

library(ggplot2)
plot(pca.electionData,main="PCA: Variance of the Votes Explained by Factors", xlab = "Factors")
mtext(side=1, "Factors Affecting the Votes",  line=1, font=2)


##Drawing a biplot

install.packages("ggfortify")
library(ggfortify)
autoplot(stats::prcomp(Data_PCA, scale=TRUE), label = FALSE, loadings.label = TRUE, main = "BiPlot : Variables of PC1 and PC2")


#PC score of each vote
prediction_Election<-predict(pca.electionData)
prediction_Election
#Picking the first four PC scores to interpret the votes
#PC1 Vs PC2
plot(prediction_Election[,1:2], pch=21,  main="PC1 and PC2 Impact on Votes")


plot(pca.electionData$rotation[,1:4], bg="black", cex=1, pch=21,  main="Loadings Plot")
text(pca.electionData$rotation[,1:4],             
     labels=rownames(pca.electionData$rotation))



#PC3 Vs PC4
plot(prediction_Election[,3:4], pch=21,  main="PC3 and PC4 Impact on Votes")




##calculating variance explained by each principal component
Var_Electiondata<-pca.electionData$sdev^2 / sum(pca.electionData$sdev^2)
Var_Electiondata
##Creating a scree plot:  plot that displays the total variance explained by each principal component – to visualize the results of PCA
Var_Electiondata_X<-Var_Electiondata[1:15]
Var_Electiondata_X

qplot (c(1:15),Var_Electiondata_X) + 
  geom_line() + 
  xlab("Principal Component") + 
  ylab("Variance Explained") +
  ggtitle("Total Variance in the Dataset Explained by Each Principal Component") +
  ylim(0, 0.3)


#### trying out the 2dPCA plot from the 30 feature dataset
election_data_train$Obama_wins
install.packages("factoextra")

library("factoextra")

Who_Won <- c()
for (i in 1:1737) {
  
  if (election_data_train$Obama_wins[i]>0) {
    a = "Obama"
  }
  else  {
    a = "Clinton"
  }
  Who_Won<-append(Who_Won,a)
}

Who_Won

election_data_train_Viz<-data.frame(election_data_train,Who_Won)
head(election_data_train_Viz)

pca.electionData
fviz_pca_ind(pca.electionData, geom.ind = "point", pointshape = 21, 
             pointsize = 2, 
             fill.ind = election_data_train_Viz$Who_Won, 
             col.ind = "black", 
             palette = "jco", 
             addEllipses = TRUE,
             label = "var",
             col.var = "black",
             repel = TRUE,
             legend.title = "Candidate") +
  ggtitle("2D PCA-plot from 32 feature dataset") +
  theme(plot.title = element_text(hjust = 0.5))

##Interpreting the four factors by looking at loadings (AKA
##correlation of each factor with the original feature)
loadings <- pca.electionData$rotation[,1:10]
loadings

### For each factor lets display the top features that 
### are responsible for 3/4 of the squared norm of the loadings

#PC1
L1<-loadings[order(abs(loadings[,1]), decreasing=TRUE)[1:27],1]
loadingfit <- lapply(1:27, function(k) ( t(L1[1:k])%*%L1[1:k] - 3/4 )^2)
L1[1:which.min(loadingfit)]

#PC2
L2<-loadings[order(abs(loadings[,2]), decreasing=TRUE)[1:27],2]
loadingfit <- lapply(1:27, function(k) ( t(L2[1:k])%*%L2[1:k] - 3/4 )^2)
L2[1:which.min(loadingfit)]

#PC3

L3<-loadings[order(abs(loadings[,3]), decreasing=TRUE)[1:27],3]
loadingfit <- lapply(1:27, function(k) ( t(L3[1:k])%*%L3[1:k] - 3/4 )^2)
L3[1:which.min(loadingfit)]


#PC4

L4<-loadings[order(abs(loadings[,4]), decreasing=TRUE)[1:27],4]
loadingfit <- lapply(1:27, function(k) ( t(L4[1:k])%*%L4[1:k] - 3/4 )^2)
L4[1:which.min(loadingfit)]