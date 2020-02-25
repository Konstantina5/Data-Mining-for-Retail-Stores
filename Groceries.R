#------------------’σκηση 1------------------

groceries <- read.csv("GroceriesInitial.csv",header=TRUE,sep=",")

sorted_groceries<-as.data.frame(groceries[order(groceries$basket_value),]) #sort grociries based on the basket_value

#discretization fuction
values<-function(baskets){
  num<-nrow(baskets)
  #creating new columns with the three basket value
  baskets[,"low_value_basket"]<-FALSE  
  baskets[,"medium_value_basket"]<-FALSE
  baskets[,"high_value_basket"]<-FALSE
  num1<-num/3
  x=0
  numbers=0
  #for every row of the baskets
  for(row in 1:num){
    x=x+1 
    if(x%%num1==0){
      numbers=numbers+1
    }
    if(numbers==0){
      baskets[row,"low_value_basket"]<-TRUE
    }else if(numbers==1){
      baskets[row,"medium_value_basket"]<-TRUE
    }else{
      baskets[row,"high_value_basket"]<-TRUE
    }
  }
  return (baskets)
}

bask<-values(sort_groceries)
bask<-bask[order(bask$id),] #reordering baskets in their initial form

product_names <- c("citrus fruit", "tropical fruit", "whole milk", "other vegetables", "rolls/buns", "chocolate", "bottled water", "yogurt","sausage", "root vegetables", "pastry", "soda","cream" )


products <- as.data.frame(t(apply(groceries[,4:35],1, function(x)
  (product_names) %in% as.character(unlist(x)))))


names(products) <- product_names
groceries_binary <- cbind(products,groceries[,2:3])
final_groceries<-cbind(groceries[,1,drop=FALSE],groceries_binary)


final_groceries<-cbind(final_groceries,bask[,36:38])


#------------------’σκηση 2------------------
library(arules)
#a)



apriori_data<-cbind(final_groceries[,2:14],final_groceries[,17:19])

rules <- apriori(apriori_data
                 ,parameter = list(supp=0.001, conf=0.5)
                 ,control = list (verbose=F))

inspect(rules)

rules <- apriori(apriori_data
                 ,parameter = list(supp=0.01, conf=0.5)
                 ,control = list (verbose=F))

inspect(rules)

rules <- apriori(apriori_data
                 ,parameter = list(supp=0.1, conf=0.5)
                 ,control = list (verbose=F))

inspect(rules)


#b)


rules <- apriori(apriori_data[,1:13]
                 ,parameter = list(supp=0.001, conf=0.5)
                 ,control = list(verbose=FALSE))


rules_conf <- sort (rules, by="confidence", decreasing=TRUE) #high-confidence rules

inspect(rules_conf[1:20]) 


#c)

rules <- apriori(apriori_data
                 ,parameter = list(supp=0.001, conf=0.5)
                 ,appearance = list(rhs=c("low_value_basket","medium_value_basket","high_value_basket"))
                 ,control = list(verbose=FALSE)) #geting rules that lead to the basket value



rules_conf <- sort (rules, by="confidence", decreasing=TRUE) # high-confidence rules.

inspect(rules_conf[1:20])



#------------------’σκηση 3------------------

#a)
data<-final_groceries[,15:16]
summary(data)

baskets <- scale(data, center = TRUE, scale = TRUE) # standardize variables
summary(baskets)

set.seed(4)
fit <- kmeans(baskets, 5) # 5 clusters


library(factoextra)


fviz_cluster(fit, data, geom = "point")

#b)
# get cluster centers
centers <- t(apply(fit$centers, 1, function(r)
  r * attr(baskets, 'scaled:scale') + attr(baskets, 'scaled:center')))
centers

#get the points of every cluster
cluster1<- data[fit$cluster==1,]
cluster2<- data[fit$cluster==2,]
cluster3<- data[fit$cluster==3,]
cluster4<- data[fit$cluster==4,]
cluster5<- data[fit$cluster==5,]



sd(unlist(cluster1)) #standard deviation for cluster1
sd(unlist(cluster2))  #standard deviation for cluster2
sd(unlist(cluster3))  #standard deviation for cluster3
sd(unlist(cluster4))  #standard deviation for cluster4
sd(unlist(cluster5))  #standard deviation for cluster5


pie_data <- table(fit$cluster)
pie_data <- pie_data/sum(pie_data)*100
pie(pie_data,labels = paste(names(pie_data), "\n", pie_data, sep=""))




#c)


#function used to convert to binary clusters
find_clusters<-function(clusters,num){
  
  #creating new columns with the 5 cluster values
  clusters[,"cluster0"]<-FALSE 
  clusters[,"cluster1"]<-FALSE
  clusters[,"cluster2"]<-FALSE
  clusters[,"cluster3"]<-FALSE
  clusters[,"cluster4"]<-FALSE
  
  #finds the cluster that every basket belong to and puts true to it
  for(row in 1:num){
    if(clusters[row,"basket_value"] %in% cluster1$basket_value & clusters[row,"recency_days"] %in% cluster1$recency_days){
      clusters[row,"cluster0"]<-TRUE
    }else if(clusters[row,"basket_value"] %in% cluster2$basket_value & clusters[row,"recency_days"] %in% cluster2$recency_days){
      clusters[row,"cluster1"]<-TRUE
    }else if(clusters[row,"basket_value"] %in% cluster3$basket_value & clusters[row,"recency_days"] %in% cluster3$recency_days){
      clusters[row,"cluster2"]<-TRUE
    }else if(clusters[row,"basket_value"] %in% cluster4$basket_value & clusters[row,"recency_days"] %in% cluster4$recency_days){
      clusters[row,"cluster3"]<-TRUE
    }else{
      clusters[row,"cluster4"]<-TRUE
    }
  }
  return(clusters)
}

clusters<-find_clusters(groceries,nrow(groceries))
temp<-cbind(final_groceries[,1:16],clusters[,36:40])
groceries_clustered<-cbind(temp,final_groceries[,17:19])


#------------------’σκηση4------------------


rules <- apriori(cbind(groceries_clustered[,2:14],groceries_clustered[,17:21])
                 ,parameter = list( supp=0.001, conf=0.5)
                 ,appearance = list(rhs=c("cluster0","cluster1","cluster2","cluster3","cluster4"))
                 ,control = list(verbose=FALSE))



rules_conf <- sort (rules, by="confidence", decreasing=TRUE) # high-confidence rules.

inspect(rules_conf[1:20])


#apriori for cluster 0
rules <- apriori(cbind(groceries_clustered[,2:14],groceries_clustered[,17:21])
                 ,parameter = list( supp=0.001, conf=0.1)
                 ,appearance = list(rhs=c("cluster0"))
                 ,control = list(verbose=FALSE))

rules_conf <- sort (rules, by="confidence", decreasing=TRUE) # high-confidence rules.
inspect(rules_conf)

#apriori for cluster 1
rules <- apriori(cbind(groceries_clustered[,2:14],groceries_clustered[,17:21])
                 ,parameter = list( supp=0.001, conf=0.3)
                 ,appearance = list(rhs=c("cluster1"))
                 ,control = list(verbose=FALSE))

rules_conf <- sort (rules, by="confidence", decreasing=TRUE) # high-confidence rules.
inspect(rules_conf)


#apriori for cluster 2
rules <- apriori(cbind(groceries_clustered[,2:14],groceries_clustered[,17:21])
                 ,parameter = list( supp=0.001, conf=0.5)
                 ,appearance = list(rhs=c("cluster2"))
                 ,control = list(verbose=FALSE))


rules_conf <- sort (rules, by="confidence", decreasing=TRUE) # high-confidence rules.
inspect(rules_conf[1:20])


#apriori for cluster 3
rules <- apriori(cbind(groceries_clustered[,2:14],groceries_clustered[,17:21])
                 ,parameter = list( supp=0.001, conf=0.5)
                 ,appearance = list(rhs=c("cluster3"))
                 ,control = list(verbose=FALSE))


rules_conf <- sort (rules, by="confidence", decreasing=TRUE) # high-confidence rules.
inspect(rules_conf[1:20])

#apriori for cluster 4
rules <- apriori(cbind(groceries_clustered[,2:14],groceries_clustered[,17:21])
                 ,parameter = list( supp=0.001, conf=0.3)
                 ,appearance = list(rhs=c("cluster4"))
                 ,control = list(verbose=FALSE))


rules_conf <- sort (rules, by="confidence", decreasing=TRUE) # high-confidence rules.
inspect(rules_conf)




