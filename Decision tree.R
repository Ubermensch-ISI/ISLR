require(tree)
require(ISLR)
data(Carseats)
data=Carseats
str(data)
attach(data)


high=ifelse(Sales<=8,"no","yes")
data$high=data.frame(high)
data$high=as.factor(high)
str(data)

#--------------------tree
tree1=tree(high~.-Sales,data=data)
summary(tree1)

plot(tree1)
text(tree1,pretty=1)

tree1

#----------Bagging :- taking averages over the training data as estimate

tree.data