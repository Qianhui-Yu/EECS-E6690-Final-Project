library(rminer)
# read previously saved file
# math=read.table(file="math2.csv",header=TRUE) \

# select inputs:
inputs=2:29 # select from 2 ("sex") to 29 ("health")
# select outputs: binary task "pass_math"
bout=which(names(math_full)=="pass_math")
cat("output class:",class(math_full[,bout]),"\n")
bmath=math_full[,c(inputs,bout)] # for easy use
y=bmath$pass_math # target

# fit rpart to all data, pure class modeling (no probabilities)
B1=fit(pass_math~.,bmath,model="rpart",task="class") # fit a decision tree
P1=predict(B1,bmath) # class predictions
print(P1[1]) # show 1st prediction
m=mmetric(y,P1,metric=c("ACC","ACCLASS"))
print(m) # accuracy, accuracy per class
m=mmetric(y,P1,metric=c("CONF")) # a)
print(m$conf) # confusion matrix
m=mmetric(y,P1,metric=c("ALL"))
print(round(m,1)) # all pure class metrics

# fit rpart to all data, default probabilistic modeling 
B2=fit(pass_math~.,bmath,model="rpart",task="prob") # fit a decision tree
P2=predict(B2,bmath) # predicted probabilities
print(P2[1,]) # show 1st prediction
m=mmetric(y,P2,metric=c("ACC"),TC=2,D=0.5)
print(m) # accuracy, accuracy per class
m=mmetric(y,P2,metric=c("CONF"),TC=2,D=0.1) # equal to a)
print(m$conf) # confusion matrix
m=mmetric(y,P2,metric=c("AUC","AUCCLASS"))
print(m) # AUC, AUC per class
m=mmetric(y,P2,metric=c("ALL"))
print(round(m,1)) # all prob metrics

# ROC and LIFT curve:
txt=paste(levels(y)[2],"AUC:",round(mmetric(y,P2,metric="AUC",TC=2),2))
mgraph(y,P2,graph="ROC",baseline=TRUE,Grid=10,main=txt,TC=2,PDF="roc-1")
txt=paste(levels(y)[2],"ALIFT:",round(mmetric(y,P2,metric="ALIFT",TC=2),2))
mgraph(y,P2,graph="LIFT",baseline=TRUE,Grid=10,main=txt,TC=2,PDF="lift-1")

