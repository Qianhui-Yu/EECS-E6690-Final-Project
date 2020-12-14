library(rminer)
# read previously saved file
# math=read.table(file="math2.csv",header=TRUE) 

inputs=1:32 # all except G3 and pass 
fout=which(names(math_full)=="five_math")
cat("output class:",class(math_full[,fout]),"\n")
cmath=math_full[,c(inputs,fout)] # for easy use
H=holdout(cmath$five_math,2/3,123)
y=cmath[H$ts,]$five_math # target

# simple fit of randomForest
C1=fit(five_math~.,cmath[H$tr,],model="randomForest")
P1=predict(C1,cmath[H$ts,]) # class predictions
print(P1[1,]) # show 1st prediction
m=mmetric(y,P1,metric=c("AUC","AUCCLASS"))
print(m) # global AUC, AUC per class
m=mmetric(y,P1,metric=c("CONF")) # a)
print(m$conf) # confusion matrix
m=mmetric(y,P1,metric=c("ALL"))
print(round(m,1)) # all prob. metrics

# ROC curve for class "A"
TC=1
txt=paste("class",levels(y)[TC],"AUC:",round(mmetric(y,P1,metric="AUC",TC=TC),2))
mgraph(y,P1,graph="ROC",baseline=TRUE,Grid=10,main=txt,TC=2,PDF="roc-2")

I=Importance(C1,cmath[H$tr,])
print(round(I$imp,digits=2))
imax=which.max(I$imp)

L=list(runs=1,sen=t(I$imp),sresponses=I$sresponses) # create a simple mining list
mgraph(L,graph="IMP",leg=names(cmath),col="gray",Grid=10,PDF="imp-1")
txt=paste("VEC curve for",names(cmath)[imax],"influence on class",levels(y)[TC])
mgraph(L,graph="VEC",xval=imax,Grid=10,data=cmath[H$tr,],TC=1,main=txt,PDF="vec-1")
