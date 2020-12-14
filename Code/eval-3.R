library(rminer)
#math=read.table(file="math2.csv",header=TRUE) 

inputs=1:32 # all except pass and five
g3=which(names(math)=="G3")
cat("output class:",class(math[,g3]),"\n")
rmath=math[,c(inputs,g3)] # for easy use
y=rmath$g3 # target

# mining for randomForest, external 3-fold, 20 Runs (=60 fitted models)
M1=mining(G3~.,rmath,model="randomForest",method=c("kfold",3,123),Runs=20)
m=mmetric(M1,metric=c("MAE","RMSE")) # 2 metrics:
print(m) # show metrics for each run
mi=meanint(m[,1])
cat("RF MAE values:",round(mi$mean,2),"+-",round(mi$int,2),"\n")

# regression scatter plot:
txt=paste("G3 MAE:",round(mi$mean,2))
mgraph(M1,graph="RSC",Grid=10,main=txt,PDF="rsc-1")

# REC curve, comparison with multiple regression: "mr":
M2=mining(G3~.,rmath,model="mr",method=c("kfold",3,123),Runs=20)
L=vector("list",2) # list of minings
L[[1]]=M1
L[[2]]=M2
mgraph(L,graph="REC",leg=c("randomForest","mr"),main="REC curve",xval=10,PDF="rec-1")

# input importance 
mgraph(M1,graph="imp",leg=names(rmath),xval=10,PDF="rec-1")
