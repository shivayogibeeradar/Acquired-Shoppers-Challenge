setwd("C:/Users/sbiradar/Desktop/project")
install.packages("data.table")
library(data.table)
k=fread(file="trainHistory.csv",sep=",",colClasses=c("factor",rep("numeric",6)))


k[,offerdate:=as.Date(offerdate)]
l=fread(file="offers.csv",colClasses = c(rep("numeric",6)))
m=fread(file="reduced2.csv",colClasses = c("factor",rep("numeric",10)))
m[,date:=as.Date(date)][,productmeasure:=factor(productmeasure)]
#l=read.csv(file="testHistory.csv",sep=",",header=TRUE)
nrow(m)

trainid={ }

combid=rbind(k$id,l$id)
combid=as.data.frame(as.numeric(combid))
head(combid,n=2)
colnames(combid)=("id")
#install.packages("bigmemory")
#install.packages("biglm", repos="http://R-Forge.R-project.org")
n1=merge(k,l,by="offer",all.k=TRUE)
n1

o=merge(m,n1,by=c("id","chain"))
nrow(o)
setkey(n1,"id")
head(o,n=2)
library(plyr)


r=o[o$company.x==o$company.y]
head(r,n=2)
l=r[,.N,c("id","company.x")]
#nrow(l)
#r[,.(sum(r$purchaseamount)),c("id","company.x")]

r=o[o$company.x==o$company.y]
w=aggregate(purchaseamount~id+company.x,data=r,sum)
g=aggregate(purchasequantity~id+company.x,data=r,sum)
s=merge(w,g,by=c("id","company.x"))
s1=merge(l,s,by=c("id","company.x"))
colnames(s1)=c("id","company.x","freqovcomp","ovpurchaseamountcomp","ovpurchasequantitycomp")

r=o[o$category.x==o$category.y]
l=r[,.N,c("id","category.x")]
w=aggregate(purchaseamount~id+category.x,data=r,sum)
g=aggregate(purchasequantity~id+category.x,data=r,sum)
s=merge(w,g,by=c("id","category.x"))
s2=merge(l,s,by=c("id","category.x"))
s2
colnames(s2)=c("id","category.x","freqovcat","ovpurchaseamountcat","ovpurchasequantitycat")
r=o[o$brand.x==o$brand.y]
l=r[,.N,c("id","brand.x")]
w=aggregate(purchaseamount~id+brand.x,data=r,sum)
g=aggregate(purchasequantity~id+brand.x,data=r,sum)
s=merge(w,g,by=c("id","brand.x"))
s3=merge(l,s,by=c("id","brand.x"))
s3
colnames(s3)=c("id","brand.x","freqovbrand","ovpurchaseamountbrand","ovpurchasequantitybrand")






head(z,n=2)
nrow(w)
str(w)
z=aggregate(r$purchasequantity~r$id+r$company.x,data=r,sum)

#o[, ndaybeforeoffer:=o$offerdate-o$date,by=o$id]
#gc()
r

ntimescompanybought=function(n){
k=o[o$offerdate-o$date<n & o$company.x==o$company.y,]
l=k[,.N,by=c("id","company.x")]
colnames(l)=c("id","company.x",paste("freqcomp",n,sep="_"))

w=aggregate(purchaseamount~id+company.x,data=k,sum)
colnames(w)=c("id","company.x",paste("purchaseamtcomp",n,sep="_"))
z=aggregate(purchasequantity~id+company.x,data=k,sum)
colnames(z)=c("id","company.x",paste("purchasequantcomp",n,sep="_"))
z1=merge(w,z,by=c("id","company.x"))
head(l,n=2)
merge(l,z1,by=c("id","company.x"))
}
ntimescompanybought(30)
#ntimescompanybought(180)

head(z2,n=2)
nrow(w)
nrow(l)

ntimescategorybought=function(n){
  k=o[o$offerdate-o$date<n & o$category.x==o$category.y,]
  l=k[,.N,by=c("id","category.x")]
  colnames(l)=c("id","category.x",paste("freqcat",n,sep="_"))
  w=aggregate(purchaseamount~id+category.x,data=k,sum)
  colnames(w)=c("id","category.x",paste("purchaseamtcat",n,sep="_"))
  z=aggregate(purchasequantity~id+category.x,data=k,sum)
  colnames(z)=c("id","category.x",paste("purchasequantcat",n,sep="_"))
  z1=merge(w,z,by=c("id","category.x"))
  merge(l,z1,by=c("id","category.x"))
  
  }

ntimesbrandbought=function(n){
  
  
  k=o[o$offerdate-o$date<n & o$brand.x==o$brand.y]
  
  l=k[,.N,by=c("id","brand.x")]
  colnames(l)=c("id","brand.x",paste("freqbrand",n,sep="_"))
  w=aggregate(purchaseamount~id+brand.x,data=k,sum)
  colnames(w)=c("id","brand.x",paste("purchaseamtbrand",n,sep="_"))
  z=aggregate(purchasequantity~id+brand.x,data=k,sum)
  colnames(z)=c("id","brand.x",paste("purchasequantbrand",n,sep="_"))
  z1=merge(w,z,by=c("id","brand.x"))
  merge(l,z1,by=c("id","brand.x"))
}
 ntimesbrandbought(180) 
  
 
 







merging <- function(a,b){
  return(merge(a,b,by.x=c("id","company"),by.y=c("id","company.x"),all.x=TRUE))
}





result <- Reduce(f=merging, lapply(c(30,60,90,180), FUN=ntimescompanybought),n1)
result[is.na(result)]=0
result
merging1 <- function(a,b){
  return(merge(a,b,by.x=c("id","category"),by.y=c("id","category.x"),all.x=TRUE))
}

result1 <- Reduce(f=merging1, lapply(c(30,60,90,180), FUN=ntimescategorybought),result)
result1[is.na(result1)]=0
result1
merging1 <- function(a,b){
  return(merge(a,b,by.x=c("id","brand"),by.y=c("id","brand.x"),all.x=TRUE))
}


result2 <- Reduce(f=merging1, lapply(c(30,60,90,180), FUN=ntimesbrandbought),result1)
result2[is.na(result2)]=0
result2
result3=merge(result2,s1,by.x=c("id","company"),by.y=c("id","company.x"),all.x=TRUE)
result3[is.na(result3)]=0

result4=merge(result3,s2,by.x=c("id","category"),by.y=c("id","category.x"),all.x=TRUE)
result4[is.na(result4)]=0
result4
result5=merge(result4,s3,by.x=c("id","brand"),by.y=c("id","brand.x"),all.x=TRUE)
result5[is.na(result5)]=0
result5
write.csv(file="feature.csv",result5)



install.packages("yaml")

result5$repeater[result5$repeater==1]="t"
result5$repeater[result5$repeater==0]="f"
result5

isclassification <- T
outcomeName <- 'repeater'
weightName <- ''
objDF <- result5

predictors <- names(result5)[!names(result5) %in% c(outcomeName, weightName,"offerdate","id")]
fileName <- 'vw.txt'
target=result5$repeater
dt2vw(result5,'vw.txt' , namespaces = NULL, result5$repeater, weight = NULL, tag = NULL, hard_parse = F, append = F)
str(result5)
########################################################################################
dt2vw <- function(data, fileName, namespaces = NULL, target, weight = NULL, tag = NULL, hard_parse = F, append = F)

  
func_Col <- function(result5) {
  lineHolders <- paste(result5$id,"|",result5$repeater ,"quantity:",result5$quantity)
  for( ic in 1:ncol(result5)) {
    nonzeroes <- which(as.logical(as.numeric(result5[,ic]))) 
    lineHolders[nonzeroes] <- paste(lineHolders[nonzeroes]," ",names(result5)[ic], ":", as.numeric(result5[nonzeroes,ic]),sep="") 
  }
  lineHolders <- paste(lineHolders,"\n",sep="")
  return(lineHolders)
}
func_Col(result5)
