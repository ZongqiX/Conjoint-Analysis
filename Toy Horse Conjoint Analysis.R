setwd("C:/Users/xzq/Desktop/MR/Final")
filenm = "MKT412R - Final Analysis Case Data"
load(paste(filenm,".Rdata",sep=""))
ls()

# Part1: Regression for each individual
atts1 = atts[-1]
colnames(desmat) = atts1
desmatf = cbind(rep(1,nrow(desmat)),desmat)
partworths = matrix(nrow=sampsize,ncol=ncol(desmatf))
for(i in 1:sampsize){ 
    partworths[i,]=lm(ratings~desmat,subset=ID==i)$coef
}
colnames(partworths) = atts

# Part2: Cluster Analysis of Utility Partworths 
library(cluster)
library(fpc)
set.seed(123456)

toclust = partworths   
pm1 = pamk(toclust,scaling=TRUE)   
pm1$nc   
# the optimal number of clusters: 3

# vitually see the performance with number of clusters
wss = matrix(NA, nrow=15, ncol=1)
for (i in 1:15) wss[i] <- sum(kmeans(toclust,
                                     centers=i,nstart=3)$withinss)
plot(1:15, wss, type="b", xlab="Number of Clusters",
     ylab="Within groups sum of squares")

km1 = kmeans(toclust,3,iter.max = 20, nstart=3)
percsize = paste(1:3," = ",format(km1$size/sum(km1$size)*100,digits=2),"%",sep="")
pie(km1$size,labels=percsize)

clusplot(toclust, km1$cluster, color=TRUE, shade=TRUE,
         labels=3, lines=0);

plotcluster(toclust, km1$cluster)  
km1$centers 


# Part3: a priori segment level conjoint analysis 

# by a priori segment age
summary(lm(ratings~desmat*ageD))   

summary(lm(ratings~desmat,subset=ageD==1)) # older kids
summary(lm(ratings~desmat,subset=ageD==0)) # young kids

# Similarly do a priori segmentation by gender
summary(lm(ratings~desmat*genderD))

summary(lm(ratings~desmat,subset=genderD==1)) # Female
summary(lm(ratings~desmat,subset=genderD==0)) # Male

# part4: Market Simulation 

#predict missing value
partworths.full = matrix(rep(partworths,each=16),ncol=5)
head(partworths.full, 30)
pratings = rowSums(desmatf*partworths.full)
finalratings = ifelse(is.na(ratings),pratings,ratings)

simDecInput = matrix(finalratings,nrow=nprofiles) 

simDec = function(inputmat,scen){
    inmkt = inputmat[scen,]
    max = apply(inmkt,2,max)
    firstChoices = (inmkt==rep(max, each=length(scen)))
    shares = firstChoices/rep(colSums(firstChoices),each=length(scen))
    rowMeans(shares)
}

#market share simulation
scen0 = c(14,6,8)
simDec0 = simDec(simDecInput,scen0)
simDec0

# What if our competitor responds by lowering price?
scen2 = c(14,6,7)
simDec2 = simDec(simDecInput,scen2)
simDec2















