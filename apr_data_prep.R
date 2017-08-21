my.vars <- finalni_lrge# a matrix with your 14 different environmental variables
names(my.vars)[c(1,3)]<-c("`Racio_novcane_likvidnosti_(Cash_ratio)`","`Racio_pokrica_kamata_zaradom_pre_kamata_i_poreza_(Interest_Coverage_Ratio)`")
my.vars$default.y=NULL

library(speedglm)
nvar<-ncol(my.vars)

#colnames(my.vars) <- paste("var", 1:nvar, sep="") # add row names "var1" - "var14"
my.grad.data <- 1:nvar
sum.vars <- vector()
auc.p <- vector()
auc.pred<-vector()
comb.mat <- matrix(numeric(0), nrow=nvar, ncol=0) # initialise the matrix containing all combinations


for ( i in 1:nvar ) { # generate and store all possible combination of sums of the 14 variables
  
  t.mat <- combn(my.grad.data, m=i)
  
  comb.mat <- cbind(comb.mat, rbind(t.mat, matrix(NA, ncol=dim(t.mat)[2] , nrow=nvar-i)))
}

colnms<-colnames(my.vars)
my.vars$default.y=finalni_lrge$default.y

for ( j in 1:dim(comb.mat)[2] ) { # calculate and store the R2 for all combinations
  
  #sum.vec <- rowSums(my.vars[, comb.mat[, j]], na.rm=TRUE)
  if(j==13)browser()
  
  sum.vars[j] <- paste( colnms[c(na.omit(comb.mat[, j]))], 
                        collapse="+")
  relacija=as.formula(paste("default.y ~ ",sum.vars[j],sep = ""))
  model = speedglm(relacija,
                   data = my.vars,
                   y=TRUE,
                   fitted = TRUE,
                   family = binomial(link = "logit"))

  model.data.frame=data.frame(fit=fitted.values(model), dif=model$y)
  auc.p[j] <- auc(as.numeric(model.data.frame$dif), as.numeric(model.data.frame$fit))
  
  model.pred<-as.numeric(predict(model, newdata = finalni_lrge.valid, type = "response"))
  dif<-as.numeric(finalni_lrge.valid$default.y)
  auc.pred[j]<-auc( dif,model.pred)
  
  if(j %in% round(seq(from=1, to=dim(comb.mat)[2],length.out = 100))) print(j/dim(comb.mat)[2])
  print(j)
  
}



result.frame <- data.frame(combination=sum.vars, auc.p=auc.p, auc.valid=auc.pred)

result.frame.sorted <- result.frame[order(auc.p, decreasing=TRUE), ]

head(result.frame.sorted, n=100) # the 10 "best" combinations