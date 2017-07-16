bank<-read.csv("C:\\Users\\chen2\\OneDrive\\MKT591\\project\\bank-additional-full.csv", head=TRUE, sep = ";")
#library(gdata)

bank <- lapply(bank, function(x){
  gsub("unknown", NA, x)})

bank<-as.data.frame(bank)

bank.new<-na.omit(bank)

bank.new$newcustomer <- ifelse(bank.new$pdays == 999,1,0)
bank.new[,!(names(bank.new)%in% c("pdays","duration"))]


#library(gmodels)


#dim(bank)
#dim(bank.new)

col1<-c("job","marital","education","default","housing","loan","contact","month","day_of_week","poutcome","y", "newcustomer")
bank.catagory <-bank.new[col1]
#bank.numeric <- bank.new[, !names(bank.new) %in% col1]
#mapply(function(x, y) corr(x, y)$p.value, bank.catagory[, -7], MoreArgs=list(bank.catagory[,-7]))
#chisq.test for independence on catogory variables 
mapply(function(x, y) chisq.test(x, y)$p.value, bank.catagory[, -11], MoreArgs=list(bank.catagory[,11]))

bank.new[col1]<-lapply(bank.new[col1],as.factor)

col<-c("age","campaign","previous","emp.var.rate","cons.price.idx","cons.conf.idx","euribor3m","nr.employed")
bank.new[col]<-lapply(bank.new[col],as.character)

bank.new[col]<-lapply(bank.new[col],as.numeric)


bank.new$cons.price.idx.scale=scale(bank.new$cons.price.idx)
bank.new$nr.employed.scale=scale(bank.new$nr.employed)
bank.new$cons.conf.idx.scale = scale(bank.new$cons.conf.idx)


library(caTools)

set.seed(311)

split = sample.split(bank.new$y, SplitRatio = 2/3)
training_set = subset(bank.new, split == TRUE)
test_set = subset(bank.new, split == FALSE)

#building logistic regression 
regressor = glm(formula = y ~ age+job+marital+education+default+housing+loan+contact+month+day_of_week+campaign+poutcome+emp.var.rate+cons.price.idx.scale+cons.conf.idx.scale+euribor3m+nr.employed.scale+newcustomer, data = training_set, family = binomial('logit'))
summary(regressor)

regressor2 = glm(formula = y ~month+poutcome+emp.var.rate+cons.price.idx.scale+cons.conf.idx.scale+euribor3m+nr.employed.scale+newcustomer, data = training_set, family = binomial('logit'))
summary(regressor2)


#regressor3 = glm(formula = y ~ age+job+marital+education+contact+month+day_of_week+campaign+poutcome+newcustomer, data = training_set, family = binomial('logit'))
#summary(regressor3)
#n = dim(training_set)[1]

#testing training set 
pred.train = predict(regressor,newdata=training_set,type='response')
pred.train<-ifelse(pred.train>0.5,1,0)
(ct=table(training_set$y,pred.train))
print("Logstc Regression using Training data")
diag(prop.table(ct,1))
cat("Acuracy ratio: ",(ct[1,1]+ct[2,2])/sum(ct))


#valiation using testing data

pred.test = predict(regressor,newdata=test_set,type='response')
pred.test<-ifelse(pred.test>0.5,1,0)
(ct=table(test_set$y,pred.test))
print("Logstc Regression using Testing data")
diag(prop.table(ct,1))
cat("Acuracy ratio: ",(ct[1,1]+ct[2,2])/sum(ct))



#Decision Tree

library(rpart)
set.seed(100)
cart_fit<-rpart(formula = y ~ age+job+marital+education+default+housing+loan+contact+month+day_of_week+campaign+poutcome+emp.var.rate+cons.price.idx.scale+cons.conf.idx.scale+euribor3m+nr.employed.scale+newcustomer, data = training_set, method = "class")
cart_fit2<-rpart(formula = y ~ age+job+marital+education+default+housing+loan+contact+month+day_of_week+campaign+poutcome+emp.var.rate+cons.price.idx.scale+cons.conf.idx.scale+euribor3m+nr.employed.scale+newcustomer, data = test_set, method = "class")
summary(cart_fit)
printcp(cart_fit)

#testing training set 2
pred.train = predict(regressor2,newdata=training_set,type='response')
pred.train<-ifelse(pred.train>0.5,1,0)
(ct=table(training_set$y,pred.train))
diag(prop.table(ct,1))
cat("Acuracy ratio: ",(ct[1,1]+ct[2,2])/sum(ct))


#valiation using testing data 2

pred.test = predict(regressor2,newdata=test_set,type='response')
pred.test<-ifelse(pred.test>0.5,1,0)
(ct=table(test_set$y,pred.test))
diag(prop.table(ct,1))
cat("Acuracy ratio: ",(ct[1,1]+ct[2,2])/sum(ct))

#Prediction from decision tree
pred1=predict(cart_fit,type = "class")
ct = table(training_set$y,pred1)
print("For Decision Tree")
print(ct)
cat("Acuracy ratio: ",(ct[1,1]+ct[2,2])/sum(ct))

#Prediction from decision tree
pred1=predict(cart_fit,type = "class")
pred2=predict(cart_fit2,type = "class")
ct2 = table(test_set$y,pred2)
print("For Decision Tree")
print(ct2)
cat("Acuracy ratio: ",(ct2[1,1]+ct2[2,2])/sum(ct2))


#plotTree
plot(cart_fit, uniform = TRUE, main="Classification Tree")
text(cart_fit, use.n = TRUE, all= TRUE, cex=0.8)


