library("e1071")
library("C50")
library("RWeka")
library("base")
library("dplyr")
csv <- read.csv("bank-additional-full-sep.csv")
csv$job <- toupper(csv$job) #轉成全大寫
csv[0,] #有哪些屬性
cat( "distinct age: ", levels(factor(csv$age)), "\n\n" ) #看age有多少種值
cat( "distinct job: ", levels(factor(csv$job)), "\n\n" ) #看job有多少種值
cat( "distinct marital: ", levels(factor(csv$marital)), "\n\n" )
cat( "distinct education: ", levels(factor(csv$education)), "\n\n" )
cat( "distinct default: ", levels(factor(csv$default)), "\n\n" )
cat( "distinct housing: ", levels(factor(csv$loan)), "\n\n" )
cat( "distinct contact: ", levels(factor(csv$contact)), "\n\n" )
cat( "distinct month: ", levels(factor(csv$month)), "\n\n" )
cat( "distinct day_of_week: ", levels(factor(csv$day_of_week)), "\n\n" )
cat( "distinct poutcome: ", levels(factor(csv$poutcome)), "\n\n" )
cat( "distinct y: ", levels(factor(csv$y)), "\n\n" )
cat( "missing: ", sum(is.na(csv$job)) ) #看有多少missing value

#naiveBayse
train_result<- naiveBayes(y~ +cut(age,breaks=4) +job +marital +education +default +housing +loan +contact +month +day_of_week +cut(duration,breaks=10) +cut(campaign,breaks=10) +factor(pdays) +factor(previous) +poutcome +cut(emp.var.rate,breaks=6) +cut(cons.price.idx,breaks=6) +cut(cons.conf.idx,breaks=6) +cut(euribor3m,breaks=6) +cut(nr.employed ,breaks=6) , data= csv)
test_file <- sample_n(read.csv("bank-test.csv"), 4119)
testing_data <- subset(test_file, select = - y)
test_target <- test_file$y
predicts <- predict(train_result, newdata = testing_data)
predict_result <- cbind(testing_data, predicts)
table(predicts,test_target)
sprintf("%.4f%%",(sum (test_target == predicts)/length(test_target))*100)
capture.output(train_result, file="trained_model.csv")

#J48
#j48model <- J48(y~. , data=csv)

#C5.0
c50model <- C5.0(y~ +duration + pdays +nr.employed , data = csv)
summary(c50model)
plot(c50model)
c50_predicts <- predict(c50model,newdata = testing_data)
c50_predict_result <- cbind(testing_data, c50_predicts)
table(c50_predicts,test_target)
sprintf("%.4f%%",(sum(test_target == c50_predicts)/length(test_target))*100)
