

data <- read.csv("NewsInfo.csv")
data$month <- factor(data$month)

library(ggplot2)
library(caret)
library(pROC)
library(corrplot)


############ Data exploration ##############

barplot(table(data$Type), main = "News Type Distribution",
        xlab = "News Type",
        ylab = "Frequency")
table(data$Type)


plot(as.Date(names(table(fake$date))), unname(table(fake$date)),
     type = "l", col = "red", ylim = c(0,max(unname(table(real$date)))+5),
     xlab = "Date", ylab = "Frequency", main = "Real vs. Fake News")
lines(as.Date(names(table(real$date))), unname(table(real$date)),
      col = "green")
rect(xleft = as.Date("2016-1-01"), ybottom = 0,
     xright = as.Date("2016-11-06"), ytop = 200,
     col = "blue", density = 3)
text(x = as.Date("2016-1-01"), y = 100,
     labels = "Election Year", cex = 2,
     adj = -0.2)
legend("topleft", legend = c("Real","Fake"),
       lty = 1,
       col = c("green","red"))



corrplot(cor(data[,-c(1,2)]))


corrplot(cor(data[data$Type=="real",-c(1,2)]), main = "real news")
corrplot(cor(data[data$Type=="fake",-c(1,2)]), main = "fake news")

levels(data$month)
for (i in 1:length(levels(data$month))){
  corrplot(cor(data[data$month == levels(data$month)[i],-c(1,2)]), 
           main = paste ("month",i, sep = " "))
}


colSums(data[,-c(1,2)])
colSums(data[data$Type=="real",-c(1,2)])
colSums(data[data$Type=="fake",-c(1,2)])

sumsType <- data.frame(SUM = as.vector(colSums(data[data$Type=="real",-c(1,2)])),
                       vars = names(colSums(data[data$Type=="real",-c(1,2)])),
                       Type = "real")

sumsType <- rbind(sumsType, data.frame(SUM = as.vector(colSums(data[data$Type=="real",-c(1,2)])),
                                       vars = names(colSums(data[data$Type=="real",-c(1,2)])),
                                       Type = "fake"))


ggplot(data = sumsType, aes(x = reorder(vars, SUM), y = SUM, fill = Type)) + 
  geom_bar(stat = "identity") + theme(axis.text.x = element_text(vjust = 0.25, 
    angle = 90))

ggplot(data = sumsType, aes(x = reorder(vars, SUM), y = SUM, fill = Type)) + 
  geom_bar(stat = "identity", position = position_dodge(), width = 0.8) + 
  theme(axis.text.x = element_text(vjust = 0.25, angle = 90))+
  labs(title = "Sum of Words by Type", x = "Words")

########### Modeling ###############

set.seed(210)
intrain <- createDataPartition(data$Type, p = 0.8)[[1]]
Train <- data[intrain,]
Test <- data[-intrain,]


AccPlot <- function(MODEL, THRESHOLD, TESTdata, modelName){
  
  thresholds <- seq(0.01, 1, 0.01)
  Acc <- c()
  
  for (i in 1:length(thresholds)){
    Preds <- predict(MODEL, TESTdata, type = "prob")
    Preds <- ifelse(Preds$fake >= thresholds[i], "fake", "real")
    Acc <- append(Acc, 1 - mean(Preds != Test$Type))
    
  }
  
  plot(thresholds, Acc, type = "l", lwd = 2,
       xlab = "Prediction Threshold",
       ylab = "Test Accuracy",
       main = paste(modelName, "Model Test Accuracy", sep = " "), 
       ylim = c(min(Acc),max(Acc) + 0.05))
  points(THRESHOLD, Acc[THRESHOLD * 100], col = "red", pch = 16, cex = 2)
  text(THRESHOLD + 0.11, Acc[THRESHOLD * 100] + 0.02, 
       labels = paste("Threshold = ", THRESHOLD, ", Acc = ", 
                      round(Acc[THRESHOLD * 100], 3), sep = ""))
  
}




############ Logistic ##################

set.seed(210)
ctrl <- trainControl(method = "LGOCV",
                     summaryFunction = twoClassSummary,
                     classProbs = TRUE,
                     savePredictions = TRUE)

logicTune <- train(Type ~., data = Train,
                   method = "glm",
                   metric = "ROC",
                   trControl = ctrl)
logicTune

summary(logicTune)


### Make graph to compare 
logicCoeff <- as.data.frame(summary(logicTune)$coefficients)
logicCoeff <- logicCoeff[-1,]
logicCoeff$Vars <- rownames(logicCoeff)
logicCoeff$Sig <- ifelse(logicCoeff$`Pr(>|z|)` <= 0.05, "*","")

VJUST <- ifelse(logicCoeff$Estimate <= 0, 1.1, 0.4)
ggplot(data = logicCoeff, aes(x = reorder(Vars, Estimate), y = Estimate,
                              fill = (Estimate >= 0))) + 
  geom_bar(stat = "identity", width = 0.4) + 
  geom_text(aes(label = Sig), col = "white", size = 10, vjust = VJUST) +
  labs(title = "Logistic Regression Estimates", 
    x = NULL, y = "Log(Odds)") +
  theme(legend.position = "none") + theme(axis.text = element_text(hjust = 1), 
    axis.text.x = element_text(vjust = 0.25, 
        angle = 90)) + theme(panel.grid.major = element_line(colour = "gray26"), 
    panel.grid.minor = element_line(linetype = "blank"), 
    panel.background = element_rect(fill = "black")) + 
  theme(plot.subtitle = element_text(size = 9)) +
  theme(axis.text.y = element_text(size = 12))



### Accuracy Plot
AccPlot(logicTune, 0.75, Test, "Logistic")

### Confusion Matrix
logicPred <- predict(logicTune, Test, type = "prob")
logicPred <- ifelse(logicPred$fake >= 0.75, "fake", "real")
table(logicPred, Test$Type)

### Test error rate
mean(logicPred != Test$Type)

### Importance
varImp(logicTune)
plot(varImp(logicTune))

### Test ROC
logicROC <- roc(response = logicTune$pred$obs,
                predictor = logicTune$pred$fake,
                levels = rev(levels(logicTune$pred$obs)))
plot(logicROC, legacy.axes = TRUE)
logicAUC <- auc(logicROC)


### Save results

Test_Acc <- c(1 - mean(logicPred != Test$Type))
Test_Error <- c(mean(logicPred != Test$Type))
Test_AUC <- c(logicAUC[1])
Models <- c("Logistic")




############# Neural Network #################

nnetGrid <- expand.grid(.size = 1:10,
                        .decay = c(0, .01, .1, 0.5))
set.seed(210)
nnetTune <- train(Type ~., data = Train,
                  method = "nnet",
                  metric = "ROC",
                  tuneGrid = nnetGrid,
                  trControl = ctrl)

nnetTune

library(devtools)
source_url("https://gist.githubusercontent.com/fawda123/7471137/raw/466c1474d0a505ff044412703516c34f1a4684a5/nnet_plot_update.r")
plot.nnet(nnetTune)

AccPlot(nnetTune, 0.75, Test, "Neural Network")

nnetPred <- predict(nnetTune, Test, type = "prob")
nnetPred <- ifelse(nnetPred$fake >= 0.75, "fake", "real")

table(nnetPred, Test$Type)

### Test error rate
mean(nnetPred != Test$Type)

### Importance
varImp(nnetTune)
plot(varImp(nnetTune))

### Test ROC
nnetROC <- roc(response = nnetTune$pred$obs,
               predictor = nnetTune$pred$fake,
               levels = rev(levels(nnetTune$pred$obs)))
plot(nnetROC, legacy.axes = TRUE)
nnetAUC <- auc(nnetROC)


### Save results

Test_Acc <- append(Test_Acc, 1 - mean(nnetPred != Test$Type))
Test_Error <- append(Test_Error, mean(nnetPred != Test$Type))
Test_AUC <- append(Test_AUC, nnetAUC[1])
Models <- append(Models, "NNET")


############## Decision Trees ############

library(rpart)
cpGrid <- expand.grid(cp = seq(0,0.1, 0.005))
set.seed(210)
treeTune <- train(Type ~., data = Train,
                  method = "rpart",
                  tuneGrid = cpGrid,
                  metric = "ROC",
                  trControl = ctrl)

treeTune

plot(treeTune$finalModel)
text(treeTune$finalModel, cex = 0.6)

AccPlot(treeTune, 0.75, Test, "Tree")

treePred <- predict(treeTune, Test, type = "prob")
treePred <- ifelse(treePred$fake >= 0.75, "fake", "real")

table(treePred, Test$Type)

### Test error rate
mean(treePred != Test$Type)

### Importance
varImp(treeTune)
plot(varImp(treeTune))

### Test ROC
treeROC <- roc(response = treeTune$pred$obs,
                predictor = treeTune$pred$fake,
                levels = rev(levels(treeTune$pred$obs)))
plot(treeROC, legacy.axes = TRUE)
treeAUC <- auc(treeROC)


### Save results

Test_Acc <- append(Test_Acc, 1 - mean(treePred != Test$Type))
Test_Error <- append(Test_Error, mean(treePred != Test$Type))
Test_AUC <- append(Test_AUC, treeAUC[1])
Models <- append(Models, "Tree")






############ Random Forest #############

mtryGrid <- data.frame(mtry = floor(seq(1, 10)))

set.seed(210)
rfTune <- train(Type ~., data = Train,
                method = "rf",
                tuneGrid = mtryGrid,
                ntree = 150,
                nodesize = 50,
                importance = TRUE,
                metric = "ROC",
                trControl = ctrl)
rfTune


AccPlot(rfTune, 0.75, Test, "Random Forest")

rfPred <- predict(rfTune, Test, type = "prob")
rfPred <- ifelse(rfPred$fake >= 0.75, "fake", "real")

table(rfPred, Test$Type)

### Test error rate
mean(rfPred != Test$Type)

### Importance
varImp(rfTune)
plot(varImp(rfTune))

### Test ROC
rfROC <- roc(response = rfTune$pred$obs,
               predictor = rfTune$pred$fake,
               levels = rev(levels(rfTune$pred$obs)))
plot(rfROC, legacy.axes = TRUE)
rfAUC <- auc(rfROC)


### Save results

Test_Acc <- append(Test_Acc, 1 - mean(rfPred != Test$Type))
Test_Error <- append(Test_Error, mean(rfPred != Test$Type))
Test_AUC <- append(Test_AUC, rfAUC[1])
Models <- append(Models, "RF")



########## Plots #############
testResults <- data.frame(Test_Acc,Test_Error,Test_AUC,Models)




### ROC Curves
par(pty = "s")
plot(logicROC, lty = 1, lwd = 3.5, 
     main = "ROC Curves by Model")
lines(rfROC, col = "red", lty = 3, lwd = 3.5)
lines(nnetROC, col = "blue", lty = 3, lwd = 3.5)
lines(treeROC, col = "green", lty = 3, lwd = 3.5)

legend("bottomright",
       legend = as.character(c("Logistic","RF","NNET","TREE")), 
       lty = c(1,3,3,3),
       lwd = c(3.5,3.5,3.5,3.5),
       col = c("black","red","blue","green"))


dev.off()

### Test error rate, sorted
OrderErr <- testResults[order(testResults$Test_Error),]

bpErr <- barplot(OrderErr$Test_Error, names.arg =  OrderErr$Models,
                 xlab = "Models", ylab = "Test Error Rate",
                 main = "Test Error Rates by Model")
text(bpErr, OrderErr$Test_Error - 0.005, 
     labels = round(OrderErr$Test_Error, 3))



### AUC, sorted
OrderAUC <- testResults[order(testResults$Test_AUC),]

bpAUC <- barplot(OrderAUC$Test_AUC, names.arg =  OrderAUC$Models,
                 xlab = "Models", ylab = "Area Under the Curve",
                 main = "AUC by Model")
text(bpAUC, OrderAUC$Test_AUC - 0.02, 
     labels = round(OrderAUC$Test_AUC, 3))


### Accuracy, sorted
OrderAcc <- testResults[order(testResults$Test_Acc),]

bpAcc <- barplot(OrderAcc$Test_Acc, names.arg =  OrderAcc$Models,
                 xlab = "Models", ylab = "Test Accuracy Rate",
                 main = "Test Accuracy Rate by Model")
text(bpAcc, OrderAcc$Test_Acc - 0.02, 
     labels = round(OrderAcc$Test_Acc, 3))




### Resamples
resamp <- resamples(list(Logistic = logicTune,
                    NNET = nnetTune,
                    Tree = treeTune,
                    RF = rfTune))
dotplot(resamp, metric = "ROC")


modelDifferences <- diff(resamp)
modelDifferences$statistics$ROC



library(highcharter)

highchart() %>%
  hc_yAxis_multiples(
    list(lineWidth = 3, lineColor = "red", title=list(text="Error")),
    list(lineWidth = 3, lineColor = "green", title=list(text="Accuracy")),
    list(lineWidth = 3, lineColor = "blue", title=list(text="AUC"))
  ) %>%
  hc_add_series(data = testResults$Test_Error, color = "red", type = "column") %>%
  hc_add_series(data = testResults$Test_Acc, color = "green", type = "column", 
                yAxis = 1) %>%
  hc_add_series(data = testResults$Test_AUC, color = "blue", type = "column", 
                yAxis = 2) %>%
  hc_xAxis(categories = testResults$Models, title = list(text = "Models"))






















