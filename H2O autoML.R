###h2o model
library(h2o)
library(mltools)
h2o.init()
set.seed(164)
h2o.no_progress() # I don't want to see the progress line
start_time <- Sys.time()
#C:/Users/melte/OneDrive/uygulama/deng/2featureSelection/IG_out
trainPath = "C:/Users/melte/OneDrive/uygulama/deng/2featureSelection/IG_out/tr_den_IG"
testPath = "C:/Users/melte/OneDrive/uygulama/deng/2featureSelection/IG_out/ts_den_IG"
for (rpt in 1:3) {

  print(paste(c("Baslayan_Calisma:", rpt), collapse = ""))
  tr.data <- read.csv(paste(c(trainPath, rpt, ".csv"),collapse = ""), header = T)
  row.names(tr.data) <- (tr.data[,1])
  tr.data <- tr.data[,-1]
  cond.tr <- factor(c(rep("g1",35), rep("g2", 10), rep("g3",26),rep("g4", 6), rep("g5",31),
                      rep("g6",7), rep("g7",21), rep("g8", 9), rep("g9",42), rep("g10", 3)))
  
  tr_dataH2o = cbind(cond.tr,tr.data)
  tr_dataH2o = as.h2o(tr_dataH2o)
  zebra_aml <- h2o.automl(y = "cond.tr", training_frame=tr_dataH2o,
                          max_runtime_secs=36)
  ts.data <- read.csv(paste(c(testPath, rpt, ".csv"),collapse = ""), header = T)
  row.names(ts.data) <- (ts.data[,1])
  ts.data <- ts.data[,-1]
  cond.tr <- factor(c(rep("g1",15), rep("g2", 4), rep("g3",11),rep("g4", 2), rep("g5",12),
                      rep("g6",3), rep("g7",9), rep("g8", 3), rep("g9", 18), rep("g10", 1)))
  ts_dataH2o = cbind(cond.tr,ts.data)
  ts_dataH2o <- as.h2o(ts_dataH2o)
  p1 = h2o.performance(zebra_aml@leader, ts_dataH2o)
  
  if(length(summary(cond.tr)) == 2){
    ##Confusion Matrix
    cm <- cbind(p1@metrics$cm$table$g1,p1@metrics$cm$table$g2)[-3,]#binary verilerde
    cm <- t(cm)
    print(cm)
    cm <- as.table(cm)
    h2oConfusion <- confusionMatrix(cm, mode = "everything")
    ##MCC 
    MCC <- mcc(confusionM = as.matrix(h2oConfusion))
    print(paste(c("MCC: ", MCC), collapse = ""))
    ## Balanced Accuracy
    print(h2oConfusion$byClass["Balanced Accuracy"])
    ## F1 Score
    print(h2oConfusion$byClass["F1"])
  } 
  else if (length(summary(cond.tr)) == 3) {
    ##Confusion Matrix
    cm <- cbind(p1@metrics$cm$table$g1,p1@metrics$cm$table$g2, 
                p1@metrics$cm$table$g3)[-4,]
    cm <- t(cm)
    print(cm)
    ##MCC 
    MCC <- mcc(confusionM = cm)
    print(paste(c("MCC: ",MCC),collapse = ""))
    ##Balanced Accuracy
    cm <- as.table(cm)
    h2oConfusion <- confusionMatrix(cm, mode = "everything")
    print("Balanced_Accuracy_for_per_class")
    print(h2oConfusion[["byClass"]][,"Balanced Accuracy"])
    ##Weighted Balanced Accuracy
    balAcc <- h2oConfusion[["byClass"]][,"Balanced Accuracy"]
    countPerGroup <- as.vector(summary(cond.tr))
    weightBalAcc <- (sum(balAcc*countPerGroup))/(length(cond.tr))
    print(paste(c("Weighted_Balanced_Accuracy: ", weightBalAcc), collapse = ""))
    ##F1 Score
    print("F1_skor_for_per_class")
    print(h2oConfusion[["byClass"]][,"F1"])
    ##Weighted F1 Score
    f <- h2oConfusion[["byClass"]][,"F1"]
    countPerGroup <- as.vector(summary(cond.tr))
    weightF1 <- (sum(f*countPerGroup))/(length(cond.tr))
    print(paste(c("WeightedF1: ", weightF1), collapse = ""))
  } 
  else if (length(summary(cond.tr)) == 10) {
    ##Confusion Matrix
    cm <- cbind(p1@metrics$cm$table$g1,p1@metrics$cm$table$g10,
                p1@metrics$cm$table$g2,p1@metrics$cm$table$g3,
                p1@metrics$cm$table$g4,p1@metrics$cm$table$g5,
                p1@metrics$cm$table$g6,p1@metrics$cm$table$g7,
                p1@metrics$cm$table$g8,p1@metrics$cm$table$g9)[-11,]
    cm <- t(cm) ## group order 1,10,2.3,..,9
    print(cm)
    ##MCC 
    MCC <- mcc(confusionM = cm)
    print(paste(c("MCC: ",MCC),collapse = ""))
    ##Balanced Accuracy
    cm <- as.table(cm)
    h2oConfusion <- confusionMatrix(cm, mode = "everything")
    print("Balanced_Accuracy_for_per_class")
    print(h2oConfusion[["byClass"]][,"Balanced Accuracy"])
    ##Weighted Balanced Accuracy
    balAcc <- h2oConfusion[["byClass"]][,"Balanced Accuracy"]
    countPerGroup <- as.vector(summary(cond.tr))
    weightBalAcc <- (sum(balAcc*countPerGroup))/(length(cond.tr))
    print(paste(c("Weighted_Balanced_Accuracy: ", weightBalAcc), collapse = ""))
    ##F1 Score
    print("F1_skor_for_per_class")
    print(h2oConfusion[["byClass"]][,"F1"])
    ##Weighted F1 Score
    f <- h2oConfusion[["byClass"]][,"F1"]
    countPerGroup <- as.vector(summary(cond.tr))
    weightF1 <- (sum(f*countPerGroup))/(length(cond.tr))
    print(paste(c("WeightedF1: ", weightF1), collapse = ""))
    
  }
}
end_time <- Sys.time()
elapsed <- end_time - start_time
print(elapsed)
