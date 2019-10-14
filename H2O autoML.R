###h2o model
library(h2o)
library(mltools)
h2o.init()
set.seed(164)
h2o.no_progress()
start_time <- Sys.time()

trainPath = "C:/Users/melte/OneDrive/uygulama/cervical/2featureSelection/IG_out/tr_cer_IG"
testPath = "C:/Users/melte/OneDrive/uygulama/cervical/2featureSelection/IG_out/ts_cer_IG"
for (rpt in 1:3) {
  print(paste(c("Baslayan_Calisma:", rpt), collapse = ""))
  tr.data <- read.csv(paste(c(trainPath, rpt, ".csv"),collapse = ""), header = T)
  row.names(tr.data) <- (tr.data[,1])
  tr.data <- tr.data[,-1]
  cond.tr <- factor(c(rep("g1",21), rep("g2", 21)))
  
  tr_dataH2o = cbind(cond.tr,tr.data)
  tr_dataH2o = as.h2o(tr_dataH2o)
  zebra_aml <- h2o.automl(y = "cond.tr", training_frame=tr_dataH2o,
                          max_runtime_secs=36)
  ts.data <- read.csv(paste(c(testPath, rpt, ".csv"),collapse = ""), header = T)
  row.names(ts.data) <- (ts.data[,1])
  ts.data <- ts.data[,-1]
  cond.tr <- factor(c(rep("g1",8), rep("g2", 8)))
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
  } else if (length(summary(cond.tr)) == 3) {
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
}
end_time <- Sys.time()
elapsed <- end_time - start_time
print(elapsed)
