n = sum(cm) # number of instances
nc = nrow(cm) # number of classes
rowsums = apply(cm, 1, sum) # number of instances per class
colsums = apply(cm, 2, sum) # number of predictions per class
diag = diag(cm)  # number of correctly classified instances per class 

precision = diag / colsums 
recall = diag / rowsums 
f1 = 2 * precision * recall / (precision + recall) 

print(" ************ Confusion Matrix ************")
print(cm)
print(" ************ Diag ************")
print(diag)
print(" ************ Precision/Recall/F1 ************")
print(data.frame(precision, recall, f1)) 
##  After that, you are able to find the macro F1.

macroPrecision = mean(precision)
macroRecall = mean(recall)
macroF1 = mean(f1)

print(" ************ Macro Precision/Recall/F1 ************")
print(data.frame(macroPrecision, macroRecall, macroF1)) 

## Weighted F1 Score (düzenlenecek)
weightF1 <- (sum(f*countPerGroup))/(length(cond.tr))

