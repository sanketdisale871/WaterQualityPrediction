data = read.csv("E:\\Sanket Disale\\2 nd Year\\Second Sem\\DS\\LabDataSet\\Expt_6_Dataset_wisconsinn breast cancerdata_csv.csv")
head(data,5) # Printing First five data sets
data = data[,-1]
data
data = na.omit(data)

library(class)
normalize = function(x){
  return ((x-min(x))/ (max(x)-min(x)))
}

datanew = as.data.frame(lapply(data[2:30],
                               normalize))
datanew = na.omit(datanew)

dim(datanew)
traindata = datanew[1:398,]
testdata = datanew[399:569,]

traindatalabels = data[1:398,1]
testdatalabels = data[399:569,1]

model_21 = knn(train=traindata, test = testdata,cl=traindatalabels, k=23)
s = data.frame(table(testdatalabels,model_21))
table(model_21)

#Accuracy = (TP+TN/Total)*100
accuracy = ((130+37)/(171))*100
accuracy
#Precision = (TP/TP+FP)*100
precision = ((130)/(130+2))*100
precision
#Sensitivity = (TP/TP+FN)*100
recall = sensitivity = ((130)/(130+2))*100
sensitivity
#specificity = (TN/TN+FP)*100
specificity = ((37)/(2+37))*100
specificity
