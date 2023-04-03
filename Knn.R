data = read.csv("E:\\Sanket Disale\\2 nd Year\\Second Sem\\DS\\LabDataSet\\Expt_5_Data_Set_knn1_csv.csv")

install.packages("class")
library(class)

traindata = as.data.frame(data[,2:3])
traindatalabels = data[1:18,4]
testdata = data.frame("x"=(3),"y"=(2))
testdata
traindata
length(traindata)

knn_1 = knn(train = traindata,test = testdata,cl=traindatalabels,k=1)
cat("Predicted Class is : ",knn_1)

knn_5 = knn(train=traindata,testdata,cl=traindatalabels,k=5)

knn_7 = knn(train = traindata,test = testdata,cl=traindatalabels,k=7)

cat("Predicted Class is : ",knn_5)
cat("Predicted Class is : ",knn_7)

# Calculating Distance from (3,2)

distance = sqrt((data$x-3)**2 + (data$y -2)**2)
distance

#Radius
newd = cbind(data,distance)
newd

radius = 1.45

newd = newd[which(newd$distance <=radius),]
newd

count1 = nrow(newd[which(newd$class==1),])
count2 = nrow(newd[which(newd$class==2),])
count3 = nrow(newd[which(newd$class==3),])
if(count1>count2){
  if(count1>count3){
    ans = 1
  }else{
    ans = 3
  }
}else{
  if(count2>count3){
    ans = 2
  }else{
    ans = 3
  }
}

cat("Predicted Class is:",ans)
