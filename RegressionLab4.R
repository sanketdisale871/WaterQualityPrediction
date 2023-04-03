#Simple Linear Regression 
toys = read.csv("E:\\Sanket Disale\\2 nd Year\\Second Sem\\DS\\LabDataSet\\Expt_4_Data Set_Toy_sales_csv.csv");

unitsales = toys$Unitsales
prices = toys$Price
simpReg = lm(prices~unitsales) # finding the relationsip between price(y) and unitsales(x)
summary(simpReg) # this will give the Result orientation like box plot

a = data.frame(unitsales=6900) # predicting price for unitsales = 6900
result = predict(simpReg,a)
print(result)

confint(simpReg) # printing confusion matrix
#par(mar=c(3,4))
plot(toys$Price,toys$Unitsales,main="Regression for price on unitsales",xlab="Price",ylab="UnitSales")
abline(lm(unitsales~prices)) # drawing the line on graph, of estimated result

# Multiple Linear Regression

plot(toys)
month = toys$Month
adexp = toys$Adexp
promexp = toys$Promexp
mulReg = lm(prices~unitsales+adexp+promexp)
confint(mulReg)
summary(mulReg)
