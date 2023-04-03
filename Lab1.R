pollutant = read.csv("E:\\Sanket Disale\\2 nd Year\\Second Sem\\DS\\LabDataSet\\Expt _1_Dataset_3_pollutant_csv.csv")

print(pollutant)

boxplot(pollutant$Day)

boxplot(pollutant$Day,
        main ="Pollutant Day",
        xlab ="X",
        ylab ="Y",
        col ="Red",
        horizontal = FALSE,
        notch = FALSE
)

# Cleaning the file
pollutant$Ozone[is.na(pollutant$Ozone)] = median(na.omit(pollutant$Ozone))
pollutant$Solar.R[is.na(pollutant$Solar.R)]=median(na.omit(pollutant$Solar.R))
 
# Cleaning data in CSV file
pollutant$Ozone[is.na(pollutant$Ozone)]=
  median(na.omit(pollutant$Ozone))

pollutant$Solar.R[is.na(pollutant$Solar.R)]=
  median(na.omit(pollutant$Solar.R))

# 1.What is the mean of “Temp” when “Month” is equal to 6?
print(mean(pollutant[which(pollutant$Month==6),]$Temp))


# 2.How many observations are there in the given data?
nrow(pollutant)


# 3.Print last two rows of the data.
print(tail(pollutant,2))

# 4.What is the value of Ozone in 47th row?
print(pollutant$Ozone[47])

# 5.How many values are missing in Ozone column?
print(sum((is.na(pollutant$Ozone))))


#6 .What is the mean of Ozone column excluding missing values?
print(mean(na.omit(pollutant$Ozone)))

#7 Extract the subset of rows of the data frame where Ozone values are above 31 and Temp values are above 90. What is the mean of Solar.R in this subset?
newsubset = subset(pollutant,Ozone >31 & Temp >90)
print(mean(pollutant$Solar.R))

#8 What was the maximum ozone value in the month of May (i.e., Month is equal to 5)?
print(max(pollutant[which(pollutant$Month ==5),]$Ozone))


# Consider the “Hair Eye Color” data set.

eyecolor = read.csv("E:\\Sanket Disale\\2 nd Year\\Second Sem\\DS\\LabDataSet\\Expt1_Datset_2_hair_eye_color_csv.csv")

print(head(eyecolor,2))

#9. How many people have brown eye color?
print(nrow(eyecolor[which(eyecolor$Eye.Color=="Brown"),]))


# 10.How many people have blonde hair?
print(nrow(eyecolor[which(eyecolor$Hair.Color=="Blonde"),]))


#11.How many Brown-haired people have Black eyes?
print(nrow(eyecolor[which(eyecolor$Hair.Color == "Brown" & eyecolor$Eye.Color=="Black"),]))


#12. What is the percentage of people with green eyes?
print(nrow(eyecolor[which(eyecolor$Eye.Color=="Green"),])/nrow(eyecolor)*100)


#13. What percentage of people have red hair and blue eyes?
print(nrow(eyecolor[which(eyecolor$Hair.Color=="Red" & eyecolor$Eye.Color=="Blue"),])/nrow(eyecolor)*100)

# Consider the “Germination” data set.
germination = read.csv("E:\\Sanket Disale\\2 nd Year\\Second Sem\\DS\\LabDataSet\\Expt1_ Dataset3_germination_csv.csv")

#14. What is the average number of seeds germinated for the uncovered boxes with level of watering equal to 4?
print(sum(germination[which(germination$Box == "Uncovered" & germination$water_amt ==4),]$germinated)/nrow(germination[which(germination$Box == "Uncoverd" & germination$water_amt ==4)]))

#15. What is the median value for the data covered boxes?
print(median(germination[which(germination$Box=="Covered"),]$germinated))

#16. Establish conclusions on the basis of available data and write them in the conclusion part.
# 16.a. Association of levels of watering with the number of germinating seeds in case of covered boxes as well as uncovered boxes.
# 16.b.Association of number of germinating seeds with the fact that the boxes were covered or uncovered.

print(sum(germination[which(germination$water_amt ==1),]$germinated)/nrow(germination[which(germination$water_amt==1),]))
print(sum(germination[which(germination$water_amt ==2),]$germinated)/nrow(germination[which(germination$water_amt==2),]))
print(sum(germination[which(germination$water_amt ==3),]$germinated)/nrow(germination[which(germination$water_amt==3),]))
print(sum(germination[which(germination$water_amt ==4),]$germinated)/nrow(germination[which(germination$water_amt==4),]))
print(sum(germination[which(germination$water_amt ==5),]$germinated)/nrow(germination[which(germination$water_amt==5),]))
print(sum(germination[which(germination$water_amt ==6),]$germinated)/nrow(germination[which(germination$water_amt==6),]))

print(sum(germination[which(germination$Box =="Covered"),]$germinated)/nrow(germination[which(germination$Box == "Covered"),]))
print(sum(germination[which(germination$Box =="Uncovered"),]$germinated)/nrow(germination[which(germination$Box == "Uncovered"),]))

# Write a single R code

#17. To display the Boxplot for sepal length of iris data set as shown below
boxplot(Sepal.Length~Species, dat = iris,
        col=c("green","yellow","blue"), notch==FALSE,
        main=="IRIS_BOXPLOT")


# 18. To display the Scatter plot for murders data set present in “dslabs” package as shown below. Give proper title, x, y axis label etc. to each plot.
library(dslabs)
print(murders)
plot(x=murders$population, y=murders$total,
     xlab="Population", ylab="Total Murders",col="red",
     main="Total murders Vs Population")

