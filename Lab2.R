travelled_abroad = read.csv("E:\\Sanket Disale\\2 nd Year\\Second Sem\\DS\\LabDataSet\\Expt_2_Data_set_Travelled_abroad_csv.csv")

# 1.  Find out the %age of Indians in the sample who have travelled abroad using the data source.
p = nrow(travelled_abroad[which(travelled_abroad$Travelledabroad == "Y"),])/nrow(travelled_abroad)*100 
print(p)

# 2.a) What is the probability that in a randomly chosen sample of 10 persons, no one has travelled abroad?
p = nrow(travelled_abroad[which(travelled_abroad$Travelledabroad
                              == "Y"),])/nrow(travelled_abroad)
a = (factorial(10)/(factorial(0)*factorial(10))*(p**0)*((1-                                                          p)**10))
dbinom(0,10,p)
ps = c(a)

# 2.b) What is the probability that in a randomly chosen sample of 10 persons, exactly one has travelled abroad?
a = (factorial(10)/(factorial(1)*factorial(9))*(p**1)*((1-p)**9))
dbinom(1,10,p)
ps = append(ps, a)

# 2.c) What is the probability that in a randomly chosen sample of 10 persons, exactly two persons have travelled abroad?
p = (factorial(10)/(factorial(2)*factorial(8))*(p**2)*((1-p)**8))
dbinom(2,10,p)
ps = append(ps, a)

# 2.d) What is the probability that in a randomly chosen sample of 10 persons, exactly three persons have travelled abroad?
a = (factorial(10)/(factorial(3)*factorial(7))*(p**3)*((1-p)**7))
dbinom(3,10,p)
ps = append(ps, a)

# 2.e) What is the probability that in a randomly chosen sample of 10 persons, exactly four persons have travelled abroad?
a = (factorial(10)/(factorial(4)*factorial(6))*(p**4)*((1-p)**6))
dbinom(4,10,p)
ps = append(ps, a)

# 2.f) What is the probability that in a randomly chosen sample of 10 persons, exactly five persons have travelled abroad.
a = (factorial(10)/(factorial(5)*factorial(5))*(p**5)*((1-p)**5))
dbinom(5,10,p)
ps = append(ps, a)

# 2.g) What is the probability that in a randomly chosen sample of 10 persons, exactly six persons have travelled abroad?
a = (factorial(10)/(factorial(6)*factorial(4))*(p**6)*((1-p)**4))
dbinom(6,10,p)
ps = append(ps, a)

# 2.h) What is the probability that in a randomly chosen sample of 10 persons, exactly seven persons have travelled abroad?
a = (factorial(10)/(factorial(7)*factorial(3))*(p**7)*((1-p)**3))
dbinom(7,10,p)
ps = append(ps, a)

# 2.i) What is the probability that in a randomly chosen sample of 10 persons, exactly eight persons have travelled abroad?
a = (factorial(10)/(factorial(8)*factorial(2))*(p**8)*((1-p)**2))
dbinom(8,10,p)
ps = append(ps, a)

# 2.j) What is the probability that in a randomly chosen sample of 10 persons, exactly nine persons have travelled abroad?
a = (factorial(10)/(factorial(9)*factorial(1))*(p**9)*((1-p)**1))
dbinom(9,10,p)
ps = append(ps, a)

# 2.k)What is the probability that in a randomly chosen sample of 10 persons, all 10 persons have travelled abroad?
a = (factorial(10)/(factorial(10)*factorial(0))*(p**10)*((1-p)**0))
dbinom(10,10,p)
ps = append(ps, a)

# 3 ) Plot the probability values as a Table / Bar graph/plot and interpret plot.

seq = 0:10

plot(x=seq, y=ps, ylabel="Probability that they travel abroad", 
     xlabel="Count of People in group of 10", col="red",type="o")

barplot(ps,names.arg = seq, ylabel="Probability that they travel 
abroad", xlabel="Count of People in group of 10", col="blue")

table = data.frame(seq,ps)
print(table)

ans = pbinom(59, 100, p)
b1 = dbinom(0:100,100,p)
plot(b1,type="o")
print(ans)




