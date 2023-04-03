hypo_data = read.csv("E:\\Sanket Disale\\2 nd Year\\Second Sem\\DS\\LabDataSet\\Expt_3_ Dataset_Hypothesis_csv.csv")

m = mean(hypo_data$Life_Hrs)
cat("Mean : ",m)

s = sd(hypo_data$Life_Hrs)
cat("\nStandard Deviation L ",s)

p1 = pnorm(m,10000,(s/sqrt(nrow(hypo_data))))
cat("\nP value for the first case is : ",p1)

if(p1<0.05){
  cat("\nReject the claim of manufacturer for significance level 0.05")
}else{
  cat("\n Claim of Manufacturer cannot be rejected for significance level 0.05")
}

if(p1<0.01){
  cat("\nReject the Manufacute Claim for significance level 0.01")
}else{
  cat("\n Claim of Manufacturer cannot be rejected for significance level 0.01")
}

