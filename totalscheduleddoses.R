# totalscheduleddoses and totalprndoses
load("C:\\Users\\zhago7\\Documents\\YF1.RData")
# Data Object "YF1" created
YF1$totalscheduleddoses <- rep(0,1250)
which(colnames(YF1)== "Medication1timesdaily")
# isolate prn == 0
# start with Patient1
for ( j in seq(177, 235, 2)){
  if((YF1[1, j+1] == 0) && (!is.na(YF1[1, j+1]))){
    YF1$totalscheduleddoses[1] <- YF1$totalscheduleddoses[1] + as.integer(YF1[1,j])
  }
  print(YF1$totalscheduleddoses[1])
}

nrow(YF1)
# Now start the loop for all patients
for(i in 1:1250){
  for ( j in seq(177, 235, 2)){
    if((YF1[i, j+1] == 0) && (!is.na(YF1[i, j+1]))){
      YF1$totalscheduleddoses[i] <- YF1$totalscheduleddoses[i] + as.integer(YF1[i,j])
    }
  }
}

# Generate "totalprndoses"
YF1$totalprndoses <- rep(0,1250)
for(i in 1:1250){
  for ( j in seq(177, 235, 2)){
    if((YF1[i, j+1] == 1) && (!is.na(YF1[i, j+1]))){
      YF1$totalprndoses[i] <- YF1$totalprndoses[i] + as.integer(YF1[i,j])
    }
  }
}
# Save the dataset YF1 to YF2.RData
# plot histograms
par(mfrow = c(2,2))
hist(YF2$totalscheduleddoses, main = "Histogram of Total Scheduled Doses",
xlab = "Total Scheduled Doses (24 hours)", col = "mistyrose")
hist(YF2$totalprndoses, main = "Histogram of Total p.r.n. Doses",
xlab = "Total PRN Doses", col = "coral")
boxplot(YF2$totalscheduleddoses, main = "Boxplot of Total Scheduled Doses",
ylab = "Total Scheduled Doses", col = "mistyrose")
boxplot(YF2$totalprndoses, main = "Boxplot of Total p.r.n. Doses",
ylab = "Total PRN Doses", col = "coral")
