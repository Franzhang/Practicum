load("C:\\Users\\zhago7\\Documents\\PatientData.RData")
# Make all values lowercase
YF <- as.data.frame(sapply(YF, tolower))
# consolidate all the values for MedicationNames
# Change factor to character
YF$Medication1NameNew <- as.character(YF$Medication1Name)
# assign NA to blank values
YF$Medication1NameNew <- ifelse(YF$Medication1NameNew == "", NA, YF$Medication1NameNew)

which(names(YF) == "Medication1NameNew") # 175
which(names(YF) == "Medication1Name") # 77
which(names(YF) == "Medication2Name") #80

load("C:\\Users\\zhago7\\Documents\\ValueTable.RData")
####################################################################################
# For the Medication1NameNew

for(i in 1:1250){
  for(j in 1:1103){
    if(!is.na(YF$Medication1NameNew[i]) && YF$Medication1NameNew[i] == ValueTable[j,1]){
      YF$Medication1NameNew[i] <- ifelse(!is.na(ValueTable[j,2]), ValueTable[j,2], YF$Medication1NameNew[i]) 
    }
  }  
}

# post check
c <- c(which(YF$Medication1Name != YF$Medication1NameNew))
YF[c,c("Medication1Name","Medication1NameNew")]

######################################################################################
sink("MedicationNamesChanged.txt")
# Do it from Medication1Name to Medication30Name.
for (k in 1:30){
  newcol <- paste("Medication", k, "NameNew", sep = "")
  # Change factor to character
  YF[[newcol]] <- as.character(YF[,74+3*k])
  # assign NA to blank values
  YF[[newcol]] <- ifelse(YF[[newcol]] == "", NA, YF[[newcol]])
  
  for(i in 1:1250){
    for(j in 1:1103){
      if(!is.na(YF[[newcol]][i]) && YF[[newcol]][i] == ValueTable[j,1]){
        YF[[newcol]][i] <- ifelse(!is.na(ValueTable[j,2]), ValueTable[j,2], YF[[newcol]][i]) 
      }
    }  
  }
  
  # post check
  c <- c(which(YF[[newcol]] != YF[,74+3*k]))
  print(YF[c,c(174+k,74+3*k)])
  
}
sink()
