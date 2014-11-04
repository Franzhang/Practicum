# make all characters lowercase
load("C:\\Users\\zhago7\\Documents\\PatientData.RData")
MedCol <- seq(77,164, by = 3)
YFLowercase <- as.data.frame(sapply(YF, tolower)) 

# all values for medication names
which(names(YF)== "Medication1Name")

sink("MedicationNamesLowercase.txt")
table(as.vector(as.matrix(YFLowercase[,MedCol])))
sink()

################################################################
# Load Medication Name consolidated data
load("C:\\Users\\zhago7\\Documents\\NameConsolidatedData.RData")
which(names(YF) == "Medication1NameNew") # 175
MedCol <- seq(175,204, by = 1)

sink("ConsolidatedMetaMedicationNames.txt")
# all values for medication names
table(as.vector(as.matrix(YF[,MedCol])))
sink()
