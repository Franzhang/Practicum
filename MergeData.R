# Attempt of Merging data
# Organize Master Patient Data
load("Master.RData")
Master$AdmitDate <- as.Date(Master$AdmitDate,"%m/%d/%Y")
Master$DischargeDate <- as.Date(Master$DischargeDate, "%m/%d/%Y")
library(foreign)
IDdata <- read.dta("copycasecontroldesignateddeidstata12.dta")
# select the variables "admit_date", "discharge_date", "caseeligible", "controleligible" and "deid"
IDdata <- IDdata[,c("admit_date","discharge_date", "caseeligible", "controleligible", "deid")]

# import casecontrol
CaseControl <- read.dta("copycasesandcontrolsdeidfilestomergewithchartreviewstata12.dta")
CaseControl <- CaseControl[ ,c(1,2,4,5)]
############## Data Preparation DONE #############

load("Merging.RData") # CaseControl, IDdata, Master

a <- grep(Master$AdmitDate[1], IDdata$admit_date, fixed = TRUE)
b <- grep(Master$DischargeDate[1], IDdata$discharge_date, fixed = TRUE)
m <- match(a, b)
# Find the position
p <- m[which(!is.na(c))]
print(IDdata[b[p],])
id <- IDdata[b[p],"deid"]
cc <- ifelse(!is.na(IDdata[b[p],"caseeligible"]) 
             && IDdata[b[p],"caseeligible"] == 1, "deid", 
             ifelse(!is.na(IDdata[b[p],"controleligible"]) 
                    && IDdata[b[p],"controleligible"] == 1, "deid_control", NA)
)

grep(id, CaseControl[[cc]])

################ No match found ###############
a <- grep(Master$AdmitDate[2], IDdata$admit_date, fixed = TRUE)
b <- grep(Master$DischargeDate[2], IDdata$discharge_date, fixed = TRUE)
m <- match(a, b)
# Find the position
p <- as.integer(na.omit(b[m]))
print(IDdata[p,])
id <- IDdata[p ,"deid"]
if(length(p) == 1){
  cc <- ifelse(!is.na(IDdata[p,"caseeligible"]) 
               && IDdata[b[p],"caseeligible"] == 1, "deid", 
               ifelse(!is.na(IDdata[b[p],"controleligible"]) 
                      && IDdata[b[p],"controleligible"] == 1, "deid_control", NA)
  )
}
if(length(p) > 1) {
  
}
