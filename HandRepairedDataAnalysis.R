# Hand Repair Completed #
# Load Hand Repaired Data#
library(foreign)
RepairedData <- read.dta("handrepairedcasecontroldataset.dta")
CaseControl <- read.dta("copycasecontroldesignateddeidstata12.dta")
# There are two variables "Admit_date" and "admit_date" in RepairedData. Compare values#
# Records 44, 47-55, 103, 247,393 have another format result in NA#

RepairedData[c(44,47:55,103,247,393),c(6,177)]
RepairedData[c(44,47:55,103,247,393),177] <- as.Date(RepairedData[c(44,47:55,103,247,393),6],"%d%b%Y")

# The following method changes values in Admit_date
# RepairedData[103,c(6,177)]
# RepairedData[103,6] <- "4/6/2011"
# RepairedData[103,177] <- as.Date("2011-04-06")

# RepairedData[247,c(6,177)]
# RepairedData[247,6] <- "5/5/2008"
# RepairedData[247,177] <- as.Date("2008-05-05")

# RepairedData[393,c(6,177)]
# RepairedData[393,6] <- "3/20/2012"
# RepairedData[393,177] <- as.Date("2012-03-20")

# Isolate case1control0, deid, admit_date, discharge_date from RepairedData
# Obtain handrepaired dataset starting at n=57 to end…
Select1 <- RepairedData[57:1280 ,c("deid","case1control0","admit_date","discharge_date")]
# Isolate deid  admit_date discharge_date controleligible caseeligible from CaseControl dataset
Select2 <- CaseControl[, c("deid", "caseeligible", "controleligible", "admit_date", "discharge_date")]
# Merge the two dataset by deid, admit_date and discharge_date
MergedData <- merge(Select1, Select2, by=c("deid","admit_date", "discharge_date")) 

# Outcome

# 1224 rows in Hand Repaired Data, 33211 rows in CaseControl data
# 1210 rows in Merged Data. 14 rows are dumped by merging.
# I want to know which rows are excluded from Hand Repaired Data
which(Select1$deid %in% MergedData$deid == FALSE)
dumped <- which(Select1$deid %in% MergedData$deid == FALSE)
Select1[dumped,]
# deid in those dumped are NAs.

# confirm when case1control0 = 1, then it is caseeligible. When case1control0 = 0, it is controleligible.
# Pre-examine
# Examine the values of caseeligible and controleligible. One is 1 and other should be NA.

table(MergedData$caseeligible, MergedData$controleligible) 
# There is no 1,1 combination. Good!

which(is.na(MergedData$caseeligible) == is.na(MergedData$controleligible))
# There is no NA, NA combination. Good!

table(MergedData$case1control0)
which(is.na(MergedData$case1control0))
# There is no NAs in case1control0. 605 controls vs. 605 cases

MergedData$eligible <- ifelse((MergedData$caseeligible == 1) & (is.na(MergedData$caseeligible) == FALSE), 1, 0)

# compare the two columns of case1control0 and eligible
which(MergedData$case1control0 != MergedData$eligible)
# No result. Excellent! columns of caseeligible and controleligible are in accordance with the case1control0 column
