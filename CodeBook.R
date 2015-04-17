# Code Book for logistic regression model #
# Total Scheduled Doses From Hand Repaired Data #
# totalscheduleddoses and totalprndoses
# Load Hand Repaired Data#
library(foreign)
RepairedData <- read.dta("handrepairedcasecontroldataset.dta")
# Dump the invalid rows 
dump <- which(is.na(RepairedData$pair) == TRUE)
RepairedData <- RepairedData[-dump, ]
# 1230 rows 177 columns are valid

for(i in 1:30){
timesdaily <- paste("Medication", i, "TimesDaily", sep = "")
prn <- paste("Medication", i, "Prn", sep = "")
freq <- paste("Medication", i, "Freq", sep = "")

RepairedData[[timesdaily]][RepairedData[[freq]] == "< once a day"] <- 0
RepairedData[[timesdaily]][RepairedData[[freq]] == "less than once daily"] <- 0
RepairedData[[timesdaily]][RepairedData[[freq]] == "BID/Q12H"] <- 2
RepairedData[[timesdaily]][RepairedData[[freq]] == "BID/Q12H+prn"] <- 2
RepairedData[[timesdaily]][RepairedData[[freq]] == "Q2H"] <- 12
RepairedData[[timesdaily]][RepairedData[[freq]] == "Q2H+prn"] <- 12
RepairedData[[timesdaily]][RepairedData[[freq]] == "Q4H"] <- 6
RepairedData[[timesdaily]][RepairedData[[freq]] == "Q4H+prn"] <- 6
RepairedData[[timesdaily]][RepairedData[[freq]] == "QAC"] <- 3
RepairedData[[timesdaily]][RepairedData[[freq]] == "QAC+prn"] <- 3
RepairedData[[timesdaily]][RepairedData[[freq]] == "Qd/Qday/Q24HR/QAM/QHS"] <- 1
RepairedData[[timesdaily]][RepairedData[[freq]] == "Qd/Qday/Q24HR/QAM/QHS+prn"] <- 1
RepairedData[[timesdaily]][RepairedData[[freq]] == "QID/6H"] <- 4
RepairedData[[timesdaily]][RepairedData[[freq]] == "QID/6H+prn"] <- 4
RepairedData[[timesdaily]][RepairedData[[freq]] == "TID/Q8"] <- 3
RepairedData[[timesdaily]][RepairedData[[freq]] == "TID/Q8+prn"] <- 3


RepairedData[[prn]][RepairedData[[freq]] == "< once a day"] <- 0
RepairedData[[prn]][RepairedData[[freq]] == "less than once daily"] <- 0
RepairedData[[prn]][RepairedData[[freq]] == "BID/Q12H"] <- 0
RepairedData[[prn]][RepairedData[[freq]] == "BID/Q12H+prn"] <- 1
RepairedData[[prn]][RepairedData[[freq]] == "Q2H"] <- 0
RepairedData[[prn]][RepairedData[[freq]] == "Q2H+prn"] <- 1
RepairedData[[prn]][RepairedData[[freq]] == "Q4H"] <- 0
RepairedData[[prn]][RepairedData[[freq]] == "Q4H+prn"] <- 1
RepairedData[[prn]][RepairedData[[freq]] == "QAC"] <- 0
RepairedData[[prn]][RepairedData[[freq]] == "QAC+prn"] <- 1
RepairedData[[prn]][RepairedData[[freq]] == "Qd/Qday/Q24HR/QAM/QHS"] <- 0
RepairedData[[prn]][RepairedData[[freq]] == "Qd/Qday/Q24HR/QAM/QHS+prn"] <- 1
RepairedData[[prn]][RepairedData[[freq]] == "QID/6H"] <- 0
RepairedData[[prn]][RepairedData[[freq]] == "QID/6H+prn"] <- 1
RepairedData[[prn]][RepairedData[[freq]] == "TID/Q8"] <- 0
RepairedData[[prn]][RepairedData[[freq]] == "TID/Q8+prn"] <- 1

}

# Now start the loop for all patients
RepairedData$TotalScheduledDoses <- 0
for(i in 1:1230){
  for ( j in seq(178, 236, 2)){
    if((RepairedData[i, j+1] == 0) && (!is.na(RepairedData[i, j+1]))){
      RepairedData$TotalScheduledDoses[i] <- RepairedData$TotalScheduledDoses[i] + as.integer(RepairedData[i,j])
    }
  }
}

# Generate "totalprndoses"
RepairedData$TotalPrnDoses <- 0
for(i in 1:1230){
  for ( j in seq(178, 236, 2)){
    if((RepairedData[i, j+1] == 1) && (!is.na(RepairedData[i, j+1]))){
      RepairedData$TotalPrnDoses[i] <- RepairedData$TotalPrnDoses[i] + as.integer(RepairedData[i,j])
    }
  }
}

# NewTech from original dataset

RepairedData$NewTech <- rep(NA, 1230)
for(i in 1:1230){
  if(!is.na(RepairedData$PICCnew_this_admission[i])
     && RepairedData$PICCnew_this_admission[i] == TRUE
     && RepairedData$PICC_removed[i] == FALSE){
    RepairedData$NewTech[i] <- 1
  }
  else if(!is.na(RepairedData$tracheostomyThisadmission[i])
          && RepairedData$tracheostomyThisadmission[i] == TRUE){
    RepairedData$NewTech[i] <- 1
  } 
  else if(!is.na(RepairedData$foley_drain_placedthisadmission[i])
          && RepairedData$foley_drain_placedthisadmission[i] == TRUE
          && RepairedData$Indwelling_foley_other_drainatd[i] == TRUE){
    RepairedData$NewTech[i] <- 1
  }
  else if(!is.na(RepairedData$Gtube_placedThisadmission[i])
          && RepairedData$Gtube_placedThisadmission[i] == TRUE
          && RepairedData$Gtube_Duodenaltube_JtubeatDisch[i] == TRUE){
    RepairedData$NewTech[i] <- 1
  }
  else if(!is.na(RepairedData$VP_shunt_newthisadmission[i])
          && RepairedData$VP_shunt_newthisadmission[i] == TRUE
          && RepairedData$VP_shunt_atdischarge[i] == TRUE){
    RepairedData$NewTech[i] <- 1
  }
  else if(RepairedData$Ngtubethisadmission[i] == TRUE
          && RepairedData$NG_NJ_NDatdischarge[i] == TRUE){
    RepairedData$NewTech[i] <- 1
  }
  else RepairedData$NewTech[i] <- 0
}


# Create variable AnyTech
# intialize new variable
RepairedData$AnyTech <- rep(NA, 1230)

## Start loop ##
for(i in 1:1230){
  if(!is.na(RepairedData$PICC_cline_port[i])
     && RepairedData$PICC_cline_port[i] == TRUE
     && !is.na(RepairedData$PICC_removed[i])
     && RepairedData$PICC_removed[i] == FALSE){
    RepairedData$AnyTech[i] <- 1
  } 
  else if(!is.na(RepairedData$Tracheostomy[i])
          && RepairedData$Tracheostomy[i] == TRUE){
    RepairedData$AnyTech[i] <- 1
  }
  else if(!is.na(RepairedData$Indwelling_foley_other_drainatd[i])
          && RepairedData$Indwelling_foley_other_drainatd[i] == TRUE){
    RepairedData$AnyTech[i] <- 1
  }
  else if(!is.na(RepairedData$Gtube_Duodenaltube_JtubeatDisch[i])
          && RepairedData$Gtube_Duodenaltube_JtubeatDisch[i] == TRUE){
    RepairedData$AnyTech[i] <- 1
  }
  else if(!is.na(RepairedData$VP_shunt_atdischarge[i])
          && RepairedData$VP_shunt_atdischarge[i] == TRUE){
    RepairedData$AnyTech[i] <- 1
  }
  else if(RepairedData$NG_NJ_NDatdischarge[i] == TRUE){
    RepairedData$AnyTech[i] <- 1
  }
  else RepairedData$AnyTech[i] <- 0
}


# Collinearity issue between NewTech and AnyTech
RepairedData$Tech <- ifelse(RepairedData$NewTech == 0 & RepairedData$AnyTech == 0, 0, 
                            ifelse(RepairedData$NewTech == 0 & RepairedData$AnyTech == 1, 1, 2)
                            )
RepairedData$Tech <- as.factor(RepairedData$Tech)

# Create variable AnyAppointmentScheduled. Value(1/0)
# Assign values
RepairedData$AnyApptSched <- ifelse(RepairedData[[171]]==0 & RepairedData[[172]]==0, 0, 1)

# Rename these variables for easy handling
colnames(RepairedData)[3] <- "case"
colnames(RepairedData)[171] <- "PrimaryCare" 
colnames(RepairedData)[172] <- "SubspecialityAppointment"
colnames(RepairedData)[173] <- "SuggestedPrimaryCare" 
colnames(RepairedData)[174] <- "SuggestedSubspeciality" 

##############
# Tidy Data #
##############
library(survival)
yf11 <- clogit(case ~ TotalScheduledDoses + TotalPrnDoses + Tech + AnyApptSched + Washomehealthcareinvolvedat + TPN_PPN + strata(pair), data = RepairedData)
summary(yf11)
yf111 <- clogit(case ~ TotalScheduledDoses + TotalPrnDoses + NewTech + AnyTech + AnyApptSched + Washomehealthcareinvolvedat + TPN_PPN + strata(pair), data = RepairedData)
summary(yf111)
pred <- predict(yf11, type = "expected")
library(ResourceSelection)
hoslem.test(RepairedData[-c(1:20), "case"], pred, g = 10)
