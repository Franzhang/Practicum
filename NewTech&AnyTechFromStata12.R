# NewTech from original dataset
# Import data from Original Dataset
library(foreign)
RepairedData <- read.dta("handrepairedcasecontroldataset.dta")
# Dump the invalid rows 
dump <- which(is.na(RepairedData$pair) == TRUE)
RepairedData <- RepairedData[-dump, ]
# 1230 rows 177 columns are valid

# variables 64, 65, 67, 68, 70, 72, 71, 78, 77
# check values
table(RepairedData$PICCnew_this_admission, useNA = "ifany")
table(RepairedData$PICC_removed, useNA = "ifany")
table(RepairedData$tracheostomyThisadmission, useNA = "ifany")
table(RepairedData$foley_drain_placedthisadmission, useNA = "ifany")
table(RepairedData$Gtube_placedThisadmission, useNA = "ifany")
table(RepairedData$VP_shunt_newthisadmission, useNA = "ifany")
which(is.na(RepairedData$PICCnew_this_admission))
# Only 82th patient has NA in PICCnew. No NA in other varaibles
#############################
# intialize new variable
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

## Check
Check <- RepairedData[,c(64, 65, 67, 68, 70, 72, 71, 78, 77, 74, 73, 178)]
fix(Check)

#############################
# Create variable AnyTech
# intialize new variable
RepairedData$AnyTech <- rep(NA, 1230)
# check variable types

# variables 62, 65, 66, 68, 71, 77
# check values
table(RepairedData$PICC_cline_port, useNA = "ifany")
table(RepairedData$Tracheostomy, useNA = "ifany")
table(RepairedData$Indwelling_foley_other_drainatd, useNA = "ifany")
table(RepairedData$Gtube_Duodenaltube_JtubeatDisch, useNA = "ifany")
table(RepairedData$VP_shunt_atdischarge, useNA = "ifany")
# No NAs

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
## Check
Check1 <- RepairedData[,c(62, 65, 66, 68, 71, 77, 73, 179)]
fix(Check1)

# output tables
  table(RepairedData$PICCnew_this_admission, RepairedData$PICC_removed, useNA = "ifany")
  table(RepairedData$NewTech, RepairedData$PICCnew_this_admission, useNA = "ifany")
  table(RepairedData$NewTech, RepairedData$PICC_removed, useNA = "ifany")
  table(RepairedData$NewTech, RepairedData$tracheostomyThisadmission, useNA = "ifany")
  table(RepairedData$NewTech, RepairedData$foley_drain_placedthisadmission, useNA = "ifany")
  table(RepairedData$NewTech, RepairedData$Gtube_placedThisadmission, useNA = "ifany")
  table(RepairedData$NewTech, RepairedData$VP_shunt_newthisadmission, useNA = "ifany")
  
  table(RepairedData$AnyTech, RepairedData$PICC_cline_port, useNA = "ifany")
  table(RepairedData$AnyTech, RepairedData$PICC_removed, useNA = "ifany")
  table(RepairedData$AnyTech, RepairedData$Tracheostomy, useNA = "ifany")
  table(RepairedData$AnyTech, RepairedData$Indwelling_foley_other_drainatd, useNA = "ifany")
  table(RepairedData$AnyTech, RepairedData$Gtube_Duodenaltube_JtubeatDisch, useNA = "ifany")
  table(RepairedData$AnyTech, RepairedData$VP_shunt_atdischarge, useNA = "ifany")
  
  table(RepairedData$NewTech, RepairedData$AnyTech, useNA = "ifany")
  

######### Odds Ratio #############
colnames(RepairedData)[3] <- "case"
library(survival)
yf6 <- clogit(case ~ NewTech + strata(pair), data = RepairedData)
yf7 <- clogit(case ~ AnyTech + strata(pair), data = RepairedData)
yf8 <- clogit(case ~ Will_TPN_PPN_continue + strata(pair), data = RepairedData)
summary(yf6)
summary(yf7)
summary(yf8)

################## END ######################
  # Multi-Way Frequency Table
  load("C:\\Users\\zhago7\\Documents\\PatientDataCollections.RData")
  temp <- table(metadata1$New_technology, 
                metadata1$PICCnew_this_admission, 
                metadata1$PICC_removed, 
                metadata1$tracheostomy.This.admission., 
                metadata1$foley_drain_placed.this.admission, 
                metadata1$Gtube_placed.This.admission, 
                metadata1$VP_shunt_new.this.admission, 
                useNA = "ifany", 
                dnn = c("New_technology", "PICCnew", "PICC_removed", 
                        "tracheostomy", "foley", "Gtube", "VP_shunt")
  )
  ftable(temp)
  temp1 <- table(metadata1$Any_technology, 
                 metadata1$PICC_c.line_port, 
                 metadata1$PICC_removed, 
                 metadata1$Tracheostomy,
                 metadata1$Indwelling_foley_other_drain.at.discharge,
                 metadata1$Gtube_Duodenaltube_Jtube.at.Discharge,
                 metadata1$VP_shunt_at.discharge,
                 useNA = "ifany",
                 dnn = c("AnyTech", "PICCport", "PICCrmv", 
                         "trach", "IndwellingDischarge", 
                         "GtubeDischarge", "VPshuntDischarge")
  )
  ftable(temp1)
  
