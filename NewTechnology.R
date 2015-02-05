# Making new variable called new_technology

load("C:\\Users\\zhago7\\Documents\\PatientDataCollections.RData")
metadata1 <- metadata
# intialize new variable
metadata1$New_technology <- rep(NA, 1250)
# check variable types
str(metadata1$PICCnew_this_admission)
# convert factor to logical values.
metadata1$PICCnew_this_admission <- as.logical(metadata1$PICCnew_this_admission)
metadata1$PICC_removed <- as.logical(metadata1$PICC_removed)
metadata1$tracheostomy.This.admission.<- as.logical(metadata1$tracheostomy.This.admission.)
metadata1$foley_drain_placed.this.admission <- as.logical(metadata1$foley_drain_placed.this.admission)
metadata1$Gtube_placed.This.admission <- as.logical(metadata1$Gtube_placed.This.admission)
metadata1$VP_shunt_new.this.admission <- as.logical(metadata1$VP_shunt_new.this.admission)
# Check the variable type
str(metadata1$PICCnew_this_admission)
str(metadata1$PICC_removed)
str(metadata1$tracheostomy.This.admission.)
str(metadata1$foley_drain_placed.this.admission)
str(metadata1$Gtube_placed.This.admission)
str(metadata1$VP_shunt_new.this.admission)
#############################
for(i in 1:1250){
  if(!is.na(metadata1$PICCnew_this_admission[i])
     && metadata1$PICCnew_this_admission[i] == TRUE
     && !is.na(metadata1$PICC_removed[i])
     && metadata1$PICC_removed[i] == FALSE){
    metadata1$New_technology[i] <- 1
  }
  else if(!is.na(metadata1$tracheostomy.This.admission.[i])
          && metadata1$tracheostomy.This.admission.[i] == TRUE){
    metadata1$New_technology[i] <- 1
  } 
  else if(!is.na(metadata1$foley_drain_placed.this.admission[i])
          && metadata1$foley_drain_placed.this.admission[i] == TRUE){
    metadata1$New_technology[i] <- 1
  }
  else if(!is.na(metadata1$Gtube_placed.This.admission[i])
          && metadata1$Gtube_placed.This.admission[i] == TRUE){
    metadata1$New_technology[i] <- 1
  }
  else if(!is.na(metadata1$VP_shunt_new.this.admission[i])
          && metadata1$VP_shunt_new.this.admission[i] == TRUE){
    metadata1$New_technology[i] <- 1
  }
  else metadata1$New_technology[i] <- 0
}

#############################
# Create variable Any_technology
# intialize new variable
metadata1$Any_technology <- rep(NA, 1250)
# check variable types
str(metadata1$PICC_c.line_port)
# convert factor to logical values.
metadata1$PICC_c.line_port <- as.logical(metadata1$PICC_c.line_port)
metadata1$Tracheostomy <- as.logical(metadata1$Tracheostomy)
metadata1$Indwelling_foley_other_drain.at.discharge <- as.logical(metadata1$Indwelling_foley_other_drain.at.discharge)
metadata1$Gtube_Duodenaltube_Jtube.at.Discharge <- as.logical(metadata1$Gtube_Duodenaltube_Jtube.at.Discharge)
metadata1$VP_shunt_at.discharge <- as.logical(metadata1$VP_shunt_at.discharge)
# Check the variable type
str(metadata1$PICC_c.line_port)
str(metadata1$PICC_removed)
str(metadata1$Tracheostomy)
str(metadata1$Indwelling_foley_other_drain.at.discharge)
str(metadata1$Gtube_Duodenaltube_Jtube.at.Discharge)
str(metadata1$VP_shunt_at.discharge)


for(i in 1:1250){
  if(!is.na(metadata1$PICC_c.line_port[i])
     && metadata1$PICC_c.line_port[i] == TRUE
     && !is.na(metadata1$PICC_removed[i])
     && metadata1$PICC_removed[i] == FALSE){
    metadata1$Any_technology[i] <- 1
  } 
  else if(!is.na(metadata1$Tracheostomy[i])
          && metadata1$Tracheostomy[i] == TRUE){
    metadata1$Any_technology[i] <- 1
  }
  else if(!is.na(metadata1$Indwelling_foley_other_drain.at.discharge[i])
          && metadata1$Indwelling_foley_other_drain.at.discharge[i] == TRUE){
    metadata1$Any_technology[i] <- 1
  }
  else if(!is.na(metadata1$Gtube_Duodenaltube_Jtube.at.Discharge[i])
          && metadata1$Gtube_Duodenaltube_Jtube.at.Discharge[i] == TRUE){
    metadata1$Any_technology[i] <- 1
  }
  else if(!is.na(metadata1$VP_shunt_at.discharge[i])
          && metadata1$VP_shunt_at.discharge[i] == TRUE){
    metadata1$Any_technology[i] <- 1
  }
  else metadata1$Any_technology[i] <- 0
}

save(
# output tables
table(metadata1$PICCnew_this_admission, metadata1$PICC_removed)

 
table(metadata1$New_technology, metadata1$PICCnew_this_admission, useNA = "ifany")
table(metadata1$New_technology, metadata1$PICC_removed, useNA = "ifany")
table(metadata1$New_technology, metadata1$tracheostomy.This.admission., useNA = "ifany")
table(metadata1$New_technology, metadata1$foley_drain_placed.this.admission, useNA = "ifany")
table(metadata1$New_technology, metadata1$Gtube_placed.This.admission, useNA = "ifany")
table(metadata1$New_technology, metadata1$VP_shunt_new.this.admission, useNA = "ifany")

table(metadata1$Any_technology, metadata1$PICC_c.line_port, useNA = "ifany")
table(metadata1$Any_technology, metadata1$PICC_removed, useNA = "ifany")
table(metadata1$Any_technology, metadata1$Tracheostomy, useNA = "ifany")
table(metadata1$Any_technology, metadata1$Indwelling_foley_other_drain.at.discharge, useNA = "ifany")
table(metadata1$Any_technology, metadata1$Gtube_Duodenaltube_Jtube.at.Discharge, useNA = "ifany")
table(metadata1$Any_technology, metadata1$VP_shunt_at.discharge, useNA = "ifany")

table(metadata1$New_technology, metadata1$Any_technology, useNA = "ifany")

##################################
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
