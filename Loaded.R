load("C:\\Users\\zhago7\\Documents\\Patient Chart Data_R")
# Data Object "YF" created
# look at the structure of data "YF"
str(YF, list.len = 180)
# Generated 5 to 6 values from certain variable
sample(table(YF[-89, ]$Medication24Name), 6)
table(YF[-89, ]$Medication30Name)
sample(table(YF[-89, ]$Medication19Route), 6)
table(YF[-89, ]$Medication30Route)
sample(table(YF[-89, ]$Medication19Freq), 6)
table(YF[-89, ]$Medication30Freq)
sample(table(YF$Notes.Elyse), 6)
sample(table(YF$Notes.Kevin), 6)
sample(table(YF$Notes.Kate), 6)

# Show list of conditions for neurologic1 to other1
table(YF$ChronicResp1)
table(YF$Neurologic1)
summary(YF$Neurologic1)
table(YF$ChronicResp1)

#### NOT Efficient
##temp <- read.delim("clipboard", header = FALSE)
##summary(temp)
##str(temp)
##names <- temp[seq(from = 1, to = nrow(temp), by = 2), 1]
##freq <- temp[seq(from = 2, to = nrow(temp), by = 2), 1]
##temp1 <- data.frame(names, freq)
##temp1$names
##temp1$freq


# table(YF[,6])
table(YF[,7])
table(YF[,8])
table(YF[,9])
table(YF[,10])
table(YF[,11])
table(YF[,12])
table(YF[,13])
table(YF[,14])
table(YF[,15])
table(YF[,16])
table(YF[,17])
# table(YF$Other1)


# Variable Medication1Freq  make 2 new variables Medication1timesdaily Medication1prn 
# Medication1timesdaily for each value in columnA make equal to value in column C
# Medication1prn for each value in columnA make it equal to value in column 

YF$Medication1timesdaily <- rep(0,1250)
YF$Medication1prn <- rep(0,1250)
YF$Medication1Freq <- as.character(YF$Medication1Freq)

YF$Medication1timesdaily[YF$Medication1Freq == "< once a day"] <- 0
YF$Medication1timesdaily[YF$Medication1Freq == "BID/Q12H"] <- 2
YF$Medication1timesdaily[YF$Medication1Freq == "BID/Q12H+prn"] <- 2
YF$Medication1timesdaily[YF$Medication1Freq == "Q2H"] <- 12
YF$Medication1timesdaily[YF$Medication1Freq == "Q2H+prn"] <- 12
YF$Medication1timesdaily[YF$Medication1Freq == "Q4H"] <- 6
YF$Medication1timesdaily[YF$Medication1Freq == "Q4H+prn"] <- 6
YF$Medication1timesdaily[YF$Medication1Freq == "QAC"] <- 3
YF$Medication1timesdaily[YF$Medication1Freq == "Qd/Qday/Q24HR/QAM/QHS"] <- 1
YF$Medication1timesdaily[YF$Medication1Freq == "Qd/Qday/Q24HR/QAM/QHS+prn"] <- 1
YF$Medication1timesdaily[YF$Medication1Freq == "QID/6H"] <- 4
YF$Medication1timesdaily[YF$Medication1Freq == "QID/6H+prn"] <- 4
YF$Medication1timesdaily[YF$Medication1Freq == "TID/Q8"] <- 3
YF$Medication1timesdaily[YF$Medication1Freq == "TID/Q8+prn"] <- 3



YF$Medication1prn[YF$Medication1Freq == "< once a day"] <- 1
YF$Medication1prn[YF$Medication1Freq == "BID/Q12H"] <- 0
YF$Medication1prn[YF$Medication1Freq == "BID/Q12H+prn"] <- 1
YF$Medication1prn[YF$Medication1Freq == "Q2H"] <- 0
YF$Medication1prn[YF$Medication1Freq == "Q2H+prn"] <- 1
YF$Medication1prn[YF$Medication1Freq == "Q4H"] <- 0
YF$Medication1prn[YF$Medication1Freq == "Q4H+prn"] <- 1
YF$Medication1prn[YF$Medication1Freq == "QAC"] <- 0
YF$Medication1prn[YF$Medication1Freq == "Qd/Qday/Q24HR/QAM/QHS"] <- 0
YF$Medication1prn[YF$Medication1Freq == "Qd/Qday/Q24HR/QAM/QHS+prn"] <- 1
YF$Medication1prn[YF$Medication1Freq == "QID/6H"] <- 0
YF$Medication1prn[YF$Medication1Freq == "QID/6H+prn"] <- 1
YF$Medication1prn[YF$Medication1Freq == "TID/Q8"] <- 0
YF$Medication1prn[YF$Medication1Freq == "TID/Q8+prn"] <- 1

YF$Medication1timesdaily[YF$Medication1Freq == ""] <- NA
YF$Medication1prn[YF$Medication1Freq == ""] <- NA

YF$Medication1prn <- as.factor(YF$Medication1prn)
YF$Medication1Freq <- as.factor(YF$Medication1Freq)
YF$Medication1timesdaily <- as.factor(YF$Medication1timesdaily)


# Make a 2 x2 table of Medication1Freq and Medication1timesdaily
# Make a 2 x2 table of Medication1Freq and Medication1prn

table(YF$Medication1Freq, YF$Medication1timesdaily)
table(YF$Medication1Freq, YF$Medication1prn)
