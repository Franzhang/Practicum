# 2X2 tables for medication 1 to 30
load("C:\\Users\\zhago7\\Documents\\Patient Chart Data_R")
# Data Object "YF" created

sink("Medication tables.txt")

for(i in 1:30){
timesdaily <- paste("Medication", i, "timesdaily", sep = "")
prn <- paste("Medication", i, "prn", sep = "")
freq <- paste("Medication", i, "Freq", sep = "")

YF[[timesdaily]] <- rep(NA,1250)
YF[[prn]] <- rep(NA,1250)

YF[[freq]] <- as.character(YF[[freq]])

YF[[timesdaily]][YF[[freq]] == "< once a day"] <- 0
YF[[timesdaily]][YF[[freq]] == "BID/Q12H"] <- 2
YF[[timesdaily]][YF[[freq]] == "BID/Q12H+prn"] <- 2
YF[[timesdaily]][YF[[freq]] == "Q2H"] <- 12
YF[[timesdaily]][YF[[freq]] == "Q2H+prn"] <- 12
YF[[timesdaily]][YF[[freq]] == "Q4H"] <- 6
YF[[timesdaily]][YF[[freq]] == "Q4H+prn"] <- 6
YF[[timesdaily]][YF[[freq]] == "QAC"] <- 3
YF[[timesdaily]][YF[[freq]] == "Qd/Qday/Q24HR/QAM/QHS"] <- 1
YF[[timesdaily]][YF[[freq]] == "Qd/Qday/Q24HR/QAM/QHS+prn"] <- 1
YF[[timesdaily]][YF[[freq]] == "QID/6H"] <- 4
YF[[timesdaily]][YF[[freq]] == "QID/6H+prn"] <- 4
YF[[timesdaily]][YF[[freq]] == "TID/Q8"] <- 3
YF[[timesdaily]][YF[[freq]] == "TID/Q8+prn"] <- 3


YF[[prn]][YF[[freq]] == "< once a day"] <- 1
YF[[prn]][YF[[freq]] == "BID/Q12H"] <- 0
YF[[prn]][YF[[freq]] == "BID/Q12H+prn"] <- 1
YF[[prn]][YF[[freq]] == "Q2H"] <- 0
YF[[prn]][YF[[freq]] == "Q2H+prn"] <- 1
YF[[prn]][YF[[freq]] == "Q4H"] <- 0
YF[[prn]][YF[[freq]] == "Q4H+prn"] <- 1
YF[[prn]][YF[[freq]] == "QAC"] <- 0
YF[[prn]][YF[[freq]] == "Qd/Qday/Q24HR/QAM/QHS"] <- 0
YF[[prn]][YF[[freq]] == "Qd/Qday/Q24HR/QAM/QHS+prn"] <- 1
YF[[prn]][YF[[freq]] == "QID/6H"] <- 0
YF[[prn]][YF[[freq]] == "QID/6H+prn"] <- 1
YF[[prn]][YF[[freq]] == "TID/Q8"] <- 0
YF[[prn]][YF[[freq]] == "TID/Q8+prn"] <- 1

YF[[timesdaily]][YF[[freq]] == ""] <- NA
YF[[prn]][YF[[freq]] == ""] <- NA

YF[[prn]] <- as.factor(YF[[prn]])
YF[[freq]] <- as.factor(YF[[freq]])
YF[[timesdaily]] <- as.factor(YF[[timesdaily]])

# Print out a 2 x2 table of Medication1Freq and Medication1timesdaily
print(paste("2 X 2 table of", freq, "and", timesdaily, sep = " "))
print(table(YF[[freq]], YF[[timesdaily]]))

# Print out a 2 x2 table of Medication1Freq and Medication1prn
print(paste("2 X 2 table of", freq, "and", prn, sep = " "))
print(table(YF[[freq]], YF[[prn]]))
}

sink()
