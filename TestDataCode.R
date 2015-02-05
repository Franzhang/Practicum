load("C:/Users/zhago7/Documents/TestDataForYifan.RData") # load test data
which(colnames(testdata)== "If.yes..is.there.a.primary.care.follow.up..E")
colnames(testdata)[171] <- "primary.care" 
colnames(testdata)[3] <- "case"
yf1 <- clogit(case ~ primary.care + strata(pair), data = testdata)
colnames(testdata)[172] <- "subspeciality.appointment"
colnames(testdata)[173] <- "suggested.primary.care" 
colnames(testdata)[174] <- "suggested.subspeciality" 
yf2 <- clogit(case ~ subspeciality.appointment + strata(pair), data = testdata)
yf3 <- clogit(case ~ suggested.primary.care + strata(pair), data = testdata)
yf4 <- clogit(case ~ suggested.subspeciality + strata(pair), data = testdata)
load("C:/Users/zhago7/Documents/PatientDataCollections1.RData")
testdata$totalscheduleddoses <- metadata1$totalscheduleddoses[1:129]
yf5 <- clogit(case ~ totalscheduleddoses + strata(pair), data = testdata)
