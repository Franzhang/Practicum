# Code for descriptive statistics
sink("DescriptiveStats.txt")
col <- c(18:21, 23:76, 167:171, 265)
for( i in col){
print(names(metadata)[i])
print(table(metadata[,i]))
}
sink()


hist(as.numeric(metadata$TotalICUDays), main = "Histogram of 
Total ICU Days", xlab = "Total ICU Days", col = "skyblue")
legend("topright", legend = c("Min. = 1.00", "1st Qu. = 14.00",
"Median = 25", "Mean = 22.84", "3rd Qu. = 31.00", "Max. = 48",
"NA's = 950"), cex = 0.8, pch = 16) 
metadata$TotalICUDays <- as.numeric(metadata$TotalICUDays)
summary(metadata$TotalICUDays)

hist(metadata$totalscheduleddoses, main = "Histogram of 
Total Scheduled Doses", xlab = "Total Scheduled Doses", col = "skyblue")
summary(metadata$totalscheduleddoses)
legend("topright", legend = c("Min. = 0.000", "1st Qu. = 2.000",
"Median = 6.000", "Mean = 9.258", "3rd Qu. = 13.000", "Max. = 56"),
cex = 1, pch = 16) 
