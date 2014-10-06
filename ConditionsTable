load("C:\\Users\\zhago7\\Documents\\Patient Chart Data_R")
# Show list of conditions for neurologic1 to other1
sink("conditions tables.txt")
for (i in 8:17) {
  print(names(YF)[i])
  print(table(YF[,i]))
}
sink()
