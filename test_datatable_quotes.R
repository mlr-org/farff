library(data.table)

s = "my name is \"joe\""

d1 = fread("test_datatable_quotes.csv", data.table = FALSE, header = FALSE)
print(d1[2,2])
print(d1[2,2] == s)

d2 = read.table("test_datatable_quotes.csv", sep = ",", header = FALSE,
  colClasses = c("numeric", "character"), quote="\\")
print(d2[2,2])
print(d2[2,2] == s)



