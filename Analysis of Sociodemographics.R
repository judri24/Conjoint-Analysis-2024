#Analysis of sociodemographic questions

library(readxl)
x<- read_excel("/Users/judithrichter/Desktop/Bachelorarbeit/Daten/Hochzuladende Dateien/Sociodemographics.xlsx", sheet = "Sociodemogr_edited")
#print(Sociodemographics)
str(x)

#Analysis of Age

mean(x$Age, na.rm = TRUE)
table(x$`Age categories`)
prop.table(table(x$`Age categories`))

#Analysis of Gender

table(x$Gender)
prop.table(table(x$Gender))

#Analysis of Occupation

table(x$Occupation)

prop.table(table(x$Occupation))

#Analysis of travel companion

table(x$Travel_companion)

prop.table(table(x$Travel_companion))
