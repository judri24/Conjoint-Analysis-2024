#Analysis of Questions about the two conjoint methods

library(readxl)
Question_about_CA <- read_excel("/Users/judithrichter/Desktop/Bachelorarbeit/Daten/Hochzuladende Dateien/Question about CA.xlsx", sheet = "Q about CA_edited")
print(Question_about_CA)

table(Question_about_CA$`G07Q01 & G07Q02`)
prop.table(table(Question_about_CA$`G07Q01 & G07Q02`))

table(Question_about_CA$`G07Q01 & G07Q02`, Question_about_CA$Equation)
addmargins(table(Question_about_CA$`G07Q01 & G07Q02`, Question_about_CA$Equation))

cor <- cor(Question_about_CA$`G07Q01 & G07Q02_numeric`, Question_about_CA$Equation)
cor

binom.test(158, 198, p=0.5, alternative = c("greater"))

#Open answers about RBC

tab_RBC <- table(Question_about_CA$`G07Q03_Code 1`)
table(Question_about_CA$`G07Q03_Code 2`)

# Adding the second codes to the first ones

tab_RBC["More/ better choice options"] <- 11

tab_RBC

#Percentages

percentages_RBC <- tab_RBC/40
percentages_RBC

#Plot

Answers = c("Better overview", "More/ better choice options", "Other", "Faster")
percentages_RBC <- data.frame(
  Answers = factor(Answers, levels = Answers),
  Percentage = c( 0.425,0.275, 0.225, 0.050)
)

library(ggplot2)
ggplot(percentages_RBC, aes(x = Answers, y = Percentage)) +
  geom_bar(stat = "identity", fill = "grey") +
  geom_text(aes(label = scales::percent(Percentage)), vjust = -0.5, size = 5) +
  labs(,
    x = "Answers",
    y = "Percentage"
  ) +
  theme_minimal(base_size = 20) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


#Open answers about CBC

tab_CBC <- table(Question_about_CA$`G07Q04_Code 1`)
tab_CBC
table(Question_about_CA$`G07Q04_Code 2`)
table(Question_about_CA$`G07Q04_Code 3`)

# Adding the second codes to the first ones

tab_CBC["Better overview"] <- 43
tab_CBC["Direct, easy comparison"] <- 54
tab_CBC["Faster"] <- 13
tab_CBC["Less choice options"] <- 49

tab_CBC

#Percentages

percentages_CBC <- tab_CBC/158
percentages_CBC

#Plot

Answers <- c("Direct, easy comparison","Less choice options", "Better overview", "Other", "Faster")
percentages_CBC <- data.frame(
  Answers = factor(Answers, levels = Answers),
  Percentage = c( 0.34177215, 0.31012658, 0.27215190,0.15189873, 0.08227848)
)

ggplot(percentages_CBC, aes(x = Answers, y = Percentage)) +
  geom_bar(stat = "identity", fill = "grey") +
  geom_text(aes(label = scales::percent(Percentage)), vjust = -0.5, size = 5) +
  labs(,
       x = "Answers",
       y = "Percentage"
  ) +
  theme_minimal(base_size = 20) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


