#Estimation of parameters - Choice-based Conjoint Analysis

library(readxl)
preference <- read_excel("../data/Data_CBC.xlsx", sheet = "CBC_ready for analysis_Effect")
#View(preference)
str(preference)

#install.packages("logitr")
library(logitr)

mnl_pref <- logitr(data = preference, outcome = "Choice", obsID = "ObsID", pars = c("DestinationCoastalRegion", "DestinationCity", "TransportationPlane", "TransportationCar", "AccommodationHotel", "AccommodationHolidayHome" , "ActivitiesSightseeing", "ActivitiesRelaxation" ))
summary(mnl_pref)

#Determining the missing part-worth utilties
#As effect-coding is used, the coefficient of the missing level can be calculated by the difference of the other two levels

coef <- mnl_pref$coefficients

# DestinationMountains = -0.01159854
DestinationMountains <- as.numeric((-1) * (coef(mnl_pref)["DestinationCoastalRegion"] + coef(mnl_pref)["DestinationCity"]))
DestinationMountains

# TransportationTrainBus = -0.07264221
TransportationTrainBus <- as.numeric((-1) * (coef(mnl_pref)["TransportationPlane"] + coef(mnl_pref)["TransportationCar"]))
TransportationTrainBus

# AccommodationCamping = -0.6686883
AccommodationCamping <- as.numeric((-1) * (coef(mnl_pref)["AccommodationHotel"] + coef(mnl_pref)["AccommodationHolidayHome"]))
AccommodationCamping
# ActivitiesSportActivities = -0.08264138
ActivitiesSportActivities <- as.numeric((-1) * (coef(mnl_pref)["ActivitiesSightseeing"] + coef(mnl_pref)["ActivitiesRelaxation"]))
ActivitiesSportActivities

#Determining the standard errors

covMatrix <- vcov(mnl_pref)
sqrt(diag(covMatrix))

#SE of DestinationMountains = 0.05645778
SE_DestinationMountains <- sqrt(sum(covMatrix[1:2, 1:2]))
SE_DestinationMountains

#SE of TransportationTrainBus = 5.289389e+03
SE_TransportationTrainBus <- sqrt(sum(covMatrix[3:4, 3:4]))
SE_TransportationTrainBus

#SE of AccommodationCamping = 0.05347711
SE_AccommodationCamping <- sqrt(sum(covMatrix[5:6, 5:6]))
SE_AccommodationCamping

#SE of ActivitiesSportActivities = 5.289389e+03
SE_ActivitiesSportActivities <- sqrt(sum(covMatrix[7:8, 7:8]))
SE_ActivitiesSportActivities

#Determining the z-values 

#z-value of Destination Mountains = -0.2054374
z_DestinationMountains <- DestinationMountains/SE_DestinationMountains
z_DestinationMountains

#z-value of TransportationTrainBus = -1.373357e-05
z_TransportationTrainBus <- TransportationTrainBus/SE_TransportationTrainBus
z_TransportationTrainBus

#z-value of AccommodationCamping = -12.5042
z_AccommodationCamping <- AccommodationCamping/SE_AccommodationCamping
z_AccommodationCamping

#z-value of ActivitiesSportActivities = -1.562399e-05
z_ActivitiesSportActivities <- ActivitiesSportActivities/SE_ActivitiesSportActivities
z_ActivitiesSportActivities

#Determining p-values

#p-value of DestinationMountains = 0.8372304
p_DestinationMountains <- 2*pnorm(abs(z_DestinationMountains), lower.tail = FALSE)
p_DestinationMountains

#p-value of TransportationTrainBus = 0.999989
p_TransportationTrainBus <- 2*pnorm(abs(z_TransportationTrainBus), lower.tail = FALSE)
p_TransportationTrainBus

#p-value of AccommodationCamping = 7.081107e-36
p_AccommodationCamping <- 2*pnorm(abs(z_AccommodationCamping), lower.tail = FALSE)
p_AccommodationCamping

#p-value of ActivitiesSportActivities =  0.9999875
p_ActivitiesSportActivities  <- 2*pnorm(abs(z_ActivitiesSportActivities), lower.tail = FALSE)
p_ActivitiesSportActivities 

#Table of missing levels

missing_levels<- data.frame(
  Levels = c("DestinationMountains", "TransportationTrainBus", "AccommodationCamping", "ActivitiesSportActivities"),
  Estimate = c(DestinationMountains, TransportationTrainBus, AccommodationCamping, ActivitiesSportActivities),
  "Standard Error" = c(SE_DestinationMountains, SE_TransportationTrainBus, SE_AccommodationCamping, SE_ActivitiesSportActivities),
  z_value = c(z_DestinationMountains, z_TransportationTrainBus, z_AccommodationCamping, z_ActivitiesSportActivities),
  p_value = c(p_DestinationMountains,p_TransportationTrainBus, p_AccommodationCamping, p_ActivitiesSportActivities)
  )

print(missing_levels)

#Relative Importance

#Sum of all ranges

Range_Destination <- (max(coef(mnl_pref)["DestinationCoastalRegion"], coef(mnl_pref)["DestinationCity"], DestinationMountains) - min(coef(mnl_pref)["DestinationCoastalRegion"], coef(mnl_pref)["DestinationCity"], DestinationMountains))
Range_Transportation <-(max(coef(mnl_pref)["TransportationPlane"], coef(mnl_pref)["TransportationCar"], TransportationTrainBus) - min(coef(mnl_pref)["TransportationPlane"], coef(mnl_pref)["TransportationCar"], TransportationTrainBus))
Range_Accommodation <- (max(coef(mnl_pref)["AccommodationHotel"],coef(mnl_pref)["AccommodationHolidayHome"], AccommodationCamping) - min(coef(mnl_pref)["AccommodationHotel"],coef(mnl_pref)["AccommodationHolidayHome"], AccommodationCamping))
Range_Activities <- (max(coef(mnl_pref)["ActivitiesSightseeing"], coef(mnl_pref)["ActivitiesRelaxation"], ActivitiesSportActivities) - min(coef(mnl_pref)["ActivitiesSightseeing"], coef(mnl_pref)["ActivitiesRelaxation"], ActivitiesSportActivities))
sum <- (Range_Destination + Range_Transportation + Range_Accommodation + Range_Activities)

Imp_Destination <- Range_Destination/sum
Imp_Destination
Imp_Transportation <- Range_Transportation/sum
Imp_Transportation
Imp_Accommodation <- Range_Accommodation/sum
Imp_Accommodation
Imp_Activities <- Range_Activities/sum
Imp_Activities

importance_table <- data.frame(
  Attribute = c("Destination", "Transportation", "Accommodation", "Activities"),
  Importance = c(round((Imp_Destination),3), round((Imp_Transportation),3), round((Imp_Accommodation),3), round((Imp_Activities),3))
)
print(importance_table)
importance_table$Attribute <- factor(importance_table$Attribute, levels = importance_table$Attribute)


#Graphic Relative Importance
library(ggplot2)

ggplot(importance_table, aes(x = Attribute, y = Importance * 100)) +  
  geom_bar(stat = "identity", fill = "grey") +
  geom_text(aes(label = paste0(round(Importance * 100, 1), "%")), vjust = -0.5, family = "Arial") +  
  theme_minimal(base_family = "Arial", base_size = 13) +
  scale_y_continuous(limits = c(0, 45), labels = scales::percent_format(scale = 1)) +  
  labs(x = "Attribute",
       y = "Relative Importance")

#2 log-likelihood test

library("lmtest")

logLik_mnl <- mnl_pref$logLik
logLik_null <- mnl_pref$nullLogLik
LLR <- -2 * (logLik_null - logLik_mnl)
LLR

p_value <- pchisq(LLR, 8, lower.tail = FALSE)
p_value

#Hypothesis 2

p_value <- pnorm(2.9353, lower.tail = FALSE)
p_value

#Hypothesis 3

z_value <- ((0.464000 - (-0.452402))/ sqrt(0.065760^2 + 0.065930^2))
z_value

p_value <- pnorm(z_value, lower.tail = FALSE)
p_value




