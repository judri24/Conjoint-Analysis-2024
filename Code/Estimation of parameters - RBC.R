#Estimation of parameters - Ranking-based Conjoint Analyse

#Creation of fractional factorial design

library(conjoint)
experiment<-expand.grid(
  Destination=c("Coastal region", "City", "Mountains"),
  Transportation=c("Plane", "Car", "Train/Bus"),
  Accommodation=c("Hotel", "Holiday home", "Camping"),
  Activities=c("Sightseeing", "Relaxation", "Sport activities")
)

#Creation of the orthogonal fractional factorial design for the ranking-based conjoint analysis

design_RBC=caFactorialDesign(data=experiment,type="orthogonal")
design_RBC

#Coding the orthogonal fractional factorial design

code=caEncodedDesign(design_RBC)
code

#Estimation of parameters

#Assignment of level names
levna <- c("Coastal Region", "City", "Mountains","Plane", "Car", "Train/Bus","Hotel", "Holiday home", "Camping", "Sightseeing", "Relaxation", "Sport activities")

library(readxl)
preferences <- read_excel("../data/Data_RBC.xlsx", sheet = "RBC_ready for analysis", range = "I1:Q199")
preferences

# Conjoint(data on preference, coded research design, the names of variables and levels, the type of data preferences)

library(conjoint)
model <- Conjoint(preferences, code, levna, y.type = "rank")

#Determining the missing part-worth utilties
#As effect-coding is used, the coefficient of the missing level can be calculated by the difference of the other two levels

# Regressionanalysis for extracting the coefficients

library(readxl)
x<- read_excel("../data/Data_RBC.xlsx", sheet = "RBC_Regression analysis")
str(x)

x$DestinationCoastalRegion <- (-1) *x$DestinationCoastalRegion
x$DestinationCity <- (-1) * x$DestinationCity
x$TransportationPlane <- (-1) * x$TransportationPlane
x$TransportationCar <- (-1) * x$TransportationCar
x$AccommodationHotel <- (-1) * x$AccommodationHotel
x$AccommodationHolidayhome <- (-1) * x$AccommodationHolidayhome
x$ActivitiesSightseeing <- (-1) * x$ActivitiesSightseeing
x$ActivitiesRelaxation <- (-1) * x$ActivitiesRelaxation

model <- lm(evaluation ~ DestinationCoastalRegion + DestinationCity + TransportationPlane + TransportationCar + AccommodationHotel + AccommodationHolidayhome + ActivitiesSightseeing + ActivitiesRelaxation, data = x)
summary(model)

# DestinationMountains = -0.1262626
DestinationMountains <- as.numeric((-1) * (coef(model)["DestinationCoastalRegion"] + coef(model)["DestinationCity"]))
DestinationMountains

# TransportationTrainBus = -0.1868687
TransportationTrainBus <- as.numeric((-1) * (coef(model)["TransportationPlane"] + coef(model)["TransportationCar"]))
TransportationTrainBus

# AccommodationCamping = -1.127946
AccommodationCamping <-as.numeric((-1) * (coef(model)["AccommodationHotel"] + coef(model)["AccommodationHolidayhome"]))
AccommodationCamping
# ActivitiesSportActivities = -0.2777778
ActivitiesSportActivities <-as.numeric((-1) * (coef(model)["ActivitiesSightseeing"] + coef(model)["ActivitiesRelaxation"]))
ActivitiesSportActivities

#Determining the standard errors

covMatrix <- vcov(model)
sqrt(diag(covMatrix))

#SE of DestinationMountains = 0.07565693
SE_DestinationMountains <- sqrt(sum(covMatrix[2:3, 2:3]))
SE_DestinationMountains

#SE of TransportationTrainBus = 0.07565693
SE_TransportationTrainBus <- sqrt(sum(covMatrix[4:5, 4:5]))
SE_TransportationTrainBus

#SE of AccommodationCamping = 0.07565693
SE_AccommodationCamping <- sqrt(sum(covMatrix[6:7, 6:7]))
SE_AccommodationCamping

#SE of ActivitiesSportActivities = 0.07565693
SE_ActivitiesSportActivities <- sqrt(sum(covMatrix[8:9, 8:9]))
SE_ActivitiesSportActivities

#Determining the t-values 

#t-value of Destination Mountains = -1.668884
t_DestinationMountains <- DestinationMountains/SE_DestinationMountains
t_DestinationMountains

#t-value of TransportationTrainBus = -2.469948
t_TransportationTrainBus <- TransportationTrainBus/SE_TransportationTrainBus
t_TransportationTrainBus

#t-value of AccommodationCamping =  -14.90869
t_AccommodationCamping <- AccommodationCamping/SE_AccommodationCamping
t_AccommodationCamping

#t-value of ActivitiesSportActivities = -3.671544
t_ActivitiesSportActivities <- ActivitiesSportActivities/SE_ActivitiesSportActivities
t_ActivitiesSportActivities

#Determining p-values

#p-value of DestinationMountains = 0.09531697
p_DestinationMountains <- 2*pt(abs(t_DestinationMountains), 1773, lower.tail = FALSE)
p_DestinationMountains

#p-value of TransportationTrainBus = 0.01360681
p_TransportationTrainBus <- 2*pt(abs(t_TransportationTrainBus), 1773, lower.tail = FALSE)
p_TransportationTrainBus

#p-value of AccommodationCamping = 1.910402e-47
p_AccommodationCamping <- 2*pt(abs(t_AccommodationCamping), 1773, lower.tail = FALSE)
p_AccommodationCamping

#p-value of ActivitiesSportActivities =  0.0002482314
p_ActivitiesSportActivities  <- 2*pt(abs(t_ActivitiesSportActivities), 1773, lower.tail = FALSE)
p_ActivitiesSportActivities 

#Table of missing levels

missing_levels<- data.frame(
  Levels = c("DestinationMountains", "TransportationTrainBus", "AccommodationCamping", "ActivitiesSportActivities"),
  Estimate = c(DestinationMountains, TransportationTrainBus, AccommodationCamping, ActivitiesSportActivities),
  "Standard Error" = c(SE_DestinationMountains, SE_TransportationTrainBus, SE_AccommodationCamping, SE_ActivitiesSportActivities),
  t_value = c(t_DestinationMountains, t_TransportationTrainBus, t_AccommodationCamping, t_ActivitiesSportActivities),
  p_value = c(p_DestinationMountains,p_TransportationTrainBus, p_AccommodationCamping, p_ActivitiesSportActivities)
)

print(missing_levels)

#Relative Importance

#Sum of all ranges

Range_Destination <- (max(coef(model)["DestinationCoastalRegion"], coef(model)["DestinationCity"],DestinationMountains) - min(coef(model)["DestinationCoastalRegion"], coef(model)["DestinationCity"],DestinationMountains))
Range_Transportation <-(max(coef(model)["TransportationPlane"], coef(model)["TransportationCar"], TransportationTrainBus)- min(coef(model)["TransportationPlane"], coef(model)["TransportationCar"], TransportationTrainBus))
Range_Accommodation <- (max(coef(model)["AccommodationHotel"],coef(model)["AccommodationHolidayhome"],AccommodationCamping) - min(coef(model)["AccommodationHotel"],coef(model)["AccommodationHolidayhome"],AccommodationCamping))
Range_Activities <- (max(coef(model)["ActivitiesSightseeing"], coef(model)["ActivitiesRelaxation"], ActivitiesSportActivities) - min(coef(model)["ActivitiesSightseeing"], coef(model)["ActivitiesRelaxation"], ActivitiesSportActivities))
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
  scale_y_continuous(limits = c(0, 40), labels = scales::percent_format(scale = 1)) +  
  labs(x = "Attribute",
       y = "Relative Importance")


#Testing hypothesis 2

pt(4.228, 1773, lower.tail = FALSE)

#Testing hypothesis 3

t_value <- ((1.03704 - (-0.91077))/ sqrt(0.07566^2 + 0.07566^2))
t_value

pt(t_value, 1773, lower.tail = FALSE)


