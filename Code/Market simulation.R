#Market simulation

#Choosing of profiles used for market simulation

min_value <- 1
max_value <- 81

# List of the excluded profiles as they were used in the conjoint analysis
excluded_values <- c(1, 5, 10, 14, 27, 34, 35, 39, 42, 47, 49, 57, 60, 70, 71, 74, 76)

allowed_values <- setdiff(min_value:max_value, excluded_values)

#Number of profiles to be used of simulation
n <- 5

set.seed(42) #For reproduction
simulation <- sample(allowed_values, n, replace = FALSE)

print(simulation)

# Market simulation RBC

library(conjoint)
experiment<-expand.grid(
  Destination=c("Coastal region", "City", "Mountains"),
  Transportation=c("Plane", "Car", "Train/Bus"),
  Accommodation=c("Hotel", "Holiday home", "Camping"),
  Activities=c("Sightseeing", "Relaxation", "Sport activities")
)

#Simulation Matrix

profiles <- experiment
sim_profiles <- profiles[c(2, 13, 30, 46, 62),]
sim_profiles
profiles <- caEncodedDesign(profiles)


#Estimation of the model
design_RBC=caFactorialDesign(data=experiment,type="orthogonal")
design_RBC

code=caEncodedDesign(design_RBC)
code

levna <- c("Coastal Region", "City", "Mountains","Plane", "Car", "Train/Bus","Hotel", "Holiday home", "Camping", "Sightseeing", "Relaxation", "Sport activities")

library(readxl)
preferences <- read_excel("../data/Data_RBC.xlsx", sheet = "RBC_ready for analysis", range = "I1:Q199")
preferences

library(conjoint)
model <- Conjoint(preferences, code, levna, y.type = "rank")

preferences <- caRankToScore(preferences) #The dataset must be transformed to score rating where 1 is the least likely chosen and 9 is the most likely chosen stimulus as otherwise the market share won't be determined correctly, without this function the total utility corresponds to the expected average ranking of the stimuli, but the expected market share is not correct
results_simulation <- ShowAllSimulations(profiles[c(2, 13, 30, 46, 62), ], preferences, code)
results_simulation <- as.data.frame(results_simulation)

#Market simulation CBC

#Choosing of profiles used for market simulation

profiles <- cbcTools::cbc_profiles(
  Destination = c("Coastal region", "City", "Mountains"),
  Transportation = c("Plane", "Car", "Train/Bus"),
  Accommodation = c("Hotel", "Holiday home", "Camping"),
  Activities = c("Sightseeing", "Relaxation", "Sport activities")
)
profiles <- as.data.frame(profiles)

sim_profiles <- profiles[c(2, 13, 30, 46, 62),]
sim_profiles

library(readxl)
sim_profiles <- read_excel("../data/Simulation_CBC.xlsx ", sheet = "Chosen profiles for simulation")
#View(preference)
str(sim_profiles)

library(readxl)
preference <- read_excel("../data/Data_CBC.xlsx", sheet = "CBC_ready for analysis_Effect")
#View(preference)
str(preference)

#install.packages("logitr")
library(logitr)

mnl_pref <- logitr(data = preference, outcome = "Choice", obsID = "ObsID", pars = c("DestinationCoastalRegion", "DestinationCity", "TransportationPlane", "TransportationCar", "AccommodationHotel", "AccommodationHolidayHome" , "ActivitiesSightseeing", "ActivitiesRelaxation" ))
summary(mnl_pref)

#Probability prediction for simulation profiles
predict_mnl_pref <- predict(mnl_pref, newdata = sim_profiles, obsID = "obsID")
predict_mnl_pref

#Comparison of predicted values with actual values

#RBC: Comparison of average ranking with predicted total utility

#Average rank of all stimuli

Rank_Stimulus1 <- round(mean(preferences$SQ001), 2)
Rank_Stimulus2 <- round(mean(preferences$SQ002), 2)
Rank_Stimulus3 <- round(mean(preferences$SQ003), 2)
Rank_Stimulus4 <- round(mean(preferences$SQ004), 2)
Rank_Stimulus5 <- round(mean(preferences$SQ005), 2)
Rank_Stimulus6 <- round(mean(preferences$SQ006), 2)
Rank_Stimulus7 <- round(mean(preferences$SQ007), 2)
Rank_Stimulus8 <- round(mean(preferences$SQ008), 2)
Rank_Stimulus9 <- round(mean(preferences$SQ009), 2)

#Predicted total utility
library(conjoint)
experiment<-expand.grid(
  Destination=c("Coastal region", "City", "Mountains"),
  Transportation=c("Plane", "Car", "Train/Bus"),
  Accommodation=c("Hotel", "Holiday home", "Camping"),
  Activities=c("Sightseeing", "Relaxation", "Sport activities")
)

#Simulation Matrix

profiles <- experiment
sim_profiles <- profiles[c(2, 13, 30, 46, 62),]
sim_profiles
profiles <- caEncodedDesign(profiles)


#Estimation of the model
design_RBC=caFactorialDesign(data=experiment,type="orthogonal")
design_RBC

code=caEncodedDesign(design_RBC)
code

levna <- c("Coastal Region", "City", "Mountains","Plane", "Car", "Train/Bus","Hotel", "Holiday home", "Camping", "Sightseeing", "Relaxation", "Sport activities")

library(readxl)
preferences <- read_excel("../data/Data_RBC.xlsx", sheet = "RBC_ready for analysis", range = "I1:Q199")
preferences

library(conjoint)
model <- Conjoint(preferences, code, levna, y.type = "rank")
results_simulation <- ShowAllSimulations(profiles[c(5,10,27,34,42,47,57,71,76), ], preferences, code)
results_simulation <- as.data.frame(results_simulation)

table_RCB <- data.frame(
  Stimulus = c(1,2,3,4,5,6,7,8,9),
  "Average Ranking" = c(Rank_Stimulus1, Rank_Stimulus2, Rank_Stimulus3, Rank_Stimulus4, Rank_Stimulus5, Rank_Stimulus6, Rank_Stimulus7, Rank_Stimulus8, Rank_Stimulus9),
  "Predicted Total Utility" = results_simulation$TotalUtility
)
print(table_RCB)

cor(table_RCB$Average.Ranking, table_RCB$Predicted.Total.Utility)

#CBC

#Actual probability of being chosen

library(readxl)
preference <- read_excel("../data/Data_CBC.xlsx", sheet = "CBC_ready for analysis_Effect")
#View(preference)
str(preference)


Question_1_Alt1 <- preference[preference$qID == 1 & preference$altID == 1, ]
prop_Question_1_Alt1 <- as.data.frame(prop.table(table(Question_1_Alt1$Choice)))
prop_Question_1_Alt1

Question_2_Alt1 <- preference[preference$qID == 2 & preference$altID == 1, ]
prop_Question_2_Alt1 <- as.data.frame(prop.table(table(Question_2_Alt1$Choice)))
prop_Question_2_Alt1

Question_3_Alt1 <- preference[preference$qID == 3 & preference$altID == 1, ]
prop_Question_3_Alt1 <- as.data.frame(prop.table(table(Question_3_Alt1$Choice)))
prop_Question_3_Alt1

Question_4_Alt1 <- preference[preference$qID == 4 & preference$altID == 1, ]
prop_Question_4_Alt1 <- as.data.frame(prop.table(table(Question_4_Alt1$Choice)))
prop_Question_4_Alt1

Question_5_Alt1 <- preference[preference$qID == 5 & preference$altID == 1, ]
prop_Question_5_Alt1 <- as.data.frame(prop.table(table(Question_5_Alt1$Choice)))
prop_Question_5_Alt1

Question_6_Alt1 <- preference[preference$qID == 6 & preference$altID == 1, ]
prop_Question_6_Alt1 <- as.data.frame(prop.table(table(Question_6_Alt1$Choice)))
prop_Question_6_Alt1

Question_7_Alt1 <- preference[preference$qID == 7 & preference$altID == 1, ]
prop_Question_7_Alt1 <- as.data.frame(prop.table(table(Question_7_Alt1$Choice)))
prop_Question_7_Alt1

Question_8_Alt1 <- preference[preference$qID == 8 & preference$altID == 1, ]
prop_Question_8_Alt1 <- as.data.frame(prop.table(table(Question_8_Alt1$Choice)))
prop_Question_8_Alt1

Question_9_Alt1 <- preference[preference$qID == 9 & preference$altID == 1, ]
prop_Question_9_Alt1 <- as.data.frame(prop.table(table(Question_9_Alt1$Choice)))
prop_Question_9_Alt1

#Predicted probability of being chosen

library(readxl)
sim_profiles <- read_excel("../data/Simulation_CBC.xlsx", sheet = "Used profiles in CBC")
predict_mnl_pref <- predict(mnl_pref, newdata = sim_profiles, obsID = "obsID")
predict_mnl_pref <- as.data.frame(predict_mnl_pref)
predict_mnl_pref


table_CBC <- data.frame(
  Observation = c(1,2,3,4,5,6,7,8,9),
  "Actual probability of choosing alternative 1" = c(prop_Question_1_Alt1[2,2], prop_Question_2_Alt1[2,2], prop_Question_3_Alt1[2,2], prop_Question_4_Alt1[2,2], prop_Question_5_Alt1[2,2], prop_Question_6_Alt1[2,2], prop_Question_7_Alt1[2,2], prop_Question_8_Alt1[2,2], prop_Question_9_Alt1[2,2]),
  "Predicted probability of choosing alternative 1" = c(predict_mnl_pref[1,2], predict_mnl_pref[3,2], predict_mnl_pref[5,2], predict_mnl_pref[7,2], predict_mnl_pref[9,2], predict_mnl_pref[11,2], predict_mnl_pref[13,2], predict_mnl_pref[15,2], predict_mnl_pref[17,2])
)
table_CBC

cor(table_CBC$Actual.probability.of.choosing.alternative.1, table_CBC$Predicted.probability.of.choosing.alternative.1)
