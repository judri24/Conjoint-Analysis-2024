#Clustering CBC

library(readxl)
x <- read_excel("Data_CBC.xlsx", sheet = "CBC_ Choices_02")
View(x)

x$Q01_A001 <- as.factor(x$Q01_A001)
x$Q01_A002 <- as.factor(x$Q01_A002)
x$Q02_A001 <- as.factor(x$Q02_A001)
x$Q02_A002 <- as.factor(x$Q02_A002)
x$Q03_A001 <- as.factor(x$Q03_A001)
x$Q03_A002 <- as.factor(x$Q03_A002)
x$Q04_A001 <- as.factor(x$Q04_A001)
x$Q04_A002 <- as.factor(x$Q04_A002)
x$Q05_A001 <- as.factor(x$Q05_A001)
x$Q05_A002 <- as.factor(x$Q05_A002)
x$Q06_A001 <- as.factor(x$Q06_A001)
x$Q06_A002 <- as.factor(x$Q06_A002)
x$Q07_A001 <- as.factor(x$Q07_A001)
x$Q07_A002 <- as.factor(x$Q07_A002)
x$Q08_A001 <- as.factor(x$Q08_A001)
x$Q08_A002 <- as.factor(x$Q08_A002)
x$Q09_A001 <- as.factor(x$Q09_A001)
x$Q09_A002 <- as.factor(x$Q09_A002)


matrix <- as.matrix(x[,c("Q01_A001","Q01_A002","Q02_A001", "Q02_A002", "Q03_A001", "Q03_A002", "Q04_A001","Q04_A002", "Q05_A001", "Q05_A002", "Q06_A001", "Q06_A002", "Q07_A001", "Q07_A002", "Q08_A001", "Q08_A002", "Q09_A001", "Q09_A002")])
print(matrix)

y <- kmeans(matrix, 2)

x$cluster <- y$cluster


#Clustering RCB

library(conjoint)
experiment<-expand.grid(
  Destination=c("Coastal region", "City", "Mountains"),
  Transportation=c("Plane", "Car", "Train/Bus"),
  Accommodation=c("Hotel", "Holiday home", "Camping"),
  Activities=c("Sightseeing", "Relaxation", "Sport activities")
)

design_RCB=caFactorialDesign(data=experiment,type="orthogonal")
design_RCB

code=caEncodedDesign(design_RCB)
code

library(readxl)
preferences <- read_excel("Data_RBC.xlsx", sheet = "RBC_ready for analysis", range = "I1:Q199")
preferences

cluster <- caSegmentation(preferences, code)

preferences$cluster <- cluster$sclu


#Correlation analysis of both clusterings

library(readxl)
data_clusters <- read_excel("Cluster analysis.xlsx")
data_clusters

cor(data_clusters$cluster_RCB, data_clusters$cluster_CBC)

#Sociodemographics of groups

library(readxl)
socio <- read_excel("Sociodemographics.xlsx", "Sociodemogr_for clustering")
socio

#Cluster 1

subset_cluster1 <- socio[socio$cluster_RBC == 1,]
subset_cluster1

#Analysis of Age
mean(subset_cluster1$Age, na.rm = TRUE)
prop.table(table(subset_cluster1$`Age categories`))

#Analysis of Gender
prop.table(table(subset_cluster1$Gender))

#Analysis of Occupation
prop.table(table(subset_cluster1$Occupation))

#Analysis of travel companion
prop.table(table(subset_cluster1$Travel_companion))

#Cluster 2

subset_cluster2 <- socio[socio$cluster_RBC == 2,]
subset_cluster2

#Analysis of Age
mean(subset_cluster2$Age, na.rm = TRUE)
prop.table(table(subset_cluster2$`Age categories`))

#Analysis of Gender
prop.table(table(subset_cluster2$Gender))

#Analysis of Occupation
prop.table(table(subset_cluster2$Occupation))

#Analysis of travel companion
prop.table(table(subset_cluster2$Travel_companion))

#Analysis of Means and Variances

library(readxl)
RBC_Cluster <- read_excel("Data_RBC.xlsx", sheet = "RBC_Cluster")
RBC_Cluster

#Entire sample

Q1_mean <- mean(RBC_Cluster$SQ001)
Q2_mean <- mean(RBC_Cluster$SQ002)
Q3_mean <- mean(RBC_Cluster$SQ003)
Q4_mean <- mean(RBC_Cluster$SQ004)
Q5_mean <- mean(RBC_Cluster$SQ005)
Q6_mean <- mean(RBC_Cluster$SQ006)
Q7_mean <- mean(RBC_Cluster$SQ007)
Q8_mean <- mean(RBC_Cluster$SQ008)
Q9_mean <- mean(RBC_Cluster$SQ009)

Q1_variance <- var(RBC_Cluster$SQ001)
Q2_variance <- var(RBC_Cluster$SQ002)
Q3_variance <- var(RBC_Cluster$SQ003)
Q4_variance <- var(RBC_Cluster$SQ004)
Q5_variance <- var(RBC_Cluster$SQ005)
Q6_variance <- var(RBC_Cluster$SQ006)
Q7_variance <- var(RBC_Cluster$SQ007)
Q8_variance <- var(RBC_Cluster$SQ008)
Q9_variance <- var(RBC_Cluster$SQ009)

#Cluster1

RBC_Cluster1 <-  RBC_Cluster[RBC_Cluster$cluster_RBC == 1, ]

Q1_mean1 <- mean(RBC_Cluster1$SQ001)
Q2_mean1 <- mean(RBC_Cluster1$SQ002)
Q3_mean1 <- mean(RBC_Cluster1$SQ003)
Q4_mean1 <- mean(RBC_Cluster1$SQ004)
Q5_mean1 <- mean(RBC_Cluster1$SQ005)
Q6_mean1 <- mean(RBC_Cluster1$SQ006)
Q7_mean1 <- mean(RBC_Cluster1$SQ007)
Q8_mean1 <- mean(RBC_Cluster1$SQ008)
Q9_mean1 <- mean(RBC_Cluster1$SQ009)

Q1_variance1 <- var(RBC_Cluster1$SQ001)
Q2_variance1 <- var(RBC_Cluster1$SQ002)
Q3_variance1 <- var(RBC_Cluster1$SQ003)
Q4_variance1 <- var(RBC_Cluster1$SQ004)
Q5_variance1 <- var(RBC_Cluster1$SQ005)
Q6_variance1 <- var(RBC_Cluster1$SQ006)
Q7_variance1 <- var(RBC_Cluster1$SQ007)
Q8_variance1 <- var(RBC_Cluster1$SQ008)
Q9_variance1 <- var(RBC_Cluster1$SQ009)

#Cluster2

RBC_Cluster2 <-  RBC_Cluster[RBC_Cluster$cluster_RBC == 2, ]

Q1_mean2 <- mean(RBC_Cluster2$SQ001)
Q2_mean2 <- mean(RBC_Cluster2$SQ002)
Q3_mean2 <- mean(RBC_Cluster2$SQ003)
Q4_mean2 <- mean(RBC_Cluster2$SQ004)
Q5_mean2 <- mean(RBC_Cluster2$SQ005)
Q6_mean2 <- mean(RBC_Cluster2$SQ006)
Q7_mean2 <- mean(RBC_Cluster2$SQ007)
Q8_mean2 <- mean(RBC_Cluster2$SQ008)
Q9_mean2 <- mean(RBC_Cluster2$SQ009)

Q1_variance2 <- var(RBC_Cluster2$SQ001)
Q2_variance2 <- var(RBC_Cluster2$SQ002)
Q3_variance2 <- var(RBC_Cluster2$SQ003)
Q4_variance2 <- var(RBC_Cluster2$SQ004)
Q5_variance2 <- var(RBC_Cluster$SQ005)
Q6_variance2 <- var(RBC_Cluster2$SQ006)
Q7_variance2 <- var(RBC_Cluster2$SQ007)
Q8_variance2 <- var(RBC_Cluster2$SQ008)
Q9_variance2 <- var(RBC_Cluster2$SQ009)

table_comparison <- data.frame(
  "Stimuli" = c("1","2", "3", "4", "5", "6","7","8","9"),
  "Mean Complete" = c(Q1_mean, Q2_mean, Q3_mean, Q4_mean, Q5_mean, Q6_mean, Q7_mean, Q8_mean, Q9_mean),
  "Mean Cluster1" = c(Q1_mean1, Q2_mean1, Q3_mean1, Q4_mean1, Q5_mean1, Q6_mean1, Q7_mean1, Q8_mean1, Q9_mean1),
  "Mean Cluster2" = c(Q1_mean2, Q2_mean2, Q3_mean2, Q4_mean2, Q5_mean2, Q6_mean2, Q7_mean2, Q8_mean2, Q9_mean2),
  "Variance Complete" = c(Q1_variance, Q2_variance, Q3_variance, Q4_variance, Q5_variance, Q6_variance, Q7_variance, Q8_variance, Q9_variance),
  "Variance Complete1" = c(Q1_variance1, Q2_variance1, Q3_variance1, Q4_variance1, Q5_variance1, Q6_variance1, Q7_variance1, Q8_variance1, Q9_variance1),
  "Variance Complete2" = c(Q1_variance2, Q2_variance2, Q3_variance2, Q4_variance2, Q5_variance2, Q6_variance2, Q7_variance2, Q8_variance2, Q9_variance2)
)

table_comparison

Average_Variance <- mean(Q1_variance, Q2_variance, Q3_variance, Q4_variance, Q5_variance, Q6_variance, Q7_variance, Q8_variance, Q9_variance)
Average_Variance_Cluster1 <- mean(Q1_variance1, Q2_variance1, Q3_variance1, Q4_variance1, Q5_variance1, Q6_variance1, Q7_variance1, Q8_variance1, Q9_variance1)
Average_Variance_Cluster2 <- mean(Q1_variance2, Q2_variance2, Q3_variance2, Q4_variance2, Q5_variance2, Q6_variance2, Q7_variance2, Q8_variance2, Q9_variance2)

average_Variances <- data.frame(
  Sample = c("Entire Sample", "Cluster 1", "Cluster 2"),
  "Average Variance" = c(Average_Variance, Average_Variance_Cluster1, Average_Variance_Cluster2)
)
average_Variances

#Analysis of frequencies of choosing an alternative

#Entire sample

library(readxl)
CBC_Cluster <- read_excel("Data_CBC.xlsx", sheet = "Cluster_not edited")
CBC_Cluster

Q1_freq <- prop.table(table(CBC_Cluster$Q02_A001))
Q2_freq <- prop.table(table(CBC_Cluster$Q03_A001))
Q3_freq <- prop.table(table(CBC_Cluster$Q04_A001))
Q4_freq <- prop.table(table(CBC_Cluster$Q05_A001))
Q5_freq <- prop.table(table(CBC_Cluster$Q06_A001))
Q6_freq <- prop.table(table(CBC_Cluster$Q07_A001))
Q7_freq <- prop.table(table(CBC_Cluster$Q08_A001))
Q8_freq <- prop.table(table(CBC_Cluster$Q09_A001))
Q9_freq <- prop.table(table(CBC_Cluster$Q10_A001))

#Cluster1

CBC_Cluster1 <-  CBC_Cluster[RBC_Cluster$cluster_RBC == 1, ]

Q1_freq1 <- prop.table(table(CBC_Cluster1$Q02_A001))
Q2_freq1 <- prop.table(table(CBC_Cluster1$Q03_A001))
Q3_freq1 <- prop.table(table(CBC_Cluster1$Q04_A001))
Q4_freq1 <- prop.table(table(CBC_Cluster1$Q05_A001))
Q5_freq1 <- prop.table(table(CBC_Cluster1$Q06_A001))
Q6_freq1 <- prop.table(table(CBC_Cluster1$Q07_A001))
Q7_freq1 <- prop.table(table(CBC_Cluster1$Q08_A001))
Q8_freq1 <- prop.table(table(CBC_Cluster1$Q09_A001))
Q9_freq1 <- prop.table(table(CBC_Cluster1$Q10_A001))

#Cluster2

CBC_Cluster2 <-  CBC_Cluster[RBC_Cluster$cluster_RBC == 2, ]

Q1_freq2 <- prop.table(table(CBC_Cluster2$Q02_A001))
Q2_freq2 <- prop.table(table(CBC_Cluster2$Q03_A001))
Q3_freq2 <- prop.table(table(CBC_Cluster2$Q04_A001))
Q4_freq2 <- prop.table(table(CBC_Cluster2$Q05_A001))
Q5_freq2 <- prop.table(table(CBC_Cluster2$Q06_A001))
Q6_freq2 <- prop.table(table(CBC_Cluster2$Q07_A001))
Q7_freq2 <- prop.table(table(CBC_Cluster2$Q08_A001))
Q8_freq2 <- prop.table(table(CBC_Cluster2$Q09_A001))
Q9_freq2 <- prop.table(table(CBC_Cluster2$Q10_A001))


table_comparison <- data.frame(
  "Stimuli" = c("1","2", "3", "4", "5", "6","7","8","9"),
  "Freq Complete" = c(Q1_freq, Q2_freq, Q3_freq, Q4_freq, Q5_freq, Q6_freq, Q7_freq, Q8_freq, Q9_freq),
  "Freq Cluster1" = c(Q1_freq1, Q2_freq1, Q3_freq1, Q4_freq1, Q5_freq1, Q6_freq1, Q7_freq1, Q8_freq1, Q9_freq1),
  "Freq Cluster2" = c(Q1_freq2, Q2_freq2, Q3_freq2, Q4_freq2, Q5_freq2, Q6_freq2, Q7_freq2, Q8_freq2, Q9_freq2)
)

table_comparison

# RBC with clusters from RBC


library(readxl)
cluster <- read_excel("Data_RBC.xlsx", sheet = "RBC_Cluster")
cluster

clusterreg <- read_excel("Data_RBC.xlsx", sheet = "RBC_Regression analysis_Cluster")
clusterreg

#Cluster 1
clusterRBC2_1 <- cluster[cluster$cluster_RBC == 1, ]
subset_clusterRBC2_1 <- clusterRBC2_1[, c("SQ001", "SQ002", "SQ003", "SQ004", "SQ005", "SQ006", "SQ007", "SQ008", "SQ009")]
str(subset_clusterRBC2_1) 

clusterregRBC2_1 <- clusterreg[clusterreg$cluster_RBC == 1, ]
clusterregRBC2_1$DestinationCoastalRegion <- (-1) *clusterregRBC2_1$DestinationCoastalRegion
clusterregRBC2_1$DestinationCity <- (-1) * clusterregRBC2_1$DestinationCity
clusterregRBC2_1$TransportationPlane <- (-1) * clusterregRBC2_1$TransportationPlane
clusterregRBC2_1$TransportationCar <- (-1) * clusterregRBC2_1$TransportationCar
clusterregRBC2_1$AccommodationHotel <- (-1) * clusterregRBC2_1$AccommodationHotel
clusterregRBC2_1$AccommodationHolidayhome <- (-1) * clusterregRBC2_1$AccommodationHolidayhome
clusterregRBC2_1$ActivitiesSightseeing <- (-1) * clusterregRBC2_1$ActivitiesSightseeing
clusterregRBC2_1$ActivitiesRelaxation <- (-1) * clusterregRBC2_1$ActivitiesRelaxation

# Conjoint(data on preference, coded research design, the names of variables and levels, the type of data preferences)

library(conjoint)
Conjoint(subset_clusterRBC2_1, code, levna, y.type = "rank")

model1 <- lm(evaluation ~ DestinationCoastalRegion + DestinationCity + TransportationPlane + TransportationCar + AccommodationHotel + AccommodationHolidayhome + ActivitiesSightseeing + ActivitiesRelaxation, data = clusterregRBC2_1)
summary(model1)

# DestinationMountains = -0.7068966
DestinationMountains1 <- as.numeric((-1) * (coef(model1)["DestinationCoastalRegion"] + coef(model1)["DestinationCity"]))
DestinationMountains1

# TransportationTrainBus =  -0.362069
TransportationTrainBus1 <- as.numeric((-1) * (coef(model1)["TransportationPlane"] + coef(model1)["TransportationCar"]))
TransportationTrainBus1

# AccommodationCamping = -2.014368
AccommodationCamping1 <-as.numeric((-1) * (coef(model1)["AccommodationHotel"] + coef(model1)["AccommodationHolidayhome"]))
AccommodationCamping1

# ActivitiesSportActivities = -0.6034483
ActivitiesSportActivities1 <-as.numeric((-1) * (coef(model1)["ActivitiesSightseeing"] + coef(model1)["ActivitiesRelaxation"]))
ActivitiesSportActivities1

#Determining the standard errors

covMatrix <- vcov(model1)
sqrt(diag(covMatrix))

#SE of DestinationMountains =  0.0823871
SE_DestinationMountains <- sqrt(sum(covMatrix[2:3, 2:3]))
SE_DestinationMountains

#SE of TransportationTrainBus =  0.0823871
SE_TransportationTrainBus <- sqrt(sum(covMatrix[4:5, 4:5]))
SE_TransportationTrainBus

#SE of AccommodationCamping =  0.0823871
SE_AccommodationCamping <- sqrt(sum(covMatrix[6:7, 6:7]))
SE_AccommodationCamping

#SE of ActivitiesSportActivities =  0.0823871
SE_ActivitiesSportActivities <- sqrt(sum(covMatrix[8:9, 8:9]))
SE_ActivitiesSportActivities

#Determining the t-values 

#t-value of Destination Mountains = -8.580185
t_DestinationMountains <- DestinationMountains1/SE_DestinationMountains
t_DestinationMountains

#t-value of TransportationTrainBus = -4.394729
t_TransportationTrainBus <- TransportationTrainBus1/SE_TransportationTrainBus
t_TransportationTrainBus

#t-value of AccommodationCamping =  -24.45004
t_AccommodationCamping <- AccommodationCamping1/SE_AccommodationCamping
t_AccommodationCamping

#t-value of ActivitiesSportActivities = -7.324548
t_ActivitiesSportActivities <- ActivitiesSportActivities1/SE_ActivitiesSportActivities
t_ActivitiesSportActivities

#Determining p-values

#p-value of DestinationMountains = 3.421562e-17
p_DestinationMountains <- 2*pt(abs(t_DestinationMountains), 1035, lower.tail = FALSE)
p_DestinationMountains

#p-value of TransportationTrainBus =  1.223394e-05
p_TransportationTrainBus <- 2*pt(abs(t_TransportationTrainBus), 1035, lower.tail = FALSE)
p_TransportationTrainBus

#p-value of AccommodationCamping = 1.41264e-104
p_AccommodationCamping <- 2*pt(abs(t_AccommodationCamping), 1035, lower.tail = FALSE)
p_AccommodationCamping

#p-value of ActivitiesSportActivities =  4.813338e-13
p_ActivitiesSportActivities  <- 2*pt(abs(t_ActivitiesSportActivities), 1035, lower.tail = FALSE)
p_ActivitiesSportActivities 

#Table of missing levels

missing_levels<- data.frame(
  Levels = c("DestinationMountains", "TransportationTrainBus", "AccommodationCamping", "ActivitiesSportActivities"),
  Estimate = c(DestinationMountains1, TransportationTrainBus1, AccommodationCamping1, ActivitiesSportActivities1),
  "Standard Error" = c(SE_DestinationMountains, SE_TransportationTrainBus, SE_AccommodationCamping, SE_ActivitiesSportActivities),
  t_value = c(t_DestinationMountains, t_TransportationTrainBus, t_AccommodationCamping, t_ActivitiesSportActivities),
  p_value = c(p_DestinationMountains,p_TransportationTrainBus, p_AccommodationCamping, p_ActivitiesSportActivities)
)

print(missing_levels)

#Relative Importance

#Sum of all ranges

Range_Destination <- (max(coef(model1)["DestinationCoastalRegion"], coef(model1)["DestinationCity"],DestinationMountains1) - min(coef(model1)["DestinationCoastalRegion"], coef(model1)["DestinationCity"],DestinationMountains1))
Range_Transportation <-(max(coef(model1)["TransportationPlane"], coef(model1)["TransportationCar"], TransportationTrainBus1)- min(coef(model1)["TransportationPlane"], coef(model1)["TransportationCar"], TransportationTrainBus1))
Range_Accommodation <- (max(coef(model1)["AccommodationHotel"],coef(model1)["AccommodationHolidayhome"],AccommodationCamping1) - min(coef(model1)["AccommodationHotel"],coef(model1)["AccommodationHolidayhome"],AccommodationCamping1))
Range_Activities <- (max(coef(model1)["ActivitiesSightseeing"], coef(model1)["ActivitiesRelaxation"], ActivitiesSportActivities1) - min(coef(model1)["ActivitiesSightseeing"], coef(model1)["ActivitiesRelaxation"], ActivitiesSportActivities1))
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
  Importance = c(Imp_Destination, Imp_Transportation, Imp_Accommodation, Imp_Activities)
)
print(importance_table)

#Cluster 2
clusterRBC2_2 <- cluster[cluster$cluster_RBC == 2, ]
subset_clusterRBC2_2 <- clusterRBC2_2[, c("SQ001", "SQ002", "SQ003", "SQ004", "SQ005", "SQ006", "SQ007", "SQ008", "SQ009")]
str(subset_clusterRBC2_2) 

clusterregRBC2_2 <- clusterreg[clusterreg$cluster_RBC == 2, ]
clusterregRBC2_2$DestinationCoastalRegion <- (-1) *clusterregRBC2_2$DestinationCoastalRegion
clusterregRBC2_2$DestinationCity <- (-1) * clusterregRBC2_2$DestinationCity
clusterregRBC2_2$TransportationPlane <- (-1) * clusterregRBC2_2$TransportationPlane
clusterregRBC2_2$TransportationCar <- (-1) * clusterregRBC2_2$TransportationCar
clusterregRBC2_2$AccommodationHotel <- (-1) * clusterregRBC2_2$AccommodationHotel
clusterregRBC2_2$AccommodationHolidayhome <- (-1) * clusterregRBC2_2$AccommodationHolidayhome
clusterregRBC2_2$ActivitiesSightseeing <- (-1) * clusterregRBC2_2$ActivitiesSightseeing
clusterregRBC2_2$ActivitiesRelaxation <- (-1) * clusterregRBC2_2$ActivitiesRelaxation

# Conjoint(data on preference, coded research design, the names of variables and levels, the type of data preferences)

library(conjoint)
Conjoint(subset_clusterRBC2_2, code, levna, y.type = "rank")

model2 <- lm(evaluation ~ DestinationCoastalRegion + DestinationCity + TransportationPlane + TransportationCar + AccommodationHotel + AccommodationHolidayhome + ActivitiesSightseeing + ActivitiesRelaxation, data = clusterregRBC2_2)
summary(model2)

# DestinationMountains = 0.695122
DestinationMountains2 <- as.numeric((-1) * (coef(model2)["DestinationCoastalRegion"] + coef(model2)["DestinationCity"]))
DestinationMountains2

# TransportationTrainBus =  0.06097561
TransportationTrainBus2 <- as.numeric((-1) * (coef(model2)["TransportationPlane"] + coef(model2)["TransportationCar"]))
TransportationTrainBus2

# AccommodationCamping =  0.1260163
AccommodationCamping2 <-as.numeric((-1) * (coef(model2)["AccommodationHotel"] + coef(model2)["AccommodationHolidayhome"]))
AccommodationCamping2

# ActivitiesSportActivities = 0.1829268
ActivitiesSportActivities2 <-as.numeric((-1) * (coef(model2)["ActivitiesSightseeing"] + coef(model2)["ActivitiesRelaxation"]))
ActivitiesSportActivities2

#Determining the standard errors

covMatrix <- vcov(model2)
sqrt(diag(covMatrix))

#SE of DestinationMountains = 0.1136106
SE_DestinationMountains <- sqrt(sum(covMatrix[2:3, 2:3]))
SE_DestinationMountains

#SE of TransportationTrainBus =  0.1136106
SE_TransportationTrainBus <- sqrt(sum(covMatrix[4:5, 4:5]))
SE_TransportationTrainBus

#SE of AccommodationCamping = 0.1136106
SE_AccommodationCamping <- sqrt(sum(covMatrix[6:7, 6:7]))
SE_AccommodationCamping

#SE of ActivitiesSportActivities =  0.1136106
SE_ActivitiesSportActivities <- sqrt(sum(covMatrix[8:9, 8:9]))
SE_ActivitiesSportActivities

#Determining the t-values 

#t-value of Destination Mountains = 6.118458
t_DestinationMountains <- DestinationMountains2/SE_DestinationMountains
t_DestinationMountains

#t-value of TransportationTrainBus =  0.5367069
t_TransportationTrainBus <- TransportationTrainBus2/SE_TransportationTrainBus
t_TransportationTrainBus

#t-value of AccommodationCamping =  1.109194
t_AccommodationCamping <- AccommodationCamping2/SE_AccommodationCamping
t_AccommodationCamping

#t-value of ActivitiesSportActivities = 1.610121
t_ActivitiesSportActivities <- ActivitiesSportActivities2/SE_ActivitiesSportActivities
t_ActivitiesSportActivities

#Determining p-values

#p-value of DestinationMountains =  1.541537e-09
p_DestinationMountains <- 2*pt(abs(t_DestinationMountains), 729, lower.tail = FALSE)
p_DestinationMountains

#p-value of TransportationTrainBus =   0.5916339
p_TransportationTrainBus <- 2*pt(abs(t_TransportationTrainBus), 729, lower.tail = FALSE)
p_TransportationTrainBus

#p-value of AccommodationCamping = 0.2677122
p_AccommodationCamping <- 2*pt(abs(t_AccommodationCamping), 729, lower.tail = FALSE)
p_AccommodationCamping

#p-value of ActivitiesSportActivities =   0.1078044
p_ActivitiesSportActivities  <- 2*pt(abs(t_ActivitiesSportActivities), 729, lower.tail = FALSE)
p_ActivitiesSportActivities 

#Table of missing levels

missing_levels<- data.frame(
  Levels = c("DestinationMountains", "TransportationTrainBus", "AccommodationCamping", "ActivitiesSportActivities"),
  Estimate = c(DestinationMountains2, TransportationTrainBus2, AccommodationCamping2, ActivitiesSportActivities2),
  "Standard Error" = c(SE_DestinationMountains, SE_TransportationTrainBus, SE_AccommodationCamping, SE_ActivitiesSportActivities),
  t_value = c(t_DestinationMountains, t_TransportationTrainBus, t_AccommodationCamping, t_ActivitiesSportActivities),
  p_value = c(p_DestinationMountains,p_TransportationTrainBus, p_AccommodationCamping, p_ActivitiesSportActivities)
)

print(missing_levels)

#Relative Importance

#Sum of all ranges

Range_Destination <- (max(coef(model2)["DestinationCoastalRegion"], coef(model2)["DestinationCity"],DestinationMountains2) - min(coef(model2)["DestinationCoastalRegion"], coef(model2)["DestinationCity"],DestinationMountains2))
Range_Transportation <-(max(coef(model2)["TransportationPlane"], coef(model2)["TransportationCar"], TransportationTrainBus2)- min(coef(model2)["TransportationPlane"], coef(model2)["TransportationCar"], TransportationTrainBus2))
Range_Accommodation <- (max(coef(model2)["AccommodationHotel"],coef(model2)["AccommodationHolidayhome"],AccommodationCamping2) -min(coef(model2)["AccommodationHotel"],coef(model2)["AccommodationHolidayhome"],AccommodationCamping2))
Range_Activities <- (max(coef(model2)["ActivitiesSightseeing"], coef(model2)["ActivitiesRelaxation"], ActivitiesSportActivities2) - min(coef(model2)["ActivitiesSightseeing"], coef(model2)["ActivitiesRelaxation"], ActivitiesSportActivities2))
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
  Importance = c(Imp_Destination, Imp_Transportation, Imp_Accommodation, Imp_Activities)
)
print(importance_table)

levels <- c("Coastal region", "City", "Mountains", "Plane", "Car", "Train/Bus", "Hotel", "Holiday home", "Camping", "Sightseeing", "Relaxation", "Sport activities")

cluster_table_RBC <- data.frame(
  Levels = factor(levels, levels = levels),
  "Cluster_RBC 1" = c(coef(model1)["DestinationCoastalRegion"], coef(model1)["DestinationCity"],DestinationMountains1, coef(model1)["TransportationPlane"], coef(model1)["TransportationCar"], TransportationTrainBus1, coef(model1)["AccommodationHotel"],coef(model1)["AccommodationHolidayhome"],AccommodationCamping1,coef(model1)["ActivitiesSightseeing"], coef(model1)["ActivitiesRelaxation"], ActivitiesSportActivities1),
  "Cluster_RBC 2" = c(coef(model2)["DestinationCoastalRegion"], coef(model2)["DestinationCity"],DestinationMountains2, coef(model2)["TransportationPlane"], coef(model2)["TransportationCar"], TransportationTrainBus2, coef(model2)["AccommodationHotel"],coef(model2)["AccommodationHolidayhome"],AccommodationCamping2,coef(model2)["ActivitiesSightseeing"], coef(model2)["ActivitiesRelaxation"], ActivitiesSportActivities2)
)

cluster_table_RBC

library(reshape2)
library(ggplot2)

cluster_table_RBC_melt <- melt(cluster_table_RBC, id.vars = "Levels", variable.name = "Cluster", value.name = "Value")
cluster_table_RBC_melt

ggplot(cluster_table_RBC_melt, aes(x = Levels, y = Value, fill = Cluster)) + 
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label = round(Value, 2)), vjust = -0.5, hjust = 0.5, position = position_dodge(0.9), size = 5) +
  labs(x = "Levels",
       y = "Values") +
  theme_minimal(base_family = "Arial", base_size = 20) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_manual(values = c("Cluster_TCA.1" = "darkgrey", "Cluster_TCA.2" = "lightgrey"),
                    labels = c("Cluster 1", "Cluster 2"))

# CBC with clusters from RBC
library(readxl)
x <- read_excel("Data_CBC.xlsx", sheet = "CBC_Cluster")
#View(x)

#Cluster 1

clusterCBC2_1 <- x[x$cluster_RBC == 1, ]
print(clusterCBC2_1)
library(logitr)

mnl_pref1 <- logitr(data = clusterCBC2_1, outcome = "Choice", obsID = "ObsID", pars = c("DestinationCoastalRegion", "DestinationCity", "TransportationPlane", "TransportationCar", "AccommodationHotel", "AccommodationHolidayHome" , "ActivitiesSightseeing", "ActivitiesRelaxation" ))
summary(mnl_pref1)

coef <- mnl_pref1$coefficients
coef
# DestinationMountains = -0.2224811
DestinationMountains1 <- as.numeric((-1) * (coef(mnl_pref1)["DestinationCoastalRegion"] + coef(mnl_pref1)["DestinationCity"]))
DestinationMountains1

# TransportationTrainBus = -0.2179515
TransportationTrainBus1 <- as.numeric((-1) * (coef(mnl_pref1)["TransportationPlane"] + (coef(mnl_pref1)["TransportationCar"])))
TransportationTrainBus1

# AccommodationCamping = -0.2990224
AccommodationCamping1 <- as.numeric((-1) * (coef(mnl_pref1)["AccommodationHotel"] - coef(mnl_pref1)["AccommodationHolidayHome"]))
AccommodationCamping1

# ActivitiesSportActivities = -0.3181535
ActivitiesSportActivities1 <- as.numeric((-1) * (coef(mnl_pref1)["ActivitiesSightseeing"] + coef(mnl_pref1)["ActivitiesRelaxation"]))
ActivitiesSportActivities1

#Determining the standard errors

covMatrix <- vcov(mnl_pref1)
sqrt(diag(covMatrix))

#SE of DestinationMountains =  0.08535977
SE_DestinationMountains <- sqrt(sum(covMatrix[1:2, 1:2]))
SE_DestinationMountains

#SE of TransportationTrainBus = 11957.06
SE_TransportationTrainBus <- sqrt(sum(covMatrix[3:4, 3:4]))
SE_TransportationTrainBus

#SE of AccommodationCamping =  0.0812717
SE_AccommodationCamping <- sqrt(sum(covMatrix[5:6, 5:6]))
SE_AccommodationCamping

#SE of ActivitiesSportActivities = 11957.06
SE_ActivitiesSportActivities <- sqrt(sum(covMatrix[7:8, 7:8]))
SE_ActivitiesSportActivities

#Determining the z-values 

#z-value of Destination Mountains = -2.606393
z_DestinationMountains <- DestinationMountains1/SE_DestinationMountains
z_DestinationMountains

#z-value of TransportationTrainBus = -1.822785e-05
z_TransportationTrainBus <- TransportationTrainBus1/SE_TransportationTrainBus
z_TransportationTrainBus

#z-value of AccommodationCamping = -3.679294
z_AccommodationCamping <- AccommodationCamping1/SE_AccommodationCamping
z_AccommodationCamping

#z-value of ActivitiesSportActivities = -2.660801e-05
z_ActivitiesSportActivities <- ActivitiesSportActivities1/SE_ActivitiesSportActivities
z_ActivitiesSportActivities

#Determining p-values

#p-value of DestinationMountains =  0.009150133
p_DestinationMountains <- 2*pnorm(abs(z_DestinationMountains), lower.tail = FALSE)
p_DestinationMountains

#p-value of TransportationTrainBus = 0.9999855
p_TransportationTrainBus <- 2*pnorm(abs(z_TransportationTrainBus), lower.tail = FALSE)
p_TransportationTrainBus

#p-value of AccommodationCamping =  0.0002338809
p_AccommodationCamping <- 2*pnorm(abs(z_AccommodationCamping), lower.tail = FALSE)
p_AccommodationCamping

#p-value of ActivitiesSportActivities = 0.9999788
p_ActivitiesSportActivities  <- 2*pnorm(abs(z_ActivitiesSportActivities), lower.tail = FALSE)
p_ActivitiesSportActivities 

#Table of missing levels

missing_levels_cluster2_1<- data.frame(
  Levels = c("DestinationMountains", "TransportationTrainBus", "AccommodationCamping", "ActivitiesSportActivities"),
  Estimate = c(DestinationMountains1, TransportationTrainBus1, AccommodationCamping1, ActivitiesSportActivities1),
  "Standard Error" = c(SE_DestinationMountains, SE_TransportationTrainBus, SE_AccommodationCamping, SE_ActivitiesSportActivities),
  z_value = c(z_DestinationMountains, z_TransportationTrainBus, z_AccommodationCamping, z_ActivitiesSportActivities),
  p_value = c(p_DestinationMountains,p_TransportationTrainBus, p_AccommodationCamping, p_ActivitiesSportActivities)
)

print(missing_levels_cluster2_1)

#Relative Importance

#Sum of all ranges

Range_Destination <- (max(coef(mnl_pref1)["DestinationCoastalRegion"], coef(mnl_pref1)["DestinationCity"], DestinationMountains1) - min(coef(mnl_pref1)["DestinationCoastalRegion"], coef(mnl_pref1)["DestinationCity"], DestinationMountains1))
Range_Transportation <-(max(coef(mnl_pref1)["TransportationPlane"], coef(mnl_pref1)["TransportationCar"], TransportationTrainBus1) - min(coef(mnl_pref1)["TransportationPlane"], coef(mnl_pref1)["TransportationCar"], TransportationTrainBus1))
Range_Accommodation <- (max(coef(mnl_pref1)["AccommodationHotel"],coef(mnl_pref1)["AccommodationHolidayHome"], AccommodationCamping1) - min(coef(mnl_pref1)["AccommodationHotel"],coef(mnl_pref1)["AccommodationHolidayHome"], AccommodationCamping1))
Range_Activities <- (max(coef(mnl_pref1)["ActivitiesSightseeing"], coef(mnl_pref1)["ActivitiesRelaxation"], ActivitiesSportActivities1) - min(coef(mnl_pref1)["ActivitiesSightseeing"], coef(mnl_pref1)["ActivitiesRelaxation"], ActivitiesSportActivities1))
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
  Importance = c(Imp_Destination, Imp_Transportation, Imp_Accommodation, Imp_Activities)
)
print(importance_table)

#Cluster 2

clusterCBC2_2 <- x[x$cluster_RBC == 2, ]
print(clusterCBC2_2)
library(logitr)

mnl_pref2 <- logitr(data = clusterCBC2_2, outcome = "Choice", obsID = "ObsID", pars = c("DestinationCoastalRegion", "DestinationCity", "TransportationPlane", "TransportationCar", "AccommodationHotel", "AccommodationHolidayHome" , "ActivitiesSightseeing", "ActivitiesRelaxation" ))
summary(mnl_pref2)

coef <- mnl_pref2$coefficients

# DestinationMountains = 0.2725183
DestinationMountains2 <- as.numeric((-1) * (coef(mnl_pref2)["DestinationCoastalRegion"] + coef(mnl_pref2)["DestinationCity"]))
DestinationMountains2

# TransportationTrainBus = 0.1381993
TransportationTrainBus2 <- as.numeric((-1) * (coef(mnl_pref2)["TransportationPlane"] + coef(mnl_pref2)["TransportationCar"]))
TransportationTrainBus2

# AccommodationCamping = -0.1037033
AccommodationCamping2 <- as.numeric((-1) * (coef(mnl_pref2)["AccommodationHotel"] + coef(mnl_pref2)["AccommodationHolidayHome"]))
AccommodationCamping2

# ActivitiesSportActivities = 0.1930535
ActivitiesSportActivities2 <- as.numeric((-1) * (coef(mnl_pref2)["ActivitiesSightseeing"] + coef(mnl_pref2)["ActivitiesRelaxation"]))
ActivitiesSportActivities2

#Determining the standard errors

covMatrix <- vcov(mnl_pref2)
sqrt(diag(covMatrix))

#SE of DestinationMountains =  0.09120907
SE_DestinationMountains <- sqrt(sum(covMatrix[1:2, 1:2]))
SE_DestinationMountains

#SE of TransportationTrainBus = NaN
SE_TransportationTrainBus <- sqrt(sum(covMatrix[3:4, 3:4]))
SE_TransportationTrainBus

#SE of AccommodationCamping =  0.07681327
SE_AccommodationCamping <- sqrt(sum(covMatrix[5:6, 5:6]))
SE_AccommodationCamping

#SE of ActivitiesSportActivities = NaN
SE_ActivitiesSportActivities <- sqrt(sum(covMatrix[7:8, 7:8]))
SE_ActivitiesSportActivities

#Determining the z-values 

#z-value of Destination Mountains = 2.987842
z_DestinationMountains <- DestinationMountains2/SE_DestinationMountains
z_DestinationMountains

#z-value of TransportationTrainBus = NaN
z_TransportationTrainBus <- TransportationTrainBus2/SE_TransportationTrainBus
z_TransportationTrainBus

#z-value of AccommodationCamping = -1.35007
z_AccommodationCamping <- AccommodationCamping2/SE_AccommodationCamping
z_AccommodationCamping

#z-value of ActivitiesSportActivities =  NaN
z_ActivitiesSportActivities <- ActivitiesSportActivities2/SE_ActivitiesSportActivities
z_ActivitiesSportActivities

#Determining p-values

#p-value of DestinationMountains = 0.002809546
p_DestinationMountains <- 2*pnorm(abs(z_DestinationMountains), lower.tail = FALSE)
p_DestinationMountains

#p-value of TransportationTrainBus =  NaN
p_TransportationTrainBus <- 2*pnorm(abs(z_TransportationTrainBus), lower.tail = FALSE)
p_TransportationTrainBus

#p-value of AccommodationCamping =  0.1769935
p_AccommodationCamping <- 2*pnorm(abs(z_AccommodationCamping), lower.tail = FALSE)
p_AccommodationCamping

#p-value of ActivitiesSportActivities =  NaN
p_ActivitiesSportActivities  <- 2*pnorm(abs(z_ActivitiesSportActivities), lower.tail = FALSE)
p_ActivitiesSportActivities 

#Table of missing levels

missing_levels_cluster2_2<- data.frame(
  Levels = c("DestinationMountains", "TransportationTrainBus", "AccommodationCamping", "ActivitiesSportActivities"),
  Estimate = c(DestinationMountains2, TransportationTrainBus2, AccommodationCamping2, ActivitiesSportActivities2),
  "Standard Error" = c(SE_DestinationMountains, SE_TransportationTrainBus, SE_AccommodationCamping, SE_ActivitiesSportActivities),
  z_value = c(z_DestinationMountains, z_TransportationTrainBus, z_AccommodationCamping, z_ActivitiesSportActivities),
  p_value = c(p_DestinationMountains,p_TransportationTrainBus, p_AccommodationCamping, p_ActivitiesSportActivities)
)

print(missing_levels_cluster2_2)

#Relative Importance

#Sum of all ranges

Range_Destination <- (max(coef(mnl_pref2)["DestinationCoastalRegion"], coef(mnl_pref2)["DestinationCity"], DestinationMountains2) - min(coef(mnl_pref2)["DestinationCoastalRegion"], coef(mnl_pref2)["DestinationCity"], DestinationMountains2))
Range_Transportation <-(max(coef(mnl_pref2)["TransportationPlane"], coef(mnl_pref2)["TransportationCar"], TransportationTrainBus2) - min(coef(mnl_pref2)["TransportationPlane"], coef(mnl_pref2)["TransportationCar"], TransportationTrainBus2))
Range_Accommodation <- (max(coef(mnl_pref2)["AccommodationHotel"],coef(mnl_pref2)["AccommodationHolidayHome"], AccommodationCamping2) - min(coef(mnl_pref2)["AccommodationHotel"],coef(mnl_pref2)["AccommodationHolidayHome"], AccommodationCamping2))
Range_Activities <- (max(coef(mnl_pref2)["ActivitiesSightseeing"], coef(mnl_pref2)["ActivitiesRelaxation"], ActivitiesSportActivities2) - min(coef(mnl_pref2)["ActivitiesSightseeing"], coef(mnl_pref2)["ActivitiesRelaxation"], ActivitiesSportActivities2))
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
  Importance = c(Imp_Destination, Imp_Transportation, Imp_Accommodation, Imp_Activities)
)
print(importance_table)

levels <- c("Coastal region", "City", "Mountains", "Plane", "Car", "Train/Bus", "Hotel", "Holiday home", "Camping", "Sightseeing", "Relaxation", "Sport activities")

cluster_table_CBC <- data.frame(
  Levels = factor(levels, levels = levels),
  "Cluster_CBC 1" = c(coef(mnl_pref1)["DestinationCoastalRegion"], coef(mnl_pref1)["DestinationCity"],DestinationMountains1, coef(mnl_pref1)["TransportationPlane"], coef(mnl_pref1)["TransportationCar"], TransportationTrainBus1, coef(mnl_pref1)["AccommodationHotel"],coef(mnl_pref1)["AccommodationHolidayHome"],AccommodationCamping1,coef(mnl_pref1)["ActivitiesSightseeing"], coef(mnl_pref1)["ActivitiesRelaxation"], ActivitiesSportActivities1),
  "Cluster_CBC 2" = c(coef(mnl_pref2)["DestinationCoastalRegion"], coef(mnl_pref2)["DestinationCity"],DestinationMountains2, coef(mnl_pref2)["TransportationPlane"], coef(mnl_pref2)["TransportationCar"], TransportationTrainBus2, coef(mnl_pref2)["AccommodationHotel"],coef(mnl_pref2)["AccommodationHolidayHome"],AccommodationCamping2,coef(mnl_pref2)["ActivitiesSightseeing"], coef(mnl_pref2)["ActivitiesRelaxation"], ActivitiesSportActivities2)
)

cluster_table_CBC

#install.packages("reshape2")

library(reshape2)
library(ggplot2)

cluster_table_CBC_melt <- melt(cluster_table_CBC, id.vars = "Levels", variable.name = "Cluster", value.name = "Value")
cluster_table_CBC_melt

ggplot(cluster_table_CBC_melt, aes(x = Levels, y = Value, fill = Cluster)) + 
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label = round(Value, 2)), vjust = -0.5, hjust = 0.5, position = position_dodge(0.9), size = 5) +
  labs(x = "Levels",
       y = "Values") +
  theme_minimal(base_family = "Arial", base_size = 20) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_manual(values = c("Cluster_CBC.1" = "darkgrey", "Cluster_CBC.2" = "lightgrey"),
                    labels = c("Cluster 1", "Cluster 2"))

