#Creation of the full factorial design for the ranking-based conjoint analysis

library(conjoint)
experiment<-expand.grid(
  Destination=c("Coastal region", "City", "Mountains"),
  Transportation=c("Plane", "Car", "Train/Bus"),
  Accommodation=c("Hotel", "Holiday home", "Camping"),
  Activities=c("Sightseeing", "Relaxation", "Sport activities")
)

#Creation of the orthogonal fractional factorial design for the ranking-based conjoint analysis

design_RBCA=caFactorialDesign(data=experiment,type="orthogonal")
design_RBCA

#Coding the orthogonal fractional factorial design

code=caEncodedDesign(design_TCA)
code

