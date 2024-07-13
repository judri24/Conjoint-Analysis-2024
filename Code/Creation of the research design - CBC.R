library(cbcTools)

#Creation of the full factorial design

profiles <- cbcTools::cbc_profiles(
  Destination = c("Coastal region", "City", "Mountains"),
  Transportation = c("Plane", "Car", "Train/Bus"),
  Accommodation = c("Hotel", "Holiday home", "Camping"),
  Activities = c("Sightseeing", "Relaxation", "Sport activities")
)

#Developement of the evaluation design (always differs when rerunning the code)
design <- cbcTools::cbc_design(
  profiles = profiles,
  n_resp = 1, #the number of respondents can be changed to get a design with all respondents
  n_alts = 2, #number of alternatives per choice set
  n_q = 9, #number of choice set per respondent
  n_blocks = 1, #Indicates whether all respondents should get the same questions or not
  no_choice = FALSE, #Indicates whether a none-alternative should be included or not
  method = "orthogonal" #Indicates that an orthoghonal fractional factorial design should be used
)
design

#Coding the fractional factorial design
code_CBC=caEncodedDesign(design)
code_CBC

#Testing the design for balance and overlap

cbcTools::cbc_balance(design)
cbcTools::cbc_overlap(design)
