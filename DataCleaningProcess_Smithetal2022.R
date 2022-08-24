##################################################################################
#
# Code to process and clean the raw data from the angling survey
#
# This script takes in raw data responses and creates a csv file with
# cleaned data.
#
#
##################################################################################
# Read in the raw data
RawDataFilename <- "AnglingSurvey_Biosecurity.csv"
RawData <- read.csv(RawDataFilename)

library(data.table)
RawData <- setDT(RawData)

###################################################################################
#
# Sub function section
#
####################################################################################
ConvertRankToNumber <- function(InputValue){
        # This sub-function replaces 1 (smallest threat) and 5 (Largest threat) with 1 and 5
        if (InputValue == "1 (Smallest threat)" | InputValue == "1 (not at all important)" ){
                Output = 1
        } else if (InputValue == "5 (Largest threat)" | InputValue == "5 (extremely important)") {
                Output = 5
        } else {Output = InputValue}
        
        return(Output)
}

CountMissingNumbers <- function(V1){
        # This counts the number of missing values from the 5 rank list
        NumberMissingValues <- sum(is.na(V1))
        return(NumberMissingValues)
}

FindMissingRank <- function(V1){
        # This function takes in the 5 ranks for those with a single NA present
        # and returns the rank that the missing value should be
        if (sum(is.na(V1)) > 1){
                return(NA)
        }
        # find the missing rank
        MisVal <- which(!c(1,2,3,4,5) %in% V1)
        if (length(MisVal) > 1){ return(NA)} # more than 1 repeated value here
        return(MisVal)
}

CheckRepeatedRank <- function(V1){
        # This function takes in the 5 ranks 
        # and returns either 1 = all unique values or 0 = duplicated values (NAs aside)
        # Get rid of NAs
        V1 <- V1[!is.na(V1)]
        # Use rle to check for duplicates
        if (sum(rle(V1)$lengths > 1) != 0){
                # Duplicates found
                return(0)
        } else {return(1)}
}

ConvertAnglingFrequency <- function(InputValue){
        # This sub-function provides a category score rather than string
        if (InputValue == "More than once a week"){
                Output = 1 
        } else if (InputValue == "Once a week") {
                Output = 2
        } else if (InputValue == "Once every 2 weeks") {
                Output = 3
        } else if (InputValue == "Once every 3 weeks") {
                Output = 4
        } else if (InputValue == "Once every month") {
                Output = 5
        } else if (InputValue == "Once every 2 months") {
                Output = 6
        } else if (InputValue == "Once every 3 months") {
                Output = 7
        } else if (InputValue == "Less than once every 3 months") {
                Output = 8
        } else {Output = NA}
        
        return(Output)
}

NewAnglingFrequency <- function(InputValue){
        # This sub-function provides a category score rather than string
        if (InputValue == "More than once a week"){
                Output = 1 
        } else if (InputValue == "Once a week") {
                Output = 1
        } else if (InputValue == "Once every 2 weeks") {
                Output = 2
        } else if (InputValue == "Once every 3 weeks") {
                Output = 2
        } else if (InputValue == "Once every month") {
                Output = 2
        } else if (InputValue == "Once every 2 months") {
                Output = 3
        } else if (InputValue == "Once every 3 months") {
                Output = 3
        } else if (InputValue == "Less than once every 3 months") {
                Output = 4
        } else {Output = NA}
        
        return(Output)
}

NewCleanFrequency <- function(InputValue){
        # This sub-function provides a category score rather than string
        if (is.na(InputValue)){return(NA)}
        if (InputValue == "After every trip"){
                Output = 1 
        } else if (InputValue == "After 2-5 trips") {
                Output = 2
        } else if (InputValue == "After 6-10 trips") {
                Output = 3
        } else if (InputValue == "After 11+ trips") {
                Output = 4
        } else if (InputValue == "Don't clean") {
                Output = NA
        } else {Output = NA}
        
        return(Output)
}

ConvertAnglingFrequencyToRisk <- function(InputValue){
        # This sub-function provides an average days per year for the angling frequency based on the category
        RiskIncreasePerDay = (99/364)
        if (InputValue == "More than once a week"){
                # This averages at (7+2)/2 = 4.5 days per week or 4.5 * 52 = 234 days per year
                Output = 1 + ((234 - 1) * RiskIncreasePerDay)
        } else if (InputValue == "Once a week") {
                # This averages at 1 day per week = 52 days per year
                Output = 1 + ((52 - 1) * RiskIncreasePerDay)
        } else if (InputValue == "Once every 2 weeks") {
                # This averages at 0.5 day per week = 26 days per year
                Output = 1 + ((26 - 1) * RiskIncreasePerDay)
        } else if (InputValue == "Once every 3 weeks") {
                # This averages at 52/3 days per year
                Output = 1 + (((52/3) - 1) * RiskIncreasePerDay)
        } else if (InputValue == "Once every month") {
                # This averages at 12 days per year
                Output = 1 + ((12 - 1) * RiskIncreasePerDay)
        } else if (InputValue == "Once every 2 months") {
                # This averages at 6 days per year
                Output = 1 + ((52 - 1) * RiskIncreasePerDay)
        } else if (InputValue == "Once every 3 months") {
                # This averages at 4 days per year
                Output = 1 + ((52 - 1) * RiskIncreasePerDay)
        } else if (InputValue == "Less than once every 3 months") {
                # This averages at between 4 days per year and 1 day per year so an average of 2.5 days per year
                Output = 1 + ((2.5 - 1) * RiskIncreasePerDay)
        } else {Output = NA}
        
        return(Output)
}
CountEquipment <- function(V1){
        # Subfunction to count the number of pieces of equipment identified
        Count <- sum(V1 != "")
        return(Count)
}

ReturnAvailHighScore <- function(V1){
        # Subfunction to return the next available highest score
        # Get rid of NAs
        V1 <- V1[!is.na(V1)]
        HighScore <- max(which(!c(1,2,3,4,5) %in% V1))
        return(HighScore)
}

ReturnHighestScore <- function(V1){
        # Subfunction to return the highest score
        # Input as hose, cost, info, time, application
        Options <- c("Availability of hose", "Cost of Cleaning", "Time it takes", "Information","Appearance of kit")
        if (sum(is.na(V1)) == 5 | sum(!is.na(V1)) == 1){
                # All NAs or just 1 value set to NA
                return(NA)
        }
        HighScore <- Options[which.max(V1)]
        return(HighScore)
}

ReturnLowestScore <- function(V1){
        # Subfunction to return the highest score
        # Input as hose, cost, info, time, application
        Options <- c("Availability of hose", "Cost of Cleaning", "Time it takes", "Information","Appearance of kit")
        if (sum(is.na(V1)) == 5 | sum(!is.na(V1)) == 1){
                # All NAs or just 1 value set to NA
                return(NA)
        }
        LowScore <- Options[which.min(V1)]
        return(LowScore)
}
##############################################################################################
#
# Data cleaning
#
###############################################################################################

#############################################
# Sort out age group
#############################################
RawData[AgeGroup == "", AgeGroup := NA]
RawData[AgeGroup == "Under 18 years", AgeGroup := "17 years and under"] # Just makes ordering easier
##################################################
# Add a new age grouping
# designed to split the ages into similar numbers
# Here Over 65, 55-65, Below 55
###################################################
RawData[, NewAgeGrouping := NA]
RawData[, NewAgeGrouping := as.numeric(NewAgeGrouping)]
RawData[AgeGroup == "65 + years", NewAgeGrouping := 3]
RawData[AgeGroup == "55 - 64 years", NewAgeGrouping := 2]
RawData[AgeGroup == "17 years and under" |
        AgeGroup == "18 - 24 years" |
        AgeGroup == "25 - 34 years" |
        AgeGroup == "35 - 44 years" |
        AgeGroup == "45 - 54 years", 
        NewAgeGrouping := 1]



# For gender have male/female and NA
RawData[Sex != "Male" & Sex != "Female", Sex := NA]

# For the types of fishing have 0 or 1 responses
RawData[tolower(Game) == "game", Game:= 1]
RawData[tolower(CoarseCarp) == "coarse - carp", CoarseCarp:= 1]
RawData[tolower(CoarseLure) == "coarse - lure", CoarseLure:= 1]
RawData[tolower(CoarseOther) == "coarse - other", CoarseOther:= 1]
RawData[tolower(Competition) == "competition", Competition:= 1]
RawData[tolower(Sea) == "sea", Sea:= 1]

RawData[Game != 1, Game := 0]
RawData[CoarseCarp != 1, CoarseCarp := 0]
RawData[CoarseLure != 1, CoarseLure := 0]
RawData[CoarseOther != 1, CoarseOther := 0]
RawData[Competition != 1, Competition := 0]
RawData[Sea != 1, Sea := 0]

# Set to numerical columns
RawData[, Game := as.numeric(Game)]
RawData[, CoarseCarp := as.numeric(CoarseCarp)]
RawData[, CoarseLure := as.numeric(CoarseLure)]
RawData[, CoarseOther := as.numeric(CoarseOther)]
RawData[, Competition := as.numeric(Competition)]
RawData[, Sea := as.numeric(Sea)]

# Simplify coarse angling to a single group
RawData[, Coarse := 0]
RawData[CoarseCarp == 1 | CoarseLure == 1 | CoarseOther == 1, Coarse := 1]

##############################################################################
# Sort out the Type of angling - create a mixed angler and a specialist
##############################################################################
RawData[, NoAnglingSpec := rowSums(.SD), .SDcols = 5:10]

# Check the main specialism - is the main angling type selected? If not then set to ""
RawData[AngMostOften == "Game" & Game != 1, AngMostOften := ""]
RawData[AngMostOften == "Coarse - carp" & CoarseCarp != 1, AngMostOften := ""]
RawData[AngMostOften == "Coarse - lure" & CoarseLure != 1, AngMostOften := ""]
RawData[AngMostOften == "Coarse - other" & CoarseOther != 1, AngMostOften := ""]
RawData[AngMostOften == "Competition" & Competition != 1, AngMostOften := ""]
RawData[AngMostOften == "Sea" & Sea != 1, AngMostOften := ""]

# Add main specialism based on a single answer to the angling types
RawData[AngMostOften == "" & NoAnglingSpec == 1 & Game == 1, AngMostOften := "Game"]
RawData[AngMostOften == "" & NoAnglingSpec == 1 & CoarseCarp == 1, AngMostOften := "Coarse - carp"]
RawData[AngMostOften == "" & NoAnglingSpec == 1 & CoarseLure == 1, AngMostOften := "Coarse - lure"]
RawData[AngMostOften == "" & NoAnglingSpec == 1 & CoarseOther == 1, AngMostOften := "Coarse - other"]
RawData[AngMostOften == "" & NoAnglingSpec == 1 & Competition == 1, AngMostOften := "Competition"]
RawData[AngMostOften == "" & NoAnglingSpec == 1 & Sea == 1, AngMostOften := "Sea"]

# Add a specialist flag
RawData[, Specialist := 0]
RawData[NoAnglingSpec == 1, Specialist := 1]
RawData[NoAnglingSpec == 0, Specialist := NA]

############################################################################
# Sort out the frequency banding to weekly, monthly, quarterly and annually
############################################################################
RawData[, AnglingFreq := apply(.SD, 1, ConvertAnglingFrequency), .SDcols = 'FreqAng']
RawData[, NewAnglingFreq := apply(.SD, 1, NewAnglingFrequency), .SDcols = 'FreqAng']
RawData[, AnglingFreqRisk := apply(.SD, 1, ConvertAnglingFrequencyToRisk), .SDcols = 'FreqAng']

#############################################################################
# Work out the number of pieces of equipment
#############################################################################
RawData[, Equip_Number := apply(.SD, 1, CountEquipment), .SDcols = c('Equip_waders', 'Equip_FeltWaders', 'Equip_PikeTube', 'Equip_LandingNet', 'Equip_KeepNet', 'Equip_cradle', 'Equip_CarpSack', 'Equip_BassBags', 'Equip_BaitBoat', 'Equip_WeighSling', 'Equip_FlyBoats', 'Equip_RowBoat', 'Equip_StinkBag')]
RawData[Equip_Number == 0, Equip_Number := NA]
######################################################
# Create a column for having waders or not
######################################################
RawData[, WadersYN := 0]
RawData[Equip_waders == "Neoprene waders / wellies" |
                Equip_FeltWaders == "Felt waders / wellies" 
        , WadersYN := 1]

###########################################################
# Tidy up the cleaning equipment and waders
#   - treat these as a group
###########################################################
RawData[, CleanYN := 0]
RawData[CleanWadersYN == "Yes" |
        CleanEquipYN == "Yes",
        CleanYN := 1]
RawData[CleanWadersYN == "" & CleanEquipYN == "", CleanYN := NA]

###########################################################
# Tidy up the cleaning solutions - convert NA to 0
###########################################################
RawData[, CleanWaterResponse := NA]
RawData[!is.na(CleanProd_HotWater), CleanWaterResponse := 1]
RawData[!is.na(CleanProd_ColdWater), CleanWaterResponse := 1]
RawData[is.na(CleanProd_HotWater), CleanProd_HotWater := 0]
RawData[is.na(CleanProd_ColdWater), CleanProd_ColdWater := 0]
RawData[is.na(CleanProd_Virkon), CleanProd_Virkon := 0]
RawData[is.na(CleanProd_WashingFac), CleanProd_WashingFac := 0]
RawData[is.na(CleanProd_Soap), CleanProd_Soap := 0]

##########################################################################################
# Clean up the others mentioned
# Use this to add to the products used if not selected
# Sort out hot water, cold water, boiling water
##########################################################################################

# Cold water
RawData[grepl("cold", CleanProd_Other, ignore.case = TRUE) == TRUE & grepl("water", CleanProd_Other, ignore.case = TRUE) == TRUE & CleanProd_ColdWater != 1, CleanProd_ColdWater := 1]

# Hot water & warm water
RawData[grepl("hot", CleanProd_Other, ignore.case = TRUE) == TRUE & grepl("water", CleanProd_Other, ignore.case = TRUE) == TRUE & CleanProd_HotWater != 1, CleanProd_HotWater := 1]
RawData[grepl("warm", CleanProd_Other, ignore.case = TRUE) == TRUE & grepl("water", CleanProd_Other, ignore.case = TRUE) == TRUE & CleanProd_HotWater != 1, CleanProd_HotWater := 1]

# Boiling water
RawData[grepl("boiling", CleanProd_Other, ignore.case = TRUE) == TRUE & grepl("water", CleanProd_Other, ignore.case = TRUE) == TRUE & CleanProd_HotWater != 1, CleanProd_HotWater := 1]

# Steam cleaning
RawData[grepl("steam", CleanProd_Other, ignore.case = TRUE) == TRUE & CleanProd_HotWater != 1, CleanProd_HotWater := 1]

# Soapy water - add to soap also fairy
RawData[grepl("soap", CleanProd_Other, ignore.case = TRUE) == TRUE & CleanProd_Soap != 1, CleanProd_Soap := 1]
RawData[grepl("fairy", CleanProd_Other, ignore.case = TRUE) == TRUE & CleanProd_Soap != 1, CleanProd_Soap := 1]
RawData[grepl("washing up liquid", CleanProd_Other, ignore.case = TRUE) == TRUE & CleanProd_Soap != 1, CleanProd_Soap := 1]
RawData[grepl("detergent", CleanProd_Other, ignore.case = TRUE) == TRUE |
          grepl("deturgent", CleanProd_Other, ignore.case = TRUE) == TRUE & CleanProd_Soap != 1, CleanProd_Soap := 1]

# Make a disinfectant column to include virkon plus others mentioned (disinfectant, dettol, bleach, sanitiser)
RawData[  grepl("disinfectant", CleanProd_Other, ignore.case = TRUE) == TRUE |
          grepl("disinfectent", CleanProd_Other, ignore.case = TRUE) == TRUE |
          grepl("DISENFECTANT", CleanProd_Other, ignore.case = TRUE) == TRUE |
          grepl("disifectant", CleanProd_Other, ignore.case = TRUE) == TRUE |
          grepl("Disifencant", CleanProd_Other, ignore.case = TRUE) == TRUE |
          grepl("Diservecton", CleanProd_Other, ignore.case = TRUE) == TRUE |
          grepl("discinfectant", CleanProd_Other, ignore.case = TRUE) == TRUE |
          grepl("discinfectent", CleanProd_Other, ignore.case = TRUE) == TRUE |
          grepl("dissinfectant", CleanProd_Other, ignore.case = TRUE) == TRUE |
          grepl("disinfect", CleanProd_Other, ignore.case = TRUE) == TRUE |
          grepl("Disinfecttent", CleanProd_Other, ignore.case = TRUE) == TRUE,
        CleanProd_Disinfectant := 1]

RawData[grepl("dettol", CleanProd_Other, ignore.case = TRUE) == TRUE | grepl("detol", CleanProd_Other, ignore.case = TRUE) == TRUE, CleanProd_Disinfectant := 1]
RawData[grepl("jeyes", CleanProd_Other, ignore.case = TRUE) == TRUE | 
        grepl("jeys", CleanProd_Other, ignore.case = TRUE) == TRUE |
          grepl("jays", CleanProd_Other, ignore.case = TRUE) == TRUE |
          grepl("jayes", CleanProd_Other, ignore.case = TRUE) == TRUE |
          grepl("keyes", CleanProd_Other, ignore.case = TRUE) == TRUE, 
        CleanProd_Disinfectant := 1]
RawData[grepl("milton", CleanProd_Other, ignore.case = TRUE) == TRUE, CleanProd_Disinfectant := 1]
RawData[grepl("hibiscrub", CleanProd_Other, ignore.case = TRUE) == TRUE, CleanProd_Disinfectant := 1]
RawData[grepl("Prolific Steri-7 Xtra", CleanProd_Other, ignore.case = TRUE) == TRUE, CleanProd_Disinfectant := 1]
RawData[  grepl("bleach", CleanProd_Other, ignore.case = TRUE) == TRUE |
          grepl("clorex", CleanProd_Other, ignore.case = TRUE) == TRUE |
          grepl("hypochlorate", CleanProd_Other, ignore.case = TRUE) == TRUE |
          grepl("sodium metasulphate", CleanProd_Other, ignore.case = TRUE) == TRUE |
          grepl("detox", CleanProd_Other, ignore.case = TRUE) == TRUE, CleanProd_Disinfectant := 1]
RawData[grepl("sanitiser", CleanProd_Other, ignore.case = TRUE) == TRUE, CleanProd_Disinfectant := 1]
RawData[grepl("sterelising fluid", CleanProd_Other, ignore.case = TRUE) == TRUE, CleanProd_Disinfectant := 1]
RawData[grepl("dip", CleanProd_Other, ignore.case = TRUE) == TRUE, CleanProd_Disinfectant := 1]
RawData[grepl("anti bac", CleanProd_Other, ignore.case = TRUE) == TRUE |
          grepl("antibac", CleanProd_Other, ignore.case = TRUE) == TRUE, CleanProd_Disinfectant := 1]
RawData[CleanProd_Virkon == 1, CleanProd_Disinfectant := 1]

# Make a group for oils/waxes/polishes
RawData[  grepl("oil", CleanProd_Other, ignore.case = TRUE) == TRUE | 
            grepl("polish", CleanProd_Other, ignore.case = TRUE) == TRUE |
            grepl("WD40", CleanProd_Other, ignore.case = TRUE) == TRUE |
            grepl("WD 40", CleanProd_Other, ignore.case = TRUE) == TRUE |
            grepl("ED 40", CleanProd_Other, ignore.case = TRUE) == TRUE |
            grepl("lubricant", CleanProd_Other, ignore.case = TRUE) == TRUE |
            grepl("wax", CleanProd_Other, ignore.case = TRUE) == TRUE |
            grepl("pledge", CleanProd_Other, ignore.case = TRUE) == TRUE, 
          CleanProd_Oils := 1]

# Create a hot water YN column (also a check for hot and cold water both selected)
# Here 1 = hot water used; 2 no hot water used
RawData[, HotWaterYN := 0]
RawData[CleanProd_HotWater == 1, HotWaterYN := 1]

# Create a cold water YN column (if no hot water selected either then set to 0)
# Here 1 = cold water used; 0 no water used (not indicated)
RawData[, ColdWaterYN := 0]
RawData[CleanProd_ColdWater == 1 & CleanProd_HotWater != 1 , ColdWaterYN := 1]

# Create a cleaning product YN column
RawData[, CleaningProdYN := 0]
RawData[CleanProd_Disinfectant == 1 |
          CleanProd_Soap == 1 |
          CleanProd_Oils == 1, 
        CleaningProdYN := 1]

############################################################
# Sort out the cleaning frequency
# If cleaning is a no then set clean freq to "Don't clean"
#############################################################
RawData[CleanFreq == "", CleanFreq := NA]
RawData[CleanYN == 0, CleanFreq := "Don't clean"]
RawData[is.na(CleanYN), CleanFreq := NA]
RawData[, NewCleanFreq := apply(.SD, 1, NewCleanFrequency), .SDcols = 'CleanFreq']

# Tidy the cleaning reasons which should be ranked
# There should be 5 numbers for each response
# Numbers should be 1 to 5
# If this is not the case then check a single number is missing
# If more than 1 number missing - Do not use the ranking
#
# The "other" column is open ended response
#############################################################
# Sort out the ranks to make them numerical
RawData[, CleanFactor_App := apply(.SD, 1, ConvertRankToNumber), .SDcols = 'CleanFactor_App']
RawData[, CleanFactor_Hose := apply(.SD, 1, ConvertRankToNumber), .SDcols = 'CleanFactor_Hose']
RawData[, CleanFactor_Cost := apply(.SD, 1, ConvertRankToNumber), .SDcols = 'CleanFactor_Cost']
RawData[, CleanFactor_Info := apply(.SD, 1, ConvertRankToNumber), .SDcols = 'CleanFactor_Info']
RawData[, CleanFactor_time := apply(.SD, 1, ConvertRankToNumber), .SDcols = 'CleanFactor_time']

RawData[, CleanFactor_App := as.numeric(CleanFactor_App)]
RawData[, CleanFactor_Hose := as.numeric(CleanFactor_Hose)]
RawData[, CleanFactor_Cost := as.numeric(CleanFactor_Cost)]
RawData[, CleanFactor_Info := as.numeric(CleanFactor_Info)]
RawData[, CleanFactor_time := as.numeric(CleanFactor_time)]

# Add a counter for the number of missing values present
RawData[, CleanFactor_MissVals := apply(.SD, 1, CountMissingNumbers), .SDcols = c('CleanFactor_App','CleanFactor_Hose','CleanFactor_Cost','CleanFactor_Info','CleanFactor_time')]

# For those with 1 missing value run a check and fill in with the missing number if applicable
RawData[CleanFactor_MissVals == 1, CleanFactor_MissRank := apply(.SD, 1, FindMissingRank), .SDcols = c('CleanFactor_App','CleanFactor_Hose','CleanFactor_Cost','CleanFactor_Info','CleanFactor_time')]
RawData[CleanFactor_MissVals == 1 & !is.na(CleanFactor_MissRank) & is.na(CleanFactor_App), CleanFactor_App := CleanFactor_MissRank]
RawData[CleanFactor_MissVals == 1 & !is.na(CleanFactor_MissRank) & is.na(CleanFactor_Hose), CleanFactor_Hose := CleanFactor_MissRank]
RawData[CleanFactor_MissVals == 1 & !is.na(CleanFactor_MissRank) & is.na(CleanFactor_Cost), CleanFactor_Cost := CleanFactor_MissRank]
RawData[CleanFactor_MissVals == 1 & !is.na(CleanFactor_MissRank) & is.na(CleanFactor_Info), CleanFactor_Info := CleanFactor_MissRank]
RawData[CleanFactor_MissVals == 1 & !is.na(CleanFactor_MissRank) & is.na(CleanFactor_time), CleanFactor_time := CleanFactor_MissRank]

# Reset the missing values after update
RawData[, CleanFactor_MissVals := apply(.SD, 1, CountMissingNumbers), .SDcols = c('CleanFactor_App','CleanFactor_Hose','CleanFactor_Cost','CleanFactor_Info','CleanFactor_time')]

# This is a flag for whether the ranking of threats is complete
RawData[, CleanFactor_RankComp := 0] 
RawData[CleanFactor_MissVals == 0, CleanFactor_RankComp := 1]

# This is a flag for whether the ranking contains all unique values
RawData[, CleanFactor_RankUnique := apply(.SD, 1, CheckRepeatedRank), .SDcols = c('CleanFactor_App','CleanFactor_Hose','CleanFactor_Cost','CleanFactor_Info','CleanFactor_time')]

# Make a column with the highest ranked factor and lowest ranked factor
RawData[, CleanFactor_Highest := apply(.SD, 1, ReturnHighestScore), .SDcols = c('CleanFactor_Hose','CleanFactor_Cost','CleanFactor_time','CleanFactor_Info','CleanFactor_App')]
RawData[, CleanFactor_Lowest := apply(.SD, 1, ReturnLowestScore), .SDcols = c('CleanFactor_Hose','CleanFactor_Cost','CleanFactor_time','CleanFactor_Info','CleanFactor_App')]

###############################################################
# Sort the open responses to clean factor reason other
# Good responses will be kept in new Clean_Responses column
###############################################################
# Get response with home in
RawData[grepl("home", CleanFactor_Other, ignore.case = TRUE), CleanHome := 1]

# Get the "good" responses
RawData[trimws(CleanFactor_Other) =="Having to track back to the lodge after a long hard day on the water.", CleanFactor_Responses := CleanFactor_Other]
RawData[trimws(CleanFactor_Other) =="I clean equipment at home so information, cost and availability of cleaning station", CleanFactor_Responses := CleanFactor_Other]
RawData[trimws(CleanFactor_Other) =="If i've caught a fish that looks unhealthy or a venue that i suspect has an issue. Especially if foreign species could be there like signal crayfish", CleanFactor_Responses := CleanFactor_Other]
RawData[trimws(CleanFactor_Other) =="My various waders are usually only used at one venue and the months between usage let's them dry long enough to render any problems dealt with.  I will bank fish Grafham over the winter using neoprene waders which will be washed down on site and will not use them again until they go to the spey in. Mid February", CleanFactor_Responses := CleanFactor_Other]
RawData[trimws(CleanFactor_Other) =="Sloppy use of available facilities by others on site (includes staff) renders effectiveness highly questionable!", CleanFactor_Responses := CleanFactor_Other]
RawData[trimws(CleanFactor_Other) =="Time (unable to check )   Cleaning stations unlikely to appear on riverside , dip stations are a good idea but guess cost would outweigh action", CleanFactor_Responses := CleanFactor_Other]
RawData[trimws(CleanFactor_Other) =="No excuse for poor maintenance and contamination issues. Being a bailiff of my local syndicate this is very high on our priority to protect our stock.", CleanFactor_Responses := CleanFactor_Other]
RawData[trimws(CleanFactor_Other) =="I know it's difficult to write questions that cover every situation and this survey is aimed at non native species. But the primary reason I clean my equipment is to remove salt (if sea fishing) and dirt. I also tend to fish when I do fish inland on just one river so chance of spreading alien species is reduced. I also ensure equipment is dried before stowage. . I probably visit other venues once or twice a year mostly fly. I do occasionaly go carp fishing at local venue and use the on site dipping facilities if I do.", CleanFactor_Responses := CleanFactor_Other]

# Create an indicator for a "good" response
RawData[trimws(CleanFactor_Other) == "", CleanFactor_Other := NA]
RawData[, CleanFactor_Output := 0]
RawData[!is.na(CleanFactor_Other), CleanFactor_Output := 1] # This is any response

# Remove the non-responses such as "", "None", "can't think of anything" etc.
RawData[grepl("none", CleanFactor_Other, ignore.case = TRUE) == TRUE, CleanFactor_Other := NA]
RawData[grepl("can't think of anything", CleanFactor_Other, ignore.case = TRUE) == TRUE, CleanFactor_Other := NA]

RawData[grepl("no", CleanFactor_Other, ignore.case = TRUE) == TRUE & grepl("clean", CleanFactor_Other, ignore.case = TRUE) == TRUE & grepl("home", CleanFactor_Other, ignore.case = TRUE) == TRUE & grepl("not", CleanFactor_Other, ignore.case = TRUE) == FALSE & grepl("dip tubs", CleanFactor_Other, ignore.case = TRUE) == FALSE & grepl("source of water", CleanFactor_Other, ignore.case = TRUE) == FALSE, CleanFactor_Other := NA]
RawData[grepl("No it only takes minutes to wipe down and dip your nets.", CleanFactor_Other, ignore.case = TRUE) == TRUE, CleanFactor_Other := NA]
RawData[trimws(CleanFactor_Other) == "no..", CleanFactor_Other := NA]
RawData[toupper(trimws(CleanFactor_Other)) == "NO", CleanFactor_Other := NA]
RawData[toupper(trimws(CleanFactor_Other)) == "BO", CleanFactor_Other := NA]
RawData[toupper(trimws(CleanFactor_Other)) == "NO.", CleanFactor_Other := NA]
RawData[toupper(trimws(CleanFactor_Other)) == "NOPE", CleanFactor_Other := NA]
RawData[trimws(CleanFactor_Other) == "no", CleanFactor_Other := NA]
RawData[trimws(CleanFactor_Other) == "NO", CleanFactor_Other := NA]
RawData[toupper(trimws(CleanFactor_Other)) == "N/A", CleanFactor_Other := NA]
RawData[toupper(trimws(CleanFactor_Other)) == "YES", CleanFactor_Other := NA]
RawData[trimws(CleanFactor_Other) == "cormarant big problem", CleanFactor_Other := NA]
RawData[trimws(CleanFactor_Other) == "THERE IS NO REASON  TO NOT CLEAN ALL EQUIPMENT", CleanFactor_Other := NA]
RawData[trimws(CleanFactor_Other) == "I always clean my equipment", CleanFactor_Other := NA]
RawData[trimws(CleanFactor_Other) == "no - I just clean it in the back garden", CleanFactor_Other := NA]
RawData[trimws(CleanFactor_Other) == "wash at home.", CleanFactor_Other := NA]
RawData[trimws(CleanFactor_Other) == "One", CleanFactor_Other := NA]
RawData[trimws(CleanFactor_Other) == "N0", CleanFactor_Other := NA]
RawData[trimws(CleanFactor_Other) == "Na", CleanFactor_Other := NA]
RawData[trimws(CleanFactor_Other) == "N one", CleanFactor_Other := NA]
RawData[trimws(CleanFactor_Other) == "", CleanFactor_Other := NA]
RawData[trimws(CleanFactor_Other) == "/", CleanFactor_Other := NA]
RawData[trimws(CleanFactor_Other) == "Not really", CleanFactor_Other := NA]
RawData[trimws(CleanFactor_Other) == "No, it's my stuff", CleanFactor_Other := NA]
RawData[trimws(CleanFactor_Other) == "Nothing i can think of", CleanFactor_Other := NA]

# Change the indicator to record responses deemed no additional reasons
RawData[CleanFactor_Output == 1 & is.na(CleanFactor_Other), CleanFactor_Output := 2]

# Remove the responses that indicate good practice but no other reason
RawData[trimws(CleanFactor_Other) == "Always clean everything, lasts much longer", CleanFactor_Other := NA]
RawData[trimws(CleanFactor_Other) == "Nothing prevents me. The options above are nonsense.", CleanFactor_Other := NA]
RawData[trimws(CleanFactor_Other) == "If I don't clean at backside, I clean at home.", CleanFactor_Other := NA]
RawData[trimws(CleanFactor_Other) == "I clean up when I get home or before I go fishing if I have forgotten or did not have the time.", CleanFactor_Other := NA]
RawData[trimws(CleanFactor_Other) == "No excuses cleaning gear is as important as the day fishing   Helps minamise diseases", CleanFactor_Other := NA]
RawData[trimws(CleanFactor_Other) == "non, i clean all my equipment after all trips fishing, irrespective of catch and consider it part of the routine", CleanFactor_Other := NA]

RawData[trimws(CleanFactor_Other) == "No. I always clean my equipment as I keep it inside my house.", CleanFactor_Other := NA]
RawData[trimws(CleanFactor_Other) == "I always clean my kit to stop any cross contamination", CleanFactor_Other := NA]
RawData[trimws(CleanFactor_Other) == "No, every fisherman should clean his gear to help prevent the spread of disease if nothing else", CleanFactor_Other := NA]
RawData[trimws(CleanFactor_Other) == "NO I just clean it when home if there is nothing available at site", CleanFactor_Other := NA]
RawData[trimws(CleanFactor_Other) == "I clean the equipment at home and make sure it is dry. Sometimes it has to wait a day or so if I am busy", CleanFactor_Other := NA]

RawData[grepl("no", CleanFactor_Other, ignore.case = TRUE) == TRUE &
                grepl("always", CleanFactor_Other, ignore.case = TRUE) == TRUE &
                grepl("clean", CleanFactor_Other, ignore.case = TRUE) == TRUE, CleanFactor_Other := NA]

RawData[trimws(CleanFactor_Other) == "i do not fish commercials,is this what you mean", CleanFactor_Other := NA]
RawData[trimws(CleanFactor_Other) == "let it dry", CleanFactor_Other := NA]
RawData[trimws(CleanFactor_Other) == "I do it either on our boat or at home.", CleanFactor_Other := NA]
RawData[trimws(CleanFactor_Other) == "Mine are cleaned at home.", CleanFactor_Other := NA]
RawData[trimws(CleanFactor_Other) == "Nothing prevents me cleaning all items including reels.", CleanFactor_Other := NA]
RawData[trimws(CleanFactor_Other) == "no, cleaning your tackle is part of fishing and just as enjoyable.", CleanFactor_Other := NA]
RawData[trimws(CleanFactor_Other) == "No, it's my stuff, it gets cleaned.", CleanFactor_Other := NA]
RawData[trimws(CleanFactor_Other) == "Children", CleanFactor_Other := NA]
RawData[trimws(CleanFactor_Other) == "the wife ?", CleanFactor_Other := NA]
RawData[trimws(CleanFactor_Other) == "Clean as required-properly-in the bath", CleanFactor_Other := NA]
RawData[trimws(CleanFactor_Other) == "Always clean it after a trip", CleanFactor_Other := NA]
RawData[trimws(CleanFactor_Other) == "na", CleanFactor_Other := NA]
RawData[trimws(CleanFactor_Other) == "No I do it all when I get home. ", CleanFactor_Other := NA]
RawData[trimws(CleanFactor_Other) == "Do cleaning when I get home", CleanFactor_Other := NA]
RawData[trimws(CleanFactor_Other) == "i have no issues cleaning equipment.people take the mick about how clean it is", CleanFactor_Other := NA]
RawData[trimws(CleanFactor_Other) == "No   -   Always dry nets and mats", CleanFactor_Other := NA]
RawData[trimws(CleanFactor_Other) == "Thoroughly drying nets is more important than cleaning ", CleanFactor_Other := NA]
RawData[trimws(CleanFactor_Other) == "I do it at home", CleanFactor_Other := NA]
RawData[trimws(CleanFactor_Other) == "Never used soap", CleanFactor_Other := NA]
RawData[trimws(CleanFactor_Other) == "Drying", CleanFactor_Other := NA]
RawData[trimws(CleanFactor_Other) == "nlo", CleanFactor_Other := NA]
RawData[trimws(CleanFactor_Other) == "Wife", CleanFactor_Other := NA]
RawData[trimws(CleanFactor_Other) == ".", CleanFactor_Other := NA]
RawData[trimws(CleanFactor_Other) == "Moaning misses", CleanFactor_Other := NA]
RawData[trimws(CleanFactor_Other) == "No I do it all when I get home.", CleanFactor_Other := NA]
RawData[trimws(CleanFactor_Other) == "Never", CleanFactor_Other := NA]
RawData[trimws(CleanFactor_Other) == "No, but the options presented under the previous question are simply not realistic questions.", CleanFactor_Other := NA]
RawData[trimws(CleanFactor_Other) == "I always clean my fishing equipment after every use", CleanFactor_Other := NA]
RawData[trimws(CleanFactor_Other) == 'The "mrs" factor as my stuff gets cleaned in the bath when she aint at home', CleanFactor_Other := NA]
RawData[trimws(CleanFactor_Other) == "No, I always hose off and brush my equipment, the hang out to dry", CleanFactor_Other := NA]
RawData[trimws(CleanFactor_Other) == "When Polish and Romanians are trying the steal it along my catch and the bailiffs do NOTHING about it.", CleanFactor_Other := NA]
RawData[trimws(CleanFactor_Other) == "I wash the keepnets  and landing net  and poles and rod then hose with cold water", CleanFactor_Other := NA]
RawData[trimws(CleanFactor_Other) == "Use disinfectant and let the equipment dry totally in the sunshine", CleanFactor_Other := NA]
RawData[trimws(CleanFactor_Other) == "No usually put it in shed and clean night before next trip", CleanFactor_Other := NA]
RawData[trimws(CleanFactor_Other) == "I always clean my fishing equipment after every use", CleanFactor_Other := NA]
RawData[trimws(CleanFactor_Other) == "Drying equipment after cleaning can be difficult", CleanFactor_Other := NA]
RawData[trimws(CleanFactor_Other) == "No , there are no issues AT ALL that would stop me cleaning my kit !", CleanFactor_Other := NA]
RawData[trimws(CleanFactor_Other) == "If I know that invasive species are present where I‚Äôm fishing I‚Äôll clean my gear and dry it before I use it again", CleanFactor_Other := NA]
RawData[trimws(CleanFactor_Other) == "All fishermen and women should / must clean their kit after a day out, it should be best practice. No excuses.?üëç", CleanFactor_Other := NA]
RawData[trimws(CleanFactor_Other) == "Ridiculous previous question... most important is to clean everything then completely dry it before fishing again...", CleanFactor_Other := NA]
RawData[trimws(CleanFactor_Other) == "no, other than I tend to use different equipment at different venues i.e. landing nets and unhooking mats", CleanFactor_Other := NA]
RawData[trimws(CleanFactor_Other) == "I have spares so I can rotate the equipment", CleanFactor_Other := NA]
RawData[trimws(CleanFactor_Other) == "Water meter,", CleanFactor_Other := NA]
RawData[trimws(CleanFactor_Other) == "No, always make sure my fly fishing kit is in mint condition", CleanFactor_Other := NA]
RawData[trimws(CleanFactor_Other) == "above question is stupid. Vital to clean kit irrespective of time or cost what it looks like. If u do not have info u r a moron!!", CleanFactor_Other := NA]
RawData[trimws(CleanFactor_Other) == "I try to clean my gear when ever I can especially after sea fishing or if  I have had to lay my tackle on the ground.", CleanFactor_Other := NA]
RawData[trimws(CleanFactor_Other) == "No I do it when I get home from a fishing trip whatever sea or course fishing", CleanFactor_Other := NA]
RawData[trimws(CleanFactor_Other) == "To be true-full other than the last part of the above the rest are 1 but as you will not let me select this option.", CleanFactor_Other := NA]
RawData[trimws(CleanFactor_Other) == "No excuse for poor maintenance and contamination issues. Being a bailiff of my local syndicate this is very high on our priority to protect our stock.", CleanFactor_Other := NA]

RawData[grepl("clean", CleanFactor_Other, ignore.case = TRUE) == TRUE &
                grepl("properly", CleanFactor_Other, ignore.case = TRUE) == TRUE &
                grepl("home", CleanFactor_Other, ignore.case = TRUE) == TRUE, CleanFactor_Other := NA]

RawData[grepl("always", CleanFactor_Other, ignore.case = TRUE) == TRUE &
                grepl("clean", CleanFactor_Other, ignore.case = TRUE) == TRUE &
                grepl("home", CleanFactor_Other, ignore.case = TRUE) == TRUE, CleanFactor_Other := NA]

# Change the indicator to record responses deemed good but actually no reason given
RawData[CleanFactor_Output == 1 & is.na(CleanFactor_Other), CleanFactor_Output := 3]

# Strip out appearance/condition of kit
RawData[trimws(CleanFactor_Other) == "Keep it in Good condition, it's expensive", CleanFactor_Other := "Kit Condition"]
RawData[trimws(CleanFactor_Other) == 'To remove salt to prevent corrosion', CleanFactor_Other := "Kit Condition"]
RawData[trimws(CleanFactor_Other) == 'Depends what equipment and how dirty it is', CleanFactor_Other := "Kit Condition"]
RawData[trimws(CleanFactor_Other) == 'how dirty it appears to be', CleanFactor_Other := "Kit Condition"]
RawData[trimws(CleanFactor_Other) == 'Drying facility', CleanFactor_Other := "Kit Condition"]
RawData[trimws(CleanFactor_Other) == 'Fear of damage', CleanFactor_Other := "Kit Condition"]
RawData[trimws(CleanFactor_Other) == 'I will clean my boots when muddy. Usually clean my fishing tackle if it looks dirty.', CleanFactor_Other := "Kit Condition"]
RawData[trimws(CleanFactor_Other) == "I know it's difficult to write questions that cover every situation and this survey is aimed at non native species. But the primary reason I clean my equipment is to remove salt (if sea fishing) and dirt. I also tend to fish when I do fish inland on just one river so chance of spreading alien species is reduced. I also ensure equipment is dried before stowage. . I probably visit other venues once or twice a year mostly fly. I do occasionaly go carp fishing at local venue and use the on site dipping facilities if I do.", CleanFactor_Other := "Kit Condition"]

# Strip out time between trips
RawData[trimws(CleanFactor_Other) == 'How soon my next fishing trip will be   The time between trips', CleanFactor_Other := "Time between trips"]
RawData[trimws(CleanFactor_Other) == 'If decent period has passed to allow kit to dry since last use', CleanFactor_Other := "Time between trips"]

# Strip out general reference to facilities as this is covered - including drying
RawData[trimws(CleanFactor_Other) == "No facilities at my fishery", CleanFactor_Other := "Facilities"]
RawData[trimws(CleanFactor_Other) == '9 times out of 10  there is no washing facilities available', CleanFactor_Other := "Facilities"]
RawData[trimws(CleanFactor_Other) == 'No wellie wash', CleanFactor_Other := "Facilities"]
RawData[trimws(CleanFactor_Other) == 'Cleaning station too near venue.', CleanFactor_Other := "Facilities"]
RawData[trimws(CleanFactor_Other) == 'Location of cleaning equipment', CleanFactor_Other := "Facilities"]
RawData[trimws(CleanFactor_Other) == 'No facilities', CleanFactor_Other := "Facilities"]
RawData[trimws(CleanFactor_Other) == 'Poor facilities at venues', CleanFactor_Other := "Facilities"]
RawData[trimws(CleanFactor_Other) == 'Location of cleaning equipment', CleanFactor_Other := "Facilities"]
RawData[trimws(CleanFactor_Other) == 'If there is no facility  I do it at home', CleanFactor_Other := "Facilities"]
RawData[trimws(CleanFactor_Other) == 'No facilities', CleanFactor_Other := "Facilities"]
RawData[trimws(CleanFactor_Other) == 'most british lakes lag behind french ones for facilities', CleanFactor_Other := "Facilities"]
RawData[trimws(CleanFactor_Other) == 'Availability of hot water', CleanFactor_Other := "Facilities"]
RawData[trimws(CleanFactor_Other) == 'If I am staying at a hotel or B & B, and the fishery has no cleaning equipment available, then I cannot clean my tackle.', CleanFactor_Other := "Facilities"]
RawData[trimws(CleanFactor_Other) == 'Drying Facilities on - Site', CleanFactor_Other := "Facilities"]
RawData[trimws(CleanFactor_Other) == 'Facilities to clean waders so I clean up at home', CleanFactor_Other := "Facilities"]
RawData[trimws(CleanFactor_Other) == 'drying room especially for nets rags ( hand cleaners)etc', CleanFactor_Other := "Facilities"]
RawData[trimws(CleanFactor_Other) == 'if at fisheries they do not supply cleaning facilities so I clean when I get home same when I go sea fishing', CleanFactor_Other := "Facilities"]

# Strip out lack of dip tanks
# Keeping this separate to lack of facilities which can be put into availability
RawData[grepl("No dips", CleanFactor_Other, ignore.case = TRUE) == TRUE, CleanFactor_Other := "No Dip Tank"]
RawData[grepl("All venues should use landing net dipping barrels onsite", CleanFactor_Other, ignore.case = TRUE) == TRUE, CleanFactor_Other := "No Dip Tank"]
RawData[grepl("I think all fisheries should have a bin for dipping our nets", CleanFactor_Other, ignore.case = TRUE) == TRUE, CleanFactor_Other := "No Dip Tank"]
RawData[grepl("Venues without dip tanks for the landing/keep nets", CleanFactor_Other, ignore.case = TRUE) == TRUE, CleanFactor_Other := "No Dip Tank"]
RawData[grepl("Lack of dip tanks between some venues", CleanFactor_Other, ignore.case = TRUE) == TRUE, CleanFactor_Other := "No Dip Tank"]
RawData[grepl("not all venues have dipping tanks. ", CleanFactor_Other, ignore.case = TRUE) == TRUE, CleanFactor_Other := "No Dip Tank"]
RawData[grepl("Net dips at venue", CleanFactor_Other, ignore.case = TRUE) == TRUE, CleanFactor_Other := "No Dip Tank"]
RawData[grepl("Net dips at venue", CleanFactor_Other, ignore.case = TRUE) == TRUE, CleanFactor_Other := "No Dip Tank"]
RawData[grepl("No network dips at fisheries", CleanFactor_Other, ignore.case = TRUE) == TRUE, CleanFactor_Other := "No Dip Tank"]
RawData[grepl("net dip box", CleanFactor_Other, ignore.case = TRUE) == TRUE, CleanFactor_Other := "No Dip Tank"]
RawData[grepl("lack of net dips", CleanFactor_Other, ignore.case = TRUE) == TRUE, CleanFactor_Other := "No Dip Tank"]
RawData[grepl("How about dipping nets at commercial fishing venues, prior to fishing?", CleanFactor_Other, ignore.case = TRUE) == TRUE, CleanFactor_Other := "No Dip Tank"]
RawData[grepl("on site availability of on site disinfectant and rinse wash tanks", CleanFactor_Other, ignore.case = TRUE) == TRUE, CleanFactor_Other := "No Dip Tank"]
RawData[grepl("A hose is not enough for cleaning the bottom of rowing boats when leaving a location, you also need access to a stiff brush.  Most anglers do their net cleaning at home due to the lack of disinfectant dips at anything other than commercial fisheries", CleanFactor_Other, ignore.case = TRUE) == TRUE, CleanFactor_Other := "No Dip Tank"]
RawData[grepl("Usually facilities in the UK are used to clean when dip is provided on site. Normally I thoroughly dry everything between trips.", CleanFactor_Other, ignore.case = TRUE) == TRUE, CleanFactor_Other := "No Dip Tank"]

# strip out Dip tank cleanliness
RawData[trimws(CleanFactor_Other) == "Chemical baths of Net Dip at commercials must transfer some chemicals to water even after rinsing . . not good", CleanFactor_Other := "Unclean Dip Tanks"]
RawData[trimws(CleanFactor_Other) == "Cleaning stations such as net dip tubs must be kept fresh or there is no point using them. I have experienced tubs that smell worse than my nets so refuse to use them preferring to do it at home.", CleanFactor_Other := "Unclean Dip Tanks"]
RawData[trimws(CleanFactor_Other) == 'The "freshness" of dip tanks at fisheries - when was the virkon/Fam30 last replaced.', CleanFactor_Other := "Unclean Dip Tanks"]
RawData[trimws(CleanFactor_Other) == "Sloppy use of available facilities by others on site (includes staff) renders effectiveness highly questionable!", CleanFactor_Other := "Unclean Dip Tanks"]

# Strip out disability/access as a factor
RawData[grepl("disabl", CleanFactor_Other, ignore.case = TRUE) == TRUE, CleanFactor_Other := "Disability access"]
RawData[grepl("health issues", CleanFactor_Other, ignore.case = TRUE) == TRUE, CleanFactor_Other := "Disability access"]
RawData[grepl("illness", CleanFactor_Other, ignore.case = TRUE) == TRUE, CleanFactor_Other := "Disability access"]
RawData[grepl("chronic pain", CleanFactor_Other, ignore.case = TRUE) == TRUE, CleanFactor_Other := "Disability access"]
RawData[grepl("arthritic", CleanFactor_Other, ignore.case = TRUE) == TRUE, CleanFactor_Other := "Disability access"]

# Strip out time
RawData[toupper(trimws(CleanFactor_Other)) == "TIME", CleanFactor_Other := "Time"]
RawData[trimws(CleanFactor_Other) == 'No, mainly time', CleanFactor_Other := "Time"]
RawData[trimws(CleanFactor_Other) == 'Allowing enough time before darkness falls.', CleanFactor_Other := "Time"]
RawData[trimws(CleanFactor_Other) == 'Drying time', CleanFactor_Other := "Time"]
RawData[trimws(CleanFactor_Other) == 'Time available', CleanFactor_Other := "Time"]
RawData[trimws(CleanFactor_Other) == "Time it takes to clean. Items don't appear dirty.", CleanFactor_Other := "Time"]
RawData[trimws(CleanFactor_Other) == 'Time available', CleanFactor_Other := "Time"]
RawData[trimws(CleanFactor_Other) == "generally it's time", CleanFactor_Other := "Time"]
RawData[trimws(CleanFactor_Other) == 'Lack of time but I‚Äôll always make time when appropriate', CleanFactor_Other := "Time"]
RawData[trimws(CleanFactor_Other) == 'Dark when you get back', CleanFactor_Other := "Time"]
RawData[trimws(CleanFactor_Other) == 'Only time lifestyle limits that forces me to clean the next day', CleanFactor_Other := "Time"]
RawData[trimws(CleanFactor_Other) == 'Q 29 needs a double answer of not important for time taken and cost.', CleanFactor_Other := "Time"]
RawData[trimws(CleanFactor_Other) == 'Just if I‚Äôm heading out shortly', CleanFactor_Other := "Time"]
RawData[trimws(CleanFactor_Other) == 'Only time, all others irrelevant, am upto date with information.', CleanFactor_Other := "Time"]
RawData[trimws(CleanFactor_Other) == 'Time (unable to check )   Cleaning stations unlikely to appear on riverside , dip stations are a good idea but guess cost would outweigh action', CleanFactor_Other := "Time"]

# Strip out laziness
RawData[grepl("bothered", CleanFactor_Other, ignore.case = TRUE) == TRUE & grepl("can't", CleanFactor_Other, ignore.case = TRUE) == TRUE, CleanFactor_Other := "Motivation"]
RawData[grepl("lazyness", CleanFactor_Other, ignore.case = TRUE) == TRUE, CleanFactor_Other := "Motivation"]
RawData[grepl("laziness", CleanFactor_Other, ignore.case = TRUE) == TRUE, CleanFactor_Other := "Motivation"]
RawData[grepl("idleness", CleanFactor_Other, ignore.case = TRUE) == TRUE, CleanFactor_Other := "Motivation"]
RawData[trimws(CleanFactor_Other) == 'I‚Äôm bored', CleanFactor_Other := "Motivation"]
RawData[trimws(CleanFactor_Other) == 'Forgetfulness', CleanFactor_Other := "Motivation"]
RawData[trimws(CleanFactor_Other) == 'If I have used the equipment or not', CleanFactor_Other := "Motivation"]
RawData[trimws(CleanFactor_Other) == 'Tiredness', CleanFactor_Other := "Motivation"]
RawData[trimws(CleanFactor_Other) == 'We just do it at the start of our fishing holiday season and again at the end', CleanFactor_Other := "Motivation"]
RawData[grepl("Having to track back to the lodge after a long hard day on the water.", CleanFactor_Other, ignore.case = TRUE) == TRUE, CleanFactor_Other := "Motivation"]

# Strip out weather
RawData[grepl("weather", tolower(trimws(CleanFactor_Other)), ignore.case = TRUE), CleanFactor_Other := "Weather"]
RawData[grepl("raining", tolower(trimws(CleanFactor_Other)), ignore.case = TRUE), CleanFactor_Other := "Weather"]
RawData[trimws(CleanFactor_Other) == 'In a storm with lightning', CleanFactor_Other := "Weather"]
RawData[trimws(CleanFactor_Other) == "Only when it's to wet to dry nets and bivvies out properly", CleanFactor_Other := "Weather"]
RawData[trimws(CleanFactor_Other) == 'Rain', CleanFactor_Other := "Weather"]
RawData[trimws(CleanFactor_Other) == 'Sunlight', CleanFactor_Other := "Weather"]
RawData[grepl("difficult", CleanFactor_Other, ignore.case = TRUE) == TRUE &
                grepl("raining", CleanFactor_Other, ignore.case = TRUE) == TRUE &
                grepl("home", CleanFactor_Other, ignore.case = TRUE) == TRUE, CleanFactor_Other := "Weather"]


# Strip out space
RawData[trimws(CleanFactor_Other) == 'limited space', CleanFactor_Other := "Limited Space"]
RawData[trimws(CleanFactor_Other) == 'Space to do so. And designated location', CleanFactor_Other := "Limited Space"]
RawData[trimws(CleanFactor_Other) == 'Garden space', CleanFactor_Other := "Limited Space"]
RawData[trimws(CleanFactor_Other) == 'Drying space', CleanFactor_Other := "Limited Space"]
RawData[grepl("space", tolower(trimws(CleanFactor_Other)), ignore.case = TRUE), CleanFactor_Other := "Limited Space"]
RawData[trimws(CleanFactor_Other) == 'Living in a flat', CleanFactor_Other := "Limited Space"]
RawData[trimws(CleanFactor_Other) == 'Live in a tower block', CleanFactor_Other := "Limited Space"]
RawData[trimws(CleanFactor_Other) == 'Size of house small 2 bedroom with no out building.', CleanFactor_Other := "Limited Space"]
RawData[trimws(CleanFactor_Other) == 'I do not have a garden to clean my landing net.', CleanFactor_Other := "Limited Space"]
RawData[trimws(CleanFactor_Other) == 'I live in a high rise flat, no garden, no outside hose, no outside water supply', CleanFactor_Other := "Limited Space"]

# Strip out different location
RawData[trimws(CleanFactor_Other) == 'Question missing, tackle cleaned if going to a different venue', CleanFactor_Other := "Moving Location"]
RawData[trimws(CleanFactor_Other) == 'use different tackle for different venues--always dry out nets etc after use', CleanFactor_Other := "Moving Location"]
RawData[trimws(CleanFactor_Other) == 'If returning to ther same river, which I do often, I occasionally do not clean my boots and waders.', CleanFactor_Other := "Moving Location"]
RawData[trimws(CleanFactor_Other) == 'Moving from venue', CleanFactor_Other := "Moving Location"]
RawData[trimws(CleanFactor_Other) == 'If I am fishing the same venue consecutively there‚Äôs no risk of contamination from different rivers so no need to clean', CleanFactor_Other := "Moving Location"]
RawData[trimws(CleanFactor_Other) == 'Cleaning isn‚Äôt done on a frequency or due to factors above it‚Äôs when I‚Äôm fishing away from my club waters', CleanFactor_Other := "Moving Location"]
RawData[trimws(CleanFactor_Other) == 'Only use equipment on one water. Use different tackle for each venue', CleanFactor_Other := "Moving Location"]
RawData[trimws(CleanFactor_Other) == 'most relevant is where i will be fishing next and in what time frame.', CleanFactor_Other := "Moving Location"]
RawData[trimws(CleanFactor_Other) == 'Mainly fish local river and ponds within walking distance of home.', CleanFactor_Other := "Moving Location"]
RawData[trimws(CleanFactor_Other) == "My various waders are usually only used at one venue and the months between usage let's them dry long enough to render any problems dealt with.  I will bank fish Grafham over the winter using neoprene waders which will be washed down on site and will not use them again until they go to the spey in. Mid February", CleanFactor_Other := "Moving Location"]
RawData[trimws(CleanFactor_Other) == 'Whether different water fished', CleanFactor_Other := "Moving Location"]
RawData[trimws(CleanFactor_Other) == 'I basically use different equipment for each venue so do not need to worry about introducing foreign species.', CleanFactor_Other := "Moving Location"]
RawData[trimws(CleanFactor_Other) == 'nets etc cleaned and dried in sunshine always, also different nets used for still waters and rivers', CleanFactor_Other := "Moving Location"]
RawData[trimws(CleanFactor_Other) == 'Moving from venue', CleanFactor_Other := "Moving Location"]
RawData[trimws(CleanFactor_Other) == 'If I‚Äôm not at home (on holiday)', CleanFactor_Other := "Moving Location"]
RawData[trimws(CleanFactor_Other) == 'I have specific nets slings mats etc for different venues', CleanFactor_Other := "Moving Location"]
RawData[trimws(CleanFactor_Other) == "I don't use the equipment in the UK", CleanFactor_Other := "Moving Location"]

 
RawData[grepl("next", CleanFactor_Other, ignore.case = TRUE) == TRUE &
                grepl("trip", CleanFactor_Other, ignore.case = TRUE) == TRUE &
                grepl("same", CleanFactor_Other, ignore.case = TRUE) == TRUE, CleanFactor_Other := "Moving Location"]
RawData[grepl("same", CleanFactor_Other, ignore.case = TRUE) == TRUE &
                grepl("water", CleanFactor_Other, ignore.case = TRUE) == TRUE &
                grepl("time", CleanFactor_Other, ignore.case = TRUE) == TRUE, CleanFactor_Other := "Moving Location"]
RawData[grepl("same", CleanFactor_Other, ignore.case = TRUE) == TRUE &
                grepl("water", CleanFactor_Other, ignore.case = TRUE) == TRUE &
                grepl("next day", CleanFactor_Other, ignore.case = TRUE) == TRUE, CleanFactor_Other := "Moving Location"]
RawData[grepl("same", CleanFactor_Other, ignore.case = TRUE) == TRUE &
                grepl("water", CleanFactor_Other, ignore.case = TRUE) == TRUE &
                grepl("fishing", CleanFactor_Other, ignore.case = TRUE) == TRUE, CleanFactor_Other := "Moving Location"]

# strip out understanding
RawData[grepl('Just poor habits and knowing what to wash and why maybe ignorance..', CleanFactor_Other, ignore.case = TRUE), CleanFactor_Other := "Understanding"]
RawData[trimws(CleanFactor_Other) == 'The lack of understanding as to why I should clean the equipment. I always ensure the nets are dried outdoors before they are used again', CleanFactor_Other := "Understanding"]
RawData[trimws(CleanFactor_Other) == "I've been fishing for years and I've only ever thought it was necessary to dry out all equipment between trips", CleanFactor_Other := "Understanding"]
RawData[trimws(CleanFactor_Other) == 'To be honest, I‚Äôm not sure on what needs cleaning and what products to use. This could be an excellent topic to promote!', CleanFactor_Other := "Understanding"]
RawData[trimws(CleanFactor_Other) == 'Unsure what to wash with', CleanFactor_Other := "Understanding"]
RawData[trimws(CleanFactor_Other) == 'I have never thought to clean the tip of my rod or any floats used.', CleanFactor_Other := "Understanding"]
RawData[trimws(CleanFactor_Other) == 'knowledge of what other chemicals to use', CleanFactor_Other := "Understanding"]
RawData[grepl("I clean equipment at home so information, cost and availability of cleaning station", CleanFactor_Other, ignore.case = TRUE) == TRUE, CleanFactor_Other := "Understanding"]
RawData[trimws(CleanFactor_Other) == 'Never thought it necessary!!', CleanFactor_Other := "Understanding"]
RawData[trimws(CleanFactor_Other) == 'to be fair , only wash my landing net and cradle or mat , weigh sling if used . Then air dry on shed.', CleanFactor_Other := "Understanding"]
RawData[trimws(CleanFactor_Other) == 'Informative notification', CleanFactor_Other := "Understanding"]
RawData[trimws(CleanFactor_Other) == "Does it NEED a clean?   I don't usually need my gear to look pristine, it gets used hard.", CleanFactor_Other := "Understanding"]
RawData[trimws(CleanFactor_Other) == 'Ignorance, never occured to me to clean wellies', CleanFactor_Other := "Understanding"]
RawData[trimws(CleanFactor_Other) == 'Thoroughly drying nets is more important than cleaning', CleanFactor_Other := "Understanding"]
RawData[trimws(CleanFactor_Other) == 'Not catching a fish.', CleanFactor_Other := "Understanding"]
RawData[trimws(CleanFactor_Other) == 'The relevance to the water and fish', CleanFactor_Other := "Understanding"]
RawData[trimws(CleanFactor_Other) == 'Condition of the water being fished.', CleanFactor_Other := "Understanding"]
RawData[trimws(CleanFactor_Other) == "Don't often wear waders, so there is a long drying period between use. Waders are mainly used in Austria where there are no NNIS problems", CleanFactor_Other := "Understanding"]
RawData[trimws(CleanFactor_Other) == 'I do not enter the water and I do not always catch fish', CleanFactor_Other := "Understanding"]
RawData[trimws(CleanFactor_Other) == 'This is for sea fishing', CleanFactor_Other := "Understanding"]
RawData[trimws(CleanFactor_Other) == 'i hose down trout bag ,and hang same and landing net on washing line to dry overnight.', CleanFactor_Other := "Understanding"]

# To prevent disease spread
RawData[grepl("If i've caught a fish that looks unhealthy or a venue that i suspect has an issue. Especially if foreign species could be there like signal crayfish", CleanFactor_Other, ignore.case = TRUE) == TRUE, CleanFactor_Other := "Disease Spread"]
RawData[trimws(CleanFactor_Other) == "No cleaning prevents disease", CleanFactor_Other := "Disease Spread"]
RawData[trimws(CleanFactor_Other) == "This is one method of keeping the risk of spreading disease down to a minimum.", CleanFactor_Other := "Disease Spread"]
RawData[trimws(CleanFactor_Other) == "Cleaning to prevent contaminating other fishing venues", CleanFactor_Other := "Disease Spread"]
RawData[trimws(CleanFactor_Other) == "Spreds diseases", CleanFactor_Other := "Disease Spread"]
RawData[trimws(CleanFactor_Other) == "Spread of disease", CleanFactor_Other := "Disease Spread"]
RawData[trimws(CleanFactor_Other) == "cleaning stops any contamition  from one water to another", CleanFactor_Other := "Disease Spread"]
RawData[trimws(CleanFactor_Other) == "I like to clean my equipment as i dont want to cross contaminate other fisheries", CleanFactor_Other := "Disease Spread"]
RawData[trimws(CleanFactor_Other) == "Lack of info re risks of disease transfer", CleanFactor_Other := "Disease Spread"]
RawData[trimws(CleanFactor_Other) == "Disease. Landing nets kept clean after each trip", CleanFactor_Other := "Disease Spread"]
RawData[trimws(CleanFactor_Other) == "No issues but.mostly use fishery supplied items to reduce the spread of disease.", CleanFactor_Other := "Disease Spread"]
RawData[trimws(CleanFactor_Other) == "Ensure it works correctly does not smell and to prevent khv transfer", CleanFactor_Other := "Disease Spread"]

# Availability of chemicals/antibac
RawData[trimws(CleanFactor_Other) == "Availability of antibacterial cleaning solutions", CleanFactor_Other := "Chemical Supply"]
RawData[trimws(CleanFactor_Other) == "The right chemicals", CleanFactor_Other := "Chemical Supply"]
RawData[trimws(CleanFactor_Other) == "Disinfectant for keepnets and landingnets rtc", CleanFactor_Other := "Chemical Supply"]
RawData[trimws(CleanFactor_Other) == "Easily available and cost of Zirkon", CleanFactor_Other := "Chemical Supply"]

# Legal
RawData[trimws(CleanFactor_Other) == "Legal requirements", CleanFactor_Other := "Legal requirement"]

# Change the indicator to record responses deemed no additional reasons
RawData[CleanFactor_Output == 1 & !is.na(CleanFactor_Other), CleanFactor_Output := 4]
#############################################################
# Check clean factor other for the options provided
# If a factor is identified but no score given then the highest
# available score is given
# Time
# No facilities = lack of hose/cleaning station
#############################################################
RawData[tolower(trimws(CleanFactor_Other)) == "Time" &
                is.na(CleanFactor_time),
        CleanFactor_time := apply(.SD, 1, ReturnAvailHighScore), .SDcols = c('CleanFactor_App','CleanFactor_Hose','CleanFactor_Cost','CleanFactor_Info','CleanFactor_time')]
RawData[grepl("No facilities", CleanFactor_Other, ignore.case = TRUE) == TRUE &
                is.na(CleanFactor_Hose), 
        CleanFactor_Hose := apply(.SD, 1, ReturnAvailHighScore), .SDcols = c('CleanFactor_App','CleanFactor_Hose','CleanFactor_Cost','CleanFactor_Info','CleanFactor_time')]



#############################################################
# Fishing abroad - 1 = yes; 2 = no; NA = no response
#############################################################
RawData[, FishingAbroadYN := NA]
RawData[FishAbroad == "Yes", FishingAbroadYN := 1]
RawData[FishAbroad == "No", FishingAbroadYN := 0]
RawData[, FishingAbroadYN := as.numeric(FishingAbroadYN)]

#############################################################
# Tidy the water entry and make numeric
############################################################
RawData[WaterEntry == "", WaterEntry := NA]
RawData[WaterEntry == "Yes", WaterEntry := 1]
RawData[WaterEntry == "No", WaterEntry := 0]
RawData[, WaterEntry := as.numeric(WaterEntry)]

#############################################################
# Tidy the threats which should be ranked
# There should be 5 numbers for each response
# Numbers should be 1 to 5
# If this is not the case then check a single number is missing
# If more than 1 number missing - Do not use the ranking
#############################################################

# Sort out the ranks to make them numerical
RawData[, Threat_Predation := apply(.SD, 1, ConvertRankToNumber), .SDcols = 'Threat_Predation']
RawData[, Threat_Pollution := apply(.SD, 1, ConvertRankToNumber), .SDcols = 'Threat_Pollution']
RawData[, Threat_INNS := apply(.SD, 1, ConvertRankToNumber), .SDcols = 'Threat_INNS']
RawData[, Threat_CC := apply(.SD, 1, ConvertRankToNumber), .SDcols = 'Threat_CC']
RawData[, Threat_HabitatQ := apply(.SD, 1, ConvertRankToNumber), .SDcols = 'Threat_HabitatQ']

RawData[, Threat_Predation := as.numeric(Threat_Predation)]
RawData[, Threat_Pollution := as.numeric(Threat_Pollution)]
RawData[, Threat_INNS := as.numeric(Threat_INNS)]
RawData[, Threat_CC := as.numeric(Threat_CC)]
RawData[, Threat_HabitatQ := as.numeric(Threat_HabitatQ)]

# Add a counter for the number of missing values present
RawData[, Threat_MissVals := apply(.SD, 1, CountMissingNumbers), .SDcols = c('Threat_Predation','Threat_Pollution','Threat_INNS','Threat_CC','Threat_HabitatQ')]

# For those with 1 missing value run a check and fill in with the missing number if applicable
RawData[Threat_MissVals == 1, Threat_MissRank := apply(.SD, 1, FindMissingRank), .SDcols = c('Threat_Predation','Threat_Pollution','Threat_INNS','Threat_CC','Threat_HabitatQ')]
RawData[Threat_MissVals == 1 & !is.na(Threat_MissRank) & is.na(Threat_Predation), Threat_Predation := Threat_MissRank]
RawData[Threat_MissVals == 1 & !is.na(Threat_MissRank) & is.na(Threat_Pollution), Threat_Pollution := Threat_MissRank]
RawData[Threat_MissVals == 1 & !is.na(Threat_MissRank) & is.na(Threat_INNS), Threat_INNS := Threat_MissRank]
RawData[Threat_MissVals == 1 & !is.na(Threat_MissRank) & is.na(Threat_CC), Threat_CC := Threat_MissRank]
RawData[Threat_MissVals == 1 & !is.na(Threat_MissRank) & is.na(Threat_HabitatQ), Threat_HabitatQ := Threat_MissRank]

# Reset the missing values after update
RawData[, Threat_MissVals := apply(.SD, 1, CountMissingNumbers), .SDcols = c('Threat_Predation','Threat_Pollution','Threat_INNS','Threat_CC','Threat_HabitatQ')]

# This is a flag for whether the ranking of threats is complete
RawData[, ThreatRankComp := 0] 
RawData[Threat_MissVals == 0, ThreatRankComp := 1]

# This is a flag for whether the ranking contains all unique values
RawData[, ThreatRankUnique := apply(.SD, 1, CheckRepeatedRank), .SDcols = c('Threat_Predation','Threat_Pollution','Threat_INNS','Threat_CC','Threat_HabitatQ')] 

############################################################
# Sort out the drying information
# DryWadersYN, DryEquipYN, DryYN_resp, LengthDry
############################################################
RawData[DryWadersYN == "", DryWadersYN := "No response"]
RawData[DryEquipYN == "", DryEquipYN := "No response"]
RawData[DryEquipYN == "No (please specify what prevents you from doing this)", DryEquipYN := "No"]

# Use the YN response to check some answers.  
# For example, dry naturally would be dry equipment?
# Here dry naturally, air dry, dry in the sun are all considered drying methods
RawData[, DryNaturallyYN := NA]
RawData[grepl("equipment is dried", DryYN_resp, ignore.case = TRUE) == TRUE, DryNaturallyYN := 1]
RawData[grepl("dry naturally", DryYN_resp, ignore.case = TRUE) == TRUE, DryNaturallyYN := 1]
RawData[grepl("dry naturaly", DryYN_resp, ignore.case = TRUE) == TRUE, DryNaturallyYN := 1]
RawData[grepl("dries naturally", DryYN_resp, ignore.case = TRUE) == TRUE, DryNaturallyYN := 1]
RawData[grepl("air dry", DryYN_resp, ignore.case = TRUE) == TRUE, DryNaturallyYN := 1]
RawData[grepl("dries itself", DryYN_resp, ignore.case = TRUE) == TRUE | grepl("dries on its own", DryYN_resp, ignore.case = TRUE) == TRUE, DryNaturallyYN := 1]
RawData[grepl("dries itself", DryYN_resp, ignore.case = TRUE) == TRUE, DryNaturallyYN := 1]
RawData[grepl("dry", DryYN_resp, ignore.case = TRUE) == TRUE & grepl("sun", DryYN_resp, ignore.case = TRUE) == TRUE, DryNaturallyYN := 1]
RawData[grepl("dry", DryYN_resp, ignore.case = TRUE) == TRUE & grepl("shed", DryYN_resp, ignore.case = TRUE) == TRUE, DryNaturallyYN := 1]
RawData[grepl("dries", DryYN_resp, ignore.case = TRUE) == TRUE & grepl("garage", DryYN_resp, ignore.case = TRUE) == TRUE, DryNaturallyYN := 1]
RawData[grepl("drys", DryYN_resp, ignore.case = TRUE) == TRUE & grepl("garage", DryYN_resp, ignore.case = TRUE) == TRUE, DryNaturallyYN := 1]

# Here the angler has said that the equipment is not wet anyway
RawData[, DryKitYN := NA]
RawData[grepl("not", DryYN_resp, ignore.case = TRUE) == TRUE &
        grepl("wet", DryYN_resp, ignore.case = TRUE) == TRUE, 
        DryKitYN := "No"]
RawData[grepl("not", DryYN_resp, ignore.case = TRUE) == TRUE &
        grepl("needed", DryYN_resp, ignore.case = TRUE) == TRUE, 
        DryKitYN := "No"]
RawData[grepl("not", DryYN_resp, ignore.case = TRUE) == TRUE &
        grepl("need", DryYN_resp, ignore.case = TRUE) == TRUE &
        grepl("drying", DryYN_resp, ignore.case = TRUE == TRUE), 
        DryKitYN := "No"]
RawData[grepl("not", DryYN_resp, ignore.case = TRUE) == TRUE &
        grepl("required", DryYN_resp, ignore.case = TRUE) == TRUE, 
        DryKitYN := "No"]
RawData[grepl("dry", DryYN_resp, ignore.case = TRUE) == TRUE & 
        grepl("already", DryYN_resp, ignore.case = TRUE) == TRUE,
        DryKitYN := "No"]

#####################################################################
# Create a DryYN based on one or other response
#####################################################################
# Set a separate flag to indicate whether drying has occurred
# Here if either waders or equipment are dried then this is set to 1
# If there is no response in either this is set to NA
# If no in either, and no yes in drying equipment or waders, then this is set to 0
# Currently a yes in either waders or equipment is a yes

RawData[, DryYN := NA]
RawData[DryEquipYN == "Yes" & DryWadersYN == "Yes", DryYN := 1]
RawData[DryEquipYN == "Yes" & DryWadersYN == "Don't use wellies or waders", DryYN := 1]
RawData[DryEquipYN == "Yes" & DryWadersYN == "No response", DryYN := 1]

RawData[DryEquipYN == "Yes" & DryWadersYN == "No", DryYN := 1]

RawData[DryEquipYN == "No" & DryWadersYN == "Don't use wellies or waders", DryYN := 0]
RawData[DryEquipYN == "No" & DryWadersYN == "No response", DryYN := 0]

RawData[DryEquipYN == "No response" & DryWadersYN == "No response", DryYN := NA]
RawData[DryEquipYN == "No response" & DryWadersYN == "Don't use wellies or waders", DryYN := NA]
RawData[, DryYN := as.numeric(DryYN)]

#####################################################################
# Tidy up the drying frequency
# If they do not dry then set to "Do not dry kit"
# If drying is not known then set to NA
# If frequency is not known then set to NA
######################################################################
RawData[, DryFreq := Freq_Dry]
RawData[DryYN == 0, DryFreq := "Do not dry kit"]
RawData[trimws(DryFreq) == "", DryFreq := NA]
RawData[is.na(DryYN), DryFreq := NA]

############################################################
# Tidy up the CCD and INNS knowledge
# convert to 1 - yes, 0 - no and NA - no response
############################################################
RawData[CCD_YN == "Yes", CCD_YN := 1]
RawData[CCD_YN == "No", CCD_YN := 0]
RawData[CCD_YN == "", CCD_YN := NA]
RawData[, CCD_YN := as.numeric(CCD_YN)]

RawData[INNS_YN == "Yes", INNS_YN := 1]
RawData[INNS_YN == "No", INNS_YN := 0]
RawData[INNS_YN == "", INNS_YN := NA]
RawData[, INNS_YN := as.numeric(INNS_YN)]

#############################################################
# Create an indicator for the "perfect" angler
#############################################################
RawData[, HotWaterAndDryYN := NA]
RawData[HotWaterYN == TRUE & DryYN == TRUE, HotWaterAndDryYN := TRUE]
RawData[HotWaterYN == FALSE & DryYN == TRUE, HotWaterAndDryYN := FALSE]
RawData[HotWaterYN == TRUE & DryYN == FALSE, HotWaterAndDryYN := FALSE]
RawData[HotWaterYN == FALSE & DryYN == FALSE, HotWaterAndDryYN := FALSE]
#################################################################################
#
# Choose the data to output
#
#################################################################################
DataCols <- c("Respondent.ID","AgeGroup","NewAgeGrouping", "Sex","AnglingFreq","FreqAng","NewAnglingFreq", "FishingAbroadYN","AnglingFreqRisk", "Equip_Number",
              "Game", "CoarseCarp","CoarseLure", "CoarseOther","Competition","Sea",
              "Coarse", "AngMostOften", "Specialist","NoAnglingSpec","AnglingClub_YN","Conserv_YN",
              "Threat_Predation", "Threat_Pollution","Threat_INNS","Threat_CC","Threat_HabitatQ",
              "Threat_MissVals","Threat_MissRank", "ThreatRankComp", "ThreatRankUnique",
              "WadersYN", "CleanWadersYN", "CleanEquipYN", "CleanYN","CleanFreq", "NewCleanFreq","WaterEntry",
              "HotWaterYN", "ColdWaterYN", "CleanWaterResponse",
              "CleaningProdYN","CleanProd_Disinfectant","CleanProd_Oils","CleanProd_Soap",
              "CleanFactor_Hose", "CleanFactor_Cost","CleanFactor_time", "CleanFactor_Info","CleanFactor_App", "CleanFactor_Highest", "CleanFactor_Lowest", 
              "CleanFactor_Other", "CleanFactor_Responses", "CleanFactor_Output",
              "CleanFactor_MissVals","CleanFactor_MissRank","CleanFactor_RankComp","CleanFactor_RankUnique",
              "DryWadersYN", "DryEquipYN", "DryYN", "DryFreq","DryNaturallyYN", "HotWaterAndDryYN",
              "CCD_YN","INNS_YN")

OutputRawData <- RawData[, .SD, .SDcols = DataCols]
