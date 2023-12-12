# _____________________
# Filter Data to SLIP + Clean  
# author: Claire Pavelka
# last edited date: 12/9/2020
# Description: filter all tables to SLIP + clean data tables
# _____________________

# _____________________
# Table of Contents -----
# 1. Set up the work space
# 2. Read in Lake + Survey Tables
# 3. Subset Lake + Survey Tables to SLIP  
# 4. Read in all other Tables
# 5. Subset all data sets to SLIP 
# 6. Clean all Tables!
# _____________________

# _____________________
# 1. Set up the work space -----

rm(list=ls()) 
  # clear the work space

# setwd() 
  # set up the working directory

library (dplyr)
library (tidyverse)
library (dataMaid)
library (lubridate)
  # load needed packages
# _____________________

# _____________________
# 2. Read in Lake + Survey Tables -----

lake_original <- read.csv ("01_raw_data/lakeTable.csv") 
  # read in the original lake csv downloaded from access
survey_original <- read.csv("01_raw_data/surveyTable.csv")
  # read in the original survey csv downloaded from access

head (lake_original)
head (survey_original)
  # check the data quickly

view (lake_original)
view (survey_original)
  # full view of both data sets

colnames (lake_original)
colnames (survey_original)
  # check column names
# _____________________

# _____________________
# 3. Subset Lake + Survey Tables to SLIP  -----
# _____________________
# Survey Table
# The original access database was designed for SLIP surveys, but data was also collected
# after the initial survey period ended. Therefore, all data frames need to be filtered to only 
# include surveys from the initial survey period (1995 - 2002). 
  # Steps:
  # 1. Filter data frame for specific years (1995 - 2002)
  # 2. Include only Initial Surveys in the Survey.Type category

  # 1. Filter data frame for specific years (1995 - 2002)
survey_SLIP <- survey_original
str(survey_SLIP)
  # check the class of each column. The date column is not in the correct format.
survey_SLIP$Date <- as.Date(survey_SLIP$Date, format= "%m/%d/%Y")
  # change the class of the Date column
str(survey_SLIP) 
  # check to see that this worked
survey_SLIP$Date <- format(survey_SLIP$Date, "%Y/%m/%d")
  # change the format of the date column to make it easier to divide into day, month and year 
  # this change will make filtering the data frame into specific years easier
head(survey_SLIP) 
  # check to see that the above code worked!
survey_SLIP <- separate(survey_SLIP, "Date", into = c("year", "month", "day"), sep = "/", remove = FALSE)
  # make new columns from the Date column to filter easier
head(survey_SLIP) 
  # check to see that this worked
table(survey_SLIP$year)
  # check what years are in the data frame
survey_SLIP <- survey_SLIP[!(survey_SLIP$year > 2002),]
  # remove all observations that occurred after 2002
table(survey_SLIP$year)
  # check to see that the above code worked

  # 2. Include only Initial Surveys in the Survey.Type category
table (survey_SLIP$Survey.Type)
  # check what survey types are in the survey_original data frame
survey_SLIP <- filter (survey_SLIP, Survey.Type == "Initial")
  # filters data to only include lakes that were initially sampled
table(survey_SLIP$Survey.Type) 
  # check to see that the filter worked
which(duplicated(survey_SLIP), arr.ind = T)
  # check for duplicates
# The survey Table now only includes SLIP data!

# _____________________
# Lake Table
# The original access database was designed for SLIP surveys, but data was also collected
# after the initial survey period ended. Therefore, all data frames need to be filtered to only 
# include surveys from the initial survey period (1995 - 2002). 
  # Steps:
  # 1. Subset Lakes Table to SLIP
  
  # 1. Subset Lakes Table to SLIP
colnames(lake_original)
colnames(survey_SLIP)
  # find the column names for the lake id column in each data frame
lake_SLIP <- lake_original[(lake_original$ï..Lake.ID %in% survey_SLIP$ï..LakeID),]
  # matches the Lake.ID from the survey data frame to the lake data frame 
# Now the lake data frame (df) only includes SLIP data

# clear the old data sets
rm(lake_original, survey_original)

# _____________________

# _____________________
# 4. Read in all other Tables -----

amphibian_original <- read.csv("01_raw_data/amphibTable.csv") 
  # read in the original Amphibian Table downloaded from access
fairyShrimp_original <- read.csv("01_raw_data/fairyshrimpTable.csv") 
  # read in the original Fairy Shrimp Table downloaded from access
fish_original <- read.csv("01_raw_data/fishTable.csv") 
  # read in the original Fish Table downloaded from access
littoralSubstrate_original <- read.csv("01_raw_data/littoralSubstrateTable.csv") 
  # read in the original Littoral Substrate Table downloaded from access
shorelineSubstrate_original <- read.csv("01_raw_data/shorelineSubstrateTable.csv") 
  # read in the original Shoreline Substrate Table downloaded from access
stream_original <- read.csv("01_raw_data/streamTable.csv") 
  # read in the original Stream Table downloaded from access
surveyor_original <- read.csv("01_raw_data/surveyorTable.csv")
  # read in the original Surveyor Inspection Table downloaded from access
zooplanktonSS_original <- read.csv("01_raw_data/zooplanktonSampleSumTable.csv") 
  # read in the original Zooplankton Sample Sum Table downloaded from access
zooplanktonSC_original <- read.csv("01_raw_data/zooplanktonSppCountsTable.csv") 
  # read in the original Zooplankton Spp Counts Table downloaded from access
zooplanktonSL_original <- read.csv("01_raw_data/zooplanktonSppLengthsTable.csv") 
  # read in the original Zooplankton Spp Lengths Table downloaded from access
zooplanktonSN_original <- read.csv("01_raw_data/zoopSppNamesTable.csv") 
  # read in the original Zooplankton Spp Names Table downloaded from access

head(amphibian_original)
head(fish_original)
head(littoralSubstrate_original)
head(shorelineSubstrate_original)
head(stream_original)
head(zooplanktonSC_original)
head(zooplanktonSL_original)
head(zooplanktonSN_original)
head(zooplanktonSS_original)
  # check the data quickly

colnames(amphibian_original)
colnames(fish_original)
colnames(littoralSubstrate_original)
colnames(shorelineSubstrate_original)
colnames(stream_original)
colnames(zooplanktonSC_original)
colnames(zooplanktonSL_original)
colnames(zooplanktonSN_original)
colnames(zooplanktonSS_original)
  # check the names of the columns 
# _____________________

# _____________________
# 5. Subset all data sets to SLIP -----
# _____________________
# Amphibian Table

str(amphibian_original) 
  # check class of each column, the date column is not in date format
amphibian_original$Date <- format(as.Date(amphibian_original$Date, format= "%d-%b-%y"), "%Y/%m/%d")
  # change the format + class of the Date column to make it easier to divide + subset 
str(amphibian_original) 
  # check to see that this worked
amphibian_original <- separate(amphibian_original, "Date", into = c("year", "month", "day"), sep = "/", remove = FALSE)
  # make new columns from the Date column to make it easier to divide + subset
head(amphibian_original)
  # check to see that this worked
table(amphibian_original$year)
  # find what years are in the Amphibian Table
amphibian_SLIP <- amphibian_original[!(amphibian_original$year > 2002),]
  # remove all observations that occurred after 2002 (past SLIP time range)
table(amphibian_SLIP$year)
  # check to see that the above code worked

colnames(amphibian_SLIP)
  # find the column name for lake id
amphibian_SLIP <- amphibian_SLIP[(amphibian_SLIP$ï..LakeID %in% lake_SLIP$ï..Lake.ID),]
  # ensures that only SLIP lakes are included

# clear the old data set
rm(amphibian_original)

# _____________________
# Fairy Shrimp Table

str(fairyShrimp_original) 
fairyShrimp_original$collect_date <- format(as.Date(fairyShrimp_original$collect_date, format= "%m/%d/%Y"), "%Y/%m/%d")
str(fairyShrimp_original) 
fairyShrimp_original <- separate(fairyShrimp_original, "collect_date", into = c("year", "month", "day"), sep = "/", remove = FALSE)
head(fairyShrimp_original)
table(fairyShrimp_original$year)
fairyShrimp_SLIP <- fairyShrimp_original[!(fairyShrimp_original$year > 2002),]
table(fairyShrimp_SLIP$year)

colnames(fairyShrimp_SLIP)
fairyShrimp_SLIP <- fairyShrimp_SLIP[(fairyShrimp_SLIP$ï..lake_id %in% lake_SLIP$ï..Lake.ID),]

# clear the old data set
rm(fairyShrimp_original)

# _____________________
# Fish Table

str(fish_original) 
fish_original$Date <- format(as.Date(fish_original$Date, format= "%d-%b-%y"), "%Y/%m/%d")
str(fish_original) 
fish_original <- separate(fish_original, "Date", into = c("year", "month", "day"), sep = "/", remove = FALSE)
head(fish_original)
table(fish_original$year)
fish_SLIP <- fish_original[!(fish_original$year > 2002),]
table(fish_SLIP$year)

colnames(fish_SLIP)
fish_SLIP <- fish_SLIP[(fish_SLIP$ï..Lake.ID %in% lake_SLIP$ï..Lake.ID),]

# clear the old data set
rm(fish_original)

# _____________________
# Littoral Substrate Table

str(littoralSubstrate_original) 
littoralSubstrate_original$survey_date <- format(as.Date(littoralSubstrate_original$survey_date, format= "%d-%b-%y"), "%Y/%m/%d")
str(littoralSubstrate_original) 
littoralSubstrate_original <- separate(littoralSubstrate_original, "survey_date", into = c("year", "month", "day"), sep = "/", remove = FALSE)
head(littoralSubstrate_original)
table(littoralSubstrate_original$year)
littoralSubstrate_SLIP <- littoralSubstrate_original[!(littoralSubstrate_original$year > 2002),]
table(littoralSubstrate_SLIP$year)

colnames(littoralSubstrate_SLIP)
littoralSubstrate_SLIP <- littoralSubstrate_SLIP[(littoralSubstrate_SLIP$ï..lake_id %in% lake_SLIP$ï..Lake.ID),]

# clear the old data set
rm(littoralSubstrate_original)

# _____________________
# Shoreline Substrate Table

str(shorelineSubstrate_original) 
shorelineSubstrate_original$survey_date <- format(as.Date(shorelineSubstrate_original$survey_date, format= "%d-%b-%y"), "%Y/%m/%d")
str(shorelineSubstrate_original) 
shorelineSubstrate_original <- separate(shorelineSubstrate_original, "survey_date", into = c("year", "month", "day"), sep = "/", remove = FALSE)
head(shorelineSubstrate_original)
table(shorelineSubstrate_original$year)
shorelineSubstrate_SLIP <- shorelineSubstrate_original[!(shorelineSubstrate_original$year > 2002),]
table(shorelineSubstrate_SLIP$year)

colnames(shorelineSubstrate_SLIP)
shorelineSubstrate_SLIP <- shorelineSubstrate_SLIP[(shorelineSubstrate_SLIP$ï..lake_id %in% lake_SLIP$ï..Lake.ID),]

# clear the old data set
rm(shorelineSubstrate_original)

# _____________________
# Stream Table

str(stream_original) 
stream_original$survey_date <- format(as.Date(stream_original$survey_date, format= "%d-%b-%y"), "%Y/%m/%d")
str(stream_original) 
stream_original <- separate(stream_original, "survey_date", into = c("year", "month", "day"), sep = "/", remove = FALSE)
head(stream_original)
table(stream_original$year)
stream_SLIP <- stream_original[!(stream_original$year > 2002),]
table(stream_SLIP$year)

colnames(stream_SLIP)
stream_SLIP <- stream_SLIP[(stream_SLIP$ï..lake_id %in% lake_SLIP$ï..Lake.ID),]

# clear the old data set
rm(stream_original)

# _____________________
# surveyor Table

str(surveyor_original) 
surveyor_original$survey_date <- format(as.Date(surveyor_original$survey_date, format= "%d-%b-%y"), "%Y/%m/%d")
str(surveyor_original) 
surveyor_original <- separate(surveyor_original, "survey_date", into = c("year", "month", "day"), sep = "/", remove = FALSE)
head(surveyor_original)
table(surveyor_original$year)
surveyor_SLIP <- surveyor_original[!(surveyor_original$year > 2002),]
table(surveyor_SLIP$year)

colnames(surveyor_SLIP)
surveyor_SLIP <- surveyor_SLIP[(surveyor_SLIP$ï..lake_id %in% lake_SLIP$ï..Lake.ID),]

# clear the old data set
rm(surveyor_original)

# _____________________
# Zooplankton Species Counts Table

str(zooplanktonSC_original) 
zooplanktonSC_original$survey_date <- format(as.Date(zooplanktonSC_original$survey_date , format= "%m/%d/%Y"), "%Y/%m/%d")
str(zooplanktonSC_original) 
zooplanktonSC_original <- separate(zooplanktonSC_original, "survey_date", into = c("year", "month", "day"), sep = "/", remove = FALSE)
head(zooplanktonSC_original)
table(zooplanktonSC_original$year) # all are in the SLIP range!

colnames(zooplanktonSC_original)
zooplanktonSC_SLIP <- zooplanktonSC_original[(zooplanktonSC_original$ï..lake_ID %in% lake_SLIP$ï..Lake.ID),]

# clear the old data set
rm(zooplanktonSC_original)

# _____________________
# Zooplankton Species Lengths Table

str(zooplanktonSL_original) 
zooplanktonSL_original$survey_date <- format(as.Date(zooplanktonSL_original$survey_date , format= "%m/%d/%Y"), "%Y/%m/%d")
str(zooplanktonSL_original) 
zooplanktonSL_original <- separate(zooplanktonSL_original, "survey_date", into = c("year", "month", "day"), sep = "/", remove = FALSE)
head(zooplanktonSL_original)
table(zooplanktonSL_original$year) # all are in the SLIP range!

colnames(zooplanktonSL_original)
zooplanktonSL_SLIP <- zooplanktonSL_original[(zooplanktonSL_original$ï..lake_id %in% lake_SLIP$ï..Lake.ID),]

# clear the old data set
rm(zooplanktonSL_original)

# _____________________
# Zooplankton Species Names Table

str(zooplanktonSN_original) 
  # this df does not have lake_id or dates
zooplanktonSN_SLIP <- zooplanktonSN_original

# clear the old data set
rm(zooplanktonSN_original)

# _____________________
# Zooplankton Sample Sums Table

str(zooplanktonSS_original) 
zooplanktonSS_original$survey_date <- format(as.Date(zooplanktonSS_original$survey_date , format= "%d/%m/%Y"), "%Y/%m/%d")
str(zooplanktonSS_original) 
zooplanktonSS_original <- separate(zooplanktonSS_original, "survey_date", into = c("year", "month", "day"), sep = "/", remove = FALSE)
head(zooplanktonSS_original)
table(zooplanktonSS_original$year) # all are in the SSIP range!

colnames(zooplanktonSS_original)
zooplanktonSS_SLIP <- zooplanktonSS_original[(zooplanktonSS_original$lake_id %in% lake_SLIP$ï..Lake.ID),]

# clear the old data set
rm(zooplanktonSS_original)

# _____________________

# _____________________
# 6. Clean all Tables! -----
# Repeat all steps below for each data table
  # 1. Assess data
  # 2. Change column names to match names in the original access database
       # (to make the metadata transferable from the original source)
  # 3. Change date format to EDI preference (YYYY-MM-DD or HH:MM:SS - TZ)
  # 4. Replace all missing cells with NA (if needed)
  # 5. Add a flag column to describe missing values (if needed) (if needed)
  # 6. Check for + remove duplicate observations
  # 7. Remove white spaces
  # 8. Look for any case issues
  # 9. Look for any misspellings
  # 10. Perform any final cleaning
  # 11. Change all classes to match access
        # (to make the metadata transferable from the original source)
  # 12. Remove old df + save as a clean .csv
# _____________________
# Amphibian Table (amphibian_SLIP)

# 1. Assess data
view(amphibian_SLIP)
  # view the df
str(amphibian_SLIP)
  # the classes need to be changed
colnames(amphibian_SLIP)
  # column names need to be updated

# 2. Change column names to match names in the original access database
amphibian_SLIP_clean <- subset(amphibian_SLIP, select = -c (3, 4, 5))
  # kept all the columns except the subdivided date column
colnames(amphibian_SLIP_clean) <- c("lake_id", "survey_date", "amphibian_species", "amphibian_life_stage",
                                 "amphibian_state", "amphibian_number", "amphibian_location", "amphibian_voucher", 
                                 "amphibian_sample", "amphibian_chytrid_survey", "amphibian_chytrid_infection", 
                                 "amphibian_comment")
  # rename all of the columns
head(amphibian_SLIP_clean)
  # check to see that this worked

# 3. Change date format to EDI preference (YYYY-MM-DD or HH:MM:SS - TZ)
amphibian_SLIP_clean$survey_date <- format(as.Date(amphibian_SLIP_clean$survey_date, format= "%Y/%m/%d"), "%Y-%m-%d")
  # change the format + class of the date column
str(amphibian_SLIP_clean)
  # check to see that this worked

# 4. Replace all missing cells with NA (if needed)
Missing_Values <-lapply(amphibian_SLIP_clean, identifyMissing)
Missing_Values
  # find all the columns that have missing values
amphibian_SLIP_clean$amphibian_comment[amphibian_SLIP_clean$amphibian_comment == ""] <- NA
  # replace all of the blank spaces with NA
Missing_Values <-lapply(amphibian_SLIP_clean, identifyMissing)
Missing_Values
  # check to see that this worked

# 5. Add a flag column to describe missing values (if needed) (if needed)
colnames(amphibian_SLIP_clean)[ apply(amphibian_SLIP_clean, 2, anyNA) ]
  # finds what columns have NA values
# no flag column needed, all NAs indicate that no data was collected (blanks in access)

# 6. check for + remove duplicate observations
which(duplicated(amphibian_SLIP_clean), arr.ind = T)
  # no duplicate values!

# 7. Remove white spaces
amphibian_SLIP_clean <- as_tibble(lapply(amphibian_SLIP_clean, trimws))

# 8. Look for any case issues
identifyCaseIssues(amphibian_SLIP_clean$amphibian_species) # no issues
identifyCaseIssues(amphibian_SLIP_clean$amphibian_life_stage) # no issues
identifyCaseIssues(amphibian_SLIP_clean$amphibian_state) # no issues
identifyCaseIssues(amphibian_SLIP_clean$amphibian_comment) # issue
identifyCaseIssues(amphibian_SLIP_clean$amphibian_location) # issue
  # finds case issues for any character columns in the data
amphibian_SLIP_clean$amphibian_location <- tolower(amphibian_SLIP_clean$amphibian_location)
amphibian_SLIP_clean$amphibian_comment <- tolower(amphibian_SLIP_clean$amphibian_comment)
  # make all text lowercase
identifyCaseIssues(amphibian_SLIP_clean$amphibian_location) # no issues
identifyCaseIssues(amphibian_SLIP_clean$amphibian_comment) # no issues
  # check to see that the above code worked

# 9. Look for any misspellings
identifyLoners(amphibian_SLIP_clean$amphibian_species) # no issues
identifyLoners(amphibian_SLIP_clean$amphibian_life_stage) # no issues
identifyLoners(amphibian_SLIP_clean$amphibian_state) # no issues 
identifyLoners(amphibian_SLIP_clean$amphibian_location) # no issues

# 10. Perform any final cleaning
# Remove columns that are not needed
colnames(amphibian_SLIP_clean)
  # remove the amphibian_sample, amphibian_chytrid_survey, amphibian_chytrid_infection 
  # and amphibian_comment columns. Removed because they are not originally part of the SLIP
  # database and/or are not relevant to data upload. 
head(amphibian_SLIP_clean)
colnames(amphibian_SLIP_clean)
amphibian_SLIP_clean <- subset(amphibian_SLIP_clean, select = -c (9:12))
colnames(amphibian_SLIP_clean)

# 11. Change all classes to match access
str(amphibian_SLIP_clean)
  # all columns are class
amphibian_SLIP_clean$lake_id <- as.integer(amphibian_SLIP_clean$lake_id)
amphibian_SLIP_clean$survey_date <- as.Date(amphibian_SLIP_clean$survey_date)
amphibian_SLIP_clean$amphibian_species <- as.factor(amphibian_SLIP_clean$amphibian_species)
amphibian_SLIP_clean$amphibian_life_stage <- as.factor(amphibian_SLIP_clean$amphibian_life_stage)
amphibian_SLIP_clean$amphibian_state <- as.factor(amphibian_SLIP_clean$amphibian_state)
amphibian_SLIP_clean$amphibian_number <- as.numeric(amphibian_SLIP_clean$amphibian_number)
amphibian_SLIP_clean$amphibian_location <- as.factor(amphibian_SLIP_clean$amphibian_location)
amphibian_SLIP_clean$amphibian_voucher <- as.integer(amphibian_SLIP_clean$amphibian_voucher)
  # change the class of each column
str(amphibian_SLIP_clean)
# check to see that this worked

# 12. Remove old df + save as a clean .csv
  # remove old dataset
rm(amphibian_SLIP)
  # save as a clean .csv
write.csv (amphibian_SLIP_clean, "02_clean_data/Amphibian.csv")

# _____________________
# Fairy Shrimp Table (fairyShrimp_SLIP)

# 1. Assess data
view(fairyShrimp_SLIP)
str(fairyShrimp_SLIP)
colnames(fairyShrimp_SLIP)

# 2. Change column names to match names in the original access database
fairyShrimp_SLIP_clean <- subset(fairyShrimp_SLIP, select = -c (3, 4, 5))
colnames(fairyShrimp_SLIP_clean) <- c("lake_id", "collect_date", "id_date", "identifier",
                                 "shrimp_species", "comments", "genetics")
head(fairyShrimp_SLIP_clean)

# 3. Change date format to EDI preference (YYYY-MM-DD or HH:MM:SS - TZ)
fairyShrimp_SLIP_clean$collect_date <- format(as.Date(fairyShrimp_SLIP_clean$collect_date, format= "%Y/%m/%d"), "%Y-%m-%d")
str(fairyShrimp_SLIP_clean)
fairyShrimp_SLIP_clean$id_date <- format(as.Date(fairyShrimp_SLIP_clean$id_date, format= "%m/%d/%Y"), "%Y-%m-%d")
str(fairyShrimp_SLIP_clean)

# 4.  Replace all missing cells with NA (if needed)
Missing_Values <-lapply(fairyShrimp_SLIP_clean, identifyMissing)
Missing_Values
  # missing values in the following columns: comments, genetics
fairyShrimp_SLIP_clean$comments[fairyShrimp_SLIP_clean$comments == ""] <- NA
fairyShrimp_SLIP_clean$genetics[fairyShrimp_SLIP_clean$genetics == ""] <- NA
Missing_Values <-lapply(fairyShrimp_SLIP_clean, identifyMissing)
Missing_Values

# 5. Add a flag column to describe missing values (if needed)
colnames(fairyShrimp_SLIP_clean)[ apply(fairyShrimp_SLIP_clean, 2, anyNA) ]
  # no flag column needed, all NAs indicate that no data was collected (blanks in access)

# 6. check for + remove duplicate observations
which(duplicated(fairyShrimp_SLIP_clean), arr.ind = T)
  # no duplicate values!

# 7. Remove white spaces
fairyShrimp_SLIP_clean <- as_tibble(lapply(fairyShrimp_SLIP_clean, trimws))

# 8. Look for any case issues
identifyCaseIssues(fairyShrimp_SLIP_clean$identifier) # no issues
identifyCaseIssues(fairyShrimp_SLIP_clean$shrimp_species) # no issues
identifyCaseIssues(fairyShrimp_SLIP_clean$comments) # no issues
identifyCaseIssues(fairyShrimp_SLIP_clean$genetics) # no issues

# 9. Look for any misspellings
identifyLoners(fairyShrimp_SLIP_clean$identifier) # no issues
identifyLoners(fairyShrimp_SLIP_clean$shrimp_species) # no issues
identifyLoners(fairyShrimp_SLIP_clean$comments) # no issues 
identifyLoners(fairyShrimp_SLIP_clean$genetics) # no issues

# 10. Perform any final cleaning
  # remove the comments column. Removed because column is not relevant to data upload. 
colnames(fairyShrimp_SLIP_clean)
fairyShrimp_SLIP_clean <- subset(fairyShrimp_SLIP_clean, select = -c (6))
colnames(fairyShrimp_SLIP_clean)
# change the order of the columns
fairyShrimp_SLIP_clean <- fairyShrimp_SLIP_clean [c ("lake_id", "collect_date", "id_date", "identifier",
                                         "shrimp_species", "genetics")]

# change the names of the people in identifier to numbers to match with the access database. 
table(fairyShrimp_SLIP_clean$identifier) 
  # determine what people are in this column
fairyShrimp_SLIP_clean$identifier [fairyShrimp_SLIP_clean$identifier  == "BELK"] <- "70"
  # not in personnel, added as number after 70 after last personnel entry
fairyShrimp_SLIP_clean$identifier [fairyShrimp_SLIP_clean$identifier  == "KNAPP"] <- "1"
  # was 1 in the personnel table
fairyShrimp_SLIP_clean$identifier [fairyShrimp_SLIP_clean$identifier  == "ROWAN"] <- "71"
  # not in personnel, added as number after 71 after 70/BELK
table(fairyShrimp_SLIP_clean$identifier) 
  # check that the above code worked

# 11. Change all classes to match access
str(fairyShrimp_SLIP_clean)
  # change to integer
fairyShrimp_SLIP_clean [, 1] <- lapply (fairyShrimp_SLIP_clean [, 1], as.integer)
  # change to factor
cols <- c (5)
fairyShrimp_SLIP_clean [, cols] <- lapply (fairyShrimp_SLIP_clean [, cols], as.factor)
  # change to date
cols <- c (2, 3)
fairyShrimp_SLIP_clean [, cols] <- lapply (fairyShrimp_SLIP_clean [, cols], as.Date)
str(fairyShrimp_SLIP_clean)

# 12. Remove old df + save as a clean .csv
  # remove old dataset
rm(fairyShrimp_SLIP)
  # save as a clean .csv
write.csv (fairyShrimp_SLIP_clean, "02_clean_data/FairyShrimp.csv")

# _____________________
# Fish Table (fish_SLIP)

# 1. Assess data
view(fish_SLIP)
str(fish_SLIP)
colnames(fish_SLIP)

# 2. Change column names to match names in the original access database
fish_SLIP_clean <- subset(fish_SLIP, select = -c (3, 4, 5))
colnames(fish_SLIP_clean) <- c("lake_id", "survey_date", "fish_id", "fish_species",
                                 "fish_length", "fish_weight", "fish_sex", "fish_egg_stage", 
                                 "fish_otolith", "fish_age", "fish_comment")
head(fish_SLIP_clean)

# 3. Change date format to EDI preference (YYYY-MM-DD or HH:MM:SS - TZ)
fish_SLIP_clean$survey_date <- format(as.Date(fish_SLIP_clean$survey_date, format= "%Y/%m/%d"), "%Y-%m-%d")
str(fish_SLIP_clean)

# 4.  Replace all missing cells with NA (if needed)
Missing_Values <-lapply(fish_SLIP_clean, identifyMissing)
Missing_Values
  # missing values found in the following columns: fish_sex, fish_egg_stage, fish_comment
fish_SLIP_clean$fish_sex [fish_SLIP_clean$fish_sex  == ""] <- NA
fish_SLIP_clean$fish_egg_stage[fish_SLIP_clean$fish_egg_stage == ""] <- NA
fish_SLIP_clean$fish_comment[fish_SLIP_clean$fish_comment == ""] <- NA
Missing_Values <-lapply(fish_SLIP_clean, identifyMissing)
Missing_Values

# 5. Add a flag column to describe missing values (if needed)
colnames(fish_SLIP_clean)[ apply(fish_SLIP_clean, 2, anyNA) ]
  # no flag column needed, all NAs indicate that no data was collected (blanks in access)

# 6. check for + remove duplicate observations
which(duplicated(fish_SLIP_clean), arr.ind = T) # no duplicate values!

# 7. Remove white spaces
fish_SLIP_clean <- as_tibble(lapply(fish_SLIP_clean, trimws))

# 8. Look for any case issues
identifyCaseIssues(fish_SLIP_clean$fish_species) # no issues
identifyCaseIssues(fish_SLIP_clean$fish_sex) # issues
identifyCaseIssues(fish_SLIP_clean$fish_egg_stage) # issues
identifyCaseIssues(fish_SLIP_clean$fish_otolith) # no issues
identifyCaseIssues(fish_SLIP_clean$fish_comment) # issues

fish_SLIP_clean$fish_sex <- toupper (fish_SLIP_clean$fish_sex)
fish_SLIP_clean$fish_egg_stage <- str_to_title (fish_SLIP_clean$fish_egg_stage)
fish_SLIP_clean$fish_comment <- tolower(fish_SLIP_clean$fish_comment)

identifyCaseIssues(fish_SLIP_clean$fish_sex) # no issues
identifyCaseIssues(fish_SLIP_clean$fish_egg_stage) #  no issues
identifyCaseIssues(fish_SLIP_clean$fish_comment) # no issues

# 9. Look for any misspellings
identifyLoners(fish_SLIP_clean$fish_species) # no issues
identifyLoners(fish_SLIP_clean$fish_sex) # no issues
identifyLoners(fish_SLIP_clean$fish_egg_stage) # no issues
identifyLoners(fish_SLIP_clean$fish_otolith) # no issues
identifyLoners(fish_SLIP_clean$fish_comment) # issues

# 10. Perform any final cleaning
  # remove the comments column. Removed because column is not relevant to data upload. 
colnames(fish_SLIP_clean)
fish_SLIP_clean <- subset(fish_SLIP_clean, select = -c (11))
colnames(fish_SLIP_clean)

# change True/ False back to Yes/ No
fish_SLIP_clean$fish_otolith [fish_SLIP_clean$fish_otolith  == "TRUE"] <- "Yes"
fish_SLIP_clean$fish_otolith [fish_SLIP_clean$fish_otolith  == "FALSE"] <- "No"
table(fish_SLIP_clean$fish_otolith)
  # check to see that this worked
# change these columns to factor class
fish_SLIP_clean$fish_otolith <- as.factor (fish_SLIP_clean$fish_otolith)
str(fish_SLIP_clean)

# change the N in fish_sex to NA (as sex cannot be determined)
fish_SLIP_clean$fish_sex [fish_SLIP_clean$fish_sex  == "N"] <- "NA"

# 11. Change all classes to match access
str(fish_SLIP_clean)
  # change to integer
cols <- c(1, 3, 5, 6, 10)
fish_SLIP_clean [, cols] <- lapply (fish_SLIP_clean [, cols], as.integer)
  # change to date
fish_SLIP_clean$survey_date <- as.Date (fish_SLIP_clean$survey_date)
  # change to factor
cols <- c (4, 7, 8)
fish_SLIP_clean [, cols] <- lapply (fish_SLIP_clean [, cols], as.factor)
str(fish_SLIP_clean)

# 12. Remove old df + save as a clean .csv
  # remove old dataset
rm (fish_SLIP)
  # save as a clean .csv
write.csv (fish_SLIP_clean, "02_clean_data/Fish.csv")

# _____________________
# Lake Table (lake_SLIP)

# 1. Assess data
view(lake_SLIP)
str(lake_SLIP)
colnames(lake_SLIP)

# 2. Change column names to match names in the original access database
lake_SLIP_clean <- subset(lake_SLIP, select = -c (2, 14:16))
colnames(lake_SLIP_clean) <- c ("lake_id", "lake_area_nbr", "lake_perimeter_nbr", "lake_type_code",
                               "lake_source_code", "lake_drainage_name", "lake_elevation_nbr", 
                               "lake_elevation_units", "lake_quad_name", "lake_county_name", 
                               "lake_juris_name", "lake_wilderness_name")
head(lake_SLIP_clean)

# 4.  Replace all missing cells with NA (if needed)
Missing_Values <-lapply(lake_SLIP_clean, identifyMissing)
Missing_Values
  # missing values found in the following columns: lake_elevation_units + lake_wilderness_name
lake_SLIP_clean$lake_elevation_units [lake_SLIP_clean$lake_elevation_units  == ""] <- NA
lake_SLIP_clean$lake_wilderness_name [lake_SLIP_clean$lake_wilderness_name == ""] <- NA
lake_SLIP_clean$lake_status [lake_SLIP_clean$lake_status == "?"] <- NA
Missing_Values <-lapply(lake_SLIP_clean, identifyMissing)
Missing_Values

# 5. Add a flag column to describe missing values (if needed)
colnames(lake_SLIP_clean)[ apply(lake_SLIP_clean, 2, anyNA) ]
  # no rows should have NA in Lake Table, replace with values
lake_SLIP_clean [ is.na (lake_SLIP_clean$lake_area_nbr),]
  # ID the row and column
lake_SLIP_clean$lake_area_nbr[is.na(lake_SLIP_clean$lake_area_nbr)] = 6847
  # replace with the correct value
lake_SLIP_clean [ is.na (lake_SLIP_clean$lake_perimeter_nbr),]
lake_SLIP_clean$lake_perimeter_nbr[is.na(lake_SLIP_clean$lake_perimeter_nbr)] = 328
lake_SLIP_clean [ is.na (lake_SLIP_clean$lake_elevation_nbr),]
lake_SLIP_clean$lake_elevation_nbr[is.na(lake_SLIP_clean$lake_elevation_nbr)] = 3035
lake_SLIP_clean [ is.na (lake_SLIP_clean$lake_wilderness_name),]
lake_SLIP_clean$lake_wilderness_name[is.na(lake_SLIP_clean$lake_wilderness_name)] = "JOHN_MUIR"
colnames(lake_SLIP_clean)[ apply(lake_SLIP_clean, 2, anyNA) ]
  # all the columns that should have values do!

# 6. check for + remove duplicate observations
which(duplicated(lake_SLIP_clean), arr.ind = T) # no duplicate values!

# 7. Remove white spaces
lake_SLIP_clean <- as_tibble(lapply(lake_SLIP_clean, trimws))

# 8. Look for any case issues
identifyCaseIssues(lake_SLIP_clean$lake_drainage_name) # no issues
identifyCaseIssues(lake_SLIP_clean$lake_elevation_units) # no issues
identifyCaseIssues(lake_SLIP_clean$lake_quad_name) # no issues
identifyCaseIssues(lake_SLIP_clean$lake_county_name) # no issues
identifyCaseIssues(lake_SLIP_clean$lake_juris_name) # issues
identifyCaseIssues(lake_SLIP_clean$lake_wilderness_name) # issues
identifyCaseIssues(lake_SLIP_clean$lake_status) # no issues

lake_SLIP_clean$lake_juris_name <- toupper (lake_SLIP_clean$lake_juris_name)
lake_SLIP_clean$lake_wilderness_name <- toupper (lake_SLIP_clean$lake_wilderness_name)

identifyCaseIssues(lake_SLIP_clean$lake_juris_name) # no issues
identifyCaseIssues(lake_SLIP_clean$lake_wilderness_name) #  no issues

# 9. Look for any misspellings
identifyLoners(lake_SLIP_clean$lake_drainage_name) # no issues
identifyLoners(lake_SLIP_clean$lake_elevation_units) # no issues
identifyLoners(lake_SLIP_clean$lake_quad_name) # no issues
identifyLoners(lake_SLIP_clean$lake_county_name) # no issues
identifyLoners(lake_SLIP_clean$lake_juris_name) # no issues
identifyLoners(lake_SLIP_clean$lake_wilderness_name) #  no issues
identifyLoners(lake_SLIP_clean$lake_status) # no issues

# 10. Perform any final cleaning
  # Remove columns that are not needed. Removed because column is not relevant to data upload. 
colnames(lake_SLIP_clean)
lake_SLIP_clean <- subset(lake_SLIP_clean, select = -c (5, 8))
  # remove the elevation_units and source code columns, put this in metadata
colnames(lake_SLIP_clean)

# 11. Change all classes to match access
str(lake_SLIP_clean)
  # change to integer
cols <- c( 1:4, 6)
lake_SLIP_clean [, cols] <- lapply (lake_SLIP_clean [, cols], as.integer)
  # change to factor
cols <- c (9, 10)
lake_SLIP_clean [, cols] <- lapply (lake_SLIP_clean [, cols], as.factor)
  # change to character
cols <- c (5, 7, 8)
lake_SLIP_clean [, cols] <- lapply (lake_SLIP_clean [, cols], as.character)
str(lake_SLIP_clean)

# 12. Remove old df + save as a clean .csv
  # remove old dataset
rm (lake_SLIP)
  # save as a clean .csv
write.csv (lake_SLIP_clean, "02_clean_data/Lake.csv")

# _____________________
# Littoral Substrate Table (littoralSubstrate_SLIP)

# 1. Assess data
view(littoralSubstrate_SLIP)
str(littoralSubstrate_SLIP)
colnames(littoralSubstrate_SLIP)

# 2. Change column names to match names in the original access database
littoralSubstrate_SLIP_clean <- subset(littoralSubstrate_SLIP, select = -c (3, 4, 5))
colnames(littoralSubstrate_SLIP_clean) <- c("lake_id", "survey_date", "littoral_type", "littoral_amount")
head(littoralSubstrate_SLIP_clean)

# 3. Change date format to EDI preference (YYYY-MM-DD or HH:MM:SS - TZ)
littoralSubstrate_SLIP_clean$survey_date <- format(as.Date(littoralSubstrate_SLIP_clean$survey_date, format= "%Y/%m/%d"), "%Y-%m-%d")
str(littoralSubstrate_SLIP_clean)

# 4.  Replace all missing cells with NA (if needed)
Missing_Values <-lapply(littoralSubstrate_SLIP_clean, identifyMissing)
Missing_Values
  # no missing values found

# 5. Add a flag column to describe missing values (if needed)
colnames(littoralSubstrate_SLIP_clean)[ apply(littoralSubstrate_SLIP_clean, 2, anyNA) ]
  # no NA values at all

# 6. check for + remove duplicate observations
which(duplicated(littoralSubstrate_SLIP_clean), arr.ind = T) # no duplicate values!

# 7. Remove white spaces
littoralSubstrate_SLIP_clean <- as_tibble(lapply(littoralSubstrate_SLIP_clean, trimws))

# 8. Look for any case issues
identifyCaseIssues(littoralSubstrate_SLIP_clean$littoral_type) # no issues

# 9. Look for any misspellings
identifyLoners (littoralSubstrate_SLIP_clean$littoral_type) # no issues

# 11. Change all classes to match access
str(littoralSubstrate_SLIP_clean)
  # change to integer
cols <- c(1, 4)
littoralSubstrate_SLIP_clean [, cols] <- lapply (littoralSubstrate_SLIP_clean [, cols], as.integer)
  # change to date
littoralSubstrate_SLIP_clean$survey_date <- as.Date (littoralSubstrate_SLIP_clean$survey_date)
  # change to factor
littoralSubstrate_SLIP_clean [, 3] <- lapply (littoralSubstrate_SLIP_clean [, 3], as.factor)
str(littoralSubstrate_SLIP_clean)

# 12. Remove old df + save as a clean .csv
  # remove old dataset
rm (littoralSubstrate_SLIP)
  # save as a clean .csv
write.csv (littoralSubstrate_SLIP_clean, "02_clean_data/LittoralSubstrate.csv")

# _____________________
# Shoreline Substrate Table (shorelineSubstrate_SLIP)

# 1. Assess data
view(shorelineSubstrate_SLIP)
str(shorelineSubstrate_SLIP)
colnames(shorelineSubstrate_SLIP)

# 2. Change column names to match names in the original access database
shorelineSubstrate_SLIP_clean <- subset(shorelineSubstrate_SLIP, select = -c (3, 4, 5))
colnames(shorelineSubstrate_SLIP_clean) <- c("lake_id", "survey_date", "shoreline_type", "shoreline_amount")
head(shorelineSubstrate_SLIP_clean)

# 3. Change date format to EDI preference (YYYY-MM-DD or HH:MM:SS - TZ)
shorelineSubstrate_SLIP_clean$survey_date <- format(as.Date(shorelineSubstrate_SLIP_clean$survey_date, format= "%Y/%m/%d"), "%Y-%m-%d")
str(shorelineSubstrate_SLIP_clean)

# 4.  Replace all missing cells with NA (if needed)
Missing_Values <-lapply(shorelineSubstrate_SLIP_clean, identifyMissing)
Missing_Values
  # no missing values found

# 5. Add a flag column to describe missing values (if needed)
colnames(shorelineSubstrate_SLIP_clean)[ apply(shorelineSubstrate_SLIP_clean, 2, anyNA) ]
  # no NA values at all 

# 6. check for + remove duplicate observations
which(duplicated(shorelineSubstrate_SLIP_clean), arr.ind = T) # no duplicate values

# 7. Remove white spaces
shorelineSubstrate_SLIP_clean <- as_tibble(lapply(shorelineSubstrate_SLIP_clean, trimws))

# 8. Look for any case issues
identifyCaseIssues(shorelineSubstrate_SLIP_clean$shoreline_type) # no issues

# 9. Look for any misspellings
identifyLoners (shorelineSubstrate_SLIP_clean$shoreline_type) # no issues

# 11. Change all classes to match access
str(shorelineSubstrate_SLIP_clean)
  # change to integer
cols <- c(1, 4)
shorelineSubstrate_SLIP_clean [, cols] <- lapply (shorelineSubstrate_SLIP_clean [, cols], as.integer)
  # change to date
shorelineSubstrate_SLIP_clean$survey_date <- as.Date (shorelineSubstrate_SLIP_clean$survey_date)
  # change to factor
shorelineSubstrate_SLIP_clean [, 3] <- lapply (shorelineSubstrate_SLIP_clean [, 3], as.factor)
str(shorelineSubstrate_SLIP_clean)

# 12. Remove old df + save as a clean .csv
  # remove old dataset
rm (shorelineSubstrate_SLIP)
  # save as a clean .csv
write.csv (shorelineSubstrate_SLIP_clean, "02_clean_data/ShorelineSubstrate.csv")

# _____________________
# Stream Table (stream_SLIP)

# 1. Assess data
view(stream_SLIP)
str(stream_SLIP)
colnames(stream_SLIP)

# 2. Change column names to match names in the original access database
stream_SLIP_clean <- subset(stream_SLIP, select = -c (3, 4, 5))
colnames(stream_SLIP_clean) <- c("lake_id", "survey_date", "stream_nos", "stream_type",
                               "stream_width", "stream_depth", "stream_fish_presence", "stream_barrier_distance", 
                               "stream_spawning_habitat", "stream_spawning_fish", "stream_redds", 
                               "stream_fry", "assoc_lake_spawning_habitat")
head(stream_SLIP_clean)

# 3. Change date format to EDI preference (YYYY-MM-DD or HH:MM:SS - TZ)
stream_SLIP_clean$survey_date <- format(as.Date(stream_SLIP_clean$survey_date, format= "%Y/%m/%d"), "%Y-%m-%d")
str(stream_SLIP_clean)

# 4.  Replace all missing cells with NA (if needed)
Missing_Values <-lapply(stream_SLIP_clean, identifyMissing)
Missing_Values
  # missing values found in the following columns: stream_type
stream_SLIP_clean$stream_type [stream_SLIP_clean$stream_type  == ""] <- NA
Missing_Values <-lapply(stream_SLIP_clean, identifyMissing)
Missing_Values

# 5. Add a flag column to describe missing values (if needed)
colnames(stream_SLIP_clean)[ apply(stream_SLIP_clean, 2, anyNA) ]
  # no flag column needed, all NAs indicate that no data was collected (blanks in access)

# 6. check for + remove duplicate observations
which(duplicated(stream_SLIP_clean), arr.ind = T) # no duplicate values!

# 7. Remove white spaces
stream_SLIP_clean <- as_tibble(lapply(stream_SLIP_clean, trimws))

# 8. Look for any case issues
identifyCaseIssues(stream_SLIP_clean$stream_type) # no issues
identifyCaseIssues(stream_SLIP_clean$stream_fish_presence) # no issues
identifyCaseIssues(stream_SLIP_clean$stream_spawning_fish) # no issues
identifyCaseIssues(stream_SLIP_clean$stream_redds) # no issues
identifyCaseIssues(stream_SLIP_clean$stream_fry) # no issues

# 9. Look for any misspellings
identifyLoners(stream_SLIP_clean$stream_type) # no issues
identifyLoners(stream_SLIP_clean$stream_fish_presence) # no issues
identifyLoners(stream_SLIP_clean$stream_spawning_fish) # no issues
identifyLoners(stream_SLIP_clean$stream_redds) # no issues
identifyLoners(stream_SLIP_clean$stream_fry) # no issues

# 10. Perform any final cleaning
# change True/ False back to Yes/ No
stream_SLIP_clean$stream_fish_presence [stream_SLIP_clean$stream_fish_presence  == "TRUE"] <- "Yes"
stream_SLIP_clean$stream_fish_presence [stream_SLIP_clean$stream_fish_presence  == "FALSE"] <- "No"

stream_SLIP_clean$stream_spawning_fish [stream_SLIP_clean$stream_spawning_fish  == "TRUE"] <- "Yes"
stream_SLIP_clean$stream_spawning_fish [stream_SLIP_clean$stream_spawning_fish  == "FALSE"] <- "No"

stream_SLIP_clean$stream_redds [stream_SLIP_clean$stream_redds  == "TRUE"] <- "Yes"
stream_SLIP_clean$stream_redds [stream_SLIP_clean$stream_redds  == "FALSE"] <- "No"

stream_SLIP_clean$stream_fry [stream_SLIP_clean$stream_fry  == "TRUE"] <- "Yes"
stream_SLIP_clean$stream_fry [stream_SLIP_clean$stream_fry  == "FALSE"] <- "No"

str(stream_SLIP_clean)
  # check to see that this worked
  # change these columns to factor class
cols <- c (7, 10:12)
stream_SLIP_clean [, cols] <- lapply (stream_SLIP_clean [, cols], as.factor)

# 11. Change all classes to match access
str(stream_SLIP_clean)
  # change to integer
cols <- c (1, 3, 5, 6, 8)
stream_SLIP_clean [, cols] <- lapply (stream_SLIP_clean [, cols], as.integer)
  # change to date
stream_SLIP_clean$survey_date <- as.Date (stream_SLIP_clean$survey_date)
  # change to factor
stream_SLIP_clean [, 4] <- lapply (stream_SLIP_clean [, 4], as.factor)
  # change to numeric
cols <- c (9, 13)
stream_SLIP_clean [, cols] <- lapply (stream_SLIP_clean [, cols], as.numeric)
str(stream_SLIP_clean)

# 12. Remove old df + save as a clean .csv
  # remove old dataset
rm (stream_SLIP)
  # save as a clean .csv
write.csv (stream_SLIP_clean, "02_clean_data/Stream.csv")

# _____________________
# Survey Table (survey_SLIP)

# 1. Assess data
view(survey_SLIP)
str(survey_SLIP)
colnames(survey_SLIP)

# 2. Change column names to match names in the original access database
survey_SLIP_clean <- subset(survey_SLIP, select = -c (3, 4, 5))
survey_SLIP_clean <- dplyr::rename (survey_SLIP_clean, "lake_id" ="ï..LakeID")
survey_SLIP_clean <- dplyr::rename (survey_SLIP_clean, "survey_date" ="Date")
survey_SLIP_clean <- dplyr::rename (survey_SLIP_clean, "survey_type" ="Survey.Type")
survey_SLIP_clean <- dplyr::rename (survey_SLIP_clean, "site_code_nbr" ="Surveyable.")
survey_SLIP_clean <- dplyr::rename (survey_SLIP_clean, "site_comments" ="Site.Comments")
survey_SLIP_clean <- dplyr::rename (survey_SLIP_clean, "air_temp" ="Air.Temp")
survey_SLIP_clean <- dplyr::rename (survey_SLIP_clean, "air_temp_time" ="Air.Temp.Time")
survey_SLIP_clean <- dplyr::rename (survey_SLIP_clean, "water_temp" ="Water.Temp")
survey_SLIP_clean <- dplyr::rename (survey_SLIP_clean, "water_temp_time" ="Water.Temp.Time")
survey_SLIP_clean <- dplyr::rename (survey_SLIP_clean, "wind" ="Wind")
survey_SLIP_clean <- dplyr::rename (survey_SLIP_clean, "sun" ="Sun")
survey_SLIP_clean <- dplyr::rename (survey_SLIP_clean, "lake_max_depth" ="Max.Depth")
survey_SLIP_clean <- dplyr::rename (survey_SLIP_clean, "lake_spawning_habitat_comment" ="Stream.Spawning.Habitat.Comments")
survey_SLIP_clean <- dplyr::rename (survey_SLIP_clean, "zoo_sample_ind" ="Zoop.Sample")
survey_SLIP_clean <- dplyr::rename (survey_SLIP_clean, "zoo_sample_time" ="Zoop.Time")
survey_SLIP_clean <- dplyr::rename (survey_SLIP_clean, "zoo_tow_number" ="TowNos")
survey_SLIP_clean <- dplyr::rename (survey_SLIP_clean, "zoo_tow_type" ="Tow.Type")
survey_SLIP_clean <- dplyr::rename (survey_SLIP_clean, "zoo_tow_depth" ="Tow.Depth")
survey_SLIP_clean <- dplyr::rename (survey_SLIP_clean, "benthic_sample_ind" ="Benthic.Survey")
survey_SLIP_clean <- dplyr::rename (survey_SLIP_clean, "benthic_sample_percent" ="X.Benthic")
survey_SLIP_clean <- dplyr::rename (survey_SLIP_clean, "nbr_benthic_sweeps" ="X.Sweeps")
survey_SLIP_clean <- dplyr::rename (survey_SLIP_clean, "lake_fairy_shrimp_ind" ="Shrimp.in.Lake")
survey_SLIP_clean <- dplyr::rename (survey_SLIP_clean, "lake_shrimp_collection" ="Lake.Collection.")
survey_SLIP_clean <- dplyr::rename (survey_SLIP_clean, "pool_fairy_shrimp_ind" ="Shrimp.in.Pools")
survey_SLIP_clean <- dplyr::rename (survey_SLIP_clean, "pool_shrimp_collection" ="Pool.Collection.")
survey_SLIP_clean <- dplyr::rename (survey_SLIP_clean, "fairy_shrimp_comments" ="Shrimp.Comments")
survey_SLIP_clean <- dplyr::rename (survey_SLIP_clean, "amphib_survey_starttime" ="AmphSurBeg")
survey_SLIP_clean <- dplyr::rename (survey_SLIP_clean, "amphib_survey_endtime" ="AmphSurEnd")
survey_SLIP_clean <- dplyr::rename (survey_SLIP_clean, "amphib_survey_duration" ="AmphSurDur")
survey_SLIP_clean <- dplyr::rename (survey_SLIP_clean, "amphib_survey_desc" ="Amphibian.Survey.Description")
survey_SLIP_clean <- dplyr::rename (survey_SLIP_clean, "amphib_survey_fish_presence" ="Visual.Fish.Survey")
survey_SLIP_clean <- dplyr::rename (survey_SLIP_clean, "actual_fish_presence" ="Actual.fish.status")
survey_SLIP_clean <- dplyr::rename (survey_SLIP_clean, "fish_survey_type" ="Fish.Survey.Type")
survey_SLIP_clean <- dplyr::rename (survey_SLIP_clean, "survey_comment" ="Survey.Comments")
survey_SLIP_clean <- dplyr::rename (survey_SLIP_clean, "fish_net_location_type" ="Net.Location")
survey_SLIP_clean <- dplyr::rename (survey_SLIP_clean, "fish_net_set_datetime" ="Net.Set.Time")
survey_SLIP_clean <- dplyr::rename (survey_SLIP_clean, "amphib_observer1" ="amphib_observer1")
survey_SLIP_clean <- dplyr::rename (survey_SLIP_clean, "amphib_observer2" ="amphib_observer2")
survey_SLIP_clean <- dplyr::rename (survey_SLIP_clean, "amphib_observer3" ="amphib_observer3")
survey_SLIP_clean <- dplyr::rename (survey_SLIP_clean, "crew_list" ="crew_list")
survey_SLIP_clean <- dplyr::rename (survey_SLIP_clean, "fish_net_pull_datetime" ="Net.Pull.Time")

head(survey_SLIP_clean)

# Change date format to EDI preference (YYYY-MM-DD or HH:MM:SS - TZ)
  # change the date to the correct format
survey_SLIP_clean$survey_date <- format(as.Date(survey_SLIP_clean$survey_date, format= "%Y/%m/%d"), "%Y-%m-%d")
str(survey_SLIP_clean)
 
  # change the time to correct format
survey_SLIP_clean$air_temp_time <- as.character (strptime (survey_SLIP_clean$air_temp_time, "%H:%M", tz="America/Los_Angeles"),"%H:%M:%S")
survey_SLIP_clean$air_temp_time <- paste0(survey_SLIP_clean$air_temp_time, "-07")
survey_SLIP_clean$air_temp_time <- na_if(survey_SLIP_clean$air_temp_time, "NA-07")

survey_SLIP_clean$water_temp_time <- as.character (strptime (survey_SLIP_clean$water_temp_time, "%H:%M", tz="America/Los_Angeles"),"%H:%M:%S")
survey_SLIP_clean$water_temp_time <- paste0(survey_SLIP_clean$water_temp_time, "-07")
survey_SLIP_clean$water_temp_time <- na_if(survey_SLIP_clean$water_temp_time, "NA-07")

survey_SLIP_clean$zoo_sample_time <- as.character (strptime (survey_SLIP_clean$zoo_sample_time, "%H:%M", tz="America/Los_Angeles"),"%H:%M:%S")
survey_SLIP_clean$zoo_sample_time <- paste0(survey_SLIP_clean$zoo_sample_time, "-07")
survey_SLIP_clean$zoo_sample_time <- na_if(survey_SLIP_clean$zoo_sample_time, "NA-07")

survey_SLIP_clean$zoo_sample_time <- as.character (strptime (survey_SLIP_clean$zoo_sample_time, "%H:%M", tz="America/Los_Angeles"),"%H:%M:%S")
survey_SLIP_clean$zoo_sample_time <- paste0(survey_SLIP_clean$zoo_sample_time, "-07")
survey_SLIP_clean$zoo_sample_time <- na_if(survey_SLIP_clean$zoo_sample_time, "NA-07")

survey_SLIP_clean$amphib_survey_starttime  <- as.character (strptime (survey_SLIP_clean$amphib_survey_starttime , "%H:%M", tz="America/Los_Angeles"),"%H:%M:%S")
survey_SLIP_clean$amphib_survey_starttime  <- paste0(survey_SLIP_clean$amphib_survey_starttime , "-07")
survey_SLIP_clean$amphib_survey_starttime  <- na_if(survey_SLIP_clean$amphib_survey_starttime , "NA-07")

survey_SLIP_clean$amphib_survey_endtime  <- as.character (strptime (survey_SLIP_clean$amphib_survey_endtime , "%H:%M", tz="America/Los_Angeles"),"%H:%M:%S")
survey_SLIP_clean$amphib_survey_endtime  <- paste0(survey_SLIP_clean$amphib_survey_endtime , "-07")
survey_SLIP_clean$amphib_survey_endtime  <- na_if(survey_SLIP_clean$amphib_survey_endtime , "NA-07")
  
  # change the date and time to the correct format (columns fish_net_set_datetime + fish_net_pull_datetime)
cols <- c (36, 41)
survey_SLIP_clean [, cols] <- lapply (survey_SLIP_clean [, cols], as.character)
  # convert both columns to character class
survey_SLIP_clean$fish_net_set_datetime <- lubridate::mdy_hms(survey_SLIP_clean$fish_net_set_datetime)
table(survey_SLIP_clean$fish_net_set_datetime)
  # check that the code worked
survey_SLIP_clean$fish_net_pull_datetime <- lubridate::mdy_hms(survey_SLIP_clean$fish_net_pull_datetime)
table(survey_SLIP_clean$fish_net_pull_datetime)
  # check that the code worked
  # add on the timezone to these entries
survey_SLIP_clean$fish_net_set_datetime  <- paste0(survey_SLIP_clean$fish_net_set_datetime , "-07")
survey_SLIP_clean$fish_net_set_datetime  <- na_if(survey_SLIP_clean$fish_net_set_datetime , "NA-07")
table(survey_SLIP_clean$fish_net_set_datetime)
  # check that the code worked
survey_SLIP_clean$fish_net_pull_datetime  <- paste0(survey_SLIP_clean$fish_net_pull_datetime , "-07")
survey_SLIP_clean$fish_net_pull_datetime  <- na_if(survey_SLIP_clean$fish_net_pull_datetime , "NA-07")
table(survey_SLIP_clean$fish_net_pull_datetime)
  # check that the code worked

# 4.  Replace all missing cells with NA (if needed)
Missing_Values <-lapply(survey_SLIP_clean, identifyMissing)
Missing_Values
  # missing values found in the following columns: site_comments, lake_spawning_habitat_comment, zoo_sample_ind, 
  # zoo_tow_type, benthic_sample_ind, lake_fairy_shrimp_ind, pool_fairy_shrimp_ind, fairy_shrimp_comments, 
  # amphib_survey_desc, amphib_survey_fish_presence, actual_fish_presence, fish_survey_type, survey_comment, 
  # fish_net_location_type, amphib_observer1, amphib_observer2, amphib_observer3
survey_SLIP_clean$site_comments [survey_SLIP_clean$site_comments  == ""] <- NA
survey_SLIP_clean$lake_spawning_habitat_comment [survey_SLIP_clean$lake_spawning_habitat_comment  == ""] <- NA
survey_SLIP_clean$zoo_sample_ind [survey_SLIP_clean$zoo_sample_ind  == ""] <- NA
survey_SLIP_clean$zoo_tow_type [survey_SLIP_clean$zoo_tow_type  == ""] <- NA
survey_SLIP_clean$benthic_sample_ind [survey_SLIP_clean$benthic_sample_ind  == ""] <- NA
survey_SLIP_clean$lake_fairy_shrimp_ind [survey_SLIP_clean$lake_fairy_shrimp_ind  == ""] <- NA
survey_SLIP_clean$pool_fairy_shrimp_ind [survey_SLIP_clean$pool_fairy_shrimp_ind  == ""] <- NA
survey_SLIP_clean$fairy_shrimp_comments [survey_SLIP_clean$fairy_shrimp_comments  == ""] <- NA
survey_SLIP_clean$amphib_survey_desc [survey_SLIP_clean$amphib_survey_desc  == ""] <- NA
survey_SLIP_clean$amphib_survey_fish_presence [survey_SLIP_clean$amphib_survey_fish_presence  == ""] <- NA
survey_SLIP_clean$actual_fish_presence [survey_SLIP_clean$actual_fish_presence  == ""] <- NA
survey_SLIP_clean$fish_survey_type [survey_SLIP_clean$fish_survey_type  == ""] <- NA
survey_SLIP_clean$survey_comment [survey_SLIP_clean$survey_comment  == ""] <- NA
survey_SLIP_clean$fish_net_location_type [survey_SLIP_clean$fish_net_location_type  == ""] <- NA
survey_SLIP_clean$amphib_observer1 [survey_SLIP_clean$amphib_observer1  == ""] <- NA
survey_SLIP_clean$amphib_observer2 [survey_SLIP_clean$amphib_observer2  == ""] <- NA
survey_SLIP_clean$amphib_observer3 [survey_SLIP_clean$amphib_observer3  == ""] <- NA

Missing_Values <-lapply(survey_SLIP_clean, identifyMissing)
Missing_Values

# 5. Add a flag column to describe missing values (if needed)
colnames(survey_SLIP_clean)[ apply(survey_SLIP_clean, 2, anyNA) ]
  # no flag column needed, all NAs indicate that no data was collected (blanks in access)

# 6. check for + remove duplicate observations
which(duplicated(survey_SLIP_clean), arr.ind = T) # no duplicate values!

# 7. Remove white spaces
survey_SLIP_clean <- as_tibble(lapply(survey_SLIP_clean, trimws))

# 8. Look for any case issues
identifyCaseIssues(survey_SLIP_clean$survey_type) # no issues
identifyCaseIssues(survey_SLIP_clean$site_comments) # issue
identifyCaseIssues(survey_SLIP_clean$lake_spawning_habitat_comment) # issue
identifyCaseIssues(survey_SLIP_clean$zoo_sample_ind) # no issues
identifyCaseIssues(survey_SLIP_clean$zoo_tow_type) # no issues
identifyCaseIssues(survey_SLIP_clean$benthic_sample_ind) # no issues
identifyCaseIssues(survey_SLIP_clean$lake_fairy_shrimp_ind) # no issues
identifyCaseIssues(survey_SLIP_clean$pool_fairy_shrimp_ind) # no issues
identifyCaseIssues(survey_SLIP_clean$fairy_shrimp_comments) # no issues
identifyCaseIssues(survey_SLIP_clean$amphib_survey_desc) # issue
identifyCaseIssues(survey_SLIP_clean$amphib_survey_fish_presence) # no issues
identifyCaseIssues(survey_SLIP_clean$actual_fish_presence) # no issues
identifyCaseIssues(survey_SLIP_clean$fish_survey_type) # no issues
identifyCaseIssues(survey_SLIP_clean$survey_comment)  # issues
identifyCaseIssues(survey_SLIP_clean$fish_net_location_type) # no issues
identifyCaseIssues(survey_SLIP_clean$amphib_observer1) # no issues
identifyCaseIssues(survey_SLIP_clean$amphib_observer2) # no issues
identifyCaseIssues(survey_SLIP_clean$amphib_observer3) # no issues
identifyCaseIssues(survey_SLIP_clean$crew_list) # no issues
  # issues with the following columns: site_comments, lake_spawning_habitat_comment, 
  # amphib_survey_desc, survey_comment
survey_SLIP_clean$site_comments <- tolower (survey_SLIP_clean$site_comments)
survey_SLIP_clean$lake_spawning_habitat_comment <- str_to_title (survey_SLIP_clean$lake_spawning_habitat_comment)
survey_SLIP_clean$amphib_survey_desc <- tolower (survey_SLIP_clean$amphib_survey_desc)
survey_SLIP_clean$survey_comment <- str_to_title (survey_SLIP_clean$survey_comment)

identifyCaseIssues(survey_SLIP_clean$site_comments) # no issues
identifyCaseIssues(survey_SLIP_clean$lake_spawning_habitat_comment) # no issues
identifyCaseIssues(survey_SLIP_clean$amphib_survey_desc) # no issues
identifyCaseIssues(survey_SLIP_clean$survey_comment) # no issues

# 9. Look for any misspellings
identifyLoners(survey_SLIP_clean$survey_type) # no issues
identifyLoners(survey_SLIP_clean$site_comments) # no issues
identifyLoners(survey_SLIP_clean$lake_spawning_habitat_comment) # no issues
identifyLoners(survey_SLIP_clean$zoo_sample_ind) # no issues
identifyLoners(survey_SLIP_clean$zoo_tow_type) # no issues
identifyLoners(survey_SLIP_clean$benthic_sample_ind) # no issues
identifyLoners(survey_SLIP_clean$lake_fairy_shrimp_ind) # no issues
identifyLoners(survey_SLIP_clean$pool_fairy_shrimp_ind) # no issues
identifyLoners(survey_SLIP_clean$fairy_shrimp_comments) # no issues
identifyLoners(survey_SLIP_clean$amphib_survey_desc) # no issues
identifyLoners(survey_SLIP_clean$amphib_survey_fish_presence) # no issues
identifyLoners(survey_SLIP_clean$actual_fish_presence) # no issues
identifyLoners(survey_SLIP_clean$fish_survey_type) # no issues
identifyLoners(survey_SLIP_clean$survey_comment) # no issues
identifyLoners(survey_SLIP_clean$fish_net_location_type) # no issues
identifyLoners(survey_SLIP_clean$amphib_observer1) # no issues
identifyLoners(survey_SLIP_clean$amphib_observer2) # no issues
identifyLoners(survey_SLIP_clean$amphib_observer3) # no issues
identifyLoners(survey_SLIP_clean$crew_list) # no issues

# 10. Perform any final cleaning
  # Remove columns that are not needed. Removed because column is not relevant to data upload.
colnames(survey_SLIP_clean)
survey_SLIP_clean <- subset(survey_SLIP_clean, select = -c (3,5,13,26,34,37:40))
colnames(survey_SLIP_clean)
  # Remove columns that are not needed

# change True/ False back to Yes/ No
survey_SLIP_clean$lake_shrimp_collection [survey_SLIP_clean$lake_shrimp_collection  == "TRUE"] <- "Yes"
survey_SLIP_clean$lake_shrimp_collection [survey_SLIP_clean$lake_shrimp_collection  == "FALSE"] <- "No"

survey_SLIP_clean$pool_shrimp_collection [survey_SLIP_clean$pool_shrimp_collection  == "TRUE"] <- "Yes"
survey_SLIP_clean$pool_shrimp_collection [survey_SLIP_clean$pool_shrimp_collection  == "FALSE"] <- "No"

# change the 0 to NA, as there is no 0 condition for wind.
survey_SLIP_clean$wind [survey_SLIP_clean$wind  == "0"] <- "NA"

# change the 0 to NA, as there is no 0 condition for sun.
survey_SLIP_clean$sun [survey_SLIP_clean$sun  == "0"] <- "NA"

# 11. Change all classes to match access
str(survey_SLIP_clean)
  # change to integer
cols <- c (1,18,25)
survey_SLIP_clean [, cols] <- lapply (survey_SLIP_clean [, cols], as.integer)
  # change to date
cols <- c (2, 5, 7, 12, 23, 24, 31, 32)
survey_SLIP_clean [, cols] <- lapply (survey_SLIP_clean [, cols], as.Date)
  # change to factor
cols <- c (3, 8, 9, 11, 14, 16, 19:22, 27:30)
survey_SLIP_clean [, cols] <- lapply (survey_SLIP_clean [, cols], as.factor)
  # change to numeric
cols <- c (4, 6, 10, 13, 15, 17)
survey_SLIP_clean [, cols] <- lapply (survey_SLIP_clean [, cols], as.numeric)
str(survey_SLIP_clean)

# 12. Remove old df + save as a clean .csv
  # remove old dataset
rm (survey_SLIP)
  # save as a clean .csv
write.csv (survey_SLIP_clean, "02_clean_data/Survey.csv")

# _____________________
# Surveyor Table (surveyor_SLIP)

# 1. Assess data
view(surveyor_SLIP)
str(surveyor_SLIP)
colnames(surveyor_SLIP)

# 2. Change column names to match names in the original access database
surveyor_SLIP_clean <- subset(surveyor_SLIP, select = -c (3, 4, 5))
colnames(surveyor_SLIP_clean) <- c("lake_id", "survey_date", "survey_type", "crew_id")
head(surveyor_SLIP_clean)

# 3. Change date format to EDI preference (YYYY-MM-DD or HH:MM:SS - TZ)
surveyor_SLIP_clean$survey_date <- format(as.Date(surveyor_SLIP_clean$survey_date, format= "%Y/%m/%d"), "%Y-%m-%d")
str(surveyor_SLIP_clean)

# 4.  Replace all missing cells with NA (if needed)
Missing_Values <-lapply(surveyor_SLIP_clean, identifyMissing)
Missing_Values
  # no problems in the data table

# 5. Add a flag column to describe missing values (if needed)
colnames(surveyor_SLIP_clean)[ apply(surveyor_SLIP_clean, 2, anyNA) ]
  # no Na values

# 6. check for + remove duplicate observations
which(duplicated(surveyor_SLIP_clean), arr.ind = T) # no duplicate values!

# 7. Remove white spaces
surveyor_SLIP_clean <- as_tibble(lapply(surveyor_SLIP_clean, trimws))

# 8. Look for any case issues
identifyCaseIssues(surveyor_SLIP_clean$survey_type) # no issues

# 9. Look for any misspellings
identifyLoners(surveyor_SLIP_clean$survey_type) # no issues

# 11. Change all classes to match access
str(surveyor_SLIP_clean)
  # change to integer
cols <- c (1,4)
surveyor_SLIP_clean [, cols] <- lapply (surveyor_SLIP_clean [, cols], as.integer)
  # change to date
surveyor_SLIP_clean$survey_date <- as.Date (surveyor_SLIP_clean$survey_date)
  # change to factor
surveyor_SLIP_clean [, 3] <- lapply (surveyor_SLIP_clean [, 3], as.factor)
str(surveyor_SLIP_clean)

# 12. Remove old df + save as a clean .csv
  # remove old dataset
rm (surveyor_SLIP)
  # save as a clean .csv
write.csv (surveyor_SLIP_clean, "02_clean_data/Surveyor.csv")

# _____________________
# Zooplankton Spp Counts Table (zooplanktonSC_SLIP)

# 1. Assess data
view(zooplanktonSC_SLIP)
str(zooplanktonSC_SLIP)
colnames(zooplanktonSC_SLIP)

# 2. Change column names to match names in the original access database
zooplanktonSC_SLIP_clean <- subset(zooplanktonSC_SLIP, select = -c (3, 4, 5))
zooplanktonSC_SLIP_clean <- dplyr::rename (zooplanktonSC_SLIP_clean, "lake_id" ="ï..lake_ID")
head(zooplanktonSC_SLIP_clean)

# 3. Change date format to EDI preference (YYYY-MM-DD or HH:MM:SS - TZ)
zooplanktonSC_SLIP_clean$survey_date <- format(as.Date(zooplanktonSC_SLIP_clean$survey_date, format= "%Y/%m/%d"), "%Y-%m-%d")
str(zooplanktonSC_SLIP_clean)

# 4.  Replace all missing cells with NA (if needed)
Missing_Values <-lapply(zooplanktonSC_SLIP_clean, identifyMissing)
Missing_Values
  # no missing values found

# 5. Add a flag column to describe missing values (if needed)
colnames(zooplanktonSC_SLIP_clean)[ apply(zooplanktonSC_SLIP_clean, 2, anyNA) ]
  # no Na values

# 6. check for + remove duplicate observations
which(duplicated(zooplanktonSC_SLIP_clean), arr.ind = T) # no duplicate values!

# 7. Remove white spaces
zooplanktonSC_SLIP_clean <- as_tibble(lapply(zooplanktonSC_SLIP_clean, trimws))

# 8. Look for any case issues
  # only numeric values

# 9. Look for any misspellings
  # only numeric values

# 10. Perform any final cleaning 
table(zooplanktonSC_SLIP_clean$SpeciesID)
zooplanktonSC_SLIP_clean <- filter(zooplanktonSC_SLIP_clean, SpeciesID != 5)
zooplanktonSC_SLIP_clean <- filter(zooplanktonSC_SLIP_clean, SpeciesID != 6)
zooplanktonSC_SLIP_clean <- filter(zooplanktonSC_SLIP_clean, SpeciesID != 8)
zooplanktonSC_SLIP_clean <- filter(zooplanktonSC_SLIP_clean, SpeciesID != 28)
zooplanktonSC_SLIP_clean <- filter(zooplanktonSC_SLIP_clean, SpeciesID != 29)
zooplanktonSC_SLIP_clean <- filter(zooplanktonSC_SLIP_clean, SpeciesID != 30)
zooplanktonSC_SLIP_clean <- filter(zooplanktonSC_SLIP_clean, SpeciesID != 50)
table(zooplanktonSC_SLIP_clean$SpeciesID)

# 11. Change all classes to match access
str(zooplanktonSC_SLIP_clean)
  # change to integer
cols <- c (1, 3:5)
zooplanktonSC_SLIP_clean [, cols] <- lapply (zooplanktonSC_SLIP_clean [, cols], as.integer)
  # change to date
zooplanktonSC_SLIP_clean$survey_date <- as.Date (zooplanktonSC_SLIP_clean$survey_date)
str(zooplanktonSC_SLIP_clean)

# 12. Remove old df + save as a clean .csv
  # remove old dataset
rm (zooplanktonSC_SLIP)
  # save as a clean .csv
write.csv (zooplanktonSC_SLIP_clean, "02_clean_data/ZooplanktonSpeciesCounts.csv")

# _____________________
# Zooplankton Spp Lengths Table (zooplanktonSL_SLIP)

# 1. Assess data
view(zooplanktonSL_SLIP)
str(zooplanktonSL_SLIP)
colnames(zooplanktonSL_SLIP)

# 2. Change column names to match names in the original access database
zooplanktonSL_SLIP_clean <- subset(zooplanktonSL_SLIP, select = -c (3, 4, 5))
zooplanktonSL_SLIP_clean <- dplyr::rename (zooplanktonSL_SLIP_clean, "lake_id" ="ï..lake_id")
head(zooplanktonSL_SLIP_clean)

# 3. Change date format to EDI preference (YYYY-MM-DD or HH:MM:SS - TZ)
zooplanktonSL_SLIP_clean$survey_date <- format(as.Date(zooplanktonSL_SLIP_clean$survey_date, format= "%Y/%m/%d"), "%Y-%m-%d")
str(zooplanktonSL_SLIP_clean)

# 4.  Replace all missing cells with NA (if needed)
Missing_Values <-lapply(zooplanktonSL_SLIP_clean, identifyMissing)
Missing_Values
  # no missing values

# 5. Add a flag column to describe missing values (if needed)
colnames(zooplanktonSL_SLIP_clean)[ apply(zooplanktonSL_SLIP_clean, 2, anyNA) ]
  # No NAs

# 6. check for + remove duplicate observations
which(duplicated(zooplanktonSL_SLIP_clean), arr.ind = T) # no duplicate values!

# 7. Remove white spaces
zooplanktonSL_SLIP_clean <- as_tibble(lapply(zooplanktonSL_SLIP_clean, trimws))

# 8. Look for any case issues
  # only numeric values

# 9. Look for any misspellings
  # only numeric values

# 10. Perform any final cleaning 
table(zooplanktonSL_SLIP_clean$SpeciesID)
zooplanktonSL_SLIP_clean <- filter(zooplanktonSL_SLIP_clean, SpeciesID != 5)
zooplanktonSL_SLIP_clean <- filter(zooplanktonSL_SLIP_clean, SpeciesID != 6)
zooplanktonSL_SLIP_clean <- filter(zooplanktonSL_SLIP_clean, SpeciesID != 29)
zooplanktonSL_SLIP_clean <- filter(zooplanktonSL_SLIP_clean, SpeciesID != 30)
zooplanktonSL_SLIP_clean <- filter(zooplanktonSL_SLIP_clean, SpeciesID != 50)
table(zooplanktonSL_SLIP_clean$SpeciesID)

# 11. Change all classes to match access
str(zooplanktonSL_SLIP_clean)
  # change to integer
cols <- c (1, 3:5)
zooplanktonSL_SLIP_clean [, cols] <- lapply (zooplanktonSL_SLIP_clean [, cols], as.integer)
  # change to date
zooplanktonSL_SLIP_clean$survey_date <- as.Date (zooplanktonSL_SLIP_clean$survey_date)
str(zooplanktonSL_SLIP_clean)

# 12. Remove old df + save as a clean .csv
  # remove old dataset
rm (zooplanktonSL_SLIP)
  # save as a clean .csv
write.csv (zooplanktonSL_SLIP_clean, "02_clean_data/ZooplanktonSpeciesLengths.csv")

# _____________________
# Zooplankton Spp Names Table (zooplanktonSN_original)

# 1. Assess data
view(zooplanktonSN_SLIP)
str(zooplanktonSN_SLIP)
colnames(zooplanktonSN_SLIP)

# 2. Change column names to match names in the original access database
zooplanktonSN_SLIP_clean <- zooplanktonSN_SLIP
colnames(zooplanktonSN_SLIP_clean) <- c("SpeciesID", "Species_Name")
head(zooplanktonSN_SLIP_clean)

# 4.  Replace all missing cells with NA (if needed)
Missing_Values <-lapply(zooplanktonSN_SLIP_clean, identifyMissing)
Missing_Values
  # no missing values found

# 5. Add a flag column to describe missing values (if needed)
colnames(zooplanktonSN_SLIP_clean)[ apply(zooplanktonSN_SLIP_clean, 2, anyNA) ]
  # no NAs

# 6. check for + remove duplicate observations
which(duplicated(zooplanktonSN_SLIP_clean), arr.ind = T) # no duplicate values!

# 7. Remove white spaces
zooplanktonSN_SLIP_clean <- as_tibble(lapply(zooplanktonSN_SLIP_clean, trimws))

# 8. Look for any case issues
identifyCaseIssues(zooplanktonSN_SLIP_clean$Species_Name) # no issues

# 9. Look for any misspellings
identifyLoners(zooplanktonSN_SLIP_clean$Species_Name) # no issues

# 10. Perform any final cleaning 
table(zooplanktonSN_SLIP_clean$SpeciesID)
zooplanktonSN_SLIP_clean <- filter(zooplanktonSN_SLIP_clean, SpeciesID != 5)
zooplanktonSN_SLIP_clean <- filter(zooplanktonSN_SLIP_clean, SpeciesID != 6)
zooplanktonSN_SLIP_clean <- filter(zooplanktonSN_SLIP_clean, SpeciesID != 8)
zooplanktonSN_SLIP_clean <- filter(zooplanktonSN_SLIP_clean, SpeciesID != 9)
zooplanktonSN_SLIP_clean <- filter(zooplanktonSN_SLIP_clean, SpeciesID != 28)
zooplanktonSN_SLIP_clean <- filter(zooplanktonSN_SLIP_clean, SpeciesID != 29)
zooplanktonSN_SLIP_clean <- filter(zooplanktonSN_SLIP_clean, SpeciesID != 30)
zooplanktonSN_SLIP_clean <- filter(zooplanktonSN_SLIP_clean, SpeciesID != 38)
zooplanktonSN_SLIP_clean <- filter(zooplanktonSN_SLIP_clean, SpeciesID != 49)
zooplanktonSN_SLIP_clean <- filter(zooplanktonSN_SLIP_clean, SpeciesID != 50)
zooplanktonSN_SLIP_clean <- filter(zooplanktonSN_SLIP_clean, SpeciesID != 51)
table(zooplanktonSN_SLIP_clean$SpeciesID)

# 11. Change all classes to match access
str(zooplanktonSN_SLIP_clean)
  # change to integer
zooplanktonSN_SLIP_clean [, 1] <- lapply (zooplanktonSN_SLIP_clean [, 1], as.integer)
str(zooplanktonSN_SLIP_clean)

# 12. Remove old dfs + save as a clean .csv
  # remove old dataset
rm (zooplanktonSN_SLIP)
  # save as a clean .csv
write.csv (zooplanktonSN_SLIP_clean, "02_clean_data/ZooplanktonSpeciesName.csv")

# _____________________
# Zooplankton Sample Sums (zooplanktonSS_SLIP)

# 1. Assess data
view(zooplanktonSS_SLIP)
str(zooplanktonSS_SLIP)
colnames(zooplanktonSS_SLIP)

# 2. Change column names to match names in the original access database
zooplanktonSS_SLIP_clean <- subset(zooplanktonSS_SLIP, select = -c (3, 4, 5))
zooplanktonSS_SLIP_clean <- dplyr::rename (zooplanktonSS_SLIP_clean, "lake_id" ="ï..lake_id")
head(zooplanktonSS_SLIP_clean)

# 3. Change date format to EDI preference (YYYY-MM-DD or HH:MM:SS - TZ)
zooplanktonSS_SLIP_clean$survey_date <- format(as.Date(zooplanktonSS_SLIP_clean$survey_date, format= "%Y/%m/%d"), "%Y-%m-%d")
str(zooplanktonSS_SLIP_clean)

zooplanktonSS_SLIP_clean$collect_date <- format(as.Date(zooplanktonSS_SLIP_clean$collect_date , format= "%m/%d/%Y"), "%Y-%m-%d")
str(zooplanktonSS_SLIP_clean) 

  # change the process date column
zooplanktonSS_SLIP_clean$process_date <- lubridate::parse_date_time(zooplanktonSS_SLIP_clean$process_date, orders = "a b d H:M:S Y")
zooplanktonSS_SLIP_clean$process_date  <- paste0(zooplanktonSS_SLIP_clean$process_date , "-07")
zooplanktonSS_SLIP_clean$process_date  <- na_if(zooplanktonSS_SLIP_clean$process_date , "NA-07")
head(zooplanktonSS_SLIP_clean)

# 4.  Replace all missing cells with NA (if needed)
Missing_Values <-lapply(zooplanktonSS_SLIP_clean, identifyMissing)
Missing_Values
  # missing values found in the following columns: sample_type, notes
zooplanktonSS_SLIP_clean$sample_type [zooplanktonSS_SLIP_clean$sample_type  == ""] <- NA
zooplanktonSS_SLIP_clean$notes [zooplanktonSS_SLIP_clean$notes  == ""] <- NA
Missing_Values <-lapply(zooplanktonSS_SLIP_clean, identifyMissing)
Missing_Values

# 5. Add a flag column to describe missing values (if needed)
colnames(zooplanktonSS_SLIP_clean)[ apply(zooplanktonSS_SLIP_clean, 2, anyNA) ]
  # no flag column needed, all NAs indicate that no data was collected (blanks in access)

# 6. check for + remove duplicate observations
which(duplicated(zooplanktonSS_SLIP_clean), arr.ind = T) # no duplicate values!

# 7. Remove white spaces
zooplanktonSS_SLIP_clean <- as_tibble(lapply(zooplanktonSS_SLIP_clean, trimws))

# 8. Look for any case issues
identifyCaseIssues(zooplanktonSS_SLIP_clean$sample_select) # no issues
identifyCaseIssues(zooplanktonSS_SLIP_clean$process_name) # issues
identifyCaseIssues(zooplanktonSS_SLIP_clean$sample_type) # no issues
identifyCaseIssues(zooplanktonSS_SLIP_clean$notes) # no issues

zooplanktonSS_SLIP_clean$process_name <- str_to_title (zooplanktonSS_SLIP_clean$process_name)
identifyCaseIssues(zooplanktonSS_SLIP_clean$process_name) # no issues

# 9. Look for any misspellings
identifyLoners(zooplanktonSS_SLIP_clean$sample_select) # no issues
identifyLoners(zooplanktonSS_SLIP_clean$process_name) # issues
identifyLoners(zooplanktonSS_SLIP_clean$sample_type) # no issues
identifyLoners(zooplanktonSS_SLIP_clean$notes) # no issues

# 10. Perform any final cleaning 
# change the names of the people in identifier to numbers 
zooplanktonSS_SLIP_clean$process_name <- as.character (zooplanktonSS_SLIP_clean$process_name)
  # convert column to character
table(zooplanktonSS_SLIP_clean$process_name) 
  # determine what people are in this column
zooplanktonSS_SLIP_clean$process_name [zooplanktonSS_SLIP_clean$process_name  == "Garton"] <- "72"
  # not in personnel, added as number after 72 after last personnel + fairy shrimp entries
zooplanktonSS_SLIP_clean$process_name [zooplanktonSS_SLIP_clean$process_name  == "Kirchner"] <- "37"
  # was 37 in the personnel table
zooplanktonSS_SLIP_clean$process_name [zooplanktonSS_SLIP_clean$process_name  == "Knapp"] <- "1"
  # was 1 in the personnel table
zooplanktonSS_SLIP_clean$process_name [zooplanktonSS_SLIP_clean$process_name  == "Rowan"] <- "71"
  # not in personnel, added as number after 71 after last personnel entry
zooplanktonSS_SLIP_clean$process_name [zooplanktonSS_SLIP_clean$process_name  == "3"] <- "37"
  # 3 is not in personnel table, add as NA
table(zooplanktonSS_SLIP_clean$process_name) 
  # check that the above code worked

# Replace names in Notes column with codes
zooplanktonSS_SLIP_clean$notes <- as.character (zooplanktonSS_SLIP_clean$notes)
  # convert column to character
zooplanktonSS_SLIP_clean$notes [zooplanktonSS_SLIP_clean$notes == "Data entered on 4-17-98 by Kirchner from data sheet"] <- "Data entered on 4-17-98 by 71 from data sheet"
zooplanktonSS_SLIP_clean$notes [zooplanktonSS_SLIP_clean$notes == "Data entered on 4-17-98 by Kirchner from data sheet."] <- "Data entered on 4-17-98 by 71 from data sheet"
  # not in personnel, added as number after 71 after last personnel entry

# 11. Change all classes to match access
str(zooplanktonSS_SLIP_clean)
  # change to integer
cols <- c (1, 7, 8, 10)
zooplanktonSS_SLIP_clean [, cols] <- lapply (zooplanktonSS_SLIP_clean [, cols], as.integer)
  # change to date
zooplanktonSS_SLIP_clean$survey_date <- as.Date (zooplanktonSS_SLIP_clean$survey_date)
zooplanktonSS_SLIP_clean$collect_date <- as.Date (zooplanktonSS_SLIP_clean$collect_date)
  # change to factor
cols <- c (4, 6, 9)
zooplanktonSS_SLIP_clean [, cols] <- lapply (zooplanktonSS_SLIP_clean [, cols], as.factor)
str(zooplanktonSS_SLIP_clean)

# 12. Remove old df + save as a clean .csv
  # remove old dataset
rm (zooplanktonSS_SLIP)
  # save as a clean .csv
write.csv (zooplanktonSS_SLIP_clean, "02_clean_data/ZooplanktonSampleSums.csv")

# _____________________
 # remove uneeded objects 
rm (Missing_Values, cols)
