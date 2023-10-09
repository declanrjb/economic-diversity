
# output data contained in result_table and raw_change_table

library(tidyverse)
library(magrittr)

source("functions.R")

# read in college data, exactly as it appears on NYTimes website
df <- read_csv("raw_times_data.csv",show_col_types=FALSE)
colnames(df) <- gsub(" ","_",str_squish(colnames(df)))

# Build a result table for change in pell
result_table <- as.data.frame(matrix(ncol=3,nrow=3))
colnames(result_table) <- c("Value_Of_Change_In_Pell","Num_Institutions","Percentage_Of_All_Institutions")

# assign possible outcomes to rows
result_table[1,]$Value_Of_Change_In_Pell <- "< 0"
result_table[2,]$Value_Of_Change_In_Pell <- "= 0"
result_table[3,]$Value_Of_Change_In_Pell <- "> 0"

# get the number of institutions for which each property holds true (these should be mutually exclusive)
result_table[1,]$Num_Institutions <- length(which(df$Pell_change_since_2011 < 0))
result_table[2,]$Num_Institutions <- length(which(df$Pell_change_since_2011 == 0))
result_table[3,]$Num_Institutions <- length(which(df$Pell_change_since_2011 > 0))

# use the above to calculate percentages
result_table$Percentage_Of_All_Institutions <- (result_table$Num_Institutions / length(df$College)) * 100

message("Results when interpreted as change in share of Pell first-years:")
print(result_table)

# Percentage of institutions that have decreased their share of Pell freshman since 2011: 47.9 ≈ 48%
# This is less than a majority

# What if we interpret this statement as being about the pure number, rather than the share? 
# The below brings in new data from IPEDS that isn't necessarily the methodology of the original author

df$Freshmen_Pell_share <- parse_number(df$Freshmen_Pell_share)
df <- cbind(df,Pell_In_2011=NA)

# create a new column for percentage of pell freshman in 2010-11
df$Pell_In_2011 <- df$Freshmen_Pell_share + df$Pell_change_since_2011

#retrieve IPEDS data from that year
admission_2010 <- read_csv("IC2010/ic2010.csv",show_col_types=FALSE)

# this warning is a formatting error in a single row, and is therefore ignored
suppressWarnings({
  codebook_2010 <- read_csv("IC2010/hd2010.csv",show_col_types=FALSE)  
})

admission_2020 <- read_csv("ADM2020/adm2020.csv",show_col_types=FALSE)
codebook_2020 <- read_csv("ADM2020/hd2020.csv",show_col_types=FALSE)

# done in google sheets by the author of this script based on the abbreviations used in the Times, possible source of error
state_abs_codebook <- read_csv("times_state_abs_codebook.csv",show_col_types=FALSE)

# standardized encoding of state names across datasets
df$State <- state_abs_codebook$Name[match(df$State,state_abs_codebook$Times_Ab)]
df$State <- soft_match(df$State,state.name,state.abb)
df$State <- gsub("District of Columbia","DC",df$State)

# preserve original formatting of college names
df <- cbind(df,Preserved_Name=NA)
df$Preserved_Name <- df$College

# standardize formatting of strings for college names 
df$College <- tidy_up_string_vector(df$College)

# import admission data from the relevant years from IPEDS
admission_2010 <- cbind(College=NA,admission_2010)
admission_2010$College <- soft_match(admission_2010$UNITID,codebook_2010$UNITID,codebook_2010$INSTNM)
admission_2010$College <- tidy_up_string_vector(admission_2010$College)

admission_2020 <- cbind(College=NA,admission_2020)
admission_2020$College <- soft_match(admission_2020$UNITID,codebook_2020$UNITID,codebook_2020$INSTNM)
admission_2020$College <- tidy_up_string_vector(admission_2020$College)

# create new columns to merge the admissions data into the main dataframe
df <- cbind(df,Freshman_2010=NA)
df <- cbind(df,Freshman_2020=NA)
df <- cbind(df,Total_Pell_2010=NA)
df <- cbind(df,Total_Pell_2020=NA)
df <- cbind(df,Raw_Change_in_Pell=NA)

retrieve_freshman_2010 <- make_freshman_retriever(2010)
retrieve_freshman_2020 <- make_freshman_retriever(2020)

# merge in the admissions data from IPEDS
df$Freshman_2010 <- sapply(df$College,retrieve_freshman_2010)
df$Freshman_2020 <- sapply(df$College,retrieve_freshman_2020)

# calculate total number of Pell-eligible first-years for 2010-11 and 2020-21, respectively
df$Total_Pell_2010 <- df$Freshman_2010 %>% multiply_by(df$Pell_In_2011 %>% divide_by(100))
df$Total_Pell_2020 <- df$Freshman_2020 %>% multiply_by(df$Freshmen_Pell_share %>% divide_by(100))
df$Raw_Change_in_Pell <- df$Total_Pell_2020 %>% subtract(df$Total_Pell_2010)

# create a new results table for change in the raw number of Pell-eligible students
raw_change_table <- as.data.frame(matrix(ncol=3,nrow=4))
colnames(raw_change_table) <- c("Value_Of_Raw_Change_In_Pell","Num_Institutions","Percentage_Of_All_Institutions")

# assign categories to result table rows (should be mutually exclusive)
raw_change_table[1,]$Value_Of_Raw_Change_In_Pell <- "< 0"
raw_change_table[2,]$Value_Of_Raw_Change_In_Pell <- "= 0"
raw_change_table[3,]$Value_Of_Raw_Change_In_Pell <- "> 0"
raw_change_table[4,]$Value_Of_Raw_Change_In_Pell <- "Cannot be Calculated with Available Data"

# calculate number of institutions that fall into each category
raw_change_table[1,]$Num_Institutions <- length(which(df$Raw_Change_in_Pell < 0))
raw_change_table[2,]$Num_Institutions <- length(which(df$Raw_Change_in_Pell == 0))
raw_change_table[3,]$Num_Institutions <- length(which(df$Raw_Change_in_Pell > 0))
raw_change_table[4,]$Num_Institutions <- length(which(is.na(df$Raw_Change_in_Pell)))

# use the above to calculate percentages
raw_change_table$Percentage_Of_All_Institutions <- (raw_change_table$Num_Institutions / length(df$College)) * 100

message("Results when interpreted as raw change in number of first-year Pell students:")
print(raw_change_table)

# Assertion by the New York Times: "over the past decade, ... most schools have seen their number of these students decrease."
# Percentage of institutions in Times' data that have decreased their share of Pell-eligible students: ~= 48%
# Percentage of calculable (not unknown) institutions in expanded IPEDS data that have definitively decreased their raw numbers: 49% (not including 25% unknown in total)
# It's possible that the missing institutions (24% of those in the original table) change the result

