
# output data contained in result_table and raw_change_table

library(tidyverse)
library(magrittr)

source("functions.R")

df <- read_csv("raw_times_data.csv")
colnames(df) <- gsub(" ","_",str_squish(colnames(df)))

result_table <- as.data.frame(matrix(ncol=3,nrow=3))
colnames(result_table) <- c("Value_Of_Change_In_Pell","Num_Institutions","Percentage_Of_All_Institutions")

result_table[1,]$Value_Of_Change_In_Pell <- "< 0"
result_table[2,]$Value_Of_Change_In_Pell <- "= 0"
result_table[3,]$Value_Of_Change_In_Pell <- "> 0"

result_table[1,]$Num_Institutions <- length(which(df$Pell_change_since_2011 < 0))
result_table[2,]$Num_Institutions <- length(which(df$Pell_change_since_2011 == 0))
result_table[3,]$Num_Institutions <- length(which(df$Pell_change_since_2011 > 0))

result_table$Percentage_Of_All_Institutions <- result_table$Num_Institutions / length(df$College)

# Assertion by the New York Times: over the past decade, ... most schools have seen their number of these students decrease.
# According to the Times' own data, percentage of institutions that have decreased their share of Pell freshman since 2011: 47.9 ≈ 48%
# Conclusion: statement is false

# What if we interpret this statement as being about the pure number, rather than the share? 
# (This doesn't make much sense, they don't present data for that, but let's give it a shot)

df$Freshmen_Pell_share <- parse_number(df$Freshmen_Pell_share)
df <- cbind(df,Pell_In_2011=NA)
df$Pell_In_2011 <- df$Freshmen_Pell_share + df$Pell_change_since_2011

admission_2010 <- read_csv("IC2010/ic2010.csv")
codebook_2010 <- read_csv("IC2010/hd2010.csv")

admission_2020 <- read_csv("ADM2020/adm2020.csv")
codebook_2020 <- read_csv("ADM2020/hd2020.csv")

state_abs_codebook <- read_csv("times_state_abs_codebook.csv")

df$State <- state_abs_codebook$Name[match(df$State,state_abs_codebook$Times_Ab)]
df$State <- soft_match(df$State,state.name,state.abb)
df$State <- gsub("District of Columbia","DC",df$State)

df <- cbind(df,Preserved_Name=NA)
df$Preserved_Name <- df$College
df$College <- tidy_up_string_vector(df$College)

admission_2010 <- cbind(College=NA,admission_2010)
admission_2010$College <- soft_match(admission_2010$UNITID,codebook_2010$UNITID,codebook_2010$INSTNM)
admission_2010$College <- tidy_up_string_vector(admission_2010$College)

admission_2020 <- cbind(College=NA,admission_2020)
admission_2020$College <- soft_match(admission_2020$UNITID,codebook_2020$UNITID,codebook_2020$INSTNM)
admission_2020$College <- tidy_up_string_vector(admission_2020$College)

df <- cbind(df,Freshman_2010=NA)
df <- cbind(df,Freshman_2020=NA)
df <- cbind(df,Total_Pell_2010=NA)
df <- cbind(df,Total_Pell_2020=NA)
df <- cbind(df,Raw_Change_in_Pell=NA)

retrieve_freshman_2010 <- make_freshman_retriever(2010)
retrieve_freshman_2020 <- make_freshman_retriever(2020)

df$Freshman_2010 <- sapply(df$College,retrieve_freshman_2010)
df$Freshman_2020 <- sapply(df$College,retrieve_freshman_2020)

df$Total_Pell_2010 <- df$Freshman_2010 %>% multiply_by(df$Pell_In_2011 %>% divide_by(100))
df$Total_Pell_2020 <- df$Freshman_2020 %>% multiply_by(df$Freshmen_Pell_share %>% divide_by(100))
df$Raw_Change_in_Pell <- df$Total_Pell_2020 %>% subtract(df$Total_Pell_2010)

raw_change_table <- as.data.frame(matrix(ncol=3,nrow=4))
colnames(raw_change_table) <- c("Value_Of_Raw_Change_In_Pell","Num_Institutions","Percentage_Of_All_Institutions")

raw_change_table[1,]$Value_Of_Raw_Change_In_Pell <- "< 0"
raw_change_table[2,]$Value_Of_Raw_Change_In_Pell <- "= 0"
raw_change_table[3,]$Value_Of_Raw_Change_In_Pell <- "> 0"
raw_change_table[4,]$Value_Of_Raw_Change_In_Pell <- "Uncalculatable with Available Data"

raw_change_table[1,]$Num_Institutions <- length(which(df$Raw_Change_in_Pell < 0))
raw_change_table[2,]$Num_Institutions <- length(which(df$Raw_Change_in_Pell == 0))
raw_change_table[3,]$Num_Institutions <- length(which(df$Raw_Change_in_Pell > 0))
raw_change_table[4,]$Num_Institutions <- length(which(is.na(df$Raw_Change_in_Pell)))

raw_change_table$Percentage_Of_All_Institutions <- raw_change_table$Num_Institutions / length(df$College)

# Assertion by the New York Times: "over the past decade, ... most schools have seen their number of these students decrease."
# Percentage of institutions in Times' data that we can show to have decreased their raw numbers: 37%
# Largest group by raw numbers: Institutions that have *increased* their numbers (38%)
# It's possible that the missing institutions (24%) make this statement accurate, but given they're a random sampling
# of institutions with weird names that seems unlikely?
# And even if so, we've now brought in secondary datasets in a desperate attempt to prove this claim is true.
# Working only with the Times' own data, there is no way to show that this statement is accurate.

