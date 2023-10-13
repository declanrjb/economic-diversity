library(tidyverse)

source("functions.R")

df <- read_csv("raw_times_data.csv")
colnames(df) <- gsub(" ","_",str_squish(colnames(df)))

df <- cbind(Times_Rank=NA,df)
df$Times_Rank <- rownames(df)

df$Freshmen_Pell_share <- parse_number(df$Freshmen_Pell_share)
df <- cbind(df,Pell_In_2011=NA)
df$Pell_In_2011 <- df$Freshmen_Pell_share + -(df$Pell_change_since_2011)

admission_2010 <- read_csv("IPEDS/IC2010/ic2010.csv")
codebook_2010 <- read_csv("IPEDS/IC2010/hd2010.csv")

admission_2020 <- read_csv("IPEDS/ADM2020/adm2020.csv")
codebook_2020 <- read_csv("IPEDS/ADM2020/hd2020.csv")

state_abs_codebook <- read_csv("codebooks/times_state_abs_codebook.csv")

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

df <- cbind(df,Simpsons=NA)
for (i in 1:length(df$Simpsons)) {
  curr_total <- df[i,]$Freshman_2020
  curr_pell <- df[i,]$Total_Pell_2020
  df[i,]$Simpsons <- calculate_simpsons(curr_pell,curr_total)
}

df <- cbind(Simpsons_Rank=NA,df)

df <- df %>% arrange(-Simpsons)
df$Simpsons_Rank <- 1:length(df$Simpsons_Rank)
df$Simpsons_Rank[which(is.na(df$Simpsons))] <- NA

simpsons_df <- df %>% select(Preserved_Name,State,Type,Freshmen_Pell_share,Simpsons_Rank) %>% filter(!is.na(Simpsons_Rank))
colnames(simpsons_df) <- c("Name","State","Type","Freshmen_Pell_share","Ranking")
simpsons_df <- cbind(simpsons_df,Type_Of_Ranking="Simpsons")

times_df <- df %>% select(Preserved_Name,State,Type,Freshmen_Pell_share,Times_Rank) %>% filter(!is.na(Times_Rank))
colnames(times_df) <- c("Name","State","Type","Freshmen_Pell_share","Ranking")
times_df <- cbind(times_df,Type_Of_Ranking="Times")

flourish_rankings <- rbind(simpsons_df,times_df)
write.csv(flourish_rankings,"backup_data/flourish_rankings.csv",row.names=FALSE)

public_table <- df %>% select(Simpsons_Rank,Times_Rank,Preserved_Name,State,Type,Freshmen_Pell_share,Simpsons,Pell_change_since_2011,Total_undergraduates,Endowment_per_student,`Net_price,_mid-income`)
public_table$Simpsons <- public_table$Simpsons %>% multiply_by(100)
public_table <- cbind(public_table,Freshmen_Non_Pell_share=NA)
public_table$Freshmen_Non_Pell_share <- 100 - public_table$Freshmen_Pell_share
write.csv(public_table,"backup_data/public_table.csv",row.names=FALSE)

# export some data to flourish
curr_df <- df %>% select(College,Type,Freshmen_Pell_share,State)
colnames(curr_df) <- c("College","Type","Pell_Share","State")
curr_df <- cbind(curr_df,Year=NA)
curr_df$Year <- 2020

df_2011 <- df %>% select(College,Type,Pell_In_2011,State)
colnames(df_2011) <- c("College","Type","Pell_Share","State")
df_2011 <- cbind(df_2011,Year=NA)
df_2011$Year <- 2010

bound_df <- rbind(curr_df,df_2011)
bound_df$College <- str_to_title(bound_df$College)

write.csv(bound_df,"backup_data/flourish_pell-share-by-year.csv",row.names=FALSE)


