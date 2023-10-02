df$Endowment_per_student <- parse_number(df$Endowment_per_student)
df$`Net_price,_mid-income` <- parse_number(df$`Net_price,_mid-income`)

df <- cbind(df,Total_Pell_Students=NA)
df$Total_Pell_Students <- df$Total_undergraduates * (df$Freshmen_Pell_share / 100)

df <- cbind(df,Endowment=NA)
df <- cbind(df,Endowment_Per_Pell_Student=NA)
df$Endowment <- df$Endowment_per_student * df$Total_undergraduates
df$Endowment_Per_Pell_Student <- df$Endowment / df$Total_Pell_Students