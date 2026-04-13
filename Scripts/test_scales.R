library(dplyr)
df <- readRDS('../Final_Data/final_data_set.rds')
yearly <- df |> group_by(year) |> summarize(
  mean_gdp = mean(gdp_change_percent, na.rm=T),
  mean_exp = mean(log_total_program_expenses, na.rm=T),
  avg_raw_exp = mean(total_program_expenses, na.rm=T)
)
print(yearly)
