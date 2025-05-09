export_data <- data |> 
  distinct(tank_text, dps_text, sup_text) |> 
  slice_sample(n = 70)

export_data$dive_score <- 0
export_data$brawl_score <- 0
export_data$spam_score <- 0

write.csv(export_data, file = "comps_to_classify_3.csv")
