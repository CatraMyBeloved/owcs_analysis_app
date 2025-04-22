hero_features <- hero_features |>
  mutate(composition = case_when(
    has_Winston & has_Ana ~ "Dive",
    has_Winston & has_Kiriko ~ "Dive",
    has_Hazard & has_Juno ~ "Hybrid",
    has_Hazard & has_Kiriko ~ "Hybrid",
    has_Ramattra & has_Lucio ~ "Brawl",
    has_Ramattra & has_Brig ~ "Brawl",
    has_Sigma ~ "Poke",
    has_DVa & has_Torbjorn ~ "Poke",
    has_Mauga ~ "Brawl",
    has_DVa & has_Juno ~ "Hybrid",
    has_Torbjorn & has_Symmetra ~ "Poke",
    .default = "Other"
  ))

hero_features |>
  filter(composition == "Other") |>
  count(heroes_played) |>
  arrange(desc(n))

hero_features |>
  count(composition)


hero_features_labeled <- hero_features |>
  filter(composition != "Other") |>
  mutate(composition = as.factor(composition))

hero_features_unlabeled <- hero_features |>
  filter(composition == "Other")
