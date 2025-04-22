hero_features <- hero_features |>
  mutate(composition = case_when(
    has_Winston & has_Ana & has_Brig ~ "Dive",
    has_Mauga & has_Juno & has_Brig ~ "Brawl",
    has_Winston & has_Lucio & has_Kiriko ~ "Dive",
    has_Sigma & has_Pharah & has_Illari ~ "Poke",
    has_Ramattra & has_Mei ~ "Brawl",
    has_Genji & has_Tracer & `has_Wrecking Ball` ~ "Dive",
    has_Sojourn & has_Pharah & !has_Mauga ~ "Poke",
    has_Sigma & has_Zen & has_Baptiste ~ "Poke",
    `has_Wrecking Ball` & has_Ana & has_Venture ~ "Dive",
    has_Ramattra & has_Juno & has_Brig ~ "Brawl",
    has_Winston & has_Ana & has_Zen ~ "Dive",
    has_Kiriko & has_Lucio & has_Hazard ~ "Brawl",
    has_Doomfist & has_Ana ~ "Dive"
  ))

hero_features_labeled <- hero_features |>
  filter(!is.na(composition)) |>
  mutate(composition = as.factor(composition))

hero_features_unlabeled <- hero_features |>
  filter(is.na(composition))

unlabeled_sample <- sample_n(hero_features_unlabeled)
