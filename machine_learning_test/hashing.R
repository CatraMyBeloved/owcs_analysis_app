data_hashing <- comps |>
  group_by(round_id) |>
  slice_sample(n = 1) |>
  ungroup()

data_hashing <- data_hashing |>
  rowwise() |>
  mutate(
    dps1 = dps[1], dps2 = dps[2],
    sup1 = sup[1], sup2 = sup[2],
    dps_opp1 = dps_opp[1],
    dps_opp2 = dps_opp[2],
    sup_opp1 = sup_opp[1],
    sup_opp2 = sup_opp[2]
  ) |>
  select(-dps, -sup, -dps_opp, -sup_opp)

data_hero_hashing <- data_hashing |>
  mutate(rowid = row_number()) |>
  select(
    tank, dps1, dps2, tank_opp, dps_opp1,
    dps_opp2, sup_opp1, sup_opp2, rowid
  ) |>
  pivot_longer(cols = !rowid, names_to = "position", values_to = "hero")

data_hero_hashing <- data_hero_hashing |>
  mutate(position = gsub("[[:digit:]]+", "", position)) |>
  rowwise() |>
  mutate(hashstring = paste(position, "=", hero))
