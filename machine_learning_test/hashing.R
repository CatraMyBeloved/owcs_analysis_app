library(digest)

num_buckets <- 32

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
  ungroup() |>
  mutate(rowid = row_number()) |>
  ungroup() |>
  select(
    tank, dps1, dps2, tank_opp, dps_opp1,
    dps_opp2, sup_opp1, sup_opp2, rowid
  ) |>
  pivot_longer(cols = !rowid, names_to = "position", values_to = "hero")

data_hero_hashing <- data_hero_hashing |>
  mutate(position = gsub("[[:digit:]]+", "", position)) |>
  rowwise() |>
  mutate(hashstring = paste(position, "=", hero))

data_hero_hashing <- data_hero_hashing |>
  mutate(
    # Create a hash value (using xxhash32 for speed)
    hash_value = digest(hashstring, algo = "xxhash32"),
    numeric_hash = as.numeric(paste0("0x", hash_value)),
    bucket = as.integer(numeric_hash %% num_buckets)
  ) |>
  ungroup()

bucket_init <- replicate(64, 0, simplify = FALSE)

bucket_vals_init <- data.frame()
bucket_vals_init <- data.frame()

for (rowid in unique(data_hero_hashing$rowid)) {
  bucket_values <- replicate(num_buckets, 0, simplify = FALSE)
  filtered_data <- data_hero_hashing[data_hero_hashing$rowid == rowid, ]

  for (i in 1:nrow(filtered_data)) {
    row_values <- filtered_data[i, ]
    bucket_idx <- row_values$bucket + 1
    bucket_values[[bucket_idx]] <- bucket_values[[bucket_idx]] + 1
  }

  bucket_row <- data.frame(t(unlist(bucket_values)))
  bucket_row$rowid <- rowid
  bucket_vals_init <- bind_rows(bucket_vals_init, bucket_row)
}

data_with_heroes_hashed <- bind_cols(
  data_hashing |>
    select(match_map_id, round_id, team_name, team_name_opp, iswin),
  bucket_vals_init
)
