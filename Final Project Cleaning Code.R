library(dplyr)
library(babynames)

babynames_grouped <- babynames |>
  group_by(name, year) |>
  summarise(
    total_n = sum(n),
    female_n = sum(ifelse(sex == "F", n, 0)),
    .groups = "drop"
  ) |>
  mutate(
    female_share = female_n / total_n,
    dist = abs(female_share - 0.5)
  )

saveRDS(babynames_grouped, "babynames_grouped.rds")
