library(janitory)

tmp_n <- unrestricted %>% filter(case == 0)

summary(tmp_n$age)
tabyl(tmp_n$race, show_missing_levels = TRUE)
tabyl(tmp_n$ethnicity, show_missing_levels = TRUE)
tabyl(tmp_n$gender, show_missing_levels = TRUE)
tabyl(tmp_n$marital_status, show_missing_levels = TRUE)

rest_n <- prepped %>% filter(case == 1)

summary(rest_n$age)
tabyl(rest_n$race, show_missing_levels = TRUE)
tabyl(rest_n$ethnicity, show_missing_levels = TRUE)
tabyl(rest_n$gender, show_missing_levels = TRUE)
tabyl(rest_n$marital_status, show_missing_levels = TRUE)
rest_n_ids <- unique(rest_n$id)

unre_n <- unrestricted %>% filter(case == 1 & !(id %in% rest_n_ids))
summary(unre_n$age)
tabyl(unre_n$race, show_missing_levels = TRUE)
tabyl(unre_n$ethnicity, show_missing_levels = TRUE)
tabyl(unre_n$gender, show_missing_levels = TRUE)
tabyl(unre_n$marital_status, show_missing_levels = TRUE)
