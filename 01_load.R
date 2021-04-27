library(here)
source("libraries.R")
source("functions.R")

path <- "//umms-HDS629-win.turbo.storage.umich.edu/umms-HDS629/MIPACT/HealthKit_Live"

diag <- read_csv(glue("{path}/EHR/EHR_Diagnosis_202010.csv"), col_types = cols()) %>%
  clean_names() %>%
  rename(
    id = participant_research_id
  )

demo <- read_csv(glue("{path}/EHR/EHR_Demographic_202010.csv"), col_types = cols()) %>%
  clean_names() %>%
  select(
    id = participant_research_id,
    age = age_at_enrollment,
    gender = gender_name,
    marital_status = marital_status_name,
    race = race_name,
    ethnicity = ethnicity_name
  )

pheno <- read_csv(glue("{path}/EHR/EHR_Phenotypes_202010.csv"),
                  col_types = cols()) %>%
  clean_names() %>%
  rename(
    id = participant_research_id,
    date = visit_activity_date
  )

tmp <- pheno %>%
  select(id, h_comp = hypertension_complicated, h_unc = hypertension_uncomplicated) %>%
  group_by(id) %>%
  summarize(
    h_comp = sum(h_comp),
    h_unc  = sum(h_unc)
  )
  
sum(as.numeric(tmp$h_comp > 0))

dat <- pheno %>%
  select(-pheno_type_group) %>%
  pivot_longer(
    names_to = "phenotype",
    values_to = "value",
    cols = -c("id", "date")
  ) %>%
  filter(value == 1) %>%
  select(-date) %>%
  distinct() %>%
  pivot_wider(
    names_from = "phenotype",
    values_from = "value",
    values_fill = 0
  ) %>%
  # mutate_all(factor) %>%
  left_join(demo,
            by = "id") %>% 
  drop_na()

tmp <- pheno %>%
  select(-pheno_type_group) %>%
  mutate(
    hypertension = ifelse(
      hypertension_uncomplicated == 1 | hypertension_complicated == 1,
      1, 0
    )
  ) %>%
  select(-c(hypertension_uncomplicated, hypertension_complicated)) %>%
  pivot_longer(
    names_to = "phenotype",
    values_to = "value",
    cols = -c("id", "date")
  ) %>%
  filter(value == 1)

case_sub <- tmp %>%
  group_by(id) %>%
  filter(phenotype == "hypertension") %>%
  filter(date == min(date)) %>%
  ungroup()
case_ids <- case_sub %>% pull(id) %>% unique()

cases <- tmp %>% filter(id %in% case_ids)
noncases <- tmp %>% filter(!(id %in% case_ids))
cases_restrict <- cases %>%
  left_join(
    case_sub %>% select(id, case_date = date), by = "id"
  ) %>%
  filter(date <= case_date - 365)

restricted <- bind_rows(cases_restrict, noncases)

prepped <- restricted %>%
  select(-c(date, case_date)) %>%
  distinct() %>%
  pivot_wider(
    names_from = "phenotype",
    values_from = "value",
    values_fill = 0
  ) %>%
  mutate_all(factor) %>%
  left_join(demo %>%
              mutate(
                id = factor(id)
              ),
            by = "id") %>% 
  drop_na() %>%
  mutate(
    case = factor(as.numeric(id %in% case_ids))
  )

write_tsv(prepped, here("project_2", "data", "restricted.txt"))

unrestricted <- dat %>%
  dplyr::mutate(
    case = ifelse(id %in% case_ids, 1, 0)
  ) %>%
  dplyr::select(-c(hypertension_uncomplicated, hypertension_complicated))
write_tsv(unrestricted, here("project_2", "data", "unrestricted.txt"))
