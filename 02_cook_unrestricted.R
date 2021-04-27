library(here)
library(tidymodels)
library(vip)
library(rpart.plot)

set.seed(123)

unrestricted <- read_tsv(here("project_2", "data", "unrestricted.txt"),
                       col_types = cols()) %>%
  mutate(
    case = factor(case_when(
      case == 1 ~ "case",
      case == 0 ~ "control"
    ))
  )

data_split <- unrestricted %>%
  select(-id) %>%
  rsample::initial_split(
    data = ,
    prop = 0.5,
    strata = case
  )

train_data <- training(data_split)
test_data  <- testing(data_split)

folds <- vfold_cv(
  data = train_data,
  v    = 10,
  repeats = 5,
  strata = case
)

my_recipe <- recipe(case ~ ., data = train_data)

rpart_mod <- decision_tree(
  cost_complexity = tune(),
  tree_depth      = tune(),
  min_n           = tune()
) %>%
  set_engine("rpart") %>%
  set_mode("classification")

rpart_grid <- grid_latin_hypercube(
  cost_complexity(),
  tree_depth(),
  min_n(),
  size = 5
)

rpart_wf <- workflow() %>%
  add_model(rpart_mod) %>%
  add_recipe(my_recipe)

tree_res <- rpart_wf %>%
  tune_grid(
    resamples = folds,
    grid      = rpart_grid,
    metrics   = metric_set(roc_auc)
  )

tree_res %>%
  collect_metrics()

tree_res %>% show_best()

best_rpart_params <- tree_res %>%
  select_best()

final_wf <- rpart_wf %>%
  finalize_workflow(best_rpart_params)

final_fit <- final_wf %>%
  last_fit(data_split)

final_fit

final_fit %>%
  collect_metrics()

rpart_fit <- final_wf %>%
  fit(data = train_data)

best_rpart <- rpart_fit %>%
  pull_workflow_fit()

best_rpart

vip(best_rpart)
ggsave(here("project_2", "fig", "unrestricted_vip.pdf"), width = 7, height = 5)

rpart.plot(
  x = best_rpart$fit,
  yesno = 2,
  type = 0,
  extra = 0
)


roc_curves <- final_fit %>%
  collect_predictions() %>%
  roc_curve(case, .pred_case)

roc_curves %>%
  ggplot(aes(x = 1- specificity, y = sensitivity)) +
  annotate(geom = "text", label = paste0("AUC: ", final_fit %>% collect_metrics() %>% filter(.metric == "roc_auc") %>% pull(.estimate) %>% round(., 4)),
           x = 0.75, y = 0.25) +
  geom_abline(linetype = 2, color = "gray40") +
  geom_line(size = 1) +
  labs(
    title = "Unrestricted data"
  )
ggsave(here("project_2", "fig", "unrestricted_roc_plt.pdf"), width = 5, height = 5)
