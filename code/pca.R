library(broom)
library(feasts)
library(tidyverse)

bikes <- readRDS("./data/ottbike_counters.ts.RDS")
bikes_features <- bikes %>%
  filter(location != 'Portage') %>%
  features(count, feature_set(pkgs='feasts'))

# remove all columns with NA's?
pcs <- bikes_features %>%
  select(-c(1)) %>%
  select_if(~ !any(is.na(.))) %>%
  mutate_if(is.integer, as.numeric) %>%
  select(-as.numeric(which(apply(bikes_features, 2, var)==0))) %>%
  select(-c(pp_pvalue, bp_pvalue)) %>%
  prcomp(scale=TRUE) %>%
  augment(bikes_features)

pcs %>%
  ggplot(aes(x=.fittedPC1, y=.fittedPC2)) +
  geom_point() +
  geom_text_repel(aes(label=location)) +
  # theme(aspect.ratio=1) +
  labs(title= "Principle components analysis of Ottawa bike counters",
       subtitle = "calculated from 36 features of each time series ")

saveRDS(pcs, "./data/bikes_pca.RDS")
