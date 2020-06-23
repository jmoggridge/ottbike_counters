# Show in New WindowClear OutputExpand/Collapse Output
# [1] "Adawé"            "Alexandria"       "Canal Ritz"
# [4] "Colonel By"       "Laurier/Bay"      "Laurier/Lyon"
# [7] "Laurier/Metcalfe" "OTrain/Bayview"   "OTrain/Gladstone"
# [10] "OTrain/Young"     "Ottawa river"     "Portage"
# [13] "Somerset"


bikes.ts %>%
  gg_season(alpha = 0.5) +
  ylab("Daily count") + xlab("Date") + ggtitle("All bike counters, all years")



laurier <- c("Laurier/Bay", "Laurier/Lyon", "Laurier/Metcalfe")
bikes.ts %>%
  filter(location %in% laurier) %>% fill_gaps() %>%
  gg_season(alpha = 0.83, size = 0.5) +
  ylab("Daily rides") + xlab("Date") + ggtitle("#Ottbike counters: Laurier at Bay, Lyon, and Metcalfe")


bikes.ts %>%
  fill_gaps() %>%
  filter(location %in% c("Adawé", "Alexandria", "Canal Ritz",
                         "Colonel By", "Laurier/Metcalfe","OTrain/Bayview",
                         "OTrain/Gladstone", "Ottawa River")) %>%
  gg_season(alpha = 0.5) +
  ylab("Daily count") + xlab("Date") + ggtitle("OttBike Counters: seasonality")


bikes.ts %>%
  fill_gaps() %>%
  filter(location %in% c("Canal Ritz", "Colonel By")) %>%
  gg_season(alpha = 0.5) +
  ylab("Daily count") + xlab("Date") + ggtitle("OttBike Counters: seasonality")


bikes.ts %>%
  fill_gaps() %>%
  filter(location %in% c("Canal Ritz")) %>%
  gg_season(alpha = 0.5, period = 'week') +
  ylab("Daily count") + xlab("Date") + ggtitle("OttBike Counters: seasonality")


bikes.ts %>%
  fill_gaps() %>%
  filter(location %in% c("Laurier/Metcalfe")) %>%
  gg_season(alpha = 0.5, period = 'week') +
  ylab("Daily count") + xlab("Date") + ggtitle("OttBike Counters: weekly")



bikes.ts %>%
  fill_gaps() %>%
  filter(location %in% c("Ottawa river", "Laurier/Metcalfe",
                         "Alexandria")) %>%
  gg_season(alpha = 0.5, period = 'year') +
  ylab("Daily count") + xlab("Date") + ggtitle("OttBike Counters: seasonal trend")

bikes.ts %>%
  fill_gaps() %>%
  filter(location %in% c("Ottawa river", "Laurier/Metcalfe",
                         "Alexandria")) %>%
  gg_season(alpha = 0.5, period = 'week') +
  ylab("Daily count") +
  ggtitle("OttBike Counters: weekly trend") +
  theme(axis.text.x = element_text(angle = 25, vjust = 0.8),
        axis.title.x = element_blank())




bikes.ts %>%
  fill_gaps() %>%
  filter(location %in% c("Ottawa river")) %>%
  gg_season(alpha = 0.5, period = 'year') +
  ylab("Daily count") + xlab("Date") + ggtitle("OttBike Counters: weekly")




cor(bikes[, c(-1)])


b2 <- bikes %>%
  filter(location %in% locations) %>%
  as_tibble() %>%
  mutate(location = as.factor(location)) %>%
  pivot_wider(id_cols = "Date",
              names_from = "location",
              values_from = 'count') %>%
  select(-c(1)) %>%
  mutate_if(is.character, as.numeric)

