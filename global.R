### GLOBAL.R script for Ottawa Bike Counters shiny app // JA Moggridge

####  bike count & geo data input ############################################
bikes <- readRDS("./data/ottbike_counters.ts.RDS") %>%
  filter(Date >= "2010-01-28",
         Date <= "2019-09-30") %>%
  mutate(weekend = ifelse(is.weekend(Date), "Weekend", "Weekday"),
         weekend = fct_rev(weekend))





geo <- read.csv("./data/locations.csv")

locations <- c("Adawé", "Alexandra", "Canal Ritz", "Colonel By",
               "Laurier/Bay",  "Laurier/Lyon", "Laurier/Metcalfe",
               "OTrain/Bayview", "OTrain/Gladstone", "OTrain/Young",
               "Ottawa River", "Portage", "Somerset")

#### weather input ##################################

weather <- readRDS("./data/bike.weather.long.RDS") %>%
  filter(Date > ymd("2011-12-15"))  # where is 2010 data? wtf?


bike.weather <- right_join(bikes, weather, by="Date") %>%
  mutate(# Weather = ifelse(Precip>0, "Precip", "Dry"),
    Wind.deg = as.numeric(Wind.deg*10),
    Wind.speed = as.numeric(Wind.speed),
    Wind.speed = replace_na(Wind.speed, 29),
    # make Precip NA's into 0s and create levels for factor
    Precip = replace_na(Precip, replace = 0),
    Precipitation = case_when(
      Precip >= 10 ~ "+++",
      Precip >= 5 ~ "++",
      Precip > 0 ~ "+",
      Precip == 0 ~ "-",
      is.na(Precip) ~ "-"),
    Precipitation = fct_infreq(Precipitation)
  ) %>%
  # filter(count>=1) %>%
  filter(!is.na(count)) %>%
  filter(!is.na(Precip))

####  PCA dataframe input ####################################

### ran this code:
# library(broom)
# library(feasts)
# bikes_features <- bikes %>%
#   filter(location != 'Portage') %>%
#   features(count, feature_set(pkgs='feasts'))
# pcs <- bikes_features %>%
#   select(-c(1)) %>%
#   select_if(~ !any(is.na(.))) %>%
#   mutate_if(is.integer, as.numeric) %>%
#   select(-as.numeric(which(apply(bikes_features, 2, var)==0))) %>%
#   select(-c(pp_pvalue, bp_pvalue)) %>%
#   prcomp(scale=TRUE, na.action = na.pass) %>%
#   augment(bikes_features)
# saveRDS(bikes_features, "./data/bikes_pca.RDS")

pcs <- readRDS("./data/bikes_pca.RDS")

####  Themes  #################################

### HTML THEMES

# makes all the horizontal lines actually show up
hr <-  tags$head(tags$style(HTML("hr {border-top: 1px solid #000000;}")))
hr <-  tags$head(tags$style(HTML("
hr {
  margin-top: 10px;
  margin-bottom: 10px;
  border: 0;
  border-top: 1px solid #000000;
}"
)))

# col_discrete <- scale_colour_carto_d(name = "", type = 'qualitative', direction = 1)
# fill_discrete <- scale_fill_carto_d(name = "", type = 'qualitative', direction = 1)
basic_theme <- theme_void() + theme_tufte(base_size = 15,
                                          base_family = 'sans')

facet_labels <- theme(strip.text = element_text(face = 'bold', angle=90, colour = 'black', size = 12), #face = "bold"
                      strip.background = element_rect(fill = "white", colour = "white", size = 1))
no_facet_labs <- theme(strip.background = element_blank(), strip.text.x = element_blank())

legend_format <- theme(legend.text = element_text(size = 11)) #face = "bold",
base_x <- geom_hline(yintercept = -1, color= 'black')
base_y <- geom_vline(xintercept = 0, colour = 'black')
# log10x <- scale_x_continuous(trans='log10')



##### Links, Icons & stuff ###

whitespace <- HTML('&nbsp;&nbsp;&nbsp;&nbsp;')
bike.icon <- HTML('<i class="fa fa-bicycle fa-1x" aria-hidden="false"></i>')


jm.link <- tags$a(href="https://jmoggridge.github.io/", "J.Moggridge")
twitter.link <- tags$a(href="https://twitter.com/quaxlikeaduck", icon("twitter"))
github.link <- tags$a(href="https://github.com/jmoggridge/ottbike_counters", icon("github"))
city.link <- tags$a(href = "https://open.ottawa.ca/datasets/bicycle-trip-counters", 'open.ottawa.ca')
cc.icon <- HTML('<i class="fa fa-creative-commons" aria-hidden="true"></i>')
cc.link <- a(href ="https://creativecommons.org/licenses/by-sa/4.0/", 'CC-SA-4.0')
city.lic.link <- a(href = "https://ottawa.ca/en/city-hall/get-know-your-city/open-data#open-data-licence-version-2-0",
                   "Open Government Licence - City of Ottawa")

title.text <- HTML('Ottawa Bike Counters')
side.text <- h5("Dashboard by", jm.link, twitter.link, github.link,"|",
                cc.link, cc.icon,"| Source:", city.link,"|",
                em("Contains information licensed under the ",city.lic.link),

                    )


#### Tab captions ######

cap.ts <- p("Daily observations for total riders.
            Notes on quality of this dataset:")

cap.season <-
  p(br(), tags$b("Seasonal patterns in Ottawa bike traffic."),
    "* Reacts to chosen single location from sidebar; plots a & b show data within specified date range.",
    tags$b("(a)"),
    "Annual pattern: each line represents a year of daily bike counts at a counter,
   with colour by year (purple to yellow: 2010 to 2019).",
    # tags$b("(b)"),"Weekly pattern: each line is a week of daily counts",
    tags$b("(b)"),"Change in annual pattern: blue lines trace the mean daily bike count by month (panels)
    for each year (x-axis within each panel); horizontal black line shows the mean daily count across the
    entire period for that month.",
    tags$b("(c)"),"Change in weekly patterns: as for 'b', except with panels for each day-of-week.",
    br(),
    "*Select from 'single location' and 'date range'"
  )

cap.corr <-
  p(tags$b(
    "Scatterplots showing pairwise similarity between
    counter locations through shared traffic patterns. "),
    "Each plot compares counts from the same day
    at two locations: straight lines indicate that counts at the two locations plotted
    are highly correlated; conversely, diffuse clouds show that the counts  at each
    location are independent.", br(),
    "*This figure reacts to 'group selection' and 'date range'.")

ACF.link <- a(href="https://en.wikipedia.org/wiki/Autocorrelation", "Autocorrelation")
PACF.link <- a(href="https://en.wikipedia.org/wiki/Partial_autocorrelation_function", "partial autocorrelation functions")

cap.ACF <- p(
  p(tags$b(ACF.link, "(ACF) and", PACF.link, "(PACF) of bike counters"),
    "represent the correlation between observations in a time series
    and indicate the periodicity of variation in observed values.
    The height of each point represents how similar observations seperated by",
    tags$em("k"),"- days are in count size.
    Scalloped shapes with peaks at 7-day intervals indicates a strong weekly cycle; a flatter curve indicates that
    counts are more consistent over the period.
    Downward slope indicates that the trend is increasing over time.
    ", "This figure is reactive to the 'group selection' and 'date-range' filters on the sidebar."),

  p("Similar to ACF, PACF shows the relationship between observations separated in time by",
    tags$em("k"), "days, but after transformation to remove the influence of linear
    correlations in the interim period.
    PACF is useful for determining the suitability and order of an autoreggressive forecasting model.
    For more information about ACF and PACF, see: ",
    a(href="https://otexts.com/fpp3/acf.html",
      "'Forecasting: Principles and Practice'"), "by Hyndman and Athanasopoulos"),br(),
  h5("*This figure is reactive to the 'group selection'")

)

cap.STL <-
  p(tags$b("Seasonal and trend decomposition using Loess method (STL)."),
    "STL splits each time series into seasonal, trend and irregular components.
Grey bars (left) correspond to the relative scaling of each panel.
Computed with the 'feasts' package STL algorithm with robust default parameters.
", h5("*This plot is reactive to the 'Single location selector' on the sidebar"))

cap.weather <-
  p(tags$b("Relationships between daily bike counts and climate stressors"),
    " (temperature, precipitation, wind angle and max. gust speed) at selected locations.
    The lower limit of wind speed measurement is ~30 km/h,
observations plotted at 29 km/h represent calm days with low wind.
Precipitation boxplots' area are proportional to the number of days of data available; notches suggest
median counts are significantly different. Temperatures are daily means. Wind direction and speed are
taken from the maximum gust observed during the day. Climate observations from Ottawa Airport.", br(),
    "* Plots are reactive to selected inputs: input for the 'All' variables tab is selected using the
    dropdown menu (single/group/all locations) and reacts to the selections on the sidebar.
    Other tabs will show a panel for each location in the 'Group selection'.
    Data can be filtered with 'date range' slider. ")


cap.pca <-
  p(tags$b("Principle components analysis (PCA) of Ottawa bike counters"),
"calculated from 36 statistics for each time series using the", em("feasts"), "package for R.
PCA is a dimensionality reduction technique that projects data onto a plane that captures the maximum possible variation along
two principle axes (PC1 and PC2). Here, PCA allows use to visually evaluate the similarity of bike counters from how they group together.
We note that there appear to be three main clusters which generally correspond with geography but not entirely: the Laurier counters, the Canal/Ottawa river group and the O-train group.
Portage has been omitted from this analysis due to low quality data and general disparity with the other locations"
)

cap.dist <-
  p(tags$b("Distribution of daily bike count by location and weekday/end."),
    "Notches in the boxplot that don't overlap suggest significantly different median counts. Boxplot points
    indicate min/max counts and outliers. Boxplot area has been scaled to the number of days measured at each location.")




#### recycling bin #####




# date.range <-   dateRangeInput("date.range", p(tags$b("Date range:"), "min 2010-01-28 | max 2019-09-30"),
#                                start  = "2010-01-28",   end = "2019-09-30",
#                                min    = "2010-01-28",   max = "2019-09-30",
#                                format = "yyyy/mm/dd",   separator = " - ",
#                                startview = "decade")



# ### heatmap ##
# output$heatmap <- renderPlot({
#     bike.matrix <- bikes %>% as_tibble() %>%
#         mutate(location = as.factor(location)) %>%
#         tidyr::pivot_wider(id_cols = "Date",
#                            names_from = "location",
#                            values_from = 'count') %>%
#         # added this to remove early years with no data
#         filter(Date > ymd("2016-01-01")) %>%
#         mutate_if(is.character, as.numeric) %>%
#         # make sure date(col #1) isn't in matrix !!! OR ELSE!!
#         #  (must remove) 6,7 is laurier/bay+lyon; 11,13,14 are young/gladst/portage
#         select(c(2:5, 8:10, 12, 14)) %>%
#         as.matrix()
#
#     colr <- viridis_pal(alpha = 0.9, begin = 0, end = 1, direction = 1,
#                         option = input$drop.colours)(1000)
#     b.matrix <- rcorr(bike.matrix)
#     heatmap.2(b.matrix$r, scale="none", # 'none' because matrix is symmetric
#               trace='none', col = colr,
#               distfun = function(x) dist(x, method = "euclidean"),
#               hclustfun = function(x) hclust(x, method = "average"),   ### hclust methods: average:UPGMA; single:NN; centroid, median,
#               dendrogram = c("row"),
#               symm = TRUE,
#               key = FALSE,
#               # key.title = "Distance",
#               density.info = "none",
#               margins = c(10, 12)) + basic_theme # use margins to avoid cutting off labels
# })


# output$table.acf <- DT::renderDataTable(DT::datatable({
#     bikes %>%
#         features(count, feat_acf) %>%
#         mutate_if(is.numeric, round, 3)},
#     rownames = FALSE,
#     options = list(paging = FALSE, autoWidth = TRUE, searching=FALSE))
# )

# per2<- period.df %>%
#     gg_season(alpha = 0.1, size = 1, period = "week") +
#     basic_theme + geom_rangeframe(colour='black') +
#     scale_color_viridis_c() +
#     theme(legend.position = 'none',
#           axis.text.y  = element_blank(),
#           axis.title.y = element_blank(), axis.title.x = element_blank(),
#           axis.text.x = element_text(angle = 35, size=11, hjust = 1, vjust = 1),
#           panel.grid.major.y = element_line(colour = "grey", size = 0.2),
#           plot.margin = margin(0.5, 0, 0.1, 0, "cm")
#     )
# tab.clust <- tabPanel(tags$b("Clustering"),
#                       plotOutput("heatmap", height = '500px', width = '100%'),
#                       cap.clust, hr()
# )


# cap.clust <-
#   p(tags$b("Hierarchical clustering of bike counter locations by similarity in daily counts since 2016."),
#     "Average-linkage clusters from normalized observations by unweighted paired group method with arithmetic mean (UPGMA).
#     Interestingly, locations do not all cluster according to proximity, but perhaps also by seasonal usage patterns (annual/hebdo).",
#     h5("This plot is not reactive to selected inputs as several counters do not have concurrent or sufficient data."))
# cap.arima <- p("An ARIMA forecast model for each bike counter that was operational in 2019")



## Basic plot lines  for count threshold
# threshold <- geom_hline(yintercept = c(100,200), linetype = "longdash", color = "black", size = 0.3, alpha = 0.7)
# thresholdv <- geom_vline(xintercept=c(100, 200), linetype="longdash", color = "black", size = 0.3, alpha = 0.7)

# for timeseries first of months' lines
# first_of_month <- geom_vline(xintercept = c(182, 213, 242), color = "darkgrey", alpha = 0.3, size = 2.5)
# annot <- data.frame(x=c(170,200,230), y=seq(1015,1015,3), label = c('June', 'July', 'Aug'))

