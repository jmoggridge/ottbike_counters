### Heapmap for bike counts( col=count ~ year *facet(location))
bikes %>%
  mutate(yday = yday(Date),
         year = format(Date, "%Y"))
ggplot()

#what is earliest non-zero obs?

min.date <- bikes %>% as_tibble() %>%
  filter(count>0)
min(min.date$Date)
sum(bikes$count, na.rm = TRUE)
# last non-zero obs
max(min.date$Date)


ggplot(bikes)+scale_colour_viridis_c() +guides(fill=guide_colorbar())

### some decent weather panels ####

bike.weather %>%
  ggplot(aes(x=Wind.deg, y=Tmean)) +
  geom_jitter(size=0.1, shape=1, alpha=0.2, height = 0.5,width=5) +
  geom_point(aes(x=0,y=0), size = 0.78, colour='steelblue')+
  geom_density2d() +
  geom_smooth(color='red4') +
  coord_polar() + basic_theme +geom_rangeframe()

bike.weather %>% filter(Wind.speed>30) %>%
  ggplot(aes(x=Wind.deg, y=Wind.speed-30)) +
  geom_jitter(size=0.1, shape=1, alpha=0.2, height = 0.5,width=5) +
  geom_density2d() +
  geom_smooth(color='red4') +
  geom_point(aes(x=0,y=0), size = 0.78, colour='steelblue')+
  coord_polar() + basic_theme +geom_rangeframe()

bike.weather %>%
  ggplot(aes(x=Wind.deg, y=log(Precip))) +
  geom_jitter(size=0.1, shape=1, alpha=0.2, height = 0.5,width=5) +
  geom_density2d() +
  geom_smooth(color='red4') +
  geom_rangeframe() +
  geom_point(aes(x=0,y=log(0)), size = 0.78, colour='green')+
  coord_polar() + basic_theme


bike.weather %>%
  ggplot(aes(x=Wind.deg, y=Tmean)) +
  geom_jitter(size=0.3, shape=1, alpha=0.4, height = 0.5,width=5) +
  geom_point(aes(x=0,y=0), size = 0.78, colour='steelblue')+
  geom_density2d() +
  geom_smooth(color='red4')+ basic_theme +geom_rangeframe()
#

bike.weather %>% filter(Wind.speed>30) %>%
  ggplot(aes(x=Wind.deg, y=Wind.speed-30)) +
  # geom_jitter(size=0.3, shape=1, alpha=0.4, height = 1.2, width=5) +
  geom_density2d() +
  geom_smooth(color='red4') +
  geom_point(aes(x=0,y=0), size = 0.78, colour='steelblue')+ basic_theme +geom_rangeframe()


bike.weather %>%
  ggplot(aes(x=Wind.deg, y=log(Precip))) +
  geom_jitter(size=0.3, shape=1, alpha=0.4, height = 0.5,width=5) +
  geom_density2d() +
  geom_smooth(color='red4') +
  geom_rangeframe() +
  geom_point(aes(x=0,y=log(0)), size = 0.78, colour='green')+ basic_theme +geom_rangeframe()

weather %>%
  mutate(Precip = log(Precip)) %>%
  select(c(2,5,6,7)) %>%
  plot()

####### some random stuff #
bikes %>%
  filter(location == 'Ottawa River') %>%
  filter(year(Date)>=2016) %>%
  group_by(year, )
  gg_lag(count, geom = 'point', size =0.5, alpha = 0.7,
         arrow = TRUE) # + basic_theme


bikes%>%
  ggplot()+
  geom_bar(aes(x=as.factor(Date,y=count), stat='identity')) +
  facet_grid(location~.)

facf <- bikes %>% features(count, feat_acf)

 bike.weather %>%
   filter(location %in% locations[-c(5,6,10,12)]) %>%
   ggplot(aes(x=count, colour = location)) +geom_density()


wday.df <- as_tibble(bikes) %>%
  filter(count > 0) %>%
  mutate(wday = as.factor(wday(Date, label=TRUE)),
         year = format(Date, "%Y")) %>%
  group_by(year, wday) %>%
  summarise(mean = mean(count))
wday.means <- as_tibble(bikes) %>%
  filter(count > 0) %>%
  mutate(wday = as.factor(wday(Date, label=TRUE))) %>%
  group_by(wday) %>%
  summarise(avg = mean(count))

ggplot(wday.df, aes(x=as.numeric(year), y=mean))+
  geom_hline(data = wday.means, aes(yintercept = avg)) +
  geom_path(size = 1, alpha = 0.8, colour = 'dodgerblue') +
  # scale_x_continuous(breaks = c(2010, 2015, 2019)) +
  # xlim(function(x){})
  labs(x = "Year", y = "Mean daily count") +
  facet_grid(. ~ wday) +
  theme_light() +
  theme(text = element_text(family = 'Century', size = 14),
        axis.text.x = element_text(angle = 90, size = 10, vjust = 0.5))
# ::patchwork




annual.df <- as_tibble(bikes) %>%
  mutate(year = format(Date, '%Y')) %>%
  group_by(location, year) %>%
  summarise(sum = sum(count)) %>%
  ungroup()
ggplot(annual.df, aes(x=year, y=sum, colour = location, group = location)) +
  geom_point()+
  geom_path() #+ geom_smooth(se = FALSE)

weather <- readRDS("data/bike.weather.long.RDS") %>%
  filter(Date > ymd("2011-12-15"))

bike.weather <- right_join(bikes,weather, by="Date") %>%
  mutate(Weather = ifelse(Precip>0, "Precip", "Dry"),
         Wind.deg = as.numeric(Wind.deg*10)) %>%
  filter(count>0)

bike.weather %>% #filter(location == 'Ottawa River') %>%
ggplot(aes(x = Wind.deg, y = count)) +
  geom_jitter(width=4, size =0.1) +
  geom_smooth(se = FALSE, size =2) +
  geom_point(aes(x=0,y=0), size =2, colour='white')+
  ylim(c(0,3000)) +
  # geom_tufteboxplot(median.type = "line", hoffset = 0, width=3) +
  ylab("Daily riders") + xlab("Wind direction ˚") +
  geom_rangeframe() + coord_polar() + #scale_y_log10() +
  basic_theme + geom_rangeframe() +
  theme(axis.text.x = element_blank())

bike.rain <- right_join(bikes,weather, by="Date") %>%
  mutate(Precip = replace_na(Precip, replace = 0)) %>%
  mutate(Precipitation = case_when(
    Precip >= 20 ~ "20 +",
    Precip >= 5 ~ "5 -",
    Precip >= 1 ~ "1 -",
    Precip < 1 ~ "0 -",
    is.na(Precip) ~ "0"
    )) %>%
  mutate(Precipitation = fct_infreq(Precipitation)) %>%
  filter(count>=1) %>%
  filter(!is.na(count)) %>%
  filter(!is.na(Precip))

# mutate(sleep_total_discr = case_when(
#   sleep_total > 13 ~ "very long",
#   sleep_total > 10 ~ "long",
#   sleep_total > 7 ~ "limited",
#   TRUE ~ "short"))

# violin for each location
ggplot(bike.rain, aes(x=location, y=count))+
  geom_violin(draw_quantiles = c(0.5), trim=TRUE, scale='area') +
  ylab("Daily riders") +
  geom_rangeframe() + basic_theme +

  theme(axis.title.x = element_blank(),
        axis.text.x = element_text(angle = 60, size=10, vjust = 1,
                                   hjust=1))

## tufte bar -rain
ggplot(bike.rain, aes(x = Precipitation, y=count)) +
  geom_tufteboxplot(median.type = "line", hoffset = 0, width=3) +
  ylab("Daily riders") + xlab("Precipitation (mm)") +
  geom_rangeframe() + facet_wrap(~location) + basic_theme +
  theme(axis.text.x = element_text(angle = 1, size=9))

## rain xy
ggplot(bike.rain, aes(x = Precip+1, y=count)) +
  geom_jitter(size =0.1, alpha=0.3, width=0.05) + geom_smooth() +
  scale_x_log10() + ylab("Daily riders") +xlab("Precipitation (mm)") +
  geom_rangeframe() + facet_wrap(~location) + basic_theme +
  theme(axis.text.x = element_text(angle = 1, size=10, vjust = 1))

## bike temp
ggplot(bike.weather, aes(x=Tmean, y=count)) +
  geom_point(size = 0.01, alpha =0.6) + geom_smooth(size=1) +
  ylab("Daily riders") + xlab("Mean temperature (˚C)") +
  geom_rangeframe() + facet_wrap(~location) + basic_theme

  # geom_jitter(size=0.1) +

ggplot(bike.weather, aes(x=Tmin, y=count)) +
  geom_point(size = 0.01) + geom_smooth()
# geom_jitter(size=0.1) +
ggplot(bike.weather, aes(x=Tmax, y=count)) +
  geom_point(size = 0.01) + geom_smooth()

ggplot(bike.weather, aes(x=yday(Date), y=log(count+1))) +
  geom_smooth() +
  geom_smooth(aes(y=Tmean)) + coord_polar()




 ### name
##########  stl  ################
#### STL


us_retail_employment %>%
  model(STL(Employed ~ trend(window=7) + season(window='periodic'),
            robust = TRUE)) %>%
  components() %>%
  autoplot()


x11_dcmp <- us_retail_employment %>%
  model(x11 = feasts:::X11(Employed, type = "additive")) %>%
  components()
autoplot(x11_dcmp) + xlab("Year") +
  ggtitle("Additive X11 decomposition of US retail employment in the US")

library(zoo)
bikes %>%
  filter(location == 'Somerset') %>%
  filter(!is.na(count)) %>%
  filter(Date > ymd('2015-01-01')) %>%
  as_tsibble() %>% fill_gaps() %>%
  mutate(count = na.approx(count)) %>%
  model(STL(count ~ season(window=13) + season(window='periodic'),
            robust = TRUE)) %>%
  components() %>%
  autoplot() + basic_theme
 ### stl

###### pca ########

### PCA

us_arrests_pca <- us_arrests %>%
  nest() %>%
  mutate(pca = map(data, ~ prcomp(.x %>% select(-state),
                                  center = TRUE, scale = TRUE)),
         pca_aug = map2(pca, data, ~augment(.x, data = .y)))

us_arrests_pca


pca.table <- bikes %>% as_tibble() %>%
  mutate(location = as.factor(location)) %>%
  drop_na() %>%
  filter(Date > ymd("2016-01-01")) %>%
  tidyr::pivot_wider(id_cols = "location",
                     names_from = "Date",
                     values_from = 'count') %>%
  mutate_if(is.character, as.numeric) %>%
  filter(row_number() %in% c(1:4, 7:9, 11, 13))
# %>%
#   drop_na()

# pca.table <- na.omit(pca.table)
pca.table %>%
  nest(data = everything()) %>%
  mutate(pca = map(~ prcomp(.x %>% select(-c(location)),
                                  center = TRUE, scale=TRUE)),
         pca_aug = map2(pca, data, ~broom::augment(.x, data= .y)))

iris.pca <- iris %>%
  nest()



  ## pca attempt
###########  x11  ###############
#
# model(x11 = feasts:::X11(sum, type = "additive", period = 12)) %>%
#   components()
# autoplot(x11.bike)

#
#
#
# test <- bikes %>% as_tibble() %>%
#   filter(location == "Alexandra") %>%
#   filter(count > 0) %>%
#   mutate(month = month(Date, label = TRUE), year = format(Date, "%Y")) %>%
#   group_by(year, month) %>%
#   summarise(avg = mean(count, na.rm = TRUE),
#             sum = sum(count, na.rm = TRUE))
# #
# test2 <- test %>%
#   group_by(month) %>%
#   summarise(month = as.factor(month),
#             avg = mean(avg, na.rm = TRUE))
# #
# ggplot(test, aes(x=as.numeric(year), y = avg)) +
#   geom_hline(data=test2, aes(yintercept = avg)) +
#   geom_path(size = 1, alpha = 0.8, colour = 'steelblue') +
#   # geom_smooth(method = loess, size = 0.8, colour = 'red2', alpha = 0.8, se = FALSE) +
#
#   scale_x_continuous(breaks = c(2010, 2015, 2019)) +
#   labs(x = "Year", y = "Monthly riders") +
#   facet_grid(.~month) +
#   theme(text = element_text(family = 'Century'),
#         axis.text.x = element_text(angle = 60, size=7, vjust = 0.5))
# #
#
#
#
#
#
# yr <- as.Date(ymd(paste(2000, '01-01')))
# class(yr)
#
#
# ## pairwise scatter with year?
#
#
#


# grid.arrange(cycle1, cycle2, nrow = 2,
#              top= grid::textGrob(title, x = 0, hjust = 0)
# grid.arrange(cycle1, cycle2, nrow = 2,
#              top=textGrob(title, hjust = 0, gp=gpar(fontsize=20)))



#     output$scatter <- renderPlot({
#         scat.df <- scatInput() %>% mutate(year = as.factor(format(Date, "%Y")))
#         (colr.df <- with(scat.df,
#                          year = levels(year),
#                          pal = I(viridis_pal(alpha = 1, begin = 0, end = 0.9, direction = 1,
#                             option = "D")(10))
#                          ))
#         scat.df <- select(scat.df, c(year)) %>%
#             mutate(matchRetVal = match(scat.df$year, colr.df$year))
#
#         pairs(s, pch=19, cex=0.2,
#               col = colr.df$pal[match$scat.df]
#               lower.panel = NULL)
#     })


# output$ACF <- renderPlot({
#     bikes %>% fill_gaps() %>%
#         filter(location %in% input$check.ACF) %>%
#         ACF(count) %>% autoplot() + geom_point() +
#         basic_theme +
#         xlab("Lag in days") + ylab("r coefficient for ACF") +
#         xlim(c(0,21)) +
#         theme(strip.text.y.right = element_text(angle = 0))
# })



## add title:
# title <- paste("Annual and weekly periods at", as.character(input$drop.cyclic)) ##
# p1 / p2 #+ plot_layout(guides = 'keep') & theme(legend.position = 'bottom')
#+ plot_annotation(title = title, theme = theme( plot.title = element_text(size = 16, family = "Century")))












#
#
# # Ottawa bike counters dashboard
#
# library(tidyverse)
# library(shiny)
# library(dplyr)
# library(ggplot2)
# library(reshape2)
# library(lubridate)
# library(shinythemes) # bootstrap themes
# library(tidyr)    # pivot_wider.. what else?
# library(ggthemes) # tufte themes
# library(leaflet)  # sidebar map
# library(forecast) # forecast statistics
# library(feasts)   # fir time series plots
# library(tsibble)  # for time series objects
# library(ggcorrplot)  ## for ?
# # library(grid)       # need for grid.arrange textGrob
# # library(gridExtra)
# library(patchwork)
# library(viridis)
# library(gplots)    ## for heatmap.2()
# library(kableExtra)
# library(corrplot)
# library(Hmisc)
# library(ggfortify)
# library(rcartocolor)
# library(RColorBrewer)
# require(graphics)
# library(gplots)
# library(extrafont)
# library(chron)
#
# source("./global.R")
#
#
# # makes all the horizontal line show up
# hr <-  tags$head(tags$style(HTML("hr {border-top: 1px solid #000000;}")))
#
# ################# Interactive Shiny doodads ######################3
#
# dropdown.ts <-
#   selectInput("drop.ts", "Select location:", locations,
#               selected = "Ottawa River")
# dropdown.cyclic.loc <-
#   selectInput("drop.cyclic", "Select location:", locations,
#               selected = "Laurier/Metcalfe")
# dropdown.seasonal.subs <-
#   selectInput("drop.seasonal.subs", "Select location:", locations,
#               selected = "Colonel By")
# check.scat <- checkboxGroupInput(
#   "check.scat", "Add/remove locations:", locations, selected = locations[c(1:4,7,11)],
#   inline = TRUE)
#
# check.ACF <- checkboxGroupInput(
#   "check.ACF", "Add/remove locations:", locations, selected = locations[c(1:4,7,11)],
#   inline = TRUE)
#
# check.PACF <- checkboxGroupInput(
#   "check.PACF", "Add/remove locations:", locations, selected = locations[c(1:4,7,11)],
#   inline = TRUE)
#
# date.range <-   dateRangeInput("date.range", "Date range:",
#                                start  = "2010-01-01", end    = "2019-09-30",
#                                min    = "2010-01-01", max    = "2019-09-30",
#                                format = "yyyy/mm", separator = " - ",
#                                startview = "decade")
# ## x11 model that wont work on weekly seasonal
# #########################    # UI TABPANELS    #######################################
#
#
# tab.ts <- tabPanel(tags$b("Time series"),
#                    h4("Daily rider count time series"), hr(),
#                    fluidRow(column(6, dropdown.ts), column(6, date.range)),
#                    plotOutput("daily.ts", height = "320px", width = "96%"),
#                    hr()
# )
# tab.cyclic <- tabPanel(tags$b("Seasonality"),
#                        h4("Annual and weekly seasonality"), hr(),
#                        fluidRow(column(12, dropdown.cyclic.loc)),
#                        plotOutput("period.ts", height = '360px', width = '96%'),
#                        cap.cycl,hr()
# )
# tab.seasonal.subs <- tabPanel(tags$b("Subseries"),
#                               h4("Long term change in seasonality"), hr(),
#                               fluidRow(column(12, dropdown.seasonal.subs)),
#                               plotOutput("seasonal.subs", height = '280px', width = '92%'),
#                               hr(),
#                               cap.seasonal.subs, br()
# )
# tab.ACF <- tabPanel(tags$b("ACF"),
#                     h4("Autocorrelation functions (ACF)"), hr(),
#                     plotOutput("ACF", height = '400px', width = '90%'),
#                     fluidRow(column(12, check.ACF)), hr(),
#                     cap.ACF, br()
# )
#
# tab.PACF <- tabPanel(tags$b("PACF"),
#                      h4("Partial autocorrelation functions (PACF)"), hr(),
#                      plotOutput("PACF", height = '400px', width = '90%'),
#                      fluidRow(column(12, check.PACF)), hr(),
#                      cap.PACF, br()
# )
#
# tab.corr <- tabPanel(tags$b("Correlation"),
#                      h4("Pairwise scatter of bike counters"), hr(),
#                      plotOutput("scatter", height = '450px', width = '95%'),
#                      fluidRow(column(12, check.scat)),
#                      cap.corr, br()
# )
#
# tab.clust <- tabPanel(tags$b("Clustering"),
#                       h4("Hierarchical clustering of Ottawa bike counters"), hr(),
#                       plotOutput("heatmap", height = '350px', width = '95%'), br(),
#                       cap.clust, br()
# )
#
#
#
# # tab.arima <- tabPanel(tags$b("Forecasting"),
# #                     h4("ARIMA forecasting the OttBike future"), hr(),
# #                     br()
# #                     )
#
#
# ###########################     UI LAYOUT     ######################################################
#
#
# ui <- fluidPage(theme = shinytheme("lumen"), #paper is good too, flatly, journal
#                 sidebarLayout(
#                   # Sidebar
#                   sidebarPanel(hr,hr(),
#                                titlePanel("#OttBike Counters", title = "#OttBike Counters"), hr(),
#                                fluidRow(column(12, leafletOutput("leaflet.map",width = "98%", height = "290px"))),
#                                hr(),
#                                p(h6(city.link),
#                                  h6("by JA Moggridge", twitter.link, github.link),
#                                  h6(cc.link))
#                   ),
#                   # Main
#                   mainPanel(tabsetPanel(tab.ts, tab.cyclic, tab.seasonal.subs,
#                                         tab.ACF, tab.PACF,
#                                         tab.corr, tab.clust),br(),br(),br())
#                 )
# )
#
# ###########################   SERVER    ######################################################
#
# # Define server logic required to draw a histogram
# server <- function(input, output) {
#
#
#   #############################      Leaflet Map     #######################################
#   output$leaflet.map <- renderLeaflet({
#     leaflet() %>%
#       addProviderTiles(providers$Stamen.TonerLite,
#                        options = providerTileOptions(noWrap = TRUE)) %>%
#       addMarkers(data = geo[, c(2,3)],label =  geo$locations)
#   })
#
#   #############################        Inputs        #######################################
#   tsInput <- reactive({
#     df = bikes %>%
#       filter(location == input$drop.ts) %>%
#       filter(Date >= input$date.range[1]) %>%
#       filter(Date <= input$date.range[2])
#     return(df)
#   })
#
#   cyclicInput <- reactive({
#     return(filter(bikes, location == input$drop.cyclic))
#   })
#   season.subsInput <- reactive({
#     return(filter(bikes, location == input$drop.seasonal.subs))
#   })
#
#   # scatInput <- reactive({
#   #     matrix <- bikes %>%
#   #         filter(location %in% input$check.scat) %>%
#   #         as_tibble() %>%
#   #         mutate(location = as.factor(location)) %>%
#   #         pivot_wider(id_cols = "Date",
#   #                     names_from = "location",
#   #                     values_from = 'count')
#   #     return(matrix)
#
#   scatInput <- reactive({
#     matrix <- bikes %>%
#       filter(location %in% input$check.scat) %>%
#       as_tibble() %>%
#       mutate(location = as.factor(location)) %>%
#       pivot_wider(id_cols = "Date",
#                   names_from = "location",
#                   values_from = 'count') %>%
#       select(-c(1)) %>%
#       mutate_if(is.character, as.numeric)
#     return(matrix)
#   })
#
#   #############################        PLOTS        #######################################
#
#   output$daily.ts <- renderPlot({
#
#     # title <- paste("Daily counts observed at", as.character(input$drop.ts))
#     ts.daily.df <- tsInput() %>% as_tibble() %>%
#       filter(!is.na(count)) %>%
#       mutate(weekend = ifelse(is.weekend(Date), "Weekend", "Weekday"))
#     max.count <- max(ts.daily.df$count)
#     ts.week.df <- ts.daily.df %>%
#       group_by(week=floor_date(Date, "week")) %>%
#       summarise(mean = mean(count))
#
#     ggplot(ts.daily.df, aes(x=Date, y=count, colour=weekend)) +
#       # geom_path(data = ts.week.df,
#       # aes(x = week, y = mean), colour = 'darkgrey') +
#       geom_point(alpha = 1, size = 0.4) +
#       ylim(c(0, max.count)) + ylab("Riders") +
#       scale_color_carto_d(palette = "Safe") +
#       basic_theme + geom_rangeframe() +
#       theme(legend.position = 'bottom', legend.title = element_blank(),
#             plot.title = element_text(hjust = -0.1))
#   })
#
#   output$period.ts <- renderPlot({
#     period.df <- cyclicInput() %>% fill_gaps()
#     p1 <- period.df %>%
#       gg_season(alpha = 0.55, size = 0.4, period = "year") +
#       ylab("Daily riders") + basic_theme + geom_rangeframe() +
#       scale_color_viridis_c() +
#       theme(legend.position = 'none',
#             axis.title.x = element_blank(),
#             plot.margin = margin(0.5, 0, 2, 0, "cm"))
#     p2 <- period.df %>%
#       gg_season(alpha = 0.54, size = 0.5, period = "week") +
#       basic_theme + geom_rangeframe() +
#       scale_color_viridis_c() +
#       theme(legend.position = 'none',
#             axis.title.y = element_blank(),
#             axis.title.x = element_blank(),
#             axis.text.x = element_text(angle = 35, vjust = 0.5)
#       )
#     # ::patchwork
#     p1 + p2
#   })
#
#   ### Seasonal subseries plots
#   output$seasonal.subs <- renderPlot({
#     monthly <- season.subsInput() %>%
#       as_tibble() %>%
#       filter(count > 0) %>%
#       mutate(month = month(Date, label = TRUE), year = format(Date, "%Y")) %>%
#       group_by(year, month) %>%
#       summarise(avg = mean(count, na.rm = TRUE),
#                 sum = sum(count, na.rm = TRUE))
#     month.means <- monthly %>%
#       group_by(month) %>%
#       summarise(month = as.factor(month),
#                 avg = mean(avg, na.rm = TRUE))
#     ggplot(monthly, aes(x = as.numeric(year), y = avg)) +
#       geom_hline(data = month.means, aes(yintercept = avg)) +
#       geom_path(size = 1, alpha = 0.8, colour = 'dodgerblue') +
#       scale_x_continuous(breaks = c(2010, 2015, 2019)) +
#       labs(x = "Year", y = "Monthly riders") +
#       facet_grid(.~month) +
#       theme_light() +
#       theme(text = element_text(family = 'Century', size = 14),
#             axis.text.x = element_text(angle = 90, size = 10, vjust = 0.5))
#   })
#   #
#   output$ACF <- renderPlot({
#     bikes %>% fill_gaps() %>%
#       filter(location %in% input$check.ACF) %>%
#       ACF(count) %>% autoplot() + geom_point() +
#       basic_theme +
#       xlab("Lag in days") + ylab("r coefficient for ACF") +
#       xlim(c(0,21)) +
#       theme(strip.text.y.right = element_text(angle = 0))
#   })
#
#   # wasn't working before bc using wrong input checkbox
#   output$PACF <- renderPlot({
#     bikes %>% fill_gaps() %>%
#       filter(location %in% input$check.PACF) %>%
#       PACF(count) %>% autoplot()+ geom_point() +
#       basic_theme +
#       xlab("Lag in days") + ylab("r coefficient for ACF") +
#       xlim(c(0,21)) +
#       theme(strip.text.y.right = element_text(angle = 0))
#   })
#   #
#   output$scatter <- renderPlot({
#     pairs(scatInput(), pch=19, cex=0.2, lower.panel = NULL)
#   })
#   #
#   output$heatmap <- renderPlot({
#     matrix <- bikes %>% as_tibble() %>%
#       mutate(location = as.factor(location)) %>%
#       tidyr::pivot_wider(id_cols = "Date",
#                          names_from = "location",
#                          values_from = 'count') %>%
#       # added this to remove early years with no data
#       filter(Date > ymd("2016-01-01")) %>%
#       mutate_if(is.character, as.numeric) %>%
#       # make sure date(1) isn't in matrix
#       #  (must remove) 6,7 is laurier/bay+lyon; 11,13,14 are smrst/gladst/portage
#       select(c(2:5, 8:10, 12, 14)) %>%
#       as.matrix()
#
#     colr <- viridis_pal(alpha = 0.9, begin = 0, end = 1, direction = 1,
#                         option = "C")(1000)
#     b <- rcorr(matrix)
#     heatmap.2(b$r, scale="none", # 'none' because matrix is symmetric
#               trace='none', col = colr,
#               distfun = function(x) dist(x, method = "euclidean"),
#               hclustfun = function(x) hclust(x, method = "average"),
#               dendrogram = c("row"),
#               symm = TRUE,
#               key.title = NA,
#               density.info = "none",
#               margins = c(10, 12)) + basic_theme # use margins to avoid cutting off labels
#   })
#   ##
# }
# ################################################################################################
# # Run the application
# shinyApp(ui = ui, server = server)
# ################################################################################################
#





################################### JUNKYARD #################################################

# scatInput <- reactive({
#     matrix <- bikes %>%
#         filter(location %in% input$check.scat) %>%
#         as_tibble() %>%
#         mutate(location = as.factor(location)) %>%
#         pivot_wider(id_cols = "Date",
#                     names_from = "location",
#                     values_from = 'count')
#     return(matrix)

### GRID PLOTS???

# & plot_annotation(theme = theme(plot.margin = margin(2,0,2,0)))
# plot_grid(p1, p2, ncol=1, align = 'v', labels=LETTERS[1:2]) #, rel_heights = c(0.5,1))
# aligned <- align_plots(p1, p2, align = "h")
# ggdraw(aligned)

# ### Seasonal subseries plots
# output$seasonal.subs <- renderPlot({
#     monthly <- season.subsInput() %>%
#         as_tibble() %>%
#         filter(count > 0) %>%
#         mutate(month = month(Date, label = TRUE), year = format(Date, "%Y")) %>%
#         group_by(year, month) %>%
#         summarise(avg = mean(count, na.rm = TRUE),
#                   sum = sum(count, na.rm = TRUE))
#     month.means <- monthly %>%
#         group_by(month) %>%
#         summarise(month = as.factor(month),
#                   avg = mean(avg, na.rm = TRUE))
#     ggplot(monthly, aes(x = as.numeric(year), y = avg)) +
#         geom_hline(data = month.means, aes(yintercept = avg)) +
#         geom_path(size = 1, alpha = 0.8, colour = 'dodgerblue') +
#         scale_x_continuous(breaks = c(2010, 2015, 2019)) +
#         labs(x = "Year", y = "Monthly Bike Riders") +
#         facet_grid(.~month) +
#         theme_light() +
#         theme(text = element_text(family = 'Century', size = 14),
#               axis.text.x = element_text(angle = 90, size = 10, vjust = 0.5))
# })
#




###################### GENERATE INPUT SELECTORS IN SERVER? will make app slow?

# library(shinydashboard)
# library(shiny)
# ui <- shinyUI(
#     dashboardPage(title = "Dashboard",
#                   dashboardHeader(),
#                   dashboardSidebar(
#                       tabsetPanel(
#                           tabPanel("tab1",
#                                    uiOutput("selectInput1")
#                           ),
#                           tabPanel("tab2",
#                                    uiOutput("selectInput2")
#                           )
#                       )),
#                   dashboardBody(
#                       verbatimTextOutput("selected")
#                   )
#     )
# )
#
# server <- shinyServer(function(input, output,session){
#
#     thechoice <- reactiveVal("a")
#     output$selectInput1 <- renderUI({
#         selectInput(inputId = "id1",
#                     label = "select",
#                     choices = c("a","b","c"),
#                     selected = thechoice())
#     })
#     output$selectInput2 <- renderUI({
#         selectInput(inputId = "id2",
#                     label = "select",
#                     choices = c("a","b","c"),
#                     selected = thechoice())
#     })
# })
#
# shinyApp(ui = ui, server = server)


# tab.ACF <- tabPanel(tags$b("Autocorrelation"),
#                     tabsetPanel(type = 'pills', tab.ACF, tab.PACF)
#                     )
# tab.seasonal.subs <- tabPanel(tags$b("by Month"),
#                               h4("Long term change in seasonality"), hr(),
#                               fluidRow(column(12, dropdown.seasonal.subs)),
#                               plotOutput("seasonal.subs", height = '280px', width = '92%'),
#                               cap.seasonal.subs, hr(),
# tab.ACF <- tabPanel(tags$b("ACF"),
#                     h4("Autocorrelation functions (ACF)"), hr(),
#                     plotOutput("ACF", height = '400px', width = '90%'),
#                     fluidRow(column(12, check.ACF)),
#                     cap.ACF,  hr(),
# )
# tab.PACF <- tabPanel(tags$b("PACF"),
#                      h4("Partial autocorrelation functions (PACF)"), hr(),
#                      plotOutput("PACF", height = '400px', width = '90%'),
#                      fluidRow(column(12, check.PACF)),
#                      cap.PACF, hr(),
# )
# tab.arima <- tabPanel(tags$b("Forecasting"),
#                     h4("ARIMA forecasting the OttBike future"), hr(),
#                     br()
#                     )


#
#     # wasn't working before bc using wrong input checkbox
#     output$PACF <- renderPlot({
#         bikes %>% fill_gaps() %>%
#             filter(location %in% input$check.PACF) %>%
#             PACF(count) %>% autoplot()+ geom_point() +
#             basic_theme +
#             xlab("Lag in days") + ylab("r") +
#             xlim(c(0,21)) +
#             theme(strip.text.y.right = element_text(angle = 0))
#     })


#
#     output$daily.ts <- renderPlot({
#
#         # title <- paste("Daily counts observed at", as.character(input$drop.ts))
#         ts.daily.df <- tsInput() %>% as_tibble() %>%
#             filter(!is.na(count)) %>%
#             mutate(weekend = ifelse(is.weekend(Date), "Weekend", "Weekday"))
#         max.count <- max(ts.daily.df$count)
#         ts.week.df <- ts.daily.df %>%
#             group_by(week=floor_date(Date, "week")) %>%
#             summarise(mean = mean(count))
#
#         ggplot(ts.daily.df, aes(x=Date, y=count, colour=weekend)) +
#             # geom_path(data = ts.week.df,
#             # aes(x = week, y = mean), colour = 'darkgrey') +
#             geom_point(alpha = 1, size = 0.65) +
#             ylim(c(0, max.count)) + ylab("Bike Riders") +
#             scale_color_carto_d(palette = "Safe") +
#             basic_theme + geom_rangeframe() +
#
#             theme(legend.position = 'bottom', legend.title = element_blank(),
#                   plot.title = element_text(hjust = -0.1),
#                   panel.grid.major.y = element_line(colour = "gray"))
#     })



# output$daily.ts <- renderPlot({
# locs <- length(input$check.location)
# dt <- input$date.range
# if (locs < length(locations)){
#     sub <- paste("from", locs, "Ottawa locations between", dt[1],'and', dt[2])
# } else {
#     sub <- paste("from all Ottawa locations between", dt[1],'and', dt[2])
# }
#     #
#     ts.daily.df <- multipleInput() %>%
#         as_tibble() %>%
#         # filter(count == 0) %>%
#         mutate(weekend = ifelse(is.weekend(Date), "Weekend", "Weekday")) %>%
#         filter(location %in% input$check.location,
#                Date >= input$date.range[1],
#                Date <= input$date.range[2])
#
#     ggplot(ts.daily.df) +
#         geom_bar(aes(x=Date,y=count, color=weekend), stat='identity')+
#         geom_hline(yintercept = 0, colour = 'grey', size = 0.6) +
#
#         labs(title = "Daily bike counts time series",
#              # subtitle = sub,
#              caption = "Data source: City of Ottawa") +
#         # scale_color_viridis_c(option = "A") +
#         scale_color_carto_d(palette = "Safe") +
#
#         basic_theme +
#         facet_grid(location ~ .) +
#         guides(colour = guide_legend(override.aes = list(size=5))) +
#         theme(plot.margin =margin(0.5, 0, 0, 0, "cm"),
#               legend.position = 'bottom', legend.title = element_blank(),
#               axis.title.y = element_blank(),
#               axis.text.y.left = element_text(size = 8),
#               strip.text.y.right = element_text(size = 12, angle = 0, hjust = 0))
# })







# output$wind.speed <- renderPlot({
#     weatherInput() %>%
#         ggplot(aes(x= Wind.speed, y = count)) +
#         geom_jitter(size = 0.2, alpha = 0.19, width = 1) +
#         geom_smooth(se = FALSE, size =1.5, na.rm=TRUE) +
#         xlab("Wind speed (kph)") +
#         basic_theme + geom_rangeframe() +
#         xlim(c(28,62)) + ylim(limit.count) +
#         theme(legend.position = "none",
#               plot.margin = margin(0, 0, 0, 0, "cm")
#         )
# })

#
#     output$wind <- renderPlot({
#
# if (input$drop.wind == 'Combine selection'){
#     wind.df<- weatherInput()
# } else {
#     wind.df<- weatherInput() %>%
#         filter(location %in% input$drop.location)
# }
#         limit.count <- c(0, max(wind.df$count)*0.65)
#
#         wind1 <- wind.df%>% ggplot(aes(x = Wind.deg, y = count)) +
#             geom_jitter(width = 4, size =0.20, alpha =0.35) +
#             geom_smooth(se = FALSE, size =2, na.rm = TRUE) +
#             geom_point(aes(x=0,y=0), size =2, colour='white')+
#             ylab("Daily Bike Riders") +
#             ylim(limit.count) +
#             geom_rangeframe() + coord_polar() +
#             basic_theme + geom_rangeframe() +
#             theme(axis.title.x = element_blank(),
#                   plot.margin = margin(0, 1, -0.3, 0, "cm")
#             )
#
#         wind2 <- wind.df %>%
#             mutate(Wind.speed = as.numeric(Wind.speed),
#                    Wind.speed, replace_na(Wind.speed, 29)) %>%
#             ggplot(aes(x= Wind.speed, y = count)) +
#             geom_jitter(size = 0.2, alpha = 0.19, width = 1) +
#             geom_smooth(se = FALSE, size =1.5, na.rm=TRUE) +
#             xlab("Wind speed (kph)") +
#             basic_theme + geom_rangeframe() +
#             xlim(c(28,62)) + ylim(limit.count) +
#             theme(legend.position = "none",
#                   plot.margin = margin(0, 0, 0, 0, "cm")
#             )
#         (wind1 | wind2)
#     })



############
# dateInput2 <- function(inputId, label, minview = "months", maxview = "decades", ...) {
#     d <- shiny::dateInput(inputId, label, ...)
#     d$children[[2L]]$attribs[["data-date-min-view-mode"]] <- minview
#     d$children[[2L]]$attribs[["data-date-max-view-mode"]] <- maxview
#
# }

# dateRangeInput2 <- function(inputId, label, minview = "months", maxview = "decades", ...) {
#     d <- shiny::dateRangeInput(inputId, label, ...)
#     d$children[[2L]]$children[[1]]$attribs[["data-date-min-view-mode"]] <- minview
#     d$children[[2L]]$children[[3]]$attribs[["data-date-min-view-mode"]] <- minview
#     d$children[[2L]]$children[[1]]$attribs[["data-date-max-view-mode"]] <- maxview
#     d$children[[2L]]$children[[3]]$attribs[["data-date-max-view-mode"]] <- maxview
#     d
# }
