################## Ottawa bike counters dashboard #########################################

#### libraries ####
library(tidyverse)
library(shiny)
library(dplyr)
library(ggplot2)
library(reshape2)
library(lubridate)
library(shinythemes) # bootstrap themes
library(tidyr)      # pivot_wider.. what else?
library(ggthemes) # tufte themes
library(leaflet)  # sidebar map
library(forecast) # forecast statistics
library(feasts)   # fir time series plots
library(tsibble)  # for time series objects
library(ggcorrplot)  ## for ?
library(patchwork)
library(viridis)
library(gplots)    ## for heatmap.2()
library(kableExtra)
library(corrplot)
library(Hmisc)
library(ggfortify)
library(rcartocolor)
library(RColorBrewer)
require(graphics)
library(extrafont)
library(chron)
library(zoo)
library(broom)
library(ggtext)
library(ggrepel)

# library(shinyWidgets)

# library(cowplot)
# library(grid)       # need for grid.arrange textGrob
# library(gridExtra)
# library(glue)

######### source .global.R #######
source("./global.R")

# makes all the horizontal lines actually show up
hr <-  tags$head(tags$style(HTML("hr {border-top: 1px solid #000000;}")))

################# Reactive Shiny doodads ######################
### Side panel
dropdown.location <-
    selectInput("drop.location",
                tags$b("Single location selection:"),
                locations, selected = "Laurier/Metcalfe")

check.location <- checkboxGroupInput(
    "check.location", tags$b("Group selection:"), locations, selected = locations[-c(1,5,6,10,12)]
    # inline = FALSE
)
date.range <-   dateRangeInput("date.range", p(tags$b("Date range:"), "min 2010-01-28 | max 2019-09-30"),
                               start  = "2010-01-28",   end = "2019-09-30",
                               min    = "2010-01-28",   max = "2019-09-30",
                               format = "yyyy/mm/dd",   separator = " - ",
                               startview = "decade")

### Main panels

drop.combo <-
    selectInput("drop.combo", tags$b("Display data from:"),
                c('Selected location', 'Selected group', 'All locations'),
                selected = 'All locations')

slider.kdays <- sliderInput("k.max", tags$b("Set lagging days range"),
                            min = 14, max = 366, step = 1,ticks = TRUE,value = 35
                                )
# dropdown.clust <-
#     selectInput("drop.clust", tags$b("Metric:"), locations, selected = "Average-linkage")
###

########    # UI TABPANELS    ########

#### TIME SERIES ###
tab.ts <- tabPanel(tags$b("Time series"),
                   br(), plotOutput("daily.ts", height = '500px', width = "100%"),
                   p("*This figure reacts to the chosen 'group selection' and 'date range'."), hr()
)
tab.season <- tabPanel(tags$b("Seasonality"),
                       br(), plotOutput("period.ts", height = '500px', width = '80%'),
                       cap.season, hr()
)
#### SCATTER / CLUSTER ####
tab.scat <- tabPanel(tags$b("Pairwise scatter"),
                     plotOutput("scatter", height = '500px', width = '100%'),
                     cap.corr, hr()
)
# tab.clust <- tabPanel(tags$b("Clustering"),
#                       plotOutput("heatmap", height = '500px', width = '100%'),
#                       cap.clust, hr()
# )
tab.pca <- tabPanel(tags$b("PCA"), br(), plotOutput("PCA", height = '400px', width = '80%'),
                    cap.pca, hr()
)

tab.corr <- tabPanel(tags$b("Similarity"), br(),
                     tabsetPanel(type = 'pills', tab.scat, tab.pca))



#### AUTOCORRELATION ####
tab.ACF <- tabPanel(tags$b("Autocorrelation"), br(),
                    plotOutput("ACF", height = '500px', width = '100%'),
                    slider.kdays,
                    cap.ACF, hr(),
                    DT::dataTableOutput("table.acf")
)

tab.STL <- tabPanel(tags$b("Decomposition"), br(),
                    plotOutput("plot.STL"),# height = '500px', width = '100%'),
                    cap.STL, hr()
)
tab.auto <- tabPanel(tags$b("Autoregression"), br(),
                     tabsetPanel(type = 'pills', tab.ACF, tab.STL) )

#### WEATHER ####
t.all <- tabPanel("All", br(),
                  plotOutput("weather.combo", height = '500px', width = '95%'),
                  fluidRow(column(12, drop.combo)))
t.temp <- tabPanel("Temperature", plotOutput("Tmean", height = '500px', width = '95%'))
t.prep <- tabPanel("Precipitation", plotOutput("precip", height = '500px', width = '95%'))
t.wd <- tabPanel("Wind direction", plotOutput("wind.deg", height = '500px', width = '95%'))
t.ws <- tabPanel("Wind speed", plotOutput("wind.spd", height = '500px', width = '95%'))
tab.weather <- tabPanel(tags$b("Weather"), br(),
                        tabsetPanel(type = 'pills',
                                    t.all, t.temp, t.prep, t.wd, t.ws),
                        cap.weather, hr())
tab.map <- tabPanel(tags$b("Map"), br(),
                    fluidRow(column(12, leafletOutput("leaflet.map",width = "90%", height = "500px"))),
                    )



##### stuff ###
whitespace <- HTML('&nbsp;&nbsp;&nbsp;&nbsp;')
bike.icon <- HTML('<i class="fa fa-bicycle fa-1x" aria-hidden="false"></i>')


###########################     UI LAYOUT     ######################################################

ui <- fluidPage(theme = shinytheme("lumen"), #paper is good too, flatly, journal
                sidebarLayout(
                    # Sidebar
                    sidebarPanel(hr, bike.icon, "24,080,530 and counting...", bike.icon,
                                 titlePanel(title = tags$b(title.text), windowTitle = "#OttBikeCounters"),
                                 hr(),
                                 h4(em("time series analysis dashboard"),br()), br(),
                                 fluidRow(column(11, date.range, dropdown.location, check.location,
                                                 h5("*Input selectors are noted at the end of the caption for each for each figure."),
                                                 hr(),
                                                 side.text)),

                    ),

                    # Main
                    mainPanel(br(), tabsetPanel(type = 'tabs',
                                          tab.ts, tab.season, tab.weather,
                                          tab.corr, tab.auto, tab.map
                                          ),
                              br(),br()
                    )
                )
)

###########################   SERVER    ######################################################
# Define server logic required to draw a histogram
server <- function(input, output) {


    #############################      Leaflet Map     #######################################

    output$leaflet.map <- renderLeaflet({
        leaflet() %>%
            addProviderTiles(providers$Stamen.TonerLite,
                             options = providerTileOptions(noWrap = TRUE)) %>%
            addMarkers(data = geo[, c(2,3)],label =  geo$locations)
    })
    #############################        Inputs        #######################################

    multipleInput <- reactive({
        df = bikes %>%
            filter(location %in% input$check.location,
                   Date >= input$date.range[1],
                   Date <= input$date.range[2])
        return(df)
    })
    singleInput <- reactive({
        return(filter(bikes, location %in% input$drop.location))
    })
    weatherInput <- reactive({
        df = bike.weather %>%
            filter(location %in% input$check.location,
                   Date >= input$date.range[1],
                   Date <= input$date.range[2])
        return(df)
    })
    scatInput <- reactive({
        matrix <- bikes %>%
            filter(location %in% input$check.location,
                   Date >= input$date.range[1],
                   Date <= input$date.range[2]) %>%
            as_tibble() %>%
            mutate(location = as.factor(location)) %>%
            pivot_wider(id_cols = "Date",
                        names_from = "location",
                        values_from = 'count') %>%
            select(-c(1)) %>%
            mutate_if(is.character, as.numeric)
        return(matrix)
    })


    #   ############################        PLOTS        #######################################

    output$daily.ts <- renderPlot({
        locs <- length(input$check.location)
        dt <- input$date.range
        if (locs < length(locations)){
            sub <- paste("from", locs, "Ottawa locations between", dt[1],'and', dt[2])
        } else {
            sub <- paste("from all Ottawa locations between", dt[1],'and', dt[2])
        }

        ts.daily.df <- multipleInput() %>%
            as_tibble() %>%
            mutate(weekend = ifelse(is.weekend(Date), "Weekend", "Weekday"),
                   weekend = fct_rev(weekend)) %>%
            filter(location %in% input$check.location,
                   Date >= input$date.range[1],
                   Date <= input$date.range[2])

        ggplot(ts.daily.df, aes(x = Date, y = count, fill=weekend, colour = weekend)) +
            geom_point(alpha = 1, size = 0.42, shape = 1) +
            # geom_col() +
            geom_hline(yintercept = 0, colour = 'grey', size = 0.6) +

            labs(title = "Daily bike counts time series",
                 subtitle = sub,
                 caption = "Data source: City of Ottawa") +
            # scale_color_carto_d(palette = "Safe") +
            # scale_fill_carto_d(palette = "Safe") +
            basic_theme +
            facet_grid(location ~ ., scales = 'free') +
            guides(colour = guide_legend(override.aes = list(size=5))) +
            theme(plot.margin =margin(0.5, 0, 0, 0, "cm"),
                  legend.position = 'bottom', legend.title = element_blank(),
                  axis.title.y = element_blank(),
                  axis.text.y.left = element_text(size = 8),
                  strip.text.y.right = element_text(size = 12, angle = 0, hjust = 0))
    })

    output$period.ts <- renderPlot({
        # summarise means
        period.df <- singleInput() %>%  fill_gaps()%>%  # need tsibble for gg_season
            filter(Date >= input$date.range[1],
                   Date <= input$date.range[2])
        monthly <- as_tibble(period.df) %>%
            filter(count > 0) %>%
            mutate(month = month(Date, label = TRUE),
                   year = format(Date, "%Y")) %>%
            group_by(year, month) %>%
            summarise(avg = mean(count, na.rm = TRUE),
                      sum = sum(count, na.rm = TRUE)) %>%
            ungroup()
        month.means <- as_tibble(period.df) %>%
            filter(count > 0) %>%
            mutate(month = as.factor(month(Date, label = TRUE)),
                   year = format(Date, "%Y")) %>%
            group_by(month) %>%
            summarise(avg = mean(count, na.rm = TRUE)) %>%
            ungroup()
        wday.df <- as_tibble(period.df) %>%
            filter(count > 0) %>%
            mutate(wday = as.factor(wday(Date, label=TRUE)),
                   year = format(Date, "%Y")) %>%
            group_by(year, wday) %>%
            summarise(mean = mean(count), na.rm = TRUE) %>%
            ungroup()
        wday.means <- as_tibble(period.df) %>%
            filter(count > 0) %>%
            mutate(wday = as.factor(wday(Date, label=TRUE))) %>%
            group_by(wday) %>%
            summarise(avg = mean(count), na.rm = TRUE) %>%
            ungroup()

        ## plots
        per1 <- period.df %>%
            gg_season(alpha = 0.25, size = 1, period = "year") +
            ylab("Daily count") +  basic_theme + geom_rangeframe(colour='black') +
            scale_color_viridis_c() +
            theme(legend.position = 'none',
                  axis.title.x = element_blank(),
                  axis.text.x = element_text(hjust = 1),
                  panel.grid.major.y = element_line(colour = "grey", size = 0.2),
                  plot.margin = margin(0.5, 0, 0.1, 0, "cm"))

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
        per3 <- ggplot(monthly, aes(x = as.numeric(year), y = avg)) +
            geom_hline(data = month.means, aes(yintercept = avg)) +
            geom_path(size = 1, alpha = 0.8, colour = 'dodgerblue') +
            geom_point(size = 1, alpha = 0.8, colour = 'dodgerblue') +
            scale_x_continuous(breaks = c(2010, 2015, 2019)) +
            ylab("Mean count") +
            facet_grid(. ~ month) + geom_rangeframe(colour = 'black') +
            basic_theme +
            theme(axis.title.x = element_blank(), # axis.title.x = element_text(vjust = 0),
                  # axis.title.y = element_blank(),
                  axis.text.x = element_text(angle = 80, size = 9, hjust = 1),
                  panel.grid.major.x = element_line(colour = "grey", size = 0.2),
                  plot.margin = margin(0, 0, 0, 0, "cm"))

        per4 <- ggplot(wday.df, aes(x = as.numeric(year), y = mean)) +
            geom_hline(data = wday.means, aes(yintercept = avg)) +
            geom_line(size = 1, alpha = 0.8, colour = 'dodgerblue') +
            geom_point(size = 1, alpha = 0.8, colour = 'dodgerblue') +
            scale_x_continuous(breaks = c(2010, 2015, 2019)) +
            ylab("Mean count") +
            facet_grid(. ~ wday) + geom_rangeframe(colour = 'black') +
            basic_theme +
            theme(
                  axis.title.x = element_blank(),
                  # axis.title.y = element_blank(),
                  axis.text.x = element_text(angle = 80, size = 9, hjust = 1),
                  panel.grid.major.x = element_line(colour = "grey", size = 0.2),
                  plot.margin = margin(0.5, 0, 0.05, 0, "cm"))

        # reactive subtitle
        dt <- input$date.range
        sub <- paste('Annual and hebdomadal patterns and their change from', dt[1], 'to', dt[2])
        # ::patchwork
        # pwork <- (per1 | per2) / per3 / per4 + plot_annotation(labs(x = "Year", y = "Mean daily count"))
        pwork <- per1 / per3 / per4 + plot_annotation(labs(x = "Year", y = "Mean daily count"))

        pwork + plot_annotation(
            title =  paste("What are the weekly and seasonal patterns at ", as.character(input$drop.location),"?", sep=''),
            subtitle = sub,
            caption = 'Data source: City of Ottawa ', theme = basic_theme,
            tag_levels = 'a') &
            theme(plot.tag = element_text(size = 12))

    })

    ### Four weather tabs
    output$Tmean <- renderPlot({
        weatherInput() %>%
            ggplot(aes(x=Tmean, y=count)) +
            geom_point(size = 0.01, alpha = 0.3) +
            geom_smooth(size=2, color="#f8766d") +
            labs(y = "Daily count",
                 x = expression(paste("Mean temperature ( ", degree ~ C, ")")),
                 title = 'How does Ottawa bike traffic respond to climate stressors?',
                 subtitle = "Ottawa bike counts by temperature",
                 caption = 'Data sources: City of Ottawa & Environment Canada') +

            scale_color_colorblind() +
            geom_rangeframe(colour='black') + facet_wrap(~location, scales="free") + basic_theme +
            theme(strip.text = element_text(size=12),
                  plot.margin = margin(0.7, 0.2, 0.4, 0.2, "cm")
            )
    })
    output$precip <- renderPlot({
        weatherInput() %>%
            ggplot(aes(x = Precipitation, y=count, colour = Precipitation)) +
            # geom_tufteboxplot(median.type = "point", hoffset = 0, width = 5) +
            # geom_violin(color = 'black', size=0.19, ) +
            geom_jitter(alpha=0.3, size=0.2) +
            geom_boxplot(alpha=0, colour='black', notch = 2) +
            # geom_point(y=mean(count), colour='black', size=4) +
            labs(y = "Daily count",
                 title = 'How does Ottawa bike traffic respond to climate stressors?',
                 subtitle = "Ottawa bike counts by daily total precipitation",
                 caption = 'Data source: City of Ottawa & Environment Canada') +
            geom_rangeframe(colour='black') + facet_wrap(~location,scales="free") + basic_theme +

            #Rename the legend title and text labels.
            scale_color_viridis_d(name= "Precipitation (mm)",direction = -1, end = 0.8,
                                  labels = c("0", "< 5 ", "5 - 20", "20 +")) +
            # guides(fill = guide_legend(reverse = TRUE)) +
            # guides(colour = guide_legend(reverse = TRUE)) +
            guides(colour = guide_legend(override.aes = list(size=5, alpha=1, pch=0))) +
            guides(fill= guide_legend(override.aes = list(size=1, alpha=1, shape=0))) +
            theme(axis.title = element_blank(),
                  # axis.text.x = element_text(angle = 90, size=10, vjust = 0.5, hjust=1),
                  axis.text.x = element_blank(),
                  strip.text = element_text(size=12),
                  legend.position = 'right',
                  plot.margin = margin(0.7, 0.2, 0.4, 0.2, "cm"))
    })
    output$wind.deg <- renderPlot({

        limit.count <- c(0, max(weatherInput()$count)*0.45)

        weatherInput() %>% ggplot(aes(x = Wind.deg, y = count)) +
            geom_jitter(width = 4, size =0.20, alpha =0.35) +
            # geom_density2d(aes(), geom = "polygon") +
            # scale_color_viridis_c() +
            geom_smooth(size = 2, na.rm = TRUE, color="#00BFC4") +
            geom_point(aes(x=0,y=0), size = 0.78, colour='red2')+
            ylim(limit.count) +
            labs(y = "Daily count",
                 x = expression(paste("Wind direction (", degree, ")")),
                 title = 'How does Ottawa bike traffic respond to climate stressors?',
                 subtitle = "Ottawa bike counts by wind direction",
                 caption = 'Data source: City of Ottawa & Environment Canada') +
            facet_wrap(.~location, shrink = TRUE) + #, scales="free_y") +
            coord_polar() +
            basic_theme + geom_rangeframe(colour='black') +
            theme(axis.title.x = element_blank(),
                  plot.margin = margin(0.7, 0.2, 0.4, 0.2, "cm"),
                  strip.text = element_text(size=12))
    })
    output$wind.spd <- renderPlot({

        # limit.count <- c(0, max(wind.df$count)*0.65)
        wind1 <- weatherInput() %>%
            ggplot(aes(x= Wind.speed, y = count)) +
            geom_jitter(size = 0.2, alpha = 0.19, width = 1) +
            geom_smooth(size =1.5, na.rm=TRUE, colour = '#00BFC4') +
            labs(y = "Daily count",
                 x ="Wind speed (kph)",
                 title = 'How does Ottawa bike traffic respond to climate stressors?',
                 subtitle = "Ottawa bike counts by maximum wind gust speed",
                 caption = 'Data source: City of Ottawa & Environment Canada') +
            basic_theme + geom_rangeframe(colour='black') +
            xlim(c(28,62)) +
            theme(axis.title.x = element_blank(),
                  plot.margin = margin(0.7, 0.2, 0.4, 0.2, "cm"),
                  strip.text = element_text(size=12))
        wind1 + facet_wrap(.~location, scales="free_y")
    })

    ### first combo weather tab
    output$weather.combo <- renderPlot({
        if (input$drop.combo == 'Selected group'){
            combo.df <- weatherInput()
            locs <- paste(length(input$check.location), "selected locations")
        } else if (input$drop.combo == 'All locations'){
            combo.df <- bike.weather %>%
                filter(Date >= input$date.range[1],
                       Date <= input$date.range[2])
            locs <- "all locations"
        }
        else {
            combo.df <- bike.weather %>%
                filter(location %in% input$drop.location)
                locs <- paste(input$drop.location)
        }
        limit.count <- c(0, max(combo.df$count)*0.55)
        Tm <- combo.df %>%
            ggplot(aes(x=Tmean, y=count)) +
            geom_point(size = 0.01, alpha = 0.3) +
            geom_smooth(size=1.8, color="#f8766d") +
            ylab("Daily count") +
            xlab(expression(paste("Mean temperature (", degree ~ C, ")"))) +
            geom_rangeframe(colour='black') + basic_theme +
            theme(strip.text = element_text(size=16))
        Pr <- combo.df %>%
            ggplot(aes(x = Precipitation, y=count, colour = Precipitation)) +
            # geom_violin(color='black', size = 0.25) +
            # geom_tufteboxplot(median.type = "line", hoffset = 0, width = 5) +
            geom_jitter(alpha=0.2, size=0.2) +
            geom_boxplot(alpha=0, colour='black', notch = 2) +
            # xlab("Precipitation (mm)") +
            scale_color_viridis_d(name= "Precipitation (mm)",direction = -1, end =0.8,
                                 labels = c("0", "< 5 ", "5 - 20", "20 +")) +
            guides(colour = guide_legend(override.aes = list(size=5, alpha=1))) +
            # guides(fill= guide_legend(override.aes = list(size=1, alpha=1, shape=0))) +            scale_color_discrete(name= "Precip. (mm)",
                                 # labels = c("0", "< 5 ", "5 - 20", "20 +")) +
            geom_rangeframe(colour='black') + basic_theme +
            theme(axis.text.x = element_blank(),
                  # axis.text.x = element_text(angle = 1, size=12),
                  axis.title.y = element_blank())
        Wd <- combo.df %>%
            ggplot(aes(x = Wind.deg, y = count)) +
            geom_jitter(width = 4, size =0.20, alpha =0.35) +
            geom_smooth(size = 1.8, na.rm = TRUE, color = "#00BFC4") +
            labs(y = "Daily count", x = "Wind direction") +
            xlim(c(0,360)) + ylim(limit.count) +
            geom_rangeframe(colour='black') + coord_polar() +
            basic_theme + geom_rangeframe(colour='black') +
            theme( #axis.title.x = element_blank(),
                  plot.margin = margin(0, 1, -0.3, 0, "cm"))
        Ws <- combo.df %>%
            ggplot(aes(x= Wind.speed, y = count)) +
            geom_jitter(size = 0.2, alpha = 0.19, width = 1) +
            geom_smooth(size =1.8, na.rm=TRUE, colour = '#00BFC4') +
            geom_point(aes(x=0,y=0), size =1, color = 'red') +
            xlab("Wind speed (km/h)") +
            basic_theme + geom_rangeframe(colour='black') +
            xlim(c(28,62)) + # ylim(limit.count) +
            theme(legend.position = "none",
                  axis.title.y = element_blank(),
                  plot.margin = margin(0, 0, 0, 0, "cm"))

        dt <- input$date.range
        # ::patchwork.weather
        (Tm + Pr + Wd + Ws) +

            plot_annotation(
                subtitle =  paste("Bike counts by weather at", locs, "during",dt[1],"-",dt[2]),
                title = 'How does Ottawa bike traffic respond to climate stressors?',
                caption = 'Data source: City of Ottawa & Environment Canada',
                theme = basic_theme)
    })

    ### Scatter xy grid and heatmap/dendrogram
    output$scatter <- renderPlot({
        pairs(scatInput(), pch=1, cex=0.1, alpha=0.5, lower.panel = NULL)
    })

    ### heatmap
    output$heatmap <- renderPlot({
        bike.matrix <- bikes %>% as_tibble() %>%
            mutate(location = as.factor(location)) %>%
            tidyr::pivot_wider(id_cols = "Date",
                               names_from = "location",
                               values_from = 'count') %>%
            # added this to remove early years with no data
            filter(Date > ymd("2016-01-01")) %>%
            mutate_if(is.character, as.numeric) %>%
            # make sure date(col #1) isn't in matrix !!! OR ELSE!!
            #  (must remove) 6,7 is laurier/bay+lyon; 11,13,14 are young/gladst/portage
            select(c(2:5, 8:10, 12, 14)) %>%
            as.matrix()

        colr <- viridis_pal(alpha = 0.9, begin = 0, end = 1, direction = 1,
                            option = "E")(1000)
        b.matrix <- rcorr(bike.matrix)
        heatmap.2(b.matrix$r, scale="none", # 'none' because matrix is symmetric
                  trace='none', col = colr,
                  distfun = function(x) dist(x, method = "euclidean"),
                  hclustfun = function(x) hclust(x, method = "average"),   ### hclust methods: average:UPGMA; single:NN; centroid, median,
                  dendrogram = c("row"),
                  symm = TRUE,
                  key = FALSE,
                  # key.title = "Distance",
                  density.info = "none",
                  margins = c(10, 12)) + basic_theme # use margins to avoid cutting off labels
    })

    ### ACF, PACF and STL plots
    output$ACF <- renderPlot({
        acf.p1 <- bikes %>% fill_gaps() %>%
            filter(location %in% input$check.location) %>%
            ACF(count, lag_max = 366) %>% autoplot() + geom_point() +
            xlim(c(0, input$k.max)) +
            labs(title = "ACF", y = "*r*-coefficient", x='Lag time (days)')+
            basic_theme +
            theme(text = element_text(size = 14),
                  strip.text.y.right = element_blank(),
                  axis.title.y = ggtext::element_markdown(vjust = 1),
                  axis.title.x = element_blank(),
                  axis.text.x = element_text(vjust=0.1))

        acf.p2 <- bikes %>% fill_gaps() %>%
            filter(location %in% input$check.location) %>%
            PACF(count, lag_max = 366) %>% autoplot()+ geom_point(size=1.3) +
            labs(title = "PACF", x='Lag time (days)') +
            xlim(c(0, input$k.max)) +
            basic_theme +
            theme(text = element_text(size = 14),
                  axis.title.y = element_blank(),
                  axis.title.x = element_blank(),
                  strip.text.y.right = element_text(angle = 0, hjust = 0, size = 14))

        dt <- c(min(bikes$Date), max(bikes$Date))
        (acf.p1 | acf.p2) + xlab("Lag time (days)") +
            plot_annotation(
                subtitle =  paste("ACF and PACF of Ottawa bike counters during ", dt[1], '-', dt[2]),
                title = 'How strongly are counts at each location correlated to prior observations?',
                caption = 'Data source: City of Ottawa', theme = basic_theme)
    })

    output$table.acf <- DT::renderDataTable(DT::datatable({
        bikes %>%
            features(count, feat_acf) %>%
            mutate_if(is.numeric, round, 3)},
        rownames = FALSE,
        options = list(paging = FALSE, autoWidth = TRUE, searching=FALSE))
    )
    ### STL
    output$plot.STL <- renderPlot({
        bikes %>%
            filter(location %in% input$drop.location) %>%
            filter(!is.na(count)) %>%
            # filter(Date > ymd('2015-01-01')) %>%
            as_tsibble() %>% fill_gaps() %>%
            mutate(count = na.approx(count)) %>%
            model(STL(count ~ season(window=13), # + season(window='periodic'),
                      robust = TRUE)) %>%
            components() %>%
            autoplot() +
            labs(title =  paste("STL decomposition of",input$drop.location,
                                "bike counter time series"),
                subtitle = 'Count = trend + annual & weekly seasonality + remainder',
                caption = 'Data source: City of Ottawa', theme = basic_theme) +
            basic_theme +
            theme(strip.text.y.right = element_text(angle = 0))
    })

    output$PCA <- renderPlot({
        pcs %>%
            ggplot(aes(x=.fittedPC1, y=.fittedPC2)) +
            geom_point(size=2) +
            geom_text_repel(aes(label=location), size = 6) +
            labs(x="PC1", y="PC2") + basic_theme + geom_rangeframe(colour='black')
    })
}
################################### APP CALL ##################################################
# Run the application
shinyApp(ui = ui, server = server)


