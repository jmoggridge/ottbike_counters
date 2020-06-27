################## Ottawa bike counters dashboard #########################################

#### libraries ####
# library(tidyverse)
library(forcats)
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
library(imputeTS) # has kalman filter algo and others for filling missing values

library(ggcorrplot)  ## for ?
library(patchwork)
library(viridis)
library(gplots)    ## for heatmap.2()
library(kableExtra)
library(corrplot)
library(Hmisc)
library(ggfortify)
# library(rcartocolor)
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

######### source ./global.R for data, themes, captions, etc. #######
source("./global.R")


################# Reactive Shiny data filters ######################
### Side panel
drop.location <-
    selectInput("drop.location",
                tags$b("Single selection:"),
                locations, selected = "Laurier/Metcalfe")

check.location <- checkboxGroupInput(
    "check.location", tags$b("Group selection:"), locations, selected = locations[c(2,4,7,8,11)]
    # inline = FALSE
)

date.slider <- sliderInput("date.slider", h5(tags$b("Date range:")),
                           min = as.Date("2010-01-28","%Y-%m-%d"),
                           max = as.Date("2019-09-30","%Y-%m-%d"),
                           value=c(as.Date("2010-01-28"), as.Date("2019-09-30")),
                           timeFormat="%Y-%m", step = 90, ticks = TRUE)
                # animate = TRUE, -> doesn't work very well though - greyed out/loading constantly
crossfade.slider <- sliderInput("crossfade", h5("Boxplot / Dotplot fader"),
                                min = 0, max = 1, value = 0.1)

drop.colours <- selectInput("drop.colours", tags$b("Plot colours:"),
                                c("viridis","magma","cividis","inferno"),
                                selected = "D")

### Main panels

drop.combo <-
    selectInput("drop.combo", tags$b("Display data from:"),
                c('Selected location', 'Selected group', 'All locations'),
                selected = 'All locations')

slider.kdays <- sliderInput("k.max", tags$b("Set lagging days range"),
                            min = 14, max = 366, step = 1,ticks = TRUE,value = 35
                                )

##########################    # UI TABPANELS    #####################################################

#### TIME SERIES ###
tab.ts <- tabPanel(tags$b("Time series"),
                   br(), plotOutput("daily.ts", height = '500px', width = "100%"),
                   p("*This figure reacts to the chosen 'group selection' and 'date range'."), hr()
)
tab.season <- tabPanel(tags$b("Seasonality"),
                       br(), plotOutput("period.ts", height = '500px', width = '80%'),
                       cap.season, hr()
)


#### SCATTER / CLUSTER ###
tab.scat <- tabPanel(tags$b("Pairwise scatter"),
                     plotOutput("scatter", height = '500px', width = '100%'),
                     cap.corr
)

tab.pca <- tabPanel(tags$b("PCA"), br(), plotOutput("PCA", height = '400px', width = '80%'),
                    cap.pca
)
tab.dist <- tabPanel(tags$b("Distributions"), br(),
                     plotOutput("plot.stats", height = '400px', width = '80%'),
                     crossfade.slider,
                     cap.dist
)

tab.corr <- tabPanel(tags$b("Comparative"), br(),
                     tabsetPanel(type = 'pills', tab.dist, tab.scat, tab.pca))



#### AUTOCORRELATION ###
tab.ACF <- tabPanel(tags$b("Autocorrelation"), br(),
                    plotOutput("ACF", height = '500px', width = '100%'),
                    slider.kdays,
                    cap.ACF, hr(),
                    # DT::dataTableOutput("table.acf")
)

tab.STL <- tabPanel(tags$b("Decomposition"), br(),
                    plotOutput("plot.STL"),# height = '500px', width = '100%'),
                    cap.STL, hr()
)
tab.auto <- tabPanel(tags$b("Statistics"), br(),
                     tabsetPanel(type = 'pills', tab.ACF, tab.STL))

#### WEATHER ###
t.all <- tabPanel(tags$b("All"), br(),
                  plotOutput("weather.combo", height = '500px', width = '90%'),
                  fluidRow(column(12, drop.combo)))
t.temp <- tabPanel(tags$b("Temperature"), plotOutput("Tmean", height = '500px', width = '95%'))
t.prep <- tabPanel(tags$b("Precipitation"), plotOutput("precip", height = '500px', width = '95%'))
t.wd <- tabPanel(tags$b("Wind direction"), plotOutput("wind.deg", height = '500px', width = '95%'))
t.ws <- tabPanel(tags$b("Wind speed"), plotOutput("wind.spd", height = '500px', width = '95%'))
tab.weather <- tabPanel(tags$b("Weather"), br(),
                        tabsetPanel(type = 'pills',
                                    t.all, t.temp, t.prep, t.wd, t.ws),
                        cap.weather, hr())
tab.map <- tabPanel(tags$b("Map"), br(),
                    fluidRow(column(12, leafletOutput("leaflet.map",width = "90%", height = "500px"))),
                    )



###########################     UI LAYOUT    ########################################################

ui <-
    fluidPage(theme = shinytheme("lumen"), #paper is good too, flatly, journal
              sidebarLayout(
                  # Sidebar
                  sidebarPanel(hr,
                               wellPanel(
                                         titlePanel(title = h2(tags$b(title.text)), windowTitle = "OttBikeCounters"),
                                         h4(em("time series analysis dashboard")),
                                         bike.icon, "24,080,530 and counting...", bike.icon
                               ),
                               fluidRow(
                                   column(12,
                                          column(11, date.slider),
                                          column(5, check.location),
                                          column(7, drop.location, drop.colours),
                                          column(11, h5("*Reactive inputs are noted in the captions for each figure."))
                                   )
                               ),
                               hr(), side.text
                  ),
                  mainPanel(br(),
                            tabsetPanel(type = 'tabs', tab.ts, tab.season, tab.weather,
                                        tab.corr, tab.auto, tab.map), br()
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
                   Date >= input$date.slider[1],
                   Date <= input$date.slider[2])
        return(df)
    })
    singleInput <- reactive({
        return(filter(bikes, location %in% input$drop.location))
    })
    weatherInput <- reactive({
        df = bike.weather %>%
            filter(location %in% input$check.location,
                   Date >= input$date.slider[1],
                   Date <= input$date.slider[2])
        return(df)
    })
    scatInput <- reactive({
        matrix <- bikes %>%
            filter(location %in% input$check.location,
                   Date >= input$date.slider[1],
                   Date <= input$date.slider[2]) %>%
            as_tibble() %>%
            mutate(location = as.factor(location)) %>%
            pivot_wider(id_cols = "Date",
                        names_from = "location",
                        values_from = 'count') %>%
            select(-c(1)) %>%
            mutate_if(is.character, as.numeric)
        return(matrix)
    })

    colorInput <- reactive({
        if (input$drop.colours == 'viridis'){color = "#187a58"}
        else if (input$drop.colours == 'magma'){color = "#d41387"}
        else if (input$drop.colours == 'cividis'){color = "#d6a000"}
        else if (input$drop.colours == 'inferno'){color = "#ff21ae"}
        return(color)
    })

    #   ############################        PLOTS        #######################################

    output$daily.ts <- renderPlot({
        locs <- length(input$check.location)
        dt <- input$date.slider
        if (locs < length(locations)){
            sub <- paste("from", locs, "Ottawa locations between", dt[1],'and', dt[2])
        } else {
            sub <- paste("from all Ottawa locations between", dt[1],'and', dt[2])
        }

        ts.daily.df <- multipleInput() %>%
            as_tibble() %>%

            filter(location %in% input$check.location,
                   Date >= input$date.slider[1],
                   Date <= input$date.slider[2])

        ggplot(ts.daily.df, aes(x = Date, y = count, fill=weekend, colour = weekend)) +
            geom_point(alpha = 1, size = 0.7, shape = 1) + # size = 0.42,
            # geom_col() +
            geom_hline(yintercept = 0, colour = 'grey', size = 0.6) +

            labs(title = "Daily bike counts time series",
                 subtitle = sub) +
            scale_color_viridis_d(option = input$drop.colours, direction = -1, end = 0.8,
                                  begin = 0.2) +
            basic_theme +
            facet_grid(location ~ ., scales = 'free') +
            guides(colour = guide_legend(override.aes = list(size=5, pch=15))) +
            theme(plot.margin =margin(0.5, 0, 0, 0, "cm"),
                  legend.position = 'bottom', legend.title = element_blank(),
                  axis.title.y = element_blank(),
                  axis.text.y.left = element_text(size = 8),
                  strip.text.y.right = element_text(size = 12, angle = 0, hjust = 0))
    })

    output$period.ts <- renderPlot({
        # summarise means
        period.df <- singleInput() %>%  fill_gaps() %>%  # need tsibble for gg_season
            filter(Date >= input$date.slider[1],
                   Date <= input$date.slider[2]) %>%
            mutate(days = as.Date(format(Date,"2015-%m-%d"),format="%Y-%m-%d"))
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
            ggplot(aes(x = days, y = count, group = year(Date), colour = year(Date))) +
            geom_path() +
            # gg_season(alpha = 0.8, size = 0.6, period = "year") +
            ylab("Daily count") +  basic_theme + geom_rangeframe(colour='black') +
            scale_color_viridis_c(option = input$drop.colours, labels=as.integer) +
            # guides(colour = guide_legend(override.aes = list(size=5, pch=15))) +
            theme(legend.position = 'right', legend.title = element_blank(),
                  axis.title.x = element_blank(),
                  axis.text.x = element_text(hjust = 1),
                  panel.grid.major.y = element_line(colour = "grey", size = 0.2),
                  plot.margin = margin(0.5, 0, 0.1, 0, "cm"))


        per3 <- ggplot(monthly, aes(x = as.numeric(year), y = avg)) +
            geom_hline(data = month.means, aes(yintercept = avg)) +
            geom_path(size = 1, alpha = 0.8, colour = colorInput()) +
            geom_point(size = 1, alpha = 0.8, colour = colorInput()) +
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
            geom_line(size = 1, alpha = 0.8, colour = colorInput()) +
            geom_point(size = 1, alpha = 0.8, colour = colorInput()) +
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
        dt <- input$date.slider
        sub <- paste('Annual and hebdomadal patterns and their change from', dt[1], 'to', dt[2])
        # ::patchwork
        # pwork <- (per1 | per2) / per3 / per4 + plot_annotation(labs(x = "Year", y = "Mean daily count"))
        pwork <- (per1) / per3 / per4 + plot_annotation(labs(x = "Year", y = "Mean daily count"))

        pwork + plot_annotation(
            title =  paste("What are the weekly and seasonal patterns at ", as.character(input$drop.location),"?", sep=''),
            subtitle = sub,
            theme = basic_theme,
            tag_levels = 'a') &
            theme(plot.tag = element_text(size = 12))

    })

    ### Four weather tabs ####
    output$Tmean <- renderPlot({
        weatherInput() %>%
            ggplot(aes(x=Tmean, y=count)) +
            geom_point(size = 0.03, alpha = 0.3) +
            geom_smooth(size=1, color=colorInput()) +
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
            geom_jitter(alpha=0.3, size=0.3) +
            geom_boxplot(alpha=0, colour='black', notch = TRUE, varwidth = TRUE) +
            # geom_point(y=mean(count), colour='black', size=4) +
            labs(y = "Daily count",
                 title = 'How does Ottawa bike traffic respond to climate stressors?',
                 subtitle = "Ottawa bike counts by daily total precipitation") +
            geom_rangeframe(colour='black') + facet_wrap(~location,scales="free") + basic_theme +

            #Rename the legend title and text labels.
            scale_color_viridis_d(option = input$drop.colours, name= "Precipitation (mm)",direction = -1, end = 0.8,
                                  labels = c("0", "< 5 ", "5 - 20", "20 +")) +
            # guides(fill = guide_legend(reverse = TRUE)) +
            # guides(colour = guide_legend(reverse = TRUE)) +
            guides(colour = guide_legend(override.aes = list(size=5, alpha=1, pch=15))) +
            guides(fill= guide_legend(override.aes = list(size=1, alpha=1, pch=15))) +
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
            geom_jitter(width = 4, size =0.3, alpha = 0.35) +
            # geom_density2d(aes(), geom = "polygon") +
            scale_color_viridis_c(option = input$drop.colours) +
            geom_smooth(size = 1, na.rm = TRUE, color = colorInput()) +
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
            geom_smooth(size =1, na.rm=TRUE, colour = colorInput()) +
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

    ### first combo weather tab ####
    output$weather.combo <- renderPlot({
        if (input$drop.combo == 'Selected group'){
            combo.df <- weatherInput()
            locs <- paste(length(input$check.location), "selected locations")
        } else if (input$drop.combo == 'All locations'){
            combo.df <- bike.weather %>%
                filter(Date >= input$date.slider[1],
                       Date <= input$date.slider[2])
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
            geom_smooth(size=1.2, color=colorInput()) +
            ylab("Daily count") +
            xlab(expression(paste("Mean temperature (", degree ~ C, ")"))) +
            geom_rangeframe(colour='black') + basic_theme +
            theme(strip.text = element_text(size=16))
        Pr <- combo.df %>%
            ggplot(aes(x = Precipitation, y=count, colour = Precipitation)) +
            # geom_violin(color='black', size = 0.25) +
            # geom_tufteboxplot(median.type = "line", hoffset = 0, width = 5) +
            geom_jitter(alpha=0.2, size=0.3) +
            geom_boxplot(alpha=0, colour='black', notch = TRUE, varwidth = TRUE) +
            # xlab("Precipitation (mm)") +
            scale_x_discrete(labels = c("-" = "0 mm", "+" = "< 5 mm",
                                        "++"="5-10 mm", "+++" = "10 +")) +
            scale_color_viridis_d(option = input$drop.colours, name= "Precipitation (mm)",direction = -1, end =0.8) +
                                 # labels = c("0", "< 5 ", "5 - 10", "10 +")) +
            guides(colour = guide_legend(override.aes = list(size=5, alpha=1))) +
            # guides(fill= guide_legend(override.aes = list(size=1, alpha=1, shape=0))) +            scale_color_discrete(name= "Precip. (mm)",
                                 # labels = c("0", "< 5 ", "5 - 20", "20 +")) +
            geom_rangeframe(colour='black') + basic_theme +
            theme(legend.position = 'none',
                # axis.text.x = element_blank(),
                  # axis.text.x = element_text(angle = 1, size=12),
                  axis.title.y = element_blank())
        Wd <- combo.df %>%
            ggplot(aes(x = Wind.deg, y = count)) +
            geom_jitter(width = 4, size =0.3, alpha =0.35) +
            geom_smooth(size = 1.2, na.rm = TRUE, color = colorInput()) +
            labs(y = "Daily count", x = "Wind direction") +
            xlim(c(0,360)) + ylim(limit.count) +
            geom_rangeframe(colour='black') + coord_polar() +
            basic_theme + geom_rangeframe(colour='black') +
            theme( #axis.title.x = element_blank(),
                  plot.margin = margin(0, 1, -0.3, 0, "cm"))
        Ws <- combo.df %>%
            ggplot(aes(x= Wind.speed, y = count)) +
            geom_jitter(size = 0.3, alpha = 0.19, width = 1) +
            geom_smooth(size =1.2, na.rm=TRUE, colour = colorInput()) +
            geom_point(aes(x=0,y=0), size =1, color = 'red') +
            xlab("Wind speed (km/h)") +
            basic_theme + geom_rangeframe(colour='black') +
            xlim(c(28,62)) + # ylim(limit.count) +
            theme(legend.position = "none",
                  axis.title.y = element_blank(),
                  plot.margin = margin(0, 0, 0, 0, "cm"))

        dt <- input$date.slider
        # ::patchwork.weather
        (Ws + Tm + Wd + Pr) + plot_layout(widths = c(1, 1), heights = c(1,1)) +
            plot_annotation(
                subtitle =  paste("Bike counts by weather at", locs, "during",dt[1],"-",dt[2]),
                title = 'How does Ottawa bike traffic respond to climate stressors?',
                caption = 'Data source: City of Ottawa & Environment Canada',
                theme = basic_theme)



    })

    ### Scatter xy grid and heatmap/dendrogram ####
    output$scatter <- renderPlot({
        pairs(scatInput(), pch=1, cex=0.1, alpha=0.5, lower.panel = NULL)
    })


    ### ACF, PACF plots ####
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
                title = 'How strongly are counts at each location correlated to prior observations?',
                subtitle =  paste("ACF and PACF of bike counts between ", dt[1], 'and', dt[2]),
                theme = basic_theme
                )
    })

    ### STL ####
    output$plot.STL <- renderPlot({

        # d.range <- c(max(min()))
        stl.df <- bikes %>%
            filter(location %in% input$drop.location) %>%
            filter(!is.na(count)) %>%
            as_tsibble() %>% fill_gaps() %>%
            mutate(count = na.interp(count))# %>%
        lower <- max(c(min(stl.df$Date), input$date.slider[1])) - months(4)
        upper <- min(c(max(stl.df$Date), input$date.slider[2]))
        stl.df %>%
            model(STL(count ~ season(window=13), # + season(window='periodic'),
                      robust = TRUE)) %>%
            components() %>%
            autoplot() +
            xlim(c(lower, upper)) + ###
            labs(title =  paste("STL decomposition of",input$drop.location,
                                "bike counter time series"),
                subtitle = 'Count ~ trend + annual season + weekly season + remainder',
                theme = basic_theme) +
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

    ### Plot stats ####

    output$plot.stats <-  renderPlot({
        multipleInput() %>%
            ggplot(aes(x=weekend, colour=weekend, y=count)) +
            geom_boxplot(alpha = 1-input$crossfade, notch = TRUE, varwidth = TRUE) +
            geom_tufteboxplot(size = 1.4,whisker.type = "point",
                              alpha = 1-input$crossfade) +
            geom_jitter(size=0.05, alpha = input$crossfade) +
            coord_flip() +
            facet_grid(location~., switch = "y") + basic_theme +
            theme(axis.title.y = element_blank(),
                  axis.text.y = element_blank(),
                  axis.ticks.y = element_blank(),
                  legend.title = element_blank(),
                  strip.text.y.left = element_text(size = 12, angle = 0, hjust = 0))
    })

}


################################### APP CALL ##################################################
# Run the application
shinyApp(ui = ui, server = server)


