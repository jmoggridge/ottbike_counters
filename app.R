
# Ottawa bike counters dashboard

library(shiny)
library(dplyr)
library(ggthemes)
library(leaflet)
library(shinythemes)
library(forecast)
library(feasts)
library(tsibble)
library(kableExtra)
library(ggplot2)
library(rcartocolor)
library(extrafont)
library(ggcorrplot)
library(gridExtra)
library(viridis)
library(gplots)
library(ggfortify)

source("./global.R")

# makes all the horizontal line show up
hr <-  tags$head(tags$style(HTML("hr {border-top: 1px solid #000000;}")))

#### Tab captions

cap.corr <- p("Scatterplots showing pairwise similarity between counter locations
                through shared traffic patterns")
cap.ACF <- p("Correlograms showing the linear relationship between bike counts
                     across lagging durations from 1 - 14 days within each time series.
                     Regular wavy patterns indicates weekly cyclic .")
cap.PACF <- p("****I don't know how to interpret this yet!!!!***")

cap.cycl <-  p("Patterns might indicate which routes are predominantly
             used either by commuters or leisure cyclists, based on the weekday/weekend cyclicity and
             peak summer volume; eg. compare Alexandra or Canal Ritz to Laurier/Metcalfe")

cap.clust <- p("Tree and heatmap showing similarity in usage between locations; the
            partitioning generally agrees with proximity.
            Clustering by UPGMA with Euclidean distance metric.  OTrain/Young, Portage, and
            Laurier/Bay were omitted, not enough data.")
cap.arima <- p("An ARIMA forecast model for each bike counter that was operational in 2019")

## Interactive doodads
dropdown.ts <-
    selectInput("drop.ts", "Select location:", locations,
                selected = "Ottawa River")
dropdown.cyclic.loc <-
    selectInput("drop.cyclic", "Select location:", locations,
                selected = "Ottawa River")
check.scat <- checkboxGroupInput(
    "check.scat", "Add/remove locations:", locations, selected = locations[c(1:4,7,8,11)],
    inline = TRUE)

check.ACF <- checkboxGroupInput(
    "check.ACF", "Add/remove locations:", locations, selected = locations[c(1:4,7,8,11)],
    inline = TRUE)
check.PACF <- checkboxGroupInput(
    "check.PCF", "Add/remove locations:", locations, selected = locations[c(1:4,7,8,11)],
    inline = TRUE)

date.range <-   dateRangeInput("date.range", "Date range:",
                               start  = "2010-01-01", end    = "2019-09-30",
                               min    = "2010-01-01", max    = "2019-09-30",
                               format = "yyyy/mm", separator = " - ",
                               startview = "decade")
####

#######################
# UI TABS

tab.ts <- tabPanel(tags$b("Time series"),
             h3("Total daily riders over time at individual counters"), hr(),
             plotOutput("daily.ts"), hr(),
             fluidRow(column(5, dropdown.ts), column(5, date.range)), br()
             )
tab.cyclic <- tabPanel(tags$b("Cyclicity"),
             h3("Annual and weekly seasonality at each bike counter"), hr(),
             plotOutput("period.ts", height = '460px', width = '90%'), hr(),
             fluidRow(column(6, dropdown.cyclic.loc)), hr(),
              cap.cycl, br()
             )
tab.ACF <- tabPanel(tags$b("ACF"),
                     h3("Autocorrelation functions for each bike counter"), hr(),
                     plotOutput("ACF", height = '600px', width = '90%'),
                    fluidRow(column(12, check.ACF)), hr(),
                     cap.ACF, br()
                     )
tab.PACF <- tabPanel(tags$b("PACF"),
                    h3("Partial autocorrelation functions for each bike counter"), hr(),
                    plotOutput("PACF"), fluidRow(column(12, check.PACF)), hr(),
                    cap.PACF, br()
                    )
tab.corr <- tabPanel(tags$b("Correlation"),
             h3("Pairwise scatter plots of all bike counters"), hr(),
             plotOutput("scatter", height = '660px', width = '90%'),
             fluidRow(column(12, check.scat)),
             cap.corr, br()
             )

tab.clust <- tabPanel(tags$b("Clustering"),
                      h3("Hierarchical clustering of Ottawa bike counters"), hr(),
                      plotOutput("heatmap", height = '500px', width = '90%'), br(),
                      cap.clust, br()
                      )


tab.arima <- tabPanel(tags$b("Forecasting"),
                    h3("ARIMA forecasting the OttBike future"), hr(),
                    br()
                    )


###############################################################################

ui <- fluidPage(theme = shinytheme("readable"), #paper is good
    sidebarLayout(
        ### Sidebar
        sidebarPanel(hr,hr(),
                     titlePanel("OttBike counters"), hr(),
                     fluidRow(column(12, leafletOutput("leaflet.map",width = "98%", height = "300px"))),
                     byline, hr()
        ),
        ### Main
        mainPanel(br(), tabsetPanel(tab.ts, tab.cyclic, tab.ACF,
                                    tab.corr, tab.clust, tab.arima),
                  br(),br(),br(),br())
    )
)

###############################################################################
# Define server logic required to draw a histogram
server <- function(input, output) {


    ## Map ###########
    output$leaflet.map <- renderLeaflet({
        leaflet() %>%
            addProviderTiles(providers$Stamen.TonerLite,
                             options = providerTileOptions(noWrap = TRUE)) %>%
            addMarkers(data = geo[, c(2,3)],label =  geo$locations)
    })

    ## Inputs #########
    tsInput <- reactive({
        df = bikes %>%
            filter(location == input$drop.ts) %>%
            filter(Date >= input$date.range[1]) %>%
            filter(Date <= input$date.range[2])
        return(df)
    })

    cyclicInput <- reactive({
        return(filter(bikes, location == input$drop.cyclic))
    })

    scatInput <- reactive({
        matrix <- bikes %>%
            filter(location %in% input$check.scat) %>%
            as_tibble() %>%
            mutate(location = as.factor(location)) %>%
            pivot_wider(id_cols = "Date",
                        names_from = "location",
                        values_from = 'count') %>%
            select(-c(1)) %>%
            mutate_if(is.character, as.numeric)
        return(matrix)
    })

    ####### PLOTS #########

    output$daily.ts <- renderPlot({
        title <- paste("Daily total riders at ", as.character(input$drop.ts))
        ggplot(tsInput(), aes(x=Date, y=count)) +
            geom_line(alpha = 0.8, size = 0.4) +
            ylab("Riders") + basic_theme + geom_rangeframe() +
            ggtitle(title)
    })
    output$period.ts <- renderPlot({
        cycle1 <-
            cyclicInput() %>% fill_gaps() %>%
                gg_season(alpha = 0.55, size = 0.4, period = "year") +
                ylab("Riders") + basic_theme + geom_rangeframe() +
                scale_color_viridis_c() +
                theme(legend.position = 'none',
                      axis.title.x = element_blank(),
                      plot.margin = margin(0.5, 0, 2, 0, "cm"))
        cycle2 <-
            cyclicInput() %>% fill_gaps() %>%
                gg_season(alpha = 0.74, period = "week") +
                ylab("Riders") + basic_theme + geom_rangeframe() +
                scale_color_viridis_c() +
                theme(legend.position = 'none',
                      axis.title.x = element_blank(),
                      axis.text.x = element_text(angle = 35, vjust = 0.5)
                      )
        grid.arrange(cycle1, cycle2, nrow = 1)
    })
    output$scatter <- renderPlot({
        pairs(scatInput(), pch=19, cex=0.2, lower.panel = NULL)
    })
    output$heatmap <- renderPlot({
        matrix <- bikes %>% as_tibble() %>%
            mutate(location = as.factor(location)) %>%
            pivot_wider(id_cols = "Date",
                        names_from = "location",
                        values_from = 'count') %>%
            select(-c(1, 5, 10, 12)) %>% mutate_if(is.character, as.numeric) %>%
            as.matrix()
        coul <- viridis_pal(alpha = 1, begin = 0, end = 1, direction = 1,
                            option = "A")(1000)
        b <- rcorr(matrix)
        heatmap.2(b$r, scale="column", trace='none', col = coul,
                  distfun = function(x) dist(x, method="euclidean"),
                  hclustfun = function(x) hclust(x, method="average"),
                  margins = c(10, 12)) # use margins to avoid cutting off labels
    })
    output$ACF <- renderPlot({
        bikes %>% fill_gaps() %>%
            filter(location %in% input$check.ACF) %>%
            ACF(count) %>% autoplot() + geom_point() +
            basic_theme +
            xlab("Lagging days") + ylab("Autocorrelation coefficient") +
            xlim(c(0,28)) +
            facet_labels
    })
    output$PACF <- renderPlot({
        bikes %>% fill_gaps() %>%
            filter(location %in% input$check.PACF) %>%
            PACF(count) %>% autoplot() + geom_point() +
            basic_theme +
            xlab("Lagging days") + ylab("PACF")
    })
}


############################################################################

# Run the application
shinyApp(ui = ui, server = server)




##############################################################################























# tab.cyclic <-
#     tabPanel(tags$b("Cyclic"),
#              br(),
#              fluidRow(column(5, dropdown.cyclic.loc),
#                       column(5, dropdown.cyclic.per)
#                       ),
#              plotOutput("period.ts"), br(), br()
#              )
# output$period.ts <- renderPlot({
#     cyclicInput() %>% fill_gaps() %>%
#         gg_season(alpha = 0.74, period = input$period) +
#         ylab("Riders") +
#         basic_theme + geom_rangeframe()
# })
# output$scatter <- renderPlot({
#     scatInput() %>% plot()
# })
#
# check.clust <- checkboxGroupInput(
#     "check.clust", "Add/remove locations:", locations, selected = locations,
#     inline = TRUE)

# dropdown.cyclic.loc <-
#     selectInput("drop.cyclic", "Bike counter:", locations,
#                 selected = "Ottawa River")
# dropdown.cyclic.per <-
#     radioButtons("period", "Period",
#                  list("Annual" = 'year', "Weekly"= 'week'),
#                  selected = "Annual")



# clustInput <- reactive({
#    matrix <- bikes %>%
#         filter(location %in% input$check.clust) %>%
#         as_tibble() %>%
#         mutate(location = as.factor(location)) %>%
#         pivot_wider(id_cols = "Date",
#                     names_from = "location",
#                     values_from = 'count') %>%
#         select(-c(1)) %>%
#        mutate_if(is.character, as.numeric)
#     return(matrix)
# })






