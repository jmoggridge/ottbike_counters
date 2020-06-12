bikes <- readRDS("./data/ottbike_counters.ts.RDS")
geo <- read.csv("./data/locations.csv")

locations <- c("Adawé", "Alexandra", "Canal Ritz", "Colonel By",
               "Laurier/Bay",  "Laurier/Lyon", "Laurier/Metcalfe",
               "OTrain/Bayview", "OTrain/Gladstone", "OTrain/Young",
               "Ottawa River", "Portage", "Somerset")


tweet.link <-
  tags$div(' Created by JA Moggridge',
           a(href = "https://twitter.com/quaxlikeaduck",
             HTML('<i class="fa fa-twitter" style = "color:#FFFFFF;"></i>')),
            a(href = "https://github.com/jmoggridge/ottbike_counters",
              HTML(' Github <i class="fa fa-github" style = "color:#90D13E;"></i>')
              )
            )

cc.link <-p(a(href ="https://creativecommons.org/licenses/by-sa/4.0/"," CC-SA-4.0 "),
            "Source:",
          a(href = "https://open.ottawa.ca/datasets/bicycle-trip-counters",
           " City of Ottawa.")
          )

byline <- p(tweet.link, cc.link)



####################3



col_discrete <- scale_colour_carto_d(name = "", type = 'qualitative', direction = 1)
fill_discrete <- scale_fill_carto_d(name = "", type = 'qualitative', direction = 1)
basic_theme <- theme_void() + theme_tufte(16, "Century")

# other fonts
  # century schoolbook
  # corbel

# how to get light lines for tufte outline?


facet_labels <- theme(strip.text = element_text(face = 'bold', colour = 'deepskyblue4', size = 12), #face = "bold"
                      strip.background = element_rect(fill = "white", colour = "white", size = 1))
no_facet_labs <- theme(strip.background = element_blank(), strip.text.x = element_blank())

legend_format <- theme(legend.text = element_text(size = 11)) #face = "bold",
base_x <- geom_hline(yintercept = -1, color= 'black')
base_y <- geom_vline(xintercept = 0, colour = 'black')
log10x <- scale_x_continuous(trans='log10')

## Basic plot lines  for count threshold
threshold <- geom_hline(yintercept = c(100,200), linetype = "longdash", color = "black", size = 0.3, alpha = 0.7)
thresholdv <- geom_vline(xintercept=c(100, 200), linetype="longdash", color = "black", size = 0.3, alpha = 0.7)

# for timeseries first of months' lines
first_of_month <- geom_vline(xintercept = c(182, 213, 242), color = "darkgrey", alpha = 0.3, size = 2.5)
annot <- data.frame(x=c(170,200,230), y=seq(1015,1015,3), label = c('June', 'July', 'Aug'))
