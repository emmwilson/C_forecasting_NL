#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
library(leaflet)
library(terra)

roads <- vect("/Users/emmersonEmmerson/Documents/Master's/C_forecasting_NL/practice_map/highways_nl.shp") %>% 
  project("EPSG:4326 - WGS 84")

parks <- vect("/Users/emmersonEmmerson/Documents/Master's/C_forecasting_NL/practice_map/parks_nl.shp") %>% 
  project("EPSG:4326 - WGS 84")

GM_c <- rast("/Users/emmersonEmmerson/Documents/Master's/C_forecasting_NL/code_for_stats/clean code/data created/GM_predict_tot_bm.tif")
TN_c <- rast("/Users/emmersonEmmerson/Documents/Master's/C_forecasting_NL/code_for_stats/clean code/data created/TN_predict_tot_bm.tif")
GM_g <- rast("/Users/emmersonEmmerson/Documents/Master's/C_forecasting_NL/code_for_stats/clean code/data created/GM_predict_gap_bm.tif")
TN_g <- rast("/Users/emmersonEmmerson/Documents/Master's/C_forecasting_NL/code_for_stats/clean code/data created/TN_predict_gap_bm.tif")

GMc.pal <- colorNumeric(c("#faf6ef", "#ed6f66", "#8a1c18", "#6c100d"), domain = values(GM_c), na.color = "transparent")
TNc.pal <- colorNumeric(c("#faf6ef", "#ed6f66", "#8a1c18", "#6c100d"), domain = values(TN_c), na.color = "transparent")



GMg.bins <- c(0.0006427, 0.2082801, 0.999221)
GMg.pal <- colorBin(c("#faf6ef", "#f7eedf", "#de5714"), bins = GMg.bins, na.color = "transparent")


# Define UI for application that draws a histogram
ui <- fluidPage(
  leafletOutput("mymap"),
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$mymap <- renderLeaflet({
      leaflet() %>%
        addProviderTiles('Esri.WorldImagery') %>%
        addMapPane("parks", zIndex = 410) %>% 
        addMapPane("output", zIndex = 420) %>% 
        addMapPane("roads", zIndex = 425) %>%
        addPolylines(data = roads, color = "#efbc15", opacity = 1, weight = 1.25,
                     options = pathOptions(pane = "roads")) %>%
        addPolygons(data = parks, color = "#1f78b4", weight = 1.25, fillColor = "#1f78b4", fillOpacity = 0.5,
                    options = pathOptions(pane = "parks")) %>% 
        addRasterImage(x = GM_c , 
                       color = GMc.pal,
                       opacity = 1,
                       group = "GM_c",
                       options = pathOptions(pane = "output")) %>%
        addLegend(pal = GMc.pal, values = values(GM_c),
          title = "Total Carbon (g/m<sup>2</sup>)",
                  group = "GM_c") %>% 
        addRasterImage(x = TN_c , 
                       color = TNc.pal,
                       opacity = 1,
                       group = "TN_c",
                       options = pathOptions(pane = "output")) %>%
        addLegend(pal = TNc.pal, values = values(TN_c),
                  title = "Total Carbon (g/m<sup>2</sup>)",
                  group = "TN_c") %>% 
        fitBounds(-58.1472419346211851,48.3868948600969233, -53.6884152774837418,49.9749083294476151) %>% 
        addLayersControl(overlayGroups = c("GM_c", "TN_c"), position = "topleft", options = layersControlOptions(collapsed = F))

    })
}

# Run the application 
shinyApp(ui = ui, server = server)
