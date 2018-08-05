library(shiny)
library(tidyverse)
library(geojsonio)
library(leaflet)
library(rsconnect)

rsconnect::setAccountInfo(name='lunaxc',
                          token='D21EC22BB6C000025D0E8F141007505C',
                          secret='5XGfFsK9G7eev8Qx3FOeJqUdHryrQ5e/GCssceWG')


PCA1 <- readRDS("census_tract_r.rds")
PCA1Com <- PCA1[PCA1$Indicator == "Communication",]
PCA1Econ <- PCA1[PCA1$Indicator == "Economic Status",]
PCA1HW <- PCA1[PCA1$Indicator == "Health and Wellness",]
t_value = NULL
for (i in 1:length(PCA1Com$GeoID)){
  t_value[i] = mean(c(PCA1Com$Value[i], PCA1Econ$Value[i], PCA1HW$Value[i]))
}
PCA1TT <- data.frame(GeoID = PCA1Com$GeoID, Value = t_value, 
                     Indicator = rep("Happiness Index", length(PCA1Com$Value)))
boston1 <- geojson_read( "census.geojson.json", what = "sp")
PCA1_C1 <- PCA1Com[match(as.character(boston1$GEOID10), PCA1Com$GeoID),]
PCA1_C2 <- PCA1Econ[match(as.character(boston1$GEOID10), PCA1Econ$GeoID),]
PCA1_C3 <- PCA1HW[match(as.character(boston1$GEOID10), PCA1HW$GeoID),]
PCA1_C4 <- PCA1TT[match(as.character(boston1$GEOID10), PCA1TT$GeoID),]
PCA1_C = rbind(PCA1_C1, PCA1_C2, PCA1_C3, PCA1_C4)


PCA11 <- readRDS("neighbour_r.rds")
PCA11Com <- PCA11[PCA11$Indicator == "Communication",]
PCA11Econ <- PCA11[PCA11$Indicator == "Economic Status",]
PCA11HW <- PCA11[PCA11$Indicator == "Health and Wellness",]
tt_value = NULL
for (i in 1:length(PCA11Com$Neighbour)){
  tt_value[i] = mean(c(PCA11Com$Value[i], PCA11Econ$Value[i], PCA11HW$Value[i]))
}
PCA11TT <- data.frame(Neighbour = PCA11Com$Neighbour, Value = tt_value, 
                     Indicator = rep("Happiness Index", length(PCA11Com$Value)))
boston2 <- geojson_read( "Boston_Neighborhoods.geojson", what = "sp")
PCA1_N1 <- PCA11Com[match(as.character(boston2$Name), PCA11Com$Neighbour),]
PCA1_N2 <- PCA11Econ[match(as.character(boston2$Name), PCA11Econ$Neighbour),]
PCA1_N3 <- PCA11HW[match(as.character(boston2$Name), PCA11HW$Neighbour),]
PCA1_N4 <- PCA11TT[match(as.character(boston2$Name), PCA11TT$Neighbour),]
PCA1_N = rbind(PCA1_N1, PCA1_N2, PCA1_N3, PCA1_N4)

city = round(mean(PCA1_N$Value), 2)


ui <- fluidPage(
  titlePanel("Boston Relative Happiness Index"),  
  sidebarLayout(
    sidebarPanel(
      h3("Boston City-Level Relative Happiness Index: ", city),
      br(), br(),
      uiOutput("IndicatorOutput"),
      br(), br(),
      em("The higher the score, the higher the relative happiness"),
      br(), br(),
      hr(style="border-color: black;"), 
      h6("Data Source: Analyze Boston, U.S. Census Bureau")
    ),
    mainPanel(
      h3("Census Tract Level"),
      leafletOutput("Censustractplot"),
      br(),br(),
      
      h3("Neighborhood Level"),
      leafletOutput("neighborplot"),
      br(),br(),
      
      tableOutput("results")
    )
  )
)

server <- function(input, output) {
  
  
  output$IndicatorOutput <- renderUI({
    selectInput("IndicatorInput", "Indicator",
                sort(unique(PCA1_N$Indicator)),
                selected = "Happiness Index")
  })
  
  filtered <- reactive({
    if(is.null(input$IndicatorInput)){
      return(NULL)
    }
    PCA1_N %>%
      filter(
        Indicator == input$IndicatorInput
      )
  })
  
  select <- reactive({
    input$IndicatorInput
  })
  
  output$Censustractplot <- renderLeaflet({
    if(is.null(filtered())){
      return()
    }
    
    PCA1_C <- PCA1_C %>%
      filter(Indicator == select()) 
    
    pal <- colorNumeric(
      palette = "RdPu",
      domain = c(0:10),
      reverse = FALSE)
    
    
    leaflet(boston1) %>%
      addTiles() %>%
      addPolygons(stroke = FALSE, smoothFactor = 0.3, fillOpacity = 1,
                  fillColor = ~pal(10 - PCA1_C$Value),
                  label = ~paste0(formatC(PCA1_C$Value, big.mark = ","))) %>%
      addLegend(pal = pal, position = 'bottomright', values = c(0,10), 
                title= "Happiness Index", opacity = 1.0, 
                labFormat = labelFormat(transform = function(x) sort(x, decreasing = TRUE))) %>% 
      addLegend(position = "bottomleft", title = "Data Source: Analyze Boston, U.S. Census Bureau", labels = NULL, colors = NULL, opacity = 0)
    
  })
  
  output$neighborplot <- renderLeaflet({
    if(is.null(filtered())){
      return()
    }
    
    PCA1_N <- PCA1_N %>%
      filter(Indicator == select())
    
    pal <- colorNumeric(
      palette = "PuRd",
      domain = c(0:10),
      reverse = FALSE)
    
    leaflet(boston2) %>%
      addTiles() %>%
      addPolygons(stroke = FALSE, smoothFactor = 0.3, fillOpacity = 1,
                  fillColor = ~pal(10 - PCA1_N$Value),
                  label = ~paste0(PCA1_N$Neighbour, ": ", formatC(PCA1_N$Value, big.mark = ","))) %>%
      addLegend(pal = pal, position = 'bottomright', values = c(0:10), 
                title= "Happiness Index", opacity = 1.0,
                labFormat = labelFormat(transform = function(x) sort(x, decreasing = TRUE))) %>% 
      addLegend(position = "bottomleft", title = "Data Source: Analyze Boston, U.S. Census Bureau", labels = NULL, colors = NULL, opacity = 0)
    
  })
  
  
  output$results <- renderTable({
    p = filtered()
    p[with(p, order(-Value)),]
  })
  
  
  
  
}

shinyApp(ui = ui, server = server)
