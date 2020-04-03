#load libraries
library(shiny)
library(shinyMobile)
library(shinydashboard)
library(leaflet)
library(dplyr)
library(leaflet.extras)
library(shinyWidgets)
library(data.table)
library(rvest)
library(lubridate)
library(htmltools)
library(taskscheduleR)

myscript <- system.file("extdata", "earthquake.R", package = "taskscheduleR")

taskscheduler_create(taskname = "update_BMKG_12HRS", rscript = myscript,
                     schedule = "HOURLY", starttime = "00:00", modifier = 12)

setwd("D:/Documents/Coding/QuakeUpV2Final")
#FETCHING CURRENT DATA
url <- 'https://www.bmkg.go.id/gempabumi/gempabumi-terkini.bmkg'

out_df <- url %>% read_html() %>% html_table() %>% .[[1]]
head(out_df)
write.csv(out_df, 'current_earthquake_data.csv', row.names = FALSE)

data <- read.csv('current_earthquake_data.csv')

#EARTHQUAKE
data$depth_type <-  ifelse(data$Kedalaman <= 70, "shallow", 
                           ifelse(data$Kedalaman <= 300 | out_df$KEDALAMAN >70, "intermediate", 
                                  ifelse(data$Kedalaman > 300, "deep", "other")))


#CONVERT MONEY
GetExchangeRates <- function(from, to, dt=Sys.Date()) {
    require(quantmod)
    obj.names <- getFX(paste0(from, "/", to), from=dt, to=dt)
    result <- numeric(length(obj.names))
    names(result) <- obj.names
    for (obj.name in obj.names) {
        result[obj.name] <- as.numeric(get(obj.name))[1]
        # Clean up    
        rm(obj.name)
    }
    return(result)
}

TestExchangeRates <- function() {
    from <- c("CAD", "JPY", "USD")
    to <- c("USD", "USD", "EUR")
    GetExchangeRates(from, to)
}



#UI
shiny::shinyApp(
    ui=f7Page(
        title="Tab Layout",
        f7TabLayout(
            panels=tagList(
                f7Panel(side="left", theme="light", f7PanelMenu(
                    id = "menu",
                    f7PanelItem(tabName = "eqNow", title = "Now", icon = f7Icon("email"), active = TRUE ),
                    f7PanelItem(tabName = "eqForecast", title = "Forecast", icon = f7Icon("home")),
                    f7PanelItem(tabName = "eqHistory", title = "History", icon = f7Icon("home")))),
            ),
            navbar=f7Navbar(
                title="QuakeUp Dashboard",
                left_panel=TRUE,
                right_panel=FALSE
            ),
            
            f7Tabs(
                animated=TRUE,
                id="tabs",
                f7Tab(
                    tabName="Map Of Indonesia",
                    icon=f7Icon("world"),
                    active=TRUE,
                    ui <- fluidPage(
                        mainPanel( 
                            #this will create a space for us to display our map
                            leafletOutput(outputId = "mymap", width="1900", height="860"),
                            #this allows me to put the checkmarks ontop of the map to allow people to view earthquake depth or overlay a heatmap
                            absolutePanel(bottom=50, left = 20, setBackgroundColor("white"), prettyCheckbox(
                                inputId = "markers", label = "Depth",animation = "pulse",
                                status = "success", outline = TRUE,  bigger = FALSE, inline=TRUE, thick = TRUE
                            ),
                            prettyCheckbox(
                                inputId = "heat", label = "uHeatmap", animation = "pulse", thick= TRUE,
                                status = "success", outline = TRUE, inline=TRUE
                            )
                            
                            )
                        ), 
                        
                        absolutePanel(top=100, right=50, dropdownButton(  numericInput(inputId = "days_input", label = "Input Number Of Days Prediction!", 30, min = 1, max = 365),
                                                                          verbatimTextOutput(outputId = "days_output"), 
                                                                          circle=TRUE, status="danger", icon=icon("gear"), right=TRUE)))
                    
                    
                ), 
                f7Tab(
                    tabName = "Weather",
                    icon = f7Icon("cloud_upload"),
                    active = FALSE,
                    f7Shadow(
                        intensity = 10,
                        hover = TRUE,
                        f7Card(
                            title = "Card header",
                            
                            footer = tagList(
                                f7Button(color = "blue", label = "My button", src = "https://www.google.com"),
                                f7Badge("Badge", color = "green")
                            )
                        )
                    )
                ),
                f7Tab(
                    tabName = "Currency",
                    icon = f7Icon("money_dollar"),
                    active = FALSE,
                    f7Shadow(
                        intensity = 10,
                        hover = TRUE,
                        f7Card(
                            title = "Currency Converter", 
                            ui <- fluidPage(
                                titlePanel("CURRENCY CONVERTER"), mainPanel( pickerInput(
                                    inputId = "caption",
                                    label = "EUR -> ",
                                    choices = paste( c("IDR", "USD", "EUR")),
                                    multiple = FALSE,
                                    selected = "IDR",
                                    choicesOpt = list(
                                        content = sprintf(
                                            paste( c("IDR", "USD", "EUR"))))
                                ), verbatimTextOutput("value"),numericInput("currencyNumber", "Currency Number", 10),
                                verbatimTextOutput("convert")), actionButton("convert", "CONVERT")
                            )
                            
                        )
                    )
                )
            )
        ) ),
    
    #EARTHQUAKE DATA
    server <- function(input, output, session) {
        #define the color pallate for the magnitidue of the earthquake
        pal <- colorNumeric(
            palette = c('gold', 'orange', 'dark orange', 'orange red', 'red', 'dark red'),
            domain = data$Magnitudo)
        
        #define the color of for the depth of the earquakes
        pal2 <- colorFactor(
            palette = c('blue', 'yellow', 'red'),
            domain = data$depth_type
        )
        
        #create the map
        output$mymap <- renderLeaflet({
            leaflet(data, width="1000", height="1000") %>% 
                setView(lng =  113.9213257, lat = -0.789275, zoom = 4.5)  %>% #setting the view over ~ center of Indonesia
                addTiles() %>%
                addFullscreenControl() %>% 
                addCircles(data = data, lat = ~ Lintang, lng = ~ Bujur, weight = 1, radius = ~sqrt(Magnitudo)*25000, popup = paste0("Wilayah: ", data$Wilayah, "<br>", "Lintang: ", data$Lintang, "<br>", "Bujur: ", data$Bujur, "<br>", "Kedalaman: ", data$Kedalaman, "<br>", "Waktu Gempa: ", data$Waktu.Gempa, "<br>", "Magnitudo: ", data$Magnitudo), label = ~as.character(paste0("Magnitude: ", sep = " ", Magnitudo)), color = ~pal(Magnitudo), fillOpacity = 0.5)
        })
        
        #create the text output
        output$days_output <- renderText( {input$days_input} )
        
        #next we use the observe function to make the checkboxes dynamic. If you leave this part out you will see that the checkboxes, when clicked on the first time, display our filters...But if you then uncheck them they stay on. So we need to tell the server to update the map when the checkboxes are unchecked.
        observe({
            proxy <- leafletProxy("mymap", data = data)
            proxy %>% clearMarkers()
            if (input$markers) {
                proxy %>% addCircleMarkers(stroke = FALSE, color = ~pal2(depth_type), fillOpacity = 0.2, label = ~as.character(paste0("Magnitude: ", sep = " ", Magnitudo))) %>%
                    addLegend("bottomright", pal = pal2, values = data$depth_type,
                              title = "Depth Type",
                              opacity = 1)}
            else {
                proxy %>% clearMarkers() %>% clearControls()
            }
        })
        
        observe({
            proxy <- leafletProxy("mymap", data = data)
            proxy %>% clearMarkers()
            if (input$heat) {
                proxy %>%  addHeatmap(lng=~Bujur, lat=~Lintang, intensity = ~Magnitudo, blur =  10, max = 0.05, radius = 15) 
            }
            else{
                proxy %>% clearHeatmap()
            }
            
            
        })
        
        observeEvent(input$convert, {
            currencyName<-input$caption
            rowNumberInputData <- which(grepl(currencyName, mydata$name))
            currency <- mydata[rowNumberInputData, "value"]
            
            
            # output$table <- DT::renderDataTable(DT::datatable({liveish_data()}))
            output$value <- renderPrint({ mydata[which(mydata$name==input$caption),]})
            
            output$convert<- renderPrint({ currency*input$currencyNumber })
        })
        
        
        
    }
)