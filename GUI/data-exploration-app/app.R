library(SoilDataR) #library(devtools); install_github("ktoddbrown/soilDataR")
library(ggplot2)
library(lubridate)
library(dplyr)
library(scales)
library(ggmap)
library(shinythemes)
library(DT)
library(stringr)
library(data.table)


source("ISCNapp_source.R")

ui <-   tagList(
  tags$style(
    type = "text/css",
    ".shiny-output-error { visibility: hidden; }",
    ".shiny-output-error:before { visibility: hidden; }" # suppressing error messages in app
  ),
  fluidPage(
    theme = shinytheme("spacelab"),
    # use theme explorer to choose new theme
    titlePanel(
      windowTitle = "Explore the ISCN Database",
      title = div(
        img(src = "ISCN_header.png", height = 100),
        "Explore the International Soil Carbon Network Database"
      )
    ),
    "You are viewing layer-level data from the 3rd generation of the International Soil Carbon Network database. In this tool, you can explore by variable. If you prefer to explore or download region-specific data, please use the Map Access tool located below.",
    br(),
    h2("Browse variables"),
    h4("Search the variables and their corresponding descriptions."),
    br(),
    dataTableOutput("variablechart"),
    # output of variable description chart
    hr(),
    h2("Visualize data"),
    fluidRow(
    column(6,
      selectInput("variable", "Select a variable to visualize",
                  namedVars)
    ),
    column(6,
      conditionalPanel(
        # this input will only appear if there are multiple methods available
        condition = "['bd_other','bd_sample','bd_tot','bd_whole','c_tot','ph_cacl','ph_h2o','ph_other','soc','wpg2'].indexOf(input.variable) > -1",
        selectInput(
          "method",
          "The variable you selected was measured in multiple ways. Please select one:",
          "Include all methods" # choices are updated by server
        )
      )
    )),
    textOutput('var_coverage'),
    br(),
    splitLayout(
      cellWidths = c("50%", "50%"),
      plotOutput('value_histogram'),
      plotOutput('date_histogram')
    ),
    plotOutput('location_map'),
    "If you need help with data browsing or would like to report an error or problem with the data, please contact nscn-support@fluxdata.org.",
    br(),
    br()
  )
) # close UI


server <- function(input, output, session) {
  # update options for methods
  observe({
    methodoptions <- c("Include all methods", measure.df$method[measure.df$var == input$variable])
    updateSelectInput(
      session,
      "method",
      "The variable you selected was measured in multiple ways. Please select one:",
      methodoptions
    )
  })
  
  # display variable info chart
  output$variablechart <-
    renderDataTable(allvars.df, options = list(pageLength = 3, rownames = FALSE))
  
  
  # measureID for selected variable(s)
  measID <- reactive({
    if (input$variable %in% duplicated) { # not the best approach, but - if there are multiple options for methods
      if (input$method == "Include all methods") { # choose all measIDs for that variable
        measID <-
          measure.df$measureID[measure.df$var == input$variable]
      } else {  # choose one specific measID
        measID <-
          measure.df$measureID[measure.df$method == input$method]
      }
    } else {
      measID <- measure.df$measureID[measure.df$var == input$variable]
    }
  })
  
  # label for variables
  varlabel <- reactive({
    names(namedVars[namedVars == input$variable])
  })
  
  
  # print coverage statistics
  output$var_coverage <- renderText({
    numfields <- length(unique(sample.df$fieldID))
    coverage <-
      length(unique(sample.df$fieldID[sample.df$measureID == measID()]))
    paste(
      "The variable you selected is present in", coverage, "out of", numfields, "sites sampled, or", percent(coverage / numfields)
    )
  })
  
  output$value_histogram <- renderPlot({
    unit <- measure.df$unit[measure.df$measureID %in% measID()][[1]]
    qplot(
      sample.df$value[sample.df$measureID %in% measID()],
      geom = "histogram",
      main = paste("Histogram of", varlabel(), "values"),
      xlab = unit
    )
  })
  
  output$date_histogram <- renderPlot({
    fields_w_var <- sample.df$fieldID[sample.df$measureID == measID()]
    field.df.subset <-
      field.df[field.df$fieldID %in% fields_w_var,]
    location.df <-
      unique(field.df.subset[, c('lat', 'lon', 'observation_date',
                                 'state', 'country')])
    location.df$obsDate <-
      as.Date(location.df$observation_date, "%m/%d/%y")
    ggplot(location.df,
           aes(x = obsDate)) + geom_histogram() + labs(title = paste("Years in which", varlabel(), "was measured"))
  })
  
  output$location_map <- renderPlot({

    fields_w_var <-
      sample.df$fieldID[sample.df$measureID == measID()]
    field.df.subset <-
      field.df[field.df$fieldID %in% fields_w_var,]
    location.df <-
      unique(field.df.subset[, c('lat', 'lon', 'observation_date',
                                 'state', 'country')])
    
    mapWorld <-
      borders("world", colour = "gray50", fill = "gray50") # create a layer of borders
    
    mp <- ggplot() +
      mapWorld +
      geom_point(aes(
        x = as.numeric(location.df$lon),
        y = as.numeric(location.df$lat)
      ),
      color = "blue",
      size = 3)
    mp
  })
} # close server

shinyApp(ui, server)
