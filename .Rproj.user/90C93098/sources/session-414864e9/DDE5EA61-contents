library(tidyverse)
library(shiny)
library(plotly)
library(rsconnect)


# Read in data
data.df <- readRDS("Data/CLEAN_2019-2023_monthly_full_data") %>%
  distinct()
colors.v <- readRDS("Data/ColourList")  # Get route colour data

# Get possible bus route inputs:
busRoutes.v <- data.df %>%
  select(Number) %>%
  unique()

# Get min and max dates
minDate <- min(data.df$Month)
maxDate <- max(data.df$Month)

# Get names of routes
numberRouteNames <- data.df %>%
  distinct(Number, Route)

# Get regional dataframe
region.df <- data.df %>% 
  group_by(Region, Month) %>% 
  summarise(Reliability = mean(Reliability, na.rm = TRUE), 
            Punctuality = mean(Punctuality, na.rm = TRUE), 
            Boardings = sum(Boardings, na.rm = TRUE)) %>% 
  filter(!is.na(Region)) %>% 
  ungroup()

region.df$Region <- factor(region.df$Region, 
                           levels = rev(c("Central", "North", "South", "East", "West", "Waiheke")),
                           ordered = TRUE)


# See above for the definitions of ui and server
ui <- fluidPage(
  
  
  # App title ----
  titlePanel("AT Bus Performance Dashboard"),
  
  br(),
  
  tabsetPanel(
    tabPanel("Individual Routes",
    
      br(),
    
    # Sidebar layout with input and output definitions ----
      sidebarLayout(
        
        # Sidebar panel for inputs ----
        sidebarPanel(
          
          p("Select Auckland Transport bus routes to compare boarding 
            numbers, reliability and punctuality statistics."),
          
          hr(),
          
          selectizeInput(inputId = "t1",
                      label = "Bus Routes:",
                      choices = busRoutes.v,
                      multiple = TRUE,
                      selected = list("OUT", "NX1"),
                      options = list(
                        plugins = list("remove_button"))),
          
          htmlOutput("routeDetails"),

          
          hr(),
          
          checkboxGroupInput(inputId = "plotSettings",
                       label = "Settings:",
                       choices = c("AT reliability & punctuality targets")),
          hr(),
          
          strong("Notes:"),
          br(),
          br(),
 
          HTML(
            "<p>
             <b>Reliability:</b> defined as the percentage of scheduled trips that were sighted departing the first stop between 59 seconds early and 9 minutes 59 seconds late (or unsighted in Rapid but sighted in AIFS/Smartrak).
             <br><br>
             <b>Punctuality:</b> defined as  the percentage of scheduled trips that were sighted departing the first stop between 59 seconds early and 4 minutes 59 seconds late.
             <br><br>
             Some bus routes were cancelled during this time frame, so their data stops abruptly.
           </p>"
          ), 
          
          br(),
    
        ),
        
        # Main panel for displaying outputs ----
        mainPanel(
          # plotlyOutput(outputId = "Plotly1", height = "700px"),
          plotlyOutput(outputId = "PlotlyRoutes", height = "800px"),

        )
        # plotlyOutput(outputId = "Plotly2", height = "150px")
      )
    ),
    tabPanel("Regional Statistics",

             br(),

             # Sidebar layout with input and output definitions ----
             sidebarLayout(

               # Sidebar panel for inputs ----
               sidebarPanel(
                 
                 p("Select regions compare boarding numbers, reliability and punctuality statistics."),
                 
                 hr(),
                 
                 # selectizeInput(inputId = "RegionSelect",
                 #                label = "Select Regions:",
                 #                choices = rev(c("Central", "North", "South", "East", "West", "Waiheke")),
                 #                multiple = TRUE,
                 #                selected = c("Central", "North", "South", "East", "West", "Waiheke"),
                 #                options = list(
                 #                  plugins = list("remove_button"))
                 # ),
                 # 
                 checkboxGroupInput(
                   inputId = "RegionSelect",
                   label = "Select Regions:",
                   choices = rev(c("Central", "North", "South", "East", "West", "Waiheke")),
                   selected = c("Central", "North", "South", "East", "West", "Waiheke")
                 ),
                 
                 hr(),
                 
                 checkboxGroupInput(inputId = "plotSettingsReg",
                                    label = "Settings:",
                                    choices = c("Display regional boarding data as proportional",
                                                "AT reliability & punctuality targets")),
                 
                 hr(),
                 
                 strong("Notes:"),
                 br(),
                 br(),
                 
                 HTML(
                  "<p>
                  <b>Reliability:</b> defined as the percentage of scheduled trips that were sighted departing the first stop between 59 seconds early and 9 minutes 59 seconds late (or unsighted in Rapid but sighted in AIFS/Smartrak).
                  <br><br>
                  <b>Punctuality:</b> defined as  the percentage of scheduled trips that were sighted departing the first stop between 59 seconds early and 4 minutes 59 seconds late.
                  <br><br>
                  Some bus routes were cancelled during this time frame, so their data stops abruptly.
                  </p>"
                 ),  
                 
                 br(),
                 
               ),
               
               mainPanel(
                 
                 plotlyOutput(
                   outputId = "PlotlyRegion1", 
                   height = "800px"
                 ) 
               )
             )
    ),
    
    footer = {
      div(style = "text-align:right;",
          em("Source: ", a("AT Metro Bus Performance Reports", href = "https://at.govt.nz/about-us/reports-publications/at-metro-patronage-report/"))
      )
    }
  )
)
    

server <- function(input, output) {
  
  output$routeDetails <- renderText({
    # Wrap in try/catch block?
    outputStr <- ""
    routes <- numberRouteNames %>% 
      filter(Number %in% input$t1)
    for (i in 1:nrow(routes)) {
      routeString <- paste('<font color=\"', colors.v[[routes$Number[i]]], '\"><b>', routes$Number[i], ":</b><i> ", routes$Route[i], "</i></font><br><br>", sep = "")
      outputStr <- paste(outputStr, routeString)
    }
    return(outputStr)
  })
  
  
  output$PlotlyRoutes <- renderPlotly({
    # Filter only selected routes
    pData.df <- data.df %>% 
      filter(Number %in% input$t1)
    
    # Get Annotations
    # pltText <- pData.df %>% 
    #   filter(!is.na(Boardings) | !is.na(Punctuality)| !is.na(Reliability)) %>% 
    #   group_by(Number) %>% 
    #   arrange(Month) %>% 
    #   slice(n())
    
    # BOARDINGS
    plt1 <- pData.df %>% 
      plot_ly(x = ~Month, y = ~Boardings, colors = colors.v, groupnorm = "") %>% 
      add_trace(text = ~Number,
                name = " ",
                mode = "lines+markers",
                hovertemplate = paste("%{text}", "<b>%{y:.3s}</b>"),
                showlegend = FALSE,
                type = "scatter",
                color = ~Number,
                opacity = 0.8) %>% 
      # add_text(x = pltText$Month + 10,
      #          y = pltText$Boardings,
      #          text = paste0("<b style='color: ", colors.v[pltText$Number], "'>", pltText$Number, "</b>"),
      #          textposition = "top right",
      #          textfont = list(size = 14)) %>%
      layout(yaxis = list(rangemode='tozero'),
             xaxis = list(title = "Year",
                          nticks = 5,
                          range = list(minDate-25, maxDate+25)),
             hovermode = "x unified",
             title = "<b>Selected Routes</b>: Boardings, Reliability, & Punctuality")
    
    # RELIABILITY
    plt2 <- pData.df %>% 
      plot_ly(x = ~Month, y = ~Reliability, colors = colors.v, groupnorm = "") %>% 
      add_trace(type = "scatter",
                text = ~Number,
                name = ~Number,
                mode = "lines",
                hovertemplate = paste("<b>%{y:.2f}</b>"),
                showlegend = FALSE,
                line = list(shape = 'spline', smoothing = 1.3),
                color = ~Number,
                opacity = 0.8) %>%
      layout(yaxis = list(
        range = c(0.65, 1),
        nticks = 4),
        xaxis = list(title = "Year",
                     nticks = 5),
        legend = list(orientation = 'h'),
        hovermode = "x unified"
      )
    
    # PUNCTUALITY
    plt3 <- pData.df %>% 
      plot_ly(x = ~Month, y = ~Punctuality, colors = colors.v, groupnorm = "") %>% 
      add_trace(type = "scatter",
                text = ~Number,
                name = ~Number,
                hovertemplate = paste("<b>%{y:.2f}</b>"),
                mode = "lines",
                showlegend = FALSE,
                line = list(shape = 'spline', smoothing = 1.3),
                color = ~Number,
                opacity = 0.8) %>%
      layout(yaxis = list(
        range = c(0.65, 1),
        nticks = 4),
        xaxis = list(title = "Year",
                     nticks = 5),
        legend = list(orientation = 'h'),
        hovermode = "x unified"
      )
    
    # Add AT targets to plot
    if ("AT reliability & punctuality targets" %in% input$plotSettings)
    {
      plt2 <- plt2 %>% 
        layout(shapes = list(list(type = "rect",
                                  fillcolor = "green", line = list(color = "white"), opacity = 0.09,
                                  x0 = min(data.df$Month)+2, x1 = maxDate+100, xref = "x",
                                  y0 = 0.98, y1 = 1, yref = "y", layer='below'),
                             list(type = "rect",
                                  fillcolor = "yellow", line = list(color = "white"), opacity = 0.1,
                                  x0 = min(data.df$Month)+2, x1 = maxDate+100, xref = "x",
                                  y0 = 0.96, y1 = 0.98, yref = "y", layer='below'),
                             list(type = "rect",
                                  fillcolor = "red", line = list(color = "white"), opacity = 0.06,
                                  x0 = min(data.df$Month)+2, x1 = maxDate+100, xref = "x",
                                  y0 = 0, y1 = 0.96, yref = "y", layer='below')),
               yaxis = list(gridcolor = "white"),
               xaxis = list(gridcolor = "white"))
      
      plt3 <- plt3 %>% 
        layout(shapes = list(list(type = "rect",
                                  fillcolor = "green", line = list(color = "white"), opacity = 0.09,
                                  x0 = min(data.df$Month)+2, x1 = maxDate+100, xref = "x",
                                  y0 = 0.95, y1 = 1, yref = "y", layer='below'),
                             list(type = "rect",
                                  fillcolor = "yellow", line = list(color = "white"), opacity = 0.1,
                                  x0 = min(data.df$Month)+2, x1 = maxDate+100, xref = "x",
                                  y0 = 0.9, y1 = 0.95, yref = "y", layer='below'),
                             list(type = "rect",
                                  fillcolor = "red", line = list(color = "white"), opacity = 0.06,
                                  x0 = min(data.df$Month)+2, x1 = maxDate+100, xref = "x",
                                  y0 = 0, y1 = 0.9, yref = "y", layer='below')),
               yaxis = list(gridcolor = "white"),
               xaxis = list(gridcolor = "white"))
    }
    
    # Create subplot
    plotSub2 <- subplot(plt1, plt2, plt3,
                        heights = c(0.7, 0.15, 0.15),
                        nrows = 3,
                        shareX = TRUE,
                        titleY = TRUE)
    
    return(plotSub2)
  })
  
  
  output$PlotlyRegion1 <- renderPlotly({

    pRegion.df <- region.df %>% 
      filter(Region %in% input$RegionSelect)
    
    if ("Display regional boarding data as proportional" %in% input$plotSettingsReg) 
    {
      reg1.plt <- pRegion.df %>% 
        plot_ly(x = ~Month, y = ~Boardings, colors = colors.v) %>% 
        add_trace(text = ~Region,
                  split = ~Region,
                  stackgroup = 'one',
                  mode = "lines",
                  hovertemplate = paste("<b>%{y:.1f}%</b>"),
                  showlegend = FALSE,
                  type = "scatter",
                  hoveron = 'points',
                  color = ~Region,
                  groupnorm = 'percent') %>%
        layout(yaxis = list(rangemode='tozero',
                            range = c(0, 100),
                            ticksuffix = '%'),
               xaxis = list(title = "Year",
                            nticks = 5,
                            range = list(minDate-5, maxDate)),
               hovermode = "x unified",
               title = "<b>Selected Regions</b>: Boardings, Reliability, & Punctuality")
    }
    else 
    {
      reg1.plt <- pRegion.df %>% 
        plot_ly(x = ~Month, y = ~Boardings, colors = colors.v) %>% 
        add_trace(text = ~Region,
                  split = ~Region,
                  stackgroup = 'one',
                  mode = "lines",
                  hovertemplate = paste("<b>%{y:.3s}</b>"),
                  showlegend = FALSE,
                  type = "scatter",
                  hoveron = 'points',
                  color = ~Region) %>%
        layout(yaxis = list(rangemode='tozero',
                            range = c(0, 7.2e6)),
               xaxis = list(title = "Year",
                            nticks = 5,
                            range = list(minDate-5, maxDate)),
               hovermode = "x unified",
               title = "<b>Selected Regions</b>: Boardings, Reliability, & Punctuality")
    }
    
    reg2.plt <- pRegion.df %>% 
      plot_ly(x = ~Month, y = ~Boardings, colors = colors.v) %>% 
      add_trace(text = ~Region,
                split = ~Region,
                stackgroup = 'one',
                mode = "lines",
                hovertemplate = paste("<b>%{y:.3d}%</b>"),
                showlegend = FALSE,
                type = "scatter",
                hoveron = 'points',
                color = ~Region,
                groupnorm = 'percent') %>%
      layout(yaxis = list(title = "Proportion",
                          range = c(0, 100),
                          ticksuffix = '%'),
             xaxis = list(title = "Year",
                          nticks = 5,
                          range = list(minDate-5, maxDate)),
             hovermode = "x unified")
    
    # RELIABILITY
    reg3.plt <- pRegion.df %>% 
      plot_ly(x = ~Month, y = ~Reliability, colors = colors.v) %>% 
      add_trace(type = "scatter",
                text = ~Region,
                name = ~Region,
                mode = "lines",
                hovertemplate = paste("<b>%{y:.2f}</b>"),
                showlegend = FALSE,
                line = list(shape = 'spline', smoothing = 1.3),
                color = ~Region,
                opacity = 0.8) %>%
      layout(yaxis = list(
        range = c(0.59, 1),
        nticks = 5),
        xaxis = list(title = "Year",
                     nticks = 5),
        legend = list(orientation = 'h'),
        hovermode = "x unified"
      )
    
    # PUNCTUALITY
    reg4.plt <- pRegion.df %>%
      plot_ly(x = ~Month, y = ~Punctuality, colors = colors.v) %>%
      add_trace(type = "scatter",
                text = ~Region,
                name = ~Region,
                hovertemplate = paste("<b>%{y:.2f}</b>"),
                mode = "lines",
                showlegend = FALSE,
                line = list(shape = 'spline', smoothing = 1.3),
                color = ~Region,
                opacity = 0.8) %>%
      layout(yaxis = list(
        range = c(0.59, 1),
        nticks = 5),
        xaxis = list(title = "Year",
                     nticks = 5),
        legend = list(orientation = 'h'),
        hovermode = "x unified"
      )
    

    # Add AT targets to plot
    if ("AT reliability & punctuality targets" %in% input$plotSettingsReg)
    {
      reg3.plt <- reg3.plt %>% 
        layout(shapes = list(list(type = "rect",
                                  fillcolor = "green", line = list(color = "white"), opacity = 0.09,
                                  x0 = min(data.df$Month)+2, x1 = maxDate+100, xref = "x",
                                  y0 = 0.98, y1 = 1, yref = "y", layer='below'),
                             list(type = "rect",
                                  fillcolor = "yellow", line = list(color = "white"), opacity = 0.1,
                                  x0 = min(data.df$Month)+2, x1 = maxDate+100, xref = "x",
                                  y0 = 0.96, y1 = 0.98, yref = "y", layer='below'),
                             list(type = "rect",
                                  fillcolor = "red", line = list(color = "white"), opacity = 0.06,
                                  x0 = min(data.df$Month)+2, x1 = maxDate+100, xref = "x",
                                  y0 = 0, y1 = 0.96, yref = "y", layer='below')),
               yaxis = list(gridcolor = "white"),
               xaxis = list(gridcolor = "white"))
      
      reg4.plt <- reg4.plt %>% 
        layout(shapes = list(list(type = "rect",
                                  fillcolor = "green", line = list(color = "white"), opacity = 0.09,
                                  x0 = min(data.df$Month)+2, x1 = maxDate+100, xref = "x",
                                  y0 = 0.95, y1 = 1, yref = "y", layer='below'),
                             list(type = "rect",
                                  fillcolor = "yellow", line = list(color = "white"), opacity = 0.1,
                                  x0 = min(data.df$Month)+2, x1 = maxDate+100, xref = "x",
                                  y0 = 0.9, y1 = 0.95, yref = "y", layer='below'),
                             list(type = "rect",
                                  fillcolor = "red", line = list(color = "white"), opacity = 0.06,
                                  x0 = min(data.df$Month)+2, x1 = maxDate+100, xref = "x",
                                  y0 = 0, y1 = 0.9, yref = "y", layer='below')),
               yaxis = list(gridcolor = "white"),
               xaxis = list(gridcolor = "white"))
    }
    
    
    RegPlots <- subplot(reg1.plt, reg3.plt, reg4.plt, 
                       heights = c(0.7, 0.15, 0.15),
                       nrows = 3,
                       shareX = TRUE,
                       titleY = TRUE)
    
    return(RegPlots)
    
  })
  
}

shinyApp(ui, server)