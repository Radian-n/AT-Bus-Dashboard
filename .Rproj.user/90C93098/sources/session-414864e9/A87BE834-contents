library(tidyverse)
library(plotly)

data.df <- readRDS("Data/CLEAN_2019-2023_monthly_full_data") %>%
  distinct()

d1.df <- data.df %>%
  group_by(Region) %>% 
  reframe(Month, Reliability, Punctuality, Boardings)

data.df %>% 
  plotly()


data.df %>% 
  plot_ly(x = ~Month, y = ~Boardings) %>% 
  add_bars(text = ~Number, color = ~Number)


set.seed(99)
plot_ly() %>%
  add_trace(
    type = "scatter",
    mode = "markers+lines+text",
    x = 4:6, 
    y = 4:6,
    text = replicate(3, praise::praise("You are ${adjective}! 🙌")),
    textposition = "right",
    hoverinfo = "text",
    textfont = list(family = "Roboto Condensed", size = 16)
  ) %>%
  layout(xaxis = list(range = c(3, 8)))


base <- map_data("world", "canada") %>%
  group_by(group) %>%
  plotly_empty(x = ~long, y = ~lat, alpha = 0.2) %>%
  layout(showlegend = FALSE, xaxis = list(scaleanchor = "y"))

base %>%
  add_polygons(hoverinfo = "none", color = I("black"), ) %>%
  add_markers(text = ~paste(name, "<br />", pop), hoverinfo = "text", 
              color = I("red"), data = maps::canada.cities)

base %>%
  add_polygons(base, split = ~subregion, hoveron = "fills")



plot_mapbox(maps::canada.cities) %>%
  add_markers(
    x = ~long, 
    y = ~lat, 
    size = ~pop, 
    color = ~country.etc,
    colors = "Accent",
    text = ~paste(name, pop),
    hoverinfo = "text"
  )



# Get route colour data
colors.v <- readRDS("Data/ColourList")

plt <- data.df %>% 
  highlight_key(~Number) %>% 
  plot_ly(., color = I("black"), alpha = 0.4) %>% 
  group_by(Number) %>% 
  add_lines(x = ~Month, y = ~Boardings)

highlight(
  plt, 
  on = "plotly_click", 
  color = colors.v,
  selectize = TRUE, 
  dynamic = TRUE, 
  persistent = TRUE
)


data.df %>%
  #filter(Number == "OUT" | Number == "NX1") %>%
  highlight_key(~Number) %>% 
  plot_ly(x = ~Month, y = ~Boardings) %>% 
  add_trace(name = ~Number, mode = "lines+markers") %>% 
  highlight(on = "plotly_hover", off = "plotly_doubleclick") 
  layout(
         yaxis = list(
           rangemode='tozero'
         ))


#hovermode = "x unified",