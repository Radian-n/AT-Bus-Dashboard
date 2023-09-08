library(tidyverse)

# Pulls in bus route colour data from the colors.csv.
# colors.csv created by writing VBA code in excel to extract background cell colour
# from AT patronage reports

colors.df <- read_csv("Data/colors.csv") %>% 
  select(-Cell)

c2023.df <- colors.df %>% 
  filter(year == 2023)
c2019.df <- colors.df %>% 
  filter(year == 2019)

match.df <- c2023.df %>% 
  left_join(c2019.df, by = c("Number", "Background")) %>% 
  select(-year.y) %>% 
  rename(year = 2)

leftover.df <- c2019.df %>% 
  filter(!Number %in% match.df$Number)

full_colors.df <- rbind(match.df, leftover.df)

full_colors.df <- full_colors.df %>% 
  mutate(colorCode = paste("#", Background, sep = "")) %>% 
  select(Number, colorCode)

full_colors.df <- full_colors.df %>%
  mutate(colorCode = ifelse(colorCode == "#FFFFFF", "#000000", colorCode))

saveRDS(full_colors.df, "Data/Colourdf")


colors.v <- full_colors.df$colorCode
colors.v <- setNames(colors.v, full_colors.df$Number)

saveRDS(colors.v, "Data/ColourList")
