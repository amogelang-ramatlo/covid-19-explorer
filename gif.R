# Load required libraries
library(tidyverse)     
library(sf)            # For working with spatial (geographic) data
library(spData)        # Contains the 'world' spatial dataset
library(gganimate)     # For creating animated plots
library(gifski)

#_______________________________________________________________
# --- Load and Prepare World Map ---
#_______________________________________________________________

# Load built-in world map (sf object of country polygons)
world_map <- st_as_sf(world)

#_______________________________________________________________
# --- Prepare COVID-19 Data ---
#_______________________________________________________________

# Read COVID-19 data (aggregated by country and date)
covid_original <- read_csv("data/countries-aggregated.csv")

# Clean and process the data
covid_modified <- covid_original %>% 
  mutate(Country = recode(
    Country,
    "Burma" = "Myanmar",
    "Cabo Verde" = "Cape Verde",
    "Cote d'Ivoire" = "Côte d'Ivoire",
    "Czechia" = "Czech Republic",
    "Congo (Kinshasa)" = "Democratic Republic of the Congo",
    "Congo (Brazzaville)" = "Republic of the Congo",
    "Eswatini" = "eSwatini",
    "Korea, South" = "Republic of Korea",
    "Russia" = "Russian Federation",
    "Taiwan*" = "Taiwan",
    "US" = "United States",
    "Syria" = "Syrian Arab Republic",
    "Summer Olympics 2020" = NA_character_,
    "Winter Olympics 2022" = NA_character_,
    "Diamond Princess" = NA_character_,
    "MS Zaandam" = NA_character_
  )
  ) %>% filter(!is.na(Country))


covid_modified <- covid_modified %>%
  mutate(Date = ymd(Date)) %>%  # Convert date column to Date type
  group_by(Country) %>% arrange(Date) %>%
  # Calculate daily changes in Confirmed, Recovered, and Deaths
  mutate(
    DailyConfirmed = Confirmed - lag(Confirmed, default = 0),
    DailyRecovered = Recovered - lag(Recovered, default = 0),
    DailyDeaths = Deaths - lag(Deaths, default = 0),
    # Replace any negative values (due to data corrections) with 0
    DailyConfirmed = ifelse(DailyConfirmed < 0, 0, DailyConfirmed),
    DailyRecovered = ifelse(DailyRecovered < 0, 0, DailyRecovered),
    DailyDeaths = ifelse(DailyDeaths < 0, 0, DailyDeaths)
  ) %>%
  ungroup()  # Remove grouping

covid_monthly <- covid_modified %>%
  mutate(Month = floor_date(Date, unit = "month")) %>%  # e.g., 2020-03-01
  group_by(Country, Month) %>%
  summarise(
    MonthlyConfirmed = sum(DailyConfirmed, na.rm = TRUE),
    MonthlyRecovered = sum(DailyRecovered, na.rm = TRUE),
    MonthlyDeaths    = sum(DailyDeaths,    na.rm = TRUE),
    .groups = "drop"
  ) %>% ungroup()

#_______________________________________________________________
# --- Join COVID Data with World Geometry ---
#_______________________________________________________________

# Rename country column to match `world$name_long` for joining
covid_monthly <- covid_monthly %>% rename(name_long = Country)

# Perform left join to bring COVID data into the spatial map (`sf`) object
sf_data <- left_join(world_map, covid_monthly, by = "name_long") %>% 
  filter(!is.na(Month))

#_______________________________________________________________
# --- Create Animated Map of Daily Confirmed Cases ---
#_______________________________________________________________
animated_plot <- ggplot(sf_data) +
  # Draw each country with fill color based on daily confirmed cases
  geom_sf(aes(fill = MonthlyConfirmed), color = "lightgray", size = 0.1) +
  
  # Use a color scale that improves visualization of skewed data
  scale_fill_viridis_c(
    option = "viridis",     # Inferno color palette (good for heatmaps)
    trans = "sqrt",         # Square-root transform to improve contrast
    na.value = "white",      # Countries with no data appear white
    labels = scales::label_number(scale_cut = scales::cut_short_scale())
  ) +
  # Set plot title and legend label
  labs(
    title = 'Daily COVID-19 Cases: {frame_time}',
    fill = "Daily Cases"
  ) +
  
  # Improve plot appearance by removing clutter
  theme_bw() +
  theme(
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank(),
    plot.title = element_text(size = 18, face = "bold")
  ) +
  
  # Animate over the time dimension (Date)
  transition_time(Month) +
  ease_aes("linear")  # Smooth animation transition

# (Optional) To render and save:
animate(animated_plot, width = 800, height = 500, fps = 10, duration = 20,
        renderer = gifski_renderer("img/gif_animation.gif"))