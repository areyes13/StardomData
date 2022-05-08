# 1.1) Working Directory & Packages ---------------------------------------
options(java.parameters = "-Xmx8000m")
options(scipen=999)

# UPDATE TO YOUR WORKING DIRECTORY
setwd("~/Projects/Stardom")

library(readr)
library(readxl)
library(dplyr)
library(tidyverse)
library(lubridate)
library(scales)

library(showtext)

# IBM PLEX SANS
font_add(family = "IBM Plex Sans", # Name you want to use 
         regular = "IBMPlexSans-Medium.ttf",
         bold = "IBMPlexSans-Bold.ttf")

# IBM PLEX SANS LIGHT (FOR PLOT TEXT)
font_add(family = "IBM Plex Sans Light", # Name you want to use 
         regular = "IBMPlexSansCondensed-Light.ttf",
         bold = "IBMPlexSansCondensed-SemiBold.ttf")

showtext_auto()


# 1.2) CUSTOM FUNCTIONS & PALETTES ----------------------------------------

# levels for belts
belt_hierarchy <- c('NONE', 'FUTURE BELT', 'HIGH SPEED BELT', 
                    'SWA BELT', 'WHITE BELT', 'RED BELT') %>% rev()

# belt color palette
belt_pal <- c('#fb6a4a',
              '#eff3ff',
              '#dfc27d',
              '#969696',
              '#abd9e9')

# custom function for title defenses
belter <- function(string = NULL) {
  out <- case_when(string %>% str_detect('CONTENDER|TOURNAMENT') ~ 'NONE',
                   string %>% str_detect('RED') ~ 'RED BELT',
                   string %>% str_detect('WHITE') ~ "WHITE BELT",
                   string %>% str_detect("HIGH SPEED") ~ "HIGH SPEED BELT",
                   string %>% str_detect('SWA') ~ "SWA BELT",
                   string %>% str_detect('FUTURE') ~ 'FUTURE BELT',
                   T ~ "NONE")
  out <-  factor(out, levels = belt_hierarchy)
  return(out)
}




# 1.3) LOAD DATA ----------------------------------------------------------
# manual mapping, kinda sucks
faction_map <- read_csv('Stardom Factions.csv')

# SET "Date" FIELD AS DATE-TYPE USING ymd(...)
# FLAG SINGLE MATCHES (BASED ON MATCH FIELDS WITH "/" OR "&)
# ADDITIONAL FLAG FOR 3 / 4 WAY MATCHES (MAY NOT BE CAPTURED IN SINGLE_FLG)
stardom <- read_excel("Stardom Match Guide.xlsx") %>%
  rename_with(toupper) %>%
  mutate(DATE = ymd(DATE),
         MATCH = toupper(MATCH),
         NOTES = toupper(NOTES),
         SINGLE_FLG = !str_detect(string = MATCH, pattern = "/|&|MATCH"),
         MULTI_FLG = str_count(string = MATCH, pattern = "VS") > 1,
         BELT = belter(NOTES),
         TITLE_FLG = BELT != 'NONE') %>%
  mutate(across(ends_with("_FLG"), ~ replace_na(., FALSE)))








# 1.4) PREP ---------------------------------------------------------------
data <- stardom %>%
  filter(DATE >= '2021-01-01') %>%
  filter(SINGLE_FLG) %>%
  filter(!str_detect(MATCH, 'RUMBLE')) %>%
  mutate(INDEX = row_number()) %>%
  separate_rows(MATCH, sep = 'VS') %>%
  mutate(MATCH = str_squish(MATCH)) %>%
  left_join(faction_map) %>%
  select(-MATCH)


# 2.1) SANDBOX ------------------------------------------------------------
# Title Matches per Faction
data %>%
  filter(TITLE_FLG,
         DATE >= '2022-01-01') %>%
  count(FACTION, BELT) %>%
  ggplot(aes(reorder(FACTION, -n, sum), n, fill = BELT))+
  geom_col(color = 'grey20')+
  scale_fill_manual(values = belt_pal, name = 'Singles Belt')+
  scale_y_continuous(limits = c(0, 13), expand = c(0,0),
                     pretty_breaks(5),
                     name = 'Count of Title Matches')+
  theme_classic()+
  theme(text = element_text(family="IBM Plex Sans"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        legend.position = "right",
        legend.text = element_text(size = 16, family = 'IBM Plex Sans Light'),
        legend.title = element_text(size = 20),
        plot.title = element_text(face = 'bold', color = 'grey20', size = 30),
        axis.title.x = element_blank(),
        axis.text.x = element_text(angle = 37, hjust = 1, size = 20),
        axis.text.y = element_text(size = 24),
        panel.border = element_blank(),
        plot.background = element_blank())







