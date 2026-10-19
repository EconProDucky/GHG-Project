# libraries
packages <- c("readxl", "dplyr", "magrittr", "plyr", "ggplot2", "tidyverse")
lapply(packages, library, character.only = TRUE)



# GHG by SECTOR & COUNTRY ANALYSIS

#setwd("F:/Data/Air and Climate")
setwd("D:/Data/Air and Climate")

# GHG per Sector Perc Data Cleaning
GHG_secp <- read_excel("GHG_by_Sector_Perc.xlsx", range = "B30:I219")
GHG_secp <- GHG_secp[2:219, ]
GHG_secp <- GHG_secp %>%
  mutate_at(c("Total GHG emissions", 
              "GHG from Energy", 
              "GHG from Energy \r\nof which: from Transport", 
              "GHG from Industrial Processes", 
              "GHG from Agriculture", 
              "GHG from Waste"), 
            as.numeric) %>%
  mutate_at("latest year available", as.integer) %>%
  na.omit(GHG_secp)

summary(GHG_secp)

# GHG Emissions Data Cleaning
GHG_em <- read_excel("GHG_Emissions.xlsx", range = "B17:J203")
GHG_em <- GHG_em[2:186, c(1,2,3,5,7,9)]
GHG_em <- GHG_em %>%
  mutate_at(c("Total GHG emissions",
              "Total GHG emissions including LULUCF/LUCF",
              "GHG emissions per capita", 
              "% change since 1990"), 
            as.numeric) %>%
  mutate_at("latest year available", as.integer) %>%
  na.omit(GHG_em)
  
summary(GHG_em)

# Joining data sets
GHG <- left_join(GHG_em, GHG_secp, by = "Country") %>%
  select(-ends_with(".y")) %>%
  na.omit(GHG)

# Renaming
colnames(GHG)[colnames(GHG) == "latest year available.x"] <- "latest year available"
colnames(GHG)[colnames(GHG) == "Total GHG emissions.x"] <- "Total GHG emissions"
GHG <- GHG %>%
  mutate(Country = recode(Country,"United Kingdom of Great Britain and Northern Ireland" = "UK and North Ireland"))

# Filtering
GHG_big <- filter(GHG, `Total GHG emissions` > 100)

# GHG Visualization
ggplot(GHG_big, aes(y = Country)) +
  geom_point(aes(x = `% change since 1990`))

ggplot(GHG_big, aes(y = Country)) +
  geom_point(aes(x = `Total GHG emissions`, color = "Total GHG emissions")) +
  geom_point(aes(x = `Total GHG emissions including LULUCF/LUCF`, color = "Total GHG + LULUCF/LUCF")) +
  scale_color_manual(values = c("Total GHG emissions" = "red",
                                "Total GHG + LULUCF/LUCF" = "blue3"))

ggplot(GHG_big, aes(y = Country)) +
  geom_point(aes(x = `GHG emissions per capita`))

ggplot(GHG_big, aes(y = Country)) +
  geom_point(aes(x = `GHG from Energy`, color = "Energy")) +
  geom_point(aes(x = `GHG from Energy \r\nof which: from Transport`, color = "Transportation")) +
  geom_point(aes(x = `GHG from Industrial Processes`, color = "Industrial Processes")) +
  geom_point(aes(x = `GHG from Agriculture`, color = "Agriculture")) +
  geom_point(aes(x = `GHG from Waste`, color = "Waste")) + 
  labs(title = "GHG Distribution Between Countries") +
  scale_color_manual(values = c("Energy" = "magenta", "Transportation" = "blue",
                              "Agriculture" = "green3", "Industrial Processes" = "purple",
                              "Waste" = "brown4"))

ggplot(GHG_big, aes(y = Country)) +
  geom_point(aes(x = `GHG from Energy \r\nof which: from Transport`, color = "Transportation")) +
  geom_point(aes(x = `GHG from Industrial Processes`, color = "Industrial Processes")) +
  geom_point(aes(x = `GHG from Agriculture`, color = "Agriculture")) +
  geom_point(aes(x = `GHG from Waste`, color = "Waste")) + 
  labs(title = "GHG Distribution Except for Energy") +
  scale_color_manual(values = c("Transportation" = "blue",
                                "Agriculture" = "green3", "Industrial Processes" = "purple",
                                "Waste" = "brown4"))

# Statistical Analysis
lm(formula = `GHG from Energy` ~ `GHG from Energy \r\nof which: from Transport` +
     `GHG emissions per capita`,
   data = GHG_big) %>%
  summary() # Zero significance

lm(formula = `GHG emissions per capita` ~ `GHG from Waste`,
   data = GHG_big) %>%
  summary() # Significance at 0.01 from `Waste`

lm(formula = `GHG emissions per capita` ~ `GHG from Energy \r\nof which: from Transport` +
     `GHG from Waste` + `GHG from Industrial Processes` + `GHG from Agriculture`,
   data = GHG_big) %>%
  summary() # Significance at 0.05 from `Waste`

# Notes: Check for line of best fit, cross validation, test different equations

# RAIN-GHG ANALYSIS

# Home laptop: `F` drive
#setwd("F:/Data/Inland Water Resources")
#setwd("F:/Data/Air and Climate")

# School/Work laptop: `D` drive


# Time Series Data Cleaning
setwd("D:/Data/Inland Water Resources")
rain_raw <- read_csv("World Precipitation.csv")
rain <- rain_raw[71:125,]

setwd("D:/Data/Air and Climate")
GHG_ts_raw <- read_excel("EDGAR_AR5_GHG_1970_2024.xlsx", sheet = "TOTALS BY COUNTRY", range = "D10:BH235")
GHG_ts_raw <- GHG_ts_raw[ , c(1,3:57)]

GHG_world <- apply(GHG_ts_raw[,2:56], 2, sum)
GHG_world.mean <- apply(GHG_ts_raw[,2:56], 2, mean)

GHG.rain <- rain %>%
  mutate(Total_GHG = GHG_world) %>%
  mutate(Avg_GHG = GHG_world.mean) %>%
  mutate(rain.diff = round(((rain$World - lag(rain$World))/lag(rain$World)), 3)) %>%
  mutate(Total_GHG.diff = round(((GHG_world - lag(GHG_world))/lag(GHG_world)), 3))

# Renaming
colnames(GHG.rain)[colnames(GHG.rain) == "World"] <- "rain" 

# Note: clean to join `GHG_world` to `rain`

# GHG.rain Visualization
ggplot(GHG.rain, aes(x = Year)) +
  geom_line(aes(y = Total_GHG)) +
  labs(title = "Time Series of GHG")

ggplot(GHG.rain, aes(x = Year)) +
  geom_line(aes(y = rain)) +
  labs(title = "Time Series of rain")

ggplot(GHG.rain, aes(x = Year)) +
  # rain_diff
  geom_line(aes(y = rain.diff, color = "Rain")) +
  geom_smooth(aes(y = rain.diff), se = FALSE, span = 1/3) +
  
  # Total_GHG.diff
  geom_line(aes(y = Total_GHG.diff, color = "GHG")) +
  geom_smooth(aes(y = Total_GHG.diff), color = "darkgreen", se = FALSE, span = 1/3) +
  
  # Labeling
  labs(title = "Time Series of Differences: rain-GHG") +
  scale_color_manual(values = c("Rain" = "blue", "GHG" = "green4"))

# Regressions
lm(formula = rain ~ Total_GHG, data = GHG.rain) %>%
  summary() # Significance at 99.9%

lm(formula = rain.diff ~ Total_GHG.diff, data= GHG.rain) %>%
  summary() # No significance


# WATER-GHG ANALYSIS

# Water Data
setwd("D:/Data/Inland Water Resources")

water <- read_excel("Water resources.xlsx", sheet = "Data", range = "B1:O118")
water <- water[2:117, c(1,2,3,4,6,8,10,12)]



# Note for next time: Clean data and check relationship between GHG and Water

# Note: Do hypothesis analysis or regressions using renewable energy(prices, output, or demand) 
