### Harms Lab R template ###
## T.K. Harms 7/2025

###################################
### Load all libraries up front ###
###################################
## If you need to add libraries as the script develops, add them up here, rather than embedded below. 
## Avoid loading unecessary libraries
library(here)
# The here package enables relative paths that work on any machine. 

library(tidyverse)
# Loads ggplot, dplyr

library(googledrive)
# Download any file stored on Drive

library(googlesheets4)
# Directly read in data entered as a Google sheet

############
### Data ###
############
### Download data from Drive
## Google sheet
condurl <- "https://docs.google.com/spreadsheets/d/1LH0pwR2zMEySa_5sMkpZdP-T6Zcw6hXKSRHa-QoDyUw/edit?gid=0#gid=0"

cond.dat <- read_sheet(condurl, col_types = "cnTcn")

## Munge datetime
cond.dat <- cond.dat %>% mutate(Date = as.Date(as.character(Date), format = "%y%m%d", origin = "1970-01-01")) %>%
                         mutate(Time = strftime(Time, format = "%H:%M:%S", tz = "America/Anchorage")) %>%
                         mutate(datetime = as.POSIXct(paste(Date, Time), format = "%Y-%m-%d %H:%M:%S", tz = "America/Anchorage"))

## Archive munged data to Google Drive
write.csv(cond.dat, here("data", "SPC2022.csv"), row.names = FALSE)
pathurl <- drive_get("https://drive.google.com/drive/folders/12CBlDtJpFglojjwK57b5PwOlYh3sciEM")
drive_upload(here("data", "SPC2022.csv"), path = as_id(pathurl), type = 'csv', name ='SPC2022.csv', overwrite = TRUE)

## csv from Drive
soilsurl <- "https://drive.google.com/drive/folders/12CBlDtJpFglojjwK57b5PwOlYh3sciEM"
soilscsv <- drive_get(as_id(soilsurl))
soils_glist <- drive_ls(soilscsv, pattern = "csv")

setwd(here("data"))
# Google Drive package does not currently allow downloading into a subdirectory. Workaround is to set the working director.
walk(soils_glist$id, ~ drive_download(as_id(.x), overwrite = TRUE))

setwd(here())
# Remember to set the working directory back to root after downloading.

soils <- read.csv(here("data", "soils_in.csv"))

### NEON, EDI, USGS all have packages for importing datasets from their repositories

#############
### Plots ###
#############
## Quick plots
cond.pl <- cond.dat %>% ggplot(aes(x = datetime, y = SPC)) +
                          geom_point() +
                          geom_line() +
                          facet_wrap(~Site)

soil.pl <- soils %>% ggplot(aes(x = veg, y = CN_molar, fill = depth)) +
                        geom_boxplot()
  
## Presentation-ready plots
cond.pl <- cond.dat %>% ggplot(aes(x = datetime, y = SPC)) +
                          geom_point() +
                          geom_line() +
                          ylab(expression("specific conductivity ("*mu*S~cm^"-1"*")")) +
                          facet_wrap(~Site, scales = "free_y") +
                          theme_bw() +
                          theme(plot.title = element_blank(),
                                axis.text = element_text(size = 20),
                                axis.title = element_text(size = 20),
                                axis.title.x = element_blank(),
                                strip.background = element_blank(),
                                strip.text = element_text(size = 20),
                                legend.text = element_text(size = 16),
                                panel.grid.major = element_blank(),
                                panel.grid.minor = element_blank())

ggsave(cond.pl, path = "plots", file = "SPC2022.pdf", width = 12, height = 6, units = "in")

soil.pl <- soils %>% ggplot(aes(x = veg, y = CN_molar, fill = depth)) +
                        geom_boxplot() +
                        scale_fill_manual(breaks = c("O1", "O2", "min"), values = c("darkgreen", "lightgreen", "brown"), labels = c("shallow organic", "deep organic", "mineral"), name = "Soil depth") +
                        ylab("C:N (molar)") +
                        theme_bw() +
                        theme(plot.title = element_blank(),
                              axis.text = element_text(size = 20),
                              axis.text.x = element_text(size = 20, angle = 30, hjust = 1),
                              axis.title = element_text(size = 20),
                              axis.title.x = element_blank(),
                              legend.text = element_text(size = 20),
                              legend.position = c(0.75, 0.85),
                              legend.title = element_text(size = 20),
                              panel.grid.major = element_blank(),
                              panel.grid.minor = element_blank())

ggsave(soil.pl, path = "plots", file = "soilCN.pdf", width = 6, height = 6, units = "in")
