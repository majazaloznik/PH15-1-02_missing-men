## 1. preliminaries ===========================================================
## 2. import and clean ========================================================

suppressPackageStartupMessages(library(sp))
library(readr)
library(tidyr)
library(dplyr)
## 1. preliminaries ===========================================================

# get my data 
df <- read_csv("data/missing.csv")

# tidy data

df %>%  
  group_by(Country) %>% 
  mutate(survey.id = 1:n(),
         survey.flag = ifelse(!is.na(`Men's Age Range`), 1, 0)) %>% 
  select(Country, survey.id, survey.flag) %>% 
  spread( key = survey.id, value = survey.flag, sep = "_") %>% 
  ungroup() %>% 
  mutate(no.surveys.m = rowSums(.[,2:8], na.rm = TRUE))


# get world map data
world <- rnaturalearth::countries110

world <- world[world$name != 'Antarctica',]

grid.lines.mj <- gridlines(world,easts = seq(-180,180,by=30), norths = seq(-90,90,by=30))
grid.lines.mi <- gridlines(world,easts = seq(-165,195,by=15), norths = seq(-90,90,by=15))
world <- spTransform(world, CRS("+proj=wintri"))
grid.lines.mj <- spTransform(grid.lines.mj,CRS("+proj=wintri"))
grid.lines.mi <- spTransform(grid.lines.mi,CRS("+proj=wintri"))

par(mar = c(0.1, 0.1, 0.1, 0.1))
plot(methods::as(world, 'Spatial'), expandBB=c(0,0,0.05,0.05))


plot(grid.lines.mi, col=grey(0.95), add=T)
plot(grid.lines.mj, col=grey(0.9), add=T)

plot(world, add=TRUE, border=grey(0.4), col=grey(0.9))
