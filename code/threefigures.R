## 1. preliminaries ===========================================================
## 2. import and clean ========================================================
## 3. prepare data ============================================================
## 4. plot ====================================================================
##=============================================================================

## 1. preliminaries ===========================================================
library(sp)
library(readr)
library(tidyr)
library(dplyr)
library(RColorBrewer)
require(extrafont)
loadfonts()

## 2. import and clean ========================================================

# get my data 
df <- read_csv("data/missing.csv")

# tidy data country
df %>%  
  group_by(Country) %>% 
  mutate(survey.id = 1:n(),
         survey.flag = ifelse(!is.na(`Men's Age Range`), 1, 0)) %>% 
  select(Country, survey.id, survey.flag) %>% 
  spread( key = survey.id, value = survey.flag, sep = "_") %>% 
  ungroup() -> df.cntry

# tidy cata year
df %>% 
  separate("Survey Year", c("year", "leftover")) %>% 
  select(-leftover) %>% 
  group_by(year) %>% 
  mutate(survey.id = 1:n(),
         survey.flag = ifelse(!is.na(`Men's Age Range`), 1, 0)) %>% 
  select(year, survey.id, survey.flag) %>% 
  spread( key = survey.id, value = survey.flag, sep = "_") %>% 
  ungroup()-> df.year


# get world map data
world <- rnaturalearth::countries110
world <- world[world$name != 'Antarctica',]

## 3. prepare data ============================================================

# country as unit of observation: data for mapping
df.cntry %>% 
  mutate(no.surveys = rowSums(!is.na(.[2:8])),
         no.surveys.m = rowSums(.[2:8], na.rm = TRUE),
         prop.men = no.surveys.m/no.surveys) %>% 
  select(Country, no.surveys:prop.men) -> mapping.df

# find data countries with names not in map 
anti_join(mapping.df, world@data, by = c("Country" = "name_long"))
# Capeverder, comoros, maldives, samoa and sao tome are not shown due to scale
# in the original, so leave them so

mapping.df %>% 
  mutate(Country = ifelse(Country == "Congo", 
                           world@data$name_long[34],
                          ifelse(Country == "Congo Dem. Republic", 
                                 world@data$name_long[33],
                                 ifelse(Country == "Cote d'Ivoire", 
                                        world@data$name_long[31],
                                        ifelse(Country == "Gambia", 
                                               world@data$name_long[61],
                                               ifelse(Country == "Kyrgyz Republic", 
                                                      world@data$name_long[85],
                                                      ifelse(Country == "Trinidad & Tobago", 
                                                             world@data$name_long[160],
                           Country))))))) -> mapping.df

left_join( world@data, mapping.df, by = c( "name_long" = "Country" )) -> world@data

# year as unit of observation 
df.year %>% 
  mutate(no.surveys = rowSums(!is.na(.[2:16])),
         no.surveys.m = rowSums(.[2:16], na.rm = TRUE),
         prop.men = no.surveys.m/no.surveys) %>% 
  select(year, no.surveys:prop.men) -> trend.df

## 4. plot ====================================================================

# gridlines and projections setup
grid.lines.mj <- gridlines(world,easts = seq(-180,180,by=30), norths = seq(-90,90,by=30))
grid.lines.mi <- gridlines(world,easts = seq(-165,195,by=15), norths = seq(-90,90,by=15))
world <- spTransform(world, CRS("+proj=wintri"))
grid.lines.mj <- spTransform(grid.lines.mj,CRS("+proj=wintri"))
grid.lines.mi <- spTransform(grid.lines.mi,CRS("+proj=wintri"))

# prepare colours
cols.7 <- brewer.pal(7, "YlGnBu")
cols.7.var <- cols.7[findInterval(world@data$no.surveys.m, vec =0:6)]
cols.7.var <- ifelse(is.na(cols.7.var), "white", cols.7.var)
cols.5 <- brewer.pal(5, "YlGnBu")
cols.5.var <- cols.5[findInterval(world@data$prop.men, 
                                  vec = c( -0.001, 0.001, 1/3, 0.5, .75), 
                                  left.open = TRUE)]
cols.5.var <- ifelse(is.na(cols.5.var), "white", cols.5.var)

findInterval(c(0, 0.1, 0.75, 1), vec = c( -0.001, 0.001, 1/3, 0.5, .75), left.open = TRUE)

plot(density(world@data$prop.men, na.rm=TRUE,adjust = 0.05),  
     axes = FALSE, ann=FALSE, lwd=2)
axis(1)
abline(v=c( -0.02, 0.02, 0.35, 0.52, .77, 1.02), col="red", lty=2) 

# map 1 
filename <- "figures/PH.15.1.02.figure1.pdf"

pdf(file = filename,
  width = 10,
  height = 6,
  family = "Garamond")
par(mar = c(0.1, 0.1, 0.1, 0.1))
plot(methods::as(world, 'Spatial'), expandBB=c(0,0,0.05,0.05))
plot(grid.lines.mi, col=grey(0.9), add=T)
plot(grid.lines.mj, col=grey(0.85), add=T)
plot(world,  border=grey(0.4), 
     col= cols.7.var,
     add = TRUE)
legend(x = -14000000, y = 0, legend = c(0:6, "no data"), fill = c(cols.7, "NA"),
       bty = "n")
text (x = -11500000, y = 1000000,
      "Number of", cex = 0.9)
text (x = -11500000, y = 300000,
      "Men's Surveys", cex = 0.9)
dev.off()
embed_fonts(filename,
            outfile = filename,
            options = "-dEPSCrop")
# map two
filename <- "figures/PH.15.1.02.figure3.pdf"
pdf(
  file = filename,
  width = 10,
  height = 6,
  family = "Garamond")
par(mar = c(0.1, 0.1, 0.1, 0.1))
plot(methods::as(world, 'Spatial'), expandBB=c(0,0,0.05,0.05))
plot(grid.lines.mi, col=grey(0.9), add=T)
plot(grid.lines.mj, col=grey(0.85), add=T)

plot(world,  border=grey(0.4), 
     col= cols.5.var,
     add = TRUE)

legend(x = -14000000, y = 0, 
       legend = c("[0.00, 0.00]",
                  "(0.00, 0.33]",
                  "(0.33, 0.50]",
                  "(0.50, 0.75]",
                  "(0.75, 1.00]", 
                  "no data"), fill = c(cols.5, "NA"),
       bty = "n")

text (x = -11500000, y = 1000000,
      "Porportion of Men's", cex = 0.9)
text (x = -11500000, y = 300000,
      "in All Surveys", cex = 0.9)
dev.off()
embed_fonts(filename,
            outfile = filename,
            options = "-dEPSCrop")


# plot 1
filename <- "figures/PH.15.1.02.figure2.pdf"

pdf(file = filename,
    width = 5,
    height = 5,
    family = "Garamond")

par(mai=c(0.7, 0.7, 0.3, 0.1) )
 plot(trend.df$year, 
       trend.df$prop.men * 100,
       type = "b",  cex = 0.5,
       tck = -0.01, las=1,
       mgp=c(1.8,0.4,0),
       bty = "n",
       lwd = 1.5, 
       col = "gray30",
      xlab = "Year",
      ylab = "Proportion of Men's in All Surveys",
      panel.first=grid(nx = NA, ny = NULL))
 dev.off()
 embed_fonts(filename,
             outfile = filename,
             options = "-dEPSCrop")