## paths+packages.R

## load packages
require(tidyverse)
require(dataRetrieval)
require(magrittr)
require(maps)
require(lubridate)

## color palettes
# categorical color palette from https://sashat.me/2017/01/11/list-of-20-simple-distinct-colors/
col.cat.grn <- "#3cb44b"   # green
col.cat.yel <- "#ffe119"   # yellow
col.cat.org <- "#f58231"   # orange
col.cat.red <- "#e6194b"   # red
col.cat.blu <- "#0082c8"   # blue
col.gray <- "gray65"       # gray for annotation lines, etc

## create and set ggplot theme
windowsFonts(Arial=windowsFont("TT Arial"))
theme_scz <- function(...){
  theme_bw(base_size=10, base_family="Arial") + 
    theme(
      text=element_text(color="black"),
      plot.title=element_text(size=rel(1), hjust=0.5),
      axis.title=element_text(face="bold", size=rel(1)),
      axis.text=element_text(size=rel(1)),
      strip.text=element_text(size=rel(1)),
      legend.title=element_text(face="bold", size=rel(1)),
      legend.text=element_text(size=rel(1)),
      panel.grid=element_blank(),
      plot.margin=unit(c(1,1,1,1), "mm"),
      strip.background=element_blank())
}

theme_set(theme_scz())

## paths to data sources
# working directory on GSAS
dir.GSAS <- file.path("Z:/2.active_projects", "PlanetaryBoundaries")

# directory containing raw data from Daren Carlisle
dir.carlisle.raw <- file.path(dir.GSAS, "2.Model_data", "regional", "USA", "riv_river_network_streamflow", "1original", "Carlisle_Flows+Biology")
