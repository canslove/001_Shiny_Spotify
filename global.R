# library loading -----

library(shiny)
library(shinythemes)
library(shinydashboard)
#library(shinyGlobe)

library(data.table)
library(dplyr) # data manupulation
library(tidyr)
library(plotly) # chart/graph
#library(reshape2)
library(ggplot2) # chart/grpah
library(ggthemes)

library(leaflet) # map
library(RColorBrewer)
library(googleVis)
library(maps)
library(DT) # data table

# For Word Cloud
library(tm)
library(wordcloud)
#library(memoise)


#source("./helpers.R")

# data load  -----
raw.df <- as.data.frame(fread("./data/data_reduced.csv", stringsAsFactors = F))
region.df <- as.data.frame(fread("./data/region_code_reduced.csv", header=TRUE, stringsAsFactors = F))
region_map.df <- as.data.frame(fread("./data/region_map.csv", header=TRUE, stringsAsFactors = F))

# Change Variable Data Type -----
raw.df <- raw.df %>% mutate(DATE = as.Date(DATE, "%Y-%m-%d"))
#raw.df <- raw.df %>% mutate(REGION = as.factor(REGION))

# remove row names
rownames(raw.df) <- NULL
rownames(region.df) <- NULL

# names -----
nmRawdf = names(raw.df)

# create variable with colnames as choice
choice <- colnames(raw.df)

# Region_list -----
list_RegionMap <- c(
  "Global" = "global",
  "Argentina(ar)" = "ar",
  "Austria(at)" = "at",
  "Australia(au)" = "au",
  "Belgium(be)" = "be",
  "Bolivia(bo)" = "bo",
  "Brazil(br)" = "br",
  "Canada(ca)" = "ca",
  "Switzerland(ch)" = "ch",
  "Chile(cl)" = "cl",
  "Colombia(co)" = "co",
  "CostaRica(cr)" = "cr",
  "CzechRepublic(cz)" = "cz",
  "Germany(de)" = "de",
  "Denmark(dk)" = "dk",
  "DominicanRepublic(do)" = "do",
  "Ecuador(ec)" = "ec",
  "Estonia(ee)" = "ee",
  "Spain(es)" = "es",
  "Finland(fi)" = "fi",
  "France(fr)" = "fr",
  "GreatBritain(gb)" = "gb",
  "Greece(gr)" = "gr",
  "Guatemala(gt)" = "gt",
  "HongKong(hk)" = "hk",
  "Honduras(hn)" = "hn",
  "Hungary(hu)" = "hu",
  "Indonesia(id)" = "id",
  "Ireland(ie)" = "ie",
  "Iceland(is)" = "is",
  "Italy(it)" = "it",
  "Japan(jp)" = "jp",
  "Lithuania(lt)" = "lt",
  "Luxembourg(lu)" = "lu",
  "Latvia(lv)" = "lv",
  "Mexico(mx)" = "mx",
  "Malaysia(my)" = "my",
  "Netherlands(nl)" = "nl",
  "Norway(no)" = "no",
  "NewZealand(nz)" = "nz",
  "Panama(pa)" = "pa",
  "Peru(pe)" = "pe",
  "Philippines(ph)" = "ph",
  "Poland(pl)" = "pl",
  "Portugal(pt)" = "pt",
  "Paraguay(py)" = "py",
  "Sweden(se)" = "se",
  "Singapore(sg)" = "sg",
  "Slovakia(sk)" = "sk",
  "ElSalvador(sv)" = "sv",
  "Turkey(tr)" = "tr",
  "Taiwan(tw)" = "tw",
  "USA(us)" = "us",
  "Uruguay(uy)" = "uy")

# Word Cloud -----

track_4_wc <- raw.df %>% 
  filter(RANKING <= 30) %>%
  select(ARTIST, TRACK_NAME, DATE, REGION)

track_4_wc$TRACK_NAME <- sapply(track_4_wc$TRACK_NAME, tolower)
track_4_wc$ARTIST <- sapply(track_4_wc$ARTIST, tolower)



############# only for test ##############################

# map-start ---------------------
colStates <- map("state", fill = TRUE, plot = FALSE,
                 region = c("florida", "louisiana", "mississippi",
                            "alabama", "georgia", "tennesse"))
#map-end ---------------------

# map/hist selectized input start -------------------- 
# convert matrix to dataframe
state_stat <- data.frame(state.name = rownames(state.x77), state.x77)
# remove row names
rownames(state_stat) <- NULL
# create variable with colnames as choice
choice2 <- colnames(state_stat)[-1]
# map/hist selectized input end --------------------

# Valid colors are:
Colorset <- c("green", "red", "yellow", "aqua", "blue", 
              "light-blue", "navy", "teal", "olive", "lime", 
              "orange", "fuchsia", "purple", "maroon", "black")
Titlecolor <- Colorset[15]
Titlewidth <- 8


## Leaflet with Shiny -----
r_colors <- rgb(t(col2rgb(colors()) / 255))
names(r_colors) <- colors()



ShinyThemeName = "united" # cerulean, cosmo, cyborg, darkly, flatly, journal, lumen, paper, readable, sandstone, simplex, slate, spacelab, superhero, united, yeti


#shinyApp(ui, server)