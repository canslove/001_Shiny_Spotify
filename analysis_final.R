# ==============================================================
# title : Analysis of Top Streamed Songs and Artists on Spotify
# author: Donghyun Kang
# output: ShinyApp.io
# fist  : Jan/23/2018
# final : Mar/11/2018
# ==============================================================

library(dplyr)
library(ggplot2)
library(ggthemes) 
library(data.table)
#library(dygraphs) # install.packages("dygraphs")
library(reshape2)
library(plotly)

# Statista data plot -----
rm(list=ls())
pwd <- getwd()
setwd('C:/Users/SAMSUNG/Documents/A.MyDoc/A.DriveE/DataScience/NYCDSA/40_Projects/C01.Shiny/Blog/')
s <- read.csv('statista_res.csv')
colnames(s) = c('date', 'user_mil')
s$date2 <- factor(s$date, as.character(s$date))

p <- ggplot(s, aes(x=date2, y=user_mil)) +
  geom_bar(aes(), stat = "identity", fill = "#FF6666") + #, position = "dodge") +
  ggtitle("Number of paying Spotify subscribers worldwide 2010-2018(in million) \n (source: Statista 2018)") +
  xlab("Date") +
  ylab("Paying Subscribers") +
  theme(axis.text.x=element_text(angle = -90, hjust = 0)) +
  theme(plot.title = element_text(hjust = -0.45, vjust=2.12))
ggplotly(p)


# Data reduction/cleaning --------------------------------
if(0){
  rm(list=ls())
  INIT = 0
  N_RANK = 100
  No_url = 1
  
  if(INIT) {
    raw.dt <- fread("../Data/data.csv", stringsAsFactors = F)
    raw.df <- as.data.frame(raw.dt)
    sum(is.na(raw.df))
    names(raw.df) # "Position" "Track Name" "Artist" "Streams" "URL" "Date" "Region" 
    raw.df <- raw.df %>% rename(ranking = `Position`,  track_name = `Track Name`)
    names(raw.df) <- toupper(names(raw.df))
    names(raw.df) # "RANKING" "TRACK_NAME" "ARTIST" "STREAMS" "URL" "DATE" "REGION"
    if(No_url) {
      raw.df <- raw.df %>%
        select(-URL)
    }
    
    target_df <- paste0('ranking',N_RANK)
    raw.df.reduced <- raw.df %>% 
      filter(RANKING <= N_RANK)
    assign(target_df, raw.df.reduced)
    
    write.csv(get(target_df), file='./data_reduced.csv', row.names=FALSE)
    write.csv(get(target_df), file='../Data/data_reduced.csv', row.names=FALSE)
  } else {
    raw.dt <- fread("./data_reduced.csv", stringsAsFactors = F)
    raw.df <- as.data.frame(raw.dt)
    names(raw.df) # "position" "TRACK_NAME" "ARTIST" "STREAMS" "URL" "DATE" "REGION"
  }
  
  # Mapping regions -----------------------------------------------
  if(INIT){
    region_code = read.csv('../Data/countries.csv', header = FALSE, stringsAsFactors=FALSE)
    region_code <- region_code %>% rename(REGION = `V1`,  COUNTRY = `V2`)
    region_code$REGION = tolower(region_code$REGION)
    U.regions$REGION <- as.character(U.regions$REGION)
    region_code_join <- left_join(U.regions, region_code, by = "REGION") %>% arrange(., REGION)
    region_code_join$COUNTRY[[which(region_code_join$REGION == "global")]] = "Global"
    write.csv(region_code_join, file='./region_code_reduced.csv', row.names=FALSE)
    write.csv(region_code_join, file='../Data/region_code_reduced.csv', row.names=FALSE)
  } else {
    region.dt <- fread("./region_code_reduced.csv", header=TRUE, stringsAsFactors = F)
    region.df <- as.data.frame(region.dt)
  }
}

# Data Read/Modify/Write --------------------------------
setwd('C:/Users/SAMSUNG/Documents/A.MyDoc/A.DriveE/DataScience/NYCDSA/40_Projects/C01.Shiny/shinyappsio/canslove')

rm(list=ls())
N_RANK = 100
Region_Factor = 0

raw.df <- as.data.frame(fread("./data/data_reduced.csv", stringsAsFactors = F))
region.df <- as.data.frame(fread("./data/region_code_reduced.csv", header=TRUE, stringsAsFactors = F))
region_map.df <- as.data.frame(fread("./data/region_map.csv", header=TRUE, stringsAsFactors = F))

# Change Variable Date Type ----------------------------------
raw.df <- raw.df %>% mutate(DATE = as.Date(DATE, "%Y-%m-%d"))
if(Region_Factor){
  raw.df <- raw.df %>% mutate(REGION = as.factor(REGION))
}
#class(raw.df$DATE)

# Data Structure Check ---------------------------------------
summary(raw.df)
head(raw.df,5)
names(raw.df)
U.ranking <- unique(raw.df$RANKING)
cat('# of ranking : \t\t', length(U.ranking)) # 100 ranks
U.track_name <- unique(raw.df$TRACK_NAME)
cat('# of unique track_name : \t', length(U.track_name)) # 9967 songs
U.regions <- as.data.frame(unique(raw.df$REGION))
names(U.regions) = "REGION"
unique(raw.df$REGION)
cat('# of unique regions : \t', length(U.regions$region_code)) # 54 Regions
U.date <- summary(raw.df$DATE)
U.date # 1/1/2017 ~ 1/9/2018
U.artist <- unique(raw.df$ARTIST)
cat('# of unique artists : \t', length(U.artist)) # 3757 artists
#mday(raw.df$DATE[1000])
str(raw.df)
# "RANKING" "TRACK_NAME" "ARTIST" "STREAMS" "URL" "DATE" "REGION"


# EDA -----
sum(is.na(raw.df))


# Top streamed songs (to see streaming power over the Region) -----

head(raw.df, 5)
str(raw.df)
tot_str_year <- raw.df %>%
  group_by(REGION) %>%
  summarise(tot_streams_mil = sum(STREAMS/1000000)) %>%  # million
  arrange(desc(tot_streams_mil))
View(tot_str_year)
write.csv(tot_str_year, file='./data_not_used/tot_str_year.csv', row.names=FALSE)
#summary(warnings())
tot_str_year$REGION2 <- factor(tot_str_year$REGION, as.character(tot_str_year$REGION))
p <- ggplot(tot_str_year, aes(x=REGION2, y=tot_streams_mil)) +
  geom_bar(aes(), stat = "identity", fill="blue") +
  ggtitle('Regional Total Downloaded Streams in 2017 (unit:million)') +
  xlab('Region') +
  ylab('Number of streams')+
  geom_vline(aes(xintercept = 1.5), colour = "#FF6666") +
  geom_vline(aes(xintercept = 11.5), colour = "#FF6666") +
  theme(axis.text.x=element_text(angle = -90, hjust = 0))
  #coord_flip()
ggplotly(p)
# -> Top 10 country : c('global', 'us', 'gb', 'br', 'mx', 'de', 'es', 'nl', 'se', 'au', 'ph'))
top10 = c('The USA', 'Great Britain', 'Brazil', 'Mexico', 'Germany', 
          'Ecuador', 'Netherlands', 'Sweden', 'Australia', 'Philippines')



sum_stream.df <- raw.df %>% group_by(REGION, TRACK_NAME, ARTIST) %>%
  summarise(tot_streams = sum(STREAMS), avg_ranking = mean(RANKING)) %>%
  arrange(desc(tot_streams))
summary(sum_stream.df)
head(sum_stream.df)

# Top 1 donwloaded stream in every country -----
top1.df <- sum_stream.df %>% group_by(REGION) %>% top_n(1, tot_streams) #%>% arrange(REGION, desc(tot_streams))
top7ctry = head(top1.df, 8)
write.csv(top7ctry, file='./data_not_used/top7ctry.csv', row.names=FALSE)
summary(soy_df)
library(gridExtra)
library(grid)
grid.table(top7ctry)
top7ctry_list = (head(top1.df, 8))[,1] 
# global, us, gb(GreatBritain), mx(Mexico), br(Brazil), de(Germany), es(Ecuador), se(Singapore)

# pie chart for soy_df -----

# "Shape of You" by Ed Sheeran in every country
soy_df <- sum_stream.df %>% filter(TRACK_NAME == "Shape of You")
write.csv(soy_df, file='./data_not_used/soy_df.csv', row.names=FALSE)
summary(soy_df)
head(soy_df,5)
p <- ggplot(soy_df, aes(x=REGION, y=(tot_streams))) +
  geom_bar(aes(), stat = "identity", fill = "green") + #, position = "dodge") +
  ggtitle("Ed Sheeran-'Shape of You' : Number of streams in every country") +
  xlab("Country") +
  ylab("Total Download Streams") +
  theme(axis.text.x=element_text(angle = -90, hjust = 0)) +
  theme(plot.title = element_text(hjust = -0.45, vjust=2.12))
ggplotly(p)


# 7 countries =============
seven_ctry2017.df <- raw.df %>%
  #filter(REGION %in% c('global', 'us', 'gb', 'mx', 'br', 'de', 'es', 'se'))
  filter(REGION %in% c('us', 'gb', 'mx', 'br', 'de', 'es', 'se')) %>%
  filter(DATE <= "2017-12-31")

# Monthly Top 10 songs in 7 countries -----
# monthly_7ctry.df <- seven_ctry2017.df %>%
#   mutate(month = month(seven_ctry2017.df$DATE)) %>%
#   group_by(REGION, month, TRACK_NAME, ARTIST) %>%
#   summarise(tot_streams = sum(STREAMS), avg_ranking = mean(RANKING)) %>%
#   arrange(REGION, month, avg_ranking) %>%
#   top_n(10, avg_ranking)

# Monthly Total Streams in all countries -----
# monthly_7ctry.df <- raw.df %>%
#   mutate(month = month(raw.df$DATE)) %>%
#   filter(REGION != "global") %>%
#   group_by(REGION, month) %>%
#   summarise(tot_streams = sum(STREAMS)) %>%
#   arrange(REGION, month)

# Monthly Total Streams in 7 countries -----
monthly_7ctry.df <- seven_ctry2017.df %>%
  mutate(month = month(seven_ctry2017.df$DATE)) %>%
  group_by(REGION, month) %>%
  summarise(tot_streams = sum(STREAMS)) %>%
  arrange(REGION, month)
write.csv(monthly_7ctry.df, file='./data_not_used/monthly_7ctry.csv', row.names=FALSE)

head(monthly_7ctry.df)
p <- ggplot(monthly_7ctry.df, aes(x=as.factor(month), y=tot_streams, group = REGION)) +
  geom_bar(aes(fill=REGION), stat = "identity", position = "dodge") +
  ggtitle("Total number of streams in every month, Seven countries") +
  xlab("Month") +
  ylab("Total Download Streams") +
  theme(axis.text.x=element_text(angle = -90, hjust = 0)) +
  theme(plot.title = element_text(hjust = -0.45, vjust=2.12))
ggplotly(p)

# Top3 streamed songs in 7 countries in 2017-----
top3song7ctry <- raw.df %>%
  filter(REGION %in% c('us', 'gb', 'mx', 'br', 'de', 'es', 'se')) %>%
  #filter(DATE <= "2017-12-31") %>%
  #filter((DATE >= "2017-01-01") & (DATE <= "2017-07-31")) %>%
  filter((DATE >= "2017-08-01") & (DATE <= "2017-12-31")) %>%
  group_by(REGION, TRACK_NAME) %>%
  summarise(total_stream = sum(STREAMS)) %>%
  arrange(REGION, total_stream) %>%
  top_n(3, (total_stream))
#write.csv(top3song7ctry, file='./data_not_used/top3song_2017full.csv', row.names=FALSE)
#write.csv(top3song7ctry, file='./data_not_used/top3song_2017_1sthalf.csv', row.names=FALSE)
write.csv(top3song7ctry, file='./data_not_used/top3song_2017_2ndhalf.csv', row.names=FALSE)

head(top3song7ctry)
p <- ggplot(top3song7ctry, aes(x=REGION, y=total_stream)) +
  geom_bar(aes(fill=TRACK_NAME), stat = "identity", position = "stack") +
  #ggtitle("Total number of streams in 2017, Seven countries") +
  #ggtitle("Total number of streams from Jan. to Jul. in 2017, Seven countries") +
  ggtitle("Total number of streams from Aug. to Dec. in 2017, Seven countries") +
  xlab("REGION") +
  ylab("Number of Streams|Track_Name") +
  #theme(axis.text.x=element_text(angle = -90, hjust = 0)) +
  theme(plot.title = element_text(hjust = -0.45, vjust=2.12)) + 
  coord_flip()
ggplotly(p)

a <- read.csv('./data_not_used/top3song_2017full.csv')
b <- read.csv('./data_not_used/top3song_2017_1sthalf.csv')
c <- read.csv('./data_not_used/top3song_2017_2ndhalf.csv')
#d <- inner_join(a, b, by = "REGION")
d <- left_join(a, b, by = "REGION")

# Top3 loved artist in 7 countries in 2017 -----
  

# Monthly # of streams for Top1 song in every country -----

# Top 10 song comparison in 7 countries (daily) -----







# "RANKING"    "TRACK_NAME" "ARTIST"     "STREAMS"    "DATE"       "REGION"













# Word cloud -----
track_4_wc <- raw.df %>% 
  filter(RANKING <= 30) %>%
  select(ARTIST, TRACK_NAME, DATE, REGION)

track_4_wc$TRACK_NAME <- sapply(track_4_wc$TRACK_NAME, tolower)
track_4_wc$ARTIST <- sapply(track_4_wc$ARTIST, tolower)
head(track_4_wc, 10)

# Wordcloud(p188, p184, 182) for artist and title -> To find who and which is most or least significant.

library(tm)
library(wordcloud)
library(RColorBrewer)
pal = brewer.pal(6,"RdGy")
pal2 <- brewer.pal(8,"Dark2")


# Test ------------------------
mydates <- as.Date(c("2017-01-01", "2017-02-20"))
myyear <- list(year(mydates) == "2007")
myyear

# Test end --------------------
sel = c("TRACK_NAME", "ARTIST")
wc_df <- track_4_wc %>% 
  filter(REGION == "us",
         DATE > (mydates[1]) & DATE < (mydates[2])) %>%
  #select(TRACK_NAME)
  select(sel[1])

# wc_df <- track_4_wc %>% 
#   filter(REGION == "us" & year(DATE) == "2017") %>% 
#   select(TRACK_NAME, DATE)
head(wc_df, 10)

wc_df_tt <- wc_df$TRACK_NAME


par(mfrow = c(1, 3), bg="black")
wordcloud(wc_df_tt, min.freq = 30, max.words=Inf, scale = c(4,0.5), random.color = TRUE, colors = pal)
title(main="Words in Daily Top30 more than 30 times in 2017",outer= T,
      cex.main = 2,   font.main= 2, col.main= "blue")
# ==> Now find the 10 artists who has a lot of frequency

# Text mining for frequency calculation to use Wordcloud2 ----------------------
FreqMining <- function(INPUT){
  d_artist_sp1 <- tolower(INPUT)
  d_artist_sp2 <- strsplit(d_artist_sp1, "\\W")
  d_artist_sp3 <- unlist(d_artist_sp2)
  freq<-table(d_artist_sp3)
  freq1<-sort(freq, decreasing=TRUE)
  freq1_df <- as.data.frame(freq1)
  colnames(freq1_df) = c('word', 'freq')
  
  return(freq1_df)
}

tm_tr = FreqMining(wc_df_tt)

# Wordcloud2 -----------------------------------------
#require(devtools)
#install_github("lchiffon/wordcloud2")
library(wordcloud2)

tm_tr <- head(tm_tr,100)

#wordcloud2(data = tm_sp, size = 1.0)
wordcloud2(tm_tr, size = 1.0, backgroundColor = "black", minRotation = -pi/2, maxRotation = -pi/2)







## Appendix codes ===============================================



#raw.dt <- fread("../Data/data.csv", stringsAsFactors = F)
#raw.df <- as.data.frame(raw.dt)
df_spotify = read.csv('./z_datasets/spotify_wkly.csv', stringsAsFactors=FALSE)
df_youtube = read.csv('./z_datasets/youtubemusic.csv', stringsAsFactors=FALSE, header = FALSE,
                      col.names = c('start_date','end_date','rank','title','artist','views'))
df_billboard = read.csv('./z_datasets/acharts_singles.csv', stringsAsFactors=FALSE)
df_billboard_albums = read.csv('./z_datasets/acharts_albums.csv', stringsAsFactors=FALSE)
df_itunes = read.csv('./z_datasets/itunes.csv', stringsAsFactors=FALSE)

# check data Nas --------------------------------
df_list = c('df_spotify', 'df_youtube', 'df_billboard', 'df_billboard_albums', 'df_itunes')
print('Nas in df_spotify : ', sum(is.na(df_spotify)))
print('Nas in df_youtube : ', sum(is.na(df_youtube)))
print('Nas in df_billboard : ', sum(is.na(df_billboard)))
print('Nas in df_billboard_albums : ', sum(is.na(df_billboard_albums)))
print('Nas in df_itunes : ', sum(is.na(df_itunes)))
which( rowSums( is.na(df_spotify) ) != 0 )

# column name check --------------------------------
colnames(df_spotify)
colnames(df_youtube)
colnames(df_billboard)
colnames(df_billboard_albums)
colnames(df_itunes)

# column name tolower --------------------------------
#names(df) <- tolower(names(df))

# reorder columns --------------------------------
df_spotify <- df_spotify[c("start_date", "end_date", "rank","title", "artist", "streams")]
#df_youtube <- df_youtube[c("start_date", "end_date", "rank","title", "artist", "views")]
df_billboard <- df_billboard[c("date", "rank","title", "artist", "weeks")]
df_billboard_albums <- df_billboard_albums[c("date", "rank","title", "artist", "weeks")]
df_itunes <- df_itunes[c("date", "release_date", "title", "artist", "genre")]

# Summary --------------------------------
summary(df_spotify)
summary(df_youtube)
summary(df_billboard)
summary(df_billboard_albums)
summary(df_itunes)

# Change Variable Data Type ----------------------------------
df_spotify_org <- df_spotify
df_spotify <- df_spotify %>% 
  mutate(start_date = as.Date(start_date, "%Y-%m-%d")) %>%
  mutate(end_date = as.Date(end_date, "%Y-%m-%d")) %>%
  mutate(rank = as.integer(rank)) %>%
  mutate(streams = as.integer(streams))
which( rowSums( is.na(df_spotify) ) != 0 )
if(sum(is.na(df_spotify))!=0){
  df_spotify <- na.omit(df_spotify)
}
summary(df_spotify)

df_youtube_org <- df_youtube
df_youtube <- df_youtube %>% 
  mutate(start_date = as.Date(start_date, "%Y-%m-%d")) %>%
  mutate(end_date = as.Date(end_date, "%Y-%m-%d")) #%>%
  #mutate(rank = as.integer(rank)) # first, rank should be changed into number
which( rowSums( is.na(df_youtube) ) != 0 )
if(sum(is.na(df_youtube))!=0){
  df_youtube <- na.omit(df_youtube)
}
summary(df_youtube)

df_billboard_org <- df_billboard
df_billboard <- df_billboard %>% 
  mutate(date = as.Date(date, "%Y-%m-%d"))
which( rowSums( is.na(df_billboard) ) != 0 )
if(sum(is.na(df_billboard))!=0){
  df_billboard <- na.omit(df_billboard)
}
summary(df_billboard)

df_billboard_albums_org <- df_billboard_albums
df_billboard_albums <- df_billboard_albums %>% 
  mutate(date = as.Date(date, "%Y-%m-%d"))
which( rowSums( is.na(df_billboard_albums) ) != 0 )
if(sum(is.na(df_billboard_albums))!=0){
  df_billboard_albums <- na.omit(df_billboard_albums)
}
summary(df_billboard_albums)

df_itunes_org <- df_itunes


# reorder rows --------------------------------
df_spotify <- arrange(df_spotify, start_date, rank)
df_youtube <- arrange(df_youtube, start_date, rank)
df_billboard <- arrange(df_billboard, date, rank)
df_billboard_albums <- arrange(df_billboard_albums, date, rank)


# Data Snooping --------------------------------
head(df_spotify,5)
head(df_youtube,5)
head(df_billboard,5)
head(df_billboard_albums,5)
head(df_itunes,5)

# Data trim to synchronize -----------------------------------
ViewPeriod = c("2017-09-15","2018-01-26")
df_spotify = subset(df_spotify, select = -c(end_date))
df_spotify <- df_spotify %>%
  filter(start_date >= as.Date(ViewPeriod[1]) & start_date <= as.Date(ViewPeriod[2])) %>%
  filter(rank <= 100)
df_youtube = subset(df_youtube, select = -c(end_date))
colnames(df_billboard)[1] <- "start_date"
df_billboard$start_date = df_billboard$start_date-1 # date sync with spotify and youtube
df_billboard <- df_billboard %>%
  filter(start_date >= as.Date(ViewPeriod[1]) & start_date <= as.Date(ViewPeriod[2])) %>%
  filter(rank <= 100)
colnames(df_billboard_albums)[1] <- "start_date" 
df_billboard_albums$start_date = df_billboard_albums$start_date-1 # date sync with spotify and youtube
df_billboard_albums <- df_billboard_albums %>%
  filter(start_date >= as.Date(ViewPeriod[1]) & start_date <= as.Date(ViewPeriod[2])) %>%
  filter(rank <= 100)

# New column for source identification
source = rep('spotify', length(df_spotify$rank))
df_spotify$source = source

source = rep('youtube', length(df_youtube$rank))
df_youtube$source = source

source = rep('billboard', length(df_billboard$rank))
df_billboard$source = source

source = rep('billboard_albums', length(df_billboard_albums$rank))
df_billboard_albums$source = source

# Data Cleaning # remove multibyte string --------------------------------
df_spotify$title[1094] <- "Blame me"
df_spotify$title <- tolower(df_spotify$title)
df_youtube$title[166] <- "Let me down"
df_youtube$title[276] <- "Let me down"
df_youtube$title[353] <- "Let me down"
df_youtube$title[952] <- "Ready for it?"
df_youtube$title[1061] <- "Ready for it?"
df_youtube$title[1177] <- "Ready for it?"
df_youtube$title[1198] <- "Let me down"
df_youtube$title[1261] <- "Ready for it?"
df_youtube$title[1392] <- "Ready for it?"
df_youtube$title[1584] <- "Not Hot"
df_youtube$title <- tolower(df_youtube$title)
df_billboard$title <- tolower(df_billboard$title)

# Data join ---------------------------------------------------------------
df_sp = subset(df_spotify, select = -c(streams))
df_yt = subset(df_youtube, select = -c(views))
df_bb = subset(df_billboard, select = -c(weeks))
df_total = rbind(df_sp, df_yt, df_bb)

df_total$title <- tolower(df_total$title)
df_total$artist <- tolower(df_total$artist)


# Top 10 glance to compare in a week [facet_grid mode] -----------------------------------
date_range = c(as.Date("2017-09-15"), as.Date("2017-09-21"))
date_range = date_range#+ 7*2
rank_range = c(1, 10)

geom_User <- switch("Bar",
                    "Scatter" = geom_point(aes(color='green')),
                    "Bar" = geom_bar(aes(color='green'),stat="identity"),
                    "Histogram" = geom_histogram(aes(color='green', fill='yellow'),stat="identity"),
                    "Distribution" = geom_density(aes(fill='lime'),stat="identity"))

df_sel <- df_total %>%
  filter(start_date >= (date_range[1]) & start_date <= (date_range[2])) %>%
  filter(rank<=rank_range[2] & rank >= rank_range[1]) %>%
  mutate(rank_bar = as.numeric(11-rank))

p <- ggplot(df_sel, aes(x=-rank, y=rank_bar, label=title, group=source )) +
  geom_bar(aes(fill= source), stat = "identity") + #, position = "dodge") +
  ggtitle(paste0("Rankings in each Sources (",rank_range[1],"~",rank_range[2],")@week of ", date_range[1] )) +
  geom_text(size = 3.5, position = position_stack(vjust = 0.7)) + #1.2
  facet_grid(.~source)+
  xlab("Rank") +
  ylab("Track Name") +
  coord_flip() +
  theme_tufte()
ggplotly(p)

# Top 10 glance to compare in a week [dodge mode] -----------------------------------
#   -> Arrange by rank per sources (but hard to distinguish)
p <- ggplot(df_sel, aes(x=-rank, y=rank_bar)) +#, label= paste0(title,"(",artist,")" ))) +
  geom_bar(aes(fill= source), stat = "identity" , position = "dodge") +
  #geom_label(aes(fill = source), colour = "white", fontface = "bold") +
  geom_text(aes(x=-rank, y=rank_bar, label= paste0(title,"(",artist,")" ), group=source), 
            size = 3, position = position_dodge(width = 1.0)) +
  ggtitle(paste0("Rankings in each Sources (",rank_range[1],"~",rank_range[2],") in week of ", date_range[1] )) +
  xlab("Rank") +
  ylab("Track Name") +
  coord_flip() +
  theme_tufte()
#resolution(df_sel$x)
ggplotly(p)

# Top 10 glance to compare in a week [by artist] -----------------------------------
# Arranging by artist (rank is mixed but easy to see rank difference)
p <- ggplot(df_sel, aes(x=artist, y=rank_bar)) + #, label=rank )) +
  geom_bar(aes(fill= source), stat = "identity", position = "dodge") +
  #geom_text(size = 3, position = position_stack(vjust = 0.3)) +
  geom_text(aes(x=artist, y=rank_bar, label= rank, group=source),
            size = 4, position = position_dodge(width = 0.9), vjust=-0.5, hjust= 1.0) +
  ggtitle(paste0("Rankings in each Sources (",rank_range[1],"~",rank_range[2],") in week of ", date_range[1])) +
  ylab("Rank") +
  xlab("Artist") +
  coord_flip() +
  theme_tufte()
ggplotly(p)


# Wordcloud(p188, p184, 182) for artist and title -> To find who and which is most or least significant.
library(tm)
library(wordcloud)
library(RColorBrewer)
pal = brewer.pal(6,"RdGy")
d_artist_sp = tolower(df_sp$artist)
d_artist_yt = tolower(df_yt$artist)
d_artist_bb = tolower(df_bb$artist)

pal2 <- brewer.pal(8,"Dark2")
par(mfrow = c(1, 3), bg="black")
wordcloud(d_artist_sp, min.freq = 30, max.words=Inf, scale = c(4,0.5), random.color = TRUE, colors = pal)
title("\n\n\n\n\n Spotify", cex.main = 2,   font.main= 4, col.main= "white")
wordcloud(d_artist_yt, min.freq = 30, max.words=Inf, scale = c(4,0.5), random.color = TRUE, colors = pal2)
title("\n\n\n\n\n YouTube", cex.main = 2,   font.main= 4, col.main= "white")
wordcloud(d_artist_bb, min.freq = 30, max.words=Inf, scale = c(4,0.5), random.color = TRUE, colors = pal)
title("\n\n\n\n\n Billboard", cex.main = 2,   font.main= 4, col.main= "white")
title(main="\n\n\n\n Artists who are registered in Top100 more than 30 times",outer= T,
      cex.main = 2,   font.main= 2, col.main= "blue")
# ==> Now find the 10 artists who has a lot of frequency

# Text mining for frequency calculation to use Wordcloud2 ----------------------
FreqMining <- function(INPUT){
  d_artist_sp1 <- tolower(INPUT)
  d_artist_sp2 <- strsplit(d_artist_sp1, "\\W")
  d_artist_sp3 <- unlist(d_artist_sp2)
  freq<-table(d_artist_sp3)
  freq1<-sort(freq, decreasing=TRUE)
  freq1_df <- as.data.frame(freq1)
  colnames(freq1_df) = c('word', 'freq')
  
  return(freq1_df)
}

tm_sp = FreqMining(d_artist_sp)
tm_yt = FreqMining(d_artist_yt)
tm_bb = FreqMining(d_artist_bb)
min_len = min(length(tm_sp$word), length(tm_yt$word), length(tm_bb$word) )
tm_all = cbind(tm_sp[1:min_len,],tm_yt[1:min_len,],tm_bb[1:min_len,])
head(tm_all,20)

# Wordcloud2 -----------------------------------------
#require(devtools)
#install_github("lchiffon/wordcloud2")
library(wordcloud2)

#wordcloud2(data = tm_sp, size = 1.0)
wordcloud2(tm_sp, size = 1.0, backgroundColor = "black", minRotation = -pi/2, maxRotation = -pi/2)
wordcloud2(tm_yt, size = 1.0, backgroundColor = "black", minRotation = -pi/2, maxRotation = -pi/2)
wordcloud2(tm_bb, size = 1.0, backgroundColor = "black", minRotation = -pi/2, maxRotation = -pi/2)
wordcloud2(tm_sp, size = 1.0, minRotation = -pi/6, maxRotation = -pi/6, rotateRatio = 1)
wordcloud2(tm_yt, size = 1.0, minRotation = -pi/6, maxRotation = -pi/6, rotateRatio = 1)
wordcloud2(tm_bb, size = 1.0, minRotation = -pi/6, maxRotation = -pi/6, rotateRatio = 1)

# wordcloud2 with shape
#wordcloud2(tm_sp, size = 1, shape = 'star') # "cardioid", "diamond"

# Lettercloud ---------------------------------------
#letterCloud( demoFreq, word = "R", color='random-light' , backgroundColor="black")
#letterCloud( demoFreq, word = "PEACE", color="white", backgroundColor="pink")
letterCloud(tm_sp, word = "Spotify", color='random-light' , backgroundColor="black")
letterCloud(tm_yt, word = "YouTube", color='random-light' , backgroundColor="black")
letterCloud(tm_bb, word = "Billboard", color='random-light' , backgroundColor="black")

# with backgroud image
#wordcloud2(tm_sp, figPath = "./figures/twitter.png", size = 1.5,color = "skyblue")
wordcloud2(tm_sp, figPath = "./figures/spotify.png", size = 1.5,color = "green")
wordcloud2(tm_yt, figPath = "./figures/youtube.png", size = 1.5,color = "red")
wordcloud2(tm_bb, figPath = "./figures/billboard.png", size = 1.5,color = "black")

# Pie Charts  w/ Freq ==================================================================

# frequency distribution
h = c(mean(tm_sp$freq),mean(tm_yt$freq),mean(tm_bb$freq))
p <- ggplot(data = tm_sp, aes(x = word, y = freq, fill = word)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  geom_hline(aes(yintercept = h[1])) +
  ggtitle(paste0("Word and its frequency in Spotify Top100 (", ViewPeriod[1], "~", ViewPeriod[2] ,")")) +
  geom_text(aes(0, h[1], label=paste0('average_frequency', as.character(format(round(h[1], 2), nsmall = 2))), hjust=-2, vjust=-1)) +
  theme_bw()
p
ggplotly(p)

p <- ggplot(data = tm_yt, aes(x = word, y = freq, fill = word)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  geom_hline(aes(yintercept = h[2])) +
  ggtitle(paste0("Word and its frequency in Spotify Top100 (", ViewPeriod[1], "~", ViewPeriod[2] ,")")) +
  geom_text(aes(0, h[2], label=paste0('average_frequency', as.character(format(round(h[2], 2), nsmall = 2))), hjust=-2, vjust=-1)) +
  theme_bw()
p
ggplotly(p)

p <- ggplot(data = tm_bb, aes(x = word, y = freq, fill = word)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  geom_hline(aes(yintercept = h[3])) +
  ggtitle(paste0("Word and its frequency in Spotify Top100 (", ViewPeriod[1], "~", ViewPeriod[2] ,")")) +
  geom_text(aes(0, h[3], label=paste0('average_frequency', as.character(format(round(h[3], 2), nsmall = 2))), hjust=-2, vjust=-1)) +
  theme_bw()
p
ggplotly(p)

# Word-Freq df cleaning cut-off = 30 freq.
title_freq = read.csv('./y_codeoutput/title_freq.csv', stringsAsFactors=FALSE)
title_freq$word = tolower(title_freq$word)

# Add addition columns to data, needed for donut plot.
title_freq <- title_freq %>% 
  group_by(source) %>%
  mutate(fraction = freq/sum(freq)) %>%
  mutate(percentage = round(fraction*100)) %>%
  mutate(ymax = cumsum(fraction)) %>%
  mutate(ymin = c(0, head(ymax, n = -1))) %>%
  mutate(lbls = paste(word, percentage)) %>%  # add percents to labels 
  mutate(lbls = paste(lbls, "%", sep=""))

tmp_sp = as.numeric(unlist(as.list(title_freq[title_freq$source=="spotify","freq"])))
tmp_yt = as.numeric(unlist(as.list(title_freq[title_freq$source=="youtube","freq"])))
tmp_bb = as.numeric(unlist(as.list(title_freq[title_freq$source=="billboard","freq"])))
lbls_sp = title_freq$lbls[title_freq$source=="spotify"]
lbls_yt = title_freq$lbls[title_freq$source=="youtube"]
lbls_bb = title_freq$lbls[title_freq$source=="billboard"]

par(mfrow = c(1, 3)) # multiframe by row
pie(tmp_sp,labels = lbls_sp, col=rainbow(length(lbls_sp)), main="Spotify")
pie(tmp_sp,labels = lbls_yt, col=rainbow(length(lbls_yt)), main="YouTube")
pie(tmp_sp,labels = lbls_bb, col=rainbow(length(lbls_bb)), main="Billboard")

# pie chart more beautiful picture find !!!!!
##############################################
###  Completed by this point
##############################################

#------------------------------------------------------------------

# Mosaic Plot
#   Divide the data according to different variables, and then use rectangles of different
#   sizes to represent different groups of data.

#install.packages("vcd")
library(vcd)
mosaic(Survived ~ Class + Sex, data=Titanic, shade=T,
       highlighting_fill=c('red4',"skyblue"),
       highlighting_direction="left")
# ------------------------------------------------------------









# * Slope chart(p112) -> show how ranking differ site from site
titlelist <- function(artist_name){
  tmp <- df_total %>%
    # mutate(artist = tolower(artist)) %>%
    # mutate(title = tolower(title)) %>%  # invalid multibyte string 1094
    filter(artist == tolower(artist_name))
  list_tmp = unique(tmp$title)
  return(list_tmp)
}
#c('Post Malone', 'Kodak Black', 'Taylor swift', 'Ed Sheeran', 'Imagine Dragons')){
Lpmalone = titlelist('Post Malone')
Lkodak = titlelist('Kodak Black')
Ltaylor = titlelist('Taylor swift')
Lsheeran = titlelist('Ed Sheeran')
Limagine = titlelist('Imagine Dragons')

# To merge the same title 
Lpmalone = unique(tolower(Lpmalone))
Lkodak = unique(tolower(Lkodak))
#Ltaylor = Ltaylor[-c(7, 13)]
Ltaylor = unique(tolower(Ltaylor))
Lsheeran = unique(tolower(Lsheeran))
Limagine = unique(tolower(Limagine))

# rank trend comaprison b/w service provider
plot_trend <- function(at_sel, title_sel){
  df_total %>%
    #mutate(artist = tolower(artist)) %>%
    filter(artist == at_sel) %>%
    #mutate(title = tolower(title)) %>%
    filter(title %in% title_sel) %>%
    #group_by(source) %>%
    ggplot(aes(x=start_date, y=-1*rank, group = source)) +
    geom_point(aes(color = source)) +
    geom_line(aes(color = source)) +
    #theme_tufte() +
    #theme(plot.background = element_rect(fill = "black")) +
    #theme_wsj()+ scale_colour_wsj("colors6") +
    #theme_calc()+ scale_colour_calc() +
    theme_hc()+ scale_colour_hc() + theme(plot.background = element_rect(fill = "black")) +
    ggtitle(paste0("Weekly Ranking Trend - ", at_sel,"(",title_sel,")")) + #Selected Artists") +
    xlab("DATE") + ylab("RANKING") +
    theme(plot.title = element_text(color="white", size=14, face="bold.italic"),
          axis.title.x = element_text(color="grey", size=12),#, face="bold"),
          axis.title.y = element_text(color="grey", size=12))#, face="bold"))
}
# --------------------------------------------------
# 'Post Malone' : c("rockstar","congratulations","go flex","candy paint", "i fall apart", "white iverson")
# 'Kodak Black' : c("roll in peace", "transportin", "tunnel vision", "codeine dreaming", "no flockin", "patty cake","halloween", "patty cake", "too much money", "roll in peace")   )
# 'Taylor swift' : c(""Look What You Made Me Do", "...ready for it?", "gorgeous", "call it what you want", "end game", "i did something bad", "delicate")
# 'Ed Sheeran' : c("shape of you", "perfect", "thinking out loud") # derivative  should be merged again
# 'Imagine Dragons' : c("thunder", "believer")
# --------------------------------------------------
at_sel = tolower("Post Malone")
#title_sel = tolower(c("rockstar", "rockstar (feat. 21 savage)"))
#title_sel = tolower(c("congratulations", "congratulations (feat. quavo)"))
#title_sel = tolower(c("candy paint"))
#title_sel = tolower(c("i fall apart"))
plot_trend(at_sel,title_sel)

at_sel = tolower("Kodak Black")
#title_sel = tolower(c("roll in peace", "roll in peace (feat. xxxtentacion)"))
title_sel = tolower(c("transportin'"))
plot_trend(at_sel,title_sel)

at_sel = tolower("Taylor swift")
#title_sel = tolower(c("look what you made me do"))
#title_sel = tolower(c("...ready for it?", "ready for it?"))
#title_sel = tolower(c("gorgeous"))
title_sel = tolower(c("end game", "end game (feat. ed sheeran & future)"))
plot_trend(at_sel,title_sel)

at_sel = tolower("Ed Sheeran")
#title_sel = tolower(c("shape of you"))
#title_sel = tolower(c("perfect", "perfect duet (ed sheeran & beyoncé)", "perfect duet (feat. beyoncÉ)", "perfect symphony (feat. andrea bocelli)", "perfect duet (ed sheeran & beyoncÉ)"))
plot_trend(at_sel,title_sel)

at_sel = tolower("Imagine Dragons")
#title_sel = tolower(c("thunder"))
title_sel = tolower(c("believer"))
plot_trend(at_sel,title_sel)

# Join with genre -------------------
df_itunes= subset(df_itunes, select = -c(date, release_date, artist))
df_itunes$title <- tolower(df_itunes$title)

df_spotify_genre <- left_join(df_spotify, df_itunes, by = "title")
df_youtube_genre <- left_join(df_youtube, df_itunes, by = "title")
df_billboard_genre <- left_join(df_billboard, df_itunes, by = "title")
df_total_genre <- left_join(df_total, df_itunes, by = "title")

# frequency in genre for respective provider
wordcloud(df_spotify_genre$genre, min.freq = 1, scale = c(2,0.5), random.color = TRUE)#, color = pal)
wordcloud(df_youtube_genre$genre, min.freq = 1, scale = c(2,0.5), random.color = TRUE)#, color = pal)
wordcloud(df_billboard_genre$genre, min.freq = 1, scale = c(2,0.5), random.color = TRUE)#, color = pal)

freq_sp_genre = freq_cnt_(df_spotify_genre$genre)
freq_yt_genre = freq_cnt_(df_youtube_genre$genre)
freq_bb_genre = freq_cnt_(df_billboard_genre$genre)
# ==> genre should be surveyed more to fill all the blank,

# Hot to make Top 100 in each site -----------------------
# 1) Sportify
#     -> Total number of downloaded stream
# 2) Youtube
#     -> Total number of viewed music on YouTube by video, artist, track, and viral score:
# 3) Billboard
#     -> This week's most popular songs across all genres, 
#        ranked by radio airplay audience impressions as measured by Nielsen Music, 
#        sales data as compiled by Nielsen Music and streaming activity data provided by online music sources.

genre_freq = read.csv('./y_codeoutput/genre_freq.csv', stringsAsFactors=FALSE)
genre_freq$genre = tolower(genre_freq$genre)
# Add addition columns to data, needed for donut plot.
genre_freq <- genre_freq %>% 
  group_by(source) %>%
  mutate(fraction = freq/sum(freq)) %>%
  mutate(percentage = round(fraction*100)) %>%
  mutate(ymax = cumsum(fraction)) %>%
  mutate(ymin = c(0, head(ymax, n = -1))) %>%
  mutate(lbls = paste(genre, percentage)) %>%  # add percents to labels 
  mutate(lbls = paste(lbls, "%", sep=""))

tmp_sp = as.numeric(unlist(as.list(genre_freq[genre_freq$source=="spotify","freq"])))
tmp_yt = as.numeric(unlist(as.list(genre_freq[genre_freq$source=="youtube","freq"])))
tmp_bb = as.numeric(unlist(as.list(genre_freq[genre_freq$source=="billboard","freq"])))
lbls_sp = genre_freq$lbls[genre_freq$source=="spotify"]
lbls_yt = genre_freq$lbls[genre_freq$source=="youtube"]
lbls_bb = genre_freq$lbls[genre_freq$source=="billboard"]

par(mfrow = c(1, 3)) # multiframe by row
pie(tmp_sp,labels = lbls_sp, col=rainbow(length(lbls_sp)), main="Spotify")
pie(tmp_sp,labels = lbls_yt, col=rainbow(length(lbls_yt)), main="YouTube")
pie(tmp_sp,labels = lbls_bb, col=rainbow(length(lbls_bb)), main="Billboard")









# * Mosaic map for each week per sites -> show how different the ranking is



# 1) Treemap(p75) for artist and title -> To find who and which is most or least significant.
# 2) Selected most / least artist/title 
#    line plot -> compare rankinng trend for 3 sites
#    radial plot(p151) -> compare 


# cf) Analysis by genre -> pie chart (p124,; p102,104,106,109)

# cf) bubble plot?

# Top 10 compare in every week --------------------------------------------
date_range = c(as.Date("2017-09-15"), as.Date("2017-09-21"))
rank_range = c(1, 10)

plot1 <- df_total %>%
  filter(start_date >= (date_range[1]) & start_date <= (date_range[2])) %>%
  filter(rank<=rank_range[2] & rank >= rank_range[1]) %>%
  mutate(rank_bar = as.numeric(11-rank))
  #group_by(title, source, artist) %>%
  #summarise(sum_Stream = sum(streams)) %>%
  #group_by(source) %>%
  #top_n(-rank_range[2]) 
  #top_n(rank, sum_Stream) %>%
  #arrange(source, desc(sum_Stream))





  #==> correatio ???? ?? ?ֳ??  p(191?, 176, 178)
  #--> ??????ũ ????��?? ?? ?ֺ? ????
  
  


























# Below is not the actual codes
#========================================================================================================

# Data Structure Check ---------------------------------------
summary(raw.df)
head(raw.df,5)
names(raw.df)
U.ranking <- unique(raw.df$RANKING)
cat('# of ranking : \t\t', length(U.ranking)) # 100 ranks
U.track_name <- unique(raw.df$TRACK_NAME)
cat('# of unique track_name : \t', length(U.track_name)) # 9967 songs
U.regions <- as.data.frame(unique(raw.df$REGION))
names(U.regions) = "REGION"
unique(raw.df$REGION)
cat('# of unique regions : \t', length(U.regions$region_code)) # 54 Regions
U.date <- summary(raw.df$DATE)
U.date # 1/1/2017 ~ 1/9/2018
U.artist <- unique(raw.df$ARTIST)
cat('# of unique artists : \t', length(U.artist)) # 3757 artists
#mday(raw.df$DATE[1000])
str(raw.df)

# Mapping regions -----------------------------------------------
if(INIT){
  region_code = read.csv('../Data/countries.csv', header = FALSE, stringsAsFactors=FALSE)
  region_code <- region_code %>% rename(REGION = `V1`,  COUNTRY = `V2`)
  region_code$REGION = tolower(region_code$REGION)
  U.regions$REGION <- as.character(U.regions$REGION)
  region_code_join <- left_join(U.regions, region_code, by = "REGION") %>% arrange(., REGION)
  region_code_join$COUNTRY[[which(region_code_join$REGION == "global")]] = "Global"
  write.csv(region_code_join, file='./region_code_reduced.csv', row.names=FALSE)
  write.csv(region_code_join, file='../Data/region_code_reduced.csv', row.names=FALSE)
} else {
  region.dt <- fread("./region_code_reduced.csv", header=TRUE, stringsAsFactors = F)
  region.df <- as.data.frame(region.dt)
}

# "RANKING"    "TRACK_NAME" "ARTIST"     "STREAMS"    "DATE"       "REGION"

# Monthly Analysis ---------------------------------------------------------------
#  When month is selected,
#   0) scroll bar for top100 songs by ARTIST, TRACK_NAME
#   1) top100 song countries
#   2) genre
#   3) steams counts by rank
# 

# All about selected song --------------------------------------------------------
#  When song and year is selected,
#   1) global map indicate !!
type = c('Maroon 5','CCSO')
date = as.Date(c("2017-01-01", "2017-01-31"), format=("%Y-%m-%d"))
input = data.frame(type, date)
#as.POSIXct(strptime("2017-01-31", "%Y-%m-%d"))
#b = as.Date("2016-01-10", format("%Y-%m-%d"))

a<- raw.df %>%
  filter(ARTIST == input$type[1],
         DATE > (input$date[1]) & DATE < (input$date[2]), REGION %in% c("ec", "fr", "ai"))
  #arrange(REGION)
rlist = c("ec", "fr", "ai")
b<- raw.df %>%
  filter(ARTIST == input$type[1],
         DATE > (input$date[1]) & DATE < (input$date[2]), REGION %in% rlist)

start = "2007-01-01"
end = "2017-01-31"
c<- unique(raw.df %>% filter(DATE > start & DATE < end) %>% select(ARTIST))

library(ggthemes)
# b <- raw.df %>% filter(ARTIST == input$type[1])
# c <- raw.df %>% filter(DATE > (input$date[1]) & DATE < (input$date[2]))
p <- ggplot(data = a, aes(x=DATE, y=RANKING))
p + geom_point(aes(color='white')) + stat_smooth(method = "lm", se = FALSE) + facet_grid("~REGION") +
  #theme_tufte() 
  #theme(plot.background = element_rect(fill = "black"))
  #theme_tufte() + theme(plot.background = element_rect(fill = "black"))
  #theme_wsj()+ scale_colour_wsj("colors6")
  #theme_calc()+ scale_colour_calc() +
  theme_hc()+ scale_colour_hc() + theme(plot.background = element_rect(fill = "black")) +
  #theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
  #                   panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))



g <- ggplot(data = raw.df[1:100,], aes(x=RANKING, y=STREAMS))
g + geom_point()
#g + geom_bar(stat='identity')

## ============================================================================== Summarize Ranking 
type = c('Maroon 5','CCSO')
date = as.Date(c("2017-01-01", "2017-01-31"), format=("%Y-%m-%d"))
input = data.frame(type, date)

A<- raw.df %>%
  filter(DATE >= (input$date[1]) & DATE <= (input$date[2]), REGION %in% c("ec", "fr", "us")) %>%
  group_by(TRACK_NAME, REGION, ARTIST) %>%
  summarise(sum_Stream = sum(STREAMS)) %>%
  #tally(REGION) %>%
  arrange(REGION, desc(sum_Stream))

B <- A %>%
  group_by(REGION) %>%
  top_n(3, sum_Stream) %>%
  arrange(REGION, desc(sum_Stream))
    
# reorder for ggplot2
#B$ARTIST <- factor(B$ARTIST, levels = B$ARTIST[order(B$sum_Stream)])
#B$ARTIST <- factor(B$ARTIST, levels = order(B$sum_Stream))
B$ARTIST  # notice the changed order of factor levels

#p <- ggplot(B, aes(x=sum_Stream, y=ARTIST))
p <- ggplot(B, aes(x=ARTIST, y=sum_Stream, label=sum_Stream ))
p + ggtitle(paste0("Rankings in each Countries (",input$date[1],"~",input$date[2],")" )) +
  geom_bar(aes(fill= REGION), stat = "identity", position = "dodge") +
  geom_text(size = 3, position = position_stack(vjust = 0.5)) +
  ylab("Total played number of Streams") +
  xlab("ARTIST or Track Name") +
  coord_flip() +
  theme_tufte()
  #theme(plot.background = element_rect(fill = "black"))
  #theme_tufte() + theme(plot.background = element_rect(fill = "black"))
  #theme_wsj() #+ scale_colour_wsj("colors6")
  #theme_calc()+ scale_colour_calc() 
  #theme_hc()+ scale_colour_hc()  
  #+ theme(plot.background = element_rect(fill = "black"))
  #theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
  #                   panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))
## ============================================================================== Summarize Ranking 







# ------- Lylics ----------------------------------------
raw.dt <- fread("../Data/data.csv", stringsAsFactors = F)
raw.df <- as.data.frame(raw.dt)

# Inspiration :
# By using the daily ranking of the 200 most listened songs in 53 countries by Spotify users,
# -	Track the current trend of music and forcast the next trend;
# -	Track the flow or influence of top ranked musics from one country to another, make some business model for commercial;
# -	Make a distribution map, and influence map by region, country, genre(music categories), and other information (such as season, weather, social event, etc);
# -	Check a possibility to develop a music recommendation service based on user-preference? Trend of music by states or region
#  ---------------------------------------------------------------
# Can you predict what is the rank position or the number of streams a song will have in the future?
# - How long does songs "resist" on the top 3, 5, 10, 20 ranking?
# - What are the signs of a song that gets into the top rank to stay?
# - Do continents share same top ranking artists or songs?
# - Are people listening to the very same top ranking songs on countries far away from each other?
# - How long time does a top ranking song takes to get into the ranking of neighbor countries?
# ==============================================================




head(A,1)
# ---------------------------------------------------------------------------------
# Change to time series for "STREAMS"
library(xts)
#sum(is.na(raw.df))
streams.ts <- xts(raw.df[,c(-1,-2,-3,-5,-6)], order.by=raw.df[,"DATE"])
#returns <- diff(streams.ts, arithmetic=FALSE ) - 1
row_sel = seq(1,900000,200)
#dygraph(streams.ts[row_sel])
#dygraph(streams.ts[row_sel]) %>% dyRangeSelector()
dygraph(streams.ts[row_sel]) %>%
  #dySeries("mdeaths", label = "Male") %>%
  dySeries("V1", label = "STREAMS") %>%
  #dyOptions(stackedGraph = TRUE) %>%
  dyRangeSelector(height = 20)
# ---------------------------------------------------------------------------------
lungDeaths <- cbind(mdeaths, fdeaths)
dygraph(lungDeaths)



#-------------------------------------------------------
#plotly test
library(plotly)

set.seed(100)
d <- diamonds[sample(nrow(diamonds), 1000), ]

p <- ggplot(data = d, aes(x = carat, y = price)) +
  geom_point(aes(text = paste("Clarity:", clarity)), size = 4) +
  geom_smooth(aes(colour = cut, fill = cut)) + facet_wrap(~ cut)

p <- ggplotly(p)

#tmp <- xts(raw.df[,c(-1,-2,-3,-5,-6)], order.by=as.Date(raw.df[,6], "%Y-%m-%d"))

#
#  daily returns
#

#
#  weekly open, high, low, close reports
#
to.weekly(tmp, name="Hero")

# library(readr)
# library(ggplot2)
# library(dplyr)
library(scales)
# library(forecast)
library(tseries) # install.packages("tseries")

theme_paper <- function() {
  theme_update(panel.background  = element_blank(),
               panel.grid.major  = element_blank(),
               axis.title        = element_text(size = 14, face = "bold"),
               axis.title.x      = element_text(vjust = 0.5),
               axis.title.y      = element_text(angle = 90, vjust = 1),
               axis.text         = element_text(size = 12),
               legend.title      = element_text(size = 17, face="bold"), 
               legend.text       = element_text(size = 17),
               strip.text        = element_text(size = 14),
               plot.margin       = unit(c(1, 1, 1, 1), "lines"),
               plot.title        = element_text(face="bold"))
}

theme_set(theme_bw())
theme_paper()

# Take one song that has data in one region only ("pl")
seven_nation_army = raw.df %>% 
  filter(ARTIST == "Shakira", 
         TRACK_NAME == 'Chantaje',
         REGION %in% c("gb", "ie", "pl"))

ggplot(seven_nation_army, aes(DATE, position)) +
  xlab("Date") +
  ylab("Rank position") +
  geom_point() +
  scale_y_reverse(breaks=seq(1, 200, 20)) +
  scale_x_date(breaks=date_breaks("month"),
               labels=date_format("%b")) +
  geom_smooth() +
  facet_wrap(~REGION, ncol=1)

seven_nation_army_in_pl = seven_nation_army %>%
  filter(REGION == "pl")

seven_nation_army_in_pl$streams_timeseries = ts(seven_nation_army_in_pl[, c('STREAMS')])
seven_nation_army_in_pl$streams_timeseries_week = ma(seven_nation_army_in_pl$streams_timeseries, order=7)
seven_nation_army_in_pl$streams_timeseries_month = ma(seven_nation_army_in_pl$streams_timeseries, order=30)




# # ref < pie chart > ===================================================
# 
# # Input the ad data
# ad = data.frame(
#   type = c("Poster", "Billboard", "Bus", "Digital"),
#   n = c(529, 356, 59, 81)
# )
# 
# # Bar plot
# library(ggplot2)
# ggplot(data = ad, aes(x = type, y = n, fill = type)) +
#   geom_bar(stat = "identity", show_guide = FALSE) +
#   theme_bw()
# 
# # Add addition columns to data, needed for donut plot.
# ad$fraction = ad$n / sum(ad$n)
# ad$ymax = cumsum(ad$fraction)
# ad$ymin = c(0, head(ad$ymax, n = -1))
# 
# # Donut plot
# ggplot(data = ad, aes(fill = type, ymax = ymax, ymin = ymin, xmax = 4, xmin = 3)) +
#   geom_rect(colour = "grey30", show_guide = FALSE) +
#   coord_polar(theta = "y") +
#   xlim(c(0, 4)) +
#   theme_bw() +
#   theme(panel.grid=element_blank()) +
#   theme(axis.text=element_blank()) +
#   theme(axis.ticks=element_blank()) +
#   geom_text(aes(x = 3.5, y = ((ymin+ymax)/2), label = type)) +
#   xlab("") +
#   ylab("")
# #=======================================================================

# Create a shareable link to your chart
# Set up API credentials: https://plot.ly/r/getting-started


# Sorting the x-axis in bargraphs using ggplot2 -----
# https://kohske.wordpress.com/2010/12/29/faq-how-to-order-the-factor-variables-in-ggplot2/
# sample data.
d <- data.frame(Team1=c("Cowboys", "Giants", "Eagles", "Redskins"), Win=c(20, 13, 9, 12))
# basic layer and options
p <- ggplot(d, aes(y=Win)) + opts(axis.text.x=theme_text(angle=90, hjust=1))
# default plot (left panel)
# the variables are alphabetically reordered.
p + geom_bar(aes(x=Team1), stat="identity") + opts(title="Default")
# re-order the levels in the order of appearance in the data.frame
d$Team2 <- factor(d$Team1, as.character(d$Team1))
# same as
# d$Team2 <- factor(d$Team1, c("Cowboys", "Giants", "Eagles", "Redskins"))
# plot on the re-ordered variables (Team2) (middle panel)
p + geom_bar(aes(x=Team2), data=d, stat="identity") + opts(title="Order by manual")
# re-order by variable Win
# the variables are re-orderd in the order of the win
d$Team3 <- reorder(d$Team1, d$Win)
# plot on the re-ordered variables (Team3) (right panel)
p + geom_bar(aes(x=Team3), data=d, stat="identity") + opts(title="Order by variable")
# we want to order by frequency, the most frequent bar coming first. This can be achieved in this way.
# ggplot(tips2, aes(x = reorder(day, -perc), y = perc)) + geom_bar(stat = "identity")


##-------------------------map mapping with region codes
#==============
# GET WORLD MAP
#==============
map.world <- map_data("world")
str(map.world)
head(map.world)

if(INIT){
  write.csv(map.world, file='./data_map.world.csv', row.names=FALSE)
}else{
  map.world.dt <- fread("./data_map.world.csv", stringsAsFactors = F)
  map.world <- as.data.frame(map.world.dt)
}

head(region.df,2)

head(map.world)

colnames(map.world) = c('long', 'lat', 'group', 'order', 'COUNTRY', 'subregion')

A = left_join(region.df, map.world, by= "COUNTRY")
B <- A %>% group_by(COUNTRY) %>%
  summarise(Latitude = mean(lat), Longitude = mean(long))

C = left_join(region.df, B, by = "COUNTRY")

write.csv(C, file='./region_map.csv', row.names=FALSE)
#==============================================================================
map.world %>% select('COUNTRY' == 'Bolivia')

# cf)
# Bolivia -62.37  -21.26

#Great Britain UK  -3.19, 54.23

# Hong Kong 114.11	22.37

# USA -80.18676758	27.27841759

# Global(NewYork) lng=-74.0059, lat=40.7128, popup="New York City"


# ------- Lylics ----------------------------------------
raw.dt <- fread("../Data/data.csv", stringsAsFactors = F)
raw.df <- as.data.frame(raw.dt)




head(A,1)
# ---------------------------------------------------------------------------------
# Change to time series for "STREAMS"
library(xts)
#sum(is.na(raw.df))
streams.ts <- xts(raw.df[,c(-1,-2,-3,-5,-6)], order.by=raw.df[,"DATE"])
#returns <- diff(streams.ts, arithmetic=FALSE ) - 1
row_sel = seq(1,900000,200)
#dygraph(streams.ts[row_sel])
#dygraph(streams.ts[row_sel]) %>% dyRangeSelector()
dygraph(streams.ts[row_sel]) %>%
  #dySeries("mdeaths", label = "Male") %>%
  dySeries("V1", label = "STREAMS") %>%
  #dyOptions(stackedGraph = TRUE) %>%
  dyRangeSelector(height = 20)
# ---------------------------------------------------------------------------------
lungDeaths <- cbind(mdeaths, fdeaths)
dygraph(lungDeaths)



#-------------------------------------------------------
#plotly test
library(plotly)

set.seed(100)
d <- diamonds[sample(nrow(diamonds), 1000), ]

p <- ggplot(data = d, aes(x = carat, y = price)) +
  geom_point(aes(text = paste("Clarity:", clarity)), size = 4) +
  geom_smooth(aes(colour = cut, fill = cut)) + facet_wrap(~ cut)

p <- ggplotly(p)

#tmp <- xts(raw.df[,c(-1,-2,-3,-5,-6)], order.by=as.Date(raw.df[,6], "%Y-%m-%d"))

#
#  daily returns
#

#
#  weekly open, high, low, close reports
#
to.weekly(tmp, name="Hero")

# library(readr)
# library(ggplot2)
# library(dplyr)
library(scales)
# library(forecast)
library(tseries) # install.packages("tseries")

theme_paper <- function() {
  theme_update(panel.background  = element_blank(),
               panel.grid.major  = element_blank(),
               axis.title        = element_text(size = 14, face = "bold"),
               axis.title.x      = element_text(vjust = 0.5),
               axis.title.y      = element_text(angle = 90, vjust = 1),
               axis.text         = element_text(size = 12),
               legend.title      = element_text(size = 17, face="bold"), 
               legend.text       = element_text(size = 17),
               strip.text        = element_text(size = 14),
               plot.margin       = unit(c(1, 1, 1, 1), "lines"),
               plot.title        = element_text(face="bold"))
}

theme_set(theme_bw())
theme_paper()

# Take one song that has data in one region only ("pl")
seven_nation_army = raw.df %>% 
  filter(ARTIST == "Shakira", 
         TRACK_NAME == 'Chantaje',
         REGION %in% c("gb", "ie", "pl"))

ggplot(seven_nation_army, aes(DATE, position)) +
  xlab("Date") +
  ylab("Rank position") +
  geom_point() +
  scale_y_reverse(breaks=seq(1, 200, 20)) +
  scale_x_date(breaks=date_breaks("month"),
               labels=date_format("%b")) +
  geom_smooth() +
  facet_wrap(~REGION, ncol=1)

seven_nation_army_in_pl = seven_nation_army %>%
  filter(REGION == "pl")

seven_nation_army_in_pl$streams_timeseries = ts(seven_nation_army_in_pl[, c('STREAMS')])
seven_nation_army_in_pl$streams_timeseries_week = ma(seven_nation_army_in_pl$streams_timeseries, order=7)
seven_nation_army_in_pl$streams_timeseries_month = ma(seven_nation_army_in_pl$streams_timeseries, order=30)
