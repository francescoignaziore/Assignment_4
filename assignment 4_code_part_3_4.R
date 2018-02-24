library(tidyverse)
library(lubridate)
library(reshape2)
library(stringr)
library(plyr)
library(dplyr)

#reading and setting up table4
table4 <- read.csv("raw.csv")
table4 <- table4[-1]
colnames(table4) <- c( "religion" ,  "<$10k",  "$10-20k" , "$20-30k", "$30-40k"  ,  "$40-50k",    "$50-75k" ,  "$75-100k",          
                        "$100-150k",  ">150k"  ,   "Don't know/refused")


#Tidying up table4, turning it into table 6
table6 <- table4 %>% gather(key = "income", value = "freq", - religion) %>%
  arrange(religion)




#reading and setting up table7

raw <- read.csv("billboard.csv")
raw <- raw[, c("year", "artist.inverted", "track", "time", "date.entered", "x1st.week", "x2nd.week", "x3rd.week", "x4th.week", "x5th.week", "x6th.week", "x7th.week", "x8th.week", "x9th.week", "x10th.week", "x11th.week", "x12th.week", "x13th.week", "x14th.week", "x15th.week", "x16th.week", "x17th.week", "x18th.week", "x19th.week", "x20th.week", "x21st.week", "x22nd.week", "x23rd.week", "x24th.week", "x25th.week", "x26th.week", "x27th.week", "x28th.week", "x29th.week", "x30th.week", "x31st.week", "x32nd.week", "x33rd.week", "x34th.week", "x35th.week", "x36th.week", "x37th.week", "x38th.week", "x39th.week", "x40th.week", "x41st.week", "x42nd.week", "x43rd.week", "x44th.week", "x45th.week", "x46th.week", "x47th.week", "x48th.week", "x49th.week", "x50th.week", "x51st.week", "x52nd.week", "x53rd.week", "x54th.week", "x55th.week", "x56th.week", "x57th.week", "x58th.week", "x59th.week", "x60th.week", "x61st.week", "x62nd.week", "x63rd.week", "x64th.week", "x65th.week", "x66th.week", "x67th.week", "x68th.week", "x69th.week", "x70th.week", "x71st.week", "x72nd.week", "x73rd.week", "x74th.week", "x75th.week", "x76th.week")]
names(raw)[2] <- "artist"

raw$artist <- iconv(raw$artist, "MAC", "ASCII//translit")
raw$track <- str_replace(raw$track, " \\(.*?\\)", "")
names(raw)[-(1:5)] <- str_c("wk", 1:76)
raw <- arrange(raw, year, artist, track)

long_name <- nchar(raw$track) > 20
raw$track[long_name] <- paste0(substr(raw$track[long_name], 0, 20), "...")
table7 <- raw



#tidying up table7 and turning it into table8
table7a <- table7 %>% gather(key = "week", value = "rank", -year, -artist, -track, -time, -date.entered, na.rm = TRUE )
table7c <- dplyr::rename(table7a, date = date.entered)
table7d <- table7c %>%   separate(week, c("new", "week"), sep = 2)
table7e <- table7d %>% select(- new)
table7f <- mutate(table7e, date = as.Date(date) + days(week) * 7 - days(7))
table8 <- table7f %>% arrange(artist, track)




View(table4)
View(table6)

View(table7)
View(table8)



