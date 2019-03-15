data <- read.csv("Seasons_Stats.csv")
library(tidyverse)

select(data, Player, Age)
select(data,Year:MP)
new_df <- select(data,1:5)
head(new_df)

new_df <- select(data,Year:MP,-GS)
head(new_df)

new_df <- select(data,Age,Player)
head(new_df)

new_df <- select(data,Player,Year,Age,everything())
head(new_df)

new_df <- select(data, starts_with("FG"))
head(new_df)

new_df <- select(data, ends_with("."))
head(new_df)

new_df <- select(data, contains("RB"))
head(new_df)

new_df <- rename(data, index = X)
head(new_df)

write.csv(new_df, "new_data.csv")


new_df <- filter(data, Age > 25)
head(new_df)

new_df <- filter(data, Age >25, Year > 2010)
head(new_df)

new_df <- filter(data, Age >25 & Year > 2010)
head(new_df)

new_df <- filter(data, Age >25 | Year > 2010)
head(new_df)

new_df <- filter(data, !is.na(GS))
head(new_df)


new_df <- filter(data, Pos == "G")
head(new_df)

new_df <- filter(data, grepl("G",Pos))
head(new_df)

new_df <- arrange(data, Age)
head(new_df)

new_df <- arrange(data, desc(Age))
head(new_df)

new_df <- arrange(data, Age, desc(G))
head(new_df)

new_df <- mutate(data, PPG = PTS/G)
new_df

new_df <- mutate(data, new_column = 1)
head(new_df)


new_df <- transmute(data, PPG = PTS/G)
head(new_df)

summarise(data, meanPTS = mean(PTS, na.rm = T))

new_df <- group_by(data, Pos,Tm)
summarise(new_df, 
          meanPTS = mean(PTS,na.rm = T),
          count = n())


df1 <- select(data, Player,Age,Pos,PTS,G)
df2 <- filter(df1, !is.na(G) & !is.na(PTS))
df3 <- mutate(df2, PPG = PTS / G)
df4 <- group_by(df3,Pos)
summarise(df4,
          meanPTS = mean(PPG,na.rm = T),
          count = n())

summarise(group_by(mutate(filter(select(data, Player,Age,Pos,PTS,G), !is.na(G) & !is.na(PTS)), PPG = PTS / G),Pos),
          meanPTS = mean(PPG,na.rm = T),
          count = n())

data %>%
    select(Player,Age,Pos,PTS,G) %>%
    filter(!is.na(G) & !is.na(PTS)) %>%
    mutate(PPG = PTS / G) %>%
    group_by(Pos) %>%
    summarise(meanPTS = mean(PPG,na.rm = T),
              count = n())
    
data %>%
    select(Player,Pos,Tm,G,MP,Age) %>%
    filter(Age<20 & G > 50) %>%
    mutate(MPG = MP / G) %>%
    filter(Pos == "C") %>%
    group_by(Player) %>%
    summarise(meanMPG = mean(MPG, na.rm = T))

