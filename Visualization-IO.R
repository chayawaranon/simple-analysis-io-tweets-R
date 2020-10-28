#install.packages("lubridate")
#install.packages("readxl")
#install.packages("wordcloud2")

library(tidyverse)
library(lubridate)
library(readxl)
library(wordcloud2)

#read .xlsx file
data <- read_excel("thailand_IO_twitter.xlsx")
#change to date type
data$account_creation_date <- as.Date(data$account_creation_date, format = "%Y-%m-%d") 

#show the number of twitter account created by year
data %>% select(account_creation_date) %>% 
  arrange(account_creation_date) %>% 
  separate(account_creation_date, into = c("year","month","day") ,sep = "-") %>% 
  ggplot(aes(x = year)) + 
  geom_bar(fill = "#1da1f2") + 
  labs(title = "Number of twitter account created by year",
       subtitle = "What year IO created accounts most often?",
        x = "Year",
        y = "Count"
       ) +
  theme_minimal()

#--------------------------------------------------------------------------------#

#show the time that io often tweet
data$tweet_time_hour <- hour(hms(strftime(data$tweet_time, format = "%H:%M:%S")))
  data %>% 
    ggplot(aes(x = tweet_time_hour)) +
    geom_histogram(binwidth = 1, color = "white", fill = "#1da1f2") +
    labs(title = "The time with the most people tweet",
         subtitle = "How often IO tweet?",
         x = "Hour",
         y = "Count"
    ) +
    theme_minimal()
  
#--------------------------------------------------------------------------------#
  
#select only word in attribute hashtags
data$hashtags <- gsub("\\[|'|'|,|\\]","",data$hashtags)

#count word in attribute hashtags
hashtags_count <- strsplit(data$hashtags, " ", fixed = T)
hashtags_count <- unlist(hashtags_count)
hashtags_count <- as.data.frame(table(hashtags_count))

tt.colors <- c("#fefefe","#f4f2a8","#030303", "#446b36")
tt.backgrond <- "#00ccff"

#make word cloud
wordcloud2(hashtags_count,
           size = 1.2,
           minSize = 5,
           rotateRatio = 0,
           color = rep_len(tt.colors,nrow(hashtags_count)),
           backgroundColor = tt.backgrond)

wordcloud2(hashtags_count,
           size = 2.5,
           minSize = 5,
           rotateRatio = 0,
           color = rep_len(tt.colors,nrow(hashtags_count)),
           backgroundColor = tt.backgrond)




    
