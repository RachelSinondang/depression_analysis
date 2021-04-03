# import libs
library(shiny)
library(shinydashboard)
library(tidyverse)
library(plotly)
library(scales)
library(glue)
library(DT)
library(tidyverse) 

# Text analysis
library(textclean)
library(tidytext)
library(tm)

library(caret)
library(e1071)
library(rpart)
library(RTextTools)
library(tm)
library(DMwR)
library(plotly)


# import dataset

suicide_rate <- read_csv("who_suicide_statistics.csv")

suicide_rate <- suicide_rate %>%
  select(-suicides_no)

suicide_rate %>%
  is.na() %>%
  colSums()

suicide_rate_edit <- suicide_rate %>%
  drop_na(population)

suicide_rate_edit <- suicide_rate_edit %>%
  mutate(country = as.factor(country),
         sex = as.factor(sex),
         age = as.factor(age))

jumlah_suicide <- suicide_rate_edit %>%
  filter(year == 2010) %>%
  group_by(age, year) %>%
  summarise(jumlah = sum(population))

jumlah_suicide_negara <-suicide_rate_edit %>%
  filter(country == "Albania", year == 1985) %>%
  group_by(country, age, year) %>%
  summarise(banyaknya = sum(population))


theme_algoritma <- theme(legend.key = element_rect(fill="black"),
                         legend.background = element_rect(color="white", fill="#263238"),
                         plot.subtitle = element_text(size=6, color="white"),
                         panel.background = element_rect(fill="#dddddd"),
                         panel.border = element_rect(fill=NA),
                         panel.grid.minor.x = element_blank(),
                         panel.grid.major.x = element_blank(),
                         panel.grid.major.y = element_line(color="darkgrey", linetype=2),
                         panel.grid.minor.y = element_blank(),
                         plot.background = element_rect(fill="#263238"),
                         text = element_text(color="white"),
                         axis.text = element_text(color="white"))


depresi <- read.csv("depresi.csv")

depresi.corpus <- VCorpus(VectorSource(depresi$text))

depresi.corpus <- tm_map(depresi.corpus, content_transformer(textstem::lemmatize_words))

depresi.siap <- bind_cols(depresi.corpus %>% sapply(as.character) %>%
                            as.data.frame(stringsAsFactors = FALSE), depresi[,2]) %>%
  `colnames<-`(c("text", "label"))

library(rsample)
RNGkind(sample.kind = "Rounding")
set.seed(100)

split_a <- initial_split(depresi.siap, prop =  0.8, strata = "label")
train <- training(split_a)

library(caret)

train_up <- upSample(x = train %>% select(-label), 
                     y = as.factor(train[,"label"]), 
                     yname = "label") 

depresi.train.dtm <- DocumentTermMatrix(VCorpus(VectorSource(train_up$text)))

depresi.train.dtm <- removeSparseTerms(depresi.train.dtm, 0.995)

train_rf <- as.data.frame(as.matrix(depresi.train.dtm))


depression_forest <- readRDS("model_depresi_forest.RDS")