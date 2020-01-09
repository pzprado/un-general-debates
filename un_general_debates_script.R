######################################################
## Unsupervised Machine Learning - LDA application
## to the United Nations General Debates dataset
######################################################

#--------------------------------------------
# Tidy data
#--------------------------------------------
if (!require(tidyverse))
  install.packages("tidyverse", repos = "http://cran.us.r-project.org")

if (!require(tidytext))
  install.packages("tidytext", repos = "http://cran.us.r-project.org")

if (!require(textdata))
  install.packages("textdata", repos = "http://cran.us.r-project.org")

if (!require(textstem))
  install.packages("textstem", repos = "http://cran.us.r-project.org")

if (!require(readtext))
  install.packages("readtext", repos = "http://cran.us.r-project.org")

if (!require(countrycode))
  install.packages("countrycode", repos = "http://cran.us.r-project.org")

if (!require(grid))
  install.packages("grid", repos = "http://cran.us.r-project.org")

if (!require(kableExtra))
  install.packages("kableExtra", repos = "http://cran.us.r-project.org")

if (!require(png))
  install.packages("png", repos = "http://cran.us.r-project.org")

#--------------------------------------------
# Machine learning
#--------------------------------------------

if (!require(quanteda))
  install.packages("quanteda", repos = "http://cran.us.r-project.org")

if (!require(topicmodels))
  install.packages("topicmodels", repos = "http://cran.us.r-project.org")

#--------------------------------------------
# Visualisation
#--------------------------------------------
if (!require(circlize))
  install.packages("circlize", repos = "http://cran.us.r-project.org")

if (!require(ggplot2))
  install.packages("ggplot2", repos = "http://cran.us.r-project.org")

if (!require(gridExtra))
  install.packages("gridExtra", repos = "http://cran.us.r-project.org")

#--------------------------------------------
# Generate the dataset
#--------------------------------------------

# Note: this file is approx 58MB and may take a few minutes

dl <- tempfile() ## download the zip file to a temporary location
download.file(
  "https://github.com/pzprado/un-general-debates/raw/master/UNGDC+1970-2018.zip",
  dl
)

unzip(dl, exdir = paste0(getwd(), '/UN_data'), overwrite = TRUE) ## unzip the file
undir <- paste0(getwd(), '/UN_data') ## point to the directory of the files

un_files <- ## generate the dataset
  readtext(
    paste0(undir, "/Converted sessions/*/*.txt"),
    docvarsfrom = "filenames",
    docvarnames = c("country", "session", "year"),
    dvsep = "_",
    encoding = "UTF-8"
  )

un_data <- ## consolidate the master dataset with country names and continent
  un_files %>%
  mutate(
    country_name = countrycode(
      sourcevar = un_files$country,
      origin = 'iso3c',
      destination = 'iso.name.en'
    ),
    continent = countrycode(
      sourcevar = un_files$country,
      origin = 'iso3c',
      destination = 'continent'
    )
  ) %>%
  select(doc_id, text, country, country_name, continent, session, year)

## fixing NA due to no match for YDYE, CSK, YUG, DDR and EU in the countrycode package

un_data[un_data$country == 'YDYE', "country_name"] <- "Yemen, Democratic"
un_data[un_data$country == 'YDYE', "continent"] <- "Asia"
un_data[un_data$country == 'CSK', "country_name"] <- "Czechoslovakia"
un_data[un_data$country == 'CSK', "continent"] <- "Europe"
un_data[un_data$country == 'YUG', "country_name"] <- "Yugoslavia"
un_data[un_data$country == 'YUG', "continent"] <- "Europe"
un_data[un_data$country == 'DDR', "country_name"] <- "East Germany"
un_data[un_data$country == 'DDR', "continent"] <- "Europe"
un_data[un_data$country == 'EU', "country_name"] <- "European Union"
un_data[un_data$country == 'EU', "continent"] <- "Europe"

un_data$doc_id <- sub(".txt", "", un_data$doc_id) ## fix the doc_id pattern

rm(dl, undir, un_files) ## Remove files - keep the environment clean


#--------------------------------------------
# Data exploration
#--------------------------------------------

class(un_data)
glimpse(un_data)

un_data.stats <- summary(un_data)
un_data.stats

un_data %>% ## Summary of unique values
  summarise(
    documents = n_distinct(doc_id),
    years = n_distinct(year),
    countries = n_distinct(country),
    continents = n_distinct(continent)
  )

un_data %>% ## Summary of unique values for 2018
  filter(year == '2018') %>%
  group_by(year) %>%
  summarise(
    documents = n_distinct(doc_id),
    countries = n_distinct(country),
    continents = n_distinct(continent)
  )

un_corpus <- corpus(un_data, text_field = "text")
class(un_corpus)

summary(un_corpus, n = 5)

un_corpus.stats <- as.data.frame(summary(un_corpus, n = 8093))

tokens_plot <- un_corpus.stats %>% 
  group_by(year, continent) %>% 
  summarize(tokens = mean(Tokens)) %>% 
  ggplot(aes(year, tokens, color = continent)) + 
  geom_smooth(method = 'loess', fill = 'NA') +
  theme(legend.position = "top") +
  ggtitle('Average speech tokens per continent') +
  labs(color = NULL)

sentence_plot <- un_corpus.stats %>% group_by(year, continent) %>% 
  summarize(sentences = mean(Sentences)) %>% 
  ggplot(aes(year, sentences, color = continent)) + 
  geom_smooth(method = 'loess', fill = 'NA') + 
  theme(legend.position = "none") +
  ggtitle('Average speech sentences per continent')


speeches_plot <- un_corpus.stats %>% 
  group_by(year) %>% 
  summarize(countries = n_distinct(country)) %>% 
  ggplot(aes(year, countries)) + 
  geom_smooth(method = 'loess', fill = 'NA') +
  ggtitle('Speeches per year')

grid.arrange(tokens_plot, sentence_plot, speeches_plot, nrow = 3)


#--------------------------------------------
# Lemmatisation
#--------------------------------------------

un_corpus_lemma <- # create a new corpus to preserve the original corpus
  corpus(un_data, text_field = "text") 

start <- Sys.time()

un_corpus_lemma$documents$texts <-
  lemmatize_strings(un_corpus_lemma$documents$texts, dictionary = lexicon::hash_lemmas)

Sys.time()-start

#--------------------------------------------
# Tokenisation
#--------------------------------------------

un_tokens <-
  quanteda::tokens(
    un_corpus_lemma
  )

top_terms.d <-
  un_tokens %>% dfm() %>% tidy() %>% group_by(term) %>% 
  summarize(count = n()) %>% arrange(desc(count)) %>% slice(1:10) 

bot_terms.d <-
  un_tokens %>% dfm() %>% tidy() %>% group_by(term) %>% 
  summarize(count = n()) %>% arrange(count) %>% slice(1:10)

top_terms.d 
bot_terms.d

# Word list of words to be discarted 

un_words <- c("unite", "nation", "country", "international", 
              "united", "nations", "countries", "will", "world")

#--------------------------------------------
# Cleaning
#--------------------------------------------

un_tokens <-
  quanteda::tokens(
    un_corpus_lemma,
    remove_numbers = TRUE,
    remove_twitter = TRUE,
    remove_punct = TRUE,
    remove_symbols = TRUE,
    remove_url = TRUE,
    remove_hyphens = TRUE,
    include_docvars = TRUE
  )

un_dfm <- 
  dfm(un_tokens,
      tolower = TRUE,
      stem = FALSE,
      remove = c(stopwords("english"),
                 un_words
      )
  )

top_terms.cl <-
  un_dfm %>% tidy() %>% group_by(term) %>% 
  summarize(count = n()) %>% arrange(desc(count)) %>% slice(1:10) 

bot_terms.cl <-
  un_dfm %>% tidy() %>% group_by(term) %>% 
  summarize(count = n()) %>% arrange(count) %>% slice(1:10)

top_terms.cl
bot_terms.cl

un_dfm <- dfm_trim(un_dfm,
                   min_docfreq = 0.002,
                   docfreq_type = "prop"
) 

top_terms.tr <-
  un_dfm %>% tidy() %>% group_by(term) %>% 
  summarize(count = n()) %>% arrange(desc(count)) %>% slice(1:10) 

bot_terms.tr <-
  un_dfm %>% tidy() %>% group_by(term) %>% 
  summarize(count = n()) %>% arrange(count) %>% slice(1:10)

top_terms.tr
bot_terms.tr

un_dfm %>% textplot_wordcloud()

#--------------------------------------------
# LDA
#--------------------------------------------

un_dtm <- convert(un_dfm, to = "topicmodels")
k <- 20 #number of topics
seed = 123 #necessary for reproducibility

start <- Sys.time() #calculating the total runtime
lda <- LDA(un_dtm, k = k, method = "GIBBS", control = list(seed = seed))
Sys.time()-start #shows total runtime as time difference between end and start of LDA processing

as.data.frame(terms(lda, 15)) %>% 
  select(1:10) %>% 
  kable(format = "latex") %>% 
  kable_styling(latex_options = c("striped", "scale_down"))

as.data.frame(terms(lda, 15)) %>% 
  select(11:20) %>% 
  kable(format = "latex") %>% 
  kable_styling(latex_options = c("striped", "scale_down"))

#--------------------------------------------
# Visualisation
#--------------------------------------------

study_topics <- c(1, 2, 3, 4, 6, 8, 10, 12, 13, 14, 16, 17
)

topic_names <- c('Peace in Africa', 
                 'War & Terrorism', 
                 'Korea', 
                 'Israel & Palestine', 
                 'Peace in Iraq',
                 'Security Council', 
                 'Human Rights', 
                 'European Conflicts', 
                 'Nuclear Weapons',
                 'South Africa & Namibia',
                 'Climate Change', 
                 'Economic Development')

df_topics <- data.frame(name = topic_names, topic = study_topics)

#Extract gamma, join metadata, create average gamma
year_topic_relationship <- tidy(lda, matrix = "gamma") %>%
  inner_join(un_data, by = c("document" = "doc_id")) %>%
  select(year, topic, gamma) %>%
  group_by(year, topic) %>%
  summarize(gamma = mean(gamma))

year_topic_relationship %>% 
  filter(topic %in% study_topics) %>%
  inner_join(df_topics, by = "topic") %>%
  ggplot(aes(x = year, y = gamma, colour = factor(name))) + 
  geom_line(size = 1.5) + 
  scale_color_brewer(palette = "Paired") +
  labs(colour = NULL)

topics.5 <- c(6, 12, 14, 16, 17)

#Extract gamma, join metadata, create average gamma
continent_topic_relationship <- tidy(lda, matrix = "gamma") %>%
  filter(topic %in% topics.5) %>%
  inner_join(un_data, by = c("document" = "doc_id")) %>%
  select(continent, topic, gamma) %>%
  group_by(continent, topic) %>%
  summarize(gamma = mean(gamma))

grid.col = c("Americas" = "blue", "Asia" = "yellow", "Europe" = "red",
             "Africa" = "green", "Oceania" = "brown", "6" = "grey", "12" = "grey",
             "14" = "grey", "16" = "grey", "17" = "grey")

circos.clear()

circos.par(gap.after = c(rep(5, length(unique(continent_topic_relationship[[1]])) - 1), 15,
                         rep(5, length(unique(continent_topic_relationship[[2]])) - 1), 15))

# ChordDiagram (continent - topic all time)

chordDiagram(continent_topic_relationship, grid.col = grid.col)
title("All Time Topic and Continent Relationship")
grid.text("  6 Peace & Iraq
12 European Conflicts
14 South Africa & Namibia
16 Climate Change
17 Economic Development", 
          x=unit(0.05, "npc"), y=unit(0.8, "npc"), just="left",
          gp=gpar(fontsize=8))

continent_topic_relationship <- tidy(lda, matrix = "gamma") %>%
  inner_join(un_data, by = c("document" = "doc_id")) %>%
  filter(topic %in% topics.5, year == "1990") %>%
  select(continent, topic, gamma) %>%
  group_by(continent, topic) %>%
  summarize(gamma = mean(gamma))

circos.clear()

circos.par(gap.after = c(rep(5, length(unique(continent_topic_relationship[[1]])) - 1), 15,
                         rep(5, length(unique(continent_topic_relationship[[2]])) - 1), 15))

# ChordDiagram (continent - topic 1990)

chordDiagram(continent_topic_relationship,  grid.col = grid.col)
title("Topic and Continent Relationship in 1990")

continent_topic_relationship <- tidy(lda, matrix = "gamma") %>%
  inner_join(un_data, by = c("document" = "doc_id")) %>%
  filter(topic %in% topics.5, year == "2000") %>%
  select(continent, topic, gamma) %>%
  group_by(continent, topic) %>%
  summarize(gamma = mean(gamma))

circos.clear()

circos.par(gap.after = c(rep(5, length(unique(continent_topic_relationship[[1]])) - 1), 15,
                         rep(5, length(unique(continent_topic_relationship[[2]])) - 1), 15))

# ChordDiagram (continent - topic 2000)

chordDiagram(continent_topic_relationship,  grid.col = grid.col)
title("Topic and Continent Relationship in 2000")

continent_topic_relationship <- tidy(lda, matrix = "gamma") %>%
  inner_join(un_data, by = c("document" = "doc_id")) %>%
  filter(topic %in% topics.5, year == "2018") %>%
  select(continent, topic, gamma) %>%
  group_by(continent, topic) %>%
  summarize(gamma = mean(gamma))

circos.clear()

circos.par(gap.after = c(rep(5, length(unique(continent_topic_relationship[[1]])) - 1), 15,
                         rep(5, length(unique(continent_topic_relationship[[2]])) - 1), 15))

# ChordDiagram (continent - topic 2018)

chordDiagram(continent_topic_relationship,  grid.col = grid.col)
title("Topic and Continent Relationship in 2018")





