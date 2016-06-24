library("magrittr")
library("dplyr")
library("stringr")
library("tm")
library("SnowballC")
library("RWeka")
library("ggplot2")
library("wordcloud")
library("showtext")


# =============================================================================
# loading data

# event data with abstracts
load(file = "event_data.RData")

# table of stopwords from http://dev.mysql.com/doc/refman/5.5/en/fulltext-stopwords.html
custom_stopwords <- readLines(con = "stopwords.csv") %>% 
  strsplit(split = ";") %>% 
  unlist() %>% 
  str_trim()




# =============================================================================
# corpus

# only consider 'contributed talks' 
ggplot(data = event_data) + 
  aes(x = type) + 
  geom_bar()

talk_data <- event_data %>% filter(type == "contributed talk")

# build corpus
corpus <- talk_data %>% 
  extract2("abstract") %>% 
  VectorSource() %>% 
  Corpus()


# corpus processing
punct_transformer <- content_transformer(function(x) str_replace_all(x, "[[:punct:]]+", ""))

corpus_cleaned <- corpus %>% 
  tm_map(content_transformer(tolower)) %>%
  tm_map(punct_transformer) %>% 
  tm_map(removeNumbers) %>% 
  tm_map(removeWords, words = c(stopwords(kind = "en"), custom_stopwords)) 



# =============================================================================
# term mapping

# building a data frame that maps a stemmed term to its non-stemmed or 'raw' form 
mapping_dtm <- corpus_cleaned %>% DocumentTermMatrix(control = list(wordLengths = c(1, Inf)))

term_mapping <- data.frame(
  raw = mapping_dtm %>% Terms(),
  stemmed = mapping_dtm %>% Terms() %>% stemDocument(),
  nraw = mapping_dtm %>% as.matrix() %>% colSums(),
  stringsAsFactors = FALSE
)



# =============================================================================
# document term matrix

# 1) dtm with all terms, 2) dtm without sparse terms (sparsity > .90), 
# 3) dtm with tf-idf weighted term frequencies and no sparse terms

dtm <- corpus_cleaned %>% 
  tm_map(stemDocument) %>% 
  DocumentTermMatrix(control = list(wordLengths = c(1, Inf)))

dtm_proc <- dtm %>% removeSparseTerms(sparse = .90)
dtm_proc_tfidf <- dtm %>% 
  weightTfIdf(normalize = TRUE) %>% 
  removeSparseTerms(sparse = .90)



# =============================================================================
# Stem completion

# Note: stemCompletion() from the tm package also performs stem completion and 
# furthermore does not require an external mapping table like the one we build
# before. However, in this case due to the way the porter stemming algorithm works 
# tm::stemCompletion() fails to complete certain terms.

evil_terms <- dtm_proc %>% Terms()
comp_terms <- character()

for(i in 1:length(evil_terms)){
  temp_evil_term <- evil_terms[i]
  
  candidates <- term_mapping %>% filter(stemmed == temp_evil_term)
  # use the most prevalent candidate
  comp_terms[i] <- candidates$raw[candidates$nraw %>% which.max()]
}

cbind(evil_terms, comp_terms)

dimnames(dtm_proc)$Terms <- comp_terms
dimnames(dtm_proc_tfidf)$Terms <- comp_terms




# =============================================================================
# Calculate word frequencies (unweighted and tf-idf weighted)

word_freq <- data.frame(
  term = dtm_proc %>% Terms(),
  freq = dtm_proc %>% as.matrix() %>% colSums(),
  stringsAsFactors = FALSE
)

word_freq <- word_freq %>% arrange(freq %>% desc)
word_freq


word_freq_tfidf <- data.frame(
  term = dtm_proc_tfidf %>% Terms(),
  freq = dtm_proc_tfidf %>% as.matrix() %>% colSums(),
  stringsAsFactors = FALSE
)

word_freq_tfidf <- word_freq_tfidf %>% arrange(freq %>% desc)
word_freq_tfidf

ggplot(data = word_freq) + aes(x = freq) + geom_histogram(binwidth = 10)
ggplot(data = word_freq_tfidf) + aes(x = freq) + geom_histogram(binwidth = .10)


# Compare term frequencies with tf-idf weighted term frequencies.
word_freq_comp <- inner_join(x = word_freq, y = word_freq_tfidf, by = "term")

ggplot(data = word_freq_comp) +
  aes(x = freq.x, y = freq.y, label = term) +
  geom_text() +
  geom_smooth(color = "red", alpha = .30) +
  theme_bw() +
  labs(
    x = "term frequency",
    y = "tf-idf weighted term frequency",
    title = "term frequencies vs. tf-idf weighted term frequencies"
  )


# example: term correaltions
findAssocs(
  x = dtm_proc,
  terms = "data",
  corlimit = .10
)

findAssocs(
  x = dtm_proc,
  terms = "r",
  corlimit = .10
)

findAssocs(
  x = dtm_proc,
  terms = "machine",
  corlimit = .10
)



# =============================================================================
# Wordcloud

# we use the showtext package to extend the number of availabe fonts for plotting 
showtext.auto()
font.add(family = "Britanic", regular = "BRITANIC.TTF")


# only plot the top n terms
n <- 100
word_freq_top100 <- word_freq[1:n, ]
word_freq_tfidf_top100 <- word_freq_tfidf[1:n, ]


# use windows() if the fonts are not correctly displayed on your on-screen
# graphic device.
#windows()
png(filename = "useR2016_wordcloud.png", width = 600, height = 600)
op <- par(no.readonly = TRUE)
par(bg = "black", family= "Britanic")
set.seed(65795)
wordcloud(
  words = word_freq_top100$term, 
  freq  = word_freq_top100$freq,
  min.freq = 1,
  random.order = TRUE, 
  random.color = FALSE, 
  colors = brewer.pal(n = 9, name = "Set1"),
  scale = c(8, .45)
)
par(op)
dev.off()


#windows()
png(filename = "useR2016_wordcloud_weighted.png", width = 600, height = 600)
op <- par(no.readonly = TRUE)
par(bg = "black", family= "Britanic")
set.seed(56596)
wordcloud(
  words = word_freq_tfidf_top100$term, 
  freq  = word_freq_tfidf_top100$freq,
  min.freq = 0,
  random.order = TRUE, 
  random.color = FALSE, 
  colors = brewer.pal(n = 9, name = "Set1"),
  scale = c(3, .50)
)
par(op)
dev.off()



# =============================================================================
# bar charts

#windows()
png(filename = "user2016_top10_terms.png", width = 1000, height = 600)
par(mai = c(1, 1.4, 1, .42))

barplot(
  height = word_freq_top100$freq[1:10], 
  names.arg = word_freq_top100$term[1:10],
  border = "transparent",
  ylim = c(0, 400),
  las = 1,
  main = "Top 10 most frequent terms from useR 2016 abstracts",
  ylab = "term frequency in corpus (tf-idf weighted)"
)

abline(h = seq(0, 400, 100), col = "white")
dev.off()


#windows()
png(filename = "user2016_top10_terms_tfidf.png", width = 1000, height = 600)
par(mai = c(1, 1.4, 1, .42))

barplot(
  height = word_freq_tfidf_top100$freq[1:10], 
  names.arg = word_freq_tfidf_top100$term[1:10],
  border = "transparent",
  ylim = c(0, 3),
  las = 1,
  main = "Top 10 most frequent terms from useR 2016 abstracts (tf-idf weighted)",
  ylab = "term frequency in corpus (tf-idf weighted)"
)

abline(h = seq(0, 3, .5), col = "white")
dev.off()


