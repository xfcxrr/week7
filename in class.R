# https://www.tidytextmining.com/tidytext.html

# 1.2 The unnest_tokens function

# # start a text list
text <- c("Because I could not stop for Death -",
          "He kindly stopped for me -",
          "The Carriage held but just Ourselves -",
          "and Immortality")
text
str(text)

# # read text list into tibble (df) with one line per string
library(dplyr)
text_df1 <- tibble(line = 1:4, text = text)
text_df1

# # tokenize text and read into tibble
install.packages(tidytext)
library(tidytext)
text_df2 <- unnest_tokens(text_df1, word, text)

# 1.3 Tidying the works of Jane Austen

library(janeaustenr)
library(dplyr)
library(stringr)

original_books <- austen_books() %>%
  group_by(book) %>%
  mutate(linenumber = row_number(),
         chapter = cumsum(str_detect(text, 
                                     regex("^chapter [\\divxlc]",
                                           ignore_case = TRUE)))) %>%
  ungroup()
original_books

# # tokenize all six books 

library(tidytext)
tidy_books <- original_books %>%
  unnest_tokens(word, text)
tidy_books

# # # Look at tokenizer package https://github.com/ropensci/tokenizers
# # # default tokenize is by word options include characters, n-grams, sentences, 
# # # lines, paragraphs, or separation around a regex pattern

# # remove "stop words"
data(stop_words)
tidy_books1 <- tidy_books %>%
  anti_join(stop_words)

# # count the number of words left after removing stop words
tidy_books1 %>%
  count(word, sort = TRUE)

# # tidy stores word counts in data frame, so plot outcomes
library(ggplot2)
tidy_books1 %>%
  count(word, sort = TRUE) %>%
  filter(n > 600) %>%                  # only words occurring > 600 words
  mutate(word = reorder(word, n)) %>%  # sort from high to low on n
  ggplot(aes(n, word)) +                   # run ggplot on data
  geom_col() +                         # notice ggplot uses + signs
  labs(y = NULL)

# # preprocessing is available in https://www.tidytextmining.com/usenet.html#pre-processing

# 1.4 The gutenbergr package                # see https://www.gutenberg.org/ as well

# 1.5 Word frequencies            # script to COMPARE between novels

# # Exercise: load Wells and Bronte from gutenbergr package

library(gutenbergr)

hgwells <- gutenberg_download(c(35, 36, 5230, 159))
bronte <- gutenberg_download(c(1260, 768, 969, 9182, 767))

tidy_hgwells <- hgwells %>%           # same as codes above
  unnest_tokens(word, text) %>%
  anti_join(stop_words)

tidy_bronte <- bronte %>%           # same as codes above
  unnest_tokens(word, text) %>%
  anti_join(stop_words)

tidy_hgwells %>%                      # same as codes above
  count(word, sort = TRUE)

tidy_bronte %>%                      # same as codes above
  count(word, sort = TRUE)

# # Bind the dataframes together into one dataframe

library(tidyr)

frequency <- bind_rows(mutate(tidy_bronte, author = "Brontë Sisters"),
                       mutate(tidy_hgwells, author = "H.G. Wells"), 
                       mutate(tidy_books1, author = "Jane Austen")) %>% 
  mutate(word = str_extract(word, "[a-z']+")) %>%
  count(author, word) %>%
  group_by(author) %>%
  mutate(proportion = n / sum(n)) %>% 
  select(-n) %>% 
  pivot_wider(names_from = author, values_from = proportion) %>%
  pivot_longer('Brontë Sisters':'H.G. Wells',
               names_to = "author", values_to = "proportion")

frequency

# # plot the comparisons
library(scales)

# expect a warning about rows with missing values being removed
ggplot(frequency, aes(x = proportion, y = `Jane Austen`, 
                      color = abs(`Jane Austen` - proportion))) +
  geom_abline(color = "gray40", lty = 2) +
  geom_jitter(alpha = 0.1, size = 2.5, width = 0.3, height = 0.3) +
  geom_text(aes(label = word), check_overlap = TRUE, vjust = 1.5) +
  scale_x_log10(labels = percent_format()) +
  scale_y_log10(labels = percent_format()) +
  scale_color_gradient(limits = c(0, 0.001), 
                       low = "darkslategray4", high = "gray75") +
  facet_wrap(~author, ncol = 2) +
  theme(legend.position="none") +
  labs(y = "Jane Austen", x = NULL)

# # correlate word uses

cor.test(data = frequency[frequency$author == "Brontë Sisters",],
         ~ proportion + `Jane Austen`)
cor.test(data = frequency[frequency$author == "H.G. Wells",], 
         ~ proportion + `Jane Austen`)


# 3 Analyzing word and document frequency: tf-idf

# # tf = term frequency
# # idf = inverse document frequency

# # The statistic tf-idf intended to measure how important a word is to 
# # a document in a collection (or corpus) of documents, for example, 
# # to one novel in a collection of novels or to one website in a 
# # collection of websites.

book_words <- austen_books() %>%
  unnest_tokens(word, text) %>%
  count(book, word, sort = TRUE)

total_words <- book_words %>% 
  group_by(book) %>% 
  summarize(total = sum(n))

book_words <- left_join(book_words, total_words)

book_words

# # proportion of times a word is used in each novel

ggplot(book_words, aes(n/total, fill = book)) +
  geom_histogram(show.legend = FALSE) +
  xlim(NA, 0.0009) +
  facet_wrap(~book, ncol = 2, scales = "free_y")

# # frequency of words inversely to proportional to rank
freq_by_rank <- book_words %>% 
  group_by(book) %>% 
  mutate(rank = row_number(), 
         `term frequency` = n/total) %>%
  ungroup()

freq_by_rank

freq_by_rank %>% 
  ggplot(aes(rank, `term frequency`, color = book)) + 
  geom_line(size = 1.1, alpha = 0.8, show.legend = FALSE) + 
  scale_x_log10() +
  scale_y_log10()

rank_subset <- freq_by_rank %>% 
  filter(rank < 500,
         rank > 10)

lm(log10(`term frequency`) ~ log10(rank), data = rank_subset)

freq_by_rank %>% 
  ggplot(aes(rank, `term frequency`, color = book)) + 
  geom_abline(intercept = -0.62, slope = -1.1, 
              color = "gray50", linetype = 2) +
  geom_line(size = 1.1, alpha = 0.8, show.legend = FALSE) + 
  scale_x_log10() +
  scale_y_log10()

# # 3.3 The bind_tf_idf() function

book_tf_idf <- book_words %>%
  bind_tf_idf(word, book, n)

book_tf_idf %>%
  select(-total) %>%
  arrange(desc(tf_idf))

# # tf-idf; it identifies words that are important 
# # to one document within a collection of documents.

library(forcats)

book_tf_idf %>%
  group_by(book) %>%
  slice_max(tf_idf, n = 15) %>%
  ungroup() %>%
  ggplot(aes(tf_idf, fct_reorder(word, tf_idf), fill = book)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~book, ncol = 2, scales = "free") +
  labs(x = "tf-idf", y = NULL)

# 4 Relationships between words: n-grams and correlations

library(dplyr)
library(tidytext)
library(janeaustenr)

# # creating ngrams = 2

austen_bigrams <- austen_books() %>%
  unnest_tokens(bigram, text, token = "ngrams", n = 2)

austen_bigrams

# # Counting and filtering

austen_bigrams %>%
  count(bigram, sort = TRUE)

# # separate word pairs for further processing

library(tidyr)

bigrams_separated <- austen_bigrams %>%
  separate(bigram, c("word1", "word2"), sep = " ")

bigrams_filtered <- bigrams_separated %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word)

# new bigram counts:
bigram_counts <- bigrams_filtered %>% 
  count(word1, word2, sort = TRUE)

bigram_counts

# find most common bigrams not containing stop words

bigrams_united <- bigrams_filtered %>%
  unite(bigram, word1, word2, sep = " ")

bigrams_united

# tri-grams?

austen_books() %>%
  unnest_tokens(trigram, text, token = "ngrams", n = 3) %>%
  separate(trigram, c("word1", "word2", "word3"), sep = " ") %>%
  filter(!word1 %in% stop_words$word,
         !word2 %in% stop_words$word,
         !word3 %in% stop_words$word) %>%
  count(word1, word2, word3, sort = TRUE)

# 4.1.2 Analyzing bigrams

# # most common street mentioned in book?

bigrams_filtered %>%
  filter(word2 == "street") %>%
  count(book, word1, sort = TRUE)

# # tf-idf of bigrams

bigram_tf_idf <- bigrams_united %>%
  count(book, bigram) %>%
  bind_tf_idf(bigram, book, n) %>%
  arrange(desc(tf_idf))
bigram_tf_idf

# # how do we build the figure 4.1? https://www.tidytextmining.com/tfidf.html#:~:text=2%2C%20scales%20%3D%20%22free_y%22)-,Figure,-3.1%3A%20Term%20frequency

# 4.1.4 Visualizing a network of bigrams with ggraph

library(igraph)

bigram_counts

# filter for only relatively common combinations

bigram_graph <- bigram_counts %>%
  filter(n > 20) %>%
  graph_from_data_frame()
bigram_graph

library(ggraph)
set.seed(2017)


# # graph bigrams occurred more than 20 times, where neither word was a stop word
ggraph(bigram_graph, layout = "fr") +
  geom_edge_link() +
  geom_node_point() +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1)

# # make it prettier

set.seed(2020)

a <- grid::arrow(type = "closed", length = unit(.15, "inches"))

ggraph(bigram_graph, layout = "fr") +
  geom_edge_link(aes(edge_alpha = n), show.legend = FALSE,
                 arrow = a, end_cap = circle(.07, 'inches')) +
  geom_node_point(color = "lightblue", size = 5) +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1) +
  theme_void()

# 4.2.1 Counting and correlating among sections

# # Work through the exercise for Pride and Prejudice
https://www.tidytextmining.com/ngrams.html#:~:text=sections%20of%20text).-,4.2.1%20Counting%20and%20correlating%20among%20sections,-Consider%20the%20book

library(widyr)

austen_section_words <- austen_books() %>%
  filter(book == "Pride & Prejudice") %>%
  mutate(section = row_number() %/% 10) %>%
  filter(section > 0) %>%
  unnest_tokens(word, text) %>%
  filter(!word %in% stop_words$word)
# count words co-occuring within sections
word_pairs <- austen_section_words %>%
  pairwise_count(word, section, sort = TRUE)
word_pairs %>%
  filter(item1 == "darcy")

# Review 4.2.2 Pairwise correlations
# https://www.tidytextmining.com/ngrams.html#:~:text=2%2C920%20more%20rows-,4.2.2%20Pairwise%20correlation,-Pairs%20like%20%E2%80%9CElizabeth

# we need to filter for at least relatively common words first
word_cors <- austen_section_words %>%
  group_by(word) %>%
  filter(n() >= 20) %>%
  pairwise_cor(word, section, sort = TRUE)

# # explore words most correlated with a word like “pounds”
word_cors %>%
  filter(item1 == "pounds")

# # find correlated words with several words of interest
word_cors %>%
  filter(item1 %in% c("elizabeth", "pounds", "married", "pride")) %>%
  group_by(item1) %>%
  slice_max(correlation, n = 6) %>%
  ungroup() %>%
  mutate(item2 = reorder(item2, correlation)) %>%
  ggplot(aes(item2, correlation)) +
  geom_bar(stat = "identity") +
  facet_wrap(~ item1, scales = "free") +
  coord_flip()

# # visualize correlations and clusters
set.seed(2016)
word_cors %>%
  filter(correlation > .15) %>%
  graph_from_data_frame() %>%
  ggraph(layout = "fr") +
  geom_edge_link(aes(edge_alpha = correlation), show.legend = FALSE) +
  geom_node_point(color = "lightblue", size = 5) +
  geom_node_text(aes(label = name), repel = TRUE) +
  theme_void()