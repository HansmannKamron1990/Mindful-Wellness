#1 - Load Libraries
library (tidyverse)
library (tidytext)
library (dplyr)

#2 - Read in data
mindful_data <- read.csv("mindful_wellness.csv")

#3 - Explore data
head(mindful_data)

#4 - Create a tidy dataset
tidy_mindful_data <- mindful_data %>%
    gather(key = key, value = value, -Wellness.Category)

#5 - Create sentiment scores
sentiment_scores <-
    tidy_mindful_data %>%
        inner_join(get_sentiments("bing")) %>%
        group_by(Wellness.Category) %>%
        summarise(sentiment_score = mean(score))

#6 - Visualize sentiment scores
ggplot(data = sentiment_scores, aes(x = Wellness.Category, y = sentiment_score)) +
    geom_bar(stat = "identity") +
    xlab("Wellness Category") +
    ylab("Sentiment Score") +
    ggtitle("Sentiment Analysis of Wellness Categories")

#7 - Perform topic modeling
mindful_tokens <-
    mindful_data %>%
        unnest_tokens(word, value)

mindful_lda <-
    mindful_tokens %>%
        group_by(Wellness.Category) %>%
        nest() %>%
        mutate(lda_model = map(data, ~lda_model(word, 3)))

#8 - Print topics
mindful_lda$lda_model %>%
    map(print, n = 3)

#9 - Visualize topics
lda_vis(mindful_lda$lda_model[[1]], mindful_lda$data[[1]])

#10 - Evaluate model
perplexity(mindful_lda$lda_model[[1]])

#11 - Calculate document to topic mapping
mindful_doc_topics <-
    mindful_lda$lda_model %>%
        map_dfr(tidy, matrix = "beta")

mindful_doc_topics_wide <-
    mindful_doc_topics %>%
        spread(topic, beta)

#12 - Visualize mapping
ggplot(data = mindful_doc_topics_wide, mapping = aes(x = Wellness.Category, y = Topic_1)) +
    geom_bar(stat = "identity") +
    xlab("Wellness Category") +
    ylab("Topic 1 Beta Score") +
    ggtitle("Document to Topic Mapping")

#13 - Create relevant words data frame
mindful_relevant_words <-
  mindful_lda$lda_model %>%
  map_dfr(broom::tidy, matrix = "gamma") %>%
  group_by(topic) %>%
  top_n(5, gamma) %>%
  ungroup() %>%
  mutate(relevance = gamma / sum(gamma))

#14 - Visualize relevant words
ggplot(data = mindful_relevant_words,
       mapping = aes(x = reorder(term, relevance),
                     y = relevance,
                     fill = as.factor(topic))) +
  geom_col(show.legend = FALSE) +
  scale_x_reordered() +
  xlab("Relevant Words") +
  ylab("Proportion of Topic") +
  facet_wrap(~ topic, nrow = 1, scales = "free") +
  ggtitle("Most Relevant Words by Topic")

#15 - Create matrix of word counts
mindful_word_counts <-
    mindful_data %>%
        unnest_tokens(word, value) %>%
        count(Wellness.Category, word)

#16 - Visualize word counts
ggplot(mindful_word_counts, aes(x = reorder(word, n),
                                y = n,
                                fill = Wellness.Category)) +
    geom_col(show.legend = FALSE) +
    xlab("Word") +
    ylab("Number of Occurrences") +
    ggtitle("Word Count by Wellness Category") +
    facet_wrap(~ Wellness.Category, nrow = 3, scales = "free")

#17 - Create matrix of correlation
mindful_cor0 <-
  mindful_word_counts %>%
  pivot_wider(id_cols = Wellness.Category,
              names_from = word,
              values_from = n,
              values_fill = 0) %>%
  select(-contains("stress"))

mindful_dist <-
  mindful_cor0 %>%
  dist()

#18 - Visualize correlation
ggplot(as.matrix(mindful_dist), aes(x = 1, y = 2)) +
  geom_tile(aes(fill = value)) +
  geom_text(aes(label = round(value, 1))) +
  scale_fill_gradient(low = "white", high = "steelblue") +
  facet_wrap(~ Wellness.Category, nrow = 3, scales = "free") +
  labs(x = NULL, y = NULL) +
  ggtitle("Distance Between Wellness Categories")

#19 - Calculate word associations
mindful_dist_long <-
  mindful_dist %>%
  as_tibble() %>%
  pivot_longer(cols = -Wellness.Category,
               names_to = "Wellness.Category.1",
               values_to = "dist.score")

word_associations <-
  mindful_dist_long %>%
  filter(!Wellness.Category == Wellness.Category.1) %>%
  filter(dist.score < 0.9) %>%
  arrange(desc(dist.score))

#20 - Visualize word associations
ggplot(data = word_associations,
       aes(x = reorder(Wellness.Category, dist.score),
           y = reorder(Wellness.Category.1, dist.score))) +
  geom_tile(aes(fill = dist.score)) +
  xlab("Wellness Category") +
  ylab("Wellness Category") +
  scale_fill_gradient(low = "white", high = "steelblue") +
  ggtitle("Similarity Between Wellness Categories")