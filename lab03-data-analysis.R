# -------------------------------
# BBC News Text Analysis with TF-IDF
# -------------------------------

# 📁 Set working directory
setwd("~/Downloads/ARPlab")

# 📦 Load required libraries
library(dplyr)
library(tidytext)
library(ggplot2)
library(lubridate)
library(stringr)
library(readr)
library(viridis)
library(ggrepel)
library(patchwork)

# -------------------------------
# 📥 Load and Prepare the Dataset
# -------------------------------

# Get all CSV files in the folder
csv_files <- list.files(pattern = "\\.csv$")

# Read and combine all CSVs into one dataframe
news_data <- csv_files %>%
  lapply(read_csv) %>%
  bind_rows()

# Clean the dataset: remove rows with empty or missing headlines and text
df_clean <- news_data %>%
  filter(
    !is.na(headline), headline != "",
    !is.na(text), text != ""
  )

# Convert timestamp to Date format
df_clean$date <- as.Date(df_clean$timestamp)

# -------------------------------
# 🧠 Text Preprocessing
# -------------------------------

# Tokenize text and remove stopwords
tokens <- df_clean %>%
  unnest_tokens(word, text) %>%
  filter(str_detect(word, "[a-z]")) %>%
  anti_join(get_stopwords())

# Count word frequency by date
word_counts <- tokens %>%
  count(date, word, sort = TRUE)

# -------------------------------
# 📊 TF-IDF Analysis
# -------------------------------

# Calculate TF-IDF scores
tfidf <- word_counts %>%
  bind_tf_idf(term = word, document = date, n = n)

# Get top 5 TF-IDF words per date
top_words <- tfidf %>%
  group_by(date) %>%
  slice_max(tf_idf, n = 5, with_ties = FALSE) %>%
  ungroup()

# Identify the latest date per word for labeling
latest_labels <- top_words %>%
  group_by(word) %>%
  filter(date == max(date)) %>%
  ungroup()

# -------------------------------
# 📈 Visualization
# -------------------------------
# 🧪 p1: TF-IDF Line Plot
p1 <- ggplot(top_words, aes(x = date, y = tf_idf, color = word, group = word)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  geom_text_repel(
    data = latest_labels, aes(label = word),
    size = 3, show.legend = FALSE, max.overlaps = 10
  ) +
  scale_color_viridis_d(option = "rocket") +
  labs(
    title = "tf-idf",
    subtitle = "across BBC/news",
    x = "Date",
    y = "TF-IDF Score",
    color = "Keyword"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "none"
  )

# 🧪 p2: Word Frequency Bar Chart (Top 10)
p2 <- tokens %>%
  count(word, sort = TRUE) %>%
  slice_max(n, n = 10) %>%
  ggplot(aes(x = reorder(word, n), y = n)) +
  geom_col(fill = "#a9373bff") +
  coord_flip() +
  labs(
    title = "top words",
    x = "Word",
    y = "Frequency"
  ) +
  theme_minimal(base_size = 13)

## Trump

word_trend <- tokens %>%
  filter(word == "trump") %>%
  count(date)

# Plot the trend
p3 <- ggplot(word_trend, aes(x = date, y = n)) +
  geom_line(color = "#2369bdff", linewidth = 1) +
  geom_point(size = 2) +
  labs(
    title = "'trump' daily freq",
    x = "Date",
    y = "Count"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

#Combine plots using patchwork
# -------------------------------

(p1 + p2 / p3) + plot_layout(guides = "collect")
