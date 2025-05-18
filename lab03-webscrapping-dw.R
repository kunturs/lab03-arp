library(rvest)
library(dplyr)
library(readr)

# Set working directory
setwd("~/Downloads/ARPlab")

# Timestamp
now <- Sys.time()
now_str <- format(now, "%Y-%m-%d_%H-%M-%S")

# DW Politics section
page <- read_html("https://www.dw.com/en/german-politics/t-63813230")

# Extract article links
links <- page %>%
  html_elements("a") %>%
  html_attr("href") %>%
  na.omit()

# Filter valid DW article links (e.g. /en/topic/a-12345678)
article_links <- unique(links[grepl("^/en/.*/a-[0-9]+$", links)])
article_links <- paste0("https://www.dw.com", article_links)

# Updated scraping function using provided classes
scrape_article <- function(url) {
  tryCatch({
    page <- read_html(url)

    headline <- page %>%
      html_element("h1.saqvxdf") %>%
      html_text(trim = TRUE)

    text <- page %>%
      html_elements("p.teaser-text") %>%
      html_text(trim = TRUE) %>%
      paste(collapse = "\n\n")

    data.frame(headline = headline, text = text, timestamp = now)
  }, error = function(e) NULL)
}

# Collect up to 100 articles
news_list <- list()
for (url in article_links) {
  if (length(news_list) >= 100) break
  result <- scrape_article(url)
  if (!is.null(result)) news_list[[length(news_list) + 1]] <- result
}

# Combine and export
news_df <- bind_rows(news_list)
write_csv(news_df, paste0("dw_news_", now_str, ".csv"))

cat("✅ Done. Articles saved to: dw_news_", now_str, ".csv\n", sep = "")
