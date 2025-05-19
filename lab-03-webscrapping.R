library(rvest)
library(dplyr)
library(readr)

# Set working directory
setwd("~/Documents/project03")

# Timestamp
now <- Sys.time()
now_str <- format(now, "%Y-%m-%d_%H-%M-%S")

# ---------------------- DW GENERAL SCRAPER ----------------------
scrape_dw_articles <- function() {
  cat("🔍 Scraping DW main news...\n")
  page <- read_html("https://www.dw.com/en/top-stories/s-9097")
  links <- page %>%
    html_elements("a") %>%
    html_attr("href") %>%
    na.omit()
  article_links <- unique(links[grepl("^/en/.*/a-[0-9]+$", links)])
  article_links <- paste0("https://www.dw.com", article_links)

  scrape_article <- function(url) {
    tryCatch({
      page <- read_html(url)
      headline <- page %>% html_element("h1") %>% html_text(trim = TRUE)
      text <- page %>% html_elements("p") %>% html_text(trim = TRUE) %>% paste(collapse = "\n\n")
      data.frame(source = "DW", headline = headline, text = text, url = url, timestamp = now)
    }, error = function(e) NULL)
  }

  news_list <- list()
  for (url in article_links) {
    if (length(news_list) >= 100) break
    result <- scrape_article(url)
    if (!is.null(result)) news_list[[length(news_list) + 1]] <- result
  }
  bind_rows(news_list)
}

# ---------------------- BBC GENERAL SCRAPER ----------------------
scrape_bbc_articles <- function() {
  cat("🔍 Scraping BBC front page...\n")
  page <- read_html("https://www.bbc.com/news")
  links <- page %>%
    html_elements("a") %>%
    html_attr("href") %>%
    na.omit()
  article_links <- unique(links[grepl("^/news", links) & !grepl("live|av|video", links)])
  article_links <- paste0("https://www.bbc.com", article_links)

  scrape_article <- function(url) {
    tryCatch({
      page <- read_html(url)
      headline <- page %>% html_element('div[data-component="headline-block"] h1') %>% html_text(trim = TRUE)
      text <- page %>% html_elements('div[data-component="text-block"] p') %>% html_text(trim = TRUE) %>% paste(collapse = "\n\n")
      data.frame(source = "BBC", headline = headline, text = text, url = url, timestamp = now)
    }, error = function(e) NULL)
  }

  news_list <- list()
  for (url in article_links) {
    if (length(news_list) >= 100) break
    result <- scrape_article(url)
    if (!is.null(result)) news_list[[length(news_list) + 1]] <- result
  }
  bind_rows(news_list)
}

# ---------------------- CNA GENERAL SCRAPER ----------------------
scrape_cna_articles <- function() {
  cat("🔍 Scraping CNA front page...\n")
  page <- read_html("https://www.channelnewsasia.com/news")
  links <- page %>%
    html_elements("a") %>%
    html_attr("href") %>%
    na.omit()
  article_links <- unique(links[grepl("^/[^/]+/.+-\\d{6,}$", links)])
  article_links <- paste0("https://www.channelnewsasia.com", article_links)

  scrape_article <- function(url) {
    tryCatch({
      article_page <- read_html(url)
      headline <- article_page %>% html_element("h1") %>% html_text(trim = TRUE)
      text <- article_page %>% html_elements("div.text-long p") %>% html_text(trim = TRUE) %>% paste(collapse = "\n\n")
      data.frame(source = "CNA", headline = headline, text = text, url = url, timestamp = now)
    }, error = function(e) NULL)
  }

  news_list <- list()
  for (url in article_links) {
    if (length(news_list) >= 100) break
    result <- scrape_article(url)
    if (!is.null(result)) {
      news_list[[length(news_list) + 1]] <- result
      cat("✅ Scraped:", url, "\n")
    }
    Sys.sleep(2)
  }
  bind_rows(news_list)
}

# ---------------------- RUN & COMBINE ----------------------
dw_data <- scrape_dw_articles()
bbc_data <- scrape_bbc_articles()
cna_data <- scrape_cna_articles()

all_news <- bind_rows(dw_data, bbc_data, cna_data)
output_file <- paste0("all_news_general_", now_str, ".csv")
write_csv(all_news, output_file)

cat("🎉 All general news scraped and saved to:", output_file, "\n")
