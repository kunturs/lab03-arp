library(rvest)
library(dplyr)
library(readr)

# Set working directory
setwd("~/Downloads/ARPlab")

# Timestamp
now <- Sys.time()
now_str <- format(now, "%Y-%m-%d_%H-%M-%S")

# Step 1: Read CNA Asia front page
page <- read_html("https://www.channelnewsasia.com/asia")

# Step 2: Extract and filter article links
links <- page %>%
  html_elements("a") %>%
  html_attr("href") %>%
  na.omit()

# Keep only full article links (e.g., /asia/xxx-1234567)
article_links <- unique(links[grepl("^/asia/.+-\\d{6,}$", links)])
article_links <- paste0("https://www.channelnewsasia.com", article_links)

# Step 3: Scraping function for CNA articles
scrape_cna_article <- function(url) {
  tryCatch({
    article_page <- read_html(url)

    headline <- article_page %>%
      html_element("h1.h1.h1--page-title") %>%
      html_text(trim = TRUE)

    # Updated text extraction: grab all <p> inside <div class="text-long">
    text <- article_page %>%
      html_elements("div.text-long p") %>%
      html_text(trim = TRUE) %>%
      paste(collapse = "\n\n")

    data.frame(
      channel = "channelnewsasia",
      headline = headline,
      text = text,
      url = url,
      timestamp = now,
      stringsAsFactors = FALSE
    )
  }, error = function(e) {
    cat("❌ Error scraping:", url, "\n")
    return(NULL)
  })
}

# Step 4: Scrape up to 100 articles
news_list <- list()
for (url in article_links) {
  if (length(news_list) >= 100) break
  result <- scrape_cna_article(url)
  if (!is.null(result)) {
    news_list[[length(news_list) + 1]] <- result
    cat("✅ Scraped:", url, "\n")
  }
  Sys.sleep(2)  # polite delay
}

# Step 5: Save to CSV
if (length(news_list) > 0) {
  news_df <- bind_rows(news_list)
  output_file <- paste0("cna_asia_news_", now_str, ".csv")
  write_csv(news_df, output_file)
  cat("✅ Done. Articles saved to:", output_file, "\n")
} else {
  cat("⚠️ No articles scraped.\n")
}
