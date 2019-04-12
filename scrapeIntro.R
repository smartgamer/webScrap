
# https://www.r-bloggers.com/practical-introduction-to-web-scraping-in-r/

# install.packages("robotstxt")
library(robotstxt)
library(rvest)
library(selectr)
library(xml2)
library(dplyr)
library(stringr)
library(forcats)
library(magrittr)
library(tidyr)
library(ggplot2)
library(lubridate)
library(tibble)
library(purrr)

paths_allowed(
  paths = c("https://www.amazon.in/mobile-phones/b?ie=UTF8&node=1389401031&ref_=nav_shopall_sbc_mobcomp_all_mobiles")
)

top_phones <- read_html("https://www.amazon.in/mobile-phones/b?ie=UTF8&node=1389401031&ref_=nav_shopall_sbc_mobcomp_all_mobiles")
top_phones

top_phones %>%
  html_nodes(".crwTitle a") %>%
  html_text() 

top_phones %>%
  html_nodes(".crwTitle a") %>%
  html_text() %>%
  str_split('\\(') %>%
  map_chr(1) %>%
  str_trim() -> mobile_name

mobile_name


# Color

top_phones %>%
  html_nodes(".crwTitle a") %>%
  html_text() 

top_phones %>%
  html_nodes(".crwTitle a") %>%
  html_text() %>%
  str_split('\\(') %>%
  map_chr(2) %>%
  str_split(",") %>%
  map_chr(1) -> mobile_color

mobile_color



# Rating

Let us extract the ratings for the phones now. If you look at the HTML code,
we can locate rating within the following:

    .crwProductDetail

It is wrapped within identified by the class .a-icon-alt which is
inside a hyperlink in the section identified by the class .crwProductDetail.

top_phones %>%
  html_nodes(".crwProductDetail span .a-icon-alt") %>%
  html_text() 

top_phones %>%
  html_nodes(".crwProductDetail span .a-icon-alt") %>%
  html_text() %>%
  str_sub(start = 1, end = 3) %>%
  as.numeric() -> mobile_rating

mobile_rating


# Number of Reviews ----
top_phones %>%
  html_nodes(".crwProductDetail span.a-size-small .a-link-normal") %>%
  html_text() 

top_phones %>%
  html_nodes(".crwProductDetail span.a-size-small .a-link-normal") %>%
  html_text() %>%
  str_replace(",", "") %>%
  as.numeric() -> mobile_review

mobile_review


# Real Price ----
top_phones %>%
  html_nodes(".crwProductDetail .crwPrice .a-text-strike") %>%
  html_text()

top_phones %>%
  html_nodes(".crwProductDetail .crwPrice .a-text-strike") %>%
  html_text() %>%
  str_trim() %>%
  str_sub(start = 5) %>%
  str_replace(",", "") %>%
  str_split("\\.") %>%
  map_chr(1) %>%
  as.numeric() -> real_price

real_price 

  
top_phones %>%
  html_nodes(".crwProductDetail .crwPrice .a-text-strike") %>%
  html_text() 

top_phones %>%
  html_nodes(".crwProductDetail .crwPrice .a-text-strike") %>%
  html_text() %>%
  str_trim() %>%
  str_sub(start = 5) %>%
  str_replace(",", "") %>%
  str_split("\\.") %>%
  map_chr(1) %>%
  as.numeric() -> real_price

real_price


# Actual Price -----
top_phones %>%
  html_nodes(".crwProductDetail .crwActualPrice") %>%
  html_text()
top_phones %>%
  html_nodes(".crwProductDetail .crwActualPrice") %>%
  html_text() %>%
  str_trim() %>%
  str_sub(start = 5) %>%
  str_replace(",", "") %>%
  str_split("\\.") %>%
  map_chr(1) %>%
  as.numeric() -> discounted_price

discounted_price

# Putting it all together…

best_sellers <- tibble(title = mobile_name, color = mobile_color, 
  rating = mobile_rating, reviews = mobile_review, `Real Price (Rs.)` = real_price,
  `Discount Price (Rs.)` = discounted_price)

best_sellers


# IMDB Top 50        --------------
# robotstxt

# Let us check if we can scrape the data from the website using paths_allowed() from robotstxt package.

paths_allowed(
  paths = c("https://www.imdb.com/search/title?groups=top_250&sort=user_rating")
)

imdb <- read_html("https://www.imdb.com/search/title?groups=top_250&sort=user_rating")
imdb

# Title -----

imdb %>%
  html_nodes(".lister-item-content h3 a") %>%
  html_text() -> movie_title

movie_title

# Year of Release
imdb %>%
  html_nodes(".lister-item-content h3 .lister-item-year") %>%
  html_text()
imdb %>%
  html_nodes(".lister-item-content h3 .lister-item-year") %>%
  html_text() %>%
  str_sub(start = 2, end = 5) %>%
  as.Date(format = "%Y") %>%
  year() -> movie_year

movie_year

# Certificate
imdb %>%
  html_nodes(".lister-item-content p .certificate") %>%
  html_text() -> movie_certificate

movie_certificate

# Runtime
imdb %>%
  html_nodes(".lister-item-content p .runtime") %>%
  html_text()
imdb %>%
  html_nodes(".lister-item-content p .runtime") %>%
  html_text() %>%
  str_split(" ") %>%
  map_chr(1) %>%
  as.numeric() -> movie_runtime

movie_runtime

# Genre
imdb %>%
  html_nodes(".lister-item-content p .genre") %>%
  html_text() 
imdb %>%
  html_nodes(".lister-item-content p .genre") %>%
  html_text() %>%
  str_trim() -> movie_genre

movie_genre

# Rating
imdb %>%
  html_nodes(".ratings-bar .ratings-imdb-rating") %>%
  html_attr("data-value") 
imdb %>%
  html_nodes(".ratings-bar .ratings-imdb-rating") %>%
  html_attr("data-value") %>% 
  as.numeric() -> movie_rating

movie_rating

# XPATH  ----
To extract votes from the web page, we will use a different technique. In this case, we will use xpath and attributes to locate the total number of votes received by the top 50 movies.

xpath is specified using the following:

    tab
    attribute name
    attribute value

# Votes
imdb %>%
  html_nodes(xpath = '//meta[@itemprop="ratingCount"]') %>% 
  html_attr('content') 
imdb %>%
  html_nodes(xpath = '//meta[@itemprop="ratingCount"]') %>% 
  html_attr('content') %>% 
  as.numeric() -> movie_votes

movie_votes


# Revenue
imdb %>%
  html_nodes(xpath = '//span[@name="nv"]') %>%
  html_text() 
imdb %>%
  html_nodes(xpath = '//span[@name="nv"]') %>%
  html_text() %>%
  str_extract(pattern = "^\\$.*") %>%
  na.omit() %>%
  as.character() %>%
  append(values = NA, after = 30) %>%
  append(values = NA, after = 46) %>%
  str_sub(start = 2, end = nchar(.) - 1) %>%
  as.numeric() -> movie_revenue

movie_revenue

# Putting it all together…

top_50 <- tibble(title = movie_title, release = movie_year, 
    `runtime (mins)` = movie_runtime, genre = movie_genre, rating = movie_rating, 
    votes = movie_votes, `revenue ($ millions)` = movie_revenue)

top_50



# Top Websites -----

Unfortunately, we had to drop this case study as the HTML code changed while we were working on this blog post. Remember, the third point we mentioned in the things to keep in mind, where we had warned that the design or underlying HTML code of the website may change. It just happened as we were finalizing this post.
# RBI Governors

In this case study, we are going to extract the list of RBI (Reserve Bank of India) Governors. The author of this blog post comes from an Economics background and as such was intereseted in knowing the professional background of the Governors prior to their taking charge at India’s central bank. We will extact the following details:

    name
    start of term
    end of term
    term (in days)
    background

# robotstxt

# Let us check if we can scrape the data from Wikipedia website using paths_allowed() from robotstxt package.

paths_allowed(
  paths = c("https://en.wikipedia.org/wiki/List_of_Governors_of_Reserve_Bank_of_India")
)

# Since it has returned TRUE, we will go ahead and download the web page using read_html() from xml2 package.

rbi_guv <- read_html("https://en.wikipedia.org/wiki/List_of_Governors_of_Reserve_Bank_of_India")
rbi_guv

# List of Governors

# The data in the Wikipedia page is luckily structured as a table and we can extract it using html_table().

rbi_guv %>%
  html_nodes("table") %>%
  html_table() 

# There are 2 tables in the web page and we are interested in the second table. Using extract2() from the magrittr package, we will extract the table containing the details of the Governors.

rbi_guv %>%
  html_nodes("table") %>%
  html_table() %>%
  extract2(2) -> profile

# Sort

# Let us arrange the data by number of days served. The Term in office column contains this information but it also includes the text days. Let us split this column into two columns, term and days, using separate() from tidyr and then select the columns Officeholder and term and arrange it in descending order using desc().

profile %>%
  separate(`Term in office`, into = c("term", "days")) %>%
  select(Officeholder, term) %>%
  arrange(desc(as.numeric(term)))

# Backgrounds

# What we are interested is in the background of the Governors? Use count() from dplyr to look at the backgound of the Governors and the respective counts.

profile %>%
  count(Background) 

# Let us club some of the categories into Bureaucrats as they belong to the Indian Administrative/Civil Services. The missing data will be renamed as No Info. The category Career Reserve Bank of India officer is renamed as RBI Officer to make it more concise.

profile %>%
  pull(Background) %>%
  fct_collapse(
    Bureaucrats = c("IAS officer", "ICS officer",
    "Indian Administrative Service (IAS) officer",
    "Indian Audit and Accounts Service officer",
    "Indian Civil Service (ICS) officer"),
    `No Info` = c(""),
    `RBI Officer` = c("Career Reserve Bank of India officer")
  ) %>%
  fct_count() %>%
  rename(background = f, count = n) -> backgrounds

backgrounds

backgrounds %>%
  ggplot() +
  geom_col(aes(background, count), fill = "blue") +
  xlab("Background") + ylab("Count") +
  ggtitle("Background of RBI Governors")

















