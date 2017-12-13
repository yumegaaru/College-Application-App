library(rvest)
library(magrittr)
library(stringr)
library(tibble)
library(tidyr)
library(dplyr)

base = "https://www.usnews.com/best-graduate-schools/top-science-schools/statistics-rankings"

us_news = data.frame()

for(i in 1:3){
  url = paste0(base, "/page+", i)
  page = read_html(url)
  rank = page %>% 
    html_nodes(".break-after") %>% 
    html_text() %>% 
    str_extract("[0-9]+") %>% 
    as.numeric() %>%
    {.[-1]}
  
  location = page %>% 
    html_nodes(".location") %>% 
    html_text()
  
  college = page %>% 
    html_nodes(".school-name") %>% 
    html_text()
  
  score = page %>% 
    html_nodes(".c_avg_acad_rep_score") %>% 
    html_text() %>% 
    str_extract("[0-9]+.[0-9]") %>% 
    {.[-1]}
  
  temp = bind_cols(rank = rank, location = location, college = college, score = score)
  
  us_news = bind_rows(us_news, temp)
}

