library(rvest)
library(magrittr)
library(stringr)
library(tibble)
library(tidyr)
library(dplyr)
library(purrr)

load("cleaned_gradcafe.Rdata")
college_list = cleaned_gradcafe %>% 
  dplyr::select(standard) %>% unique() %>%  
  arrange((standard)) %>% 
  unname() %>% unlist()

base = "https://en.wikipedia.org/wiki/"

wiki = data.frame(matrix(ncol = 4, nrow = 0))
colnames(wiki) = c("college", "introduction", "seal_url", "logo_url")

for(i in 1:length(college_list)){
  college = str_replace_all(college_list[i],"--|—", ", ")
  college = str_replace_all(college," ", "_")
  url = paste0(base, college)
  page = read_html(url)
  
  ## Get introduction
  intro = page %>% html_nodes(".vcard+ p") %>% 
    html_text() %>% 
    str_replace_all("\\[\\d+\\]","") %>% 
    {.[1]}
  if(is.na(intro)){
    intro = page %>% html_nodes(".noviewer+ p")  %>% 
      html_text() %>% 
      str_replace_all("\\[\\d+\\]","") %>% 
      {.[1]}
  }
  if(is.na(intro)){
    intro = page %>% html_nodes(".tright+ p")  %>% 
      html_text() %>% 
      str_replace_all("\\[\\d+\\]","") %>% 
      {.[1]}
  }
  if(str_detect(intro, "^Coordinates")){
    intro = page %>% html_nodes("p:nth-child(4) , p:nth-child(3)")  %>% 
      html_text() %>% 
      str_replace_all("\\[\\d+\\]","") %>% 
      paste(., collapse = " ")
  }
  
  ## picture
  picUrl1 = page %>% html_nodes(".image img") %>% {.[1]} %>% 
    str_extract('src="[\\s,\\S]+?"') %>% 
    str_extract('//[\\s,\\S]+.[a-z]+')
  
  picUrl2 = page %>% html_nodes(".image img") %>% {.[2]} %>% 
    str_extract('src="[\\s,\\S]+?"') %>% 
    str_extract('//[\\s,\\S]+.[a-z]+')
  
  temp = bind_cols(college = college_list[i], introduction = intro,
                   seal_url = picUrl1, logo_url=picUrl2) 
  wiki = bind_rows(wiki,temp)
}

save(wiki, file = "wiki.Rdata")
