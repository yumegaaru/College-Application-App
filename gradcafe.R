library(rvest)
library(magrittr)
library(stringr)
library(tibble)
library(tidyr)
library(dplyr)
library(purrr)

## Initiate data frame
gradcafe = data.frame(matrix(ncol = 13, nrow = 0))
colnames(gradcafe) = c("institution", "program", "degree","semester","student",
                       "status", "method", "date", "GPA", "GRE_Subject", "GRE_General_V",
                       "GRE_General_Q", "GRE_General_W")

## Scrape fro thegradcafe.com
for(i in 1:19) {
  url = paste("http://thegradcafe.com/survey/index.php?q=statistics&t=a&pp=250&o=&p=",i, sep="")
  page = read_html(url)
  institution = page %>% 
    html_nodes('.instcol') %>% 
    html_text()%>%
    str_replace("^\\s+","") %>% 
    {.[-1]}
  prog_date = page %>% 
    html_nodes(".instcol+ td") %>% 
    html_text() %>% 
    {.[-1]}
  program = prog_date %>% 
    str_extract("^[\\s,\\S]+,") %>% 
    str_replace_all(",","")
  degree = prog_date %>% 
    str_extract("Masters|PhD") %>% 
    str_extract("\\w+") %>% 
    ifelse(is.na(.), "Other",.)
  semester = prog_date %>% 
    str_extract("(\\S+)$") %>% 
    str_extract("\\w+")
  
  decision = page %>% 
    html_nodes('td:nth-child(3)') %>% 
    html_text() %>%
    {.[-1]} %>%
    str_split("(\\ via\\ |\\ on\\ |\\ Undergrad GPA:\\ |GRE General \\(V/Q/W\\):\\ |GRE Subject:\\ )")
  
  n.obs = sapply(decision, length)
  seq.max = seq_len(max(n.obs))
  mat = t(sapply(decision, "[", i = seq.max)) %>%
    as.data.frame(stringsAsFactors  = FALSE) %>%
    setNames(c("status", "method", "date", "GPA", "GRE_General", "GRE_Subject")) %>%
    mutate(GPA = as.numeric(GPA)) %>%
    mutate(GRE_Subject = as.numeric(str_replace(GRE_Subject, "♦","")))%>%
    mutate(GRE_General_V = as.numeric(substring(GRE_General,1,3))) %>%
    mutate(GRE_General_Q = as.numeric(substring(GRE_General,5,7))) %>%
    mutate(GRE_General_W = as.numeric(substring(GRE_General,9,12))) %>% 
    mutate(GRE_General = NULL) %>% 
    mutate(date = as.Date(date, format = "%d %b %Y"))
  
  student = page%>%
    html_nodes('td:nth-child(4)') %>%
    html_text() %>%
    {.[-1]} %>% 
    recode("A" = "American", "U" = "International with US degree", 
           "I" = "International without US degree",
           "O" = "Other", "?" = "Unknown") %>% 
    ifelse(.=="", "Unknown",.)
  
  temp = bind_cols(institution = institution, program = program, degree = degree,
                   semester=semester, student=student) %>% 
    bind_cols(mat)
  gradcafe = bind_rows(gradcafe,temp)
}

## Filter out spring applications 
## & applications before 2012 or after 2018
gradcafe = gradcafe %>%
  filter(!str_detect(semester,"12|S|18"))
levels(gradcafe$semester) = sapply(c(13:17), function(x) c(paste0("F",x)))

## Save scraped data to gradcafe.Rdata
save(gradcafe, file = "gradcafe.Rdata")



