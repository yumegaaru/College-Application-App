library(readr)
library(magrittr)
library(stringr)
library(tibble)
library(tidyr)
library(dplyr)
library(purrr)


###########institution names
institution = read_csv("School Names - Sheet1.csv", col_names = FALSE)
names(institution) = c("standard", "abbr")
institution = institution %>%
  mutate(
    abbr = str_replace_all(abbr, "\"", ""),
    standard = str_replace_all(standard, "\"", "")) %>% 
  mutate(standard = str_replace_all(standard, "\\[\\d+\\] ","")) %>% 
  mutate(abbr = str_split(abbr, ","))

inst = data.frame(standard = rep(institution$standard, sapply(institution$abbr, length)), 
                  abbr = unlist(institution$abbr)) %>%
  mutate(standard = as.character(standard)) %>%
  mutate(abbr = toupper(abbr)) %>%
  mutate(abbr = str_trim(abbr))

###########clean columns except institution
load(file = "gradcafe.Rdata")
load(file = "gre_score.Rdata")

cleaned_gradcafe = gradcafe %>%
  mutate(GRE_Subject = ifelse(GRE_Subject < 100, GRE_Subject*10, GRE_Subject)) %>%
  mutate(GRE_General_V = ifelse(GRE_General_V < 200, GRE_General_V, gre[GRE_General_V])) %>%
  mutate(GRE_General_Q = ifelse(GRE_General_Q < 200, GRE_General_Q, gre[GRE_General_Q])) %>%
  mutate(GRE_General_V = ifelse(GRE_General_V < 130, NA, GRE_General_V)) %>%
  mutate(GRE_General_Q = ifelse(GRE_General_Q < 130, NA, GRE_General_Q)) %>%
  mutate(GRE_General_W = ifelse(GRE_General_W == 0, NA, GRE_General_W)) %>%
  mutate(GRE_General_W = ifelse(GRE_General_W < 1 & GRE_General_W > 0, GRE_General_W*10, GRE_General_W)) %>%
  filter(GPA <= 4 | is.na(GPA)) %>%
  filter(GRE_General_V <= 170 | is.na(GRE_General_V)) %>%
  filter(GRE_General_Q <= 170 | is.na(GRE_General_Q)) %>%
  filter(GRE_General_W <= 6 | is.na(GRE_General_W))

##########clean institution
cleaned_gradcafe = cleaned_gradcafe %>%
  mutate(institution = toupper(institution)) %>%
  mutate(institution = str_replace_all(institution, "  ", " ")) %>%
  mutate(institution = str_replace_all(institution, 
         "(UIVERSITY|UNIVERSTY|UNIVERISTY|UNIVERCITY|UNIVERSIY|
         UNIVERITY|UNIVERSEITY|UNIVERISITY|UNIERSITY|UNIVERAITY|
         UNIEVRSITY|UNVERSITY|UNVIERSITY|UNIVESITY)", "UNIVERSITY")) %>%
  filter(!str_detect(institution, "BRITISH")) %>%
  mutate(indicator = sapply(institution, function(x) which(str_detect(x,inst$abbr)))) %>%
  mutate(indicator = sapply(indicator, function(x) ifelse(length(x)==0, NA, min(x)))) %>%
  mutate(standard = ifelse(is.na(indicator), NA, inst$standard[indicator]))

cleaned_gradcafe = cleaned_gradcafe %>% drop_na(standard)

save(cleaned_gradcafe, file = "cleaned_gradcafe.Rdata")
