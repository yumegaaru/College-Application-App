library(rvest)
library(magrittr)
library(stringr)
library(tibble)
library(tidyr)
library(dplyr)
library(purrr)

gre_score = data.frame(prior = seq(800,200,-10),
           current = c(rep(170,5),rep(169,2),rep(168,2),167,166,rep(165,2),rep(164,2),
                       163,rep(162,2),161,rep(160,2),159,rep(158,2),157,rep(156,2),155,
                       rep(154,2),153,rep(152,2),rep(151,2),150,rep(149,2),148,147,
                       rep(146,2),145,144,rep(143,2),142,141,140,139,138,137,135,134,133,132,
                       131,rep(130,4)))

gre = vector()

for(i in seq_along(gre_score$prior)){
  gre[gre_score$prior[i]] = gre_score$current[i]
}

save(gre, file = "gre_score.Rdata")
