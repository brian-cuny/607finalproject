library(tidyverse)
library(magrittr)
library(httr)
library(jsonlite)
library(httr)
library(RCurl)
library(XML)
library(purrrlyr)

Movie.API.Query <- function(movie, year){
  print(movie)
  GET('http://www.omdbapi.com/', 
      add_headers('Content-Type'='application/json', 'Accept-Encoding'='gzip'),
      query=list('t'=movie, 'apikey'=apikey, 'y'=year, 'plot'='full')
  ) %>%
  content(as='text') %>%
  fromJSON(flatten=FALSE) %>%
  .[-15] %>%
  as.tibble() %>%
  select(c(1, 2, 3, 5, 6, 9, 10, 14, 15, 18, 21)) %>%
  mutate(Genre = str_extract(Genre, '([^,]+)'),
         Runtime = str_extract(Runtime, '(\\d+)'),
         Actors = IMDB.Star.Query(imdbID),
         BoxOffice = parse_number(BoxOffice)
  ) %>%
  separate(Actors, c('Lead_1', 'Lead_2'), sep=', ') %>%
  mutate(Lead_1_Male = Wikipedia.Gender.Query(Lead_1),
         Lead_2_Male = Wikipedia.Gender.Query(Lead_2)
  ) %>%
  select(c(1:6, 13, 7, 14), everything())
}

IMDB.Star.Query <- function(movie.id){
  Sys.sleep(1)
  getURL(paste0('https://www.imdb.com/title/', movie.id,'/')) %>% 
    htmlParse() %>%
    xpathSApply('//*[@id="title-overview-widget"]//span[@itemprop="actors"]//a', xmlValue) %>%
    .[1:2] %>%
    paste(collapse=', ')
}

Wikipedia.Gender.Query <- function(lead){
  Sys.sleep(1)
  lead <- str_replace_all(lead, ' ', '_')
  initial.query <- getURL(paste0('https://en.wikipedia.org/wiki/', lead)) %>% 
            htmlParse() %>%
            xpathSApply('//*[@id="mw-content-text"]/div/p[position()<3]', xmlValue) %>%
            unlist()  %>%
            paste(collapse='')
  if(str_detect(initial.query, 'may refer to:')){
    initial.query <- getURL(paste0('https://en.wikipedia.org/wiki/', lead, '_(actor)')) %>% 
      htmlParse() %>%
      xpathSApply('//*[@id="mw-content-text"]/div/p[position()<3]', xmlValue) %>%
      unlist() %>%
      paste(collapse='')
  }
  if(str_detect(initial.query, 'actor') & !str_detect(initial.query, 'actress')){
    return(TRUE)
  }else if(str_detect(initial.query, 'actress') & !str_detect(initial.query, 'actor')){
    return(FALSE)
  }else{
    return(NA)
  }
}

Top.Movie.Query <- function(years, rank){
  years %>%
    map_(~getURL(paste0('http://www.boxofficemojo.com/yearly/chart/?yr=', .x, '&p=.htm')) %>% 
              htmlParse() %>%
              xpathSApply('//*[@id="body"]/table[3]//tr//td', xmlValue) %>%
              .[15:914] %>%
              matrix(ncol=9, byrow=T) %>%
              as.data.frame() %>%
              filter(row_number() <= rank) %>%
              mutate(Movie=str_replace(V2, paste0('(.*?)( \\(', .x, '\\))$'), '\\1'),
                     Year = .x) %>%
              select(Movie, Year)
    )
}

top.movies <- Top.Movie.Query(c(2017, 2016), 3)

top.movies <- map2_df(top.movies$Movie, top.movies$Year, ~Movie.API.Query(.x, .y))






Wikipedia.Gender.Query('Patrick Stewart')
Wikipedia.Gender.Query('Gal Gadot')
Wikipedia.Gender.Query('Tom Holland')
Wikipedia.Gender.Query('Neel Sethi')
Wikipedia.Gender.Query('Anna Kendrick')
Wikipedia.Gender.Query('Jaime FitzSimons')










