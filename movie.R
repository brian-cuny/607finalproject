library(tidyverse)
library(magrittr)
library(httr)
library(jsonlite)
library(httr)
library(RCurl)
library(XML)
library(tidytext)
library(SnowballC)

Movie.API.Query <- function(movie, year){
  print(movie)
  initial.query <- GET('http://www.omdbapi.com/', 
      add_headers('Content-Type'='application/json', 'Accept-Encoding'='gzip'),
      query=list('t'=movie, 'apikey'=apikey, 'y'=year, 'plot'='full')
  ) %>%
  content(as='text') %>%
  fromJSON(flatten=FALSE) %>%
  .[-15] %>%
  as.tibble()
  if(ncol(initial.query) == 2){
    print('Movie Not Found!')
    tibble(Title=movie, Year=as.character(year))
  }else{
    initial.query %>%
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
  Sys.sleep(0.5)
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

top.movies <- Top.Movie.Query(2017:2008, 50)

top.movies.query <- map2_df(top.movies$Movie[476:500], top.movies$Year[476:500], ~Movie.API.Query(.x, .y))

write_csv(top.movies.query, 'C:\\Users\\Brian\\Desktop\\GradClasses\\Spring18\\607\\607finalproject\\2008_2.csv')


word.analysis <- top.20.movies %>%
  select(Title, Lead_1_Male, Lead_2_Male, Plot) %>%
  unnest_tokens(word, Plot) %>%
  anti_join(stop_words) %>%
  mutate(#word = wordStem(word),
         males = Lead_1_Male + Lead_2_Male
         )

data.tfidf <- word.analysis %>%
  count(males, word) %>%
  bind_tf_idf(word, males, n) %>%
  arrange(males, tf_idf) %>%
  mutate(order = row_number()) %>%
  group_by(males) %>%
  top_n(10, tf_idf)

ggplot(data.tfidf, aes(order, tf_idf, fill=males)) +
  geom_bar(show.legend=FALSE, stat='identity') +
  facet_wrap(~males, scales='free') +
  coord_flip() +
  theme(axis.text.x=element_text(angle=-30, vjust=1, hjust=0)) +
  scale_x_continuous(
    breaks = data.tfidf$order,
    labels = data.tfidf$word
  )


Wikipedia.Gender.Query('Patrick Stewart')
Wikipedia.Gender.Query('Gal Gadot')
Wikipedia.Gender.Query('Tom Holland')
Wikipedia.Gender.Query('Neel Sethi')
Wikipedia.Gender.Query('Anna Kendrick')
Wikipedia.Gender.Query('Jaime FitzSimons')










