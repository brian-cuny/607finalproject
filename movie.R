library(tidyverse)
library(magrittr)
library(httr)
library(jsonlite)
library(httr)
library(RCurl)
library(XML)
library(tidytext)
library(SnowballC)
library(RNeo4j)
library(ggrepel)
library(ggraph)
library(igraph)
library(scales)


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

top.movies.query <- map2_df(top.movies$Movie, top.movies$Year, ~Movie.API.Query(.x, .y))



all.movies <- 2008:2017 %>%  
                map_df(~read_csv(paste0('C:\\Users\\Brian\\Desktop\\GradClasses\\Spring18\\607\\607finalproject\\', ., '.csv')))

all.movies %<>%
  mutate(Type = ifelse(Lead_1_Male & Lead_2_Male, 'Male/Male', 
                ifelse(Lead_1_Male & !Lead_2_Male, 'Male/Female', 
                ifelse(!Lead_1_Male & Lead_2_Male, 'Female/Male', 'Female/Female'))))


# exploratory analysis ----------------------------------------------------

movie.count <- all.movies %>%
  count(Type)



library(treemap)
library(d3Tree)

movie.count %>%
  treemap(index='Type',
          vSize='n',
          type='index',
          title='Blockbuster Distribution',
          palette = c("#e41a1c", "#377eb8", "#4daf4a", "#984ea3"))

chisq.test(x=movie.count$n, p=rep(0.25, 4)) 


#There is unequal representation in mm, mf, fm and ff movies.


gender.genre <- all.movies %>%
  count(Type, Genre) %>%
  group_by(Type) %>%
  mutate(prop = n/sum(n)) %>%
  ungroup(Type) %>%
  arrange(Type, desc(prop))

gender.genre %>%
  select(-n) %>%
  spread(Genre, prop)

gender.genre.compare <- all.movies %>%
  count(Type, Genre) %>%
  spread(Genre, n) %>%
  mutate_all(. %>% replace_na(0)) %>% 
  select(-1) %>%
  as.matrix()


chisq.test(gender.genre.compare)

tree.movies <- all.movies %>%
  mutate(BoxOffice = BoxOffice/100,
         Title = substr(Title, 0, 20),
         Type = ifelse(Lead_1_Male & Lead_2_Male, 'Male/Male', ifelse(Lead_1_Male & !Lead_2_Male, 'Male/Female', 
                ifelse(!Lead_1_Male & Lead_2_Male, 'Female/Male', 'Female/Female')))
  ) %>%
  select(-Lead_1_Male, -Lead_2_Male, -Poster, -imdbID)


tree <- treemap(tree.movies, index=c('Type', 'Genre', 'Title'),
          vSize='BoxOffice',
          type='index',
          title='Top Movies By Gender of Lead Stars',
          palette='Set1',
          fontsize.labels = c(20, 15, 10))

treemap(tree.movies, index=c('Type', 'Genre'),
        vSize='BoxOffice',
        type='index',
        title='Top Movies By Gender of Lead Stars',
        palette='Set1',
        fontsize.labels = c(20, 15))

d3tree3(tree, rootname='All Movies')

ggplot(gender.genre) +
  geom_bar(aes(reorder(Genre, desc(Genre)), prop, fill=Type), stat='identity', position='dodge') +
  coord_flip() +
  scale_x_discrete() +
  scale_y_continuous(expand=c(0, 0)) +
  scale_fill_brewer(palette = 'Set1') +
  labs(x=NULL,
       y='Proportion') +
  theme_bw() +
  theme(panel.grid.major.y = element_blank())
  

#Women get comedy movies much more than Men, who get action movies

top.box.office <- all.movies %>%
  select(1, 2, 14, 15) %>%
  group_by(Type) %>%
  top_n(5, BoxOffice) %>%
  arrange(Type, desc(BoxOffice)) %>%
  mutate(BoxOffice = dollar(BoxOffice))

#MM movies are all action movies. MF movies 4 of the 5 F are romantic interests, in FM are action movies, FF are about princesses

movie.ratings <- all.movies %>%
  count(Type, Rated) %>%
  group_by(Type) %>%
  mutate(prop = n/sum(n)) %>%
  ungroup(Type) %>%
  arrange(Type, Rated)

#FF movies are more likely to be rated R. This is a result of the FF movies being comedies and the non FF movies are action/adventures. 
#The only 'Adult' FF action movies are Spy and Ghostbusters. Both star Melissa McCarthy and both were advertised as comedies first and action second


# - -----------------------------------------------------------------------
# plot sentiment analysis -------------------------------------------------
# - -----------------------------------------------------------------------

word.analysis <- all.movies %>%
  select(Title, Type, Plot) %>%
  unnest_tokens(word, Plot) %>%
  anti_join(stop_words) %>%
  filter(word %in% get_sentiments('nrc')$word) %>%
  filter(!duplicated(.)) %>%
  count(Type, word, sort=TRUE) %>%
  bind_tf_idf(word, Type, n)
  
label.data <- word.analysis %>%
  arrange(Type, n) %>%
  mutate(order = row_number()) %>%
  group_by(Type) %>%
  top_n(10, n)
  
ggplot(label.data, aes(order, n, fill=Type)) +
  geom_bar(show.legend=FALSE, stat='identity') +
  facet_wrap(~Type, scales='free') +
  coord_flip() +
  theme(axis.text.x=element_text(angle=-30, vjust=1, hjust=0)) +
  scale_x_continuous(
    breaks = label.data$order,
    labels = label.data$word
  ) +
  scale_y_continuous(expand = c(0, 0, 0.03, 0.03)) +
  labs(x=NULL,
       y=NULL) +
  theme_bw() +
  theme(panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        strip.background = element_rect(fill='grey50')
        )
  



# FF action movies contain more dainty words, while others do not. However, the overall sentiments are equally represented.

word.analysis %>%
  count(Type, sentiment) %>%
  group_by(Type) %>%
  mutate(prop = n/sum(n)) %>%
  ungroup(Type) %>%
  arrange(Type, desc(prop)) %>%
  ggplot() +
  geom_bar(aes(reorder(sentiment, desc(sentiment)), prop, fill=Type), stat='identity', position='dodge') +
  coord_flip() +
  scale_x_discrete()

Genre.FM.Analysis <- function(genre){
  word.analysis <- all.movies %>%
    filter(Genre == genre) %>%
    select(Title, Lead_1_Male, Plot) %>%
    unnest_tokens(word, Plot) %>%
    anti_join(stop_words) %>%
    filter(word %in% get_sentiments('nrc')$word) %>%
    count(Lead_1_Male, word, sort=TRUE)
  
  label.data <- word.analysis %>%
    arrange(Lead_1_Male, n) %>%
    mutate(order = row_number()) %>%
    group_by(Lead_1_Male) %>%
    top_n(10, n)
  
  ggplot(label.data, aes(order, n, fill=Lead_1_Male)) +
    geom_bar(show.legend=FALSE, stat='identity') +
    facet_wrap(~Lead_1_Male, scales='free') +
    coord_flip() +
    theme(axis.text.x=element_text(angle=-30, vjust=1, hjust=0)) +
    scale_x_continuous(
      breaks = label.data$order,
      labels = label.data$word
    )
}

Genre.FM.Analysis('Comedy')



# movie suggestions -------------------------------------------------------

tidy.movies <- all.movies %>%
  select(1:3, 5:9, 14:15) %>%
  gather(key='x', value='Actor', Lead_1, Lead_2) %>%
  gather(key='y', value='Gender', Lead_1_Male, Lead_2_Male) %>%
  filter(str_detect(y, x)) %>%
  mutate(Gender = ifelse(Gender == TRUE, 'Male', 'Female')) %>%
  select(-c(x, y))

neo4j.password <- 'asdfasdf'

graph <- startGraph('http://localhost:7474/db/data/', username='neo4j', password=neo4j.password)

clear(graph)

addConstraint(graph, 'Movie', 'title')
addConstraint(graph, 'Actor', 'name')

query <- '
MERGE (actor:Actor {name: {name}, gender: {gender}})
MERGE (movie:Movie {title: {title}, year: TOINT({year}), rated: {rated}, genre: {genre}, boxoffice: TOINT({boxoffice})})
CREATE (actor)-[a:ACTED_IN]->(movie)
SET a.type = {type}
'
tx <- newTransaction(graph)

for(i in 1:nrow(tidy.movies)){
  row <- tidy.movies[i, ]
  
  appendCypher(tx, query,
               name = row$Actor,
               gender = row$Gender,
               title = row$Title,
               year = row$Year,
               rated = row$Rated,
               genre = row$Genre,
               boxoffice = row$BoxOffice,
               type = row$Type
               )
}

commit(tx)

summary(graph)

female.actors <- cypher(graph, "MATCH (a:Actor)-[acted:ACTED_IN]->(m:Movie)
                                WHERE a.gender = 'Female'
                                AND NOT(acted.type = 'Female/Female')
                                RETURN DISTINCT a.name")

tidy.movies %>%
  filter(Actor %in% female.actors$a.name) %>%
  count(Actor, sort=TRUE)

#Amy Adams, Cameron Diaz

action.movie.analysis <- word.analysis %>%
  filter(Type == 'Male/Male') 

set.seed(240)
sample.index <- sample(seq_len(nrow(action.movie.analysis)), 8, prob=action.movie.analysis$tf_idf)

synopsis.words <- action.movie.analysis[sample.index, ]

#Amy Adams and Cameron Diaz star in 'Action Blockbuster'! While journeying to the Sahara Desert in search of a fabled magnificent treasure, Professor Slater (Amy Adams) discoveres a conspiracy that will shake the very foundation of the world. Bound together by fate with Annie (Cameron Diaz), a local adventurer with a large cash bounty on her head, and tracked across the Desert by deadly, mysterious forces, Professor Slater must uncover an ancient secret buried by time. Towering over our heroes is the explosive adventure of a lifetime. Will they survive and what will they discover?

x.vect <- seq(0, 95, 5)
rep.vect <- rep(20, 5)

all.movies %>%
  mutate(x=rep(seq(0, 120, 5), each=20),
         y=rep(seq(120, 0, -5), 20)) %>%
  ggplot(aes(x, y)) +
  geom_text(aes(label=Title)) +
  labs(x=NULL,
       y='Box Office',
       fill='Year') +
  scale_fill_brewer(palette = 'Set1')

all.movies %>%
  select(Title) %>% 
  mutate(Empty = '') %>%
  graph_from_data_frame() %>%
  ggraph(layout='treemap', weight='size') +
  geom_node_tile(aes(fill=depth), size=0.25)
  geom_node_text(aes(label=name), repel=TRUE) +
  theme_void() +
  labs(title='')



# shiny -------------------------------------------------------------------

library(shiny)
library(tidyverse)
library(d3treeR)
library(treemap)
library(d3Tree)

ui <- fluidPage(
        sliderInput(inputId = 'num',
                label = 'Choose a number',
                value=min(all.movies$Year), min=min(all.movies$Year), max=max(all.movies$Year),
                step=1
        ),
        actionButton('clicks', 'Click Me!'),
        d3treeOutput3('hist'),
        tableOutput('all')
)

server <- function(input, output){
  
  observeEvent(input$clicks, {
    print(as.numeric(input$clicks))
  })
  
  data <- eventReactive(input$clicks, {
    all.movies %>%
      filter(Year != input$num)
  })

  output$all <- renderTable({
    data()
  })
  
  output$hist <- renderD3tree3({
    tree <- gender.genre %>%
      treemap(index=c('Type', 'Genre'),
              vSize='n',
              type='index')
    
    d3tree(tree)
  })
}

shinyApp(ui = ui, server = server)




















