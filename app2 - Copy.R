library(ggplot2)
library(tidyverse)
library(shiny)
library(scales)
library(magrittr)


group.colors <- c('Male/Male' = "#984ea3", 'Male/Female' = "#4daf4a", 'Female/Male' ="#377eb8", 'Female/Female' = "#e41a1c")

tool.tip.movies <- all.movies %>%
  select(1:6, 8, 15, 14) %>%
  arrange(Year, BoxOffice) %>%
  mutate(Order = row_number()) %>%
  rename(`Lead 1` = Lead_1, 
         `Lead 2` = Lead_2)

background.bar <- geom_histogram(data=tool.tip.movies, aes(Order, BoxOffice), 
                             fill='grey20', stat='identity',  width=1)


ui <- fluidPage(
          selectInput('Year', label='Year', choices = c('All', years)),
          selectInput('Rated', label='Rated', choice = c('All', rateds)),
          selectInput('Genre', label='Genre', choice = c('All', genres)),
          selectInput('Type', label='Gender Balance', choice = c('All', types)),
          actionButton('action', 'Filter'),
          plotOutput("plot"),
          dataTableOutput('table')
    )


server <- function(input, output) {
  
  data <- eventReactive(input$action, {
    to.ret <- tool.tip.movies
    if(input$Year != 'All'){
      to.ret %<>% filter(Year == input$Year)
    }
    if(input$Rated != 'All'){
      to.ret %<>% filter(Rated == input$Rated)
    }
    if(input$Genre != 'All'){
      to.ret %<>% filter(Genre == input$Genre)
    }
    if(input$Type != 'All'){
      to.ret %<>% filter(Type == input$Type)
    }
    to.ret
  })
  
  output$plot <- renderPlot({
    data() %>%
      ggplot() + 
      background.bar +
      geom_bar(aes(Order, BoxOffice, fill=factor(Type)), stat='identity', show.legend = FALSE, width=1) + 
      geom_text(data=data.frame(x=seq(25, 500, 50), text=2008:2017), aes(x, y=325000000, label=text), size=5) +
      scale_x_discrete(expand=c(.01, .01), labels=NULL, breaks=0:500) +
      scale_y_continuous(expand=c(0, 0, .01, .01), labels=dollar_format(), breaks=seq(0, 1000000000, 100000000)) +
      labs(x = NULL,
           y = NULL) +
      theme_bw() +
      theme(axis.ticks.x = element_blank(),
            panel.grid.minor.x = element_blank(),
            panel.grid.major.x = element_blank()) +
      scale_fill_manual(values=group.colors)
    
  })
  
  output$table <- renderDataTable({
    data() %>%
      select(-Order) %>%
      mutate(BoxOffice = formattable::currency(BoxOffice))
  })
  
}

shinyApp(ui, server)
















