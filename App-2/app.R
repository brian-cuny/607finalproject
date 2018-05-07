library(tidyverse)
library(shiny)
library(scales)
library(magrittr)

load('realdata.RData')

ui <- fluidPage(
          tags$head(tags$title('Blockbuster Movie Explorer by Brian Weinfeld')),
          div(class='jumbotron', style='padding-top: 10px; padding-bottom: 10px', 
              h1(align='center', 'Blockbuster Movie Explorer'), 
              h3(align='right', 'By Brian Weinfeld')
              ),
          div(
              div(class='well col-xs-2',
                selectInput('Year', label='Year', choices = c('All', 2008:2017)),
                selectInput('Rated', label='Rated', choice = c('All', "G", "PG", "PG-13", "R" )),
                selectInput('Genre', label='Genre', choice = c('All', "Action", "Adventure", "Animation",
                                                         "Biography", "Comedy", "Crime", "Documentary",
                                                         "Drama", "Family", "Fantasy", "Horror",     
                                                         "Mystery", "Sci-Fi")),
                selectInput('Type', label='Gender Balance', choice = c('All', "Female/Female", "Female/Male",
                                                                 "Male/Female", "Male/Male")),
                actionButton('action', 'Filter', class='btn btn-primary btn-block')
                ),
              div(class='col-xs-10', plotOutput("plot"))
            ),
          div(class='col-xs-12', style='margin-top: 10px', dataTableOutput('table'))
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
  }, ignoreNULL=FALSE)
  
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
















