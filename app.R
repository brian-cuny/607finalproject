library(shiny)
library(tidyverse)
library(d3treeR)
library(treemap)

ui <- fluidPage(
  d3tree3Output('tree', width='1500px', '1000px')
)

server <- function(input, output){
  output$tree <- renderD3tree3({
    d3tree3(tree, root='Movie')
  })
}

shinyApp(ui = ui, server = server)
