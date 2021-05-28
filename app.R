library(shiny)
library(tidyverse)
dat = read.csv('final_data.csv')
dat = dat %>% group_by(singer, title) %>% filter(n()>=4) %>% ungroup
sdata = read.csv('shiny_data.csv')
ui <- fluidPage(
  tags$head(HTML("<title>클러스터 별 음원 주간 차트</title> <link rel='icon' type='image/gif/png' href='favicon.png'>")),
  titlePanel(div(img(height=64, width=64, src="favicon.png"),
                 "클러스터 별 음원 주간 차트")),
  sidebarLayout(
    sidebarPanel(
      helpText(""),
      selectInput("cluster", 
                  label = "Choose a cluster",
                  choices = c("All", "partitional cluster", "hierarchical clustering"),
                  ),

      selectInput("num", 
                  label = "Choose a cluster number",
                  choices = NULL,
                  ),
      
      selectInput("singer", 
                  label = "Choose a singer",
                  choices = NULL),
      
      selectInput("song", 
                  label = "Choose a song",
                  choices = NULL)
    ),
    
    mainPanel(
      plotOutput("chart")
    )
  )
)

server <- function(input, output, session) {
  observe({
    cluster = input$cluster
    if (cluster != "All"){
    updateSelectInput(session, 'num',
                      choices = c(1,2,3,4,5))
    } else {
        updateSelectInput(session, "num",
                          choices = NULL)
      }
  })

  
  observe({
    num = input$num
    cluster = input$cluster
    if (cluster != "All"){
      cluster = ifelse(cluster=="partitional cluster",'pc','hc')
      if (!is.null(num)){
        updateSelectInput(session, "singer",
                          choices = sdata %>% filter(eval(as.symbol(cluster))==num) %>% select(singer) %>% unique()
      )}
    } else{
      updateSelectInput(session, "singer",
                        choices = dat %>% select(singer) %>% unique())
    }
  })
  
  observe({
    
    num = input$num
    cluster = input$cluster
    singer0 = input$singer
    if (cluster != "All"){
      cluster = ifelse(cluster=="partitional cluster",'pc','hc')
      updateSelectInput(session, "song",
                      choices = sdata %>% filter(eval(as.symbol(cluster))==num, singer==singer0) %>% select(title) %>% unique()
          )
    }else {
      updateSelectInput(session, "song", 
                        choices = sdata %>% filter(singer==singer0) %>% select(title) %>% unique())
    }
  })
  
  tmp <-  reactive({
    dat %>% select(title, singer, week, rank) %>% filter(singer %in% input$singer & title %in% input$song)
  })
  
  output$chart = renderPlot({
    tmp = tmp()
    ggplot(tmp, aes(x=as.Date(week), y=rank)) +
      geom_line() +
      geom_point() +
      theme_minimal() +
      scale_x_date(date_labels = "%y-%m-%d") +
      ylim(100,1) +
      xlab("Week")+
      ggtitle(tmp$title, subtitle = tmp$singer)+
      theme(text = element_text(colour = 'gray'),
            axis.text = element_text(colour = 'gray'),
            legend.position = (c(.9,.2)), 
            plot.title = element_text(size = 20)
      )
  })
  
}
shinyApp(ui = ui, server = server)
