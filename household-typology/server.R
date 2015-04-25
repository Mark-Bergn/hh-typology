#Read data
s2009 <- read.csv('s2009.csv')
s2010 <- read.csv('s2010.csv')
s2011 <- read.csv('s2011.csv')

# Define server logic required to summarize and view the selected dataset
shinyServer(function(input, output) {
  
  # Determine whether to show all Time levels or just one
  dataRecency <- reactive({
    if (input$recencyall==T)
      x <- c("no previous", "distant", "recent", "very recent")
    else
      x <- input$recency
  })
})
  

