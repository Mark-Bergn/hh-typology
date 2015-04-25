library(shiny)

#https://github.com/wch/testapp/blob/master/custom-style/ui.R
widget_style <-
  "display: inline-block;
  vertical-align: text-top;
  padding: 7px;
  border: solid;
  border-width: 1px;
  border-radius: 4px;
  border-color: #CCC;"


shinyUI(bootstrapPage(
  
  # Application title
  headerPanel("Typology of financial attitudes in UK households"),
  
  # Sidebar with controls to select a dataset
  fluidRow(
    column(4,
           wellPanel(selectInput("year", label=h5("Select a year of the Bank of England-NMG household survey"),
                                 choices=c("2009","2010","2011"), selected = "2011"),
                     #h5("Select the number of household typologies"),
                     #actionButton("clusttwo", label=h2("2")),
                     #actionButton("clustfour", label=h2("4")),
                     style = "height: 250px;"
                     radioButtons("clustnum", label=h5("Select the number of household typologies"),
                                  choices=c("Two","Four"), selected="Two")   
           )
           ),
    
    column(4,
           conditionalPanel(condition="input.year=='2009' & input.clustnum=='Two'",
           #conditionalPanel(condition="input.year=='2009' & input.clusttwo > input.clustfour",
                            navlistPanel(
                              "Select a household type",
                              tabPanel("Secure", value=1),
                              tabPanel("Insecure", value=2),
                              widths=c(10,10),
                              id='hh09_two'
                            )
           ),
           
           conditionalPanel(condition="input.year=='2009' & input.clustnum=='Four'",
           #conditionalPanel(condition="input.year=='2009' & input.clusttwo <= input.clustfour",
                            navlistPanel(
                              "Select a household type",
                              tabPanel("Cluster 1", value=1, icon = icon("table")),
                              tabPanel("Cluster 2", value=2),
                              tabPanel("Cluster 3", value=3),
                              tabPanel("Cluster 4", value=4),
                              widths=c(10,10),
                              id='hh09_four'
                            )
           ),
           
           conditionalPanel(condition="input.year=='2010' & input.clustnum=='Two'",
           #conditionalPanel(condition="input.year=='2010' & input.clusttwo > input.clustfour",
                            navlistPanel(
                              "Select a household type",
                              tabPanel("Secure", value=1),
                              tabPanel("Insecure", value=2),
                              widths=c(10,10),
                              id='hh10_two'
                            )
           ),
           
           conditionalPanel(condition="input.year=='2010' & input.clustnum=='Four'",
           #conditionalPanel(condition="input.year=='2010' & input.clusttwo <= input.clustfour",
                            navlistPanel('There does not seem to be further separtion beyound 2 types in 2010',
                                         widths=c(10,10)
                                         )
           ),
           
           conditionalPanel(condition="input.year=='2011' & input.clustnum=='Two'",
           #conditionalPanel(condition="input.year=='2011' & input.clusttwo > input.clustfour",
                            navlistPanel(
                              "Select a household type",
                              tabPanel(h4("Secure"), value=1),
                              tabPanel(h4("Insecure"), value=2),
                              widths=c(10,10),
                              id='hh11_two'
                            )
                            
           ),
           
           conditionalPanel(condition="input.year=='2011' & input.clustnum=='Four'",
           #conditionalPanel(condition="input.year=='2011' & input.clusttwo <= input.clustfour",
                            navlistPanel(
                              "Select a household type",
                              tabPanel("Highly Secure", value=1),
                              tabPanel("Secure but worried", value=2),
                              tabPanel("Struggling to keep up", value=3),
                              tabPanel("Falling behind", value=4),
                              widths=c(10,10),
                              id='hh11_four'
                            )
                           
           )
    ),
    
    column(4,
           conditionalPanel(condition="input.mainpanelState==2",
                            navlistPanel(
                              "Select the demographic ",
                              tabPanel("Family", value=1),
                              tabPanel("Location", value=2),
                              tabPanel("Blah", value=3),
                              tabPanel("Blahblah", value=4),
                              widths=c(10,10),
                              id='demog'
                            )
           )
    )
  ),
    
  mainPanel(
    tabsetPanel(
      tabPanel(tags$h5("Financial attitudes and position"), helpText('Financial attitudes and position'), value=1),
      tabPanel(tags$h5("Demographics"), helpText('Demographics'), value=2),
      id="mainpanelState"
    )
  )
))
  
    
                                      
                    
  
  
    

                     
                     
    
    