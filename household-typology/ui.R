library(shiny)

#https://github.com/wch/testapp/blob/master/custom-style/ui.R
widget_style <-
  "display: inline-block;
  vertical-align: text-top;
  padding: 7px;
  border: solid;
  border-width: 0;
  border-radius: 4px;
  border-color: #CCC;"


shinyUI(bootstrapPage(
  
  # Application title
  headerPanel("Typology of financial attitudes in UK households"),
  
  # Sidebar with controls to select a dataset
  fluidRow(
    column(4,
           wellPanel(selectInput("year", label=h5("Select a year of the Bank of England-NMG household survey"),
                                 choices=c("2009","2010","2011"), selected = "2009"),
                     #h5("Select the number of household typologies"),
                     #actionButton("clusttwo", label=h2("2")),
                     #actionButton("clustfour", label=h2("4")),
                     radioButtons("clustnum", label=h5("How many levels of financial typology would you like to see?"),
                                  choices=c("Two","Four"), selected="Two"),
                     style = "height: 260px;"
           )
           ),
    
    column(4,
           #conditionalPanel(condition="input.year=='2009' & input.clusttwo > input.clustfour",
           conditionalPanel(condition="input.year=='2009' & input.clustnum=='Two'",
                            absolutePanel(height='260px', width='400px',
                                          navlistPanel(
                                            tabPanel(h4("Secure"), value=1),
                                            tabPanel(h4("Insecure"), value=2),
                                            widths=c(12,12),
                                            id='hh09_two'
                                            ),
                                          p("Clustering households according to questions related to financial attitude reveals the above household types",style="font-weight:bold; font-style:italic;")
                            )
                            ),
                                                  
           #conditionalPanel(condition="input.year=='2009' & input.clusttwo <= input.clustfour",
           conditionalPanel(condition="input.year=='2009' & input.clustnum=='Four'",
                            absolutePanel(height='260px', width='400px',
                                          navlistPanel(
                                            tabPanel("Cluster 1", value=1, icon = icon("table")),
                                            tabPanel("Cluster 2", value=2),
                                            tabPanel("Cluster 3", value=3),
                                            tabPanel("Cluster 4", value=4),
                                            widths=c(12,12),
                                            id='hh09_four'
                                          ),
                                          p("Clustering households according to questions related to financial attitude reveals the above household types",style="font-weight:bold; font-style:italic;")
                            )
                            ),
           
           #conditionalPanel(condition="input.year=='2010' & input.clusttwo > input.clustfour",
           conditionalPanel(condition="input.year=='2010' & input.clustnum=='Two'",
                            absolutePanel(height='260px', width='400px',
                                          navlistPanel(
                                            tabPanel(h4("Secure"), value=1),
                                            tabPanel(h4("Insecure"), value=2),
                                            widths=c(12,12),
                                            id='hh10_two'
                                          ),
                                          p("Clustering households according to questions related to financial attitude reveals the above household types",style="font-weight:bold; font-style:italic;")
                            )
           ),
           
           #conditionalPanel(condition="input.year=='2010' & input.clusttwo > input.clustfour",
           conditionalPanel(condition="input.year=='2010' & input.clustnum=='Four'",
                            absolutePanel(height='260px', width='400px',
                                          navlistPanel(
                                            "There does not seem to be further than two levels of financial typology in 2010",
                                            widths=c(12,12)
                                          )             
                            )
           ),
           
           
           #conditionalPanel(condition="input.year=='2011' & input.clusttwo > input.clustfour",
           conditionalPanel(condition="input.year=='2011' & input.clustnum=='Two'",
                            absolutePanel(height='260px', width='400px',
                                          navlistPanel(
                                            tabPanel(h4("Secure"), value=1),
                                            tabPanel(h4("Insecure"), value=2),
                                            widths=c(12,12),
                                            id='hh11_two'
                                          ),
                                          p("Clustering households according to questions related to financial attitude reveals the above household types",style="font-weight:bold; font-style:italic;")
                            )
           ),
           
           #conditionalPanel(condition="input.year=='2011' & input.clusttwo > input.clustfour",
           conditionalPanel(condition="input.year=='2011' & input.clustnum=='Four'",
                            absolutePanel(height='260px', width='400px',
                                          navlistPanel(
                                            tabPanel("Highly Secure", value=1),
                                            tabPanel("Secure but worried", value=2),
                                            tabPanel("Struggling to keep up", value=3),
                                            tabPanel("Falling behind", value=4),
                                            widths=c(12,12),
                                            id='hh11_four'
                                          ),
                                          p("Clustering households according to questions related to financial attitude reveals the above household types",style="font-weight:bold; font-style:italic;")
                            )
           )     
    ),
    
    column(4,
           conditionalPanel(condition="input.mainpanelState==2",
                            absolutePanel(height='260px', width='400px',
                                          navlistPanel(
                                            tabPanel("Family", value=1),
                                            tabPanel("Location", value=2),
                                            tabPanel("Blah", value=3),
                                            tabPanel("Blahblah", value=4),
                                            widths=c(12,12),
                                            id='demog'
                                            ),
                                          p("What are the defining features of this household type?",style="font-weight:bold; font-style:italic;")
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
  
    
                                      
                    
  
  
    

                     
                     
    
    