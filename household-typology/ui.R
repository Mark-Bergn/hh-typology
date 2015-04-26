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


shinyUI(fluidPage(
  
  # Application title
  headerPanel("Typology of financial attitudes: the Bank of England household survey"),
  
  # Sidebar with controls to select a dataset
  fluidRow(
    column(4,
           wellPanel(selectInput("year", label=h5("Select a year of the Bank of England-NMG household survey"),
                                 choices=c("2009","2010","2011"), selected = "2009"),
                     #h5("Select the number of household typologies"),
                     #actionButton("clusttwo", label=h2("2")),
                     #actionButton("clustfour", label=h2("4")),
                     radioButtons("clustnum", label=h5("What is the level of clustering that you wish to apply on households?"),
                                  choices=c("Broad","Detailed"), selected="Broad"),
                     style = "height: 250px;"
           )
           ),
    
    column(4,
           #conditionalPanel(condition="input.year=='2009' & input.clusttwo > input.clustfour",
           conditionalPanel(condition="input.year=='2009' & input.clustnum=='Broad'",
                            absolutePanel(height='250px', width='400px',
                                          navlistPanel(
                                            tabPanel("All types", value=0, icon=icon("users")),
                                            tabPanel(h4("Secure"), value=1, icon=icon("user")),
                                            tabPanel(h4("Insecure"), value=2, icon=icon("user")),
                                            widths=c(12,12),
                                            id='hh09_two'
                                            #id='hhclust'
                                            ),
                                          p("Clustering households according to questions related to financial attitude reveals the above household types",style="font-weight:bold; font-style:italic;")
                            )
                            ),
                                                  
           #conditionalPanel(condition="input.year=='2009' & input.clusttwo <= input.clustfour",
           conditionalPanel(condition="input.year=='2009' & input.clustnum=='Detailed'",
                            absolutePanel(height='250px', width='400px',
                                          navlistPanel(
                                            tabPanel("All types", value=0, icon = icon("users")),
                                            tabPanel("Cluster 1", value=1, icon = icon("user")),
                                            tabPanel("Cluster 2", value=2, icon = icon("user")),
                                            tabPanel("Cluster 3", value=3, icon = icon("user")),
                                            tabPanel("Cluster 4", value=4, icon = icon("user")),
                                            widths=c(12,12),
                                            id='hh09_four'
                                            #id='hhclust'
                                          ),
                                          p("Clustering households according to questions related to financial attitude reveals the above household types",style="font-weight:bold; font-style:italic;")
                            )
                            ),
           
           #conditionalPanel(condition="input.year=='2010' & input.clusttwo > input.clustfour",
           conditionalPanel(condition="input.year=='2010' & input.clustnum=='Broad'",
                            absolutePanel(height='250px', width='400px',
                                          navlistPanel(
                                            tabPanel("All types", value=0, icon=icon("users")),
                                            tabPanel(h4("Secure"), value=1, icon=icon("user")),
                                            tabPanel(h4("Insecure"), value=2, icon=icon("user")),
                                            widths=c(12,12),
                                            id='hh10_two'
                                            #id='hhclust'
                                          ),
                                          p("Clustering households according to questions related to financial attitude reveals the above household types",style="font-weight:bold; font-style:italic;")
                            )
           ),
           
           #conditionalPanel(condition="input.year=='2010' & input.clusttwo > input.clustfour",
           conditionalPanel(condition="input.year=='2010' & input.clustnum=='Detailed'",
                            absolutePanel(height='250px', width='400px',
                                          navlistPanel(
                                            tabPanel("All types", value=0, icon=icon("users")),
                                            helpText("There does not seem to be further than two levels of financial typology in 2010"),
                                            widths=c(12,12),
                                            id='hh10_four'
                                            #id='hhclust'
                                          )             
                            )
           ),
           
           
           #conditionalPanel(condition="input.year=='2011' & input.clusttwo > input.clustfour",
           conditionalPanel(condition="input.year=='2011' & input.clustnum=='Broad'",
                            absolutePanel(height='250px', width='400px',
                                          navlistPanel(
                                            tabPanel("All types", value=0, icon=icon("users")),
                                            tabPanel(h4("Secure"), value=1, icon=icon("user")),
                                            tabPanel(h4("Insecure"), value=2, icon=icon("user")),
                                            widths=c(12,12),
                                            id='hh11_two'
                                            #id='hhclust'
                                          ),
                                          p("Clustering households according to questions related to financial attitude reveals the above household types",style="font-weight:bold; font-style:italic;")
                            )
           ),
           
           #conditionalPanel(condition="input.year=='2011' & input.clusttwo > input.clustfour",
           conditionalPanel(condition="input.year=='2011' & input.clustnum=='Detailed'",
                            absolutePanel(height='250px', width='400px',
                                          navlistPanel(
                                            tabPanel("All types", value=0, icon=icon("users")),
                                            tabPanel("Highly Secure", value=1, icon=icon("user")),
                                            tabPanel("Secure but worried", value=2, icon=icon("user")),
                                            tabPanel("Struggling to keep up", value=3, icon=icon("user")),
                                            tabPanel("Falling behind", value=4, icon=icon("user")),
                                            widths=c(12,12),
                                            id='hh11_four'
                                            #id='hhclust'
                                          ),
                                          p("Clustering households according to questions related to financial attitude reveals the above household types",style="font-weight:bold; font-style:italic;")
                            )
           )     
    ),
    
    column(4,
           conditionalPanel(condition="input.mainpanelState==1",
                            absolutePanel(height='250px', width='400px',
                                          navlistPanel(
                                            tabPanel("Income", value="fihhyr_a"),
                                            tabPanel("Disposable income", value="dfihhyr_a"),
                                            tabPanel("Unsecured debt", value="ustot_a"),
                                            widths=c(12,12),
                                            id="finance"
                                          ),
                                          p("What is the financial position of this household type?",style="font-weight:bold; font-style:italic;")
                            )
           ),
           conditionalPanel(condition="input.mainpanelState==2",
                            absolutePanel(height='250px', width='400px',
                                          navlistPanel(
                                            tabPanel("Family", value=1),
                                            tabPanel("Location", value=2),
                                            tabPanel("Blah", value=3),
                                            tabPanel("Blahblah", value=4),
                                            widths=c(12,12),
                                            id='demog'
                                            ),
                                          p("What are the demographics of this household type?",style="font-weight:bold; font-style:italic;")
                            )
           )
    )
  ),
    
  mainPanel(
    width=12,
    tabsetPanel(
      tabPanel(
        tags$h5("Financial position and attitudes"),
        fluidRow(
          column(7,
                 plotOutput("mdsplot", height=800), textOutput("clust")
                 ),
          column(5,
                 plotOutput("attplot", height=260, width='90%'), tableOutput("summary") #textOutput("finance"),
          )
          ),
        value=1
        ),
      tabPanel(tags$h5("Demographics"), helpText('Demographics'), value=2),
      id="mainpanelState"
    )
  )
))
  
    
                                      
                    
  
  
    

                     
                     
    
    