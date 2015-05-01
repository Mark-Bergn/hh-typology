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

source('labels.R')

shinyUI(fluidPage(
  
  list(tags$head(HTML('<link rel="icon", href="bankofengland60.jpg", 
                                   type="image/jpg" />'))),
  titlePanel("", windowTitle = "Bank of England household survey"),
  # Application title
  headerPanel(fluidRow(
    column(1,
           tags$h1(
             tags$img(src="Bank_of_England.svg", width="70px")
           )
           ),
    column(11,
           tags$h1("Bank of England household survey - UK financial attitudes")
           )
  )
  ),
  
  # Sidebar with controls to select a dataset
  fluidRow(
    column(4,
           wellPanel(radioButtons("year", label=h4("Select a year of the Bank of England household survey"),
                                 choices=c("2011","2010","2009"), selected = "2011"),
                     #h5("Select the number of household typologies"),
                     #actionButton("clusttwo", label=h2("2")),
                     #actionButton("clustfour", label=h2("4")),
                     radioButtons("clustnum", label=tags$h4("Households can be partitioned into different groups based on financial attitudes. What is the level of partition that you wish to see?",
                                                            style="line-height=110%;"),
                                  choices=c("Broad","Detailed"), selected="Broad"),
                     style = "padding: 20px;"
           )
           ),
    
    column(4,
           #conditionalPanel(condition="input.year=='2009' & input.clusttwo > input.clustfour",
           conditionalPanel(condition="input.year=='2009' & input.clustnum=='Broad'",
                            #absolutePanel(height='250px', width='400px',
                                          navlistPanel(
                                            "Household types",
                                            tabPanel("All types", value=0, icon=icon("gears")),
                                            tabPanel("Secure households", value=1, icon=icon("gear")),
                                            tabPanel("Struggling households", value=2, icon=icon("gear")),
                                            widths=c(12,12),
                                            id='hh09_two'
                                            #id='hhclust'
                                            )#,
                                          #p("Clustering households according to questions related to financial attitude reveals the above household types",style="font-weight:bold; font-style:italic;")
                            #)
                            ),
                                                  
           #conditionalPanel(condition="input.year=='2009' & input.clusttwo <= input.clustfour",
           conditionalPanel(condition="input.year=='2009' & input.clustnum=='Detailed'",
                            #absolutePanel(height='250px', width='400px',
                                          navlistPanel(
                                            "Household types",
                                            tabPanel("All types", value=0, icon = icon("gears")),
                                            tabPanel("Highly secure", value=1, icon = icon("gear")),
                                            tabPanel("Cautiously secure", value=2, icon = icon("gear")),
                                            tabPanel("Struggling but managing", value=3, icon = icon("gear")),
                                            tabPanel("Falling behind", value=4, icon = icon("gear")),
                                            widths=c(12,12),
                                            id='hh09_four'
                                            #id='hhclust'
                                          )#,
                                          #p("Clustering households according to questions related to financial attitude reveals the above household types",style="font-weight:bold; font-style:italic;")
                            #)
                            ),
           
           #conditionalPanel(condition="input.year=='2010' & input.clusttwo > input.clustfour",
           conditionalPanel(condition="input.year=='2010' & input.clustnum=='Broad'",
                            #absolutePanel(height='250px', width='400px',
                                          navlistPanel(
                                            "Household types",
                                            tabPanel("All types", value=0, icon=icon("gears")),
                                            tabPanel("Secure households", value=1, icon=icon("gear")),
                                            tabPanel("Struggling households", value=2, icon=icon("gear")),
                                            widths=c(12,12),
                                            id='hh10_two'
                                            #id='hhclust'
                                          )#,
                                          #p("Clustering households according to questions related to financial attitude reveals the above household types",style="font-weight:bold; font-style:italic;")
                            #)
           ),
           
           #conditionalPanel(condition="input.year=='2010' & input.clusttwo > input.clustfour",
           conditionalPanel(condition="input.year=='2010' & input.clustnum=='Detailed'",
                            #absolutePanel(height='250px', width='400px',
                                          navlistPanel(
                                            "Household types",
                                            tabPanel("All types", value=0, icon=icon("gears")),
                                            helpText("There does not seem to be further than two levels of household types in 2010"),
                                            widths=c(12,12),
                                            id='hh10_four'
                                            #id='hhclust'
                                          )             
                            #)
           ),
           
           
           #conditionalPanel(condition="input.year=='2011' & input.clusttwo > input.clustfour",
           conditionalPanel(condition="input.year=='2011' & input.clustnum=='Broad'",
                            #absolutePanel(height='250px', width='400px',
                                          navlistPanel(
                                            "Household types",
                                            tabPanel("All types", value=0, icon=icon("gears")),
                                            tabPanel("Secure households", value=1, icon=icon("gear")),
                                            tabPanel("Struggling households", value=2, icon=icon("gear")),
                                            widths=c(12,12),
                                            id='hh11_two'
                                            #id='hhclust'
                                          )#,
                                          #p("Clustering households according to questions related to financial attitude reveals the above household types",style="font-weight:bold; font-style:italic;")
                            #)
           ),
           
           #conditionalPanel(condition="input.year=='2011' & input.clusttwo > input.clustfour",
           conditionalPanel(condition="input.year=='2011' & input.clustnum=='Detailed'",
                            #absolutePanel(height='250px', width='400px',
                                          navlistPanel(
                                            "Household types",
                                            tabPanel("All types", value=0, icon=icon("gears")),
                                            tabPanel("Highly Secure", value=1, icon=icon("gear")),
                                            tabPanel("Secure but worried", value=2, icon=icon("gear")),
                                            tabPanel("Struggling to keep up", value=3, icon=icon("gear")),
                                            tabPanel("Falling behind", value=4, icon=icon("gear")),
                                            widths=c(12,12),
                                            id='hh11_four'
                                            #id='hhclust'
                                          )#,
                                          #p("Clustering households according to questions related to financial attitude reveals the above household types",style="font-weight:bold; font-style:italic;")
                            #)
           )     
    ),
    
    column(4,
           conditionalPanel(condition="input.mainpanelState==1",
                            #absolutePanel(height='250px', width='400px',
                                          navlistPanel(
                                            "Financial position",
                                            tabPanel("Income", value="fihhyr_a", icon=icon("gbp")),
                                            tabPanel("Disposable income", value="dfihhyr_a", icon=icon("gbp")),
                                            tabPanel("Unsecured debt", value="ustot_a", icon=icon("credit-card")),
                                            widths=c(12,12),
                                            id="finance"
                                          )#,
                                          #p("What is the financial position of this household type?",style="font-weight:bold; font-style:italic;")
                            #)
           ),
           conditionalPanel(condition="input.mainpanelState==2",
                            #absolutePanel(height='250px', width='400px',
                                          navlistPanel(
                                            "Demographics",
                                            tabPanel("Individuals", value=1, icon=icon("user")),
                                            tabPanel("Households", value=2, icon=icon("users")),
                                            widths=c(12,12),
                                            id='demog'
                                            )#,
                                          #p("What are the demographics of this household type?",style="font-weight:bold; font-style:italic;")
                            #)
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
                 helpText(tags$h4('We used a technique called',
                                  a('Multidimensional Scaling', href="http://en.wikipedia.org/wiki/Multidimensional_scaling", target="_blank"),
                                  mdsexplain, style="line-height:150%;color:black;")), 
                 plotOutput("mdsplot", height="700px"), #textOutput("clust")
                 helpText(tags$h4(mdsexplain2, style="line-height:150%;color:black;"))
                 #tableOutput("summary")
                 #tags$head(tags$style("#mdstext{color: black;
                                 #font-size: 15px;
                                 #font-style: italic;
                                  #line-height:120%;
                                 #}"))
                 ),
          column(5,
                 plotOutput("attplot", height="900px", width='95%'), tableOutput("summary") #textOutput("finance"),
          )
          ),
        value=1
        ),
      tabPanel(
        tags$h5("Demographics"),
        fluidRow(
          column(7,
                 helpText(tags$h4(demogexplain, style="line-height:150%;color:black;")),
                 plotOutput("mapplot", height=600, width = '90%')
                 #tableOutput("table")
          ),
          column(5,
                 #helpText('Demographics'),
                 plotOutput("demplot", height=800, width = '95%')
                 #tableOutput("table")
                 )
          ),           
        value=2
        ),
      id="mainpanelState"
    )
  )
))
  
    
                                      
                    
  
  
    

                     
                     
    
    