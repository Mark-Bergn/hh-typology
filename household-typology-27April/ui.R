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
  
  # Application title
  headerPanel(fluidRow(
    column(1,
           tags$h1(
             tags$img(src="Bank_of_England.svg", width="90px")
           )
           ),
    column(11,
           tags$h1("Financial attitudes in UK households: the Bank of England survey")
           )
  )
  ),
  
  # Sidebar with controls to select a dataset
  fluidRow(
    column(4,
           wellPanel(radioButtons("year", label=h5("Select a year of the Bank of England household survey"),
                                 choices=c("2011","2010","2009"), selected = "2011"),
                     #h5("Select the number of household typologies"),
                     #actionButton("clusttwo", label=h2("2")),
                     #actionButton("clustfour", label=h2("4")),
                     radioButtons("clustnum", label=h5("Households can be partitioned into different groups based on financial attitudes. What is the level of partition that you wish to see?"),
                                  choices=c("Broad","Detailed"), selected="Broad"),
                     style = "height: 250px;"
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
                                            tabPanel("Conservative and secure", value=2, icon = icon("gear")),
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
                                            tabPanel("Families", value=2, icon=icon("users")),
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
                 helpText(tags$h4(mdsexplain, style="font-style:italic;line-height:150%;color:black;")),
                 #textOutput("explain",
                  #          tags$h4(tags$style("#explain{
                   #                            font-style:italic;line-height:150%;color:black;
                   #                            }"))
                    #        ),
                 plotOutput("mdsplot", height="800px")#, textOutput("clust")
                 ),
          column(5,
                 plotOutput("attplot", height="1000px", width='95%'), tableOutput("summary") #textOutput("finance"),
          )
          ),
        value=1
        ),
      tabPanel(
        tags$h5("Demographics"),
        fluidRow(
          column(7,
                 plotOutput("mapplot", height=800, width = '90%')
          ),
          column(5,
                 helpText('Demographics'),
                 plotOutput("demplot", height=800, width = '90%')
                 )
          ),           
        value=2
        ),
      id="mainpanelState"
    )
  )
))
  
    
                                      
                    
  
  
    

                     
                     
    
    