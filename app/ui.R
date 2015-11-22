## UI
# setwd("/Users/Ollie/Documents/Uni/Auckland University/2015/Summer Scholarship/RShiny")

## Require package shiny
library(shiny)

## Require package shinysky
require(shinysky)

########## START UI ##########

shinyUI(fluidPage(
  
  ## Sidebar at top
  fluidRow(
    column(3,
           
           ## Select Area
           selectInput("selectArea",
                       label = h4("Select Area"),
                       choices = c("Kaipupu", "Mataia", "MEG Coro Kiwi Project", "Okura Bush", "Te Henga"),
                       selected = "Kaipupu"
           ),
           br(),
           br(),
           ## Submit button
           actionButton("submit", label = "Load Graphics", styleclass = "success")
           #uiOutput("submit")
    ),
    
    column(3,
           ## Select Subject ("subject")
           radioButtons("selectSubject",
                        label = h4("Colour Catches By:"),
                        choices = c("Species", "Line", "Bait", "Trap Type", "Person")
           )
    ),
    
    column(3,
           #h4("Specific plots:"),
           
           ## Select to view by person or line ("by")
           radioButtons("selectBy",
                        label = h4("Display a plot for each:"),
                        choices = c("Person", "Line", "Species", "Bait", "Trap Type", "No Selection")
           )
    ),
    
    column(3,
           ## Select how to aggregate the data ("aggregate")
           radioButtons("selectAggregate",
                        label = h4("Show catches for each:"),
                        choices = c("Month in a selected year", "Month over all years", "Year")
           )
    ),
    
    ## Conditional dropdown
    #conditionalPanel(
    #  condition = "input.selectAggregate == 'Month'",
    #  numericInput("selectYear",
    #    label = "Select Year",
    #    value = 2015
    #  )
    #)
    
    column(3,  
           uiOutput("selectYear")
    )
  ),
  
  hr(),
  hr(),
  
  ## Main Panel
  fluidRow(
    column(4,
           uiOutput("colourtitle"),
           uiOutput("subject"),
           uiOutput("selectSpecies"),
           #actionButton("allColour", label = "Select All"),
           helpText("Click + Control: add/remove a selection"),
           helpText("Click + Shift: add/remove multiple selections"),
           uiOutput("breakdowntitle"),
           uiOutput("by"),
           uiOutput("subby")
           #actionButton("allSubby", label = "Select All")
           #conditionalPanel(
            # condition = "input.subby.length = 0",
             #actionButton("download", "Download Graphics")
           #)
    ),
    column(8,
           plotOutput("overall"),
           busyIndicator("Please Wait", wait = 0),
           uiOutput("stackedplot")
           #uiOutput("blah")
    )
    #column(6,
    #  ## Expand by input$selectBy checkbox
    #  uiOutput("expand")
    #),
    #column(6,
    #  uiOutput("breakdowntitle"),
    #  uiOutput("by"),
    #  uiOutput("subby")
    #),
    #column(3,
    #  uiOutput("submit")
    #)
  ),
  
  
  fluidRow(
    #column(5.2,
    #uiOutput("breakdowntitle"),
    #uiOutput("by")
    #),
    #column(5.2,
    #  br(),
    #  br(),
    #  br(),
    #  uiOutput("subby")
    #)
    
  )
  
  ## Submit button
  #uiOutput("submit")
  #br(),
  #uiOutput("wait")
  #uiOutput("download")
  #downloadButton('downloadData',
  #  "Download Graphics")
  
  
  #imageOutput("plot1.png"),
  #fluidRow(
  #  column(10,
  #    plotOutput("overall")
  #  )
  #),
  
  #fluidRow(
  #  column(10,
  #    uiOutput("stackedplot")
  #  )
  #)
  
  #imageOutput("gif")
  
  
  
))