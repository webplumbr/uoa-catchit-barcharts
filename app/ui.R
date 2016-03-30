## UI
# setwd("/Users/Ollie/Documents/Uni/Auckland University/2015/Summer Scholarship/RShiny")

## Require package shiny
library(shiny)

## Require package shinysky
require(shinysky)

projectNames <- c("Ark", "D'Urville Island", "EICT", "Kaipupu", "Mataia", "MEACT", "MEG Coro Kiwi Project", "Okura Bush", "Tahi", "Te Henga", "Tuff Crater", "Whinray Reserve")

########## START UI ##########

shinyUI(fluidPage(

  tabsetPanel(selected = "Bar Chart Application",
    tabPanel("Bar Chart Application",

      ## Sidebar at top
      fluidRow(
               column(3,

               ## Select Area
               selectInput("selectArea",
                           label = h4("Select Area"),
                           choices = projectNames,
                           selected = "Ark"
               ),
               br(),
               br(),
               ## Submit button
               #actionButton("submit", label = "Load Graphics", styleclass = "success")
               uiOutput("submit"),
               br(),
               conditionalPanel(
                 condition = "input.submit > 0",
                 downloadButton('download', "Download Graphics", class = "mybutton"),
                 tags$head(tags$style(".mybutton{background-color:LightSkyBlue;} .skin-black .sidebar .mybutton{color: green;}") )
               )
        ),

        column(3,
               ## Select Subject ("subject")
               radioButtons("selectSubject",
                            label = h4("Colour Catches By:"),
                            choices = c("Species", "Bait", "Trap Type", "Line", "Person (trap checked)", "Person (trap set)")
               ),
               br(),
               ## Conditional panel for bait proportions
               #conditionalPanel(
              #   condition = "input.selectSubject != 'Species'",
              #   checkboxInput("colourProp",
              #                 label = "View overall data as proportions",
              #                 value = FALSE
              #   )
              # )
                checkboxInput("prop", "View data as percentages", value = FALSE)
        ),

        column(3,
               #h4("Specific plots:"),

              ## Select to view by person or line ("by")
              radioButtons("selectBy",
                           label = h4("Display a plot for each:"),
                           choices = c("Person (trap checked)", "Person (trap set)", "Line",  "Trap Type", "Bait", "Species", "No Selection")
              )
              ## Conditional panel for bait proportions
              #conditionalPanel(
              #  condition = "input.selectBy != 'Species' && input.selectBy != 'No Selection' && input.selectBy != 'Species' && input.selectSubject != 'Species'",
              #  checkboxInput("byProp",
              #                 label = "View individual data as proportions",
              #                 value = FALSE
              #   )
              # )
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
               busyIndicator("Please Wait", wait = 0),
               fluidRow(
                 column(10,
                        uiOutput("overallTitle", hover = "overallTitle_hover"),
                 #       uiOutput("overall_hoverinfo")
                        uiOutput("overallInfo")
                 )
               ),
               #uiOutput("overallTitle"),
               #uiOutput("overall_hoverinfo"),
               #verbatimTextOutput("overall_hoverinfo"),
               plotOutput("overall", hover = "overall_hover"),
               #uiOutput("individual_hoverinfo"),
               uiOutput("individualTitle"),
               uiOutput("stackedplot")
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
      ),

    ## Table panel
    tabPanel("Data Tables",
             #conditionalPanel(
            #   condition = "input.submit > 0",
            #   downloadButton('downloadTables', "Download Tables", class = "mybutton"),
            #   tags$head(tags$style(".mybutton{background-color:LightSkyBlue;} .skin-black .sidebar .mybutton{color: green;}") )
            # ),
             conditionalPanel(
               condition = "input.submit > 0",
               h4("To save, highlight the table(s) of interest and paste them into an Excel document.  You can then copy tables from Excel into Powerpoint or Word for presentations and reports.", style = "color:grey")
             ),
             HTML("<br><p>"),
             uiOutput("tableTitle1"),
             uiOutput("overallTable"),
             uiOutput("tableTitle2"),
             uiOutput("individualTables")
    ),

    ## Help manual panel
    tabPanel("Help",
             h2("Manual"),
             h4("Bar Chart Application"),
             p("The bar chart application allows users to generate customized graphics for the selected area.
               The bar charts will depict catch numbers for the selected project, which can be colour coded by a range of variables."),
             p("Two different types of bar chart will be produced when a user chooses to request graphics:"),
             p(strong("1."), "An overall bar chart"),
             p(strong("2."), "Individual bar chart(s)"),
             p("The default settings will display an overall bar chart depicting the total number of catches for each species caught within the selected timeframe.
               It will also display individual bar charts depicting the number of catches made by each volunteer."),
             p("The type of catch data that is displayed in the overall plot can be customized by changing the 'Colour catches by' option, while changing the 'Display a plot for each' option will affect what is displayed in the individual bar charts."),
             br(),
             p("To begin, select the", strong("'Bar Chart Application'"), " tab and choose the project you wish to view by selecting from the", strong("'Select Area'"), " options."),
             img(src = "SelectArea.png", width = 750),
             br(),
             br(),
             br(),
             p("Next, choose the type of data you wish to view by selecting from the", strong("'Colour catches by'"), " options.
              The selected data will be displayed in the overall bar chart."),
             img(src = "SelectSubject.png", width = 750),
             p(strong(em("Species")), " - displays the total number of catches made for each species"),
             p(strong(em("Bait")), " - displays the total number of catches made by each bait type"),
             p(strong(em("Trap Type")), " - displays the total number of catches made by each trap type"),
             p(strong(em("Line")), " - displays the total number of catches made by each line of traps"),
             p(strong(em("Person (trap checked)")), " - displays the total number of catches discovered by each individual"),
             p(strong(em("Person (trap set)")), " - displays the total number of successful traps set by each individual"),
             br(),
             p("Once you have selected how you wish to colour code your graphics, you can choose to only view specific elements of the data by selecting the options in the", strong("'Colour Options'"), " box.
               For example, the selections below would only produce bar charts including the species 'Ship Rat' and 'Mouse'."),
             img(src = "SelectColour.png", width = 750),
             br(),
             br(),
             br(),
             p("Next, choose how you would like to breakdown the overall data by selecting from the", strong("'Display a plot for each'"), " options.
               The selected data will be displayed in individual bar charts."),
             img(src = "SelectBy.png", width = 750),
             p(strong(em("Person (trap checked)")), " - breaks down the overall data by each individual who checked the traps"),
             p(strong(em("Person (trap set)")), " - breaks down the overall data by each individual who set the traps"),
             p(strong(em("Line")), " - breaks down the overall data by each line of traps"),
             p(strong(em("Trap Type")), " - breaks down the overall data by each trap type"),
             p(strong(em("Bait")), " - breaks down the overall data by each bait type"),
             p(strong(em("Species")), " - breaks down the overall data by each species"),
             p(strong(em("No Selection")), " - displays no individual plots"),
             br(),
             br(),
             p("Like the overall bar chart, you can choose the specific elements of the data you wish to produce individual bar charts for.
               This is done by selecing", strong("'List'"), " within the", strong("'Specific Plot Options'"), " and selecting only the elements you are interested in.
               For example, the selections below would only produce individual bar charts for 'Rob M' and 'Sonya'."),
             img(src = "SelectSubby.png", width = 750),
             br(),
             br(),
             p("Next, choose the timeframe you wish to view data for by selecting from the", strong("'Show catches for each'"), " option."),
             img(src = "SelectYear.png", width = 750),
             p(strong(em("Month in a selected year")), " - displays monthly data in a selected year"),
             p(strong(em("Month over all years")), " - displays monthly data for all years"),
             p(strong(em("Year")), " - displays yearly data"),
             br(),
             br(),
             p("Finally, choose whether to view the data as raw numbers or convert it into percentages."),
             img(src = "SelectProp.png", width = 750),
             br(),
             br(),
             p("Once you are happy with your selections click", strong("'Load Graphics'."), "After a few seconds your customized graphics should appear on screen!"),
             p("The below example produces an overall bar chart containing information about all species, while individual plots are only requested for Jonathan's catch data."),
             img(src = "FinalGraphics.png", width = 750),
             br(),
             br(),
             p("To download a PDF copy of the bar charts you have just produced, simply click the", strong("'Download Graphics'"), " button."),
             img(src = "DownloadGraphics.png", width = 750),
             br(),
             br(),
             h4("Data Tables"),
             p("The", strong("'Data Tables'"), " tab allows you to access the raw data in tabular form.
               These tables are used to create the graphics within the Bar Chart Application tab."),
             #p("Like the bar charts, a PDF copy of these tables is also available to download by clicking the", strong("'Download Tables'"), " button."),
             p("To save, highlight the table(s) of interest and paste them into an Excel document.  You can then copy tables from Excel into Powerpoint or Word for presentations and reports"),
             img(src = "DataTable.png", width = 750),
             br(),
             br(),
             h4("Acknowledgements"),
             p("This application was written and developed by Oliver Stevenson within the Department of Statistics at the University of Auckland."),
             p("A big thank you to Rachel Fewster and the team at CatchIT for their support and providing the data for this application.")
             #img(src = "uoa.png", width = 200, align = "middle")
    )
  )



  #imageOutput("gif")



))
