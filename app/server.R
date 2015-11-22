## Server
## setwd("/Users/Ollie/Documents/Uni/Auckland University/2015/Summer Scholarship/RShiny")
## shinyapps::deployApp('graphics-final')
## shinysky::run.shinysky.example()
## To get shinysky
##if (require(devtools)) install.packages("devtools")#if not alrady installed
##devtools::install_github("AnalytixWare/ShinySky")
##library(shinysky)
##shinysky::run.shinysky.example()

## Require package shiny
require(shiny)

## Require package shinysky (colour actionButton)
require(shinysky)

## Require RColorBrewer
require(RColorBrewer)

## Require prettyR
require(prettyR)

## Source Rfunc.R
#source("RfuncShiny.R")

## Useful functions
## Create the trimming function (trims leading and trailling spaces)
trim.func <- function(x){
  gsub("^\\s+|\\s+$", "", x)
}

## Create colour schemes
colour.diff <- brewer.pal(9, "Pastel1")
colour.diff2 <- brewer.pal(8, "Pastel2")
colour.paired <- brewer.pal(12, "Paired")

########### SERVER START ########## 

shinyServer(
  function(input, output) {
    
    ## Reactive function of the data
    area.data <- reactive({
      
      ## Update when select area is updated
      input$selectArea
      
      isolate({
        print("Isolate ui.data Loading - Area")
      
        ## Area name.csv
        csvname <- paste(input$selectArea, ".csv", sep = "")
        
        ## Read the all-time data and get the calender years and months
        dat <- read.csv(csvname, as.is = TRUE)
        dat.ft <- ifelse(length(grep("/", dat$Date[1])) > 0, "%d/%m/%Y", "%d-%m-%Y")
        dat$Date <- as.Date(dat$Date, format = dat.ft)
        dat$calmonth <- as.numeric(format(as.Date(dat$Date, format = dat.ft), "%m"))
        dat$calmonthname <- format(as.Date(dat$Date, format = dat.ft), "%b")
        dat$calyear <- as.numeric(format(as.Date(dat$Date, format = dat.ft), "%Y"))
        
        ##### Prevcheck #####
        ## Remove traps that haven't been checked
        dat <- dat[dat$TrapChecked == "Y", ]
        
        ## Create julian dates
        dat$julian <- julian(as.Date(dat$Date, format = "%d-%m-%Y"), origin = as.Date("01-01-2000", format = "%d-%m-%Y"))
        
        ## Order by julian date so traps are in order
        dat <- dat[order(dat$julian), ]
        
        ## Define prevcheck, prevperson and prevrow as empty
        dat$prevcheck <- rep(0, nrow(dat))
        dat$prevbait <- rep(0, nrow(dat))
        dat$prevperson <- rep(0, nrow(dat))
        
        ## Give each trap check an index
        dat$index <- row(dat[1])
               
        ## Get unique species, names, lines and traps
        unique.species <- unique(dat$Species[dat$Species != ""])
        unique.names <- unique(dat$FirstName[dat$FirstName != ""])
        unique.lines <- unique(dat$Line[dat$Line != ""])
        unique.traps <- unique(dat$TrapName[dat$TrapName != ""])
        unique.bait <- unique(dat$Bait[dat$Bait != ""])
        unique.years <- unique(dat$calyear[dat$calyear != ""])
        
        ##### Prevcheck #####
        ## For each trap sort prevcheck
        for(i in 1:length(unique.traps)){
          
          ## Checks referring to the ith trap
          dat.traps <- dat[dat$TrapName == unique.traps[i], c("Species", "index", "prevcheck")]

          ## Assign prevcheck as the index of the previous check
          dat.traps$prevcheck <- c(dat.traps$index[1], dat.traps$index[-length(dat.traps$index)])
          
          ## Apply prevchecks to complete dataset 
          dat$prevcheck[dat.traps$index] <- dat.traps$prevcheck
        }
        ##### End of prevcheck ######
        
        ## Determining duplicates
        alias <- trim.func(unique.names)
        
        ## Determine which (if any) are duplicate First Names
        which.dups <- which(duplicated(alias) | rev(duplicated(rev(alias))))
        
        ## Add username to duplicates for dat$FirstName
        if(length(which.dups) != 0){
          alias[which.dups] <- paste(alias[which.dups], unique(dat$Username)[which.dups], sep= ", ")
          ## Assign each person their FirstName + Username
          for(i in 1:length(dat[, 1])){
            person.counter <- match(dat$FirstName[i], unique.names)
            dat$FirstName[i] <- alias[person.counter]
          }
        }
        
        ## Re-define unique.names and dat
        unique.names <- unique(dat$FirstName)
        
        ## Add previous bait and person to the data frame
        dat$prevbait <- dat$Bait[dat$prevcheck]
        
        dat$prevbait[dat$Species != ""] <- dat$Bait[dat$prevcheck[dat$Species != ""]]
        dat$prevperson[dat$Species != ""] <- dat$FirstName[dat$prevcheck[dat$Species != ""]]
        print(head(dat))
        
        ## Redefine datcatch
        datcatch <- dat[dat$Species != "", ]
        
        ## Reactive list to return
        list(dat = dat, datcatch = datcatch, unique.species = unique.species, unique.names = unique.names, unique.lines = unique.lines, unique.traps = unique.traps, unique.bait = unique.bait, unique.years = unique.years)
        
      })
    })
    
    
    ########## Reactive UI for TopBar selections
    
    ## Reactive year selections
    output$selectYear <- renderUI({
      
      ## Define the data as where a species was caught
      datcatch <- area.data()$datcatch
      
      ## Get the unique years available for the area and sort into decreasing order
      unique.years <- unique(datcatch$calyear)
      unique.years <- sort(unique.years, decreasing = TRUE)
      
      ## Conditional dropdown
      conditionalPanel(
        condition = "input.selectAggregate == 'Month in a selected year'",
        selectInput("selectYear",
                    label = "Select Year",
                    choices = c(unique.years)
        )
      )
      
    })
    
    
    ########## Reactive UI ##########
    
    ## Reactive colour title
    output$colourtitle <- renderUI({
      
      conditionalPanel(
        condition = "input.selectArea != ''",
        h3("Colour Options:")
      )
      
    })
    
    
    ## Reactive 'subject' checkbox
    output$subject <- renderUI({
      
      ## Define 'subject'
      subject <- input$selectSubject
      if(input$selectSubject == "Person"){
        subject <- "FirstName"
      }
      if(input$selectSubject == "Trap Type"){
        subject <- "TrapType"
      }
      if(input$selectSubject == "Bait"){
        subject <- "prevbait"
      }
      
      ## Define the data as where a species was caught
      datcatch <- area.data()$datcatch
      datcatch$subject <- datcatch[[subject]]
      
      ## Select the year (if necessary) - give an error if no catches in that year
      if(input$selectAggregate == "Month in a selected year"){
        datcatch <- datcatch[datcatch$calyear == input$selectYear, ]
        if(is.na(datcatch[1, 1]) == TRUE){
          return("No data for the area in selected year")
        }
      }
      
      ## Remove cases where subject has been left blank (namely for when 'Sex' is selected)
      ## Probably redundant now
      datcatch <- datcatch[datcatch$subject != "", ]
      
      ## Sort subject in descending order of total catches
      sort.table <- sort(table(datcatch$subject), decreasing = TRUE)
      
      ## Find unique subject
      unique.subject <- names(sort.table)
      
      print("List of Subjects Loading")
      
      ## Conditional scrolling selection
      selectInput('subject', 
                  label = h5(paste("Select ", input$selectSubject, ":", sep = "")),
                  choices = unique.subject,
                  selected = unique.subject,
                  multiple = TRUE, 
                  selectize = FALSE,
                  size = 5)
      
    })
    
    ## Select all colour by options
    #observe({
      ## Select all is clicked
    #  input$allColour
      ## All species are selected
    #  output$subject <- renderUI({
    #    selectInput("subject",
    #                label = h4("Select Species:"),
    #                choices = sort(c(area.data()$unique.species)),
    #                selected = c(area.data()$unique.species),
    #                multiple = TRUE,
    #                selectize = FALSE,
    #                size = 6
    #    )
    #  })
    #})
    
    
    ## Reactive breakdown title
    output$breakdowntitle <- renderUI({
      
      conditionalPanel(
        condition = "input.selectBy != 'No Selection' && input.selectArea != ''",
        h3("Specific plot options:")
      )
      
    })
    
    
    
    ## Reactive 'by' radio buttons
    output$by <- renderUI({
      
      if(input$selectBy == "No Selection"){
        return(NULL)
      }
      
      print("Select All/List")
      
      ## Define 'by'
      if(input$selectBy == "Person"){
        by.vec <- "People"
      }
      if(input$selectBy == "Line"){
        by.vec <- "Lines"
      }
      if(input$selectBy == "Species"){
        by.vec <- "Species"
      }
      if(input$selectBy == "Bait"){
        by.vec <- "Baits"
      }
      if(input$selectBy == "Trap Type"){
        by.vec <- "Trap Types"
      }
      
      ## 'by' dropdown
      conditionalPanel(
        condition = "input.selectBy != 'No Selection'",
        radioButtons("by",
                     label = h5(paste("Specify a subset of ", by.vec, " to view:", sep = "")),
                     choices = c(paste("All ", by.vec, sep = ""), paste("List ", by.vec, sep = ""))
        )
      )
      
    })
    
    
    ## Reactive 'by' checkbox
    isolate({
      
      ## Run when subject is changed
      input$subject
      
      output$subby <- renderUI({
        
        ## Gets rid of initial error message
        #if(is.null(input$subject)){
        #  return(NULL)
        #}
        
        print("List of SubBy Start...")
        
        ## Define 'subject' and 'by'
        subject <- input$selectSubject
        by.vec <- input$selectBy
        
        ## Correct for if subject is 'Person' or 'Trap Type' or 'Bait'
        if(input$selectSubject == "Person"){
          subject <- "FirstName"
        }
        if(input$selectSubject == "Trap Type"){
          subject <- "TrapType"
        }
        if(input$selectSubject == "Bait"){
          subject <- "prevbait"
        }
        
        ## Define the data as where a species was caught
        datcatch <- area.data()$datcatch
        datcatch$subject <- datcatch[[subject]]
        
        ## Select the year (if necessary) or return an error if no data in select year
        if(input$selectAggregate == "Month in a selected year"){ 
          datcatch <- datcatch[datcatch$calyear == input$selectYear, ]
          if(is.na(datcatch[1, 1]) == TRUE){
            return(NULL)
          }
        }
        
        ## Extract and trim unique First Names
        FirstName <- area.data()$unique.names
        alias <- trim.func(FirstName)
        
        ## Determine which (if any) are duplicate First Names
        which.dups <- which(duplicated(alias) | rev(duplicated(rev(alias))))
        
        ## Add username to duplicates
        if(length(which.dups) != 0){
          alias[which.dups] <- paste(alias[which.dups], unique(datcatch$Username)[which.dups], sep= ", ")
          print("DUPLICATES!")
          ## Assign each person their alias
          for(i in 1:length(datcatch[, 1])){
            person.counter <- match(datcatch$FirstName[i], FirstName)
            datcatch$alias[i] <- alias[person.counter]
          }
        }else{
          datcatch$alias <- datcatch$FirstName
        }
        
        ## Extract input$selectBy
        datcatch$by <- datcatch[[by.vec]]
        if(by.vec == "Person"){
          datcatch$by <- datcatch$alias
        }
        if(by.vec == "Trap Type"){
          datcatch$by <- datcatch$TrapType
        }
        if(by.vec == "Bait"){
          datcatch$by <- datcatch$prevbait
        }
        
        ## Remove cases where 'by' has been left blank (e.g. no line or person entered)
        datcatch <- datcatch[datcatch$by != "", ]
        
        ## Extract cases only regarding those specified in input$subject
        unique.subject <- input$subject
        
        ## Remove secondary errors
        #if(unique.subject[1] %in% unique(datcatch$subject) == FALSE){
        #  return(NULL)
        #}
        
        store.vec <- data.frame()
        for(i in 1:length(unique.subject)){
            datvec <- datcatch[datcatch$subject == unique.subject[i], ]
            store.vec <- rbind(store.vec, datvec)
        }
        datcatch <- store.vec
        
        ## Sort input$selectBy in descending order
        sort.table <- table(datcatch$by)
        sort.table <- sort(sort.table, decreasing = TRUE)
        
        ## Should be in descending order
        unique.by <- names(sort.table)
        
        ## Adapt 'Person' and Trap Type plural
        if(input$selectBy == "Person"){
          by.vec <- "People"
        }
        
        print("List of SubBy End...")
        
        ## Conditional scrolling selection
        conditionalPanel(
          condition = "input.selectBy != 'No Selection' && input.by == 'List People' || input.by == 'List Lines' || input.by == 'List Species' || input.by == 'List Baits' || input.by == 'List Trap Types'",
          selectInput("subby", 
                      label = h5(paste("Select subset of ", by.vec, ":", sep = "")),
                      choices = unique.by,
                      selected = unique.by,
                      multiple = TRUE, 
                      selectize = FALSE,
                      size = 10)
        )
        
      })
    })
    
    
    ## Help Text
    output$helptext <- renderUI({
      
      if(input$selectArea == ""){
        return(NULL)
      }
      
      if(input$selectBy == "No Selection"){
        return(NULL)
      }
      
      if(input$by == "All People" | input$by == "All Lines" | input$by == "All Species" | input$by == "All Trap Types" | input$by == "All Baits"){
        return(NULL)
      }
      
      #HTML("<font color=\'#CC0033\'><strong>Only the selected data will be plotted</strong></font><p>")
      
    })
    
    
    
    
    
    ## Load blank when Area is changed
    #observeEvent(input$selectArea, priority = 17, {
    observe({input$selectArea
      
      output$overall <- renderPlot({
        return(NULL)
      })
      output$stackedplot <- renderPlot({
        return(NULL)
      })
      print("Blank due to area change")
    })
    
    ## Load blank when 'colour by' is changed
    #observeEvent(input$selectSubject, priority = 16, {
    observe({input$selectSubject
      
      output$overall <- renderPlot({
        return(NULL)
      })
      output$stackedplot <- renderPlot({
        return(NULL)
      })
      print("Blank due to colour by change")
    })
    
    ## Load blank when 'breakdown by' is changed
    #observeEvent(input$selectBy, priority = 15, {
    observe({input$selectBy
    
      output$overall <- renderPlot({
        return(NULL)
      })
      output$stackedplot <- renderPlot({
        return(NULL)
      })
      print("Blank due to select by change")
    })
    
    ## Load blank when 'aggregate by' is changed
    observeEvent(input$selectAggregate, priority = 14, {
      output$overall <- renderPlot({
        return(NULL)
      })
      output$stackedplot <- renderPlot({
        return(NULL)
      })
      print("Blank due to aggregate by")
    })
    
    ## Load blank when 'select year' is changed
    observeEvent(input$selectYear, priority = 13, {
      output$overall <- renderPlot({
        return(NULL)
      })
      output$stackedplot <- renderPlot({
        return(NULL)
      })
      print("Blank due to year change")
    })
    
    ## Load blank when subject elements are changed
    observeEvent(input$subject, priority = 12, {
      output$overall <- renderPlot({
        return(NULL)
      })
      output$stackedplot <- renderPlot({
        return(NULL)
      })
    })
    
    ## Load blank when 'list/all' is changed
    #observeEvent(input$by, priority = 11, {
    #  
    #  output$overall <- renderPlot({
    #    return(NULL)
    #  })
    #  output$stackedplot <- renderPlot({
    #    return(NULL)
    #  })
    #})
    
    ## Load blank when "select by" is changed
    observeEvent(input$subby, priority = 10, {
      
      output$overall <- renderPlot({
        return(NULL)
      })
      output$stackedplot <- renderPlot({
        return(NULL)
      })
    })
    

    ########## START OF OVERALL PLOT ###########
    
    ## Generate code required to plot the Overall Bar Plot
    observeEvent(input$submit, priority = 9, {
    #observe({input$submit
    
      output$overall <- renderPlot({
      
        ## If no subject or no specific plot options are selected give error
        if(is.null(input$subject)){
          stop("No colour options selected")
        }
        if(input$selectBy != "No Selection" & is.null(input$subby)){
          stop("No specific plot options selected")
        }
        print("Overall pre-code is running")
        
        
        ## Set the raw data
        dat <- area.data()$dat
        
        ## Set the subject, year, aggregate
        subject <- input$selectSubject
        if(input$selectSubject == "Person"){
          subject <- "FirstName"
        }
        if(input$selectSubject == "Trap Type"){
          subject <- "TrapType"
        }
        if(input$selectSubject == "Bait"){
          subject <- "prevbait"
        }
        subject.elements <- input$subject
        year <- input$selectYear
        agg <- input$selectAggregate
        byvec <- input$subby
        
        ## Extract the calender month, month name, year and specified subject from the data
        datcatch <- area.data()$datcatch
        datcatch$subject <- datcatch[[subject]]
        
        ## Extract and trim unique First Names
        FirstName <- area.data()$unique.names
        alias <- trim.func(FirstName)
        
        ## Determine which (if any) are duplicates
        which.dups <- which(duplicated(alias) | rev(duplicated(rev(alias))))
        
        ## Add username to duplicates
        alias[which.dups] <- paste(alias[which.dups], unique(datcatch$Username)[which.dups], sep= ", ")
        
        ## Assign each person their alias
        for(i in 1:length(datcatch[, 1])){
          person.counter <- match(datcatch$FirstName[i], FirstName)
          datcatch$alias[i] <- alias[person.counter]
        }
        
        ## Extract a specific year of data (e.g. 2014) or years depending on aggregate
        if(agg == "Month in a selected year"){
          datYear <- datcatch[datcatch$calyear == year, ]
        }else{
          datYear <- datcatch
        }
        
        ## Extact cases from that year(s) regarding the subject where a species was caught by people in personvec/linevec
        datSubject <- datYear[datYear$subject != "", ]
        
        ## Select all cases if no 'by' selection is made
        if(input$selectBy == "No Selection"){
          datBy <- datSubject
        }
        
        ## Otherwise extract cases only regarding those specified in input$selectBy
        store.byvec <- data.frame()
        for(i in 1:length(byvec)){
          if(input$selectBy == "Person"){
            datBy <- datSubject[datSubject$alias == byvec[i], ]
          }
          if(input$selectBy == "Line"){
            datBy <- datSubject[datSubject$Line == byvec[i], ]
          }
          if(input$selectBy == "Species"){
            datBy <- datSubject[datSubject$Species == byvec[i], ]
          }
          if(input$selectBy == "Bait"){
            datBy <- datSubject[datSubject$Bait == byvec[i], ]
          }
          if(input$selectBy == "Trap Type"){
            datBy <- datSubject[datSubject$TrapType == byvec[i], ]
          }
          store.byvec <- rbind(store.byvec, datBy)
        }
        datCounts <- store.byvec
        
        ## Create a contingency table of counts of subject by month (including empty months) or by year if aggregate is year in decreasing popularity
        if(agg == "Month in a selected year"){
          monthnames <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
          datTable <- table(factor(datCounts$subject, levels = subject.elements), factor(datCounts$calmonthname, levels = monthnames))
          sumtable <- as.table(sort(rowSums(datTable), decreasing = TRUE))
          unique.subject <- rownames(sumtable)
          datTable <- table(factor(datCounts$subject, levels = subject.elements), factor(datCounts$calmonthname, levels = monthnames))
        }
        if(agg == "Month over all years"){
          monthnames <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
          datTable <- table(factor(datCounts$subject, levels = subject.elements), factor(datCounts$calmonthname, levels = monthnames))
          sumtable <- as.table(sort(rowSums(datTable), decreasing = TRUE))
          unique.subject <- rownames(sumtable)
          datTable <- table(factor(datCounts$subject, levels = subject.elements), factor(datCounts$calmonthname, levels = monthnames))
        }
        if(agg == "Year"){
          years <- area.data()$unique.years
          years <- sort(years)
          datTable <- table(factor(datCounts$subject, levels = subject.elements), factor(datCounts$calmonthname, levels = years))
          sumtable <- as.table(sort(rowSums(datTable), decreasing = TRUE))
          unique.subject <- rownames(sumtable)
          datTable <- table(factor(datCounts$subject, levels = subject.elements), factor(datCounts$calyear, levels = years))
        }
        
        
        print("Overall pre-code finished")
      
      
      
      ########## Plotting commands ############
      
      ## Plot the overall data and add non-dynamic labels
      #output$overall <- renderPlot({
        print("Overall is plotting")
        print(input$submit)
        
        ## Busy indicator
        if (input$submit == 0)
          return()
        #Sys.sleep(2)
        
        ## Alternate method
        layout(matrix(1:2, ncol = 2), widths = c(0.8, 0.2))
        par(mar = c(6, 4, 3, 0))
        
        ## Legend auto-cex
        min.cex <- 0.8
        max.cex <- 2
        legend.string <- c(input$subject)
        auto.cex <- max(min.cex, min(c(20/length(legend.string)), 20/nchar(legend.string)))
        auto.cex <- min(auto.cex, max.cex)
        
        ## Add colours if need more than 9
        if(length(legend.string) > 9){
          colour.diff <- colorRampPalette(brewer.pal(brewer.pal.info["Pastel1", "maxcolors"], "Pastel1"))(length(legend.string))
        }
        
        ## Hold until graphics are ready to be plotted
        dev.hold()
        barplot(datTable, beside = TRUE, col = colour.diff[1:length(rownames(datTable))])#, ylim = c(0, max(colSums(datYrtable)) + 0.1*(max(colSums(datYrtable)))))
        dev.flush()
        
        ## Non-dynamic labels
        title(ylab = "Number Caught", cex.lab = 1.2)
        
        ## Add dynamic titles
        if(agg == "Month in a selected year"){
          title(main = paste("Number of catches made by Month \n in ", dat$Area[1], " by ", input$selectSubject," (", year, ")", sep = ""), xlab = "Month", cex.main = 1.5, cex.lab = 1.2)
        }
        if(agg == "Month over all years"){
          title(main = paste("Number of all time catches made by Month \n in ", dat$Area[1], " by ", input$selectSubject, sep = ""), xlab = "Month (all time)", cex.main = 1.5, cex.lab = 1.2)
        }
        if(agg == "Year"){
          title(main = paste("Number of all time catches made by Year \n in ", dat$Area[1], " by ", input$selectSubject, sep = ""), xlab = "Year", cex.main = 1.5, cex.lab = 1.2)
        }
        
        ## Add dynamic legend
        par(mar=c(0, 0, 0, 0), mgp=c(0, 0, 0))
        plot(1, type="n", bty="n", xaxt="n", yaxt="n")
        legend("topleft", legend = legend.string, col = 1, pt.bg = colour.diff[1:length(input$subject)], pch = 22, cex = auto.cex*0.8, pt.cex = auto.cex*1.75, xpd = TRUE, inset = c(0, 0))
        #frame()
        #legend("topleft", legend = legend.string, col = 1, pt.bg = colour.diff[1:length(input$subject)], pch = 22, cex = auto.cex*0.75, pt.cex = auto.cex*1.75, xpd = TRUE, inset = c(-0.5, 0)) ##cex = 1.4, pt.cex = 2.25, inset = inset)
      })
    })
    
    ########## END OF OVERALL PLOT ##########
    
    
    
    
    
    
    ########## START OF STACKED SUBJECT PLOTS ##########
    
    ## Plot stacked plots of selected subject only when 'submit' is pressed while 'expand' is checked
    observeEvent(input$submit, priority = 8, {
    #observe({input$submit
        print("Stacked pre-code is running")
        
        ## If a 'by' is not selected do nothing
        if(input$selectBy == "No Selection"){
          return(NULL)
        }
        
        ## Give error if input$subby is empty
        if(length(input$subby) == 0 && input$selectBy != "No Selection"){
          return(paste("No ", input$selectBy, " selected to breakdown", sep = ""))
        }
        
        ## Set the raw data
        dat <- area.data()$dat
        
        ## Set the subject, year, aggregate
        subject <- input$selectSubject
        if(input$selectSubject == "Person"){
          subject <- "FirstName"
        }
        if(input$selectSubject == "Trap Type"){
          subject <- "TrapType"
        }
        if(input$selectSubject == "Bait"){
          subject <- "prevbait"
        }
        subject.elements <- input$subject
        year <- input$selectYear
        agg <- input$selectAggregate
        byvec <- input$subby
        select.by <- input$selectBy
        
        ## Extract the calender month, month name, year and specified subject from the data
        datcatch <- area.data()$datcatch
        datcatch$subject <- datcatch[[subject]]
        
        ## Extract and trim unique First Names
        FirstName <- area.data()$unique.names
        alias <- trim.func(FirstName)
        
        ## Determine which (if any) are duplicates
        which.dups <- which(duplicated(alias) | rev(duplicated(rev(alias))))
        
        if(length(which.dups) > 0 ){
          ## Add username to duplicates
          alias[which.dups] <- paste(alias[which.dups], unique(datcatch$Username)[which.dups], sep= ", ")
          
          ## Assign each person their alias
          for(i in 1:length(dat[, 1])){
            person.counter <- match(datcatch$FirstName[i], FirstName)
            datcatch$alias[i] <- alias[person.counter]
          }
          
        }else{
          datcatch$alias <- datcatch$FirstName
        }
        
        ## Extract a specific year of data (e.g. 2014) or years depending on aggregate
        if(agg == "Month in a selected year"){
          datYr <- datcatch[datcatch$calyear == year, ]
        }else{
          datYr <- datcatch
        }
        
        ## Extact cases from that year(s) regarding the subject where a species was caught by people in personvec/linevec
        datSubject <- datYr[datYr$subject != "", ]
        
        ## Extract cases only regarding those elements specified in "input$subby"
        store.byvec <- data.frame()
        
        for(i in 1:length(byvec)){
          if(input$selectBy == "Person"){
            datBy <- datSubject[datSubject$alias == byvec[i], ]
          }
          if(input$selectBy == "Line"){
            datBy <- datSubject[datSubject$Line == byvec[i], ]
          }
          if(input$selectBy == "Species"){
            datBy <- datSubject[datSubject$Species == byvec[i], ]
          }
          if(input$selectBy == "Bait"){
            datBy <- datSubject[datSubject$Bait == byvec[i], ]
          }
          if(input$selectBy == "Trap Type"){
            datBy <- datSubject[datSubject$TrapType == byvec[i], ]
          }
          store.byvec <- rbind(store.byvec, datBy)
        }
        datCounts <- store.byvec
        
        ## Create a contingency table of counts of subject by month (including empty months) or by year if aggregate is year
        if(agg == "Month in a selected year"){
          monthnames <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
          datTable <- table(factor(datCounts$subject, levels = subject.elements), factor(datCounts$calmonthname, levels = monthnames))
          sumtable <- as.table(sort(rowSums(datTable), decreasing = TRUE))
          unique.subject <- rownames(sumtable)
          datTable <- table(factor(datCounts$subject, levels = subject.elements), factor(datCounts$calmonthname, levels = monthnames))
        }
        if(agg == "Month over all years"){
          monthnames <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
          datTable <- table(factor(datCounts$subject, levels = subject.elements), factor(datCounts$calmonthname, levels = monthnames))
          sumtable <- as.table(sort(rowSums(datTable), decreasing = TRUE))
          unique.subject <- rownames(sumtable)
          datTable <- table(factor(datCounts$subject, levels = subject.elements), factor(datCounts$calmonthname, levels = monthnames))
        }
        if(agg == "Year"){
          years <- area.data()$unique.years
          years <- sort(years)
          datTable <- table(factor(datCounts$subject, levels = subject.elements), factor(datCounts$calmonthname, levels = years))
          sumtable <- as.table(sort(rowSums(datTable), decreasing = TRUE))
          unique.subject <- rownames(sumtable)
          datTable <- table(factor(datCounts$subject, levels = subject.elements), factor(datCounts$calyear, levels = years))
        }
        
        ## Individual "by" data - create a list of tables consisting of each element of "by"s catch information (includes all available subjects regardless if present or not)
        namesTables <- list()
        if(agg == "Month in a selected year" | agg == "Month over all years"){
          if(select.by == "Person"){
            for(i in 1:length(byvec)){
              namesTables[[byvec[i]]] = table(factor(datCounts[datCounts$alias == byvec[i], ]$subject, levels = subject.elements), factor(datCounts[datCounts$alias == byvec[i], ]$calmonthname, levels = monthnames))
            }
          }
          if(select.by == "Line"){
            for(i in 1:length(byvec)){
              namesTables[[byvec[i]]] = table(factor(datCounts[datCounts$Line == byvec[i], ]$subject, levels = subject.elements), factor(datCounts[datCounts$Line == byvec[i],]$calmonthname, levels = monthnames))
            }
          }
          if(select.by == "Species"){
            for(i in 1:length(byvec)){
              namesTables[[byvec[i]]] = table(factor(datCounts[datCounts$Species == byvec[i], ]$subject, levels = subject.elements), factor(datCounts[datCounts$Species == byvec[i],]$calmonthname, levels = monthnames))
            }
          }
          if(select.by == "Bait"){
            for(i in 1:length(byvec)){
              namesTables[[byvec[i]]] = table(factor(datCounts[datCounts$Bait == byvec[i], ]$subject, levels = subject.elements), factor(datCounts[datCounts$Bait == byvec[i],]$calmonthname, levels = monthnames))
            }
          }
          if(select.by == "Trap Type"){
            for(i in 1:length(byvec)){
              namesTables[[byvec[i]]] = table(factor(datCounts[datCounts$TrapType == byvec[i], ]$subject, levels = subject.elements), factor(datCounts[datCounts$TrapType == byvec[i],]$calmonthname, levels = monthnames))
            }
          }
        }
        if(agg == "Year"){
          if(select.by == "Person"){
            for(i in 1:length(byvec)){
              namesTables[[byvec[i]]] = table(factor(datCounts[datCounts$alias == byvec[i], ]$subject, levels = subject.elements), factor(datCounts[datCounts$alias == byvec[i], ]$calyear, levels = years))
            }
          }
          if(select.by == "Line"){
            for(i in 1:length(byvec)){
              namesTables[[byvec[i]]] = table(factor(datCounts[datCounts$Line == byvec[i], ]$subject, levels = subject.elements), factor(datCounts[datCounts$Line == byvec[i],]$calyear, levels = years))
            }
          }
          if(select.by == "Species"){
            for(i in 1:length(byvec)){
              namesTables[[byvec[i]]] = table(factor(datCounts[datCounts$Species == byvec[i], ]$subject, levels = subject.elements), factor(datCounts[datCounts$Species == byvec[i],]$calyear, levels = years))
            }
          }
          if(select.by == "Bait"){
            for(i in 1:length(byvec)){
              namesTables[[byvec[i]]] = table(factor(datCounts[datCounts$Bait == byvec[i], ]$subject, levels = subject.elements), factor(datCounts[datCounts$Bait == byvec[i],]$calyear, levels = years))
            }
          }
          if(select.by == "Trap Type"){
            for(i in 1:length(byvec)){
              namesTables[[byvec[i]]] = table(factor(datCounts[datCounts$TrapType == byvec[i], ]$subject, levels = subject.elements), factor(datCounts[datCounts$TrapType == byvec[i],]$calyear, levels = years))
            }
          }
        }

        
        print("Stacked pre-code is finished")
        ########### Plotting commands ###########
        
        ## Set plotlength for right number of outputs (# persons or # lines)
        plotlength <- length(input$subby)
        
        # Insert the right number of plot output objects into the web page
        output$stackedplot <- renderUI({
          
          plot_output_list <- lapply(1:plotlength, function(i) {
            plotname <- paste("plot", i, sep="")
            plotOutput(plotname)
          })
          
          # Convert the list to a tagList - this is necessary for the list of items
          # to display properly.
          do.call(tagList, plot_output_list)
        })
        
        
        ## Call renderPlot for each one. Plots are only actually generated when they
        ## are visible on the web page.
        
        for (i in 1:plotlength){
          # Need local so that each item gets its own number. Without it, the value
          # of i in the renderPlot() will be the same across all instances, because
          # of when the expression is evaluated.
          
          local({
            my_i <- i
            
            plotname <- paste("plot", my_i, sep = "")
            
            
            ## Code for individual graphs PER SUBJECT ELEMENT
            #overall.table.stacked <- rbind()
            #for(k in 1:length(personvec)){
            #  overall.table.stacked <- rbind(overall.table.stacked, namesYrtables[[personvec[k]]][i,])
            #}
            
            ## Call each plot
            output[[plotname]] <- renderPlot({
              
              ## Adjust the margins and clipping to allow legends outside plots
              #par(mar = c(5.2, 4.2, 4.2, 13.2), xpd = NA)
              
              ## Alternate method
              layout(matrix(1:2, ncol=2), widths=c(0.8, 0.2))
              par(mar=c(6, 4, 3, 0))
              
              ## Legend auto-cex
              min.cex <- 0.8
              max.cex <- 2
              legend.string <- c(input$subject)
              auto.cex <- max(min.cex, min(c(20/length(legend.string)), 20/nchar(legend.string)))
              auto.cex <- min(auto.cex, max.cex)
              
              ## Create a colour vector for the plots (paired colours)
              colour.diff <- brewer.pal(9, "Pastel1")
              colour.diff2 <- brewer.pal(8, "Pastel2")
              colour.paired <- brewer.pal(12, "Paired")
              
              if(length(legend.string) > 9){
                colour.diff <- colorRampPalette(brewer.pal(brewer.pal.info["Pastel1", "maxcolors"], "Pastel1"))(length(legend.string))
              }
              print("Stacked is plotting")
              
              #barplot(oliver, col = colour.diff[1:length(input$subject)], ylim = c(0, max(colSums(namesYrtables[[my_i]])) + 0.1*max(colSums(namesYrtables[[my_i]]))))
              
              ## Plot the stacked plot of individual/line data for each element of the subject
              dev.hold()
              barplot(namesTables[[my_i]], col = colour.diff[1:length(input$subject)], ylim = c(0, max(colSums(namesTables[[my_i]])) + 0.1*max(colSums(namesTables[[my_i]]))))
              #dev.flush()
              
              ## Add non-dynamic titles
              title(ylab = "Number Caught", cex.lab = 1.2)
              
              ## Add dynamic titles
              if(agg == "Month in a selected year"){
                if(select.by == "Person"){
                  title(paste("Individual catch data for ", input$subby[my_i], ": \n", input$selectSubject, " by Month in ", dat$Area[1], " (", year, ")", sep = ""), xlab = "Month", cex.main = 1.5, cex.lab = 1.2)
                  #title(paste("Individual catch data for ", input$subby[my_i], " by ", input$selectSubject, "\n by Month in ", dat$Area[1], " (", year, ")", sep = ""), xlab = "Month", cex.main = 1.5, cex.lab = 1.2)
                }
                if(select.by == "Line"){
                  title(paste("Line catch data for ", input$subby[my_i], ": \n", input$selectSubject, " by Month in ", dat$Area[1], " (", year, ")", sep = ""), xlab = "Month", cex.main = 1.5, cex.lab = 1.2)
                }
                if(select.by == "Species"){
                  title(paste("Catch data for ", input$subby[my_i], ": \n", input$selectSubject, " by Month in ", dat$Area[1], " (", year, ")", sep = ""), xlab = "Month", cex.main = 1.5, cex.lab = 1.2)
                }
                if(select.by == "Bait"){
                  title(paste("Bait catch data for ", input$subby[my_i], ": \n", input$selectSubject, " by Month in ", dat$Area[1], " (", year, ")", sep = ""), xlab = "Month", cex.main = 1.5, cex.lab = 1.2)
                }
                if(select.by == "Trap Type"){
                  title(paste("Trap catch data for ", input$subby[my_i], ": \n", input$selectSubject, " by Month in ", dat$Area[1], " (", year, ")", sep = ""), xlab = "Month", cex.main = 1.5, cex.lab = 1.2)
                }
              }
              if(agg == "Month over all years"){
                if(select.by == "Person"){
                  title(paste("Individual catch data for ", input$subby[my_i], ": \n", input$selectSubject, " by Month in ", dat$Area[1], sep = ""), xlab = "Month (all time)", cex.main = 1.5, cex.lab = 1.2)
                }
                if(select.by == "Line"){
                  title(paste("Line catch data for ", input$subby[my_i], ": \n", input$selectSubject, " by Month in ", dat$Area[1], sep = ""), xlab = "Month (all time)", cex.main = 1.5, cex.lab = 1.2)
                }
                if(select.by == "Species"){
                  title(paste("Catch data for ", input$subby[my_i], ": \n", input$selectSubject, " by Month in ", dat$Area[1], sep = ""), xlab = "Month (all time)", cex.main = 1.5, cex.lab = 1.2)
                }
                if(select.by == "Bait"){
                  title(paste("Bait catch data for ", input$subby[my_i], ": \n", input$selectSubject, " by Month in ", dat$Area[1], sep = ""), xlab = "Month (all time)", cex.main = 1.5, cex.lab = 1.2)
                }
                if(select.by == "Trap Type"){
                  title(paste("Trap catch data for ", input$subby[my_i], ": \n", input$selectSubject, " by Month in ", dat$Area[1], sep = ""), xlab = "Month (all time)", cex.main = 1.5, cex.lab = 1.2)
                }
              }
              if(agg == "Year"){
                if(select.by == "Person"){
                  title(paste("Individual all time catch data for ", input$subby[my_i], ": \n", input$selectSubject, " by Year in ", dat$Area[1], sep = ""), xlab = "Year", cex.main = 1.5, cex.lab = 1.2)
                }
                if(select.by == "Line"){
                  title(paste("Line catch data for ", input$subby[my_i], ": \n", input$selectSubject, " by Year in ", dat$Area[1], sep = ""), xlab = "Year", cex.main = 1.5, cex.lab = 1.2)
                }
                if(select.by == "Species"){
                  title(paste("Catch data for ", input$subby[my_i], ": \n", input$selectSubject, " by Year in ", dat$Area[1], sep = ""), xlab = "Year", cex.main = 1.5, cex.lab = 1.2)
                }
                if(select.by == "Bait"){
                  title(paste("Bait catch data for ", input$subby[my_i], ": \n", input$selectSubject, " by Year in ", dat$Area[1], sep = ""), xlab = "Year", cex.main = 1.5, cex.lab = 1.2)
                }
                if(select.by == "Trap Type"){
                  title(paste("Trap catch data for ", input$subby[my_i], ": \n", input$selectSubject, " by Year in ", dat$Area[1], sep = ""), xlab = "Year", cex.main = 1.5, cex.lab = 1.2)
                }
              }
              
              br()
              br()
              dev.flush()
              
              ## Add dynamic legend
              #frame()
              #legend("topleft", legend = legend.string, col = 1, pt.bg = colour.diff[1:length(input$subject)], pch = 22, cex = auto.cex*0.75, pt.cex = auto.cex*1.75, xpd = TRUE, inset = c(-0.5, 0)) ##cex = 1.4, pt.cex = 2.25, inset = inset)
              
              par(mar=c(0, 0, 0, 0), mgp=c(0, 0, 0))
              plot(1, type="n", bty="n", xaxt="n", yaxt="n")
              legend("topleft", legend = legend.string, col = 1, pt.bg = colour.diff[1:length(input$subject)], pch = 22, cex = auto.cex*0.8, pt.cex = auto.cex*1.75, xpd = TRUE, inset = c(0, 0))
              
            })
            
          
          })
        }
        
        ## End of Observes
      #}) 
    })
    
    ## Download Graphics
    observeEvent(input$download, {
      #isolate({
        
        print("downloading...")
      print(length(input$subby))
        
        ## Re-jig aggregate so it lines up with RfuncShiny.R
        if(input$selectAggregate == "Month in a selected year"){
          agg <- "MonthYear"
        }
        if(input$selectAggregate == "Month over all years"){
          agg <- "Month"
        }
        if(input$selectAggregate == "Year"){
          agg <- "Year"
        }
        
        speciesbyyear.func(csvname = paste(input$selectArea, ".csv", sep = ""), subject = input$selectSubject, select.by = input$selectBy, agg = agg, year = input$selectYear, subjectvec = input$subject, byvec = input$subby, file.type = "pdf", title = TRUE)
      #})
    })

  }
)
