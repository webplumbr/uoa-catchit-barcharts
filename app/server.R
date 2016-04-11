## Server
## setwd("/Users/Ollie/Documents/Uni/Auckland University/2016/Summer Scholarship/")
## shinyapps::deployApp('barchart-application')
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

## Require gplots for table downloads
require('gplots')

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

    ## Load Graphics button
    output$submit <- renderUI({
      actionButton("submit", label = "Load Graphics", styleclass = "success")
    })

    ## Reactive function of the specific area's daya
    area.data <- reactive({

      ## Update when area is changed
      input$selectArea
      isolate({
        ## **** Prasanna - add the correct path name here (including the trailing /) ****
        pathName <- "/srv/csv-downloads/" # e.g. pathName <- "/home/fewster/BARCHART/"
        ## Area name.csv
        ## Strip out spaces, add pathName, "AllTime" and "TrapChecks.csv" to create the CSV name:
        csvname <- paste0(pathName, "AllTime", gsub(pattern=" ", replacement="", x=input$selectArea), "TrapChecks.csv")
        ## *****
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

        ## Define prevcheck, prevperson, prevrow and nextcheck as empty
        dat$prevcheck <- rep(0, nrow(dat))
        dat$prevbait <- rep(0, nrow(dat))
        dat$prevperson <- rep(0, nrow(dat))
        #dat$nextcheck <- rep(0, nrow(dat)) # This is the month that this trap was NEXT checked and set

        ## Give each trap check an index
        dat$index <- row(dat[1])

        ## Get unique species, names, lines and traps
        unique.species <- unique(dat$Species[dat$Species != ""])
        unique.names <- unique(dat$FirstName[dat$FirstName != ""])
        unique.lines <- unique(dat$Line[dat$Line != ""])
        unique.traps <- unique(dat$TrapName[dat$TrapName != ""])
        unique.bait <- unique(dat$Bait[dat$Bait != ""])
        unique.years <- unique(dat$calyear[dat$calyear != ""])

        ## For each trap sort prevcheck
        for(i in 1:length(unique.traps)){

          ## Checks referring to the ith trap
          #dat.traps <- dat[dat$TrapName == unique.traps[i], c("Species", "calmonth", "calmonthname", "index", "prevcheck", "nextcheck")]
          dat.traps <- dat[dat$TrapName == unique.traps[i], c("Species", "calmonth", "calmonthname", "index", "prevcheck")]

          ## Assign prevcheck and prevset as the index/calmonthname of the previous check
          dat.traps$prevcheck <- c(dat.traps$index[1], dat.traps$index[-length(dat.traps$index)])
          dat.traps$prevset <- c(dat.traps$calmonthname[1], dat.traps$calmonthname[-length(dat.traps$calmonthname)])

          ## Assign nextcheck as the index/calmonthname of the next future check
          #dat.traps$nextcheck <- c(dat.traps$calmonthname[-1], "NA")

          ## Apply prevchecks and prevsets to complete dataset
          dat$prevcheck[dat.traps$index] <- dat.traps$prevcheck
          dat$nextcheck[dat.traps$index] <- dat.traps$nextcheck
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

        ## Add previous bait and person to the data frame (previous person is the person who set the checked trap)
        dat$prevbait <- dat$Bait[dat$prevcheck]
        dat$prevbait[dat$Species != ""] <- dat$Bait[dat$prevcheck[dat$Species != ""]]
        dat$prevperson <- dat$FirstName[dat$prevcheck]

        ## Redefine datcatch
        datcatch <- dat[dat$Species != "", ]

        ###### End of area.data manipulation ######

        ## Reactive list to return
        list(dat = dat, datcatch = datcatch, unique.species = unique.species, unique.names = unique.names, unique.lines = unique.lines, unique.traps = unique.traps, unique.bait = unique.bait, unique.years = unique.years)

      })
    })


    ######### Start of working data ###########
    ## Reactive function of the working data
    working.data <- reactive({

      ## Update when load graphics is clicked
      input$submit

      ## Isolate so only updates when Load is clicked
      isolate({

        ## Busy Indicator
        if (input$submit == 0)
          return()

        ## Set the subject, year, aggregate
        subject <- input$selectSubject
        if(input$selectSubject == "Person (trap checked)"){
          subject <- "FirstName"
        }
        if(input$selectSubject == "Person (trap s?cutet)"){
          subject <- "prevperson"
        }
        if(input$selectSubject == "Trap Type"){
          subject <- "TrapType"
        }
        if(input$selectSubject == "Bait"){
          subject <- "prevbait"
        }

        ## Set the by
        by <- input$selectBy
        if(input$selectBy == "Person (trap checked)"){
          by <- "FirstName"
        }
        if(input$selectBy == "Person (trap set)"){
          by <- "prevperson"
        }
        if(input$selectBy == "Trap Type"){
          by <- "TrapType"
        }
        if(input$selectBy == "Bait"){
          by <- "prevbait"
        }

        ## Currently selected options
        subject.elements <- input$subject
        year <- input$selectYear
        agg <- input$selectAggregate
        select.by <- input$selectBy
        byvec <- input$subby

        ## Load the data and add the subject
        ###### If viewing by proportions need to use the whole dataset ######
        dat <- area.data()$dat
        datcatch <- area.data()$datcatch
        datcatch$subject <- datcatch[[subject]]

        ## Select the appropriate date range
        if(agg == "Month in a selected year"){
          datDate <- datcatch[datcatch$calyear == input$selectYear, ]
        }else{
          datDate <- datcatch
        }

        ## 18/2/2016
        ###### Individual plot options ######
        ## This is a bit of a hack but should do the trick for the hover options + is a bit quicker than before anyway
        ## If you ever need to tidy this up, this code chunk determines the options for for output$subby, it could definitely be tidier...

        ## Create a seperate datcatch object for the individual options code so I'm not stuffing up any other code
        datcatch.by <- datDate

        ## Define by.vec for the individual options so not code clash
        by.vec <- select.by

        ## Extract and trim unique First Names
        #FirstName <- area.data()$unique.names
        #alias <- trim.func(FirstName)

        ## Extract input$selectBy
        datcatch.by$by <- datcatch.by[[by.vec]]
        if(by.vec == "Person (trap checked)"){
          datcatch.by$by <- datcatch.by$FirstName
        }
        if(by.vec == "Person (trap set)"){
          datcatch.by$by <- datcatch.by$prevperson
        }
        if(by.vec == "Trap Type"){
          datcatch.by$by <- datcatch.by$TrapType
        }
        if(by.vec == "Bait"){
          datcatch.by$by <- datcatch.by$prevbait
        }

        ## Remove cases where 'by' has been left blank (e.g. no line or person entered)
        datcatch.by <- datcatch.by[datcatch.by$by != "", ]

        ## Extract cases only regarding those specified in input$subject
        ## Again - this is a seperate object just for the individual options so it won't clash with any other code
        unique.subject <- input$subject

        store.vec <- data.frame()
        for(i in 1:length(unique.subject)){
          datvec <- datcatch.by[datcatch.by$subject == subject.elements[i], ]
          store.vec <- rbind(store.vec, datvec)
        }
        datcatch.by <- store.vec

        ## Sort input$selectBy in descending order
        sort.table <- table(datcatch.by$by)
        sort.table <- sort(sort.table, decreasing = TRUE)

        ## Should be in descending order
        unique.by <- names(sort.table)

        ## Adapt 'Person' and Trap Type plural
        if(input$selectBy == "Person (trap checked)" | input$selectBy == "Person (trap set)"){
          by.vec <- "People"
        }

        ## print("Individual options")

        ###### Individual plot options end ######


        ## Get the whole dataset to calculate proportions but remove the traps that have not yet been checked
        if(input$prop == TRUE){
          datProp <- area.data()$dat
          #datProp <- area.data()$dat[dat$nextcheck != "NA", ]
          datProp$subject <- datProp[[subject]]
          if(agg == "Month in a selected year"){
            datProp <- datProp[datProp$calyear == year, ]
            #datProp <- datProp[datProp$prevbait == byvec, ]
          }
        }


        ## Select all cases if no 'by' selection is made
        if(select.by == "No Selection"){
          datCounts <- datDate
        }else{
        ## Otherwise extract cases only regarding those specified in input$selectBy
          store.byvec <- data.frame()
          for(i in 1:length(byvec)){
            if(select.by == "Person (trap checked)"){
              datBy <- datDate[datDate$FirstName == byvec[i], ]
            }
            if(select.by == "Person (trap set)"){
              datBy <- datDate[datDate$prevperson == byvec[i], ]
            }
            if(select.by == "Line"){
              datBy <- datDate[datDate$Line == byvec[i], ]
            }
            if(select.by == "Species"){
              datBy <- datDate[datDate$Species == byvec[i], ]
            }
            if(select.by == "Bait"){
              datBy <- datDate[datDate$prevbait == byvec[i], ]
            }
            if(select.by == "Trap Type"){
              datBy <- datDate[datDate$TrapType == byvec[i], ]
            }
            store.byvec <- rbind(store.byvec, datBy)
          }
          datCounts <- store.byvec
        }

        ## All available information (used for hover information)
        subject.all <- unique(datDate$subject)
        by.all <- unique(datDate[[by]])

        ## print("Available options")

        ###### End of output$subby options #####
        project.area <- input$selectArea


        ## Create a contingency table of counts of subject by month (including empty months) or by year if aggregate is year in decreasing popularity
        if(agg == "Month in a selected year"){
          monthnames <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
          datTable <- table(factor(datCounts$subject, levels = subject.elements), factor(datCounts$calmonthname, levels = monthnames))
          sumtable <- as.table(sort(rowSums(datTable), decreasing = TRUE))
          unique.subject <- rownames(sumtable)
          datTable <- table(factor(datCounts$subject, levels = subject.elements), factor(datCounts$calmonthname, levels = monthnames))

          ## Put into proportions if species proportions are selected
          if(input$prop == TRUE & input$selectSubject == "Species"){
            ## Need a vector of 12 (one for each month) of the number of traps set in each month
            monthVec <- as.vector(table(factor(datProp$calmonthname, levels = monthnames)))
            monthMatrix <- matrix(rep(monthVec, times = length(subject.elements)), ncol = 12, byrow = TRUE)

            ## Divide datTable by this vector to get the proportion of species caught
            datTable <- datTable/monthMatrix*100
          }
          ## Put into proportions if bait proportions are selected
          if(input$prop == TRUE & input$selectSubject == "Bait"){
            datTable <- datTable/table(factor(datProp$prevbait, levels = subject.elements), factor(datProp$calmonthname, levels = monthnames))*100
          }
          ## Put into proportions if TrapType proportions are selected
          if(input$prop == TRUE & input$selectSubject == "Trap Type"){
            datTable <- datTable/table(factor(datProp$TrapType, levels = subject.elements), factor(datProp$calmonthname, levels = monthnames))*100
          }
          ## Put into proportions if Line proportions are selected
          if(input$prop == TRUE & input$selectSubject == "Line"){
            datTable <- datTable/table(factor(datProp$Line, levels = subject.elements), factor(datProp$calmonthname, levels = monthnames))*100
          }
          ## Put into proportions if Person proportions are selected (catches discovered by person)
          if(input$prop == TRUE & input$selectSubject == "Person (trap checked)"){
            datTable <- datTable/table(factor(datProp$FirstName, levels = subject.elements), factor(datProp$calmonthname, levels = monthnames))*100
          }
          ## Put into proportions if Previous Person proportions are selected (traps set by person)
          if(input$prop == TRUE & input$selectSubject == "Person (trap set)"){
            datTable <- datTable/table(factor(datProp$prevperson, levels = subject.elements), factor(datProp$calmonthname, levels = monthnames))*100
          }
        }
        if(agg == "Month over all years"){
          monthnames <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
          datTable <- table(factor(datCounts$subject, levels = subject.elements), factor(datCounts$calmonthname, levels = monthnames))
          sumtable <- as.table(sort(rowSums(datTable), decreasing = TRUE))
          unique.subject <- rownames(sumtable)
          datTable <- table(factor(datCounts$subject, levels = subject.elements), factor(datCounts$calmonthname, levels = monthnames))

          ## Put into proportions if species proportions are selected
          if(input$prop == TRUE & input$selectSubject == "Species"){
            ## Need a vector of 12 (one for each month) of the number of traps set in each month
            monthVec <- as.vector(table(factor(datProp$calmonthname, levels = monthnames)))
            monthMatrix <- matrix(rep(monthVec, times = length(subject.elements)), ncol = 12, byrow = TRUE)

            ## Divide datTable by this vector to get the proportion of species caught
            datTable <- datTable/monthMatrix*100
          }
          ## Put into proportions if bait proportions are selected
          if(input$prop == TRUE & input$selectSubject == "Bait"){
            datTable <- datTable/table(factor(datProp$prevbait, levels = subject.elements), factor(datProp$calmonthname, levels = monthnames))*100
          }
          ## Put into proportions if TrapType proportions are selected
          if(input$prop == TRUE & input$selectSubject == "Trap Type"){
            datTable <- datTable/table(factor(datProp$TrapType, levels = subject.elements), factor(datProp$calmonthname, levels = monthnames))*100
          }
          ## Put into proportions if Line proportions are selected
          if(input$prop == TRUE & input$selectSubject == "Line"){
            datTable <- datTable/table(factor(datProp$Line, levels = subject.elements), factor(datProp$calmonthname, levels = monthnames))*100
          }
          ## Put into proportions if Person proportions are selected (catches discovered by person)
          if(input$prop == TRUE & input$selectSubject == "Person (trap checked)"){
            datTable <- datTable/table(factor(datProp$FirstName, levels = subject.elements), factor(datProp$calmonthname, levels = monthnames))*100
          }
          ## Put into proportions if Previous Person proportions are selected (traps set by person)
          if(input$prop == TRUE & input$selectSubject == "Person (trap set)"){
            datTable <- datTable/table(factor(datProp$prevperson, levels = subject.elements), factor(datProp$calmonthname, levels = monthnames))*100
          }
        }
        if(agg == "Year"){
          years <- area.data()$unique.years
          years <- sort(years)
          datTable <- table(factor(datCounts$subject, levels = subject.elements), factor(datCounts$calmonthname, levels = years))
          sumtable <- as.table(sort(rowSums(datTable), decreasing = TRUE))
          unique.subject <- rownames(sumtable)
          datTable <- table(factor(datCounts$subject, levels = subject.elements), factor(datCounts$calyear, levels = years))

          ## Put into proportions if species proportions are selected
          if(input$prop == TRUE & input$selectSubject == "Species"){
            ## Need a vector of 12 (one for each month) of the number of traps set in each month
            yearVec <- as.vector(table(factor(datProp$calyear, levels = years)))
            yearMatrix <- matrix(rep(yearVec, times = length(subject.elements)), ncol = length(years), byrow = TRUE)

            ## Divide datTable by this vector to get the proportion of species caught
            datTable <- datTable/yearMatrix*100
          }

          ## Put into proportions if bait proportions are selected
          if(input$prop == TRUE & input$selectSubject == "Bait"){
            datTable <- datTable/table(factor(datProp$prevbait, levels = subject.elements), factor(datProp$calyear, levels = years))*100
          }
          ## Put into proportions if TrapType proportions are selected
          if(input$prop == TRUE & input$selectSubject == "Trap Type"){
            datTable <- datTable/table(factor(datProp$TrapType, levels = subject.elements), factor(datProp$calyear, levels = years))*100
          }
          ## Put into proportions if Line proportions are selected
          if(input$prop == TRUE & input$selectSubject == "Line"){
            datTable <- datTable/table(factor(datProp$Line, levels = subject.elements), factor(datProp$calyear, levels = years))*100
          }
          ## Put into proportions if Person proportions are selected (catches discovered by person)
          if(input$prop == TRUE & input$selectSubject == "Person (trap checked)"){
            datTable <- datTable/table(factor(datProp$FirstName, levels = subject.elements), factor(datProp$calyear, levels = years))*100
          }
          ## Put into proportions if Previous Person proportions are selected (traps set by person)
          if(input$prop == TRUE & input$selectSubject == "Person (trap set)"){
            datTable <- datTable/table(factor(datProp$prevperson, levels = subject.elements), factor(datProp$calyear, levels = years))*100
          }
        }
        ## print(datTable)

        ## The number of times a bait has been set
        #if(input$prop == TRUE & input$selectSubject == "Bait"){
        #  freqs <- as.numeric(table(factor(datProp$prevbait, levels = subject.elements), factor(datProp$calmonthname, levels = monthnames)))
        #  freqx <- seq(from = 1, to = length(freqs)) + 0.5
        #  freqy <- as.numeric(datTable) ########## Last Code ##########
        #}else{
          freqs <- NULL
          freqx <- NULL
          freqy <- NULL
        #}


        ## Individual "by" data - create a list of tables consisting of each element of "by"s catch information (includes all available subjects regardless if present or not)
        namesTables <- list()
        ## If plotting by month in a year or all years
        if(agg == "Month in a selected year" | agg == "Month over all years"){
          if(select.by == "Person (trap checked)"){
            for(i in 1:length(byvec)){
              ## Put into proportions if individual proportions are selected
              if(input$prop == TRUE & input$selectBy == "Person (trap checked)" & input$selectSubject != "Species"){
                ## Determine the dataset that needs to be divided by to get proportion data
                datByprop <- datProp[datProp$FirstName == byvec[i], ]

                ## Divide the i'th individual table by the above dataset
                namesTables[[byvec[i]]] = table(factor(datCounts[datCounts$FirstName == byvec[i], ]$subject, levels = subject.elements), factor(datCounts[datCounts$FirstName == byvec[i],]$calmonthname, levels = monthnames))/table(factor(datByprop$subject, levels = subject.elements), factor(datByprop$calmonthname, levels = monthnames))*100

              }else{
                ## If species is the subject
                if(input$prop == TRUE & input$selectBy == "Person (trap checked)" & input$selectSubject == "Species"){
                  # Determine the dataset that needs to be divided by to get proportion data
                  datByprop <- datProp[datProp$FirstName == byvec[i], ]

                  ## Need a vector of 12 (one for each month) of the number of traps set in each month
                  monthVec <- as.vector(table(factor(datByprop$calmonthname, levels = monthnames)))
                  monthMatrix <- matrix(rep(monthVec, times = length(subject.elements)), ncol = 12, byrow = TRUE)

                  ## Divide the i'th individual table by the above dataset
                  namesTables[[byvec[i]]] = table(factor(datCounts[datCounts$FirstName == byvec[i], ]$subject, levels = subject.elements), factor(datCounts[datCounts$FirstName == byvec[i], ]$calmonthname, levels = monthnames))/monthMatrix*100
                }else{
                  ## If individual proportions are not selected
                  namesTables[[byvec[i]]] = table(factor(datCounts[datCounts$FirstName == byvec[i], ]$subject, levels = subject.elements), factor(datCounts[datCounts$FirstName == byvec[i], ]$calmonthname, levels = monthnames))
                }
              }
            }
          }
          if(select.by == "Person (trap set)"){
            for(i in 1:length(byvec)){
              ## Put into proportions if individual proportions are selected
              if(input$prop == TRUE & input$selectBy == "Person (trap set)" & input$selectSubject != "Species"){
                ## Determine the dataset that needs to be divided by to get proportion data
                datByprop <- datProp[datProp$prevperson == byvec[i], ]

                ## Divide the i'th individual table by the above dataset
                namesTables[[byvec[i]]] = table(factor(datCounts[datCounts$prevperson == byvec[i], ]$subject, levels = subject.elements), factor(datCounts[datCounts$prevperson == byvec[i], ]$calmonthname, levels = monthnames))/table(factor(datByprop$subject, levels = subject.elements), factor(datByprop$calmonthname, levels = monthnames))*100
                #print(byvec[i])
                #print(table(factor(datCounts[datCounts$prevperson == byvec[i], ]$subject, levels = subject.elements), factor(datCounts[datCounts$prevperson == byvec[i], ]$calmonthname, levels = monthnames)))
                #print(table(factor(datByprop$subject, levels = subject.elements), factor(datByprop$calmonthname, levels = monthnames)))
                #print(namesTables[[i]])
              }else{
                ## If species is the subject
                if(input$prop == TRUE & input$selectBy == "Person (trap set)" & input$selectSubject == "Species"){
                  # Determine the dataset that needs to be divided by to get proportion data
                  datByprop <- datProp[datProp$prevperson == byvec[i], ]

                  ## Need a vector of 12 (one for each month) of the number of traps set in each month
                  monthVec <- as.vector(table(factor(datByprop$calmonthname, levels = monthnames)))
                  monthMatrix <- matrix(rep(monthVec, times = length(subject.elements)), ncol = 12, byrow = TRUE)

                  ## Divide the i'th individual table by the above dataset
                  namesTables[[byvec[i]]] = table(factor(datCounts[datCounts$prevperson == byvec[i], ]$subject, levels = subject.elements), factor(datCounts[datCounts$prevperson == byvec[i], ]$calmonthname, levels = monthnames))/monthMatrix*100

                }else{
                  ## If individual proportions are not selected
                  namesTables[[byvec[i]]] = table(factor(datCounts[datCounts$prevperson == byvec[i], ]$subject, levels = subject.elements), factor(datCounts[datCounts$prevperson == byvec[i], ]$calmonthname, levels = monthnames))
                }
              }
            }
          }
          if(select.by == "Line"){
            for(i in 1:length(byvec)){
              ## Put into proportions if individual proportions are selected
              if(input$prop == TRUE & input$selectBy == "Line" & input$selectSubject != "Species"){
                ## Determine the dataset that needs to be divided by to get proportion data
                datByprop <- datProp[datProp$Line == byvec[i], ]

                ## Divide the i'th individual table by the above dataset
                namesTables[[byvec[i]]] = table(factor(datCounts[datCounts$Line == byvec[i], ]$subject, levels = subject.elements), factor(datCounts[datCounts$Line == byvec[i],]$calmonthname, levels = monthnames))/table(factor(datByprop$subject, levels = subject.elements), factor(datByprop$calmonthname, levels = monthnames))*100

              }else{
                ## If species is the subject
                if(input$prop == TRUE & input$selectBy == "Line" & input$selectSubject == "Species"){
                  ## Determine the dataset that needs to be divided by to get proportion data
                  datByprop <- datProp[datProp$Line == byvec[i], ]

                  ## Need a vector of 12 (one for each month) of the number of traps set in each month
                  monthVec <- as.vector(table(factor(datByprop$calmonthname, levels = monthnames)))
                  monthMatrix <- matrix(rep(monthVec, times = length(subject.elements)), ncol = 12, byrow = TRUE)

                  ## Divide the i'th individual table by the above dataset
                  namesTables[[byvec[i]]] = table(factor(datCounts[datCounts$Line == byvec[i], ]$subject, levels = subject.elements), factor(datCounts[datCounts$Line == byvec[i], ]$calmonthname, levels = monthnames))/monthMatrix*100

                }else{
                  ## If individual proportions are not selected
                  namesTables[[byvec[i]]] = table(factor(datCounts[datCounts$Line == byvec[i], ]$subject, levels = subject.elements), factor(datCounts[datCounts$Line == byvec[i],]$calmonthname, levels = monthnames))
                }
              }
            }
          }
          if(select.by == "Species"){
            ## Put into proportions if individual proportions are selected
            for(i in 1:length(byvec)){
              if(input$prop == TRUE & input$selectBy == "Species"){
                ## Determine the dataset that needs to be divided by to get proportion data
                datByprop <- datProp[datProp$Species == byvec[i], ]

                ## Need a vector of 12 (one for each month) of the number of traps set in each month
                ## Unlike other options - for species we have to divide by the number of traps in each month across all elements of species because we do not know the number of times a species attempted to take a bait
                monthVec <- as.vector(table(factor(dat$calmonthname, levels = monthnames)))
                monthMatrix <- matrix(rep(monthVec, times = length(subject.elements)), ncol = 12, byrow = TRUE)

                ## Divide the i'th individual table by the above dataset
                namesTables[[byvec[i]]] = table(factor(datCounts[datCounts$Species == byvec[i], ]$subject, levels = subject.elements), factor(datCounts[datCounts$Species == byvec[i], ]$calmonthname, levels = monthnames))/monthMatrix*100
              }else{
                ## If individual proportions are not selected
                namesTables[[byvec[i]]] = table(factor(datCounts[datCounts$Species == byvec[i], ]$subject, levels = subject.elements), factor(datCounts[datCounts$Species == byvec[i],]$calmonthname, levels = monthnames))
              }
            }
          }
          if(select.by == "Bait"){
            for(i in 1:length(byvec)){
              ## Put into proportions if individual proportions are selected
              if(input$prop == TRUE & input$selectBy == "Bait" & input$selectSubject != "Species"){
                ## Determine the dataset that needs to be divided by to get proportion data
                datByprop <- datProp[datProp$prevbait == byvec[i], ]

                ## Divide the i'th individual table by the above dataset
                namesTables[[byvec[i]]] = table(factor(datCounts[datCounts$prevbait == byvec[i], ]$subject, levels = subject.elements), factor(datCounts[datCounts$prevbait == byvec[i], ]$calmonthname, levels = monthnames))/table(factor(datByprop$subject, levels = subject.elements), factor(datByprop$calmonthname, levels = monthnames))*100

              }else{
                ## If species is the subject
                if(input$prop == TRUE & input$selectBy == "Bait" & input$selectSubject == "Species"){
                  # Determine the dataset that needs to be divided by to get proportion data
                  datByprop <- datProp[datProp$prevbait == byvec[i], ]

                  ## Need a vector of 12 (one for each month) of the number of traps set in each month
                  monthVec <- as.vector(table(factor(datByprop$calmonthname, levels = monthnames)))
                  monthMatrix <- matrix(rep(monthVec, times = length(subject.elements)), ncol = 12, byrow = TRUE)

                  ## Divide the i'th individual table by the above dataset
                  namesTables[[byvec[i]]] = table(factor(datCounts[datCounts$prevbait == byvec[i], ]$subject, levels = subject.elements), factor(datCounts[datCounts$prevbait == byvec[i], ]$calmonthname, levels = monthnames))/monthMatrix*100


                }else{
                  ## If individual proportions are not selected
                  namesTables[[byvec[i]]] = table(factor(datCounts[datCounts$prevbait == byvec[i], ]$subject, levels = subject.elements), factor(datCounts[datCounts$prevbait == byvec[i], ]$calmonthname, levels = monthnames))
                }
              }
            }
          }
          if(select.by == "Trap Type"){
            for(i in 1:length(byvec)){
              ## Put into proportions if individual proportions are selected
              if(input$prop == TRUE & input$selectBy == "Trap Type" & input$selectSubject != "Species"){
                ## Determine the dataset that needs to be divided by to get proportion data
                datByprop <- datProp[datProp$TrapType == byvec[i], ]

                ## Divide the i'th individual table by the above dataset
                namesTables[[byvec[i]]] = table(factor(datCounts[datCounts$TrapType == byvec[i], ]$subject, levels = subject.elements), factor(datCounts[datCounts$TrapType == byvec[i],]$calmonthname, levels = monthnames))/table(factor(datByprop$subject, levels = subject.elements), factor(datByprop$calmonthname, levels = monthnames))*100

              }else{
                ## If species is the subject
                if(input$prop == TRUE & input$selectBy == "Trap Type" & input$selectSubject == "Species"){
                  # Determine the dataset that needs to be divided by to get proportion data
                  datByprop <- datProp[datProp$TrapType == byvec[i], ]

                  ## Need a vector of 12 (one for each month) of the number of traps set in each month
                  monthVec <- as.vector(table(factor(datByprop$calmonthname, levels = monthnames)))
                  monthMatrix <- matrix(rep(monthVec, times = length(subject.elements)), ncol = 12, byrow = TRUE)

                  ## Divide the i'th individual table by the above dataset
                  namesTables[[byvec[i]]] = table(factor(datCounts[datCounts$TrapType == byvec[i], ]$subject, levels = subject.elements), factor(datCounts[datCounts$TrapType == byvec[i], ]$calmonthname, levels = monthnames))/monthMatrix*100

                }else{
                  ## If individual proportions are not selected
                  namesTables[[byvec[i]]] = table(factor(datCounts[datCounts$TrapType == byvec[i], ]$subject, levels = subject.elements), factor(datCounts[datCounts$TrapType == byvec[i],]$calmonthname, levels = monthnames))
                }
              }
            }
          }
        }
        ## If plotting by year
        if(agg == "Year"){
          if(select.by == "Person (trap checked)"){
            for(i in 1:length(byvec)){
              ## Put into proportions if individual proportions are selected
              if(input$prop == TRUE & input$selectBy == "Person (trap checked)" & input$selectSubject != "Species"){
                ## Determine the dataset that needs to be divided by to get proportion data
                datByprop <- datProp[datProp$FirstName == byvec[i], ]

                ## Divide the i'th individual table by the above dataset
                namesTables[[byvec[i]]] = table(factor(datCounts[datCounts$FirstName == byvec[i], ]$subject, levels = subject.elements), factor(datCounts[datCounts$FirstName == byvec[i],]$calyear, levels = years))/table(factor(datByprop$subject, levels = subject.elements), factor(datByprop$calyear, levels = years))*100

              }else{
                ## If species is the subject
                if(input$prop == TRUE & input$selectBy == "Person (trap checked)" & input$selectSubject == "Species"){
                  # Determine the dataset that needs to be divided by to get proportion data
                  datByprop <- datProp[datProp$FirstName == byvec[i], ]

                  ## Need a vector of the number of traps set in each year
                  yearVec <- as.vector(table(factor(datByprop$calyear, levels = years)))
                  yearMatrix <- matrix(rep(yearVec, times = length(subject.elements)), ncol = length(years), byrow = TRUE)

                  ## Divide the i'th individual table by the above dataset
                  namesTables[[byvec[i]]] = table(factor(datCounts[datCounts$FirstName == byvec[i], ]$subject, levels = subject.elements), factor(datCounts[datCounts$FirstName == byvec[i], ]$calyear, levels = years))/yearMatrix*100

                }else{
                  ## If individual proportions are not selected
                  namesTables[[byvec[i]]] = table(factor(datCounts[datCounts$FirstName == byvec[i], ]$subject, levels = subject.elements), factor(datCounts[datCounts$FirstName == byvec[i], ]$calyear, levels = years))
                }
              }
            }
          }
          if(select.by == "Person (trap set)" & input$selectSubject != "Species"){
            for(i in 1:length(byvec)){
              ## Put into proportions if individual proportions are selected
              if(input$prop == TRUE & input$selectBy == "Person (trap set)" & input$selectSubject != "Species"){
                ## Determine the dataset that needs to be divided by to get proportion data
                datByprop <- datProp[datProp$prevperson == byvec[i], ]

                ## Divide the i'th individual table by the above dataset
                namesTables[[byvec[i]]] = table(factor(datCounts[datCounts$prevperson == byvec[i], ]$subject, levels = subject.elements), factor(datCounts[datCounts$prevperson == byvec[i],]$calyear, levels = years))/table(factor(datByprop$subject, levels = subject.elements), factor(datByprop$calyear, levels = years))*100

              }else{
                ## If species is the subject
                if(input$prop == TRUE & input$selectBy == "Person (trap set)" & input$selectSubject == "Species"){
                  # Determine the dataset that needs to be divided by to get proportion data
                  datByprop <- datProp[datProp$prevperson == byvec[i], ]

                  ## Need a vector of 12 (one for each month) of the number of traps set in each month
                  yearVec <- as.vector(table(factor(datByprop$calyear, levels = years)))
                  yearMatrix <- matrix(rep(yearVec, times = length(subject.elements)), ncol = length(years), byrow = TRUE)

                  ## Divide the i'th individual table by the above dataset
                  namesTables[[byvec[i]]] = table(factor(datCounts[datCounts$prevperson == byvec[i], ]$subject, levels = subject.elements), factor(datCounts[datCounts$prevperson == byvec[i], ]$calyear, levels = years))/yearMatrix*100

                }else{
                  ## If individual proportions are not selected
                  namesTables[[byvec[i]]] = table(factor(datCounts[datCounts$prevperson == byvec[i], ]$subject, levels = subject.elements), factor(datCounts[datCounts$prevperson == byvec[i], ]$calyear, levels = years))
                }
              }
            }
          }
          if(select.by == "Line"){
            for(i in 1:length(byvec)){
              ## Put into proportions if individual proportions are selected
              if(input$prop == TRUE & input$selectBy == "Line" & input$selectSubject != "Species"){
                ## Determine the dataset that needs to be divided by to get proportion data
                datByprop <- datProp[datProp$Line == byvec[i], ]

                ## Divide the i'th individual table by the above dataset
                namesTables[[byvec[i]]] = table(factor(datCounts[datCounts$Line == byvec[i], ]$subject, levels = subject.elements), factor(datCounts[datCounts$Line == byvec[i],]$calyear, levels = years))/table(factor(datByprop$subject, levels = subject.elements), factor(datByprop$calyear, levels = years))*100

              }else{
                ## If species is the subject
                if(input$prop == TRUE & input$selectBy == "Line" & input$selectSubject == "Species"){
                  # Determine the dataset that needs to be divided by to get proportion data
                  datByprop <- datProp[datProp$Line == byvec[i], ]

                  ## Need a vector of 12 (one for each month) of the number of traps set in each month
                  yearVec <- as.vector(table(factor(datByprop$calyear, levels = years)))
                  yearMatrix <- matrix(rep(yearVec, times = length(subject.elements)), ncol = length(years), byrow = TRUE)

                  ## Divide the i'th individual table by the above dataset
                  namesTables[[byvec[i]]] = table(factor(datCounts[datCounts$Line == byvec[i], ]$subject, levels = subject.elements), factor(datCounts[datCounts$Line == byvec[i], ]$calyear, levels = years))/yearMatrix*100

                }else{
                  ## If individual proportions are not selected
                  namesTables[[byvec[i]]] = table(factor(datCounts[datCounts$Line == byvec[i], ]$subject, levels = subject.elements), factor(datCounts[datCounts$Line == byvec[i],]$calyear, levels = years))
                }
              }
            }
          }
          if(select.by == "Species"){
            for(i in 1:length(byvec)){
              if(input$prop == TRUE & input$selectBy == "Species"){
                ## Determine the dataset that needs to be divided by to get proportion data
                datByprop <- datProp[datProp$Species == byvec[i], ]

                ## Need a vector of X (one for each year) of the number of traps set in each month
                ## Unlike other options - for species we have to divide by the number of traps in each month across all elements of species because we do not know the number of times a species attempted to take a bait
                yearVec <- as.vector(table(factor(dat$calyear, levels = years)))
                yearMatrix <- matrix(rep(yearVec, times = length(subject.elements)), ncol = length(years), byrow = TRUE)

                ## Divide the i'th individual table by the above dataset
                namesTables[[byvec[i]]] = table(factor(datCounts[datCounts$Species == byvec[i], ]$subject, levels = subject.elements), factor(datCounts[datCounts$Species == byvec[i], ]$calyear, levels = years))/yearMatrix*100
              }else{
                ## If individual proportions are not selected
                namesTables[[byvec[i]]] = table(factor(datCounts[datCounts$Species == byvec[i], ]$subject, levels = subject.elements), factor(datCounts[datCounts$Species == byvec[i],]$calyear, levels = years))
              }
            }
          }
          if(select.by == "Bait"){
            for(i in 1:length(byvec)){
              ## Put into proportions if individual proportions are selected
              if(input$prop == TRUE & input$selectBy == "Bait" & input$selectSubject != "Species"){
                ## Determine the dataset that needs to be divided by to get proportion data
                datByprop <- datProp[datProp$prevbait == byvec[i], ]

                ## Divide the i'th individual table by the above dataset
                namesTables[[byvec[i]]] = table(factor(datCounts[datCounts$prevbait == byvec[i], ]$subject, levels = subject.elements), factor(datCounts[datCounts$prevbait == byvec[i], ]$calyear, levels = years))/table(factor(datByprop$subject, levels = subject.elements), factor(datByprop$calyear, levels = years))*100

              }else{
                ## If species is the subject
                if(input$prop == TRUE & input$selectBy == "Bait" & input$selectSubject == "Species"){
                  # Determine the dataset that needs to be divided by to get proportion data
                  datByprop <- datProp[datProp$prevbait == byvec[i], ]

                  ## Need a vector of 12 (one for each month) of the number of traps set in each month
                  yearVec <- as.vector(table(factor(datByprop$calyear, levels = years)))
                  yearMatrix <- matrix(rep(yearVec, times = length(subject.elements)), ncol = length(years), byrow = TRUE)

                  ## Divide the i'th individual table by the above dataset
                  namesTables[[byvec[i]]] = table(factor(datCounts[datCounts$prevbait == byvec[i], ]$subject, levels = subject.elements), factor(datCounts[datCounts$prevbait == byvec[i], ]$calyear, levels = years))/yearMatrix*100

                }else{
                  ## If individual proportions are not selected
                  namesTables[[byvec[i]]] = table(factor(datCounts[datCounts$prevbait == byvec[i], ]$subject, levels = subject.elements), factor(datCounts[datCounts$prevbait == byvec[i], ]$calyear, levels = years))
                }
              }
            }
          }
          if(select.by == "Trap Type"){
            for(i in 1:length(byvec)){
              ## Put into proportions if individual proportions are selected
              if(input$prop == TRUE & input$selectBy == "Trap Type" & input$selectSubject != "Species"){
                ## Determine the dataset that needs to be divided by to get proportion data
                datByprop <- datProp[datProp$TrapType == byvec[i], ]

                ## Divide the i'th individual table by the above dataset
                namesTables[[byvec[i]]] = table(factor(datCounts[datCounts$TrapType == byvec[i], ]$subject, levels = subject.elements), factor(datCounts[datCounts$TrapType == byvec[i],]$calyear, levels = years))/table(factor(datByprop$subject, levels = subject.elements), factor(datByprop$calyear, levels = years))*100

              }else{
                ## If species is the subject
                if(input$prop == TRUE & input$selectBy == "Trap Type" & input$selectSubject == "Species"){
                  # Determine the dataset that needs to be divided by to get proportion data
                  datByprop <- datProp[datProp$TrapType == byvec[i], ]

                  ## Need a vector of 12 (one for each month) of the number of traps set in each month
                  yearVec <- as.vector(table(factor(datByprop$calyear, levels = years)))
                  yearMatrix <- matrix(rep(yearVec, times = length(subject.elements)), ncol = length(years), byrow = TRUE)

                  ## Divide the i'th individual table by the above dataset
                  namesTables[[byvec[i]]] = table(factor(datCounts[datCounts$TrapType == byvec[i], ]$subject, levels = subject.elements), factor(datCounts[datCounts$TrapType == byvec[i], ]$calyear, levels = years))/yearMatrix*100

                }else{
                  ## If individual proportions are not selected
                  namesTables[[byvec[i]]] = table(factor(datCounts[datCounts$TrapType == byvec[i], ]$subject, levels = subject.elements), factor(datCounts[datCounts$TrapType == byvec[i],]$calyear, levels = years))
                }
              }
            }
          }
        }

        ## Assign the appropriate titles for the overall plot
        if(agg == "Month in a selected year"){
          overallXlab <- "Month"
          ## If the overall plot is to be plotted as a proportion
          if(input$prop == TRUE){
            overallYlab <- "Percentage (%)"
            if(input$selectSubject == "Species" | input$selectSubject == "Person (trap checked)"){
              overallMain <- paste("Percentage of traps checked with a catch in \n ", project.area, " by ", input$selectSubject, " (", year, ")", sep = "")
            }
            if(input$selectSubject == "Person (trap set)"){
              overallMain <- paste("Percentage of successful traps set in \n ", project.area, " by Person", " (", year, ")", sep = "")
            }
            if(input$selectSubject == "Bait" | input$selectSubject == "Trap Type" | input$selectSubject == "Line"){
              overallMain <- paste("Percentage of successful traps set in \n ", project.area, " by ", input$selectSubject," (", year, ")", sep = "")
            }
          }else{
            overallYlab <- "Number Caught"
            ## If the overall plot is to be plotted as total numbers
            if(input$selectSubject == "Person (trap checked)"){
              overallMain <- paste("Number of catches discovered in \n ", project.area, " by Person", " (", year, ")", sep = "")
            }
            if(input$selectSubject == "Person (trap set)"){
              overallMain <- paste("Number of successful traps set in \n ", project.area, " by Person", " (", year, ")", sep = "")
            }
            if(input$selectSubject != "Person (trap checked)" & input$selectSubject != "Person (trap set)"){
              overallMain <- paste("Number of catches made in \n ", project.area, " by ", input$selectSubject," (", year, ")", sep = "")
            }
          }
        }
        if(agg == "Month over all years"){
          overallXlab <- "Month (all time)"
          ## If the overall plot is to be plotted as a proportion
          if(input$prop == TRUE){
            overallYlab <- "Percentage (%)"
            if(input$selectSubject == "Species" | input$selectSubject == "Person (trap checked)"){
              overallMain <- paste("Percentage of traps checked with a catch in \n ", project.area, " by ", input$selectSubject, sep = "")
            }
            if(input$selectSubject == "Person (trap set)"){
              overallMain <- paste("Percentage of successful traps set in \n ", project.area, " by Person", sep = "")
            }
            if(input$selectSubject != "Person (trap checked)" & input$selectSubject != "Person (trap set)"){
              overallMain <- paste("Percentage of successful traps set in \n ", project.area, " by ", input$selectSubject, sep = "")
            }
          }else{
            overallYlab <- "Number Caught"
            ## If the overall plot is to be plotted as total numbers
            if(input$selectSubject == "Person (trap checked)"){
              overallMain <- paste("Number of catches discovered in \n ", project.area, " by Person", sep = "")
            }
            if(input$selectSubject == "Person (trap set)"){
              overallMain <- paste("Number of successful traps set in \n ", project.area, " by Person", sep = "")
            }
            if(input$selectSubject != "Person (trap checked)" & input$selectSubject != "Person (trap set)"){
              overallMain <- paste("Number of catches made in \n ", project.area, " by ", input$selectSubject, sep = "")
            }
          }
        }
        if(agg == "Year"){
          overallXlab <- "Year"
          ## If the overall plot is to be plotted as a proportion
          if(input$prop == TRUE){
            overallYlab <- "Percentage (%)"
            if(input$selectSubject == "Species" | input$selectSubject == "Person (trap checked)"){
              overallMain <- paste("Percentage of traps checked with a catch in \n ", project.area, " by ", input$selectSubject, sep = "")
            }
            if(input$selectSubject == "Person (trap set)"){
              overallMain <- paste("Percentage of successful traps set in \n ", project.area, " by Person", sep = "")
            }
            if(input$selectSubject != "Person (trap checked)" & input$selectSubject != "Person (trap set)"){
              overallMain <- paste("Percentage of successful traps set in \n ", project.area, " by ", input$selectSubject, sep = "")
            }
          }else{
            overallYlab <- "Number Caught"
            ## If the overall plot is to be plotted as total numbers
            if(input$selectSubject == "Person (trap checked)"){
              overallMain <- paste("Number of all time catches discovered in \n ", project.area, " by Person", sep = "")
            }
            if(input$selectSubject == "Person (trap set)"){
              overallMain <- paste("Number of all time succesful traps set in \n ", project.area, " by Person", sep = "")
            }
            if(input$selectSubject != "Person (trap checked)" & input$selectSubject != "Person (trap set)"){
              overallMain <- paste("Number of all time catches made in \n ", project.area, " by ", input$selectSubject, sep = "")
            }
          }
        }

        list(datCounts = datCounts, datTable = datTable, namesTables = namesTables, subject = subject, subject.elements = subject.elements, byvec = byvec, agg = agg, year = year, project.area = project.area, overallMain = overallMain, overallXlab = overallXlab, overallYlab = overallYlab, subject.all = subject.all, by.all = by.all, unique.by = unique.by)
      })
    })
    ######### End of working data ###########


    ########## Reactive UI for TopBar selections

    ## Reactive year selections
    output$selectYear <- renderUI({

      ## Get the unique years available for the area and sort into decreasing order
      unique.years <- area.data()$unique.years
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
      #subject <- working.data()$subject

      subject <- input$selectSubject
      if(input$selectSubject == "Person (trap checked)"){
        subject <- "FirstName"
      }
      if(input$selectSubject == "Person (trap set)"){
        subject <- "prevperson"
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

      ## print("List of Subjects Loading")

      ## Conditional scrolling selection
      selectInput('subject',
                  label = h5(paste("Select ", input$selectSubject, ":", sep = "")),
                  choices = unique.subject,
                  selected = unique.subject,
                  multiple = TRUE,
                  selectize = FALSE,
                  size = 5)

    })


    ## Reactive breakdown title
    output$breakdowntitle <- renderUI({

      conditionalPanel(
        condition = "input.selectBy != 'No Selection' && input.selectArea != ''",
        h3("Individual plot options:")
      )

    })



    ## Reactive 'by' radio buttons
    output$by <- renderUI({

      if(input$selectBy == "No Selection"){
        return(NULL)
      }

      ## print("Select All/List")

      ## Define 'by'
      if(input$selectBy == "Person (trap checked)" | input$selectBy == "Person (trap set)"){
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

        ## print("List of SubBy Start...")

        ## Define 'subject' and 'by'
        subject <- input$selectSubject
        by.vec <- input$selectBy

        ## Correct for if subject is 'Person' or 'Trap Type' or 'Bait'
        if(input$selectSubject == "Person (trap checked)"){
          subject <- "FirstName"
        }
        if(input$selectSubject == "Person (trap set)"){
          subject <- "prevperson"
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

        ## Extract input$selectBy
        datcatch$by <- datcatch[[by.vec]]
        if(by.vec == "Person (trap checked)"){
          datcatch$by <- datcatch$FirstName
        }
        if(by.vec == "Person (trap set)"){
          datcatch$by <- datcatch$prevperson
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
        if(input$selectBy == "Person (trap checked)"){
          by.vec <- "People"
        }

        ## print("List of SubBy End...")

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
      output$overallTitle <- renderUI({
        return(NULL)
      })
      output$overallInfo <- renderUI({
        return(NULL)
      })
      output$overall <- renderPlot({
        return(NULL)
      })
      output$individualTitle <- renderUI({
        return(NULL)
      })
      output$stackedplot <- renderPlot({
        return(NULL)
      })
      ## print("Blank due to area change")
    })

    ## Load blank when 'colour by' is changed
    #observeEvent(input$selectSubject, priority = 16, {
    observe({input$selectSubject
      output$overallTitle <- renderUI({
        return(NULL)
      })
      output$overallInfo <- renderUI({
        return(NULL)
      })
      output$overall <- renderPlot({
        return(NULL)
      })
      output$individualTitle <- renderUI({
        return(NULL)
      })
      output$stackedplot <- renderPlot({
        return(NULL)
      })
      ## print("Blank due to colour by change")
    })

    ## Load blank when 'breakdown by' is changed
    #observeEvent(input$selectBy, priority = 15, {
    observe({input$selectBy
      output$overallTitle <- renderUI({
        return(NULL)
      })
      output$overallInfo <- renderUI({
        return(NULL)
      })
      output$overall <- renderPlot({
        return(NULL)
      })
      output$individualTitle <- renderUI({
        return(NULL)
      })
      output$stackedplot <- renderPlot({
        return(NULL)
      })
      ## print("Blank due to select by change")
    })

    ## Load blank when colour by proportion option is changed
    observeEvent(input$prop, {
      output$overallTitle <- renderUI({
        return(NULL)
      })
      output$overallInfo <- renderUI({
        return(NULL)
      })
      output$overall <- renderPlot({
        return(NULL)
      })
      output$individualTitle <- renderUI({
        return(NULL)
      })
      output$stackedplot <- renderPlot({
        return(NULL)
      })
      ## print("Blank due to bait proportions")
    })

    ## Load blank when by proportion option is changed
    observeEvent(input$prop, {
      output$overallTitle <- renderUI({
        return(NULL)
      })
      output$overallInfo <- renderUI({
        return(NULL)
      })
      output$overall <- renderPlot({
        return(NULL)
      })
      output$individualTitle <- renderUI({
        return(NULL)
      })
      output$stackedplot <- renderPlot({
        return(NULL)
      })
      ## print("Blank due to bait proportions")
    })

    ## Load blank when 'aggregate by' is changed
    observeEvent(input$selectAggregate, priority = 14, {
      output$overallTitle <- renderUI({
        return(NULL)
      })
      output$overallInfo <- renderUI({
        return(NULL)
      })
      output$overall <- renderPlot({
        return(NULL)
      })
      output$individualTitle <- renderUI({
        return(NULL)
      })
      output$stackedplot <- renderPlot({
        return(NULL)
      })
      ## print("Blank due to aggregate by")
    })

    ## Load blank when 'select year' is changed
    observeEvent(input$selectYear, priority = 13, {
      output$overallTitle <- renderUI({
        return(NULL)
      })
      output$overallInfo <- renderUI({
        return(NULL)
      })
      output$overall <- renderPlot({
        return(NULL)
      })
      output$individualTitle <- renderUI({
        return(NULL)
      })
      output$stackedplot <- renderPlot({
        return(NULL)
      })
      ## print("Blank due to year change")
    })

    ## Load blank when subject elements are changed
    observeEvent(input$subject, priority = 12, {
      output$overallTitle <- renderUI({
        return(NULL)
      })
      output$overallInfo <- renderUI({
        return(NULL)
      })
      output$overall <- renderPlot({
        return(NULL)
      })
      output$individualTitle <- renderUI({
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

      output$overallTitle <- renderUI({
        return(NULL)
      })
      output$overallInfo <- renderUI({
        return(NULL)
      })
      output$overall <- renderPlot({
        return(NULL)
      })
      output$individualTitle <- renderUI({
        return(NULL)
      })
      output$stackedplot <- renderPlot({
        return(NULL)
      })
    })


    ########## START OF OVERALL PLOT ###########

    ## Generate code required to plot the Overall Bar Plot
    observeEvent(input$submit, priority = 9, {

      ## Overall bar chart title
      output$overallTitle <- renderUI({
        h3("Overall bar chart:")
      })

      ## Overall bar chart
      output$overall <- renderPlot({

        ## Busy indicator
        if (input$submit == 0)
          return()
        #Sys.sleep(2)

        ## If no subject or no specific plot options are selected give error
        if(is.null(input$subject)){
          stop("No colour options selected")
        }
        if(input$selectBy != "No Selection" & is.null(input$subby)){
          stop("No individual plot options selected")
        }

        ## print("Overall pre-code is running")

        ## Load the table of data
        datTable <- working.data()$datTable

        ## Define selected inputs
        project.area <- input$selectArea
        agg <- working.data()$agg
        year <- working.data()$year

        ########## Plotting commands ############

        ## Plot the overall data and add non-dynamic labels
        #output$overall <- renderPlot({
        ## print("Overall is plotting")

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
        if(input$prop == TRUE){
          xx <- barplot(datTable, beside = TRUE, col = colour.diff[1:length(rownames(datTable))], main = working.data()$overallMain, xlab = working.data()$overallXlab, ylab = working.data()$overallYlab, cex.main = 1.5, cex.lab = 1.2, ylim = c(0, min(100, max(datTable, na.rm = TRUE) + 0.1*max(datTable, na.rm = TRUE))))
        }else{
         ## Not plotting percentages. Use a trick with axTicks to ensure only whole numbers appear on the axis labels:
          xx <- barplot(datTable, beside = TRUE, col = colour.diff[1:length(rownames(datTable))], main = working.data()$overallMain, xlab = working.data()$overallXlab, ylab = working.data()$overallYlab, cex.main = 1.5, cex.lab = 1.2, ylim = c(0, max(as.numeric(datTable, na.rm = TRUE)) + 0.125*max(as.numeric(datTable, na.rm = TRUE))), yaxt="n")
          yaxis.labs <- axTicks(2)  ## This command detects what the y-axis ticks just plotted are.
          axis(side=2, at=yaxis.labs[yaxis.labs == trunc(yaxis.labs)])
        }
        dev.flush()

        ## Y-axis labels
        ## If proportions are requested
        #if(input$prop == TRUE & input$selectSubject != "Species"){
        #  title(ylab = "", cex.lab = 1.2)
        #}else{
        #  title(ylab = "Number Caught", cex.lab = 1.2)
        #}

        ## Add dynamic legend
        par(mar=c(0, 0.1, 0.1, 0), mgp=c(0, 0, 0))
        plot(1, type="n", bty="n", xaxt="n", yaxt="n")
        legend("topleft", legend = legend.string, col = 1, pt.bg = colour.diff[1:length(input$subject)], pch = 22, cex = auto.cex*0.8, pt.cex = auto.cex*1.75, xpd = TRUE, inset = c(0, 0))

      })
    })


    ## Overall bar chart hover information
    #output$overall_hoverinfo <- renderUI({
    #  ## Only hover if there are bar charts in view
    #  if(!(is.null((input$overall_hover)$x))){
    #
    #    ## Only hover if not all byvec are selected
    #    if(length(working.data()$byvec) != length(working.data()$by.all)){
    #
    #      ## Assign plurals to subjects
    #      plural.vec <- c(Person.checked = "people", Bait = "baits", Trap.Type="trap types", Line = "lines", Person.set = "people", Species = "species")
    #
    #      ## Rename TrapType and Previous Person
    #      names(plural.vec)[1] <- "Person (trap checked)"
    #      names(plural.vec)[3] <- "Trap Type"
    #      names(plural.vec)[5] <- "Person (trap set)"
    #
    #      ## List of elements that are in the data
    #      title.vec <- paste(working.data()$byvec, sep = ", ", collapse = ", ")
    #      print(title.vec)
    #
    #      h5(paste0("Only data from the selected ", plural.vec[input$selectBy], " are displayed (", title.vec, ")"), style = "color:grey")
    #   }
    #
    # }
    #})

    ## Overall bar chart extra information
    observeEvent(input$submit, {
      output$overallInfo <- renderUI({

            ## Only appear if not all byvec are selected
            if(length(working.data()$byvec) != length(working.data()$unique.by)){

              ## Assign plurals to subjects
              plural.vec <- c(Person.checked = "people", Bait = "baits", Trap.Type="trap types", Line = "lines", Person.set = "people", Species = "species")

              ## Rename TrapType and Previous Person
              names(plural.vec)[1] <- "Person (trap checked)"
              names(plural.vec)[3] <- "Trap Type"
              names(plural.vec)[5] <- "Person (trap set)"

              ## List of elements that are in the data
              title.vec <- paste(working.data()$byvec, sep = ", ", collapse = ", ")

              h5(paste0("Only data from the selected ", plural.vec[input$selectBy], " are displayed (", title.vec, ")"), style = "color:grey")
           }

        })
      })


    ## Individual bar chart hover title
    #output$individual_hoverinfo <- renderUI({
    #  if(!(is.null((input$overall_hover)$x))){
    #    h3("Individual Bar Charts:")
    #  }
    #})

    ########## END OF OVERALL PLOT ##########






    ########## START OF STACKED SUBJECT PLOTS ##########

    ## Plot stacked plots of selected subject only when 'submit' is pressed while 'expand' is checked
    observeEvent(input$submit, priority = 8, {
    #observe({
        ## print("Stacked pre-code is running")

        ## If a 'by' is not selected do nothing
        if(input$selectBy == "No Selection"){
          return(NULL)
        }

        ## Give error if input$subby is empty
        if(length(input$subby) == 0 && input$selectBy != "No Selection"){
          return(paste("No ", input$selectBy, " selected to breakdown", sep = ""))
        }

        ## Individual bar chart title
        output$individualTitle <- renderUI({
          if(length(working.data()$byvec > 1)){
            h3("Individual bar charts:")
          }
          if(length(working.data()$byvec == 1)){
            h3("Individual bar chart:")
          }
        })

        ## Load the raw data
        dat <- area.data()$dat

        ## Load the tables of data
        datTable <- working.data()$datTable
        namesTables <- working.data()$namesTables
        #print(namesTables)

        ## Define selected inputs
        agg <- working.data()$agg
        year <- working.data()$year
        select.by <- input$selectBy
        subject <- input$selectSubject
        subject.elements <- working.data()$subject.elements
        byvec <- input$subby


        ## print("Stacked pre-code is finished")

        ########### Plotting commands ###########

        ## Set plotlength for right number of outputs (# persons or # lines)
        plotlength <- length(byvec)

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
              legend.string <- c(subject.elements)
              auto.cex <- max(min.cex, min(c(20/length(legend.string)), 20/nchar(legend.string)))
              auto.cex <- min(auto.cex, max.cex)

              ## Create a colour vector for the plots (paired colours)
              colour.diff <- brewer.pal(9, "Pastel1")
              colour.diff2 <- brewer.pal(8, "Pastel2")
              colour.paired <- brewer.pal(12, "Paired")

              if(length(legend.string) > 9){
                colour.diff <- colorRampPalette(brewer.pal(brewer.pal.info["Pastel1", "maxcolors"], "Pastel1"))(length(legend.string))
              }
              ## print("Stacked is plotting")

              ## Plot the stacked plot of individual/line data for each element of the subject
              dev.hold()
              if(input$prop == TRUE){
                bside = TRUE
                barplot(namesTables[[my_i]], beside = bside, col = colour.diff[1:length(subject.elements)], ylim = c(0, min(100, max(namesTables[[my_i]], na.rm = TRUE) + 0.1*max(namesTables[[my_i]], na.rm = TRUE))))
                ## print(max(namesTables[[my_i]], na.rm = TRUE))
              }else{
                ## Not plotting percentages: use a trick with axTicks to ensure only whole numbers are
                ## plotted as axis labels:
                bside = FALSE
                barplot(namesTables[[my_i]], beside = bside, col = colour.diff[1:length(subject.elements)], yaxt="n")#, ylim = c(0, max(colSums(namesTables[[my_i]])) + 0.1*max(colSums(namesTables[[my_i]]))))
                yaxis.lbs <- axTicks(2)  ## This command detects what the y-axis ticks just plotted are.
                axis(side=2, at=yaxis.lbs[yaxis.lbs == trunc(yaxis.lbs)])
              }

              ## Add dynamic titles
              if(agg == "Month in a selected year"){
                ## If individual proportions are selected
                #if(input$prop == TRUE & input$selectBy != "Species"){
                if(input$prop == TRUE){
                  if(input$selectBy == "Species"){
                    title(main = paste("Percentage of traps that caught a \n", byvec[my_i], " in ", dat$Area[1], " (", year, ")", sep = ""), xlab = "Month", ylab = "Percentage (%)", cex.main = 1.5, cex.lab = 1.2)
                  }
                  if(input$selectBy == "Person (trap checked)"){
                    title(main = paste("Percentage of traps checked with a catch by \n ", byvec[my_i], " in ", dat$Area[1], " (", year, ")", sep = ""), xlab = "Month", ylab = "Percentage (%)", cex.main = 1.5, cex.lab = 1.2)
                  }
                  if(input$selectBy == "Person (trap set)"){
                    title(main = paste("Percentage of successful traps set by \n ", byvec[my_i], " in ", dat$Area[1], " (", year, ")", sep = ""), xlab = "Month", ylab = "Percentage (%)", cex.main = 1.5, cex.lab = 1.2)
                  }
                  if(input$selectBy == "Line"){
                    title(main = paste("Percentage of successful traps set for \n ", byvec[my_i], " in ", dat$Area[1], " (", year, ")", sep = ""), xlab = "Month", ylab = "Percentage (%)", cex.main = 1.5, cex.lab = 1.2)
                  }
                  if(input$selectBy == "Bait"){
                    title(main = paste("Percentage of successful traps set using \n ", byvec[my_i], " in ", dat$Area[1], " (", year, ")", sep = ""), xlab = "Month", ylab = "Percentage (%)", cex.main = 1.5, cex.lab = 1.2)
                  }
                  if(input$selectBy == "Trap Type"){
                    title(main = paste("Percentage of successful traps set for \n ", byvec[my_i], " traps in ", dat$Area[1], " (", year, ")", sep = ""), xlab = "Month", ylab = "Percentage (%)", cex.main = 1.5, cex.lab = 1.2)
                  }
                }else{
                  ## If individual proportions are not selected
                  if(select.by == "Person (trap checked)"){
                    title(paste("Catches discovered by ", byvec[my_i], "\n in ", dat$Area[1], " (", year, ")", sep = ""), xlab = "Month", ylab = "Number Caught", cex.main = 1.5, cex.lab = 1.2)
                  }
                  if(select.by == "Person (trap set)"){
                    title(paste("Successful traps set by ", byvec[my_i], "\n in ", dat$Area[1], " (", year, ")", sep = ""), xlab = "Month", ylab = "Number Caught", cex.main = 1.5, cex.lab = 1.2)
                  }
                  if(select.by == "Line"){
                    title(paste("Line catch data for ", byvec[my_i], "\n in ", dat$Area[1], " (", year, ")", sep = ""), xlab = "Month", ylab = "Number Caught", cex.main = 1.5, cex.lab = 1.2)
                  }
                  if(select.by == "Species"){
                    title(paste("Catch data for ", byvec[my_i], " in ", dat$Area[1], " (", year, ")", sep = ""), xlab = "Month", ylab = "Number Caught", cex.main = 1.5, cex.lab = 1.2)
                  }
                  if(select.by == "Bait"){
                    title(paste("Bait catch data for ", byvec[my_i], "\n in ", dat$Area[1], " (", year, ")", sep = ""), xlab = "Month", ylab = "Number Caught", cex.main = 1.5, cex.lab = 1.2)
                  }
                  if(select.by == "Trap Type"){
                    title(paste("Trap catch data for ", byvec[my_i], "\n in ", dat$Area[1], " (", year, ")", sep = ""), xlab = "Month", ylab = "Number Caught", cex.main = 1.5, cex.lab = 1.2)
                  }
                }
              }
              if(agg == "Month over all years"){
                ## If individual proportions are selected
                #if(input$prop == TRUE & input$selectBy != "Species"){
                if(input$prop == TRUE){
                  if(input$selectBy == "Species"){
                    title(main = paste("Percentage of traps that caught a \n", byvec[my_i], " in ", dat$Area[1], sep = ""), xlab = "Month", ylab = "Percentage (%)", cex.main = 1.5, cex.lab = 1.2)
                  }
                  if(input$selectBy == "Person (trap checked)"){
                    title(main = paste("Percentage of traps checked with a catch by \n ", byvec[my_i], " in ", dat$Area[1], " (", year, ")", sep = ""), xlab = "Month (all time)", ylab = "Percentage (%)", cex.main = 1.5, cex.lab = 1.2)
                  }
                  if(input$selectBy == "Person (trap set)"){
                    title(main = paste("Percentage of successful traps set by \n ", byvec[my_i], " in ", dat$Area[1], " (", year, ")", sep = ""), xlab = "Month (all time)", ylab = "Percentage (%)", cex.main = 1.5, cex.lab = 1.2)
                  }
                  if(input$selectBy == "Line"){
                    title(main = paste("Percentage of successful traps set for \n ", byvec[my_i], " in ", dat$Area[1], " (", year, ")", sep = ""), xlab = "Month (all time)", ylab = "Percentage (%)", cex.main = 1.5, cex.lab = 1.2)
                  }
                  if(input$selectBy == "Bait"){
                    title(main = paste("Percentage of successful traps set using \n ", byvec[my_i], " in ", dat$Area[1], " (", year, ")", sep = ""), xlab = "Month (all time)", ylab = "Percentage (%)", cex.main = 1.5, cex.lab = 1.2)
                  }
                  if(input$selectBy == "Trap Type"){
                    title(main = paste("Percentage of successful traps set for \n ", byvec[my_i], " traps in ", dat$Area[1], " (", year, ")", sep = ""), xlab = "Month (all time)", ylab = "Percentage (%)", cex.main = 1.5, cex.lab = 1.2)
                  }
                }else{
                  ## If individual proportions are not selected
                  if(select.by == "Person (trap checked)"){
                    title(paste("Catches discovered by ", byvec[my_i], "\n in ", dat$Area[1], sep = ""), xlab = "Month (all time)", ylab = "Number Caught", cex.main = 1.5, cex.lab = 1.2)
                  }
                  if(select.by == "Person (trap set)"){
                    title(paste("Successful traps set by ", byvec[my_i], "\n in ", dat$Area[1], sep = ""), xlab = "Month (all time)", ylab = "Number Caught", cex.main = 1.5, cex.lab = 1.2)
                  }
                  if(select.by == "Line"){
                    title(paste("Line catch data for ", byvec[my_i], "\n in ", dat$Area[1], sep = ""), xlab = "Month (all time)", ylab = "Number Caught", cex.main = 1.5, cex.lab = 1.2)
                  }
                  if(select.by == "Species"){
                    title(paste("Catch data for ", byvec[my_i], " in ", dat$Area[1], sep = ""), xlab = "Month (all time)", ylab = "Number Caught", cex.main = 1.5, cex.lab = 1.2)
                  }
                  if(select.by == "Bait"){
                    title(paste("Bait catch data for ", byvec[my_i], "\n in ", dat$Area[1], sep = ""), xlab = "Month (all time)", ylab = "Number Caught", cex.main = 1.5, cex.lab = 1.2)
                  }
                  if(select.by == "Trap Type"){
                    title(paste("Trap catch data for ", byvec[my_i], "\n in ", dat$Area[1], sep = ""), xlab = "Month (all time)", ylab = "Number Caught", cex.main = 1.5, cex.lab = 1.2)
                  }
                }
              }
              if(agg == "Year"){
                ## If individual proportions are selected
                #if(input$prop == TRUE & input$selectBy != "Species"){
                if(input$prop == TRUE){
                  if(input$selectBy == "Species"){
                    title(main = paste("Percentage of traps that caught a \n", byvec[my_i], " in ", dat$Area[1], sep = ""), xlab = "Month", ylab = "Percentage (%)", cex.main = 1.5, cex.lab = 1.2)
                  }
                  if(input$selectBy == "Person (trap checked)"){
                    title(main = paste("Percentage of traps checked with a catch by \n ", byvec[my_i], " in ", dat$Area[1], sep = ""), xlab = "Year", ylab = "Percentage (%)", cex.main = 1.5, cex.lab = 1.2)
                  }
                  if(input$selectBy == "Person (trap set)"){
                    title(main = paste("Percentage of successful traps set by \n ", byvec[my_i], " in ", dat$Area[1], sep = ""), xlab = "Year", ylab = "Percentage (%)", cex.main = 1.5, cex.lab = 1.2)
                  }
                  if(input$selectBy == "Line"){
                    title(main = paste("Percentage of successful traps set for \n ", byvec[my_i], " in ", dat$Area[1], sep = ""), xlab = "Year", ylab = "Percentage (%)", cex.main = 1.5, cex.lab = 1.2)
                  }
                  if(input$selectBy == "Bait"){
                    title(main = paste("Percentage of successful traps set using \n ", byvec[my_i], " in ", dat$Area[1], sep = ""), xlab = "Year", ylab = "Percentage (%)", cex.main = 1.5, cex.lab = 1.2)
                  }
                  if(input$selectBy == "Trap Type"){
                    title(main = paste("Percentage of successful traps set for \n ", byvec[my_i], " traps in ", dat$Area[1], sep = ""), xlab = "Year", ylab = "Percentage (%)", cex.main = 1.5, cex.lab = 1.2)
                  }
                }else{
                  ## If individual proportions are not selected
                  if(select.by == "Person (trap checked)"){
                    title(paste("Catches discovered by ", byvec[my_i], "\n in ", dat$Area[1], sep = ""), xlab = "Year", ylab = "Number Caught", cex.main = 1.5, cex.lab = 1.2)
                  }
                  if(select.by == "Person (trap set)"){
                    title(paste("Successful traps set by ", byvec[my_i], "\n in ", dat$Area[1], sep = ""), xlab = "Year", ylab = "Number Caught", cex.main = 1.5, cex.lab = 1.2)
                  }
                  if(select.by == "Line"){
                    title(paste("Line catch data for ", byvec[my_i], "\n in ", dat$Area[1], sep = ""), xlab = "Year", ylab = "Number Caught", cex.main = 1.5, cex.lab = 1.2)
                  }
                  if(select.by == "Species"){
                    title(paste("Catch data for ", byvec[my_i], " in ", dat$Area[1], sep = ""), xlab = "Year", ylab = "Number Caught", cex.main = 1.5, cex.lab = 1.2)
                  }
                  if(select.by == "Bait"){
                    title(paste("Bait catch data for ", byvec[my_i], "\n in ", dat$Area[1], sep = ""), xlab = "Year", ylab = "Number Caught", cex.main = 1.5, cex.lab = 1.2)
                  }
                  if(select.by == "Trap Type"){
                    title(paste("Trap catch data for ", byvec[my_i], "\n in ", dat$Area[1], sep = ""), xlab = "Year", ylab = "Number Caught", cex.main = 1.5, cex.lab = 1.2)
                  }
                }
              }

              br()
              br()
              dev.flush()

              ## Add dynamic legend
              par(mar = c(0, 0.1, 0.1, 0), mgp = c(0, 0, 0))
              plot(1, type="n", bty="n", xaxt="n", yaxt="n")
              legend("topleft", legend = legend.string, col = 1, pt.bg = colour.diff[1:length(subject.elements)], pch = 22, cex = auto.cex*0.8, pt.cex = auto.cex*1.75, xpd = TRUE, inset = c(0, 0))

            })
          })
        }
    })

    ########### END OF STACKED BAR CHART CODE ##########

    ## Download Graphics
    output$download <- downloadHandler(
      filename = paste("Graphics.pdf", sep = ""),
      content = function(file){

        ## Give error if no graphics have been called
        if(input$submit == 0){
          stop("No graphics have been requested")
        }

        pdf(file = file, onefile = TRUE, width = 12, height = 8)
        ## Overall Plot ##
        ## Data
        dat <- working.data()$dat
        datTable <- working.data()$datTable
        project.area <- working.data()$project.area
        ## Layout
        layout(matrix(1:2, ncol = 2), widths = c(0.8, 0.2))
        par(mar = c(6, 6, 3, 0))

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

        ## Overall plot
        if(input$prop == TRUE){
          xx <- barplot(datTable, beside = TRUE, col = colour.diff[1:length(rownames(datTable))], main = working.data()$overallMain, xlab = working.data()$overallXlab, ylab = working.data()$overallYlab, cex.main = 1.5, cex.lab = 1.2, ylim = c(0, min(100, max(datTable, na.rm = TRUE) + 0.1*max(datTable, na.rm = TRUE))))
        }else{
          ## Not plotting percentages: plot whole-number-only labels
          xx <- barplot(datTable, beside = TRUE, col = colour.diff[1:length(rownames(datTable))], main = working.data()$overallMain, xlab = working.data()$overallXlab, ylab = working.data()$overallYlab, cex.main = 1.5, cex.lab = 1.2, ylim = c(0, max(as.numeric(datTable, na.rm = TRUE)) + 0.125*max(as.numeric(datTable, na.rm = TRUE))), yaxt="n")
          yaxis.lbs <- axTicks(2)  ## This command detects what the y-axis ticks just plotted are.
          axis(side=2, at=yaxis.lbs[yaxis.lbs == trunc(yaxis.lbs)])

        }

        ## Add dynamic legend
        par(mar=c(0, 0.1, 0.25, 0), mgp=c(0, 0, 0))
        plot(1, type="n", bty="n", xaxt="n", yaxt="n")
        legend("topleft", legend = legend.string, col = 1, pt.bg = colour.diff[1:length(input$subject)], pch = 22, cex = auto.cex*0.8, pt.cex = auto.cex*1.75, xpd = TRUE, inset = c(0, 0))

        ## Ensure no individual plots are downloaded when no selection is selected
        if(input$selectBy != "No Selection"){

          ##### Individual plots ######
          namesTables <- working.data()$namesTables

          ## Define selected inputs
          agg <- working.data()$agg
          year <- working.data()$year
          select.by <- input$selectBy
          subject <- input$selectSubject
          subject.elements <- working.data()$subject.elements
          byvec <- input$subby

          ## Determine whether bar charts should be stacked or not
          if(input$prop == TRUE){
            bside = TRUE
          }else{
            bside = FALSE
          }

          ## Produce individual bar charts
          for(i in 1:length(namesTables)){

            ## Layout
            layout(matrix(1:2, ncol = 2), widths=c(0.8, 0.2))
            par(mar = c(6, 6, 3, 0), mgp = c(3, 1, 0))

            ## Plot the bar chart
            if(input$prop == TRUE){
              bside = TRUE
              barplot(namesTables[[i]], beside = bside, col = colour.diff[1:length(subject.elements)], ylim = c(0, min(100, max(namesTables[[i]], na.rm = TRUE) + 0.1*max(namesTables[[i]], na.rm = TRUE))))
            }else{
              ## Not plotting percentages: whole-number-only labels
              bside = FALSE
              barplot(namesTables[[i]], beside = bside, col = colour.diff[1:length(subject.elements)], ylim = c(0, max(colSums(namesTables[[i]])) + 0.1*max(colSums(namesTables[[i]]))), yaxt="n")
              yaxis.lbs <- axTicks(2)  ## This command detects what the y-axis ticks just plotted are.
              axis(side=2, at=yaxis.lbs[yaxis.lbs == trunc(yaxis.lbs)])

            }

            ## Add dynamic titles
            if(agg == "Month in a selected year"){
              ## If individual proportions are selected
              #if(input$prop == TRUE & input$selectBy != "Species"){
              if(input$prop == TRUE){
                if(input$selectBy == "Species"){
                  title(main = paste("Percentage of traps that caught a \n", byvec[i], " in ", project.area, " (", year, ")", sep = ""), xlab = "Month", ylab = "Percentage (%)", cex.main = 1.5, cex.lab = 1.2)
                }
                if(input$selectBy == "Person (trap checked)"){
                  title(main = paste("Percentage of traps checked with a catch by \n ", byvec[i], " in ", project.area, " (", year, ")", sep = ""), xlab = "Month", ylab = "Percentage (%)", cex.main = 1.5, cex.lab = 1.2)
                }
                if(input$selectBy == "Person (trap set)"){
                  title(main = paste("Percentage of successful traps set by \n ", byvec[i], " in ", project.area, " (", year, ")", sep = ""), xlab = "Month", ylab = "Percentage (%)", cex.main = 1.5, cex.lab = 1.2)
                }
                if(input$selectBy == "Line"){
                  title(main = paste("Percentage of successful traps set for \n ", byvec[i], " in ", project.area, " (", year, ")", sep = ""), xlab = "Month", ylab = "Percentage (%)", cex.main = 1.5, cex.lab = 1.2)
                }
                if(input$selectBy == "Bait"){
                  title(main = paste("Percentage of successful traps set using \n ", byvec[i], " in ", project.area, " (", year, ")", sep = ""), xlab = "Month", ylab = "Percentage (%)", cex.main = 1.5, cex.lab = 1.2)
                }
                if(input$selectBy == "Trap Type"){
                  title(main = paste("Percentage of successful traps set for \n ", byvec[i], " traps in ", project.area, " (", year, ")", sep = ""), xlab = "Month", ylab = "Percentage (%)", cex.main = 1.5, cex.lab = 1.2)
                }
              }else{
                ## If individual proportions are not selected
                if(select.by == "Person (trap checked)"){
                  title(paste("Catches discovered by ", byvec[i], "\n in ", project.area, " (", year, ")", sep = ""), xlab = "Month", ylab = "Number Caught", cex.main = 1.5, cex.lab = 1.2)
                }
                if(select.by == "Person (trap set)"){
                  title(paste("Successful traps set by ", byvec[i], "\n in ", project.area, " (", year, ")", sep = ""), xlab = "Month", ylab = "Number Caught", cex.main = 1.5, cex.lab = 1.2)
                }
                if(select.by == "Line"){
                  title(paste("Line catch data for ", byvec[i], "\n in ", project.area, " (", year, ")", sep = ""), xlab = "Month", ylab = "Number Caught", cex.main = 1.5, cex.lab = 1.2)
                }
                if(select.by == "Species"){
                  title(paste("Catch data for ", byvec[i], " in ", project.area, " (", year, ")", sep = ""), xlab = "Month", ylab = "Number Caught", cex.main = 1.5, cex.lab = 1.2)
                }
                if(select.by == "Bait"){
                  title(paste("Bait catch data for ", byvec[i], "\n in ", project.area, " (", year, ")", sep = ""), xlab = "Month", ylab = "Number Caught", cex.main = 1.5, cex.lab = 1.2)
                }
                if(select.by == "Trap Type"){
                  title(paste("Trap catch data for ", byvec[i], "\n in ", project.area, " (", year, ")", sep = ""), xlab = "Month", ylab = "Number Caught", cex.main = 1.5, cex.lab = 1.2)
                }
              }
            }
            if(agg == "Month over all years"){
              ## If individual proportions are selected
              #if(input$prop == TRUE & input$selectBy != "Species"){
              if(input$prop == TRUE){
                if(input$selectBy == "Species"){
                  title(main = paste("Percentage of traps that caught a \n", byvec[i], " in ", project.area, sep = ""), xlab = "Month", ylab = "Percentage (%)", cex.main = 1.5, cex.lab = 1.2)
                }
                if(input$selectBy == "Person (trap checked)"){
                  title(main = paste("Percentage of traps checked with a catch by \n ", byvec[i], " in ", project.area, " (", year, ")", sep = ""), xlab = "Month (all time)", ylab = "Percentage (%)", cex.main = 1.5, cex.lab = 1.2)
                }
                if(input$selectBy == "Person (trap set)"){
                  title(main = paste("Percentage of successful traps set by \n ", byvec[i], " in ", project.area, " (", year, ")", sep = ""), xlab = "Month (all time)", ylab = "Percentage (%)", cex.main = 1.5, cex.lab = 1.2)
                }
                if(input$selectBy == "Line"){
                  title(main = paste("Percentage of successful traps set for \n ", byvec[i], " in ", project.area, " (", year, ")", sep = ""), xlab = "Month (all time)", ylab = "Percentage (%)", cex.main = 1.5, cex.lab = 1.2)
                }
                if(input$selectBy == "Bait"){
                  title(main = paste("Percentage of successful traps set using \n ", byvec[i], " in ", project.area, " (", year, ")", sep = ""), xlab = "Month (all time)", ylab = "Percentage (%)", cex.main = 1.5, cex.lab = 1.2)
                }
                if(input$selectBy == "Trap Type"){
                  title(main = paste("Percentage of successful traps set for \n ", byvec[i], " traps in ", project.area, " (", year, ")", sep = ""), xlab = "Month (all time)", ylab = "Percentage (%)", cex.main = 1.5, cex.lab = 1.2)
                }
              }else{
                ## If individual proportions are not selected
                if(select.by == "Person (trap checked)"){
                  title(paste("Catches discovered by ", byvec[i], "\n in ", project.area, sep = ""), xlab = "Month (all time)", ylab = "Number Caught", cex.main = 1.5, cex.lab = 1.2)
                }
                if(select.by == "Person (trap set)"){
                  title(paste("Successful traps set by ", byvec[i], "\n in ", project.area, sep = ""), xlab = "Month (all time)", ylab = "Number Caught", cex.main = 1.5, cex.lab = 1.2)
                }
                if(select.by == "Line"){
                  title(paste("Line catch data for ", byvec[i], "\n in ", project.area, sep = ""), xlab = "Month (all time)", ylab = "Number Caught", cex.main = 1.5, cex.lab = 1.2)
                }
                if(select.by == "Species"){
                  title(paste("Catch data for ", byvec[i], " in ", project.area, sep = ""), xlab = "Month (all time)", ylab = "Number Caught", cex.main = 1.5, cex.lab = 1.2)
                }
                if(select.by == "Bait"){
                  title(paste("Bait catch data for ", byvec[i], "\n in ", project.area, sep = ""), xlab = "Month (all time)", ylab = "Number Caught", cex.main = 1.5, cex.lab = 1.2)
                }
                if(select.by == "Trap Type"){
                  title(paste("Trap catch data for ", byvec[i], "\n in ", project.area, sep = ""), xlab = "Month (all time)", ylab = "Number Caught", cex.main = 1.5, cex.lab = 1.2)
                }
              }
            }
            if(agg == "Year"){
              ## If individual proportions are selected
              #if(input$prop == TRUE & input$selectBy != "Species"){
              if(input$prop == TRUE){
                if(input$selectBy == "Species"){
                  title(main = paste("Percentage of traps that caught a \n", byvec[i], " in ", project.area, sep = ""), xlab = "Month", ylab = "Percentage (%)", cex.main = 1.5, cex.lab = 1.2)
                }
                if(input$selectBy == "Person (trap checked)"){
                  title(main = paste("Percentage of traps checked with a catch by \n ", byvec[i], " in ", project.area, sep = ""), xlab = "Year", ylab = "Percentage (%)", cex.main = 1.5, cex.lab = 1.2)
                }
                if(input$selectBy == "Person (trap set)"){
                  title(main = paste("Percentage of successful traps set by \n ", byvec[i], " in ", project.area, sep = ""), xlab = "Year", ylab = "Percentage (%)", cex.main = 1.5, cex.lab = 1.2)
                }
                if(input$selectBy == "Line"){
                  title(main = paste("Percentage of successful traps set for \n ", byvec[i], " in ", project.area, sep = ""), xlab = "Year", ylab = "Percentage (%)", cex.main = 1.5, cex.lab = 1.2)
                }
                if(input$selectBy == "Bait"){
                  title(main = paste("Percentage of successful traps set using \n ", byvec[i], " in ", project.area, sep = ""), xlab = "Year", ylab = "Percentage (%)", cex.main = 1.5, cex.lab = 1.2)
                }
                if(input$selectBy == "Trap Type"){
                  title(main = paste("Percentage of successful traps set for \n ", byvec[i], " traps in ", project.area, sep = ""), xlab = "Year", ylab = "Percentage (%)", cex.main = 1.5, cex.lab = 1.2)
                }
              }else{
                ## If individual proportions are not selected
                if(select.by == "Person (trap checked)"){
                  title(paste("Catches discovered by ", byvec[i], "\n in ", project.area, sep = ""), xlab = "Year", ylab = "Number Caught", cex.main = 1.5, cex.lab = 1.2)
                }
                if(select.by == "Person (trap set)"){
                  title(paste("Successful traps set by ", byvec[i], "\n in ", project.area, sep = ""), xlab = "Year", ylab = "Number Caught", cex.main = 1.5, cex.lab = 1.2)
                }
                if(select.by == "Line"){
                  title(paste("Line catch data for ", byvec[i], "\n in ", project.area, sep = ""), xlab = "Year", ylab = "Number Caught", cex.main = 1.5, cex.lab = 1.2)
                }
                if(select.by == "Species"){
                  title(paste("Catch data for ", byvec[i], " in ", project.area, sep = ""), xlab = "Year", ylab = "Number Caught", cex.main = 1.5, cex.lab = 1.2)
                }
                if(select.by == "Bait"){
                  title(paste("Bait catch data for ", byvec[i], "\n in ", project.area, sep = ""), xlab = "Year", ylab = "Number Caught", cex.main = 1.5, cex.lab = 1.2)
                }
                if(select.by == "Trap Type"){
                  title(paste("Trap catch data for ", byvec[i], "\n in ", project.area, sep = ""), xlab = "Year", ylab = "Number Caught", cex.main = 1.5, cex.lab = 1.2)
                }
              }
            }

            ## Add dynamic legend
            par(mar=c(0, 0.1, 0.25, 0), mgp=c(0, 0, 0))
            plot(1, type="n", bty="n", xaxt="n", yaxt="n")
            legend("topleft", legend = legend.string, col = 1, pt.bg = colour.diff[1:length(subject.elements)], pch = 22, cex = auto.cex*0.8, pt.cex = auto.cex*1.75, xpd = TRUE, inset = c(0, 0))

          }
        }

        dev.off()
      }
    )

    ## Tables panel - updates when graphics are loaded
    observeEvent(input$submit, priority = 20, {

      output$tableTitle1 <- renderUI({
        ## Include year in title if applicable
        if(input$selectAggregate == "Month in a selected year"){
          h4(paste("Overall Data Table (", input$selectArea, " - ", input$selectSubject, " - ", input$selectYear, ")", sep = ""))
        }else{
          h4(paste("Overall Data Table (", input$selectArea, " - ", input$selectSubject, ")", sep = ""))
        }
      })

      ## Overall data table
      output$overallTable <- renderTable({
        ## print("Overall Table")

        working.data()$datTable
      })

      ## Individual tables title
      output$tableTitle2 <- renderUI({

        ## Don't appear if no selection
        if(input$selectBy != "No Selection"){
          ## Determine appropriate title
          if(length(working.data()$byvec > 1)){
            h4("Individual Data Tables:")
          }
          if(length(working.data()$byvec == 1)){
            h4("Individual Data Table:")
          }
        }

      })


      if(input$selectSubject != "No Selection"){
        ## Get the individual tables
        individual.tables <- working.data()$namesTables

        ## Get the Individual elements
        byvec <- working.data()$byvec
      }

      ########## Individual tables ########## (same as plots)
      output$individualTables <- renderUI({

        ## No individual tables if No Selection
        if(input$selectBy == "No Selection"){
          return(NULL)
        }

        ## print("Individual tables")

        ## Taken from individual plot code
        table_output_list <- lapply(1:length(byvec), function(i) {
          tablename <- paste("table", i, sep="")
          tableOutput(tablename)
        })

        # Convert the list to a tagList - this is necessary for the list of items
        # to display properly.
        do.call(tagList, table_output_list)
      })

      for (i in 1:length(byvec)){
        # Need local so that each item gets its own number. Without it, the value
        # of i in the renderPlot() will be the same across all instances, because
        # of when the expression is evaluated.
        local({
          my_i <- i

          tablename <- paste("table", my_i, sep = "")

          ## Call each table
          output[[tablename]] <- renderTable({

            ## Generate the correct table
            individual.tables[[my_i]]

            ## Caption code taken from:
            ## http://stackoverflow.com/questions/28003715/caption-in-rendertable-shiny
          }, caption = byvec[my_i],
             caption.placement = getOption("xtable.caption.placement", "top"),
             caption.width = getOption("xtable.caption.width", NULL))
        })
      }
    })

    ## Download Tables
    output$downloadTables <- downloadHandler(

      ## Download function and filename
      filename = paste("Tables.pdf", sep = ""),
      content = function(file){

        ## Give error if no graphics have been called
        if(input$submit == 0){
          stop("No graphics have been requested")
        }

        ## Open PDF device
        pdf(file = file, onefile = TRUE, width = 12, height = 8)

        ## Define byvec for individual table titles
        byvec <- working.data()$byvec

        ## Overall table
        textplot(working.data()$datTable)
        #write.csv(file = file, working.data()$datTable)

        ## Overall table title
        if(input$selectAggregate == "Month in a selected year"){
          title(main = paste("Overall Data Table (", input$selectArea, " - ", input$selectSubject, " - ", input$selectYear, ")", sep = ""), cex.main = 2)
        }else{
          title(main = paste("Overall Data Table (", input$selectArea, " - ", input$selectSubject, ")", sep = ""), cex.main = 2)
        }

        ########## Individual tables ########## (same as plots)
        ## Only produce tables if a valid selection has been made
        if(input$selectBy != "No Selection"){
          individual.tables <- working.data()$namesTables

          ## Cycle through each dable
          for(i in 1:length(individual.tables)){

            ## Add the individual table to the file
            textplot(working.data()$namesTables[[i]])
            #write.csv(working.data()$namesTables[[i]], append = TRUE)

            ## Add the individual table title
            title(main = paste("Individual data table for " , byvec[i], sep = ""), col = "grey", cex.main = 1.5)

          }
        }

        ## Close the PDF device
        dev.off()
      }

    )

    ## Help manual
    output$help <- renderText({

    })

  }
)
