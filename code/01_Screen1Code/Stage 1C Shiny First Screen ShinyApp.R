#=======================================================================
# How to run
# Press Ctrl-A to select all, then run.  Shiny should start up
#
# IT WILL LOOK EMPTY.  You will have to press "next" to get to the first one
# 
# When you get bored close it down and everything is already saved - then upload to git
# To start again, just select all and run again.
#
# If you get errors, try closing R shiny and starting again. This genuinely works.
#=======================================================================
#-----------------------------------------------------------------------
# Clear the workspace and Load libraries
#-----------------------------------------------------------------------
rm(list=ls())
library(shiny)   ;  library(tidyverse)
library(DT)      ;  library(rmdformats)
library(stringr) ;  library(shinyWidgets)
library(RISmed)  ;  library(bibliometrix)
library(bib2df)  ;  library(knitr)
library(magrittr);  library(shinythemes)


#-----------------------------------------------------------------------
# Set up directories
# Annoyingly, it wants this to be hardwired. It should be where the main github folder is
#-----------------------------------------------------------------------
maindir     <- "~/Documents/GitHub/systematic-mapping-flash-floods-2024Clean"
Workingfile <- paste(maindir,"data/MainScreeningData_Screen1.RData",sep="/")

#-----------------------------------------------------------------------
# Load the data
#-----------------------------------------------------------------------
load(Workingfile)

#-----------------------------------------------------------------------
# Back-up dataset to one that is date/timestamped
#-----------------------------------------------------------------------
backupname <- paste("MainScreeningData_Screen1_",format(Sys.time(), "%Y%m%d-%H%M"),".RData",sep="")
save(data_bib, file = paste(maindir,"data/03_Screen1Backup",backupname,sep="/"))


#-----------------------------------------------------------------------
# If screening has already started, sort data so screened data is at the bottom
# Once each row is screened, the column screen1_assessed is set to TRUE for that paper
# So check if *any* of the "Screen1_assessed" rows are marked TRUE
#-----------------------------------------------------------------------
if(length(which(data_bib$Screen1_Assessed == TRUE)) > 1){
   # If so, sort to get them at the bottom
   data_bib <- data_bib[with(data_bib, order(Screen1_Assessed,Screen1_Reject)), ]
}


#=======================================================================
# Highlighting Rules
# Need to put in colorblind friendly options
# This could be made into a better function, where maybe we add in these options in an excel file
#=======================================================================
wordHighlightyellow   <- function(SuspWord,colH = "#FFE9A8") {paste0('<span style="background-color:',colH,'">',SuspWord,'</span>')}
wordHighlightgreen    <- function(SuspWord,colH = "#BEDDBA") {paste0('<span style="background-color:',colH,'">',SuspWord,'</span>')}
wordHighlightblue     <- function(SuspWord,colH = "#A3C4D9") {paste0('<span style="background-color:',colH,'">',SuspWord,'</span>')}
wordHighlightred      <- function(SuspWord,colH = "#CFA6B6") {paste0('<span style="background-color:',colH,'">',SuspWord,'</span>')}
wordHighlightdarkblue <- function(SuspWord,colH = "#BAB4D4") {paste0('<span style="background-color:',colH,'">',SuspWord,'</span>')}
wordHighlightgrey     <- function(SuspWord,colH = grey(0.9)) {paste0('<span style="background-color:',colH,'">',SuspWord,'</span>')}


helenhighlight <- function(YourData){
   YourData %<>% str_replace_all(regex("flash ", ignore_case = TRUE), wordHighlightyellow)
   YourData %<>% str_replace_all(regex("flash-", ignore_case = TRUE), wordHighlightyellow)
   YourData %<>% str_replace_all(regex("flash", ignore_case = TRUE), wordHighlightyellow)
   YourData %<>% str_replace_all(regex("floods", ignore_case = TRUE), wordHighlightyellow)
   YourData %<>% str_replace_all(regex("flooding", ignore_case = TRUE), wordHighlightyellow)
   YourData %<>% str_replace_all(regex("flood ", ignore_case = TRUE), wordHighlightyellow)
   YourData %<>% str_replace_all(regex("flood", ignore_case = TRUE), wordHighlightyellow)
   YourData %<>% str_replace_all(regex("flood-", ignore_case = TRUE), wordHighlightyellow)
   YourData %<>% str_replace_all(regex("risk", ignore_case = TRUE), wordHighlightyellow)
   YourData %<>% str_replace_all(regex("landslide", ignore_case = TRUE), wordHighlightyellow)
   YourData %<>% str_replace_all(regex("landslides", ignore_case = TRUE), wordHighlightyellow)
   YourData %<>% str_replace_all(regex("mudslide", ignore_case = TRUE), wordHighlightyellow)
   YourData %<>% str_replace_all(regex("mudslides", ignore_case = TRUE), wordHighlightyellow)
   
   YourData %<>% str_replace_all(regex("exposure", ignore_case = TRUE), wordHighlightgreen)
   YourData %<>% str_replace_all(regex("vulnerability", ignore_case = TRUE), wordHighlightgreen)
   YourData %<>% str_replace_all(regex("impacting", ignore_case = TRUE), wordHighlightgreen)
   YourData %<>% str_replace_all(regex("impacts", ignore_case = TRUE), wordHighlightgreen)
   YourData %<>% str_replace_all(regex("impact", ignore_case = TRUE), wordHighlightgreen)
   YourData %<>% str_replace_all(regex("focus group", ignore_case = TRUE), wordHighlightgreen)
   YourData %<>% str_replace_all(regex("stakeholder", ignore_case = TRUE), wordHighlightgreen)
   YourData %<>% str_replace_all(regex("culture", ignore_case = TRUE), wordHighlightgreen)
   YourData %<>% str_replace_all(regex("questionnaire", ignore_case = TRUE), wordHighlightgreen)
   YourData %<>% str_replace_all(regex("killed", ignore_case = TRUE), wordHighlightgreen)
   YourData %<>% str_replace_all(regex("deaths", ignore_case = TRUE), wordHighlightgreen)
   YourData %<>% str_replace_all(regex("death", ignore_case = TRUE), wordHighlightgreen)
   YourData %<>% str_replace_all(regex("damage", ignore_case = TRUE), wordHighlightgreen)
   YourData %<>% str_replace_all(regex("disaster", ignore_case = TRUE), wordHighlightgreen)
   YourData %<>% str_replace_all(regex("economic", ignore_case = TRUE), wordHighlightgreen)
   YourData %<>% str_replace_all(regex("social ", ignore_case = TRUE), wordHighlightgreen)
   YourData %<>% str_replace_all(regex("fatalities", ignore_case = TRUE), wordHighlightgreen)
   
   YourData %<>% str_replace_all(regex("modelling", ignore_case = TRUE), wordHighlightblue)
   YourData %<>% str_replace_all(regex("modeling", ignore_case = TRUE), wordHighlightblue)
   YourData %<>% str_replace_all(regex("models", ignore_case = TRUE), wordHighlightblue)
   YourData %<>% str_replace_all(regex("model", ignore_case = TRUE), wordHighlightblue)
   YourData %<>% str_replace_all(regex("dynamical ", ignore_case = TRUE), wordHighlightblue)
   YourData %<>% str_replace_all(regex("dynamics ", ignore_case = TRUE), wordHighlightblue)
   YourData %<>% str_replace_all(regex("hydro ", ignore_case = TRUE), wordHighlightblue)
   YourData %<>% str_replace_all(regex("dynamic ", ignore_case = TRUE), wordHighlightblue)
   YourData %<>% str_replace_all(regex("gis ", ignore_case = TRUE), wordHighlightblue)
   YourData %<>% str_replace_all(regex("remote sensing", ignore_case = TRUE), wordHighlightblue)
   YourData %<>% str_replace_all(regex("hydrological ", ignore_case = TRUE), wordHighlightblue)
   YourData %<>% str_replace_all(regex("hydrometeorologial ", ignore_case = TRUE), wordHighlightblue)
   YourData %<>% str_replace_all(regex("hydro", ignore_case = TRUE), wordHighlightblue)
   YourData %<>% str_replace_all(regex("discharge", ignore_case = TRUE), wordHighlightblue)
   
   YourData %<>% str_replace_all(regex("convective ", ignore_case = TRUE), wordHighlightdarkblue)
   YourData %<>% str_replace_all(regex("convection ", ignore_case = TRUE), wordHighlightdarkblue)
   YourData %<>% str_replace_all(regex("forecast ", ignore_case = TRUE), wordHighlightdarkblue)
   YourData %<>% str_replace_all(regex("weather",ignore_case = TRUE), wordHighlightdarkblue)
   YourData %<>% str_replace_all(regex("precipitation",ignore_case = TRUE), wordHighlightdarkblue)
   YourData %<>% str_replace_all(regex("rainfall",ignore_case = TRUE), wordHighlightdarkblue)
   YourData %<>% str_replace_all(regex(" rain",ignore_case = TRUE), wordHighlightdarkblue)
   YourData %<>% str_replace_all(regex("radar ",ignore_case = TRUE), wordHighlightdarkblue)
   
   YourData %<>% str_replace_all(regex(" compilation",ignore_case = TRUE), wordHighlightred)
   YourData %<>% str_replace_all(regex("atlas",ignore_case = TRUE), wordHighlightred)
   YourData %<>% str_replace_all(regex(month.name[1],ignore_case = TRUE), wordHighlightred)
   YourData %<>% str_replace_all(regex(month.name[2],ignore_case = TRUE), wordHighlightred)
   YourData %<>% str_replace_all(regex(month.name[3],ignore_case = TRUE), wordHighlightred)
   YourData %<>% str_replace_all(regex(month.name[4],ignore_case = TRUE), wordHighlightred)
   YourData %<>% str_replace_all(regex(month.name[6],ignore_case = TRUE), wordHighlightred)
   YourData %<>% str_replace_all(regex(month.name[7],ignore_case = TRUE), wordHighlightred)
   YourData %<>% str_replace_all(regex(month.name[8],ignore_case = TRUE), wordHighlightred)
   YourData %<>% str_replace_all(regex(month.name[9],ignore_case = TRUE), wordHighlightred)
   YourData %<>% str_replace_all(regex(month.name[10],ignore_case = TRUE), wordHighlightred)
   YourData %<>% str_replace_all(regex(month.name[11],ignore_case = TRUE), wordHighlightred)
   YourData %<>% str_replace_all(regex(month.name[12],ignore_case = TRUE), wordHighlightred)
   YourData %<>% str_replace_all(regex("190",ignore_case = TRUE), wordHighlightred)
   YourData %<>% str_replace_all(regex("191",ignore_case = TRUE), wordHighlightred)
   YourData %<>% str_replace_all(regex("192",ignore_case = TRUE), wordHighlightred)
   YourData %<>% str_replace_all(regex("193",ignore_case = TRUE), wordHighlightred)
   YourData %<>% str_replace_all(regex("194",ignore_case = TRUE), wordHighlightred)
   YourData %<>% str_replace_all(regex("195",ignore_case = TRUE), wordHighlightred)
   YourData %<>% str_replace_all(regex("196",ignore_case = TRUE), wordHighlightred)
   YourData %<>% str_replace_all(regex("197",ignore_case = TRUE), wordHighlightred)
   YourData %<>% str_replace_all(regex("198",ignore_case = TRUE), wordHighlightred)
   YourData %<>% str_replace_all(regex("199",ignore_case = TRUE), wordHighlightred)
   YourData %<>% str_replace_all(regex("200",ignore_case = TRUE), wordHighlightred)
   YourData %<>% str_replace_all(regex("201",ignore_case = TRUE), wordHighlightred)
   YourData %<>% str_replace_all(regex("202",ignore_case = TRUE), wordHighlightred)
}


#=======================================================================
# GUI function
#=======================================================================
ui <- fluidPage(
   
   
   #--------------------------------------------------------------------
   # Feel free to change the theme to serve your preferences, you'll be staring at this for a while
   # so might as well make it look good.
   theme = shinytheme("darkly"),
   #--------------------------------------------------------------------
   # Set up styles, this was me just messing around until I got something that
   # was easy on my eyes with chatgpt
   
   tags$head(
      tags$style(HTML("
      body {
        background-color: #212121; /* Overall background color */
        color: #E8E8E8; /* Default text color */
      }
      .custom-datatable-container {
        background-color: #E8E8E8; /* Light grey background for the DataTable */
        padding: 15px;
      }
      .custom-verbatim {
        font-family: 'Menlo-regular', Courier, monospace; /* Monospaced font */
        background-color: #2F2F2F; /* Background color for verbatim text output */
        color: #BBBBBB; /* Text color for verbatim text output */
        border-radius: 5px; /* Optional: Rounded corners */
        font-size: 0.7em; /* Decrease font size (adjust as needed) */
        overflow: auto; /* Ensure long text is scrollable */
      }
      .custom-header-indent {
        margin-left: 0pt;
        color: #41b6c4 /* header color
      }
    "))
   ),
   
   
   #--------------------------------------------------------------------
   # This creates a shiny sidebar.  There are other options
   sidebarLayout(
      sidebarPanel(
        #--------------------------------------------------------------------
         # Gold star
         #--------------------------------------------------------------------
         hr(),
         hr(),
         hr(),
         fluidRow(column( width = 12, tags$header(class = "custom-header-indent",tags$h4(tags$b("Gold star"))))),
         fluidRow( column(width = 12, materialSwitch(inputId = "goldstarButton", label = "Key paper! MUST INCLUDE",  value = FALSE,  width = "100%",  status = "danger"))),
         hr(),
        #--------------------------------------------------------------------
        # Discard
        #--------------------------------------------------------------------
         fluidRow(column( width = 12, tags$header(class = "custom-header-indent",tags$h4(tags$b("Discard Paper"))))),
         fluidRow( column(width = 12, materialSwitch(inputId = "discardButton", label = "REMOVE: Paper off-topic",  value = FALSE,  width = "100%",  status = "danger"))),
         fluidRow( column(width = 12, materialSwitch(inputId = "rainButton", label = HTML("REMOVE: Paper on rain/hydrology,<br>it's not on floods as a hazard"),  value = FALSE,  width = "100%",  status = "danger"))),
         hr(),
        
         #--------------------------------------------------------------------
         # Basic methods
         fluidRow(column( width = 12, tags$header(class = "custom-header-indent",tags$h4(tags$b("Paper Characteristics"))))),
         fluidRow( column(width = 12, materialSwitch(inputId = "modelButton", label = "Uses Models/Forecast/Maps",  value = FALSE,  width = "100%",  status = "danger"))),
         fluidRow( column(width = 12, materialSwitch(inputId = "socialButton", label = "Socio-political or Impact focus",  value = FALSE,  width = "100%",  status = "danger"))),
         fluidRow( column(width = 12, materialSwitch(inputId = "eventButton", label = "Paper about a specific flood event",  value = FALSE,  width = "100%",  status = "danger"))),
         hr(),
        
         #--------------------------------------------------------------------
         # The next button
         fluidRow(column( width = 12, tags$header(class = "custom-header-indent",tags$h4(tags$b("Issues/Next"))))),
         fluidRow( column(width = 12, materialSwitch(inputId = "missingdetailButton", label = "ISSUE: Abstract missing",  value = FALSE,  width = "100%",  status = "danger"))),
         fluidRow(textInput(inputId = "notesField", label = "Notes", value = "")),
         hr(),
         fluidRow(( column(width = 12,actionButton("nextButton", "Next"))))
      ),
      
      #--------------------------------------------------------------------
      # Where the main data resides. 
      # The table itself and whether it has been pre-screened
      # Additional information for screening
      mainPanel(
         hr(),
         div(class = "spacer"),
         div(
            class = "custom-datatable-container",
            DT::dataTableOutput("table")
         ),
         div(class = "spacer"),
         hr(),
         div(class = "spacer"),
         # Use a custom textOutput instead of verbatimTextOutput
         div(class = "custom-verbatim",
             uiOutput("moreInfo")
         ),
         div(class = "spacer"),
         htmlOutput("count"),
         htmlOutput("total")
      ), 
      
      #--------------------------------------------------------------------
      # Where the sidebar sits (left or right). 
      position="right"
   )
)

#=======================================================================
# Server
#=======================================================================
server <-  function(input,output,session){
   
   #--------------------------------------------------------------------
   # Create a "reactive value" which allows us to play with the output of a button click
   values <- reactiveValues(); values$count <- 0
   
   
   #--------------------------------------------------------------------
   # At the same time on a next click (bloody shiny), 
   # select the row you care about and highlight it
   highlighter <- eventReactive(
      {input$nextButton 
      },
      {
         updateMaterialSwitch(session=session, inputId="goldstarButton",value=FALSE)
         updateMaterialSwitch(session=session, inputId="missingdetailButton",value=FALSE)
         updateMaterialSwitch(session=session, inputId="discardButton",value=FALSE)
         updateMaterialSwitch(session=session, inputId="rainButton",value=FALSE)
         updateMaterialSwitch(session=session, inputId="modelButton",value=FALSE)
         updateMaterialSwitch(session=session, inputId="socialButton",value=FALSE)
         updateMaterialSwitch(session=session, inputId="eventButton",value=FALSE)
         updateTextInput(session=session, inputId="notesField", value = "")
         save(list="data_bib",file=Workingfile)
         
         print("-------------------------------------------------------")
         print(paste("Current Count: ", values$count))
         print(data_bib[values$count, c("TI")])
         print(data_bib[values$count, c("Screen1_Reject","Screen1_RejectRain",
                                             "Screen1_Event","Screen1_Model","Screen1_Social",
                                             "Screen1_MissingDetails","Screen1_GoldStar")])
         
         
         #-----------------------------------------------------
         # If the row number is not at the end, increment up
         # THIS IS *REALLY BAD CODING*, ADDED IN BECAUSE IT WANTS TO RECALCULATE THE VALUE.
         # if(sum(c(input$discardButton,input$rainButton,input$modelButton,input$socialButton))>0){
         if(values$count != nrow(data_bib)){
            #-----------------------------------------------------
            # move to the next row
            values$count <- values$count + 1
            
            #-----------------------------------------------------
            # choose that row in the table
            YourData <- data_bib[values$count,c("TI","AB")]
            
            if (is.null(YourData) || nrow(YourData) == 0) {
               print("YourData is empty!")
               return(NULL)  # Return NULL to prevent further processing
            }
            
            YourData2 <- helenhighlight(YourData)
            #print(YourData2)
           
            #-----------------------------------------------------
            # Output to data_bib   
            # Set that the screening has been assessed.
            data_bib$Screen1_Assessed      [values$count-1] <<- TRUE
            data_bib$Screen1_GoldStar[values$count-1]       <<- input$goldstarButton
            data_bib$Screen1_Reject        [values$count-1] <<- input$discardButton
            data_bib$Screen1_RejectRain    [values$count-1] <<- input$rainButton
            data_bib$Screen1_Event         [values$count-1] <<- input$eventButton
            data_bib$Screen1_Model         [values$count-1] <<- input$modelButton
            data_bib$Screen1_Social        [values$count-1] <<- input$socialButton
            data_bib$Screen1_MissingDetails[values$count-1] <<- input$missingdetailButton
            data_bib$Screen1_Notes         [values$count-1] <<- input$notesField

            return(YourData2)
         }
         #-----------------------------------------------------
         # Or put the final row
         else{
            YourData <- data_bib[ nrow(data_bib),c("TI","AB")]
            YourData2 <- helenhighlight(YourData)
            
            #-----------------------------------------------------
            # Output to data_bib         
            data_bib$Screen1_Assessed       [nrow(data_bib)] <<- TRUE
            data_bib$Screen1_GoldStar       [nrow(data_bib)] <<- input$goldstarButton
            data_bib$Screen1_Reject         [nrow(data_bib)] <<- input$discardButton
            data_bib$Screen1_RejectRain     [nrow(data_bib)] <<- input$rainButton
            data_bib$Screen1_Event          [nrow(data_bib)] <<- input$eventButton
            data_bib$Screen1_Model          [nrow(data_bib)] <<- input$modelButton
            data_bib$Screen1_Social         [nrow(data_bib)] <<- input$socialButton
            data_bib$Screen1_MissingDetails [nrow(data_bib)] <<- input$missingdetailButton
            data_bib$Screen1_Notes          [nrow(data_bib)] <<- input$notesField
            return(YourData2)
         }
      })  
   
   
   
   
   #--------------------------------------------------------------------
   # Output the table to the GUI
   
   output$table <- DT::renderDataTable({ data <- highlighter()}, escape = FALSE,options = list(dom = 't',bSort=FALSE)) 
   output$reject <- renderUI({ HTML(paste(highlightExclude(), sep = '<br/>'))})
   
   #--------------------------------------------------------------------
   # More info about the screening selections
   hr()
   output$moreInfo <- renderUI({
      HTML(paste(
         "<strong>HOW TO SCREEN</strong><br><br>",
         "<div style='margin-left: 20px;'>", # Indent the entire section
         "<strong>1. GOLD STAR</strong> [YES means] - The paper is extremely relevant to our analysis<br>",
         "<div style='margin-left: 160px;'>AKA it directly studies flash flood impact/vulnerability/response<br></div><br>",
         
         "<strong>2. DISCARD: [YES] -</strong> Discard this paper. Off-topic/Not relevant at all<br>",
         "<div style='margin-left: 160px;'>No relevant content is discussed in this paper.<br></div><br>",
         
         "<strong>3. DISCARD_RAIN: [YES] -</strong> Discard this paper because it's just about<br>>",
         "<div style='margin-left: 160px;'>hydrology/water/rain in general, AKA this paper<br<br></div><br>",
         "<div style='margin-left: 160px;'>does not study floods as a hazard.<br></div><br>",
         
         "<strong>4. TOPICS:</strong><br><br>", 
         "If you're keeping the paper, then use these toggles if it matches one of these families:<br><br>",
         "<div style='margin-left: 40px;'><strong>A.</strong> Uses Models/Forecast/Maps: [YES] - Observing, forecasting, or mapping floods<br></div>",
         "<div style='margin-left: 40px;'><strong>B.</strong> Socio-political or Impact focus: [YES] - How people interact, respond, or communicate about floods<br></div>",
         "<div style='margin-left: 40px;'><strong>C.</strong> Event: [YES] - the paper is written about one or more actual historical flood events<br></div>",
         "</div>",
         
         "<strong>2. MISSING DETAILS/NOTES: [YES] -</strong> The abstract is missing and needs to be added<br>",
         "<div style='margin-left: 160px;'>This needs to be added before proceeding with the analysis.<br></div><br>",
         
         sep = ""
      ))
   })
   hr()
   output$count <- renderUI({ HTML(paste("You have reviewed", (values$count - 2) ,"papers in this session")) })
   output$total <- renderUI({HTML(paste("In total, we have reviewed", (sum(data_bib$Screen1_Assessed, na.rm = TRUE)), "of", (length(data_bib$Screen1_Assessed))))})
}

#=======================================================================
# Run the server
#=======================================================================

# Run the application 
shinyApp(ui = ui, server = server)

