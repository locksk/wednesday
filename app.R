
# Load packages
library(shiny)
library(bslib)
library(tidyverse)
library(ggplot2)
library(googlesheets4)
library(shinyWidgets)
library(shinyjs)
library(data.table)
library(googledrive)
library(gargle)
library(shinylive)
library(httpuv)

# load data
#setwd(dir = "D:/siobh/Documents/Uni/PhD/Misc/wednesday/")
#dat <- readxl::read_xlsx(path = "data.xlsx")
drive_auth(path = ".secrets/wednesday-447408-55ba7ae3d65c.json")
gs4_auth(path = ".secrets/wednesday-447408-55ba7ae3d65c.json")

main <- read_sheet("https://docs.google.com/spreadsheets/d/1PMq5uXQ4matv8rvERxLlRF4BR6VBzMC464JIGATqYlg/edit?gid=0#gid=0",sheet = 1)
now <- weekdays(Sys.Date())
main <- main %>% filter(!grepl(now, Closed))
main$Endorsed[!is.na(main$Endorsed)] <- "Yes"
main$Endorsed[is.na(main$Endorsed)] <- "No"

# Define UI for application and create theme
ui <- fluidPage(tags$script(src = "myscript.js"),
    theme = bs_theme(
        bg = "#E1F8D0",
        fg = "#233628",
        primary = "#4e4e4e",
        base_font = font_google("Space Mono"),
        code_font = font_google("Roboto Mono"),
        heading_font = font_google("Press Start 2P"),
        font_scale = 1.2
    ), 
    
    # Application title
    titlePanel(div(br(),
                   span("sbarc wednesday food selection tool", style = "padding-left: 45px, padding-top: 40px"),
                   style={'background-color:#E1F8D0; padding-left: 45px, padding-top: 40px'},
                   br(),
                   span(em(h6("(we love little treats)")), style = "color:grey"),
                   br()
                   #img(height = 100, width = 200,  src = "...")
                   )
                  ),
    
    # Sidebar with a drop-down menu to select Y & X variables. 
    sidebarLayout(
        sidebarPanel(
          pickerInput(
            inputId = "cuisineSelect", 
            label = "1) Select type of food:", 
            choices = na.omit(unique(main$Type)), 
            selected = na.omit(unique(main$Type)), 
            options = pickerOptions(
              actionsBox = TRUE, 
              size = 10,
              selectedTextFormat = "count > 3"
            ), 
            multiple = TRUE
          ),
          pickerInput(
            inputId = "deliverySelect", 
            label = "2) Select availability (delivery etc):", 
            choices = na.omit(unique(main$Delivery)), 
            selected = na.omit(unique(main$Delivery)), 
            options = pickerOptions(
              actionsBox = TRUE, 
              size = 10,
              selectedTextFormat = "count > 3"
            ), 
            multiple = TRUE
          ),
          pickerInput(
            inputId = "distanceSelect", 
            label = "3) Select distance:", 
            choices = na.omit(unique(main$Distance)), 
            selected = na.omit(unique(main$Distance)), 
            options = pickerOptions(
              actionsBox = TRUE, 
              size = 10,
              selectedTextFormat = "count > 3"
            ), 
            multiple = TRUE
          ),
          pickerInput(
            inputId = "timeSelect", 
            label = "4) Select time of day:", 
            choices = unique(main$Opens), 
            selected = unique(main$Opens),
            options = pickerOptions(
              actionsBox = TRUE, 
              size = 10,
              selectedTextFormat = "count > 3"
            ), 
            multiple = TRUE
          ),
          pickerInput(
            inputId = "endorsedSelect", 
            label = "5) Previously endorsed?", 
            choices = unique(main$Endorsed), 
            selected = unique(main$Endorsed), 
            options = pickerOptions(
              actionsBox = TRUE, 
              size = 10,
              selectedTextFormat = "count > 3"
            ), 
            multiple = TRUE
          ),
          
          
          div(style="display:inline-block;width:100%;text-align: center;",actionButton("go", label = "food pls", icon = icon("ghost"))),
          br(),
          br(),
          div(style="display:inline-block;width:100%;text-align: center;",actionButton("selectone", label = "help pls", icon = icon("hat-wizard"))),
          br(),
          br(),
          div(style="display:inline-block;width:100%;text-align: center;",actionButton("sheet", onclick ="window.open('https://docs.google.com/spreadsheets/d/1PMq5uXQ4matv8rvERxLlRF4BR6VBzMC464JIGATqYlg/edit?gid=0#gid=0', '_blank')", label = "archives pls", icon = icon("scroll")))
          
        ),
        mainPanel(
            tabsetPanel(type = "tabs",
                        tabPanel("Info", 
                                 br(),
                                 
                                 h4("Instructions."),
                                 p("1. Make selections using the sidebar, if you want to retain all options press 'Select All'."),
                                 p("2. Press the 'food pls' button to get three randomly selected options from the database. Note: only options that are open on the day should be presented (provided the database is correct, and the app is working - best double check before you go though just in case)."),
                                 p("3. If you still cannot decide, press the 'help pls' button to get further guidance (randomly selects Choice 1, 2, or 3 - no decision making whatsoever)."),
                                 br(),
                                 tags$figure(
                                   class = "centerFigure",
                                   tags$img(
                                     src = "maindy_map_labelled.png",
                                     width = 600,
                                     alt = "Map of Maindy/Cathays area with Hadyn Ellis Building Circled"
                                   ),
                                   tags$figcaption("Map of Maindy/Cathays area"),
                                 ),
                                 br(),
                                 h4("Database."),
                                 p("The dataset used by this Shiny App includes information on restaurants, cafes, and takeaways in the area surrounding Maindy Road, Cardiff."),
                                 p("Data includes, and can be filtered by:"),
                                 p("* Cost"),
                                 p("* Distance from the Hadyn Ellis Building."),
                                 p("* Opening hours"),
                                 p("* Endorsements."),
                                 p("* Food type."),
                                 br(),
                                 p("The dataset is in a google sheets file that can be accessed and edited by everyone with a link. This can be opened using the `archives` button in the sidebar, alternatively for a quick look just use the 'Database' tab within this app. When filling in the database with your suggestions please follow the instructions in the second sheet so that the data is nice and tidy (note: as of right now it is certainly not tidy but hopefully I will sort this soon!)."),
                                 a(href="https://dataviz.shef.ac.uk/blog/05/02/2021/Shiny-Template#use-templates", "This Shiny App was built using code from Dataviz.shef"),
                                 br(),
                                 br(),
                                 br(),
                                 ),
                                 #[code omitted]),
                        tabPanel("Results", tableOutput("results"), " ", verbatimTextOutput("help")),
                        tabPanel("Database", (DT::dataTableOutput(outputId = "table")))
        )
    )
  )
)

# Define server logic required to draw a plot.
server <- function(input, output) {
  
    dat <- eventReactive(input$go,{
      main %>% filter(Type %in% input$cuisineSelect) %>%
        filter(Delivery %in% input$deliverySelect) %>%
        filter(Distance %in% input$distanceSelect) %>%
        filter(Opens %in% input$timeSelect) %>%
        filter(Endorsed %in% input$endorsedSelect)
        
      })
  
    output$results <- renderTable ({
        print("Your food choices for today are: ")
              random_dat <- data.frame(Choice_1 = t(sample_n(dat(), 1)[1:6]),
                                 Choice_2 = t(sample_n(dat(), 1)[1:6]),
                                 Choice_3 = t(sample_n(dat(), 1)[1:6]))
        colnames(random_dat) <- c("Choice 1", "Choice 2", "Choice 3")
        rownames(random_dat) <- c("<strong> Place </strong>", "Type", "Location", "Distance", "Cost", "Extra Notes")
        print(random_dat)
    }, rownames = T, na = " "  )
    
    random_dat_col <- eventReactive(input$selectone,{
     sample(1:3, 1)
    })
    
    output$help <- renderText({
        print(paste0("Today you should pick Choice ", random_dat_col(), "!"))
      })
        
  #  output$summary <- renderPrint ({
   #       print(paste0("Dataset created by the psychosis team at Cardiff Univeristy.", '\n', "Data includes information on restaurants, cafes, and takeaways in the area surrounding Maindy Road, Cardiff.", '\n'))
    ##      print(paste0("Data includes, and can be filtered by: Cost, Opening Hours, Endorsements, and Type of Cuisine.", '\n'))
     #     print(paste0("Still to add: Distance from the Hadyn Ellis Building", '\n'))
    #      
      #    print(paste0("Based on the filter options, three options will be randomly selected from the dataset. If further help is needed to make a selection, use the 'help' button.", '\n', "Provided information in the google sheets is correct, all suggested places should be open on the day used. "))
     #     
     #     a(href="https://dataviz.shef.ac.uk/blog/05/02/2021/Shiny-Template#use-templates", "This Shiny App was build using code from Dataviz.shef")
      #      })
    
  #   h4("Dataset created by the psychosis team at Cardiff Univeristy"),
  #        h5("Data includes information on restaurants, cafes, and takeaways in the area surrounding Maindy Road, Cardiff."),
  #        h5("Data includes, and can be filtered by:"),
  #        tags$li("Cost and Distance from the Hadyn Ellis Building"),
   #       tags$li("Opening hours"),
   #       tags$li("Ratings and Endorsements"),
  #        tags$li("Cuisine"),
  #        h5("End goal is that the spreadsheet may be hosted on google docs so that everyone can update it."),
  #        a(href="https://dataviz.shef.ac.uk/blog/05/02/2021/Shiny-Template#use-templates", "This Shiny App was build using code from Dataviz.shef")
    
    output$table <- DT::renderDataTable({DT::datatable(main)})
    #output$table <- renderTable ({
     #   print("This is the raw dataset used for this Shiny App:")
     #   print(main)
   # })
}

# Run the application 
shinyApp(ui = ui, server = server)

