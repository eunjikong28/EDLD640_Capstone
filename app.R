
library(bslib)
library(here)
library(rio)
library(rvest)
library(shiny)
library(sortable)
library(stringr)
library(tidyverse)
library(shinyjs)


# Import data ----
## Web scrapping of data is in datasets.R file
all_courses_uo <- data.table::fread(here("datasets/all_course_uo.csv"), header = TRUE, showProgress = F)
cds_progression <- data.table::fread(here("datasets/cds_progression.csv"), header = TRUE, showProgress = F)
departments <- data.table::fread(here("datasets/departments.csv"), header = TRUE, showProgress = F)[,2]
uo_req_wide <- import(here("datasets/uo_req_courses_wide.csv"))
cds_req <- import(here("datasets/CDS undergrad req course.xlsx"))
uo_lang <- import(here("datasets/uo_lang.xlsx"))

# USER INTERFACE ----
ui <- tagList(
  
  ## Initial setup ----
  tags$style(HTML("
    .nav-link {
      font-size: 15px !important;
    }  ")), 

  div(
    style = "background: linear-gradient(to bottom, #005020 0%, #007030 100%); padding: 30px; position: relative;",
    
    
    img(src = "uo-logo-white.png", height = "auto", style = "max-height: 60px;"),
    
    div(
      style = "position: absolute; right: 0px; top: 0%; ;
              background-color: #005020; padding: 8px 30px; border-radius: 8px;
              display: flex; gap: 50px;",
      
      a("One Stop", href = "https://onestop.uoregon.edu/?utm_source=banner-module&utm_campaign=banner", style = "color: #D8DCDA ; text-decoration: none; "),
      a("Apply", href = "https://www.uoregon.edu/admissions-and-financial-aid?utm_source=banner-module&utm_campaign=banner", style = "color: #D8DCDA ; text-decoration: none; "),
      a("Visit", href = "https://visit.uoregon.edu/?utm_source=banner-module&utm_campaign=banner", style = "color: #D8DCDA ; text-decoration: none;"),
      a("Give", href = "https://giving.uoregon.edu/s/1540/22-it/home.aspx?gid=2&pgid=61", style = "color: #D8DCDA ; text-decoration: none; "),
    )
  ),
  
  
  page_navbar(
    
    tags$style(HTML("
    .nav-link {
      color: green !important;
    }
  ")),
    ## Home Page ----
    nav_panel("Home",
              fluidRow(
                tags$div(
                  style = "display: flex; justify-content: center;",
                  img(src = "planning.png", alt = "Planning", 
                      style = "width: 100vw; height: auto; display: block; margin: 0; padding: 0;")
                ),
                
                div(
                  style = "text-align: center;",
                  br(),
                  h1("Welcome to the University of Oregon Course Planner!"),
                  br(),
                  br(),
                  h3("To get started, please enter your major, select your degree type (BA or BS), and choose your matriculation year. This information helps generate a personalized course plan based on the specific requirements for your program. Once you submit, you’ll be able to organize your courses and track your academic progress effectively."),
                  br(),
                  br(),
                ),
                
                fluidRow(
                  column(width = 4,
                         selectInput("major", "Your Major", 
                                     choices = c("Communication Disorders and Sciences", "Educational Foundations"))),
                  column(width = 4,
                         selectInput("degree", "Type of Degree", 
                                     choices = c("BA", "BS"))),
                  column(width = 4,
                         numericInput("year", "Matriculation Year", value = 2000, min = 2000)),
                  
                ),
                
                actionButton("submit_info", "Submit")
              ),
              fluidRow(
                style = "background-color: #222222; padding: 10px;",  # Black background + padding for spacing
                column(width = 2),
                column(width = 2, 
                       br(),
                       br(),
                       p("CAMPUS", style = "color: white; font-weight: bold;margin-bottom: 3px;"),
                       div(
                         a("News", href = "https://news.uoregon.edu/", style = "color: #FEE11A ; text-decoration: none;"),
                         span(style = "color: white; padding: 0 10px;", "|"),
                         a("Events", href = "https://calendar.uoregon.edu/", style = "color: #FEE11A ; text-decoration: none;"),
                       ),
                       div(a("Maps", href = "https://www.uoregon.edu/maps", style = "color: #FEE11A ; text-decoration: none;"),
                           span(style = "color: white; padding: 0 10px;", "|"),
                           a("Direction", href = "https://www.uoregon.edu/directions", style = "color: #FEE11A ; text-decoration: none;")),
                       a("A-Z Index", href = "https://www.uoregon.edu/azindex", style = "color: #FEE11A ; text-decoration: none; display: block;"),
                       a("Find People", href = "https://www.uoregon.edu/findpeople", style = "color: #FEE11A ; text-decoration: none; display: block;"),
                       a("Report a Concern", href = "https://www.uoregon.edu/reportaconcern", style = "color: #FEE11A ; text-decoration: none; display: block;"),
                       br(),
                ),
                column(width = 2,
                       br(),
                       br(),
                       p("TOOLS", style = "color: white; font-weight: bold; bold; margin-bottom: 3px;"),
                       a("Webmail", href = "https://webmail.uoregon.edu/", style = "color: #FEE11A ; text-decoration: none; display: block;"),
                       a("UOmail", href = "https://uomail.uoregon.edu/", style = "color: #FEE11A ; text-decoration: none; display: block;"),
                       a("Canvas", href = "https://canvas.uoregon.edu/", style = "color: #FEE11A ; text-decoration: none; display: block;"),
                       a("Duckweb", href = "https://duckweb.uoregon.edu/", style = "color: #FEE11A ; text-decoration: none; display: block;"),
                       br(),),
                column(width = 2,
                       br(),
                       br(),
                       p("RESOURCES", style = "color: white; font-weight: bold;margin-bottom: 3px;"),
                       a("Class Schedule", href = "http://classes.uoregon.edu/", style = "color: #FEE11A ; text-decoration: none; display: block;"),
                       a("Academic Calendar", href = "http://registrar.uoregon.edu/calendars/academic", style = "color: #FEE11A ; text-decoration: none; display: block;"),
                       a("UO Libraries", href = "http://library.uoregon.edu/", style = "color: #FEE11A ; text-decoration: none; display: block;"),
                       a("Media Relations", href = "https://news.uoregon.edu/expert/journalists", style = "color: #FEE11A ; text-decoration: none; display: block;"),
                       br(),),
                column(width = 3,
                       br(),
                       br(),
                       p("University of Oregon", style = "color: white; font-weight: bold;margin-bottom: 3px;"),
                       p("1585 E 13th Ave.", style = "color: white; margin-bottom: 2px;"),
                       p("Eugene, OR 97403", style = "color: white;margin-bottom: 2px;"),
                       br(),
                       p("P: 541-346-1000", style = "color: white;margin-bottom: 2px;"),
                       br(),
                       a("Contact Us", href = "https://www.uoregon.edu/contact", style = "color: #FEE11A ; text-decoration: none; display: block;"),
                       br(),
                       br(),
                ),
                column(width = 1)
              )
    ),
    
    ## Selection Page----
    nav_panel("Course Selection",
              useShinyjs(),  # Initialize shinyjs
              
              fluidRow(
                column(width = 3, 
                       
                       # Department selection
                       selectInput(inputId="department",
                                   label="Which department courses would you like to see",
                                   choices = departments,
                                   selected = "AEIS"),
                       # Search
                       textInput(
                         inputId = "subsetChooseListText",
                         label = "Search courses",
                         value = ""),
                       
                       actionButton("reset", "Reset"),
                ),
                column(width = 9, 
                       h4("Now it’s time to map out your courses! Drag and drop the courses you have taken or plan to take into the appropriate term boxes. At the bottom, you'll find an example course progression to guide you in structuring your schedule. Use this tool to ensure a balanced workload each term and to stay on track for graduation."),
                       h4(a("Duck on Track", href = "https://advising.uoregon.edu/ducksontrack", style = "color: #007030;", target = "_blank"), "is another useful tool to check your progress toward requirement completion"))
              ),

              fluidRow( #1
                
                tags$style(HTML("

                      /* When an item is dragged */
                      .sortable-chosen {
                        background-color: #d4edda !important; /* Light green */
                        border: 2px solid #28a745 !important; /* Darker green border */
                      }
                    ")), 
                
                div(
                  class = "bucket-list-container default-sortable",
                  
                  ### Original list ----
                  div(
                    class = "default-sortable bucket-list bucket-list-horizontal",
                    
                    fluidRow( #2
                      column(width = 9,
                             fluidRow( #3
                               column(width = 3,
                                      div(
                                        style = "
                                  max-height: 500px;
                                  overflow-y: auto;
                                  max-width: 400px;",
                                        uiOutput("selection_list", style="flex:1 0 200px;")),),
                               column(width = 9,
                                      fluidRow( #5
                                        # Year 1
                                        column(width = 3,
                                               rank_list(
                                                 text = textOutput("year1_fall"),
                                                 labels = c(),
                                                 input_id = "rank_list_2",
                                                 options = sortable_options(group = "mygroup")
                                               ),
                                               rank_list(
                                                 text = textOutput("year1_winter"),
                                                 labels = c(),
                                                 input_id = "rank_list_3",
                                                 options = sortable_options(group = "mygroup")
                                               ),
                                               rank_list(
                                                 text = textOutput("year1_spring"),
                                                 labels = c(),
                                                 input_id = "rank_list_4",
                                                 options = sortable_options(group = "mygroup")
                                               ),),
                                        # Year 2
                                        column(width = 3,
                                               rank_list(
                                                 text = textOutput("year2_fall"),
                                                 labels = c(),
                                                 input_id = "rank_list_5",
                                                 options = sortable_options(group = "mygroup")
                                               ),
                                               rank_list(
                                                 text = textOutput("year2_winter"),
                                                 labels = c(),
                                                 input_id = "rank_list_6",
                                                 options = sortable_options(group = "mygroup")
                                               ),
                                               rank_list(
                                                 text = textOutput("year2_spring"),
                                                 labels = c(),
                                                 input_id = "rank_list_7",
                                                 options = sortable_options(group = "mygroup")
                                               )),
                                        # Year 3
                                        column(width = 3,
                                               rank_list(
                                                 text = textOutput("year3_fall"),
                                                 labels = c(),
                                                 input_id = "rank_list_8",
                                                 options = sortable_options(group = "mygroup")
                                               ),
                                               rank_list(
                                                 text = textOutput("year3_winter"),
                                                 labels = c(),
                                                 input_id = "rank_list_9",
                                                 options = sortable_options(group = "mygroup")
                                               ),
                                               rank_list(
                                                 text = textOutput("year3_spring"),
                                                 labels = c(),
                                                 input_id = "rank_list_10",
                                                 options = sortable_options(group = "mygroup")
                                               ),),
                                        # Year 4
                                        column(width = 3,
                                               rank_list(
                                                 text = textOutput("year4_fall"),
                                                 labels = c(),
                                                 input_id = "rank_list_11",
                                                 options = sortable_options(group = "mygroup")
                                               ),
                                               rank_list(
                                                 text = textOutput("year4_winter"),
                                                 labels = c(),
                                                 input_id = "rank_list_12",
                                                 options = sortable_options(group = "mygroup")
                                               ),
                                               rank_list(
                                                 text = textOutput("year4_spring"),
                                                 labels = c(),
                                                 input_id = "rank_list_13",
                                                 options = sortable_options(group = "mygroup")
                                               ),)
                                      ),
                                      fluidRow( #6
                                        column(width = 10),
                                        column(width = 2,
                                               #actionButton("submit_course", "Submit")
                                               )
                                      ))
                             ),
                             
                             ### Typical progression ---- 
                             fluidRow( #4
                               column(width = 12,
                                      plotOutput("typical", height = "750px", width = "1050px"))
                             )),
                      ### Req check ----
                      column(width = 3,
                             tabsetPanel(
                                         tabPanel("UO Requirement",
                                                  tableOutput("uo_requirement_check"),
                                                  a("For more information, go to the UO requirement page", href = "https://registrar.uoregon.edu/graduation/degree-requirements", style = "color: #007030;",target = "_blank"),),
                                         tabPanel("CDS Requirement",
                                                  tableOutput("cds_requirement_check"),
                                                  a("For more information, go to the CDS page", href = "https://education.uoregon.edu/cds/undergraduate/ba", style = "color: #007030;",target = "_blank"),),)
                             )
                    )
                    
                    
                    
                    
                  )
                )
              ),
              ### Footer ----
              fluidRow(
                style = "background-color: #222222; padding: 10px;",  # Black background + padding for spacing
                column(width = 2),
                column(width = 2, 
                       br(),
                       br(),
                       p("CAMPUS", style = "color: white; font-weight: bold;margin-bottom: 3px;"),
                       div(
                         a("News", href = "https://news.uoregon.edu/", style = "color: #FEE11A ; text-decoration: none;"),
                         span(style = "color: white; padding: 0 10px;", "|"),
                         a("Events", href = "https://calendar.uoregon.edu/", style = "color: #FEE11A ; text-decoration: none;"),
                       ),
                       div(a("Maps", href = "https://www.uoregon.edu/maps", style = "color: #FEE11A ; text-decoration: none;"),
                           span(style = "color: white; padding: 0 10px;", "|"),
                           a("Direction", href = "https://www.uoregon.edu/directions", style = "color: #FEE11A ; text-decoration: none;")),
                       a("A-Z Index", href = "https://www.uoregon.edu/azindex", style = "color: #FEE11A ; text-decoration: none; display: block;"),
                       a("Find People", href = "https://www.uoregon.edu/findpeople", style = "color: #FEE11A ; text-decoration: none; display: block;"),
                       a("Report a Concern", href = "https://www.uoregon.edu/reportaconcern", style = "color: #FEE11A ; text-decoration: none; display: block;"),
                       br(),
                ),
                column(width = 2,
                       br(),
                       br(),
                       p("TOOLS", style = "color: white; font-weight: bold; bold; margin-bottom: 3px;"),
                       a("Webmail", href = "https://webmail.uoregon.edu/", style = "color: #FEE11A ; text-decoration: none; display: block;"),
                       a("UOmail", href = "https://uomail.uoregon.edu/", style = "color: #FEE11A ; text-decoration: none; display: block;"),
                       a("Canvas", href = "https://canvas.uoregon.edu/", style = "color: #FEE11A ; text-decoration: none; display: block;"),
                       a("Duckweb", href = "https://duckweb.uoregon.edu/", style = "color: #FEE11A ; text-decoration: none; display: block;"),
                       br(),),
                column(width = 2,
                       br(),
                       br(),
                       p("RESOURCES", style = "color: white; font-weight: bold;margin-bottom: 3px;"),
                       a("Class Schedule", href = "http://classes.uoregon.edu/", style = "color: #FEE11A ; text-decoration: none; display: block;"),
                       a("Academic Calendar", href = "http://registrar.uoregon.edu/calendars/academic", style = "color: #FEE11A ; text-decoration: none; display: block;"),
                       a("UO Libraries", href = "http://library.uoregon.edu/", style = "color: #FEE11A ; text-decoration: none; display: block;"),
                       a("Media Relations", href = "https://news.uoregon.edu/expert/journalists", style = "color: #FEE11A ; text-decoration: none; display: block;"),
                       br(),),
                column(width = 3,
                       br(),
                       br(),
                       p("University of Oregon", style = "color: white; font-weight: bold;margin-bottom: 3px;"),
                       p("1585 E 13th Ave.", style = "color: white; margin-bottom: 2px;"),
                       p("Eugene, OR 97403", style = "color: white;margin-bottom: 2px;"),
                       br(),
                       p("P: 541-346-1000", style = "color: white;margin-bottom: 2px;"),
                       br(),
                       a("Contact Us", href = "https://www.uoregon.edu/contact", style = "color: #FEE11A ; text-decoration: none; display: block;"),
                       br(),
                       br(),
                       
                ),
                column(width = 1)
              )
              
              
    ),
    
    
    
    
    ## Course Output Page ----
    # nav_panel("Course Planner", 
    #           layout_sidebar(
    #             sidebar = sidebarPanel(
    #               #selectInput("analysis", "Type of analysis:", choices = c("Analysis 1", "Analysis 2")),
    #               
    #               style = "height: 400px; width: 200px; padding: 20px; border: 1px solid #ccc; background-color: #f9f9f9;"
    #             ),
    #             mainPanel(
    #               h3("Course Plan Visualization"),
    #               
    #               # tags$p("All courses"),
    #               # tableOutput("requirement_check"),
    #               
    #               tags$p("Year 1 Fall"),
    #               verbatimTextOutput("results_2"),
    #               
    #               tags$p("Year 1 Winter"),
    #               verbatimTextOutput("results_3"),
    #               
    #               tags$p("Year 1 Spring"),
    #               verbatimTextOutput("results_4"),
    #               
    #               tags$p("Year 2 Fall"),
    #               verbatimTextOutput("results_5"),
    #               
    #               tags$p("Year 2 Winter"),
    #               verbatimTextOutput("results_6"),
    #               
    #               tags$p("Year 2 Spring"),
    #               verbatimTextOutput("results_7"),
    #               
    #               tags$p("Year 3 Fall"),
    #               verbatimTextOutput("results_8"),
    #               
    #               tags$p("Year 3 Winter"),
    #               verbatimTextOutput("results_9"),
    #               
    #               tags$p("Year 3 Spring"),
    #               verbatimTextOutput("results_10"),
    #               
    #               tags$p("Year 4 Fall"),
    #               verbatimTextOutput("results_11"),
    #               
    #               tags$p("Year 4 Winter"),
    #               verbatimTextOutput("results_12"),
    #               
    #               tags$p("Year 4 Spring"),
    #               verbatimTextOutput("results_13"),
    #               
    #               #plotOutput("typical", height = "750px", width = "1400px")
    #               
    #             )
    #           )), 
    
    id = "page"
  ),
  tags$script("
    Shiny.addCustomMessageHandler('resetInputValue', function(name) {
      Shiny.setInputValue(name, null);
    });
  "))



# SERVER ----
server <- function(input, output, session) {
  
  ## Page 1 ----
  ### After pressing submit into----
  observeEvent(input$submit_info, {
    updateNavlistPanel(session, "page", selected = "Course Selection")
  })
  
  ## Page 2 ----
  ### Title of Table in Page 2 ----
  
  output$year1_fall <- renderText({
    paste("Fall", input$year)
  })
  output$year1_winter <- renderText({
    paste("Winter", input$year+1)
  })
  output$year1_spring <- renderText({
    paste("Spring", input$year+1)
  })
  
  output$year2_fall <- renderText({
    paste("Fall", input$year+1)
  })
  output$year2_winter <- renderText({
    paste("Winter", input$year+2)
  })
  output$year2_spring <- renderText({
    paste("Spring", input$year+2)
  })
  
  output$year3_fall <- renderText({
    paste("Fall", input$year+2)
  })
  output$year3_winter <- renderText({
    paste("Winter", input$year+3)
  })
  output$year3_spring <- renderText({
    paste("Spring", input$year+3)
  })
  
  output$year4_fall <- renderText({
    paste("Fall", input$year+3)
  })
  output$year4_winter <- renderText({
    paste("Winter", input$year+4)
  })
  output$year4_spring <- renderText({
    paste("Spring", input$year+4)
  })
  
  
  ### Department choice in Page 2 ----
  
  varList <- reactive({
    req(input$department)
    resetToken()  # Force dependency on reset
    all_courses_uo |> 
      filter(str_detect(department, paste0("^", input$department))) |> 
      mutate(output = paste0(course_code,": ", title)) |> 
      pull(output)
  })
  
  
  
  ### Search in Page 2 ----
  subsetChooseList <- reactive({
    resetToken()  # Force dependency on reset
    
    items <- varList()
    pattern <- input$subsetChooseListText
    if (nchar(pattern) < 1) {
      return(items)
    }
    items[
      grepl(
        x = items,
        pattern = input$subsetChooseListText,
        ignore.case = TRUE
      )
    ]
  })
  
  ### Buckets  ----
  output$selection_list <- renderUI({
    resetToken()  # Force dependency on reset
    
    labels <- subsetChooseList() #labels should be from search list
    
    # remove already chosen items
    # labels <- labels[!(
    #   labels %in% input$rank_list_2 |
    #     labels %in% input$rank_list_3
    # )]
    rank_list(
      text = "Course Options",
      labels = labels,
      input_id = "rank_list_1",
      options = sortable_options(group = "mygroup")
    )
  })
  
  
  ### Reset ----
  
  # Create a reset token to force reactives to update
  resetToken <- reactiveVal(0)
  
  # More reliable way to reset the UI
  observeEvent(input$reset, {
    # Reset the input fields
    updateSelectInput(session, "department", selected = "AEIS") 
    updateTextInput(session, "subsetChooseListText", value = "")
    
    # Clear the visual rank lists using JavaScript
    runjs("
      // Clear rank_list_2
      $('#rank_list_2').find('.rank-list-item').remove();
      $('#rank_list_2').trigger('change');
      // Clear rank_list_3
      $('#rank_list_3').find('.rank-list-item').remove();
      $('#rank_list_3').trigger('change');
      // Clear rank_list_4
      $('#rank_list_4').find('.rank-list-item').remove();
      $('#rank_list_4').trigger('change');
      
      // Clear rank_list_5
      $('#rank_list_5').find('.rank-list-item').remove();
      $('#rank_list_5').trigger('change');
      // Clear rank_list_6
      $('#rank_list_6').find('.rank-list-item').remove();
      $('#rank_list_6').trigger('change');
      // Clear rank_list_7
      $('#rank_list_7').find('.rank-list-item').remove();
      $('#rank_list_7').trigger('change');
      
      // Clear rank_list_8
      $('#rank_list_8').find('.rank-list-item').remove();
      $('#rank_list_8').trigger('change');
      // Clear rank_list_9
      $('#rank_list_9').find('.rank-list-item').remove();
      $('#rank_list_9').trigger('change');
      // Clear rank_list_10
      $('#rank_list_10').find('.rank-list-item').remove();
      $('#rank_list_1').trigger('change');
      
       // Clear rank_list_11
      $('#rank_list_11').find('.rank-list-item').remove();
      $('#rank_list_11').trigger('change');
      // Clear rank_list_12
      $('#rank_list_12').find('.rank-list-item').remove();
      $('#rank_list_12').trigger('change');
      // Clear rank_list_13
      $('#rank_list_13').find('.rank-list-item').remove();
      $('#rank_list_13').trigger('change');
      
    ")
    
    # Reset the actual input values in Shiny
    session$sendCustomMessage("resetInputValue", "rank_list_1")
    session$sendCustomMessage("resetInputValue", "rank_list_2")
    session$sendCustomMessage("resetInputValue", "rank_list_3")
    session$sendCustomMessage("resetInputValue", "rank_list_4")
    
    session$sendCustomMessage("resetInputValue", "rank_list_5")
    session$sendCustomMessage("resetInputValue", "rank_list_6")
    session$sendCustomMessage("resetInputValue", "rank_list_7")
    
    session$sendCustomMessage("resetInputValue", "rank_list_8")
    session$sendCustomMessage("resetInputValue", "rank_list_9")
    session$sendCustomMessage("resetInputValue", "rank_list_10")
    
    session$sendCustomMessage("resetInputValue", "rank_list_11")
    session$sendCustomMessage("resetInputValue", "rank_list_12")
    session$sendCustomMessage("resetInputValue", "rank_list_13")
    
    # Increment reset token to force reactives to update
    resetToken(resetToken() + 1)
    
  })
  
  
  ### Typical class progression ----
  output$typical <- renderPlot(
    ggplot(cds_progression, aes(x = year, y = factor(identifier1, levels = rev(unique(identifier1))), text = label)) + 
      geom_label(aes(label = str_wrap(label, width = 28)), 
                 fill = cds_progression$color, color = "black", alpha = 0.5, size = 5) +  # Increase size here
      ggtitle("Typical CDS Course Progression") +
      scale_x_continuous(limits = c(0.5, 4.5), breaks = 1:4, labels = paste("Year", 1:4)) + 
      theme_minimal() +
      theme(
        axis.title.y = element_blank(),  
        axis.title.x = element_blank(), 
        axis.text.x = element_text(size = 20, color = "black"),  
        axis.text.y = element_text(size = 15, color = "black"),
        panel.grid = element_blank(),                 # Remove grid lines
        panel.border = element_rect(colour = "black", fill=NA, linewidth=1),
        plot.title = element_text(size = 20, hjust = 0.5)
      )
  )
  
  ### After pressing submit course ----
  observeEvent(input$submit_course, {
    updateNavlistPanel(session, "page", selected = "Course Planner")
  })
  
  
  
  ## Page 3 ----
  
  ### Checking inputs ----
  output$results_2 <- renderPrint({resetToken()
    input$rank_list_2})
  output$results_3 <- renderPrint({resetToken()
    input$rank_list_3})
  output$results_4 <- renderPrint({resetToken()
    input$rank_list_4})
  
  output$results_5 <- renderPrint({resetToken()
    input$rank_list_5})
  output$results_6 <- renderPrint({resetToken()
    input$rank_list_6})
  output$results_7 <- renderPrint({resetToken()
    input$rank_list_7})
  
  output$results_8 <- renderPrint({resetToken()
    input$rank_list_8})
  output$results_9 <- renderPrint({resetToken()
    input$rank_list_9})
  output$results_10 <- renderPrint({resetToken()
    input$rank_list_10})
  
  output$results_11 <- renderPrint({resetToken()
    input$rank_list_11})
  output$results_12 <- renderPrint({resetToken()
    input$rank_list_12})
  output$results_13 <- renderPrint({resetToken()
    input$rank_list_13})
  
  ### Total inputs df ----
  total_inputs <- reactive({
    df <- data.frame(courses = c(input$rank_list_2, input$rank_list_3, input$rank_list_4,
                                 input$rank_list_5, input$rank_list_6, input$rank_list_7, 
                                 input$rank_list_8, input$rank_list_9, input$rank_list_10,
                                 input$rank_list_11, input$rank_list_12, input$rank_list_13),
                     year = c(rep(1, time = length(input$rank_list_2)+length(input$rank_list_3)+length(input$rank_list_4)),
                              rep(2, time = length(input$rank_list_5)+length(input$rank_list_6)+length(input$rank_list_7)),
                              rep(3, time = length(input$rank_list_8)+length(input$rank_list_9)+length(input$rank_list_10)),
                              rep(4, time = length(input$rank_list_11)+length(input$rank_list_12)+length(input$rank_list_13))),
                     term = c(rep("Fall", times = length(input$rank_list_2)),rep("Winter", times = length(input$rank_list_3)),rep("Spring", times = length(input$rank_list_4)),
                              rep("Fall", times = length(input$rank_list_5)),rep("Winter", times = length(input$rank_list_6)),rep("Spring", times = length(input$rank_list_7)),
                              rep("Fall", times = length(input$rank_list_8)),rep("Winter", times = length(input$rank_list_9)),rep("Spring", times = length(input$rank_list_10)),
                              rep("Fall", times = length(input$rank_list_11)),rep("Winter", times = length(input$rank_list_12)),rep("Spring", times = length(input$rank_list_13))),
                     stringsAsFactors = FALSE) |> 
      mutate(course_code = sub(":.*", "", courses),
             title = sub(".*?:", "", courses),
             course_number = str_extract(course_code, "\\d+\\w*$")) |> 
      left_join(all_courses_uo |> select(course_code, credits), by = "course_code") |> # add credit
      left_join(uo_req_wide |> select(course_code, AC:US), by = "course_code") |> # add uo req
      left_join(cds_req |> select(course_code, fulfill), by = "course_code") |> 
      mutate(credits = as.numeric(credits),
             status = "user_added",
             term = fct_relevel(term, "Fall", "Winter", "Spring")) |> 
      group_by(year, term) |> 
      mutate(total_credits = sum(credits, na.rm = TRUE)) |> 
      ungroup()
    #need language
    #need prereq
  })
  
  ### UO Req check table ----
  uo_req_check <- reactive({
    df <- total_inputs()
    min_credit_df <- data.frame(req_name = "min_credit",
                                req = "Min. credits: 180 credits",
                                current_req = sum(df$credits, na.rm = TRUE)) |>
      mutate(satisfied = ifelse(current_req >= 180, "Fulfilled", "Not Fulfilled"),
             current_req = as.character(current_req)) |> 
      select(req_name,req, current_req, satisfied)
    
    min_upper_df <- data.frame(req_name = "min_upper",
                               req = "Min. upper division credits: 62 credits",
                               current_req = sum(filter(df, course_number >= 300)$credits, na.rm = TRUE)) |> 
      mutate(satisfied = ifelse(current_req >= 62, "Fulfilled", "Not Fulfilled"),
             current_req = as.character(current_req)) |> 
      select(req_name,req, current_req, satisfied)
    
    
    writing_df <- data.frame(req_name = "writing", req = "Writing req: WR 121Z and WR 122Z") |> 
      mutate(current_req = ifelse(any(grepl("WR 121Z", df$course_code)) & any(grepl("WR 122Z", df$course_code)), "WR 121Z and WR 122Z", 
                                  ifelse(any(grepl("WR 121Z", df$course_code)), "WR 121Z", 
                                         ifelse(any(grepl("WR 122Z", df$course_code)), "WR 122Z", "None"))), 
             satisfied = ifelse(current_req == "WR 121Z and WR 122Z", "Fulfilled", "Not Fulfilled"))
    
    al_df <- data.frame(req_name = "al",
                        req = "Arts & Letters: 15 credits",
                        current_req = sum(filter(df, !is.na(AL))$credits, na.rm = TRUE)) |> 
      mutate(satisfied = ifelse(current_req >= 15, "Fulfilled", "Not Fulfilled"),
             current_req = paste0(as.character(current_req), " credit(s): ", paste(filter(df, !is.na(AL))$course_code, collapse = ", "))) |> 
      select(req_name,req, current_req, satisfied)
    
    ssc_df <- data.frame(req_name = "ssc",
                         req = "Social Science: 15 credits",
                         current_req = sum(filter(df, !is.na(SSC))$credits, na.rm = TRUE)) |> 
      mutate(satisfied = ifelse(current_req >= 15, "Fulfilled", "Not Fulfilled"),
             current_req = paste0(as.character(current_req), " credit(s): ", paste(filter(df, !is.na(SSC))$course_code, collapse = ", "))) |> 
      select(req_name,req, current_req, satisfied)
    
    sc_df <- data.frame(req_name = "sc",
                        req = "Science: 15 credits",
                        current_req = sum(filter(df, !is.na(SC))$credits, na.rm = TRUE)) |> 
      mutate(satisfied = ifelse(current_req >= 15, "Fulfilled", "Not Fulfilled"),
             current_req = paste0(as.character(current_req), " credit(s): ", paste(filter(df, !is.na(SC))$course_code, collapse = ", "))) |> 
      select(req_name,req, current_req, satisfied)
    
    
    us_df <- data.frame(req_name = "us",
                        req = "US: 1 course",
                        current_req = nrow(filter(df, US == "US"))) |> 
      mutate(satisfied = ifelse(current_req >= 1, "Fulfilled", "Not Fulfilled"),
             current_req = paste0(as.character(current_req), " course(s): ", paste(filter(df, !is.na(US))$course_code, collapse = ", "))) |> 
      select(req_name,req, current_req, satisfied)
    
    gp_df <- data.frame(req_name = "gp",
                        req = "Global Perspective: 1 course",
                        current_req = nrow(filter(df, GP == "GP"))) |> 
      mutate(satisfied = ifelse(current_req >= 1, "Fulfilled", "Not Fulfilled"),
             current_req = paste0(as.character(current_req), " course(s): ", paste(filter(df, !is.na(GP))$course_code, collapse = ", "))) |> 
      select(req_name,req, current_req, satisfied)
    
    total_uo_req_check <- bind_rows(min_credit_df,min_upper_df,writing_df, al_df,ssc_df, sc_df, us_df, gp_df)|> 
      mutate(satisfied = case_when(satisfied == "Fulfilled" ~ "✔️",
                                   satisfied == "Not Fulfilled" ~"❌", 
                                   TRUE ~ satisfied)) |> 
      rename("Requirements" = req,
             "Current" = current_req,
             " " = satisfied) |> 
      select(-req_name)
    
    #language req
    
    return(total_uo_req_check)
  })
  
  
  ### CDS Req check table ----
  cds_req_check <- reactive({
    df <- total_inputs()
    
    # CDS requirements (Area a, b, c)
    
    area_a_df <- data.frame(req_name = "area_a",
                            req = filter(cds_req, fulfill == "CDS: Area A")$course_code) |> 
      mutate(current_req = case_when(req %in% df$course_code ~ req, 
                                     TRUE ~ "None")) |> 
      mutate(satisfied = case_when(current_req == "None" ~ "Not Fulfilled",
                                   TRUE ~ "Fulfilled"))
    
    
    df_stat <- df |> 
      filter(fulfill == "CDS: Area B Stat") 
    
    
    area_b_stat_df <- data.frame(req_name = "area_b_stat",
                                 req = "Area B Statistics: 1 course",
                                 current_req = case_when(nrow(df_stat) == 0 ~ "None",
                                                         TRUE ~ paste(df_stat$course_code, collapse = ", "))) |> 
      mutate(satisfied = ifelse(nrow(filter(df_stat)) >= 1, "Fulfilled", "Not Fulfilled"))
    
    
    
    df_soc <- df |> 
      filter(fulfill == "CDS: Area B Soc" |
               grepl("^PSY", course_code) |
               grepl("^SOC", course_code)) |> 
      filter(course_code != "PSY 302") |> 
      filter(course_code != "SOC 312")
    
    
    area_b_soc_df <- data.frame(req_name = "area_b_soc",
                                req = "Area B Social-Behavioral Science: 1 course",
                                current_req = case_when(nrow(df_soc) == 0 ~ "None",
                                                        TRUE ~ paste(df_soc$course_code, collapse = ", "))) |> 
      mutate(satisfied = ifelse(nrow(filter(df_soc)) >= 1, "Fulfilled", "Not Fulfilled"))
    
    
    df_bio <- df |> 
      filter(fulfill == "CDS: Area B Bio" |
               grepl("^HPHY", course_code)) |> 
      filter(course_code != "HPHY 211") |> 
      filter(course_code != "HPHY 212")
    
    
    area_b_bio_df <- data.frame(req_name = "area_b_bio",
                                req = "Area B Biological Science: 1 course",
                                current_req = case_when(nrow(df_bio) == 0 ~ "None",
                                                        TRUE ~ paste(df_bio$course_code, collapse = ", "))) |> 
      mutate(satisfied = ifelse(nrow(filter(df_bio)) >= 1, "Fulfilled", "Not Fulfilled"))
    
    df_phys <- df |> 
      filter(fulfill == "CDS: Area B Phys" |
               grepl("^CH", course_code) |
               grepl("^PHYS", course_code)) 
    
    
    area_b_phys_df <- data.frame(req_name = "area_b_phys",
                                 req = "Area B Physical Science: 1 course",
                                 current_req = case_when(nrow(df_phys) == 0 ~ "None",
                                                         TRUE ~ paste(df_phys$course_code, collapse = ", "))) |> 
      mutate(satisfied = ifelse(nrow(filter(df_phys)) >= 1, "Fulfilled", "Not Fulfilled"))
    
    df_area_c <- df |> 
      filter(fulfill == "CDS: Area C") 
    
    
    area_c_df <- data.frame(req_name = "area_c",
                            req = "Area C: 1 course",
                            current_req = case_when(nrow(df_area_c) == 0 ~ "None",
                                                    TRUE ~ paste(df_area_c$course_code, collapse = ", "))) |> 
      mutate(satisfied = ifelse(nrow(filter(df_area_c)) >= 1, "Fulfilled", "Not Fulfilled"))
    
    
    
    total <- bind_rows(area_a_df, area_b_stat_df, area_b_soc_df, area_b_bio_df, area_b_phys_df, area_c_df) |> 
      mutate(satisfied = case_when(satisfied == "Fulfilled" ~ "✔️",
                                   satisfied == "Not Fulfilled" ~"❌", 
                                   TRUE ~ satisfied))|> 
      rename("Requirements" = req,
             "Current" = current_req,
             " " = satisfied) |> 
      select(-req_name)
    
    return(total)  # Return the modified dataframe
    
  })
  
  output$uo_requirement_check <- renderTable({
    uo_req_check()
  })
  
  output$cds_requirement_check <- renderTable({
    cds_req_check()
  })
  
  # ### Viz data frame ----
  # 
  # viz_df <- reactive ({
  #   df <- total_inputs()
  #   
  #   #### CDS ----
  #   cds_req_area_a_incomplete <- cds_progression |> 
  #     filter(str_detect(fulfill, "CDS: Area A")) |> 
  #     filter(!code %in% df$course_code)|> 
  #     select(1:6) |> 
  #     rename(course_code = code) |> 
  #     mutate(status = "prediction")
  #   
  #   predict_area_a <- df |>
  #     select(course_code, title, credits, year, term, status, fulfill) |> 
  #     bind_rows(cds_req_area_a_incomplete) 
  #   
  #   predict_area_b_stat <- predict_area_a |> 
  #     (\(df) if (!any(df$fulfill == "CDS: Area B Stat", na.rm = TRUE))
  #       bind_rows(df, tibble(course_code = "",
  #                            title = "CDS requirement Area B in Statistics",
  #                            credits = 4,
  #                            year = 0,
  #                            term = "",
  #                            status = "prediction",
  #                            fulfill = "CDS: Area B Stat"))
  #      else df)()
  #   
  #   predict_area_b_soc <- predict_area_b_stat |> 
  #     (\(df) if (!any(df$fulfill == "CDS: Area B Soc", na.rm = TRUE))
  #       bind_rows(df, tibble(course_code = "",
  #                            title = "CDS requirement Area B in Social Science",
  #                            credits = 4,
  #                            year = 0,
  #                            term = "",
  #                            status = "prediction",
  #                            fulfill = "CDS: Area B Soc"))
  #      else df)()
  #   
  #   predict_area_b_bio <- predict_area_b_soc |> 
  #     (\(df) if (!any(df$fulfill == "CDS: Area B Bio", na.rm = TRUE))
  #       bind_rows(df, tibble(course_code = "",
  #                            title = "CDS requirement Area B in Biological Science",
  #                            credits = 4,
  #                            year = 0,
  #                            term = "",
  #                            status = "prediction",
  #                            fulfill = "CDS: Area B Bio"))
  #      else df)()
  #   
  #   predict_area_b_phys <- predict_area_b_bio |> 
  #     (\(df) if (!any(df$fulfill == "CDS: Area B Phys", na.rm = TRUE))
  #       bind_rows(df, tibble(course_code = "",
  #                            title = "CDS requirement Area B in Physcial Science",
  #                            credits = 4,
  #                            year = 0,
  #                            term = "",
  #                            status = "prediction",
  #                            fulfill = "CDS: Area B Phys"))
  #      else df)()
  #   
  #   predict_area_c <- predict_area_b_phys |> 
  #     (\(df) if (!any(df$fulfill == "CDS: Area C", na.rm = TRUE))
  #       bind_rows(df, tibble(course_code = "",
  #                            title = "CDS requirement Area C",
  #                            credits = 4,
  #                            year = 0,
  #                            term = "",
  #                            status = "prediction",
  #                            fulfill = "CDS: Area C"))
  #      else df)() 
  #   
  #   
  #   cds_prediction_1 <- predict_area_c|> 
  #     group_by(year, term) |> 
  #     mutate(total_credits = sum(credits, na.rm = TRUE)) |> 
  #     ungroup() |> 
  #     mutate(year_term = factor(paste(year, term), levels = c("1 Fall", "1 Winter", "1 Spring",
  #                                                             "2 Fall", "2 Winter", "2 Spring",
  #                                                             "3 Fall", "3 Winter", "3 Spring",
  #                                                             "4 Fall", "4 Winter", "4 Spring"))) |> 
  #     arrange(year_term)
  #   
  #   cds_prediction_2 <- cds_prediction_1 |> 
  #     filter(total_credits <= 14) |> 
  #     mutate(year_term = case_when(year == 0 ~ first(year_term),
  #                                  TRUE ~ year_term))
  #   
  #   
  #   |> 
  #     mutate(year_term = case_when(year == 0 ~ last(year)))
  #   
  #   ### UO ----
  #   
  #   
  #   
  #   # predict_area_b <- predict_area_a |> 
  #   #   # Check if "CDS: Area B" is missing in fulfill
  #   #   (\(df) if (!any(df$fulfill == "CDS: Area B")) 
  #   #     bind_rows(df, tibble(title = "hello", fulfill = "CDS: Area B")) 
  #   #    else df)()
  #   
  #   
  #   |> 
  #     mutate(course_number = str_extract(course_code, "\\d+\\w*$")) |> 
  #     left_join(uo_req_wide |> select(course_code, AC:US), by = "course_code") |> # add uo req
  #     left_join(cds_req |> select(course_code, fulfill), by = "course_code")
  #   
  # })
  
  
  
}

# Run the Application ----
shinyApp(ui, server)
