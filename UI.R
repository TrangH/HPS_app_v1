ui <- fluidPage(
    # App title ----
    titlePanel(
      p( 
        h1("Digitally United We Stand, Digitally Divided We Fall?", align = "center"),
        br(),
        h3("An Analysis of Learning Loss, Learning Modes and Learning-Facility Access for U.S K-12 Students In the COVID-19 Era.", align = "center"),
        br()
        )
    ),
    #titlePanel('Digitally United We Stand, Digitally Divided We Fall?
    #An Analysis of Learning Loss, Learning Modes and Learning-Facility Access for U.S K-12 Students In the COVID-19 Era.'
    #),
    
    # Sidebar layout with input and output definitions ----
    sidebarLayout(
        
        # Sidebar panel for inputs ----
        sidebarPanel(width = 3,
            # Input: Select a file ----
            fileInput("file1", "Upload/Update T1 data here",
                      multiple = TRUE, 
                      accept=c('text/csv', 
                               'text/comma-separated-values,
                               text/plain','.csv')),
            fileInput("file2", "Upload/Update T2 data here",
                      multiple = TRUE, 
                      accept=c('text/csv', 
                               'text/comma-separated-values,
                               text/plain','.csv')),
            fileInput("file3", "Upload/Update T3 data here",
                      multiple = TRUE, 
                      accept=c('text/csv', 
                               'text/comma-separated-values,
                                text/plain','.csv')),
            helpText('About the author:',
                     br(),
                     'I am Trang Hoang, a Master of Education student at the College of Education, University of Washington. My primary research interest is the efficacy of educational public policies.',
                     br(),
                     br(),
                     'I thank Professor Chris Adolph and participants in the Visualizing Time Series Data Group (CSSS569) at the University of Washington for helpful comments and encouragement. All errors are mine.',
                     br(),
                     br(),
                     'This application is under development. All comments and suggestions are welcomed.
                     Contact:', a("trangh2@uw.edu",href="mailto:trangh2@uw.edu", target="_blank")
                    )
          ),
        
        # Main panel for displaying outputs ----
        mainPanel(
            tabsetPanel(
                tabPanel("Spatial Distribution Plots", 
                         sidebarLayout(
                           sidebarPanel(width = 4,
                             ## User chooses the variable to display
                             #The filter are applied to all maps
                             selectInput(inputId = "inputvar_Dimension", 
                                         label = "Select a demographic characteristic",
                                         choices = ""),
                             selectInput(inputId = "inputvar_Round", 
                                         label = "Select a survey round (most recent first)",
                                         choices = "")
                             ),
                           #Maps
                           mainPanel(width = 8,
                              selectInput(inputId = "inputvar_Q1", 
                                          label = "Learning loss: Time spent learning was",
                                          choices = c("Much less", 
                                               "Little less","Unchanged",
                                               "Little more", "Much more")),
                              leaflet::leafletOutput("map_Q1",
                                   width  = "500px", height = "300px"),
                              selectInput(inputId = "inputvar_Q2", 
                                       label = "Learning mode: Students had their classes",
                                        choices = c("Using online resources", 
                                                             "Using paper materials sent home",
                                                             "Cancelled", "Unchanged")),
                             leaflet::leafletOutput("map_Q2",
                                           width  = "500px", height = "300px"),
                             selectInput(inputId = "inputvar_Q3", 
                                         label = "Learning facility: Students had access to",
                                         choices = c("Digital devices", "Internet")),
                             leaflet::leafletOutput("map_Q3",
                                           width  = "500px",height = "300px"),
                             withMathJax(),
                             helpText('Notes:',
                                      br(),
                                      'These choropleth maps present the spatial distribution of various variables extracted 
                                      from the U.S Household Pulse Survey database. The survey sample consists of K-12 students enrolled in private or public schools.',
                                      br(),
                                      br(),
                                      'Data source: "', a("Household Pulse Survey Data Tables",href="https://www.census.gov/programs-surveys/household-pulse-survey/data.html", target="_blank"),
                                      '". United States Census Bureau.'
                                      )
                                )
                           )),
                tabPanel("Table 1", 
                         sidebarLayout(
                           sidebarPanel(width = 4,
                                        #User chooses the variable to display
                                        selectInput(inputId ="T1_Dimension", 
                                                    label = "Select a demographic characteristic",
                                                    choices = c("")),
                                        selectInput(inputId ="T1_Round", 
                                                    label = "Select a survey round",
                                                    choices = c("")),
                                        #Select number of rows to display
                                        radioButtons("disp_T1", "Display",
                                                     choices = c(`First 5 rows` = "head",
                                                                 `All rows` = "all"),
                                                     selected = "head")
                           ),
                           mainPanel(width = 8,
                                     tableOutput("Q1"),
                                     withMathJax(),
                                     helpText('Notes:',
                                              br(),
                                              'From survey rounds 1 to 12, the unprocessed table 1 table presents information on time spent on home-based education for households with children in school. 
                                              From round 13 onwards, it shows: (a) Time spent on learning activities, relative to pre-pandemic level and (b) days spent in live contact with teachers relative to pre-pandemic level.
                                              The displayed (processed) results  are for time spent on learning activities.
                                              Surveyed students that did not response are not included in the computation of the total figure.',
                                              br(),
                                              br(),
                                              'Data source: "', a("Household Pulse Survey Data Tables",
                                               href="https://www.census.gov/programs-surveys/household-pulse-survey/data.html", target="_blank"),
                                              '". United States Census Bureau.'
                                     ),
                                     downloadButton('download_Q1',"Download unprocessed T1 data"))
                         )),
                tabPanel("Table 2", 
                         sidebarLayout(
                           sidebarPanel(width = 4,
                             #User chooses the variable to display
                             selectInput(inputId ="T2_Dimension", 
                                 label = "Select a demographic characteristic",
                                 choices = c("")),
                             selectInput(inputId ="T2_Round", 
                                 label = "Select a survey round",
                                 choices = c("")),
                             #Select number of rows to display
                             radioButtons("disp_T2", "Display",
                                          choices = c(`First 5 rows` = "head",
                                                      `All rows` = "all"),
                                          selected = "head")
                           ),
                           mainPanel(width = 8,
                             tableOutput("Q2"),
                             withMathJax(),
                             helpText('Notes:',
                                      br(),
                                      'The unprocessed table 2 presents information on COVID-19 Pandemic impact on how children enrolled in public or private schools received education. 
                                      Surveyed students that did not response are not included in the computation of the total figure.',
                                      br(),
                                      br(),
                                      'Data source: "', a("Household Pulse Survey Data Tables",
                                                          href="https://www.census.gov/programs-surveys/household-pulse-survey/data.html", target="_blank"),
                                      '". United States Census Bureau.'
                             ),
                             downloadButton('download_Q2',"Download unprocessed T2 data"))
                         )),
                tabPanel("Table 3", 
                         sidebarLayout(
                           sidebarPanel(width = 4,
                           #User chooses the variable to display
                             selectInput(inputId ="T3_Dimension", 
                               label = "Select a demographic characteristic",
                               choices = c("")),
                             selectInput(inputId ="T3_Round", 
                               label = "Select a survey round",
                               choices = c("")),
                           #Select number of rows to display
                           radioButtons("disp_T3", "Display",
                                        choices = c(`First 5 rows` = "head",
                                                    `All rows` = "all"),
                                        selected = "head")
                           ),
                           mainPanel(width = 8,
                             tableOutput("Q3"),
                             withMathJax(),
                             helpText('Notes:',
                                      br(),
                                      'The unprocessed table 3 presents information on computer and internet availability in households with children enrolled in public or private schools.
                                      Surveyed students that did not response are not included in the computation of the total figure.',
                                      br(),
                                      br(),
                                      'Data source: "', a("Household Pulse Survey Data Tables",
                                      href="https://www.census.gov/programs-surveys/household-pulse-survey/data.html", target="_blank"),
                                      '". United States Census Bureau.'
                             ),
                             downloadButton('download_Q3',"Download unprocessed T3 data"))
                         ))
            )
        )
    )
)
