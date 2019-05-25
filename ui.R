
# Sys.setlocale(category = 'LC_ALL','en_US.UTF-8') 


shinyUI(navbarPage("Hausy Search App",
                   tabPanel("All Typeforms",
                            mainPanel(tableOutput("search_tbl"))
                   ),
                   tabPanel("Select Typeform",
                            sidebarPanel(
                                    
                                    numericInput("record", label ="Select Record Number:", value = 100,
                                                 min=1),
                                    
                                    textInput("primera", "Update Primera:", value = "",
                                              placeholder = "Separate multiple locations with commas, no spaces."),
                                    
                                    textInput("segunda", "Update Segunda:", value = "",
                                              placeholder = "Separate multiple locations with commas, no spaces."),
                                    
                                    textInput("tercera", "Update Tercera:", value = "",
                                              placeholder = "Separate multiple locations with commas, no spaces."),
                                    
                                    textInput("max.price", "Max Price", placeholder = "Fix Zeroes in Maximum Price", value = ""),
                                    
                                    textInput("min.price", "Min Price", placeholder = "Fix Zeroes in Minimum Price", value = ""),
                                    
                                    
                                    
                                    actionButton("goButton",label = "Update Typeform"),
                                    
                                    radioButtons("property",label = "Search Selected Property Type Only, or Any Type",
                                                 choices = c("Selected Type","Any Type"),selected = "Selected Type",
                                                 inline = TRUE)
                                    
                                    
                                    
                            ),       
                            mainPanel(
                                    tableOutput("tf1"),
                                    tableOutput("tf2")
                            )
                   ),
                   tabPanel("Prepare Typeform for Scoring",
                            sidebarPanel(
                                    actionButton("prepareButton",label = "Check Locations")
                            ),
                            mainPanel(
                                    tableOutput("tf3"),
                                    h4("Delegation Choices"),
                                    textOutput("deleg"),
                                    h4("By Delegation"),
                                    textOutput("dels"),
                                    h4("Colonia Choices"),
                                    textOutput("colonia"),
                                    h4("By Colonia"),
                                    textOutput("cols")
                            )
                   ),
                   tabPanel("Scores",
                            sidebarPanel(
                                    actionButton("RunCode",label = "Run Code"),
                                    h5("The code will take about 2-3 minutes to run")
                            ),
                            
                            mainPanel(tableOutput("scores"))
                   ),
                   tabPanel("Download Results",
                            sidebarPanel(
                                    downloadLink("downloadData", "Download"))
                   )
                   
))



