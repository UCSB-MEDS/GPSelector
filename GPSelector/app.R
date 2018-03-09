library(shiny)
library(DT)
library(shinydashboard)
library(rhandsontable)
library(knitr)
library(kableExtra)
library(tidyverse)
library(lpSolve)

#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
ui <- dashboardPage(
  dashboardHeader(title = "GP-Student Matcher"),
  dashboardSidebar(
    sidebarMenu(
      fileInput(inputId = "dataIN",
                label = "Upload your CSV file"),
      menuItem("View data", tabName = "data", icon = icon("th"), startExpanded = T),
      menuItem("Project maximums", tabName = "gp_max", icon = icon("database")),
      actionButton(inputId = "run",
                   label = "Run!"),
      menuItem("Results (matrix)", tabName = "res_mat", icon = icon("users")),
      menuItem("Results (by group)", tabName = "res_gp", icon = icon("users")),
      wellPanel(shiny::fluidRow(valueBoxOutput(outputId = "total", width = 12))),
      p(),
      downloadButton(outputId = "down_mat",
                     label = "Download matrix"),
      p(),
      downloadButton(outputId = "down_gp",
                     label = "Download grouped")
    )
  ),
  dashboardBody(
    tabItems(
      # First tab content
      tabItem(tabName = "data",
              p("This ShinyApp uses integer linear integer programming to match students to Group Projects, based on their point allocation strategy. ", a("The original code", href = "https://github.com/jcvdav/GPSelector/blob/master/SelectionCode.m"), "was written in MATLAB by Professor Christopher Costello. The new raw code behind the App can be found", a("here", href = "https://github.com/jcvdav/GPSelector")),
              h3("Point Allocation"),
              DT::dataTableOutput("prevIN")
      ),
      tabItem(tabName = "gp_max",
              h3("The project maximums are set to a default of 5 members per group. You can modify these by directly writing into this table, just like Excel!"),
              rHandsontableOutput("max", width = 300)
      ),
      tabItem(tabName = "res_mat",
              h3("Solution (matrix)"),
              DT::dataTableOutput("solution")),
      tabItem(tabName = "res_gp",
              h3("Solution (by groups)"),
              tableOutput("GP_table"))
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  # First menu item
  points <- reactive({
    inFile <- input$dataIN
    
    if (is.null(inFile)) {
      return(NULL)
    } else {
      data <- read.csv(inFile$datapath, strip.white = T, stringsAsFactors = F) %>% 
        janitor::clean_names()
      return(data)
    }
  })
  
  output$prevIN <- DT::renderDataTable(DT::datatable(points(),
                                                     options = list(pageLength = 100,
                                                                    scrollX=TRUE)))
  
  # Second menu item
  
  Btable <- reactive({
    req(input$dataIN)
    if (is.null(input$max)) {
      data <- data.frame(Project = colnames(points())[-c(1,2)],
                         Points = 5)
      return(data)
    } else {
      data <- hot_to_r(input$max)
      return(data)
    }
  })
  
  output$max <- renderRHandsontable({
    DF = Btable()
    if (!is.null(DF))
      rhandsontable(DF, stretchH = "all") %>% 
      hot_col("Project", readOnly = T)
  })
  
  ## Get results
  
  solution <- eventReactive(input$run, {
    fmat <- as.matrix(points()[-c(1,2)])
    
    B <- as.matrix(Btable()$Points)
    
    k <- dim(B)[1] # Number of projects
    N <-  dim(fmat)[1]
    if (dim(fmat)[2] != k){
      print('Matrices are not entered correctly')
    }
    
    f <- t(fmat) %>% 
      as.data.frame() %>% 
      gather(Project, Points) %>% 
      select(Points)
    
    A0 <- diag(1, k)
    A <- A0
    
    for (i in 1:(N-1)){
      A <- cbind(A, A0)
    }
    
    Aeq <- matrix(0, N, N*k)
    
    for (i in 1:N) {
      Aeq[i, (i*k-k+1):(i*k)] = 1
    }
    
    Beq <- matrix(1, N, 1)
    
    const.mat <- rbind(A, Aeq)
    const.dir <-  c(rep("<=", k), rep("==", N))
    const.rhs <- rbind(B, Beq)
    
    solution <- lp(direction = "max", objective.in = f, const.mat = const.mat, const.dir = const.dir, const.rhs = const.rhs, all.bin = T)
    
    sol_out <- cbind(points()[c(1,2)], matrix(solution$solution, N, k, byrow = T))
    colnames(sol_out) <- colnames(points())
    
    sol_by_gp <- sol_out %>% 
      mutate(Name = paste(first_name, last_name)) %>% 
      select(Name, everything()) %>%
      select(-c(first_name, last_name)) %>% 
      gather(Project, Value, -1) %>%
      filter(Value > 0) %>%
      select(Project, Name) %>% 
      arrange(Project)
    
    return(list(sol_out = sol_out,
                objval = solution$objval,
                sol_by_gp = sol_by_gp))
  })
  
  ## Results for "results as matrix"
  output$total <- renderValueBox({
    req(input$dataIN)
    valueBox(value = solution()$objval,
             subtitle = "Optimal solution")
  })
  
  output$solution <- DT::renderDataTable(DT::datatable(data = solution()$sol_out,
                                                       options = list(pageLength = 100)))
  
  ## Results for "resutls by group"
  
  output$GP_table <- function() {
    solution()$sol_by_gp %>%
      knitr::kable("html") %>%
      kable_styling("striped", full_width = F) %>% 
      collapse_rows()
  }
  
  ## Download button matrix
  output$down_mat <- downloadHandler(
    filename = function() {
      paste0("Solution_matrix", ".csv")
    },
    content = function(file) {
      write.csv(solution()$sol_out, file, row.names = F)
    }
  )
  
  ## Download button matrix
  output$down_gp <- downloadHandler(
    filename = function() {
      paste0("Solution_by_GP", ".csv")
    },
    content = function(file) {
      write.csv(solution()$sol_by_gp, file, row.names = F)
    }
  )
  
}

# Run the application 
shinyApp(ui = ui, server = server)