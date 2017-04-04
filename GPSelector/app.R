library(shiny)
library(readxl)
library(tidyverse)
library(lpSolve)
library(rhandsontable)
library(DT)
library(shinydashboard)

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("Bren Group Project to Student Matcher"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      fileInput(inputId = "dataIN",
                label = "Upload your CSV file"),
      actionButton(inputId = "run",
                   label = "Run!"),
      p(),
      downloadButton(outputId = "down",
                     label = "Download results")
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      tabsetPanel(
        tabPanel("Point Allocation",
                 DT::dataTableOutput("prevIN")),
        tabPanel("Project Maximums",
                 h3("The project maximums are set to a default of 5 members per group. You can modify these by directly writing into this table, just like Excel!"),
                 rHandsontableOutput("max", width = 300)),
        tabPanel("Solution",
                 textOutput("total"),
                 DT::dataTableOutput("solution"))
      ),
      
      tags$head(tags$style("#total{color: red;
                                 font-size: 30px;
                           font-face: bold;
                           }")
      )
    )
  )
)

# Define server logic
server <- function(input, output) {
  
  points <- reactive({
    inFile <- input$dataIN
    
    if (is.null(inFile)) {
      return(NULL)
    } else {
      data <- read.csv(inFile$datapath, strip.white = T, stringsAsFactors = F)
      return(data)
    }
  })
  
  ## Content of first tab
  
  output$prevIN <- DT::renderDataTable(DT::datatable(points(),
                                                     options = list(pageLength = 100)))
  
  ## Content of second tab
  
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
  
  ## Content of third tab
  
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
    
    return(list(sol_out = sol_out, objval = solution$objval))
  })
  
  output$total <- renderText(paste("Optimum solution yields a value of", solution()$objval))
  
  output$solution <- DT::renderDataTable(DT::datatable(data = solution()$sol_out,
                                                       options = list(pageLength = 100)))
  
  output$down <- downloadHandler(
    filename = function() {
      paste0("Solution", ".csv")
    },
    content = function(file) {
      write.csv(solution()$sol_out, file, row.names = F)
    }
  )
}

# Run the app
shinyApp(ui = ui, server = server)

