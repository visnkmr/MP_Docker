shinyUI(fluidPage(
  titlePanel("Reddit data analysis"),
  
  sidebarLayout(
    sidebarPanel(
      
      
      textInput("search",label = "Enter Query : ",""),
      actionButton("submit", "Go!")    
      ),
    
    mainPanel(
      
      textOutput("text1"),
      plotOutput("result"),
      plotOutput("result2")
    )
  )
))