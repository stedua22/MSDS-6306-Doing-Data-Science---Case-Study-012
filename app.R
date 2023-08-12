library(shiny)

# Define UI for application that draws a histogram
ui <- shinyUI(pageWithSidebar(
  
  headerPanel("Frito Lay Data"),
  
  titlePanel("Histogram of Attrition"),
  sidebarLayout(
    sidebarPanel(
      checkboxGroupInput("cb_job_level", "Filter by Job Level:",
                         choices = c("1", "2", "3", "4"),
                         selected = c("1", "2", "3", "4")),
      checkboxGroupInput("cb_attrition", "Filter by Attrition:",
                         choices = c("Yes", "No"),
                         selected = c("Yes", "No")),
      sliderInput("income_range", "Filter by Monthly Income:",
                  min = 1000, max = 20000, value = c(5000, 15000))
    ),
    mainPanel(
      plotOutput("histogram")
    )
  )
))


# Define server logic required to draw a histogram ----
server <- function(input, output) {
  
  data <- reactive({
    dat <- read.csv("https://raw.githubusercontent.com/stedua22/MSDS-6306-Doing-Data-Science---Case-Study-02/main/Casestudy2.csv", header = TRUE)
  
    dat
  })
  
  filtered_data <- reactive({
    dat <- data()
    
    if (!is.null(input$cb_job_level)) {
      dat <- dat %>% filter(JobLevel %in% input$cb_job_level)
    }
    if (!is.null(input$cb_attrition)) {
      dat <- dat %>% filter(Attrition %in% input$cb_attrition)
    }
    dat <- dat %>% filter(MonthlyIncome >= input$income_range[1] & MonthlyIncome <= input$income_range[2])
    
    dat
  })
  
  output$histogram <- renderPlot({
    filtered_dat <- filtered_data()
    
    ggplot(filtered_dat, aes(x = Attrition, fill = Attrition)) +
      geom_bar() +
      facet_grid(rows = vars(JobLevel)) +
      scale_fill_manual(values = c("Yes" = "lightgreen", "No" = "blue"))  # Customize colors
  })
}



# Run the application 
shinyApp(ui = ui, server = server)






