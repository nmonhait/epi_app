#epi app
library(shiny)
library(shinythemes)

ui <- fluidPage(theme = shinytheme("cerulean"),
                #give the app a title
        titlePanel("Epi Calculator"),
      navlistPanel(
        "What are you trying to calculate?",
        tabPanel("Prevalence",
                 sidebarPanel(
                   selectInput(inputId = "measure_p",
                               label = "Prevalence Type",
                               choices = c("Point Prevalence",
                                           "Period Prevalence")),
                   textInput(inputId = "outcome_p", label = "Outcome",
                             value = ""),
                   textInput(inputId = "time_p", label = "Time",
                             value = "")
                 ),
                 mainPanel(
                   column(6, numericInput(
                          inputId = "cases_p", label = "Existing Cases",
                          value = ""),
                   column(12, 
                          numericInput(
                     inputId = "total_p", label = "Total Population",
                     value = "", width = '100%'
                   )),
                   column(12, (htmlOutput("value_p"))
                 )))),
        tabPanel("Incidence",
                 sidebarPanel(
                   selectInput(inputId = "measure_i",
                              label = "Incidence Type",
                              choices = c("Incidence Density (individual)",
                                           "Incidence Density (aggregate)",
                                           "Cumulative Incidence")),
                 textInput(inputId = "outcome_i", label = "Outcome",
                           value = ""))
                 ,
                 mainPanel(
                   column(8,
                          numericInput(inputId = "new_i", label = "New Cases",
                                value = "")),
                   column(8,
                          numericInput(inputId = "pt_i", label = "Person-Time or Starting # At-Risk",
                                value = "")
                 ),
                 column(12,
                        htmlOutput("value_i"))
                
        )),
        tabPanel("Measure of Association",
                 
                 #input for OR/RR
                 sidebarPanel(
                   selectInput(inputId = "measure",
                               label = "Measure of Association",
                               choices = c("Odds Ratio", "Risk Ratio"),
                               selected = "Odds Ratio"),
                   textInput(inputId = "exposure_m", label = "Exposure",
                             value = ""),
                   textInput(inputId = "outcome_m", label = "Outcome",
                             value = "")
                 ),
                 
                 #numeric input for a,b,c,d cells
                 mainPanel(
                   column(4,numericInput(inputId = "a", label = "A",
                                         value = 0)),
                   column(4, offset = 1, numericInput(inputId = "b", label = "B",
                                                      value = 0)),
                   column(4, numericInput(inputId = "c", label = "C",
                                          value = 0)),
                   column(4, offset = 1, numericInput(inputId = "d", label = "D",
                                                      value = 0)),
                   column(12, (htmlOutput("value")))
                          
                   ))
))



server <- function(input, output){
  observe({
    if(input$measure == "Odds Ratio"){
      output$value <- 
        renderText({paste("People with", input$outcome_m, "are",
                          "<b>", (input$a*input$d) / (input$b*input$c), "</b>",
                          "times as likely to have", input$exposure_m, 
                          "compared to those without", input$outcome_m, ".")})}
    else if(input$measure == "Risk Ratio"){
      output$value <- 
        renderText({paste("People with", input$exposure_m, "are",
                          "<b>", ((input$a / (input$a + input$b)) / (input$c / (input$c + input$d))), "</b>",
                          "times as likely to have", input$outcome_m, 
                          "compared to those without", input$exposure_m, ".")})}})
  observe({
    if(input$measure_p == "Point Prevalence"){
      output$value_p <- 
        renderText({paste("<b>", ((input$cases_p / input$total_p)*100), "%", "</b>",
                          "of the population had", input$outcome_p, 
                          "at this point in time",".")})}
    else if(input$measure_p == "Period Prevalence"){
      output$value_p <- 
        renderText({paste("<b>", ((input$cases_p / input$total_p)*100), "%", "</b>",
                          "of the population had", input$outcome_p, 
                          "in", input$time_p)})
  }})
  observe({
    if(input$measure_i == "Incidence Density (individual)"){
      output$value_i <- 
        renderText({paste(
                          "<b>", ((input$new_i/ input$pt_i) * 100000), "</b>",
                          input$outcome_i, "per 100,000 person-years",
                          ".")})}
    else if(input$measure_i == "incidence Density (aggregate)"){
      output$value_i <- 
        renderText({paste(
                          "<b>", ((input$new_i / input$pt_i) * 100000), "</b>",
                          input$outcome_i, "per 100,000 person-years",
                          ".")})}
    else if(input$measure_i == "Cumulative Incidence"){
      output$value_i <- 
        renderText({paste("The cumulative incidence of", input$outcome_i,
                          "was",
          "<b>", ((input$new_i / input$pt_i) * 100), "</b>", "% ",
          ".")})} 
    })
}

shinyApp(ui = ui, server = server)