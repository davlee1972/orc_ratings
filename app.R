library(shiny)
library(shinydashboard)
library(data.table)
library(DT)

orc_ratings = setDT(read.table("orc_ratings.csv", header = TRUE, sep = ",", check.names = FALSE))

ui = fluidPage(
  titlePanel("ORC Ratings"),
  fluidRow(
    column(8,
           checkboxGroupInput("course", label = h3("Course Types"), 
                              choices = names(orc_ratings)[-1],
                              inline = TRUE,
                              selected = names(orc_ratings)[2])
    ),
    column(2,
           selectInput("yacht",
                       h4("Yacht Name"),
                       orc_ratings$Yacht, selected = "Kuai")
    ),
    column(2,
           numericInput("time", label = h4("Time On Course"), value = 60)
    )
  ),
  # Create a new row for the table.
  fluidRow(
    tabBox(width = 12,
           tabPanel('Time Owed',  DT::dataTableOutput("owed")),
           tabPanel('ORC Ratings',  DT::dataTableOutput("ratings"))
    )
  )
)


server = function(input, output) {
  
  owed_times = reactive({
    req(input$course)
    owed_times_dt = data.table()
    lapply(input$course, function(x) {
      data <- setnames(orc_ratings[,c('Yacht', ..x)], c("Yacht Name","rating"))
      boat_rating = data[data$Yacht == input$yacht, rating]
      data = data[order(-rating)]
      data[, Course := x]
      data[, `Time On Course` := (rating / boat_rating * input$time) - input$time]
      data[, `60 min` := (rating / boat_rating * 60) - 60]
      data[, rating := NULL]
      if (length(owed_times_dt) == 0) {
        owed_times_dt <<- data
      }
      else
      {
        owed_times_dt <<- rbind(owed_times_dt, data)
      }
    })
    owed_times_dt$`90 min` = owed_times_dt$`60 min` * 1.5
    owed_times_dt$`120 min` = owed_times_dt$`60 min` * 2

    owed_times_dt$`Time On Course` = paste0(owed_times_dt$`Time On Course` -(owed_times_dt$`Time On Course`%%sign(owed_times_dt$`Time On Course`)) ," min ", round(owed_times_dt$`Time On Course`%%sign(owed_times_dt$`Time On Course`)*60,0), " sec")
    owed_times_dt$`60 min` = paste0(owed_times_dt$`60 min` -(owed_times_dt$`60 min`%%sign(owed_times_dt$`60 min`)) ," min ", round(owed_times_dt$`60 min`%%sign(owed_times_dt$`60 min`)*60,0), " sec")
    owed_times_dt$`90 min` = paste0(owed_times_dt$`90 min` -(owed_times_dt$`90 min`%%sign(owed_times_dt$`90 min`)) ," min ", round(owed_times_dt$`90 min`%%sign(owed_times_dt$`90 min`)*60,0), " sec")
    owed_times_dt$`120 min` = paste0(owed_times_dt$`120 min` -(owed_times_dt$`120 min`%%sign(owed_times_dt$`120 min`)) ," min ", round(owed_times_dt$`120 min`%%sign(owed_times_dt$`120 min`)*60,0), " sec")
    owed_times_dt[`Yacht Name`== input$yacht, names(owed_times_dt)[c(-1,-2)] := "--------------" ]
    owed_times_dt
  }
  )
  
  output$owed <- DT::renderDataTable(DT::datatable(
    data = owed_times(),
    extensions = c('FixedHeader', 'Buttons', 'ColReorder', 'Scroller'),
    rownames= FALSE,
    options = list(
      dom = 'Bfrti',
      searching = FALSE,
      pageLength = nrow(owed_times()),
      buttons = c('copy', 'print')
    )))
  
  output$ratings <- DT::renderDataTable(DT::datatable(
    data = orc_ratings,
    extensions = c('FixedHeader', 'Buttons', 'ColReorder', 'Scroller'),
    rownames= FALSE,
    options = list(
      dom = 'Bfrti',
      searching = FALSE,
      pageLength = nrow(orc_ratings),
      buttons = c('copy', 'csv', 'print')
    )
  ))
  
}


shinyApp(server = server, ui = ui)