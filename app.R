library(shiny)
library(shinydashboard)
library(data.table)
library(DT)

orc_ratings = setDT(read.table("orc_ratings.csv", header = TRUE, sep = ",", check.names = FALSE))

ui = fluidPage(
  titlePanel("ORC Ratings"),
  fluidRow(
    column(4,
           checkboxGroupInput("course", label = h3("Course Types"), 
                              choices = names(orc_ratings)[-1],
                              inline = TRUE,
                              selected = names(orc_ratings)[-1])
    ),
    column(4,
           selectInput("yacht",
                       h4("Yacht Name"),
                       orc_ratings$Yacht, selected = "Kuai")
    ),
    column(4,
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
      data <- setnames(orc_ratings[,c('Yacht', ..x)], c("Yacht","rating"))
      boat_rating = data[data$Yacht == input$yacht, rating]
      data = data[order(-rating)]
      data[, Course := x]
      data[, toc := (rating / boat_rating * input$time) - input$time]
      data[, t_60 := (rating / boat_rating * 60) - 60]
      data[, rating := NULL]
      if (length(owed_times_dt) == 0) {
        owed_times_dt <<- data
      }
      else
      {
        owed_times_dt <<- rbind(owed_times_dt, data)
      }
    })
    owed_times_dt$t_90 = owed_times_dt$t_60 * 1.5
    owed_times_dt$t_120 = owed_times_dt$t_60 * 2

    owed_times_dt$toc = paste0(owed_times_dt$toc -(owed_times_dt$toc%%sign(owed_times_dt$toc)) ," min ", round(owed_times_dt$toc%%sign(owed_times_dt$toc)*60,0), " sec")
    owed_times_dt$t_60 = paste0(owed_times_dt$t_60 -(owed_times_dt$t_60%%sign(owed_times_dt$t_60)) ," min ", round(owed_times_dt$t_60%%sign(owed_times_dt$t_60)*60,0), " sec")
    owed_times_dt$t_90 = paste0(owed_times_dt$t_90 -(owed_times_dt$t_90%%sign(owed_times_dt$t_90)) ," min ", round(owed_times_dt$t_90%%sign(owed_times_dt$t_90)*60,0), " sec")
    owed_times_dt$t_120 = paste0(owed_times_dt$t_120 -(owed_times_dt$t_120%%sign(owed_times_dt$t_120)) ," min ", round(owed_times_dt$t_120%%sign(owed_times_dt$t_120)*60,0), " sec")
    owed_times_dt[Yacht == input$yacht, names(owed_times_dt)[c(-1,-2)] := "--------------" ]
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