library(shiny)

x <- AirPassengers

sp <- spectrum(x)
df <- data_frame(x = sp$freq, y = sp$spec)

shinyServer(function(input, output) {

  output$oPlot <- renderDygraph({
    dygraph(df) %>%
      dyRangeSelector() %>%
      dyOptions(logscale = TRUE)
  })

  output$oMax <- renderPrint({
    wd <- input$oPlot_date_window
    req(wd)
    z <-
      df %>%
      filter(x >= wd[1], x <= wd[2]) %>%
      slice(which.max(y))
    print(z)
  })

})
