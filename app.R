library(shiny)
library(shinyMobile)

source("data_process.r")

shinyApp(
  ui = f7Page(
    title = "Riichi Mahjong Calculator",
    tags$head(includeHTML("www/splashscreens.html")),
    tags$head(includeHTML("www/google-analytics.html")),
    options = list(
      dark = TRUE,
      pullToRefresh = FALSE,
      toolbar = list(
        hideNavOnPageScroll = TRUE
      )
    ),
    allowPWA = TRUE,
    f7SingleLayout(
      navbar = f7Navbar(
        title = "Riichi Mahjong Calculator",
        hairline = TRUE,
        shadow = TRUE
      ),
      toolbar = f7Toolbar(
        position = "bottom",
        f7Link(label = "About Author", href = "https://www.annielyu.com"),
        f7Link(label = "Source Code", href = "https://github.com/XiaodanLyu/richi-mahjong-calculator")
      ),
      # main content
      f7Block(h3("和牌：")),
      f7Card(
        f7Toggle(
          inputId = "is_zhuang",
          label = "庄家",
          color = "blue",
          checked = FALSE
        )
      ),
      f7Card(
        f7Toggle(
          inputId = "is_mo",
          label = "自摸",
          color = "blue",
          checked = TRUE
        )
      ),
      f7Picker(
        inputId = "fan",
        label = "番数：",
        choices = unique(data$fan),
        value = "1"
      ),
      f7Picker(
        inputId = "fu",
        label = "符数：",
        choices = unique(data$fu),
        value = "30"
      ),
      f7Block(h3("结算：")),
      f7List(
        f7ListItem(
          title = "小结：",
          span(strong(textOutput("summary")), style = "color:DodgerBlue")
        ),
        f7ListItem(
          title = "入账：",
          span(strong(textOutput("point_in"), style = "color:MediumSeaGreen"))
        ),
        f7ListItem(
          title = "出账：",
          span(strong(textOutput("point_out"), style = "color:Tomato"))
        )
      )
    )
  ),
  server = function(input, output) {
    out <- reactive(
      data %>% 
        filter(is_zhuang == input$is_zhuang,
               is_mo == input$is_mo,
               fan == input$fan,
               fu == input$fu)
    )
    output$summary <- renderText({
      print(paste0(ifelse(input$is_zhuang, "庄家", "闲家"),
                   ifelse(input$is_mo, "自摸", "荣和"),
                   " ",
                   input$fan,
                   "番",
                   input$fu,
                   "符"))
    })
    output$point_in <- renderText({
      print(paste0("赢家：", unique(out()$total), "点"))
    })
    output$point_out <- renderText({
      print(
        glue::glue_collapse(paste0(out()$payee, ": ", out()$point, "点"), ", ")
      )
    }) 
  }
)