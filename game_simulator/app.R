## Load libs
library(shiny)
library(ggplot2)
library(glue)
library(DescTools)
library(bslib)
library(hrbrthemes)

## Load utils with gaming function
source("../scripts/game_utils.R")

## UI
ui <- fluidPage(
  theme = theme <- bs_theme(
    bg = "#FFFFFF", fg = "#999999", primary = "black",
    base_font = font_google("Montserrat")
  ),

  ## Application title
  titlePanel(title = span("Play the Box Game",
    style = "color: #9474CA;
             font-size: 35px;
             font-style: bold"
  )),

  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(
      textOutput("descr", ),
      tags$head(tags$style("#descr{color: #999999;
                                 font-size: 15px;
                                 font-style: italics;
                                 }")),
      br(),
      img(src = "turtle.png", width = "100%"),
      br(),
      br(),
      numericInput("n_of_plays",
        "How many games do you want to play?",
        10e3,
        min = 1,
        max = 10e6
      ),
      actionButton("submit", "Play!")
    ),

    # Show a plot of the generated distribution
    mainPanel(
      textOutput("thanks", ),
      tags$head(tags$style("#thanks{color: #69B2CC;
                                 font-size: 25px;
                                 font-style: bold;
                                 }")),
      br(),
      textOutput("avg_win", ),
      br(),
      textOutput("avg_picks", ),
      br(),
      textOutput("summary_picks", ),
      plotOutput("picks_plot", width = "50%", height = "250px"),
      br(),
      textOutput("summary_win", ),
      plotOutput("wins_plot", width = "50%", height = "250px"),
      br()
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  output$thanks <- renderText({
    if (input$submit > 0) {
      n_plays <- input$n_of_plays
      glue("Thanks for playing {n_plays} games.")
    }
  })
  
  output$descr <- renderText({
    n_plays <- input$n_of_plays
    "There are two boxes. One of them contains $2. 
     If you pick the box with cash, you win. 
     If you pick the empty box, the cash in the other box gets doubled. 
     Pick until you win!"
  })

  data <- reactive({
    if (input$submit > 0) {
      results <- play_game(n_of_plays = input$n_of_plays)
      df <- data.frame(
        mean_win = round(mean(results$results_win), 2),
        mode_win = DescTools::Mode(results$results_win),
        max_win = max(results$results_win),
        mean_picks = round(mean(results$results_picks), 2),
        mode_picks = DescTools::Mode(results$results_picks),
        max_picks = max(results$results_picks)
      )
      df
    }
  })

  output$table <- renderTable({
    data()
  })

  output$avg_win <- renderText({
    data <- data()
    if (input$submit > 0) {
      text <- glue("On average, you won ${data$mean_win} per game, but your most
                   common win was just ${data$mode_win}. However, in one game 
                   you won an astonishing ${data$max_win}. Amazing, or?")
      text
    }
  })

  output$avg_picks <- renderText({
    data <- data()
    if (input$submit > 0) {
      text <- glue("It usually took {data$mean_picks} picks to find the box with 
                   money even though very often it was really just {data$mode_picks} 
                   pick. But you also had to pick {data$max_picks} times to find 
                   the cash. With just two boxes!")
      text
    }
  })
  
  output$summary_picks <- renderText({
    data <- data()
    if (input$submit > 0) {
      text <- "Here you can see a plot summarizing the number of picks it took to 
               find the box with cash:"
      text
    }
  })
  
  output$summary_win <- renderText({
    data <- data()
    if (input$submit > 0) {
      n_plays <- input$n_of_plays
      text <- glue("And of course you wonder how much money you accumulated
                    in {n_plays} games. Here you see how your wallet
                    grew tight while playing:")
      text
    }
  })

  output$picks_plot <- renderPlot({
    results <- play_game(input$n_of_plays)
    if (input$submit > 0) {

      results <- play_game(input$n_of_plays)
      results$game_number <- 1:nrow(results)
  
      ggplot(data = results, aes(x = results_picks)) +
                geom_bar(fill = "#4CA8C8") +
                ggtitle("Number of Picks") +
                xlab("Number of Picks") +
                ylab("Frequency") +
                scale_y_log10() +
                theme_ipsum(plot_title_face = "plain",
                            plot_title_size = 14,
                            grid="Y")
          
      # p2 <- ggplot(data = results, aes(x = game_number, y = cumsum(results_win))) +
      #   geom_line(color = "#9474CA", size=2) +
      #   ggtitle("Accumulated Prize over Number of Games") +
      #   xlab("Number of Games") +
      #   ylab("Accumulated Prize in $") +
      #   theme_ipsum() +
      #         theme(plot.background = element_rect(fill = "#FAFAFA"),
      #               panel.grid.major = element_blank(),
      #               panel.grid.minor = element_blank(),
      #               axis.line = element_blank())
      # 

    }
  })
  
  output$wins_plot <- renderPlot({
    results <- play_game(input$n_of_plays)
    if (input$submit > 0) {

      results <- play_game(input$n_of_plays)
      results$game_number <- 1:nrow(results)
          
      ggplot(data = results, aes(x = game_number, y = cumsum(results_win))) +
        geom_line(color = "#9474CA", size=2) +
        ggtitle("Accumulated Prize over Number of Games") +
        xlab("Number of Games") +
        ylab("Accumulated Prize in $") +
                theme_ipsum(plot_title_face = "plain",
                            plot_title_size = 14,
                            grid="Y")
      # 

    }
  })
}

## Run app
shinyApp(ui = ui, server = server)
