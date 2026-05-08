library(shiny)
library(dplyr)
library(ggplot2)
library(DT)
library(shinyBS)
library(plotly)

addResourcePath("player_images", "www/player_images")

help_text <- function(text) {
  tags$span(
    "?",
    style = "
      display: inline-block;
      position: relative;
      background-color: #ddd;
      color: #333;
      border-radius: 50%;
      width: 18px;
      height: 18px;
      text-align: center;
      font-size: 12px;
      font-weight: bold;
      cursor: help;
      margin-left: 6px;
    ",
    tags$span(
      text,
      class = "tooltip-text"
    )
  )
}

ui <- fluidPage(
  
  # Hide slider numbers
  tags$head(
    tags$style(HTML("
    .irs-min, .irs-max, .irs-from, .irs-to, .irs-single, .irs-grid-text {
      display: none !important;
    }

    .tooltip-text {
      visibility: hidden;
      width: 260px;
      background-color: #333;
      color: white;
      text-align: left;
      border-radius: 6px;
      padding: 8px;
      position: absolute;
      z-index: 999;
      left: 24px;
      top: -8px;
      font-size: 13px;
      font-weight: normal;
      line-height: 1.4;
    }

    span:hover > .tooltip-text {
      visibility: visible;
    }
  "))
  ),
  
  tags$script(HTML("
  Shiny.addCustomMessageHandler('clickUpdate', function(message) {
    $('#update').click();
  });
")),
  
  # Title
  div(
    style = "padding: 20px 10px 10px 10px; border-bottom: 2px solid #e5e5e5; margin-bottom: 15px;",
    h1("🐐 NBA GOAT Calculator", style = "font-weight: 700; margin-bottom: 5px;"),
    p("Build your own NBA GOAT ranking by adjusting what matters most to you.",
      style = "font-size: 16px; color: #555;")
  ),
  
  # Main app tabs

    sidebarLayout(
      sidebarPanel(
        
        actionButton("help", "How it works"),
        
        br(),
        br(),
        
        # Main category sliders
        h4("Set your GOAT philosophy"),
        p("Move sliders right to increase importance and left to decrease importance.",
          style = "color: #666; font-size: 13px; margin-bottom: 20px;"),
        
        sliderInput(
          "team",
          tagList("🏆 Team Accolades", help_text("Overall team success. Go to Advanced Weights to adjust Championships vs Conference Championships.")),
          min = 1, max = 10, value = 6, step = 1, ticks = FALSE
        ),
        
        sliderInput(
          "individual",
          tagList("🥇 Individual Accolades", help_text("Individual awards and selections. Go to Advanced Weights to adjust MVPs, Finals MVPs, DPOY, All-NBA, All-Defense, and All-Star selections.")),
          min = 1, max = 10, value = 7, step = 1, ticks = FALSE
        ),
        
        sliderInput(
          "stats",
          tagList("📊 Statistics", help_text("Career box score production. Go to Advanced Weights to adjust points, rebounds, assists, steals, and blocks.")),
          min = 1, max = 10, value = 3, step = 1, ticks = FALSE
        ),
        
        sliderInput(
          "advanced",
          tagList("📈 Advanced Stats", help_text("Advanced statistical value. Go to Advanced Weights to adjust PER and Win Shares.")),
          min = 1, max = 10, value = 3, step = 1, ticks = FALSE
        ),
        
        sliderInput(
          "longevity",
          tagList("⏳ Longevity", help_text("Career length based on seasons played. This category does not have additional Advanced Weight controls.")),
          min = 1, max = 10, value = 1, step = 1, ticks = FALSE
        ),
        
        br(),
        
        fluidRow(
          column(
            7,
            actionButton(
              "update",
              "Update Rankings",
              width = "100%",
              class = "btn-primary",
              style = "font-size: 12px; padding-left: 4px; padding-right: 4px;"
            )
          ),
          column(
            5,
            actionButton(
              "reset",
              "Reset",
              width = "100%",
              style = "font-size: 12px; padding-left: 4px; padding-right: 4px;"
            )
          )
        ),
        
        # Filters
        h4(tagList(
          "Filters",
          help_text("Filters show only selected players by era or position. They do not recalculate the model or change overall rank movement.")
        )),
        
        checkboxGroupInput(
          "era",
          "Era",
          choices = c(
            "Classic Era (1950-1979)",
            "Golden Age Era (1980-1999)",
            "Post-Jordan Era (2000-2009)",
            "Modern Era (2010-Present)"
          ),
          selected = c(
            "Classic Era (1950-1979)",
            "Golden Age Era (1980-1999)",
            "Post-Jordan Era (2000-2009)",
            "Modern Era (2010-Present)"
          )
        ),
        
        checkboxGroupInput(
          "position",
          "Position",
          choices = sort(unique(df$position)),
          selected = sort(unique(df$position))
        )
      ),
  
  mainPanel(
    tabsetPanel(
  selected = "Main Rankings",
    
  tabPanel(
    "Main Rankings",
    
    div(
      style = "padding: 15px 5px;",
      h3("GOAT Rankings", style = "font-weight: 700;"),
      p("Explore how player rankings change based on your GOAT philosophy.",
        style = "color: #555; font-size: 15px;")
    ),
    
    DTOutput("rankings")
  ),
    
    tabPanel(
      "Visualization",
      
      div(
        style = "padding: 15px 5px;",
        h3("GOAT Score Visualization", style = "font-weight: 700;"),
        p("This chart shows how the current ranking scores compare across players.",
          style = "color: #555; font-size: 15px;")
      ),
      
      plotlyOutput("goat_plot", height = "700px")
    ),
    
  tabPanel(
    "Advanced Weights",
    
    div(
      style = "padding: 15px 5px;",
      h3("Advanced Weight Controls", style = "font-weight: 700;"),
      p("Fine-tune how individual statistics and accolades contribute to the final GOAT score.",
        style = "color: #555; font-size: 15px;")
    ),
    
    fluidRow(
        
        column(
          6,
          
          wellPanel(
            h4("🏆 Team Accolades"),
            p("Adjust how much championships and conference championships matter inside the team category."),
            sliderInput("champ_w", "Championships", min = 1, max = 10, value = 9, step = 1, ticks = FALSE),
            sliderInput("conf_w", "Conference Championships", min = 1, max = 10, value = 1, step = 1, ticks = FALSE)
          ),
          
          wellPanel(
            h4("🥇 Individual Accolades"),
            p("Adjust how much each individual award or selection matters."),
            sliderInput("mvp_w", "MVPs", min = 1, max = 10, value = 6, step = 1, ticks = FALSE),
            sliderInput("finals_w", "Finals MVPs", min = 1, max = 10, value = 6, step = 1, ticks = FALSE),
            sliderInput("dpoy_w", "DPOY", min = 1, max = 10, value = 3, step = 1, ticks = FALSE),
            sliderInput("allnba_w", "All-NBA", min = 1, max = 10, value = 2, step = 1, ticks = FALSE),
            sliderInput("alldef_w", "All-Defense", min = 1, max = 10, value = 2, step = 1, ticks = FALSE),
            sliderInput("allstar_w", "All-Star", min = 1, max = 10, value = 1, step = 1, ticks = FALSE)
          )
        ),
        
        column(
          6,
          
          wellPanel(
            h4("📊 Statistics"),
            p("Adjust how much each career box score statistic matters."),
            sliderInput("points_w", "Points", min = 1, max = 10, value = 4, step = 1, ticks = FALSE),
            sliderInput("rebounds_w", "Rebounds", min = 1, max = 10, value = 2, step = 1, ticks = FALSE),
            sliderInput("assists_w", "Assists", min = 1, max = 10, value = 2, step = 1, ticks = FALSE),
            sliderInput("steals_w", "Steals", min = 1, max = 10, value = 1, step = 1, ticks = FALSE),
            sliderInput("blocks_w", "Blocks", min = 1, max = 10, value = 1, step = 1, ticks = FALSE)
          ),
          
          wellPanel(
            h4("📈 Advanced Stats"),
            p("Adjust how much PER and Win Shares matter inside the advanced stats category."),
            sliderInput("per_w", "PER", min = 1, max = 10, value = 5, step = 1, ticks = FALSE),
            sliderInput("ws_w", "Win Shares", min = 1, max = 10, value = 5, step = 1, ticks = FALSE)
          )
        )
      )
    ),
    )
  )))

server <- function(input, output, session) {
  
  # Create default ranking (baseline model)
  default_rankings <- df |> 
    ungroup() |>
    arrange(desc(goat_score)) |> 
    mutate(Rank = row_number()) |>
    select(player, Rank)
  
  # Store previous ranking (start with default)
  previous_rankings <- reactiveVal(NULL)
  
  
  # Show explanation when app opens
  observeEvent(TRUE, {
    showModal(modalDialog(
      title = "Welcome to the NBA GOAT Calculator",
      "This app ranks NBA players using five categories: team accolades, individual accolades, statistics, advanced stats, and longevity.",
      "Use the Main Rankings tab to adjust the overall importance of each category.",
      "Use the Visualization tab to see the GOAT scores as a graph.",
      "Use the Advanced Weights tab to fine-tune the specific variables inside each category.",
      "Click Update Rankings after changing the sliders to generate a new ranking and compare it to the previous one.",
      easyClose = TRUE,
      footer = NULL
    ))
  }, once = TRUE)
  
  # Show explanation again when button is clicked
  observeEvent(input$help, {
    showModal(modalDialog(
      title = "Welcome to the NBA GOAT Calculator",
      "This app ranks NBA players using five categories: team accolades, individual accolades, statistics, advanced stats, and longevity.",
      "Use the Main Rankings tab to adjust the overall importance of each category.",
      "Use the Visualization tab to see the GOAT scores as a graph.",
      "Use the Advanced Weights tab to fine-tune the specific variables inside each category.",
      "Click Update Rankings after changing the sliders to generate a new ranking and compare it to the previous one.",
      easyClose = TRUE,
      footer = NULL
    ))
  })
  
  observeEvent(input$reset, {
    
    # MAIN CATEGORY SLIDERS
    updateSliderInput(session, "team", value = 6)
    updateSliderInput(session, "individual", value = 7)
    updateSliderInput(session, "stats", value = 3)
    updateSliderInput(session, "advanced", value = 3)
    updateSliderInput(session, "longevity", value = 1)
    
    # TEAM WEIGHTS
    updateSliderInput(session, "champ_w", value = 9)
    updateSliderInput(session, "conf_w", value = 1)
    
    # INDIVIDUAL WEIGHTS
    updateSliderInput(session, "mvp_w", value = 6)
    updateSliderInput(session, "finals_w", value = 6)
    updateSliderInput(session, "dpoy_w", value = 3)
    updateSliderInput(session, "allnba_w", value = 2)
    updateSliderInput(session, "alldef_w", value = 2)
    updateSliderInput(session, "allstar_w", value = 1)
    
    # STATS
    updateSliderInput(session, "points_w", value = 4)
    updateSliderInput(session, "rebounds_w", value = 2)
    updateSliderInput(session, "assists_w", value = 2)
    updateSliderInput(session, "steals_w", value = 1)
    updateSliderInput(session, "blocks_w", value = 1)
    
    # ADVANCED
    updateSliderInput(session, "per_w", value = 5)
    updateSliderInput(session, "ws_w", value = 5)
    
    session$onFlushed(function() {
      session$sendCustomMessage("clickUpdate", list())
    }, once = TRUE)
  })
  
  current_rankings <- eventReactive(input$update, {
    
    rankings <- df
    
    # Save old rankings BEFORE recalculating
    old_rankings <- previous_rankings()
    
    # Balance main category weights
    total_main <- input$team + input$individual + input$stats + input$advanced + input$longevity
    
    team_weight <- input$team / total_main
    individual_weight <- input$individual / total_main
    stats_weight <- input$stats / total_main
    advanced_weight <- input$advanced / total_main
    longevity_weight <- input$longevity / total_main
    
    # Balance team weights
    total_team <- input$champ_w + input$conf_w
    
    champ_weight <- input$champ_w / total_team
    conf_weight <- input$conf_w / total_team
    
    # Create team score
    rankings$team_score_custom <- champ_weight * rankings$z_championships +
      conf_weight * rankings$z_conf_champ
    
    # Balance individual weights
    total_individual <- input$mvp_w + input$finals_w + input$dpoy_w +
      input$allnba_w + input$alldef_w + input$allstar_w
    
    mvp_weight <- input$mvp_w / total_individual
    finals_weight <- input$finals_w / total_individual
    dpoy_weight <- input$dpoy_w / total_individual
    allnba_weight <- input$allnba_w / total_individual
    alldef_weight <- input$alldef_w / total_individual
    allstar_weight <- input$allstar_w / total_individual
    
    # Create individual score
    rankings$individual_score_custom <- mvp_weight * rankings$z_mvps +
      finals_weight * rankings$z_finals_mvps +
      dpoy_weight * rankings$z_dpoy +
      allnba_weight * rankings$z_all_nba +
      alldef_weight * rankings$z_all_def +
      allstar_weight * rankings$z_all_star
    
    # Balance stats weights
    total_stats <- input$points_w + input$rebounds_w + input$assists_w +
      input$steals_w + input$blocks_w
    
    points_weight <- input$points_w / total_stats
    rebounds_weight <- input$rebounds_w / total_stats
    assists_weight <- input$assists_w / total_stats
    steals_weight <- input$steals_w / total_stats
    blocks_weight <- input$blocks_w / total_stats
    
    # Create stats score
    rankings$stats_score_custom <- points_weight * rankings$z_points +
      rebounds_weight * rankings$z_rebounds +
      assists_weight * rankings$z_assists +
      steals_weight * rankings$z_steals +
      blocks_weight * rankings$z_blocks
    
    # Balance advanced weights
    total_advanced <- input$per_w + input$ws_w
    
    per_weight <- input$per_w / total_advanced
    ws_weight <- input$ws_w / total_advanced
    
    # Create advanced score
    rankings$advanced_score_custom <- per_weight * rankings$z_per +
      ws_weight * rankings$z_win_shares
    
    # Create final GOAT score
    rankings$goat_score <- team_weight * rankings$team_score_custom +
      individual_weight * rankings$individual_score_custom +
      stats_weight * rankings$stats_score_custom +
      advanced_weight * rankings$advanced_score_custom +
      longevity_weight * rankings$longevity_score
    
    # Sort rankings
    rankings <- rankings |> 
      ungroup() |>
      arrange(desc(goat_score)) |> 
      mutate(
        Rank = row_number(),
        GOAT_Score = round(
          70 + 30 * (goat_score - min(goat_score)) / 
            (max(goat_score) - min(goat_score)),
          1
        )
      )
    
    # Get previous rankings
    previous <- previous_rankings()
    
    # Only join previous rankings if they exist
    if (!is.null(previous)) {
      
      rankings <- rankings |> 
        left_join(
          previous |> select(player, Previous_Rank = Rank),
          by = "player"
        )
      
    } else {
      
      rankings <- rankings |> 
        mutate(Previous_Rank = NA)
    }
    
    # THEN calculate movement
    rankings <- rankings |> 
      mutate(
        Previous_Rank_Display = ifelse(
          is.na(Previous_Rank),
          "-",
          as.character(Previous_Rank)
        ),
        Movement = Previous_Rank - Rank,
        Movement_Icon = case_when(
          Movement > 0 ~ "↑",
          Movement < 0 ~ "↓",
          TRUE ~ "—"
        )
      )
    
    # Final output
    rankings <- rankings |> 
      select(Movement_Icon, Rank, player, Previous_Rank_Display, GOAT_Score, era, position)
    
    # Update stored rankings AFTER calculations finish
    previous_rankings(
      rankings |> select(player, Rank)
    )
    
    rankings
    
  }, ignoreInit = FALSE, ignoreNULL = FALSE)
  
  
  output$rankings <- renderDT({
    
    table_data <- current_rankings() |> 
      filter(
        sapply(era, function(x) any(input$era %in% unlist(strsplit(x, ", ")))),
        position %in% input$position
      ) |> 
      mutate(
        image = paste0("player_images/", tolower(gsub(" ", "_", player)), ".jpg"),
        image_tag = paste0('<img src="', image, '" height="40" style="border-radius:50%; margin-right:8px;">'),
        player = paste0(image_tag, player)
      ) |> 
      select(Movement_Icon, Rank, player, Previous_Rank_Display, GOAT_Score) |> 
      rename(
        " " = Movement_Icon,
        "Overall Rank" = Rank,
        "Player" = player,
        "Previous Rank" = Previous_Rank_Display,
        "GOAT Score" = GOAT_Score
      ) |> 
    
    mutate(
      `GOAT Score` = sprintf("%.1f", `GOAT Score`)
    )
    
    datatable(
      table_data,
      escape = FALSE,
      rownames = FALSE,
      options = list(
        pageLength = 30,
        lengthChange = FALSE
      )
    ) |> 
      formatStyle(
        " ",
        color = styleEqual(
          c("↑", "↓", "—"),
          c("green", "red", "gray")
        ),
        fontWeight = "bold",
        fontSize = "22px"
      )
  })
  
  output$goat_plot <- renderPlotly({
    
    plot_data <- current_rankings() |> 
      filter(
        sapply(era, function(x) any(input$era %in% unlist(strsplit(x, ", ")))),
        position %in% input$position
      ) |> 
      mutate(
        Tier = case_when(
          Rank <= 3 ~ "Top 3",
          Rank <= 10 ~ "Top 10",
          TRUE ~ "Others"
        ),
        
        Tier = factor(
          Tier,
          levels = c("Top 3", "Top 10", "Others")
        )
      )
    
    p <- ggplot(plot_data, aes(x = GOAT_Score, y = reorder(player, GOAT_Score), fill = Tier)) +
      geom_col(width = 0.75) +
      geom_text(aes(label = GOAT_Score),
                hjust = -0.2,
                size = 3) +
      scale_fill_manual(
        values = c(
          "Top 3" = "#f4c542",
          "Top 10" = "#3b82f6",
          "Others" = "#9ca3af"
        )
      ) +
      coord_cartesian(xlim = c(0, 105)) +
      labs(
        x = "GOAT Score",
        y = "",
        fill = ""
      ) +
      theme_minimal() +
      theme(
        plot.margin = margin(t = -20),
        
        legend.position = "top",
        legend.title = element_blank(),
        
        axis.text.y = element_text(
          size = 12,
          face = "bold"
        ),
        
        axis.title.x = element_text(
          size = 14,
          face = "bold"
        )
      )
    
    ggplotly(p) |>
      layout(
        margin = list(
          t = 20,
          b = 80
        )
      )
  })
}

# Run app
shinyApp(ui = ui, server = server)