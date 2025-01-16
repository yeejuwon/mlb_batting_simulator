# # Install necessary libraries
# install.packages('shiny')
# install.packages('dplyr')
# install.packages('ggplot2')
# install.packages('DT')

# Load necessary libraries
library(shiny)
library(dplyr)
library(ggplot2)
library(DT)


# Data loading function
load_data <- function() {
  batter_stats <- read.csv("data/cluster_stats.csv")
  
  probs_rhp <- read.csv("data/batstatmat_v_rhp.csv")
  probs_lhp <- read.csv("data/batstatmat_v_lhp.csv")
  
  column_names <- colnames(read.csv("data/outs_matrix.csv", nrows = 1))
  col_classes <- c("character", rep("numeric", length(column_names) - 1))
  outs_matrix <- read.csv(
    "data/outs_matrix.csv",
    colClasses = col_classes,
    stringsAsFactors = FALSE
  )
  
  column_names <- colnames(read.csv("data/runs_matrix.csv", nrows = 1))
  col_classes <- c("character", rep("numeric", length(column_names) - 1))
  runs_matrix <- read.csv(
    "data/runs_matrix.csv",
    colClasses = col_classes,
    stringsAsFactors = FALSE
  )
  
  state_transitions <- read.csv(
    "data/state_transition_matrix.csv",
    colClasses = rep("character", ncol(read.csv("data/state_transition_matrix.csv", nrows = 1)))
  )
  
  player_cluster <- read.csv("data/batting_stats_final.csv", row.names = 1)
  colnames(player_cluster)[colnames(player_cluster) == "wRC."] <- "wRC+"
  
  # Set row names and remove first column
  rownames(runs_matrix) <- runs_matrix$current_state
  runs_matrix <- runs_matrix[, -1]
  
  rownames(outs_matrix) <- outs_matrix$current_state
  outs_matrix <- outs_matrix[, -1]
  
  rownames(state_transitions) <- state_transitions$current_state
  state_transitions <- state_transitions[, -1]
  
  list(
    batter_stats = batter_stats,
    probs_rhp = probs_rhp,
    probs_lhp = probs_lhp,
    outs_matrix = outs_matrix,
    runs_matrix = runs_matrix,
    state_transitions = state_transitions,
    player_cluster = player_cluster
  )
}

# Define cluster nicknames
cluster_info <- data.frame(
  cluster = 1:8,
  nickname = c(
    "Disciplined Line Drive Hitters (R)",
    "Contact-Oriented Ground Ball Hitters (R)",
    "Aggressive All-Field Hitters (R)",
    "Powerful Pull-Hitters (R)",
    "Aggressive Power Hitters (L)",
    "Elite All-Around Sluggers (L)",
    "Patient Line-Drive Hitters (L)",
    "Contact-Oriented Ground Ball Hitters (L)"
  )
)

# Park factor adjustment function
adjust_runs <- function(runs, home_team, handedness) {
  lhb_factors <- data.frame(
    team = c('COL', 'BOS', 'CIN', 'MIA', 'PIT', 'WSN', 'PHI', 'KCR', 'MIN', 'HOU', 'TEX', 'BAL', 'ATL', 'CHC', 'CHW', 'CLE', 'NYY', 'ARI', 'LAA', 'MIL', 'TOR', 'LAD', 'STL', 'DET', 'SFG', 'NYM', 'SDP', 'TBR', 'SEA', 'OAK'),
    factor = c(1.11, 1.1, 1.05, 1.04, 1.04, 1.04, 1.03, 1.03, 1.02, 1.01, 1.01, 1.0, 0.99, 0.99, 0.99, 0.99, 0.99, 0.99, 0.98, 0.98, 0.98, 0.98, 0.98, 0.97, 0.96, 0.96, 0.96, 0.94, 0.93, 0.97)
  )
  
  rhb_factors <- data.frame(
    team = c('COL', 'BOS', 'KCR', 'CIN', 'ARI', 'LAA', 'TOR', 'MIN', 'LAD', 'STL', 'ATL', 'TEX', 'CHW', 'MIA', 'HOU', 'PHI', 'NYY', 'BAL', 'WSN', 'TBR', 'PIT', 'DET', 'NYM', 'OAK', 'MIL', 'SFG', 'SDP', 'CHC', 'CLE', 'SEA'),
    factor = c(1.13, 1.04, 1.04, 1.04, 1.03, 1.02, 1.02, 1.02, 1.01, 1.01, 1.01, 1.01, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 0.99, 0.99, 0.98, 0.98, 0.98, 0.97, 0.97, 0.97, 0.97, 0.96, 0.95, 0.92)
  )
  
  if (handedness == "L") {
    factor <- lhb_factors$factor[lhb_factors$team == home_team]
  } else {
    factor <- rhb_factors$factor[rhb_factors$team == home_team]
  }
  if (length(factor) == 1) {
    return(runs * factor)
  } else {
    return(runs)  # Default if team not found
  }
}

# Simulation function
run_simulation <- function(input_data) {
  num_simulations <- as.integer(input_data$num_simulations)
  p_throws <- ifelse(input_data$p_throws == "Right", "R", "L")
  home_team <- input_data$home_team
  
  lineup_names <- strsplit(input_data$lineup_names, ",")[[1]]
  lineup_names <- trimws(lineup_names)
  
  lineup_clusters <- sapply(lineup_names, function(name) {
    batter_cluster <- input_data$player_cluster[input_data$player_cluster$Name_FG == name, "batter_cluster"]
    if (length(batter_cluster) == 0) {
      return(NA)
    } else {
      return(batter_cluster)
    }
  })
  
  event_names <- c("DoublePlay", "Error", "GroundOut", "HBP.CatInt", "HR", "K", 
                   "LineOut.InfFly", "Triple", "Walk", "LongSingle", "MediumSingle", 
                   "ShortSingle", "ShortDouble", "LongDouble", "LongFly", "MediumFly", "ShortFly")
  
  avg_runs <- numeric(num_simulations)
  
  for (sim in 1:num_simulations) {
    total_runs <- 0
    lineup_index <- 1 
    num_innings <- 9
    
    for (inning in 1:num_innings) {
      current_state <- '0000' 
      inning_runs <- 0
      outs_count <- 0
      
      while (outs_count < 3) {  
        batter_cluster <- lineup_clusters[lineup_index]
        
        if (is.na(batter_cluster)) {
          next  
        }

        if (p_throws == "R") {
          probs <- input_data$probs_rhp
        } else {
          probs <- input_data$probs_lhp
        }
        
        player_probs <- probs[probs$batter_cluster == batter_cluster, ][-1]
        
        event_probabilities <- cumsum(as.numeric(player_probs))
        random_value <- runif(1) 
        event_index <- which(event_probabilities >= random_value)[1]

        event_name <- event_names[event_index]
        
        inning_runs <- inning_runs + as.numeric(input_data$runs_matrix[current_state, event_index])
        outs_increment <- as.numeric(input_data$outs_matrix[current_state, event_index])
        outs_count <- outs_count + outs_increment
        current_state <- input_data$state_transitions[current_state, event_index]
        
        lineup_index <- (lineup_index %% length(lineup_names)) + 1
      }
      
      total_runs <- total_runs + inning_runs
    }
    
    avg_runs[sim] <- total_runs
  }
  
  # Return Simulation Results
  list(
    avg_runs = mean(avg_runs),
    max_runs = max(avg_runs),
    min_runs = min(avg_runs),
    adjusted_runs = avg_runs
  )
}

find_best_lineup_position <- function(new_player_name, input_data) {

  match <- input_data$player_cluster[input_data$player_cluster$Name_FG == new_player_name, ]
  if (nrow(match) == 0) {
    print(paste("Player", new_player_name, "not found in player_cluster."))
    return(NULL)
  }

  new_player_cluster <- match$batter_cluster[1]
  print(paste("New Player", new_player_name, "assigned to Cluster", new_player_cluster))

  lineup_names <- strsplit(input_data$lineup_names, ",")[[1]]
  lineup_names <- trimws(lineup_names)
  lineup_clusters <- sapply(lineup_names, function(name) {
    batter_cluster <- input_data$player_cluster[input_data$player_cluster$Name_FG == name, "batter_cluster"]
    if (length(batter_cluster) == 0) {
      return(NA)
    } else {
      return(batter_cluster)
    }
  })
  
  best_position <- NULL
  best_avg_runs <- -Inf
  results <- list()
  
  for (i in 1:9) {
    test_lineup_clusters <- append(lineup_clusters, new_player_cluster, after = i - 1)
    test_lineup_names <- append(lineup_names, new_player_name, after = i - 1)
    
    sim_data <- run_simulation(list(
      num_simulations = input_data$num_simulations,
      p_throws = input_data$p_throws,
      home_team = input_data$home_team,
      lineup_names = test_lineup_names,
      player_cluster = input_data$player_cluster,
      probs_rhp = input_data$probs_rhp,
      probs_lhp = input_data$probs_lhp,
      outs_matrix = input_data$outs_matrix,
      runs_matrix = input_data$runs_matrix,
      state_transitions = input_data$state_transitions
    ))
    
    avg_runs <- sim_data$avg_runs
    results[[i]] <- list(position = i, avg_runs = avg_runs)

    if (avg_runs > best_avg_runs) {
      best_position <- i
      best_avg_runs <- avg_runs
    }
  }
  
  print("Simulation Results for Each Lineup Position:")
  for (res in results) {
    print(paste("Position", res$position, ": Average Runs =", round(res$avg_runs, 4)))
  }
  
  print(paste("Best Position for", new_player_name, ":", best_position, "(Average Runs:", round(best_avg_runs, 4), ")"))
  
  return(list(best_position = best_position, best_avg_runs = best_avg_runs))
}

find_best_cluster_for_position <- function(position, input_data) {

  lineup_names <- strsplit(input_data$lineup_names, ",")[[1]]
  lineup_names <- trimws(lineup_names)
  lineup_clusters <- sapply(lineup_names, function(name) {
    batter_cluster <- input_data$player_cluster[input_data$player_cluster$Name_FG == name, "batter_cluster"]
    if (length(batter_cluster) == 0) {
      return(NA)
    } else {
      return(batter_cluster)
    }
  })

  if (position < 1 || position > length(lineup_clusters)) {
    print(paste("Invalid position:", position, ". Must be between 1 and", length(lineup_clusters), "."))
    return(NULL)
  }
  
  best_cluster <- NULL
  best_avg_runs <- -Inf
  results <- list()
  
  for (cluster in unique(input_data$player_cluster$batter_cluster)) {

    test_lineup_clusters <- lineup_clusters
    test_lineup_clusters[position] <- cluster 
    
    sim_data <- run_simulation(list(
      num_simulations = input_data$num_simulations,
      p_throws = input_data$p_throws,
      home_team = input_data$home_team,
      lineup_names = input_data$lineup_names,
      player_cluster = input_data$player_cluster,
      probs_rhp = input_data$probs_rhp,
      probs_lhp = input_data$probs_lhp,
      outs_matrix = input_data$outs_matrix,
      runs_matrix = input_data$runs_matrix,
      state_transitions = input_data$state_transitions
    ))
    
    avg_runs <- sim_data$avg_runs
    results[[length(results) + 1]] <- list(cluster = cluster, avg_runs = avg_runs)
    
    if (avg_runs > best_avg_runs) {
      best_cluster <- cluster
      best_avg_runs <- avg_runs
    }
  }
  
  sorted_results <- results[order(sapply(results, function(x) x$avg_runs), decreasing = TRUE)]
  
  print(paste("Simulation Results for Position", position, "against", input_data$p_throws, "HP (Sorted by Average Runs):"))
  for (res in sorted_results) {
    print(paste("Cluster", res$cluster, ": Average Runs =", round(res$avg_runs, 4)))
  }
  
  print(paste("Best Cluster for Position", position, ":", best_cluster, "(Average Runs:", round(best_avg_runs, 4), ")"))
  return(best_cluster)
}


########################################################################################################################################
########################################################################################################################################
########################################################################################################################################

# Shiny app UI
ui <- navbarPage(
  "CSAS 2025 : MLB Batting Lineup Simulator",

  tabPanel("Home",  
           fluidPage(
             titlePanel("Welcome to the Yonsei Blues Webpage"),
             
             sidebarLayout(
               sidebarPanel(
                 h3("About Yonsei Blues"),
                 p("Yonsei Blues is a team of undergraduate students from Yonsei Universit in Korea, all members of the Yonsei Sports Analytics Lab (YSAL).
                   Our team, Yonsei Blues, is dedicated to applying statistical methods to analyze sports data, with a particular interest in baseball and MLB. Through our analyses, we aim to provide valuable insights that can be applied in professional fields")
               ),
               
               mainPanel(
                 h4("About the App:"),
                 p("This webpage consists of four main sections. It offers a tool for users to create their own batting lineups and run simulations to analyze their effectiveness. Users can explore optimal batting orders, determine the best batting position for a new player, and identify the most suitable player clusters for different batting positions. This aims to assist in forming optimal batter lineups that can be practically used by MLB teams."),
                 
                 p("The four sections are as follows:"),
                 tags$ol(
                   tags$li(h5("Batter Cluster Summary:"),
                           p("In this section, users can explore and compare the stats of MLB batters classified into 8 distinct clusters. Each cluster represents a different batting style, and the section provides a summary of the key characteristics of each cluster.")),
                   tags$li(h5("Batting Lineup Simulation:"),
                           p("In this section, users can select a team and create a batting lineup by inputting players. It simulates the expected runs generated by the input lineup and displays the distribution of runs across simulations.")),
                   tags$li(h5("Best Batting Position:"),
                           p("This section helps users determine the best batting position for a new player in an existing lineup. The simulation identifies the player's cluster and suggests the optimal spot for the player within the current lineup.")),
                   tags$li(h5("Best Cluster for Batting Position:"),
                           p("This section identifies the best batter cluster for each batting position (1-9) in the lineup. For example, the simulation may suggest that cluster 3 is the most suitable for position 1, and it will recommend the top 10 players from that cluster based on their wRC+ values."))
                 )
               )
             )
           )
  ),
  
  tabPanel("1. Cluster Characteristics Summary",
           fluidPage(
             titlePanel("Characteristics of each cluster (1~8)"),
             sidebarLayout(
               sidebarPanel(
                 h4("Clusters with Keywords"),
                 uiOutput("cluster_nicknames")
               ),
               mainPanel(
                 h4("Standard Stats"),
                 DTOutput("standard_stats_table"),
                 br(),
                 h4("Advanced Stats"),
                 DTOutput("advanced_stats_table"),
                 br(),
                 h4("Batted Ball Stats"),
                 DTOutput("batted_ball_stats_table"),
                 br(),
                 h4("Plate Discipline Stats"),
                 DTOutput("plate_discipline_stats_table")
               )
             )
           )
  ),
  
      
  tabPanel("2. Batting Lineup Simulation", 
           sidebarLayout(
             sidebarPanel(
               numericInput("num_simulations", "Number of simulations:", 1000, min = 1, max = 10000),
               selectInput("p_throws", "Pitcher Handedness:", choices = c("Right", "Left")),
               selectInput("home_team", "Home Team:", choices = sort(c("BOS", "NYM", "LAD", "HOU", "CHC", "SFG", "STL", "ATL", "COL", "PHI",
                                                                       "SDP", "MIN", "MIA", "BAL", "CIN", "CLE", "TBR", "OAK", "KCR", "TEX",
                                                                       "DET", "CHW", "SEA", "PIT", "WSN", "TOR", "MIL", "ARI", "LAA", "NYY"))),
               uiOutput("lineup_setting_1"),
               actionButton("run_simulation", "Run Simulation")
             ),
             mainPanel(
               verbatimTextOutput("simulation_results"),
               plotOutput("runs_plot"),
               tableOutput("lineup_data_1")
             )
           )
  ),
  
  tabPanel("3. Best Batting Position", 
           sidebarLayout(
             sidebarPanel(
               numericInput("num_simulations", "Number of simulations:", 1000, min = 1, max = 10000),
               selectInput("p_throws", "Pitcher Handedness:", choices = c("Right", "Left")),
               selectInput("home_team", "Home Team:", choices = sort(c("BOS", "NYM", "LAD", "HOU", "CHC", "SFG", "STL", "ATL", "COL", "PHI",
                                                                       "SDP", "MIN", "MIA", "BAL", "CIN", "CLE", "TBR", "OAK", "KCR", "TEX",
                                                                       "DET", "CHW", "SEA", "PIT", "WSN", "TOR", "MIL", "ARI", "LAA", "NYY"))),
               uiOutput("new_player_name_ui"),
               uiOutput("lineup_setting_2"),
               actionButton("find_best_position", "Find Best Position")
             ),
             mainPanel(
               uiOutput("lineup_data_title"),
               tableOutput("lineup_data_2"),
               
               uiOutput("best_position_title"),
               verbatimTextOutput("best_position"),

               uiOutput("no_results_message_2")
             )
           )
  ),
  
  tabPanel("4. Best Cluster for Batting Position", 
           sidebarLayout(
             sidebarPanel(
               numericInput("num_simulations", "Number of simulations:", 1000, min = 1, max = 10000),
               selectInput("p_throws", "Pitcher Handedness:", choices = c("Right", "Left")),
               selectInput("home_team", "Home Team:", choices = sort(c("BOS", "NYM", "LAD", "HOU", "CHC", "SFG", "STL", "ATL", "COL", "PHI",
                                                                       "SDP", "MIN", "MIA", "BAL", "CIN", "CLE", "TBR", "OAK", "KCR", "TEX",
                                                                       "DET", "CHW", "SEA", "PIT", "WSN", "TOR", "MIL", "ARI", "LAA", "NYY"))),
               uiOutput("lineup_setting_3"),
               numericInput("position", "Enter Position (1-9):", value = 1, min = 1, max = 9),
               actionButton("find_best_cluster", "Find Best Cluster")
             ),
             mainPanel(
               uiOutput("current_lineup"),
               tableOutput("lineup_data_3"),
               
               uiOutput("best_cluster_title"),
               verbatimTextOutput("best_cluster"),
               
               uiOutput("player_recommender_title"),
               tableOutput("player_recommender"),
               
               uiOutput("no_results_message_3")
             )
           )
  )
)


#server code
server <- function(input, output, session) {
  data <- load_data()
  
  # Store simulation results in a reactive value
  sim_results <- reactiveVal(NULL)
  
  # Generate the list of players from player_cluster$Name_FG dynamically
  player_names <- sort(unique(data$player_cluster$Name_FG))
  
  # Dynamically generate the selectInput for each batter (1st to 9th)
  output$lineup_setting_1 <- renderUI({
    lapply(1:9, function(i) {
      selectInput(paste0("player_", i), paste0("Batter ", i ,":"), choices = c("Choose Player", player_names))
    })
  })
  
  
  # Render cluster nicknames
  output$cluster_nicknames <- renderUI({
    tags$ul(
      lapply(1:nrow(cluster_info), function(i) {
        tags$li(
          strong(paste("Cluster", cluster_info$cluster[i], ":")), 
          cluster_info$nickname[i]
        )
      })
    )
  })
  
  # Render Standard Stats Table
  output$standard_stats_table <- renderDT({
    datatable(round(data$batter_stats[, c("batter_cluster", "PA", "AB", "AVG", "OBP", "SLG", "OPS")], 3), 
              options = list(pageLength = nrow(data$batter_stats), dom = 't'),
              rownames = FALSE)
  })
  
  # Render Advanced Stats Table
  output$advanced_stats_table <- renderDT({
    advanced_stats <- round(data$batter_stats[, c("batter_cluster", "ISO", "BABIP", "wRC", "wRAA", "wOBA", "wRC.")], 3)
    # Rename columns for display
    colnames(advanced_stats) <- c("Batter Cluster", "ISO", "BABIP", "wRC", "wRAA", "wOBA", "wRC+")
    datatable(advanced_stats, options = list(pageLength = nrow(advanced_stats), dom = 't'), rownames = FALSE)
  })
  
  
  # Render Batted Ball Stats Table
  output$batted_ball_stats_table <- renderDT({
    # Rename and round specific columns
    batted_ball_stats <- data$batter_stats[, c("batter_cluster", "GB.", "FB.", "GB.FB", "HR.FB", "LD.", "Oppo.", "Cent.", "Pull.")]
    batted_ball_stats <- data.frame(
      batter_cluster = batted_ball_stats$batter_cluster,
      GB_pct = round(batted_ball_stats$GB., 3),
      FB_pct = round(batted_ball_stats$FB., 3),
      GB_FB = round(batted_ball_stats$GB.FB, 3),
      HR_FB = round(batted_ball_stats$HR.FB, 3),
      LD_pct = round(batted_ball_stats$LD., 3),
      Oppo_pct = round(batted_ball_stats$Oppo., 3),
      Cent_pct = round(batted_ball_stats$Cent., 3),
      Pull_pct = round(batted_ball_stats$Pull., 3)
    )
    # Rename columns for display
    colnames(batted_ball_stats) <- c("Batter Cluster", "GB%", "FB%", "GB/FB", "HR/FB", "LD%", "Oppo%", "Cent%", "Pull%")
    datatable(batted_ball_stats, options = list(pageLength = nrow(batted_ball_stats), dom = 't'), rownames = FALSE)
  })
  
  # Render Plate Discipline Stats Table
  output$plate_discipline_stats_table <- renderDT({
    # Rename and round specific columns
    plate_discipline_stats <- data$batter_stats[, c("batter_cluster", "BB.", "K.", "BB.K", "f_strike_percent", "whiff_percent", 
                                                    "z_swing_percent", "iz_contact_percent", "z_swing_miss_percent")]
    plate_discipline_stats <- data.frame(
      batter_cluster = plate_discipline_stats$batter_cluster,
      BB_pct = round(plate_discipline_stats$BB., 3),
      K_pct = round(plate_discipline_stats$K., 3),
      BB_K = round(plate_discipline_stats$BB.K, 3),
      f_strike_percent = round(plate_discipline_stats$f_strike_percent, 3),
      whiff_percent = round(plate_discipline_stats$whiff_percent, 3),
      z_swing_percent = round(plate_discipline_stats$z_swing_percent, 3),
      iz_contact_percent = round(plate_discipline_stats$iz_contact_percent, 3),
      z_swing_miss_percent = round(plate_discipline_stats$z_swing_miss_percent, 3)
    )
    # Rename columns for display
    colnames(plate_discipline_stats) <- c("Batter Cluster", "BB%", "K%", "BB/K", "F-Strike%", "Whiff%", "Z-Swing%", "Iz-Contact%", "Z-Swing Miss%")
    datatable(plate_discipline_stats, options = list(pageLength = nrow(plate_discipline_stats), dom = 't'), rownames = FALSE)
  })
  
  

  observeEvent(input$run_simulation, {
    lineup_names <- sapply(1:9, function(i) input[[paste0("player_", i)]])
    
    lineup_clusters <- sapply(lineup_names, function(name) {

      name <- trimws(name)
      batter_cluster <- data$player_cluster[data$player_cluster$Name_FG == name, "batter_cluster"]
      
      if (length(batter_cluster) == 0) {
        return(NA)
      } else {
        return(batter_cluster)
      }
    })

    input_data <- list(
      num_simulations = as.integer(input$num_simulations),
      p_throws = input$p_throws,
      home_team = input$home_team,
      lineup_names = lineup_names,
      player_cluster = data$player_cluster,
      probs_rhp = data$probs_rhp,
      probs_lhp = data$probs_lhp,
      outs_matrix = data$outs_matrix,
      runs_matrix = data$runs_matrix,
      state_transitions = data$state_transitions
    )
    
    sim_data <- run_simulation(input_data)
    sim_results(sim_data)
    
    lineup_data <- data$player_cluster[data$player_cluster$Name_FG %in% lineup_names, ]
    lineup_data <- lineup_data[, c("Name_FG", "Tm", "batter_cluster", "mean_batspeed", "mean_swinglength", "attack_angle", "PA", "AB", "AVG", "OBP", "SLG", "OPS", 'wRC+')]
    lineup_data$mean_batspeed <- format(round(lineup_data$mean_batspeed, 2), nsmall = 2)
    lineup_data$mean_swinglength <- format(round(lineup_data$mean_swinglength, 2), nsmall = 2)
    lineup_data$attack_angle <- format(round(lineup_data$attack_angle, 2), nsmall = 2)
    lineup_data$AVG <- format(round(lineup_data$AVG, 3), nsmall = 3)
    lineup_data$OBP <- format(round(lineup_data$OBP, 3), nsmall = 3)
    lineup_data$SLG <- format(round(lineup_data$SLG, 3), nsmall = 3)
    lineup_data$OPS <- format(round(lineup_data$OPS, 3), nsmall = 3)
    lineup_data <- lineup_data[match(lineup_names, lineup_data$Name_FG), ]
    
    output$lineup_data_1 <- renderTable({
      lineup_data
    })
  })
  
  output$simulation_results <- renderPrint({
    sim_data <- sim_results() 
    if (!is.null(sim_data)) {
      cat("Average Runs Made by Input Batting Lineup:", round(sim_data$avg_runs, 3), "\n")
      cat("Max Runs Made by Input Batting Lineup::", round(sim_data$max_runs, 3), "\n")
      cat("Min Runs Made by Input Batting Lineup::", round(sim_data$min_runs, 3), "\n")
    } else {
      cat("No simulation has been run yet.")
    }
  })
  
  # Plot distribution of runs
  output$runs_plot <- renderPlot({
    sim_data <- sim_results()  # Get the latest simulation results
    if (!is.null(sim_data)) {
      ggplot(data.frame(runs = sim_data$adjusted_runs), aes(x = runs)) +
        geom_histogram(binwidth = 1, fill = "blue", color = "black") +
        labs(title = "Distribution of Runs in Simulations", x = "Runs", y = "Frequency")
    } else {
      plot(1, type = "n", main = "No simulation results to display.")
    }
  })

  output$best_position_title <- renderUI({
    if (is.null(input$find_best_position) || input$find_best_position == 0) {
      return(NULL)
    }
    h3("Best Batting Position Result")
  })
  
  output$lineup_data_title <- renderUI({
    if (is.null(input$find_best_position) || input$find_best_position == 0) {
      return(NULL)
    }
    h3("Optimized Batting Order with New Player")
  })
  
  output$no_results_message_2 <- renderUI({
    if (is.null(input$find_best_position) || input$find_best_position == 0) {
      return(h4("Please start the simulation to view results."))
    }
    return(NULL)
  })

  output$lineup_setting_2 <- renderUI({
    lapply(1:9, function(i) {
      selectInput(paste0("player_", i), paste0("Batter ", i ,":"), choices = c("Choose Player", player_names))
    })
  })

  output$new_player_name_ui <- renderUI({
    selectInput("new_player_name", "Select New Player:", choices = c("Choose Player", player_names))
  })
  
  # reactiveVal for average runs
  avg_runs_reactive <- reactiveVal(NULL)
  avg_runs_per_position <- reactiveVal(rep(NA, 9))
  
  observeEvent(input$find_best_position, {
    new_player_name <- input$new_player_name
    lineup_names <- sapply(1:9, function(i) input[[paste0("player_", i)]])
    
    best_position <- find_best_lineup_position(new_player_name, list(
      num_simulations = input$num_simulations,
      p_throws = input$p_throws,
      home_team = input$home_team,
      lineup_names = lineup_names,
      player_cluster = data$player_cluster,
      probs_rhp = data$probs_rhp,
      probs_lhp = data$probs_lhp,
      runs_matrix = data$runs_matrix,
      outs_matrix = data$outs_matrix,
      state_transitions = data$state_transitions
    ))
    
    output$best_position <- renderPrint({
      if (is.null(best_position)) {
        cat("Player not found or invalid input.")
      } else {
        player_cluster <- data$player_cluster[data$player_cluster$Name_FG == new_player_name, "batter_cluster"]
        if (length(player_cluster) > 0) {
          cat(paste("Cluster for", new_player_name, "is:", player_cluster, "\n"))
        } else {
          cat(paste("Cluster for", new_player_name, "not found.\n"))
        }
        cat(paste("Best Position for", new_player_name, "is:", best_position$best_position,"\n"))
      }
    })
    
    lineup_names[best_position$best_position] <- new_player_name
    lineup_data <- data$player_cluster[data$player_cluster$Name_FG %in% lineup_names, ]
    lineup_data <- lineup_data[, c("Name_FG", "Tm", "batter_cluster", "mean_batspeed", "mean_swinglength", "attack_angle", "PA", "AB", "AVG", "OBP", "SLG", "OPS", "wRC+")]
    
    lineup_data$mean_batspeed <- format(round(lineup_data$mean_batspeed, 2), nsmall = 2)
    lineup_data$mean_swinglength <- format(round(lineup_data$mean_swinglength, 2), nsmall = 2)
    lineup_data$attack_angle <- format(round(lineup_data$attack_angle, 2), nsmall = 2)
    lineup_data$AVG <- format(round(lineup_data$AVG, 3), nsmall = 3)
    lineup_data$OBP <- format(round(lineup_data$OBP, 3), nsmall = 3)
    lineup_data$SLG <- format(round(lineup_data$SLG, 3), nsmall = 3)
    lineup_data$OPS <- format(round(lineup_data$OPS, 3), nsmall = 3)
    lineup_data <- lineup_data[match(lineup_names, lineup_data$Name_FG), ]
    
    output$lineup_data_2 <- renderTable({
      lineup_data 
    })
  })
  
  output$lineup_setting_3 <- renderUI({
    lapply(1:9, function(i) {
      selectInput(paste0("player_", i), paste0("Batter ", i ,":"), choices = c("Choose Player", player_names))
    })
  })

  output$current_lineup <- renderUI({
    if (is.null(input$find_best_cluster) || input$find_best_cluster == 0) {
      return(NULL)
    }
    h3("Current Batting Order")
  })
  
  output$best_cluster_title <- renderUI({
    if (is.null(input$find_best_cluster) || input$find_best_cluster == 0) {
      return(NULL)
    }
    h3("Best Cluster for Input Batting Position")
  })
  
  output$player_recommender_title <- renderUI({
    if (is.null(input$find_best_cluster) || input$find_best_cluster == 0) {
      return(NULL)
    }
    h3("Player Recommendations for Input Batting Position")
  })
  
  
  output$no_results_message_3 <- renderUI({
    if (is.null(input$find_best_cluster) || input$find_best_cluster == 0) {
      return(h4("Please start the simulation to view results."))
    }
    return(NULL)
  })
  
  observeEvent(input$find_best_cluster, {
    position <- input$position
    lineup_names <- sapply(1:9, function(i) input[[paste0("player_", i)]])
    
    best_cluster <- find_best_cluster_for_position(position, list(
      num_simulations = input$num_simulations,
      p_throws = input$p_throws,
      home_team = input$home_team,
      lineup_names = lineup_names,
      player_cluster = data$player_cluster,
      probs_rhp = data$probs_rhp,
      probs_lhp = data$probs_lhp,
      runs_matrix = data$runs_matrix,
      outs_matrix = data$outs_matrix,
      state_transitions = data$state_transitions
    ))
    
    output$best_cluster <- renderPrint({
      if (is.null(best_cluster)) {
        cat("Invalid position or error in finding best cluster.")
      } else {
        cat(paste("Best Cluster for Position", position, "is:", best_cluster))
      }
    })
    
    lineup_data <- data$player_cluster[data$player_cluster$Name_FG %in% lineup_names, ]
    lineup_data <- lineup_data[, c("Name_FG", "Tm", "batter_cluster", "mean_batspeed", "mean_swinglength", "attack_angle", "PA", "AB", "AVG", "OBP", "SLG", "OPS", "wRC+")]
    remaining_players <- data$player_cluster[!(data$player_cluster$Name_FG %in% lineup_names), ]
    recommend_table <- remaining_players[, c("Name_FG", "Tm", "batter_cluster", "mean_batspeed", "mean_swinglength", "attack_angle", "PA", "AB", "AVG", "OBP", "SLG", "OPS", "wRC+")]
    recommend_table <- recommend_table[recommend_table$batter_cluster == best_cluster, ]
    
    sorted_recommend <- recommend_table[order(-recommend_table$`wRC+`), ]
    top_10_players <- head(sorted_recommend, 10)
    
    top_10_players$mean_batspeed <- format(round(top_10_players$mean_batspeed, 2), nsmall = 2)
    top_10_players$mean_swinglength <- format(round(top_10_players$mean_swinglength, 2), nsmall = 2)
    top_10_players$attack_angle <- format(round(top_10_players$attack_angle, 2), nsmall = 2)
    top_10_players$AVG <- format(round(top_10_players$AVG, 3), nsmall = 3)
    top_10_players$OBP <- format(round(top_10_players$OBP, 3), nsmall = 3)
    top_10_players$SLG <- format(round(top_10_players$SLG, 3), nsmall = 3)
    top_10_players$OPS <- format(round(top_10_players$OPS, 3), nsmall = 3)
    
    output$player_recommender <- renderTable({
      top_10_players
    })
    
    lineup_data$mean_batspeed <- format(round(lineup_data$mean_batspeed, 2), nsmall = 2)
    lineup_data$mean_swinglength <- format(round(lineup_data$mean_swinglength, 2), nsmall = 2)
    lineup_data$attack_angle <- format(round(lineup_data$attack_angle, 2), nsmall = 2)
    lineup_data$AVG <- format(round(lineup_data$AVG, 3), nsmall = 3)
    lineup_data$OBP <- format(round(lineup_data$OBP, 3), nsmall = 3)
    lineup_data$SLG <- format(round(lineup_data$SLG, 3), nsmall = 3)
    lineup_data$OPS <- format(round(lineup_data$OPS, 3), nsmall = 3)
    lineup_data <- lineup_data[match(lineup_names, lineup_data$Name_FG), ]
    
    output$lineup_data_3 <- renderTable({
      lineup_data
    })
  })
  
}


# Run the Shiny app
shinyApp(ui = ui, server = server)