# ---------------------------
# pandemic_sandbox.R
# test functions and scenarios
#
# ---------------------------

source("cities.R")
city_names <- ls(envir = city_env)
# Program game constants
game_setup <- function(epidemic_cards = 4,
                       n_players = 2,
                       cube_start = 24,
                       player_card_start = 59,
                       infection_rate_scale = c(2, 2, 3, 3, 4, 4),
                       epidemic_count = 0,
                       outbreak_start = 1,
                       n_city = 12,
                       n_color = 4,
                       max_infect = 3,
                       seed = Sys.time()){
  
  set.seed(seed)
  # Set up board
  board <- list(cities = vector("list", length = n_city * n_color),
       cured = c("black"=FALSE,
                  "blue" = FALSE,
                  "red" = FALSE,
                  "yellow" = FALSE),
       eradicated = c("black"=FALSE,
                       "blue" = FALSE,
                       "red" = FALSE,
                       "yellow" = FALSE),
       cubes_left = c("black"=cube_start,
                       "blue" = cube_start,
                       "red" = cube_start,
                       "yellow" = cube_start),
       outbreak_count = outbreak_start,
       max_infect = max_infect,
       turn = 1,
       epidemic_count = epidemic_count,
       infection_rate_scale = infection_rate_scale,
       infection_rate = infection_rate_scale[1],
       already_infected = c(),
       infection_deck = c(),
       infection_discard = c(),
       player_card_deck = c(),
       player_card_discard = c()
  )
  
  # Add cities to board
  for (city in city_names){
    
    city_obj <- city_env[[city]]
    color <- city_obj@color
    city_list <- list(city = city_obj@name,
                      color = city_obj@color,
                      connections = city_obj@connections,
                      cubes = list(black = 0,
                                   blue = 0,
                                   red = 0,
                                   yellow = 0))
    add_to <- which.min(sapply(board[["cities"]], function(x) length(x) > 1))
    board[["cities"]][[add_to]] <- city_list
    
  }
  names(board$cities) <- city_names
  
  board[["infection_deck"]] <- sample(city_names, length(city_names))
  
  # Create player "hands" and shuffle in epidemic cards. For now just remove cards in hand.
  player_cards_pre <- sample(c(city_names, paste0("EVENT",1:5)))
  hand_start <- 4:2
  hand_cards <- hand_start[n_players-1]
  player_cards_post <- player_cards_pre[-seq(hand_cards*n_players)]
  
  # Add epidemic cards into the appropriate spots
  pc_cards <- length(player_cards_post) %/% epidemic_cards
  pc_extra <- length(player_cards_post) %% epidemic_cards
  pc_cards <- rep(pc_cards, epidemic_cards)
  if (pc_extra != 0) pc_cards[1:pc_extra] <- pc_cards[1:pc_extra] + 1
  pc_bounds <- c(0, Reduce(sum, pc_cards, accumulate = TRUE))
  epi_ind <- sapply(1:length(pc_cards), function(x) sample((pc_bounds[x]+1):pc_bounds[x+1], 1))
  epi_ind <- epi_ind + 0:(length(epi_ind) - 1)
  pc <- player_cards_post
  x <- lapply(epi_ind, function(x) pc <<- c(pc[1:(x-1)], "EPIDEMIC", pc[x:length(pc)]))
  rm(x)
  
  board[["player_card_deck"]] <- pc
  
  return(board)
}



infect_city <- function(board, city, infect_num = 1){
  
  # Check arguments
  if (infect_num < 1 | infect_num > 3) stop("infect_num must be 1, 2, or 3.")
  if (is.null(board[["cities"]][[city]])) stop("city is not on board.")
  
  # Has this city already been infected this turn?
  if (city %in% board[["already_infected"]]) {
    message(sprintf("%s has already been infected this draw", city))
    return(board)
  } else {
    board[["already_infected"]] <- c(city, board[["already_infected"]])
  }
  
  # Is there an outbreak?
  infect_color <- board[["cities"]][[city]][["color"]]
  city_cubes <- board[["cities"]][[city]][["cubes"]][[infect_color]]
  if (city_cubes + infect_num > 3){
    outbreak(board, city)
  } else { # if city_cubes >= 3
    
    # Is it possible to infect?
    cubes_left <- board[["cubes_left"]][[infect_color]]
    cubes_placed <- min(infect_num, 3 - city_cubes)
    if (cubes_left < infect_num) {
      message(sprintf("GAME OVER: Not enough %s cubes to infect %s", infect_color, city))
      return(board)
    }
    else board[["cubes_left"]][[infect_color]] <- cubes_left - infect_num
    
    # Infect city according to outbreak algorithm
    message(sprintf("Infecting %s", city))
    board[["cities"]][[city]][["cubes"]][[infect_color]] <- infect_num
    
    return(board)
  } # end else if city_cubes >= 3

}

outbreak <- function(board, city){
  
  message(sprintf("OUTBREAK in %s", city))
  board[["outbreak_count"]] <- board[["outbreak_count"]] + 1
  if (board[["outbreak_count"]] > 7) {
    message("GAME OVER: Outbreak limit exceeded.")
    return(board)
  }
  else{
    outbreak_cities <- board[["cities"]][[city]][["connections"]]
    for (oc in outbreak_cities) board <- infect_city(board, oc, 1)
    return(board)
  }
}

draw_infection_card <- function(board, infect_num = 1, location = 1){
  
  infection_card <- board[["infection_deck"]][[location]]
  board[["infection_deck"]] <- board[["infection_deck"]][-location]
  board[["infection_discard"]] <- c(infection_card, board[["infection_discard"]])
  board <- infect_city(board, infection_card, infect_num)
  board[["already_infected"]] <- vector("character", 0)
  return(board)
  
  
}

draw_player_card <- function(board){
  
  player_card <- board[["player_card_deck"]][[1]]
  board[["player_card_deck"]] <- board[["player_card_deck"]][-1]
  board[["player_card_discard"]] <- c(player_card, board[["player_card_discard"]])
  if (player_card != "EPIDEMIC") return(board)
  else {
    message("EPIDEMIC!!!")
    board[["epidemic_count"]] <- board[["epidemic_count"]] + 1
    board[["infection_rate"]] <- board[["infection_rate_scale"]][board[["epidemic_count"]]]
    draw_infection_card(board, 3, length(board[["infection_deck"]])) 
    shuffle_infection_cards <- sample(board[["infection_discard"]], length(board[["infection_discard"]]))
    board[["infection_discard"]] <- vector("character", 0)
    board[["infection_deck"]] <- c(shuffle_infection_cards, board[["infection_deck"]])
    return(board)
  }
  
  
}

take_turn <- function(board){
  
  board <- draw_player_card(board)
  board <- draw_player_card(board)
  infect_draws <- board[["infection_rate"]]
  for (i in seq(infect_draws)) {
    board <- draw_infection_card(board, 1, 1)
  }
  if (length(board[["player_card_deck"]]) < 1) message("GAME OVER: No player cards to draw.")
  board[["turn"]] <- board[["turn"]] + 1
  return(board)
  
}

initial_infect <- function(board) {
  board <- draw_infection_card(board, 3, 1)
  board <- draw_infection_card(board, 3, 1)
  board <- draw_infection_card(board, 3, 1)
  board <- draw_infection_card(board, 2, 1)
  board <- draw_infection_card(board, 2, 1)
  board <- draw_infection_card(board, 2, 1)
  board <- draw_infection_card(board, 1, 1)
  board <- draw_infection_card(board, 1, 1)
  board <- draw_infection_card(board, 1, 1)
  return(board)
}

# Set up initial infection
board <- game_setup(epidemic_cards = 6)
board <- initial_infect(board)

board <- take_turn(board)

infections <- do.call(rbind, lapply(board$cities, function(x) data.frame(x[['cubes']])))
apply(infections, 2, sum)
board$cubes_left

connection_df <- do.call(rbind, lapply(board[["cities"]], function(x){
  data.frame(city = x[["city"]], color = x[["color"]], connections = length(x[["connections"]]),
             stringsAsFactors = FALSE)
}))

tapply(connection_df$connections, connection_df$color, sum)

# Based on board state, how do we model Pr(winning)?
# Start by modeling probabilities of certain events, like outbreaks or epidemics.

# Probability of epidemics:
# The probability of drawing a card is 
#            (# player cards drawn since last possible epi)  /
#            (# of player cards per epidemic card)
#
# Epidemic milestones, or places where epidemics can be drawn can be calculated at board set up
# In fact, they are as part of the shuffling process.
#   Number of player cards after dealing 
