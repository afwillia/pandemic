# ---------------------------
# pandemic_sandbox.R
# test functions and scenarios
#
# ---------------------------

source("cities.R")
city_names <- ls(envir = city_env)
# Program game constants
game_setup <- function(cube_start = 24,
                       player_card_start = 59,
                       infection_rate_start = 2,
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
       already_infected = c(),
       infection_deck = c(),
       infection_discard = c()
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
    if (city_cubes >= 3){
      outbreak(board, city)
    } else { # if city_cubes >= 3
      
      # Is it possible to infect?
      cubes_left <- board[["cubes_left"]][[infect_color]]
      if (cubes_left < infect_num) {
        message(sprintf("GAME OVER: Not enough %s cubes to infect %s", infect_color, city))
        return(board)
      }
      else board[["cubes_left"]][[infect_color]] <- cubes_left - infect_num
      
      # Infect city according to outbreak algorithm
      board[["cities"]][[city]][["cubes"]][[infect_color]] <- infect_num
      
      return(board)
    } # end else if city_cubes >= 3

}

outbreak <- function(board, city){
  
  message(sprintf("OUTBREAK in %s", city))
  board[["outbreak_count"]] <- board[["outbreak_count"]] + 1
  if (board[["outbreak_count"]] > 7) {
    warning("GAME OVER: Outbreak limit exceeded.")
    return(board)
  }
  else{
    outbreak_cities <- board[["cities"]][[city]][["connections"]]
    board <- infect_city(board, oc, 1)
    return(board)
  }
}

draw_infection_card <- function(board, infect_num){
  
  infection_card <- board[["infection_deck"]][[1]]
  board[["infection_deck"]] <- board[["infection_deck"]][-1]
  board[["infection_discard"]] <- c(infection_card, board[["infection_discard"]])
  board <- infect_city(board, infection_card, infect_num)
  board[["already_infected"]] <- vector("character", 0)
  return(board)
  
  
}



# Set up initial infection
board <- game_setup()

board <- draw_infection_card(board, 3)
board <- draw_infection_card(board, 3)
board <- draw_infection_card(board, 3)
board <- draw_infection_card(board, 2)
board <- draw_infection_card(board, 2)
board <- draw_infection_card(board, 2)
board <- draw_infection_card(board, 1)
board <- draw_infection_card(board, 1)
board <- draw_infection_card(board, 1)

infections <- do.call(rbind, lapply(board$cities, function(x) data.frame(x[['cubes']])))
apply(infections, 2, sum)
board$cubes_left

connection_df <- do.call(rbind, lapply(board[["cities"]], function(x){
  data.frame(city = x[["city"]], color = x[["color"]], connections = length(x[["connections"]]),
             stringsAsFactors = FALSE)
}))

tapply(connection_df$connections, connection_df$color, sum)



