# ---------------------------
# pandemic_sandbox.R
# test functions and scenarios
#
# ---------------------------

cities <- c("algiers", "atlanta", "baghdad", "bangkok", "beijing", "bogota", "buenos_aires", "cairo",    
            "chennai", "chicago", "delhi", "essen", "ho_chi_minh_city", "hong_kong", "istanbul", "jakarta", "johannesburg",
            "karachi", "khartoum", "kinshasa", "kolkata", "lagos", "lima", "london", "los_angeles", "madrid", "manila",
            "mexico_city", "miami", "milan", "montreal", "moscow", "mumbai", "new_york", "osaka", "paris", "riyadh", "san_francisco",
            "santiago", "sao_paulo", "seoul", "shanghai", "st_petersberg", "sydney", "taipei", "tehran", "tokyo", "washington")   

### Test board set up
# shuffle infection cards
setClass("card", representation(name = "character",
                                removed = "logical"))


infection_cards <- unname(sapply(ls(envir = city_env), function(x) eval(parse(text = x), envir = city_env)@name))

# Shuffle infection deck
infection_deck <- sample(infection_cards, length(infection_cards))

cubes_left <- function(color, initial_n = 24) {
  cube_slot <- paste0("cube_count_", color)
  cubes_used <- sum(unname(sapply(ls(envir = city_env), function(x) slot(eval(parse(text = x), envir = city_env), 
                                                                         cube_slot))))
  initial_n - cubes_used
}

infect_cities <- function(city) {
  city_obj <- eval(parse(text = city), envir = globalenv())
  infect_color <- slot(city_obj, "color")
  total_cubes <- cubes_left(infect_color)
  if (total_cubes < 1) stop(sprintf("Game over: can't place more %s disease cubes.", infect_color))
  cube_count <- slot(city_obj, paste0("cube_count_", infect_color))
  if (cube_count < 3) {
    slot(city_obj, paste0("cube_count_", infect_color)) <- cube_count + 1
    return()
  }
  
}


list(
  yellow = list(cubes_left = 24,
                cured = FALSE,
                eradicated = FALSE,
                cities = list()),
  red = list(cubes_left = 24,
             cured = FALSE,
             eradicated = FALSE,
             cities = list()),
  black = list(cubes_left = 24,
               cured = FALSE,
               eradicated = FALSE,
               cities = list()),
  blue = list(cubes_left = 24,
              cured = FALSE,
              eradicated = FALSE,
              cities = list())
)

lapply(ls(envir = city_env), function(x){
  obj <- eval(parse(text = x), envir = city_env)
  list(color = obj@color, city = obj@name, connections = obj@connections, population = obj@population)
})


cube_start <- 24
player_card_start <- 59
infection_rate_start <- 2
outbreak_start <- 1
setClass("board", 
         representation(cubes_remain_black = "numeric",
                                   cubes_remain_blue = "numeric",
                                   cubes_remain_yellow = "numeric",
                                   cubes_remain_red = "numeric",
                                   player_cards_remain = "numeric",
                                   infection_rate = "numeric",
                                   outbreak_counter = "numeric"),
         prototype(cubes_remain_black = cube_start,
                                        cubes_remain_blue = cube_start,
                                        cubes_remain_yellow = cube_start,
                                        cubes_remain_red = cube_start,
                                        player_cards_remain = player_card_start,
                                        infection_rate = infection_rate_start,
                                        outbreak_counter = outbreak_start)
         )







