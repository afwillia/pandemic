# ----------------------------------------------------------
# cities.R
#
# Define class for Pandemic cities
#
# ----------------------------------------------------------

# Define city class
setClass("city", representation(name = "character",
                                color = "character",
                                connections = "character",
                                drawn = "logical",
                                removed = "logical",
                                research_center = "logical",
                                cube_count_black = "numeric",
                                cube_count_blue = "numeric",
                                cube_count_red = "numeric",
                                cube_count_yellow = "numeric",
                                population = "numeric",
                                lat = "numeric",
                                long = "numeric"),
         prototype(name = NA_character_,
                   color = NA_character_,
                   connections = NA_character_,
                   drawn = FALSE,
                   removed = FALSE,
                   research_center = FALSE,
                   cube_count_black = 0,
                   cube_count_blue = 0,
                   cube_count_red = 0,
                   cube_count_yellow = 0,
                   population = NA_integer_,
                   lat = NA_real_,
                   long = NA_real_)
         )

# Define cities in a separate environment
city_env <- new.env()
assign("algiers" , new("city", name = "algiers", color = "black", connections = c("madrid", "paris", "istanbul", "cairo")), envir = city_env)
assign("atlanta" , new("city", name = "atlanta", color = "blue", connections = c("chicago", "washington", "miami")), envir = city_env)
assign("baghdad" , new("city", name = "baghdad", color = "black", connections = c("istanbul", "cairo", "riyadh", "karachi", "tehran")), envir = city_env)
assign("bangkok" , new("city", name = "bangkok", color = "red", connections = c("kolkata", "chennai", "jakarta", "ho_chi_minh_city", "hong_kong")), envir = city_env)
assign("beijing" , new("city", name = "beijing", color = "red", connections = c("shanghai", "seoul")), envir = city_env)
assign("buenos_aires" , new("city", name = "buenos_aires", color = "yellow", connections = c("bogota", "sao_paulo")), envir = city_env)
assign("bogota" , new("city", name = "bogota", color = "yellow", connections = c("miami", "mexico_city", "lima", "buenos_aires", "sao_paulo")), envir = city_env)
assign("cairo" , new("city", name = "cairo", color = "black", connections = c("istanbul", "algiers", "khartoum", "riyadh", "karachi", "tehran")), envir = city_env)
assign("chennai" , new("city", name = "chennai", color = "black", connections = c("delhi", "mumbai", "kolkata", "bangkok", "jakarta")), envir = city_env)
assign("chicago" , new("city", name = "chicago", color = "blue", connections = c("san_francisco", "los_angeles", "mexico_city", "atlanta", "montreal")), envir = city_env)
assign("delhi" , new("city", name = "delhi", color = "black", connections = c("tehran", "karachi", "mumbai", "chennai", "kolkata")), envir = city_env)
assign("essen" , new("city", name = "essen", color = "blue", connections = c("london", "paris", "milan", "st_petersberg")), envir = city_env)
assign("ho_chi_minh_city" , new("city", name = "ho_chi_minh_city", color = "red", connections = c("hong_kong", "bangkok", "jakarta", "manila")), envir = city_env)
assign("hong_kong" , new("city", name = "hong_kong", color = "red", connections = c("shanghai", "kolkata", "bangkok", "ho_chi_minh_city", "manila", "taipei")), envir = city_env)
assign("istanbul" , new("city", name = "istanbul", color = "black", connections = c("st_petersberg", "milan", "algiers", "cairo", "baghdad", "moscow")), envir = city_env)
assign("jakarta" , new("city", name = "jakarta", color = "red", connections = c("bangkok", "chennai", "sydney", "ho_chi_minh_city")), envir = city_env)
assign("johannesburg" , new("city", name = "johannesburg", color = "yellow", connections = c("kinshasa", "khartoum")), envir = city_env)
assign("karachi" , new("city", name = "karachi", color = "black", connections = c("tehran", "baghdad", "riyadh", "mumbai", "delhi")), envir = city_env)
assign("khartoum" , new("city", name = "khartoum", color = "yellow", connections = c("cairo", "lagos", "kinshasa", "johannesburg")), envir = city_env)
assign("kinshasa" , new("city", name = "kinshasa", color = "black", connections = c("lagos", "johannesburg", "khartoum")), envir = city_env)
assign("kolkata" , new("city", name = "kolkata", color = "black", connections = c("delhi", "chennai", "bangkok", "hong_kong")), envir = city_env)
assign("lagos"  , new("city", name = "lagos", color = "yellow", connections = c("sao_paulo", "kinshasa", "khartoum")), envir = city_env)
assign("lima"  , new("city", name = "lima", color = "yellow", connections = c("mexico_city", "bogota", "santiago")), envir = city_env)
assign("london"  , new("city", name = "london", color = "blue", connections = c("new_york", "madrid", "paris", "essen")), envir = city_env)
assign("los_angeles"  , new("city", name = "los_angeles", color = "yellow", connections = c("san_francisco", "sydney", "mexico_city", "chicago")), envir = city_env)
assign("madrid"  , new("city", name = "madrid", color = "blue", connections = c("new_york", "sao_paulo", "algiers", "paris", "london")), envir = city_env)
assign("manila"  , new("city", name = "manila", color = "red", connections = c("taipei", "hong_kong", "ho_chi_minh_city", "sydney", "san_francisco")), envir = city_env)
assign("miami"  , new("city", name = "miami", color = "yellow", connections = c("atlanta", "mexico_city", "bogota", "washington")), envir = city_env)
assign("mexico_city"  , new("city", name = "mexico_city", color = "yellow", connections = c("chicago", "los_angeles", "lima", "bogota", "miami")), envir = city_env)
assign("milan"  , new("city", name = "milan", color = "blue", connections = c("essen", "paris", "istanbul")), envir = city_env)
assign("mumbai"  , new("city", name = "mumbai", color = "black", connections = c("karachi", "riyadh", "delhi", "chennai")), envir = city_env)
assign("montreal"  , new("city", name = "montreal", color = "blue", connections = c("chicago", "washington", "new_york")), envir = city_env)
assign("moscow"  , new("city", name = "moscow", color = "black", connections = c("st_petersberg", "istanbul", "tehran")), envir = city_env)
assign("new_york"  , new("city", name = "new_york", color = "blue", connections = c("montreal", "washington", "madrid", "london")), envir = city_env)
assign("osaka"  , new("city", name = "osaka", color = "red", connections = c("tokyo", "taipei")), envir = city_env)
assign("paris"  , new("city", name = "paris", color = "blue", connections = c("london", "madrid", "algiers", "milan", "essen")), envir = city_env)
assign("riyadh"  , new("city", name = "riyadh", color = "black", connections = c("baghdad", "cairo", "karachi")), envir = city_env)
assign("san_francisco"  , new("city", name = "san_francisco", color = "blue", connections = c("tokyo", "milan", "los_angeles", "chicago")), envir = city_env)
assign("santiago"  , new("city", name = "santiago", color = "yellow", connections = c("lima")), envir = city_env)
assign("sao_paulo"  , new("city", name = "sao_paulo", color = "yellow", connections = c("bogota", "buenos_aires", "lagos", "madrid")), envir = city_env)
assign("seoul"  , new("city", name = "seoul", color = "red", connections = c("beijing", "shanghai", "tokyo")), envir = city_env)
assign("shanghai"  , new("city", name = "shanghai", color = "red", connections = c("beijing", "hong_kong", "taipei", "tokyo", "seoul")), envir = city_env)
assign("st_petersberg"  , new("city", name = "st_petersberg", color = "blue", connections = c("essen", "istanbul", "moscow")), envir = city_env)
assign("sydney"  , new("city", name = "sydney", color = "red", connections = c("manila", "jakarta", "los_angeles")), envir = city_env)
assign("taipei"  , new("city", name = "taipei", color = "red", connections = c("shanghai", "hong_kong", "manila", "osaka")), envir = city_env)
assign("tehran"  , new("city", name = "tehran", color = "black", connections = c("moscow", "baghdad", "karachi", "delhi")), envir = city_env)
assign("tokyo"  , new("city", name = "tokyo", color = "red", connections = c("seoul", "san_francisco", "shanghai", "osaka")), envir = city_env)
assign("washington"  , new("city", name = "washington", color = "blue", connections = c("new_york", "montreal", "atlanta", "miami")), envir = city_env)

# Test if all city connections are real cities
cities <- c("algiers", "atlanta", "baghdad", "bangkok", "beijing", "bogota", "buenos_aires", "cairo",    
"chennai", "chicago", "delhi", "essen", "ho_chi_minh_city", "hong_kong", "istanbul", "jakarta", "johannesburg",
"karachi", "khartoum", "kinshasa", "kolkata", "lagos", "lima", "london", "los_angeles", "madrid", "manila",
 "mexico_city", "miami", "milan", "montreal", "moscow", "mumbai", "new_york", "osaka", "paris", "riyadh", "san_francisco",
 "santiago", "sao_paulo", "seoul", "shanghai", "st_petersberg", "sydney", "taipei", "tehran", "tokyo", "washington")   

validate_connections <- function(city, all_cities) {
  
  # Since city is a string, evaluate it in the global env
  city_ev <- eval(parse(text=city), envir = city_env)
  city_name <- city_ev@name
  connections <- city_ev@connections
  mutual_cons <- sum(unlist(lapply(connections, function(x){
    con_city <- eval(parse(text = x), envir = city_env)
    city_name %in% x
  })))
  
  # 
  bad_cons <- connections[!connections %in% all_cities]
  # Do connecting cities exist?
  if (length(bad_cons) > 0) warning(sprintf("%s connects to invalid cities %s", city, bad_cons))
  # Does city name match object name?
  else if (city_name != city) warning(sprintf("%s object name does not match slot @ name", city))
  # Do cities mutually connect to each other?
  else if (mutual_cons > 0) warning(sprintf("%s does not have a mutual connection", city))
  
  else 0
}

sapply(cities, function(x, y) validate_connections(x, cities))



