#
# cities_test.R
# test cases for cities.R
#

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

validate <- sapply(cities, function(x, y) validate_connections(x, cities))
