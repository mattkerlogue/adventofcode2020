# Part One -----
# Highest seat ID

input <- readLines("data/2020-12-05_input.txt")

seat_id <- function(x) {

  rx <- gsub("L|F", 0, x)
  rx <- gsub("R|B", 1, rx)

  row <- strtoi(substr(rx, 1, 7), base = "2")
  col <- strtoi(substr(rx, 8, 10), base = "2")

  id <- (row * 8) + col

  return(id)

}

seats_boarded <- seat_id(input)

max(seats_boarded)

# Part Two -----
# Find your seat

id_range <- range(seats_boarded)
all_seats <- seq(id_range[1], id_range[2])

my_position <- which(!(all_seats %in% seats_boarded))
all_seats[my_position]
