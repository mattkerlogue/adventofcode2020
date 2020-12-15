# Part One ---
# How many trees (#) encountered

input <- readLines("data/2020-12-03-input.txt")

field <- strsplit(input, split = character())

height <- length(input)
width <- nchar(input[1])

y <- 1
x <- 1
trees <- 0

while (y < height) {

  y <- y + 1
  x <- x + 3

  if (x > width) {
    x <- x - width
  }

  spot <- field2[[y]][x]

  if (spot == "#") {
    trees <- trees + 1
  }

}

# Part One ---
# Product of tree slopes

runs <- matrix(
  c(1,1,
    3,1,
    5,1,
    7,1,
    1,2),
  ncol = 2,
  byrow = TRUE
)

runs_trees <- numeric()

for (i in 1:nrow(runs)) {

  right <- runs[i, 1]
  down <- runs[i, 2]
  trees <- 0

  y <- 1
  x <- 1

  while (y < height) {

    y <- y + down
    x <- x + right

    if (y > height) {
      break
    }

    if (x > width) {
      x <- x - width
    }

    spot <- field[[y]][x]

    if (spot == "#") {
      trees <- trees + 1
    }

  }

  runs_trees <- c(runs_trees, trees)

}

prod(runs_trees)
