# Part One -----
# Find the product of the two numbers that sum to 2020

input <- readLines("data/2020-12-01-input.txt")
input <- as.numeric(input)

x <- length(input)

mat <- matrix(data = numeric(),
              nrow = x,
              ncol = x,
              dimnames = list(input, input))

for (i in 1:x) {
  mat[i,] <- input
  mat[,i] <- mat[,i] + input
}

ind <- which(mat == 2020, arr.ind = TRUE)

a <- as.numeric(rownames(mat)[ind[1]])
b <- as.numeric(colnames(mat)[ind[2]])

(a + b) == 2020

result <- a * b

# Part Two -----
# Find the product of the three numbers that sum to 2020

collector <- list()

for (i in 1:x) {

  nmat <- mat + input[i]

  if (sum(nmat == 2020) > 0) {

    nind <- which(nmat == 2020, arr.ind = TRUE)

    a <- as.numeric(rownames(nmat)[nind[1]])
    b <- as.numeric(colnames(nmat)[nind[2]])
    c <- as.numeric(input[i])

    check <- a + b + c

    if (check == 2020) {
      nlist <- list(list(
        a = a,
        b = b,
        c = c
      ))

      collector <- append(
        collector,
        nlist
      )
    }

  }
}

prod(as.numeric(collector[[1]]))


