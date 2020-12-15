# Part One -----
# Password must contain given occurrences of a letter

library(dplyr)

input <- readLines("data/2020-12-02-input.txt")

tbl <- tibble::tibble(source = input) %>%
  tidyr::separate(source,
                  into = c("min_count", "max_count", "letter", "password"),
                  remove = FALSE) %>%
  dplyr::mutate(
    ltr_count = purrr::map2_dbl(password, letter, stringr::str_count),
    across(c(min_count, max_count), as.numeric),
    match = ltr_count >= min_count & ltr_count <= max_count
  )

sum(tbl$match)

# Part Two -----
# Password must contain single instance of the letter at either first or
# second position

tbl2 <- tbl %>%
  dplyr::mutate(
    first = purrr::map2_chr(password,
                            min_count,
                            ~stringr::str_sub(.x,
                                              start = .y,
                                              end = .y)),
    second = purrr::map2_chr(password,
                           max_count,
                           ~stringr::str_sub(.x,
                                             start = .y,
                                             end = .y)),
    match2 = xor(first == letter, second == letter)
  )

sum(tbl2$match2)


