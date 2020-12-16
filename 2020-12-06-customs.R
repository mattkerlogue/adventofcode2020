# Part One -----
# How questions answered

library(dplyr)

input <- readLines("data/2020-12-06-input.txt")

breaks <- input == ""

x <- length(input)

group_counts <- numeric(0)
collector <- character(0)

for (i in 1:(x + 1)) {

  if (breaks[i] | i == x + 1) {

    group <- paste0(collector, collapse = "")
    group_num <- length(unique(charToRaw(group)))
    group_counts <- c(group_counts, group_num)

    collector <- character(0)

  } else {

    collector <- c(collector, input[i])

  }
}

sum(group_counts)

# Part Two -----
# All questions answered


groups <- character(0)
collector <- character(0)

for (i in 1:(x + 1)) {

  if (breaks[i] | i == x + 1) {

    group <- paste0(collector, collapse = ",")
    groups <- c(groups, group)

    collector <- character(0)

  } else {

    collector <- c(collector, input[i])

  }
}

ltr_cols <- tibble::tibble(col = letters, value = NA) %>%
  tidyr::pivot_wider(names_from = col)

group_tbl <- tibble::tibble(answers = groups) %>%
  tibble::rowid_to_column(var = "group") %>%
  tidyr::separate_rows(answers, sep = ",") %>%
  dplyr::add_count(group, name = "group_n") %>%
  dplyr::bind_cols(ltr_cols) %>%
  tidyr::pivot_longer(cols = c(-group, -answers, -group_n),
                      names_to = "question") %>%
  dplyr::mutate(
    value = purrr::map2_lgl(
      answers,
      question,
      ~stringr::str_detect(.x, .y))) %>%
  dplyr::group_by(group, question) %>%
  dplyr::summarise(
    group_n = mean(group_n),
    count = sum(value)
    ) %>%
  dplyr::ungroup() %>%
  dplyr::filter(count == group_n) %>%
  dplyr::count(group)

sum(group_tbl$n)
