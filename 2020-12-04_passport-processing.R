# Part One -----
# Process passports (cid missing ok)

library(dplyr)

input <- readLines("data/2020-12-04-input.txt")

breaks <- input == ""

x <- length(input)

passports <- character(0)
collector <- character(0)

for (i in 1:(x + 1)) {

  if (breaks[i] | i == x + 1) {

    passport <- paste0(collector, collapse = " ")
    passport <- stringr::str_replace_all(passport, " ", ",")

    passports <- c(passports, passport)

    collector <- character(0)

  } else {

    collector <- c(collector, input[i])

  }
}

passport_tbl <- tibble::tibble(passport_read = passports) %>%
  tibble::rowid_to_column() %>%
  tidyr::separate_rows(passport_read, sep = ",") %>%
  tidyr::separate(passport_read, into = c("var", "value"), sep = ":") %>%
  tidyr::pivot_wider(names_from = var, values_from = value) %>%
  dplyr::mutate(missing_cols = rowSums(is.na(select(., -rowid, -cid))),
                valid = missing_cols == 0)

sum(passport_tbl$valid)


# Part Two -----
# Validate passport data

passport_tbl2 <- passport_tbl %>%
  dplyr::mutate(
    across(c(byr, iyr, eyr), as.numeric),
    byr = if_else(nchar(byr) == 4, byr >= 1920 & byr <= 2002, TRUE, FALSE),
    iyr = if_else(nchar(iyr) == 4, iyr >= 2010 & iyr <= 2020, TRUE, FALSE),
    eyr = if_else(nchar(eyr) == 4, eyr >= 2020 & eyr <= 2030, TRUE, FALSE),
    hgt_val = as.numeric(stringr::str_remove_all(hgt, "\\D")),
    hgt = case_when(
      stringr::str_detect(hgt, "cm") & hgt_val >= 150 & hgt_val <= 193 ~ TRUE,
      stringr::str_detect(hgt, "in") & hgt_val >= 59 & hgt_val <= 76 ~ TRUE,
      TRUE ~ FALSE
    ),
    hcl = stringr::str_detect(hcl, "^#[0-9a-f]{6}$"),
    ecl = ecl %in% c("amb", "blu", "brn", "gry", "grn", "hzl", "oth"),
    pid = (stringr::str_detect(pid, "^[0-9]{9}$"))
    ) %>%
  dplyr::mutate(
    valid_cols = rowSums(
        select(., -rowid, -cid, -missing_cols, -valid, -hgt_val)
      ),
    verified = case_when(
      valid_cols == 7 ~ TRUE,
      TRUE ~ FALSE
    )
  )

sum(passport_tbl2$verified, na.rm = TRUE)
