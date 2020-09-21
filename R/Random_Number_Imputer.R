# Random Number Imputer
# Function that imputes a random number below the detection limit.

Random_Number_Imputer <- function(data,
                                  columns,
                                  split_symbol = "<",
                                  digits = 6) {
    require(purrr)
    require(stringr)
    require(tibble)
    require(dplyr)
    require(magrittr)


    laser_data <- data[columns]
    laser_split <- tibble::tibble(1:nrow(laser_data),
                          nrow(laser_data):1)

    for (i in 1:ncol(laser_data)) {
        for (j in 1:nrow(laser_data)) {
            if (stringr::str_detect(laser_data[j, i], split_symbol)) {
                laser_split <- laser_data[[i]] %>%
                    stringr::str_split(split_symbol, simplify = TRUE)

                for (k in nrow(laser_split):1) {
                    if (laser_split[[k, 1]] == "" |
                        is.na(laser_split[[k, 1]])) {
                        laser_split[[k, 1]] <-
                            round(stats::runif(n = 1, min = 0,
                                        max = as.numeric(laser_split[k, 2])),
                                  digits = digits)
                    }
                }
                laser_data[[i]] <- laser_split[, 1]
            }
        }
    }

    laser_data <- laser_data %>%
        purrr::map(as.character) %>%
        purrr::map(as.numeric)
    data[columns] <- laser_data
    return(data)
}
