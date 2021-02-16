
library(tidyverse)

btex_parameters <- tibble(enclosure = rep(str_c("M", c(1,2,3,5,6,7,8), sep = ""), 4),
                          compound = rep(c("Benzene", "Toluene", "Ethylbenzene", "Xylene"), each = 7),
                          kot = c(rnorm(7, 30, 2),
                                  rnorm(7, 25, 2),
                                  rnorm(7, 20, 2),
                                  rnorm(7, 15, 2)),
                          kow	= c(rnorm(7, 0.2, 0.03),
                                  rnorm(7, 0.25, 0.03),
                                  rnorm(7, 0.23, 0.03),
                                  rnorm(7, 0.24, 0.03)),
                          kwt = c(rnorm(7, 0.2, 0.03),
                                  rnorm(7, 0.25, 0.03),
                                  rnorm(7, 0.23, 0.03),
                                  rnorm(7, 0.24, 0.03)))

write_csv(btex_parameters, "data_raw/btex_parameters.csv")
