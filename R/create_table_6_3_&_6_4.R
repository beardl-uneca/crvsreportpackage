#' Creates Tables 6.3 or 6.4
#'
#' @description
#' Table 6.3 Ten leading causes of death, males
#' @description
#' Table 6.4 Ten leading causes of death, females
#'
#' @param data data frame being used
#' @param data_year year the data is for
#' @param sex_value whether male or female
#'
#' @return dara frame with tabulated results
#' @export
#'
#' @import dplyr
#' @import tidyr
#' @import janitor
#'
#' @examples
#' t6.3 <- create_t6.3_t6.4(dth_data, sex_value = "male")
#'
create_t6.3_t6.4 <- function(data, data_year = NA, sex_value = "male"){

  # if data_year is not provided, take the latest year in the data
  if (is.na(data_year)){
    data_year = data %>% pull(!!sym(date_var)) %>% max(na.rm = TRUE)
  }

output <- data |>
  filter(doryr == data_year & sex == sex_value) |>
  group_by(substr(fic10und,1,3)) |>
  summarise(total = n()) |>
  rename(causecd = `substr(fic10und, 1, 3)`)

output <- left_join(output, cause, by = c("causecd" = "code")) |>
  group_by(group, description) |>
  summarise(total = sum(total)) |>
  arrange(desc(total))

  r99_dths <- output |>
    filter(group %in% c("R00:R99"))
  na_dths <- output |>
    filter(substr(group,1,1) %in% c( NA))

  output <- output |>
    filter(!substr(group,1,1) %in% c("R", NA))

  output2 <- output |>
    tail(nrow(output)-10)

  output2 <- rbind(output2, na_dths) |>
    mutate(group = "-",
           description = "All other causes")|>
    group_by(group, description) |>
    summarise(total = sum(total))
rm(na_dths)

output <- head(output, 10)

output <- rbind(output, r99_dths, output2)
total_deaths <- sum(output$total)
output <- output |>
  adorn_totals("row")  |>
  mutate(proportion = round_excel(total/sum(total_deaths)*100,2))

return(output)

}

