#' Create Table 6.2
#'
#' @param data data frame being used
#' @param data_year year of data: dodyr or doryr
#'
#' @return Returns data frame with tabulated results
#' @export
#' @import dplyr
#' @import tidyr
#' @import janitor
#'
#' @examples t6.2 <- create_t6.2(dth_data, dodyr)
create_t6.2 <- function(data, year_var){
  year_var <- enquo(year_var)
  year_var_name <- quo_name(year_var)

output <- data |>
  filter(!substr(fic10und,1,1) %in% c("", "U") & sex %in% c("male", "female") & !!year_var %in% c(2018:max(!!year_var, na.rm = TRUE))) |>
  group_by(!!year_var, substr(fic10und,1,3)) |>
  summarise(total = n()) |>
  arrange(!!year_var) |>
  rename(causecd = `substr(fic10und, 1, 3)`)

output <- left_join(output, cause, by = c("causecd" = "code")) |>
  group_by(!!year_var, description)|>
  summarise(total = sum(total)) |>
  group_by(!!year_var) |>
  slice_max(order_by = total, n = 10, with_ties = FALSE) |>
  mutate(rank = rank(-total)) |>
  pivot_wider(id_cols = rank, names_from = year_var_name, values_from = description)

return(output)
}

