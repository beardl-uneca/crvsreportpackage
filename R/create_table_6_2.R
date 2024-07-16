#' Create Table 6.2
#'
#' Table 6.2 Top 10 causes of death (all ages, both sexes)
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
#' @examples
#' t6.2 <- create_t6.2(dth_data, dodyr)
create_t6.2 <- function(data, date_var){
  # date_var <- enquo(date_var)
  # date_var_name <- quo_name(date_var)
  latest_year <- data %>% pull(!!sym(date_var)) %>% max(na.rm = TRUE)

output <- data |>
  filter(!substr(fic10und,1,1) %in% c("", "U") & sex %in% c("male", "female") & !!sym(date_var) %in% generate_year_sequence(latest_year)) |>
  group_by(!!sym(date_var), substr(fic10und,1,3)) |>
  summarise(total = n()) |>
  arrange(!!sym(date_var)) |>
  rename(causecd = `substr(fic10und, 1, 3)`)

output<- left_join(output, cause, by = c("causecd" = "code")) |>
  group_by(!!sym(date_var), description)|>
  summarise(total = sum(total)) |>
  group_by(!!sym(date_var)) |>
  slice_max(order_by = total, n = 10, with_ties = FALSE) |>
  mutate(rank = rank(-total)) |>
  pivot_wider(id_cols = rank, names_from = date_var_name, values_from = description)

return(output)
}

