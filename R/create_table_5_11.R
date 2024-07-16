#' creates Table 5.11
#'
#'Table 5.11 Foetal deaths by year of occurrence and sex of foetus
#'
#' @param data data frame being used
#' @param date_var occurrence data variable being used
#' @param num_yrs number of years the of data
#' @param tablename name for csv output use _ instead of . for names
#'
#' @return data frame with tabulated result
#' @export
#'
#' @import dplyr
#' @import tidyr
#' @import janitor
#'
#' @examples
#' t5.11 <- create_t5.11(bth_data, date_var = dobyr, tablename = "Table_5_11")
#'
create_t5.11 <- function(data, date_var, num_yrs = 5, tablename = NA){
  latest_year <- data %>% pull(!!sym(date_var)) %>% max(na.rm = TRUE)
  years <- generate_year_sequence(latest_year = latest_year, num_yrs = num_yrs)

  output <- data |>
    filter(!is.na(sbind) & !!sym(date_var) %in% years) |>
    group_by(sex, !!sym(date_var)) |>
    summarise(total = n()) |>
    pivot_wider(names_from = sex, values_from = total, values_fill = 0) |>
    adorn_totals("col")

  write.csv(output, paste0("./outputs/", tablename, ".csv"), row.names = FALSE)
  return(output)
}

