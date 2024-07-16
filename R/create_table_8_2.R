#' Creates Table 8.2
#'
#' Table 8.2 Live births, deaths, and infant and child deaths by place of usual residence of mother (births) or decedent (deaths)
#'
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
#' t8.2 <- create_t8.2(tablename = "Table _8_2")
#'
create_t8.2 <- function(data_year = NA, tablename = NA){

  # if data_year is not provided, take the latest year in the data
  if (is.na(data_year)){
    data_year = data %>% pull(!!sym(date_var)) %>% max(na.rm = TRUE)
  }

  outputb <- bth_data |>
    filter(is.na(sbind) & dobyr == data_year) |>
    group_by(rgn, sex) |>
    summarise(total = n()) |>
    pivot_wider(names_from = sex, values_from = total) |>
    adorn_totals("col")
  colnames(outputb) <- c("Region_of_Occurrence", "Live_Births_Female", "Live_Births_Male", "NS", "Live_Biths_Total")

  outputd <- dth_data |>
    filter(dodyr == data_year) |>
    group_by(rgn, sex) |>
    summarise(total = n()) |>
    pivot_wider(names_from = sex, values_from = total) |>
    adorn_totals("col")
  colnames(outputd) <- c("Region_of_Occurrenced", "Deaths_Female", "Deaths_Male", "NS",  "Deaths_Total")

  outputi <- dth_data |>
    filter(ageinyrs < 5 & dodyr == data_year) |>
    group_by(rgn, sex) |>
    summarise(total = n()) |>
    pivot_wider(names_from = sex, values_from = total) |>
    adorn_totals("col")
  colnames(outputi) <- c("Region_of_Occurrencei", "Under5_Deaths_Female", "Under5_Deaths_Male", "NS",  "Under5_Deaths_Total")

  output <- cbind(outputb, outputd, outputi) |>
    select(Region_of_Occurrence, Live_Births_Female, Live_Births_Male, Live_Biths_Total, Deaths_Female, Deaths_Male, Deaths_Total, Under5_Deaths_Female, Under5_Deaths_Male, Under5_Deaths_Total)


  write.csv(output, paste0("./outputs/", tablename, ".csv"), row.names = FALSE)
  return(output)
}
