
#' Calculates Table 5.5 Deaths by place of occurrence and site of occurrence
#'
#' @param data data frame being used
#' @param date_var occurrence data variable being used
#' @param data_year year the data is for
#' @param tablename name for csv output use _ instead of . for names
#'
#' @return data frame with tabulated results
#' @export
#'
#' @import dplyr
#' @import tidyr
#' @import janitor
#'
#' @examples t5.5 <- create_t5_5(data = dth_data, date_var = dodyr, data_year = 2022, tablename = NA)
#'
create_t5_5 <- function(data, date_var = dodyr, data_year = 2022, tablename = NA){
  output <- data |>
    filter({{date_var}} == data_year) |>
    group_by(ruindpod, pod) |>
    summarise(total = n()) |>
    pivot_wider(names_from = pod, values_from = total, values_fill = 0) |>
    adorn_totals(c("col")) |>
    mutate(ruindpod = case_when(
      ruindpod == "Total" ~ "All deaths",
      TRUE ~ ruindpod)) |>
    rename(`Place of occurrence` = ruindpod, `Total number of deaths` = Total)

  outputb <- data |>
    filter({{date_var}} == data_year) |>
    group_by(rgnpod, pod) |>
    summarise(Total = n()) |>
    pivot_wider(names_from = pod, values_from = Total, values_fill = 0) |>
    adorn_totals(c("col", "row")) |>
    mutate(rgnpod = ifelse(is.na(rgnpod), "Not Stated", rgnpod)) |>
    rename(`Place of occurrence` = rgnpod, `Total number of deaths` = Total)

  output <- rbind(output, outputb)

  write.csv(output, paste0("./outputs/", tablename, ".csv"), row.names = FALSE)
  return(output)
}
