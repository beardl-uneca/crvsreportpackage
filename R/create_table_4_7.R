#' Create Table 4.7
#'
#' Table 4.7 Live births by place of occurrence, site of delivery and attendant at birth
#'
#' @param data data frame being used
#' @param date_var occurrence data variable being used
#' @param data_year year of data
#' @param tablename name of the table being saved as a csv file
#'
#' @return data frames for tabulated versions of Table 4.7
#' @export
#'
#' @import dplyr
#' @import tidyr
#' @import janitor
#' @examples t4.7 <- create_table_4.7(bth_data, dobyr, 2022, tablename = "Table_4_7")
#'
create_t4.7 <- function(data, date_var, data_year = NA, tablename = NA){

  # if data_year is not provided, take the latest year in the data
  if (is.na(data_year)){
    data_year = data %>% pull(!!sym(date_var)) %>% max(na.rm = TRUE)
  }

  output <- data |>
    filter(!!sym(date_var) == data_year & is.na(sbind)) |>
    group_by(rgnpob, pob, attend) |>
    summarise(total = n()) |>
    pivot_wider(names_from = attend, values_from = total, values_fill = 0) |>
    adorn_totals("col")

  outputall <- output |>
    group_by(pob) |>
    summarise(`1` = sum(`1`), `2` = sum(`2`), `3` = sum(`3`), `4` = sum(`4`), `5` = sum(`5`)) |>
    mutate(rgnpob = "all") |>
    select(rgnpob, pob, `1`:`5`) |>
    adorn_totals(c("row", "col"))

  outputrgn <- output |>
    group_by(rgnpob) |>
    summarise(`1` = sum(`1`), `2` = sum(`2`), `3` = sum(`3`), `4` = sum(`4`), `5` = sum(`5`)) |>
    mutate(pob = "total") |>
    select(rgnpob, pob, `1`:`5`) |>
    adorn_totals("col")

  output <- rbind(output, outputrgn) |>
    arrange(rgnpob)

  output <- rbind(outputall, output)


  write.csv(output, paste0("./outputs/", tablename, ".csv"), row.names = FALSE)
  return(output)
}
