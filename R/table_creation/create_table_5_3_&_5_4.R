#' Calculates Tables 5.3 & 5.4
#' Deaths by place of occurrence and place of usual residence of decedent
#'
#' @param data dataframe being used
#' @param sex_filter male or female
#' @param date_var occurrence data being used e.g. dobyr, dodyr etc
#' @param data_year year the data is for
#' @param tablename name of the table being saved as a csv file
#'
#' @return data frame of tabulated results
#' @export
#'
#' @import dplyr
#' @import tidyr
#' @import janitor
#'
#' @examples t5.3 <- create_t5.3_and_t5.4(dth_data, dodyr, 2022, sex_filter = "male", tablename = "Table_5_3")
#'
create_t5.3_and_t5.4 <- function(data, date_var, data_year = 2022, sex_filter = NA, tablename = "Table_5_3"){
  output <- data |>
    filter(sex == sex_filter & {{date_var}} == data_year) |>
    group_by(rgnpod, usual_res_plocc) |>
    summarise(total = n()) |>
    pivot_wider(names_from = usual_res_plocc, values_from = total, values_fill = 0) |>
    adorn_totals(c("col", "row"))

  write.csv(output, paste0("./outputs/", tablename, ".csv"), row.names = FALSE)
  return(output)
}
