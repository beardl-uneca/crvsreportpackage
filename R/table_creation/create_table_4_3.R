#' Creates Table 4.3
#'
#' @param data data frame being used
#' @param date_var occurrence data being used e.g. dobyr, dodyr etc
#' @param data_year year the data is for
#' @param tablename name for csv output use _ instead of . for names
#'
#' @return data frame with tablutated results
#' @export
#'
#' @import dplyr
#' @import tidyr
#' @import janitor
#'
#' @examples t4.3 <- create_t4.3(bth_data, dobyr, 2022)

create_t4.3 <- function(data, date_var, data_year = 2022, tablename = "Table_4_3"){
  output <- data |>
    filter(is.na(sbind) & {{date_var}} == data_year) |>
    group_by(rgnpob, usual_res_plocc) |>
    summarise(total = n()) |>
    pivot_wider(names_from = usual_res_plocc, values_from = total, values_fill = 0) |>
    adorn_totals(c("col", "row"))

  write.csv(output, paste0("./outputs/", tablename, ".csv"), row.names = FALSE)
  return(output)
}



