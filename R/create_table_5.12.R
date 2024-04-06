#' Create Table 5.12
#'
#' @param data data frame being used
#' @param date_var occurrence data being used e.g. dobyr, dodyr etc
#' @param data_year 
#' @param tablename name for csv output use _ instead of . for names
#'
#' @return data frame with tablutated results
#' @export
#'
#' @import dplyr
#' @import tidyr
#' @import janitor
#'
#' @examples t5.12 <- create_t5.12(bth_data, date_var = dobyr, data_year = 2022, tablename = "Table_5_12")
#' 
create_t5.12 <- function(data, date_var, data_year = 2022, tablename = NA){
output<- data |>
  filter(!is.na(sbind) & {{date_var}} == 2022) |>
  group_by(gest_grp, bthwgt_grp) |>
  summarise(total = n()) |>
  pivot_wider(names_from = bthwgt_grp, values_from = total, values_fill = 0)

write.csv(output, paste0("./outputs/", tablename, ".csv"), row.names = FALSE)
return(output)
}



