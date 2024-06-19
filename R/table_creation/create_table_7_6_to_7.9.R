#' Creates Tables 7.6 to 7.9
#'
#' @param data data frame being used
#' @param data_year year the data is for
#' @param group_var the tables columns
#' @param by_var tables rows
#' @param tablename name for csv output use _ instead of . for names
#'
#' @return data frame with tabulated results
#' @export
#'
#' @import dplyr
#' @import tidyr
#' @import janitor
#'
#' @examples t7.6 <- create_t7.6_to_7.9(div_data, data_year = 2019, group_var = "age_w", by_var = "age_h", tablename = "Table_7_6")
#' t7.7 <- create_t7.6_to_7.9(div_data, data_year = 2019, group_var = "age_h", by_var = "dur_grp", tablename = "Table_7_7")
#' t7.8 <- create_t7.6_to_7.9(div_data, data_year = 2019, group_var = "age_w", by_var = "dur_grp", tablename = "Table_7_8")
#' t7.9 <- create_t7.6_to_7.9(div_data, data_year = 2019, group_var = "child", by_var = "dur_grp", tablename = "Table_7_9")
#' 
#' 
create_t7.6_to_7.9 <- function(data, data_year = 2019, group_var = "age_w", by_var = "age_h", tablename = NA){
dur_order <- c("<1", "1", "2", "3", "4", "5", "6", "7", "8", "9",
               "10-14", "15-19", "20-24", "25-29", "30+", "Total")
output <- data |>
  filter(data_year == 2019) |>
  group_by(!!sym(group_var), !!sym(by_var)) |>
  summarise(total = n()) |>
  pivot_wider(names_from = !!sym(group_var), values_from = total, values_fill = 0) |>
  adorn_totals(c("row", "col"))

if(by_var == "dur_grp"){
  output <- output %>%
    mutate(dur_grp = factor(dur_grp, levels = dur_order)) %>%
    arrange(dur_grp)
}

write.csv(output, paste0("./outputs/", tablename, ".csv"), row.names = FALSE)
return(output)
}


