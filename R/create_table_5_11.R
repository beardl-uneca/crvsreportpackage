#' creates Table 5.11
#'
#' @param data data frame being used
#' @param date_var occurrence data variable being used
#' @param tablename name for csv output use _ instead of . for names
#'
#' @return data frame with tabulated result
#' @export
#' 
#' @import dplyr
#' @import tidyr
#' @import janitor
#' 
#' @examples t5.11 <- create_t5.11(bth_data, date_var = dobyr, tablename = "Table_5_11")
#' 
create_t5.11 <- function(data, date_var, tablename = NA){
  output <- data |>
    filter(!is.na(sbind) & {{date_var}} %in% 
             ((max({{date_var}}, na.rm = TRUE)-5): (max({{date_var}}, na.rm = TRUE)-1))) |>
    group_by(sex, {{date_var}}) |>
    summarise(total = n()) |>
    pivot_wider(names_from = sex, values_from = total, values_fill = 0) |>
    adorn_totals("col")
    
  write.csv(output, paste0("./outputs/", tablename, ".csv"), row.names = FALSE)   
  return(output)
}

