#' Creat Table 8.1
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
#' @examples t8.1 <- create_t8.1(tablename = "Table _8_1")
create_t8.1 <- function(tablename = NA){
  outputb <- bth_data |>
    filter(is.na(sbind) & dobyr %in% c((max(dobyr, na.rm = TRUE)-5):(max(dobyr, na.rm = TRUE)-1))) |>
    group_by(dobyr, sex) |>
    summarise(total = n()) |>
    pivot_wider(names_from = sex, values_from = total) |>
    adorn_totals("col")
  colnames(outputb) <- c("Year_of_Occurrence", "Live_Births_Female", "Live_Births_Male", "NS", "Live_Biths_Total")
  
  outputd <- dth_data |>
    filter(dodyr %in% c((max(dodyr, na.rm = TRUE)-5):(max(dodyr, na.rm = TRUE)-1))) |>
    group_by(dodyr, sex) |>
    summarise(total = n()) |>
    pivot_wider(names_from = sex, values_from = total) |>
    adorn_totals("col")
  colnames(outputd) <- c("Year_of_OccurrenceD", "Deaths_Female", "Deaths_Male", "NS",  "Deaths_Total")
  
  outputi <- dth_data |>
    filter(ageinyrs < 5 & dodyr %in% c((max(dodyr, na.rm = TRUE)-5):(max(dodyr, na.rm = TRUE)-1))) |>
    group_by(dodyr, sex) |>
    summarise(total = n()) |>
    pivot_wider(names_from = sex, values_from = total) |>
    adorn_totals("col")
  colnames(outputi) <- c("Year_of_OccurrenceI", "Under5_Deaths_Female", "Under5_Deaths_Male", "NS",  "Under5_Deaths_Total")
  
  output <- cbind(outputb, outputd, outputi) |>
    select(Year_of_Occurrence, Live_Births_Female, Live_Births_Male, Live_Biths_Total, Deaths_Female, Deaths_Male, Deaths_Total, Under5_Deaths_Female, Under5_Deaths_Male, Under5_Deaths_Total)
  
  
 write.csv(output, paste0("./outputs/", tablename, ".csv"), row.names = FALSE) 
  return(output)
}
