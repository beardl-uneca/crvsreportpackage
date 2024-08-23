#' Creates Tables 3.2 and 3.3
#'
#' @description Table 3.2 Proportion in percentage of live births by year of registration and year of occurrence
#' @description Table 3.3 Proportion in percentage  of deaths by year of registration and year of occurrence
#'
#' @param data name of data frame being used
#' @param occ_var year variable for births or deaths occurrences
#' @param topic whether births or deahts is being run
#' @param tablename name for csv output use _ instead of . for names
#'
#' @return data frame with tabulated results
#' @export
#'
#' @import dplyr
#' @import tidyr
#' @import janitor
#'
#' @examples t3.2 <- create_t3.2_t3.3(bth_data, occ_var = dobyr, topic = "births", tablename = "Table_3_2")
create_t3.2_t3.3 <- function(data, occ_var, topic = NA, tablename = "table_3_2"){
  max_value <- data %>% pull({{occ_var}}) %>% max(na.rm = TRUE)

  if(tolower(topic) == "births"){
    output <- data |>
      filter(is.na(sbind) & !is.na(doryr) &
               {{occ_var}} %in% generate_year_sequence(max_value)) |>
      group_by(doryr, {{occ_var}}) |>
      summarise(Total = n())

    output2 <- output %>%
      group_by(doryr) %>%
      summarise(total = sum(Total))

    # Merge total live births back into the original dataframe
    output <- output %>%
      left_join(output2, by = c("doryr" = "doryr")) %>%
      mutate(Percentage := round_excel((Total/ total) * 100, 2)) %>%
      select(-c(total, Total)) |>
      pivot_wider(names_from = doryr, values_from = Percentage, values_fill = 0) %>%
      adorn_totals("row", name = "Grand total")
  }else if(topic == "deaths"){
    output <- data |>
      filter(!is.na(doryr) & {{occ_var}} %in% generate_year_sequence()) |>
      group_by(doryr, {{occ_var}}) |>
      summarise(Total = n())

    output2 <- output %>%
      group_by(doryr) %>%
      summarise(total = sum(Total))

    # Merge total deaths back into the original dataframe
    output <- output %>%
      left_join(output2, by = c("doryr" = "doryr")) %>%
      mutate(Percentage := round_excel((Total/ total) * 100, 2)) %>%
      select(-c(total, Total)) |>
      pivot_wider(names_from = doryr, values_from = Percentage, values_fill = 0) %>%
      adorn_totals("row", name = "Grand total")
  }

  write.csv(output, paste0("./outputs/", tablename, ".csv"), row.names = FALSE)
  return(output)
}
