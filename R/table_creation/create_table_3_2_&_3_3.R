#' Creates Tables 3.2 & 3.3 and outputs csv files
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

  if(topic == "births"){
    output <- data |>
      filter(is.na(sbind) & !doryr %in% c("", "2023") &
               {{occ_var}} %in%c ((max_value - 5) : (max_value - 1))) |>
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
      filter(!doryr %in% c("", "2023") & {{occ_var}} %in% c((max_value - 5) : (max_value - 1))) |>
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
  }

  write.csv(output, paste0("./outputs/", tablename, ".csv"), row.names = FALSE)
  return(output)
}
