#' Creates Tables 3.8 and 3.9
#'
#' @param data name of data frame being used
#' @param est_data data frame of estimated data being used
#' @param by_var the occurrence year being used e.g. dobyr or dodyr
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
#' @examples t3.8 <- create_t3.8_t3.9(bth_data, bth_est, rgn, topic = "births", tablename = "Table_3_8")
#'
create_t3.8_t3.9 <- function(data, est_data, by_var, topic = NA, tablename = NA){
  by_var <- enquo(by_var)
  by_var_name <- quo_name(by_var)

counts<- data |>
  filter(doryr == 2022 & if (topic == "births") is.na(sbind) else TRUE) |>
  group_by(!!by_var, sex) |>
  summarise(total = n()) |>
  pivot_wider(names_from = sex, values_from = total, values_fill = 0) |>
  select(-`not stated`) |>
  adorn_totals("col")

ests <- est_data |>
  filter(year == 2022)

output <- merge(counts, ests, by = by_var_name, all.x = TRUE) |>
  mutate(f_comp = round_excel(female.x/female.y, 2),
         m_comp = round_excel(male.x/male.y, 2),
         t_comp = round_excel(Total/total, 2)) |>
  mutate(f_adj = ceiling(female.x/f_comp),
         m_adj = ceiling(male.x/m_comp),
         t_adj = ceiling(Total/t_comp)) |>
  select(rgn, male.x, m_adj, female.x, f_adj, Total, t_adj)

write.csv(output, paste0("./outputs/", tablename, ".csv"), row.names = FALSE)
return(output)
}
