#' lit_recode_practice_area
#' @description recode practice area
#' @export
lit_recode_practice_area <- function(x){


  x <- x %>% rename_col_do_check("practice_area", "Practice_Area__c")


  if( !col_check(x, "litigation", hard_stop = FALSE)){
    x <- x %>% lit_create_litigation_flag()
  }


  x %>%
    mutate(
      practice_area = practice_area %>% recode("Class Action" = "Class-Action"),
      practice_area = ifelse(practice_area == "PI" & litigation == TRUE, "PI-Litigation",
                             ifelse(practice_area == "PI" & litigation == FALSE, "PI-Pre-Litigation", practice_area)
      ),

      practice_area = ifelse( (practice_area == "Class-Action") & (case_type == "Class Action - Consumer Litigation"), "Class-Action-Consumer", practice_area),
      practice_area = ifelse( (practice_area == "Class-Action") & (case_type == "Consumer"), "Class-Action-Consumer", practice_area),

      practice_area = ifelse( (practice_area == "Class-Action") & (case_type == "Class Action - Wage & Hour"), "Class-Action-W&H", practice_area),
      practice_area = ifelse( (practice_area == "Class-Action") & (case_type == "Wage & Hour (Class Action)"), "Class-Action-W&H", practice_area),
      practice_area = ifelse( (practice_area == "Class-Action") & (case_type == "Wage & Hour"), "Class-Action-W&H", practice_area),
    )

}
