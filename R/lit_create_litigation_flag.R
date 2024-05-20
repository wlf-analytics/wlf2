#' lit_create_litigation_flag
#' @description create litigation flag
#' @export
lit_create_litigation_flag <- function(x){

  x <- x %>% rename_col_do_check("date_litigation_at", "Litigation_At__c")

  x %>%
    mutate(
      date_litigation_at = date_litigation_at %>% as.Date(),
      litigation = date_litigation_at %>% is.na() %>% not()
    )

}
