#' lit_process_drop_sub_vars
#' @description process drop sub vars
#' @export
lit_process_drop_sub_vars <- function(x){


  x <- x %>%
    rename_col_do_check("date_drop_c", "Drop_Date__c") %>%
    rename_col_do_check("date_drop_or_pending_drop_c", "date_drop_or_pending_drop_c") %>%
    rename_col_do_check("date_drop_subout_c", "Drop_Subout_Date__c") %>%
    rename_col_do_check("date_dropped_at_c", "Dropped_At__c") %>%
    rename_col_do_check("date_subout_at_c", "Subout_At__c") %>%
    rename_col_do_check("date_subout_c", "Subout_Date__c")


  col_check(x, c("status", "closed_reason"))


  x %>%
    mutate(
      date_drop_c = date_drop_c %>% as.Date(),
      date_drop_or_pending_drop_c = date_drop_or_pending_drop_c %>% as.Date(),
      date_drop_subout_c = date_drop_subout_c %>% as.Date(),
      date_dropped_at_c = date_dropped_at_c %>% as.Date(),

      date_subout_at_c = date_subout_at_c %>% as.Date(),
      date_subout_c = date_subout_c %>% as.Date(),
    ) %>%
    rowwise() %>%
    mutate(
      date_drop = ifelse(status == "dropped" | (status == "closed" & closed_reason == "dropped"), date_drop_subout_c, NA) %>% as.Date(),
      date_subout = date_subout_c,
      date_subout_unauditted = ifelse(status == "subout" | (status == "closed" & closed_reason == "subout"), date_drop_subout_c, NA) %>% as.Date(),

    ) %>%
    mutate(
      date_subout = ifelse(is.na(date_subout), date_subout_unauditted, date_subout) %>% as.Date(),
      date_drop = ifelse(
        (status == "dropped" | (status == "closed" & closed_reason == "dropped")) & is.na(date_drop),
        date_dropped_at_c, date_drop) %>% as.Date(),
    ) %>%
    ungroup() %>%
    mutate(

      drop_month = date_drop %>% zoo::as.yearmon() %>% zoo::as.Date(),

      subout_month = date_subout %>% zoo::as.yearmon() %>% zoo::as.Date(),

      drop_same_month = (signed_month == drop_month),
      subout_same_month = (signed_month == subout_month),

      drop_days = (date_drop - date_signed_agreement) %>% as.numeric(),
      subout_days = (date_subout - date_signed_agreement) %>% as.numeric(),
    )

}



