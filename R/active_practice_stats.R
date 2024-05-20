#' active_practice_stats
#' @description foo boo
#' @export
active_practice_stats <- function(where, end_date, do_more = FALSE){

  warning("very finicky function. please inspect outputs and make sure you do it exactly the same as last month... We need a more sustainable method.")

  wlf::start()

  df <- read_xl(where) %>%
    rename_col(
      matter_created_date = matter_created_date,
      litigation_at = litigation_at
    ) %>%
    mutate(
      Name = matter_matter_name,
      matter_created_date = matter_created_date %>% as.Date("%m/%d/%Y"),
      litigation_at = litigation_at %>% as.Date("%m/%d/%Y"),
      date = matter_created_date,
      date = ifelse(is.na(date), litigation_at, date)
    ) %>%
    filter(
      (date <= end_date) | is.na(date)
    ) %>%
    left_join(
      lit_get_data(
        from_object = "litify_pm__Matter__c",
        select_object = c(
          Name, litify_pm__Incident_date__c, Received_Signed_Agreement__c,
        )
      ),
      by = "Name"
    ) %>%
    mutate(
      litify_pm__Incident_date__c = litify_pm__Incident_date__c %>% as.Date(),
      Received_Signed_Agreement__c = Received_Signed_Agreement__c %>% as.Date()
    )

  result <- list()

  result[["df"]] <- df

  result[["inv_age"]] <- df %>%
    summarise(
      time_from_incident = mean(end_date - litify_pm__Incident_date__c, na.rm = T),
      time_from_signed_agreement = mean(end_date - Received_Signed_Agreement__c, na.rm = T)
    )

  if(do_more){
    result[["attorney_table"]] <- df %>%
      mutate(
        same_month = zoo::as.yearmon(litigation_at)  == zoo::as.yearmon(end_date)
      ) %>%
      group_by(principal_attorney) %>%
      summarise(
        this_month = sum(same_month, na.rm = T),
        count = n(),
      ) %>%
      ungroup() %>%
      bind_rows(
        TOTAL = summarise_if(
          .,
          is.numeric,
          sum
        )
      )
  }

  return(result)
}

