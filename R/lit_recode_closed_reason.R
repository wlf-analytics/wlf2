#' lit_recode_closed_reason
#' @description recode practice area
#' @export
lit_recode_closed_reason <- function(x){


  x <- x %>% rename_col_do_check("closed_reason", "litify_pm__Closed_Reason__c")

  x %>%
    mutate(
      closed_reason = closed_reason %>% recode(
        "Dropped" = "dropped",
        "Referred Out" = "referred_out",
        "Settled" = "settled",
        "Subbed-Out" = "subout",
        "Dropped by client" = "dropped",
        "Verdict" = "verdict"
      )
    )

}
