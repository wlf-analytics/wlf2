#' lit_filter_out_test_cases
#' @description filters out test cases
#' @export
lit_filter_out_test_cases <- function(x){


  col_check(x, c("Display_Name2__c", "litify_pm__Status__c", "litify_pm__Closed_Reason__c"))


  x %>%
    filter(
      Display_Name2__c %>% stringr::str_detect(stringr::coll("test", ignore_case = TRUE), negate = TRUE) %>% if_na_return(TRUE)
    ) %>%

    filter(
      litify_pm__Status__c %>% stringr::str_detect(stringr::coll("test", ignore_case = TRUE), negate = TRUE) %>% if_na_return(TRUE)
    ) %>%

    filter(
      litify_pm__Closed_Reason__c %>% stringr::str_detect(stringr::coll("test", ignore_case = TRUE), negate = TRUE) %>% if_na_return(TRUE)
    ) %>%

    filter(
      litify_pm__Closed_Reason__c %>% stringr::str_detect(stringr::coll("duplicate", ignore_case = TRUE), negate = TRUE) %>% if_na_return(TRUE)
    )

}
