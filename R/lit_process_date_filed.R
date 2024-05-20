#' lit_process_date_filed
#' @description process date filed
#' @export
lit_process_date_filed <- function(x, remove_input_dates = TRUE, insert_if_missing = FALSE){

  wlf::start()

  if(insert_if_missing){
    x <- x %>% insert_missing_column(litify_pm__Filed_Date__c, Date_Complaint_Was_Filed__c, X3P_Lawsuit_Filed__c, Government_Claim_Filed__c)
  }


  col_check(x, c("litify_pm__Filed_Date__c", "Date_Complaint_Was_Filed__c", "X3P_Lawsuit_Filed__c", "Government_Claim_Filed__c"))


  x <- x %>%
    rowwise() %>%
    mutate(

      litify_pm__Filed_Date__c = litify_pm__Filed_Date__c %>% as.Date(),
      Date_Complaint_Was_Filed__c = Date_Complaint_Was_Filed__c %>% as.Date(),
      X3P_Lawsuit_Filed__c = X3P_Lawsuit_Filed__c %>% as.Date(),
      Government_Claim_Filed__c = Government_Claim_Filed__c %>% as.Date(),

      date_filed = ifelse(

        all(is.na(base::c(litify_pm__Filed_Date__c, Date_Complaint_Was_Filed__c, X3P_Lawsuit_Filed__c, Government_Claim_Filed__c))),

        as.Date(NA),

        min(litify_pm__Filed_Date__c, Date_Complaint_Was_Filed__c, X3P_Lawsuit_Filed__c, Government_Claim_Filed__c, na.rm = TRUE)

      ) %>% as.Date()

    ) %>%
    ungroup()


  if(remove_input_dates){
    x <- x %>%
      select(
        -litify_pm__Filed_Date__c,
        -Date_Complaint_Was_Filed__c,
        -X3P_Lawsuit_Filed__c,
        -Government_Claim_Filed__c
      )
  }


  return(x)
}
