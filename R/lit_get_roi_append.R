#' lit_get_roi_append
#' @description queries litify for roi append data
#' @export
lit_get_roi_append <- function(
    cases = NULL,
    limit_clean = TRUE
){

  wlf::start(lib_sales_force = TRUE)


  roi_append <- lit_get_data(
    from_object = "litify_pm__Matter__c",
    select_object = c(
      Id, Name, Needles_CaseID__c, litify_pm__Case_Type__r.Name, Case_Severity__c,
      litify_pm__Filed_Date__c, Date_Complaint_Was_Filed__c, X3P_Lawsuit_Filed__c, Government_Claim_Filed__c,
    ),
    from_object_child = "Parties__r",
    select_object_child = c(BillingAddress, ShippingAddress),
    cases = cases,
    cases_field = "Needles_CaseID__c",
    col_name_clean = FALSE
  ) %>%
    lit_process_date_filed() %>%
    rename_col(
      id_matter = Id,
      severity = Case_Severity__c,
      case_type = litify_pm__Case_Type__r.Name,
      matter_name = Name,
      case = Needles_CaseID__c,
      billing_zip = Account.BillingAddress.postalCode,
      shipping_zip = Account.ShippingAddress.postalCode,

      lit_date_filed = litify_pm__Filed_Date__c,
      lit_date_complaint_was_filed = Date_Complaint_Was_Filed__c,
      lit_date_x3p_filed = X3P_Lawsuit_Filed__c,
      lit_date_government_claim_filed = Government_Claim_Filed__c
    ) %>%
    rowwise() %>%
    mutate(
      zip = ifelse(is.na(billing_zip), shipping_zip, billing_zip)
    ) %>%
    ungroup() %>%
    select(
      id_matter, matter_name, case, case_type, severity, zip, date_filed
    )


  return(roi_append)

}
