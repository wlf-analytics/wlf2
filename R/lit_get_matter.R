#' lit_get_matter
#' @description queries litify for matter data
#' @export
lit_get_matter <- function(attach_accident_type = FALSE){

  wlf::start(lib_sales_force = TRUE)


  df <- lit_get_data(
    from_object = "litify_pm__Matter__c",

    select_object = wlf:::cq(
      Id, Name, Needles_CaseID__c, litify_pm__Status__c, Display_Name2__c, Litigation_At__c,

      Received_Signed_Agreement__c, litify_pm__Incident_date__c,

      Practice_Area__c, litify_pm__Case_Type__r.Name, Policy_Limit__c, Case_Severity__c,
      Government_Case__c,

      Drop_Date__c, Dropped_At__c, Drop_Or_Pending_Drop_At__c,
      Drop_Subout_Date__c, Subout_Date__c, Subout_At__c,
      litify_pm__Closed_Reason__c, Sub_Status__c,

      Lead_Case__c, Lead_Matter__c,
      litify_pm__Companion__r.Lead_Case__c,
      litify_pm__Companion__r.Id,

      litify_pm__Filed_Date__c, Date_Complaint_Was_Filed__c,
      X3P_Lawsuit_Filed__c, Government_Claim_Filed__c,

      Closed_At__c, litify_pm__Closed_Date__c,

      litify_pm__Client__r.BillingPostalCode
    )
  ) %>%

    lit_filter_out_test_cases() %>%

    lit_process_date_filed() %>%

    rename_col(
      .select = TRUE,
      .distinct = TRUE,

      id_matter = Id,
      case = Needles_CaseID__c,
      case_name = Display_Name2__c,
      name_matter = Name,

      client_zip = litify_pm__Client__r.BillingPostalCode,

      practice_area = Practice_Area__c,
      case_severity = Case_Severity__c,
      case_type = litify_pm__Case_Type__r.Name,
      policy_limit = Policy_Limit__c,
      government = Government_Case__c,

      source_type = litify_pm__Source_Type__c,
      source_category = litify_pm__Source__r.Category__c,
      marketing_details = Marketing_Details__c,

      date_litigation_at = Litigation_At__c,
      date_signed_agreement = Received_Signed_Agreement__c,
      date_incident = litify_pm__Incident_date__c,
      date_filed = date_filed,

      status = litify_pm__Status__c,
      closed_reason = litify_pm__Closed_Reason__c,

      date_drop_c = Drop_Date__c,
      date_drop_or_pending_drop_c = Drop_Or_Pending_Drop_At__c,
      date_drop_subout_c = Drop_Subout_Date__c,
      date_dropped_at_c = Dropped_At__c,

      sub_status = Sub_Status__c,
      date_subout_at_c = Subout_At__c,
      date_subout_c = Subout_Date__c,

      lead_case = Lead_Case__c,
      lead_matter = Lead_Matter__c,
      companion_lead_case = litify_pm__Companion__r.Lead_Case__c,
      id_companion = litify_pm__Companion__r.Id,

      date_closed_at = Closed_At__c,
      date_closed_date = litify_pm__Closed_Date__c
    ) %>%

    mutate(

      case = case %>% as.numeric(),

      date_signed_agreement = date_signed_agreement %>% as.Date(),
      date_litigation_at = date_litigation_at %>% as.Date(),
      date_incident = date_incident %>% as.Date(),

      date_closed_at = date_closed_at %>% as.Date(),
      date_closed_date = date_closed_date %>% as.Date(),

      lead_case = lead_case %>% recode("Yes" = TRUE, "No" = FALSE),

      government = government %>% str_scrub() %>% recode("yes" = TRUE, "no" = FALSE, .default = NA),
      policy_limit_cleaned = policy_limit %>% translate_limits(),

      date_drop_c = date_drop_c %>% as.Date(),
      date_drop_or_pending_drop_c = date_drop_or_pending_drop_c %>% as.Date(),
      date_drop_subout_c = date_drop_subout_c %>% as.Date(),
      date_dropped_at_c = date_dropped_at_c %>% as.Date(),

      date_subout_at_c = date_subout_at_c %>% as.Date(),
      date_subout_c = date_subout_c %>% as.Date(),

      signed_month = date_signed_agreement %>% zoo::as.yearmon() %>% zoo::as.Date(),
      litigation_start_month = date_litigation_at %>% zoo::as.yearmon() %>% zoo::as.Date(),

      litigation_same_month = (signed_month == litigation_start_month) %>% if_na_return(FALSE),

    ) %>%

    lit_create_litigation_flag() %>%

    lit_recode_practice_area() %>%

    lit_recode_closed_reason() %>%

    lit_recode_status() %>%

    lit_process_drop_sub_vars()


  if(attach_accident_type){

    questionnaire <- lit_get_intake_questionnaire(
      cases = df[["id_matter"]],
      cases_field = "id_matter",
      clean_cols = FALSE,
      question_filter = c('Type of Accident.', 'Type of Accident')
    ) %>%
      select(-id_intake) %>%
      distinct() %>%
      group_by(id_matter) %>%
      mutate(
        type_of_accident = ifelse( n() == 1, type_of_accident, paste0(type_of_accident, collapse = "|"))
      ) %>%
      ungroup() %>%
      distinct()

    df <- left_join(df, questionnaire, by = "id_matter")
  }


  return(df)
}





#' lit_get_matter_DEPRECATE
#' @description queries litify for matter data
#' @export
lit_get_matter_DEPRECATE <- function(
    cases = NULL, limit = NULL,
    cases_field = c("case", "id_intake", "id_matter", NULL)
){

  cases_field <- match.arg(cases_field)

  if( !is.null(cases_field) ){

    cases_field <- switch(
      cases_field,
      "case" = "Needles_CaseID__c",
      "id_intake" = "litify_pm__Intakes__r.Id",
      "id_matter" = "Id",
      "custom" = custom_field
    )
  }


  matter <- lit_get_data(
    from_object = "litify_pm__Matter__c",
    select_object = c(
      "Id", "Needles_CaseID__c", "litify_pm__Client__r.Name", "litify_pm__Incident_date__c",
      "Policy_Limit__c", "Received_Signed_Agreement__c", "Practice_Area__c",
      "litify_pm__Case_Type__r.Name", "Case_Severity__c", "litify_pm__Source__r.Name",
      "litify_pm__Primary_Intake__c", "Government_Case__c",
      "litify_pm__Filed_Date__c", "Date_Complaint_Was_Filed__c",
      "X3P_Lawsuit_Filed__c", "Government_Claim_Filed__c"
    ),
    cases = cases,
    cases_field = cases_field,
    limit = limit,
    col_name_clean = FALSE

  ) %>% rename_col(
    id_matter = Id,
    case = Needles_CaseID__c,
    client_name = litify_pm__Client__r.Name,
    date_incident = litify_pm__Incident_date__c,
    policy_limit = Policy_Limit__c,
    date_agreement_signed = Received_Signed_Agreement__c,
    practice_area = Practice_Area__c,
    case_type = litify_pm__Case_Type__r.Name,
    severity = Case_Severity__c,
    source = litify_pm__Source__r.Name,
    id_intake = litify_pm__Primary_Intake__c,
    government = Government_Case__c,
    lit_date_filed = litify_pm__Filed_Date__c,
    lit_date_complaint_was_filed = Date_Complaint_Was_Filed__c,
    lit_date_x3p_filed = X3P_Lawsuit_Filed__c,
    lit_date_government_claim_filed = Government_Claim_Filed__c
  ) %>% dplyr::select(
    id_matter, id_intake, case, client_name,
    practice_area, case_type, severity, government, policy_limit, source,
    date_incident,date_agreement_signed,
    lit_date_filed,
    lit_date_complaint_was_filed,
    lit_date_x3p_filed,
    lit_date_government_claim_filed
  ) %>%
    rowwise() %>%
    mutate(
      lit_date_filed = lit_date_filed %>% as.Date(),
      lit_date_complaint_was_filed  = lit_date_complaint_was_filed  %>% as.Date(),
      lit_date_x3p_filed = lit_date_x3p_filed %>% as.Date(),
      lit_date_government_claim_filed = lit_date_government_claim_filed %>% as.Date(),

      date_filed = ifelse(
        all(
          is.na(
            base::c(lit_date_filed, lit_date_complaint_was_filed, lit_date_x3p_filed, lit_date_government_claim_filed)
          )
        ),
        as.Date(NA),
        min(
          lit_date_filed, lit_date_complaint_was_filed, lit_date_x3p_filed, lit_date_government_claim_filed
          , na.rm = TRUE)
      ) %>% as.Date(),

      date_filed = ifelse(is.infinite(date_filed), NA, date_filed) %>% as.Date()
    ) %>%
    ungroup() %>%
    mutate(
      government = government %>% str_scrub() %>% dplyr::recode("yes" = TRUE, "no" = FALSE, .default = NA),
      policy_limit = policy_limit %>% translate_limits(),
      date_incident = date_incident %>% as.Date(),
      date_filed = date_filed %>% as.Date(),
      date_agreement_signed = date_agreement_signed %>% as.Date()
    )


  return(matter)
}
