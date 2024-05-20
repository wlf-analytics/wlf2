#' process_case_costs
#' @description Process case costs
#' @param where Location of the QB export
#' @export
process_case_costs <- function(where, sheet_name = "Sheet1"){

  wlf::start(lib_sales_force = T)

  costs <- read_xl(where, sheet = sheet_name) %>%
    suppressMessages() %>%
    select(type, date, num, name, source_name, memo, class, split, debit, credit, original_amount, balance) %>%
    distinct() %>%
    filter_all(any_vars(!is.na(.))) %>%
    filter(!is.na(type)) %>%
    mutate(
      date = date %>% as.Date(origin = "1899-12-30"),
      case = name %>% strsplit(" ") %>% map(head(1)) %>% unlist() %>% as.numeric() %>% suppressWarnings(),
      month = date %>% lubridate::month(label = TRUE)
    )



  costs_attorneys <- lit_get_data(
    "litify_pm__Matter__c",
    wlf:::c(Id, Needles_CaseID__c, litify_pm__Case_Type__r.Name, Assigned_Attorney__r.Name, litify_pm__Principal_Attorney__r.Name),
    add_links = TRUE
  ) %>%
    suppressWarnings() %>%
    rename_col(
      .select = TRUE,
      matter_id = Id,
      case = Needles_CaseID__c,
      case_type = litify_pm__Case_Type__r.Name,
      assigned_attorney = Assigned_Attorney__r.Name,
      principal_attorney = litify_pm__Principal_Attorney__r.Name,
      link_matter = link_Id
    ) %>%
    mutate(
      case = case %>% as.numeric(),
      attorney = ifelse(is.na(principal_attorney), assigned_attorney, principal_attorney)
    ) %>%
    select(
      matter_id, case, case_type,
      assigned_attorney, principal_attorney, attorney, link_matter
    )


  results <- list(
    missing = list(),
    tables = list()
  )


  results[["missing"]][["no_case"]] <- costs %>% filter(is.na(case))
  costs <- costs %>% filter(!is.na(case))


  results[["missing"]][["no_matter"]] <- costs %>% filter(!case %in% costs_attorneys[["case"]])
  costs <- costs %>% filter(case %in% costs_attorneys[["case"]])

  results[["missing"]][["no_date"]] <- costs %>% filter(is.na(date))
  costs <- costs %>% filter(!is.na(date))

  results[["missing"]][["cases_with_multiple_matters"]] <-
    names(table(costs_attorneys[["case"]])[table(costs_attorneys[["case"]]) > 1]) %>% as.numeric() %>% unique()


  results[["missing"]][["expenses_with_multiple_matters"]] <- costs %>% filter(case %in% results[["missing"]][["cases_with_multiple_matters"]])
  costs <- costs %>% filter(!case %in% results[["missing"]][["cases_with_multiple_matters"]])

  results[["missing"]][["cases_without_attorneys"]] <- costs_attorneys %>%
    filter(case %in% costs[["case"]]) %>%
    filter(is.na(attorney)) %>%
    select(case) %>% unlist() %>% unique()


  results[["missing"]][["expenses_without_attorneys"]] <- costs %>% filter(case %in% results[["missing"]][["cases_without_attorneys"]])
  costs <-  costs %>% filter(!case %in% results[["missing"]][["cases_without_attorneys"]])


  costs <- dplyr::left_join(
    costs,
    costs_attorneys %>% select(case, case_type, attorney, link_matter),
    by = "case")


  results[["tables"]][["costs_by_case_attorney"]] <- costs %>%
    group_by(case, month, attorney) %>%
    summarise(original_amount = sum(original_amount, na.rm = TRUE)) %>%
    ungroup() %>%
    tidyr::pivot_wider(names_from = month , values_from = original_amount)


  results[["tables"]][["costs_by_attorney"]] <- costs %>%
    group_by(month, attorney) %>%
    summarise(original_amount = sum(original_amount, na.rm = TRUE)) %>%
    ungroup()



  attorneys <- results[["tables"]][["costs_by_attorney"]][["attorney"]] %>% unique() %>% sort()

  missing_attorneys <- attorneys[str_scrub(attorneys) %in% str_scrub(wlf::df_name_to_department[["name"]])]

  missing_attorneys_dept <- df_name_to_department %>%
    filter(
      str_scrub(df_name_to_department[["name"]]) %in% str_scrub(attorneys)
    ) %>%
    filter(is.na(department)) %>%
    select(name) %>%
    unlist()



  if( length(missing_attorneys_dept) > 0 ){
    warning(glue("We're missing the following attorneys from department classification: {glue_collapse(missing_attorneys, sep = ', ')}"))
  }



  results[["tables"]][["costs_by_department"]] <-
    left_join(
      results[["tables"]][["costs_by_attorney"]] %>% mutate(attorney = str_scrub(attorney)),
      df_name_to_department %>% select(-other) %>% mutate(name = str_scrub(name)),
      by = join_by(attorney == name)
    ) %>%
    group_by(month, department) %>%
    summarise(
      original_amount = sum(original_amount, na.rm = TRUE)
    ) %>%
    ungroup() %>%
    tidyr::pivot_wider(
      names_from = month ,
      values_from = original_amount
    )


  results[["tables"]][["costs_by_attorney"]] <- results[["tables"]][["costs_by_attorney"]] %>%
    tidyr::pivot_wider(
      names_from = month ,
      values_from = original_amount
    )


  return(results)
}

