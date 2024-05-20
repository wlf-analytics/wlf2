#' lit_get_drops
#' @description queries litify for drop and subout data
#' @export
lit_get_drops <- function(
    start_date = as.Date("2021-01-01"),
    parallel_process = TRUE,
    add_lead_gen_source = FALSE
){

  wlf::start(lib_sales_force = TRUE)


  if( parallel_process ){
    start(lib_future = TRUE)

    old_plan <- plan(multisession)

    on.exit(plan(old_plan), add = TRUE)
  }


  if( add_lead_gen_source ){

    if( parallel_process ){

      futureAssign("df", lit_get_matter(), seed = NULL)
      futureAssign("sources", lit_get_lead_gen_source(), seed = NULL)

    }else if( !parallel_process ){
      df <- lit_get_matter()
      sources <- lit_get_lead_gen_source()
    }

  }else if( !add_lead_gen_source ){
    df <- lit_get_matter()
  }



  if( add_lead_gen_source ){

    df <- left_join(
      df,
      sources %>% select(
        id_matter,
        matter_source, matter_details,
        intake_source, intake_details,
        source_original, details_original,
        source, details,
        lead_gen_source, sub_lead_gen
      ),
      by = "id_matter",
      relationship = "one-to-one"
    )

  }


  df <- df %>%
    mutate(
      drop_days_30 = drop_days <= 30,
      drop_days_60 = drop_days <= 60,
      drop_days_90 = drop_days <= 90,
      drop_days_90_180 = (drop_days > 90) & (drop_days <= 180),
      drop_days_180 = drop_days > 180,
    )

  ######################

  drop_summary_month <- function(dfx, ...){

    df_summary <- dfx %>%
      group_by(..., signed_month, .drop = FALSE) %>%
      summarise(
        cases_signed = n(),
        drops_signed_month = sum(!is.na(date_drop)),
        subout_signed_month = sum(!is.na(date_subout)),
        lit_cases_signed = litigation_same_month %>% sum(),
        lit_cases_converted = ((litigation_same_month %>% not()) & litigation) %>% sum(),

        drop_days_ave = drop_days %>% mean(na.rm=T),

        drop_days_30_count = drop_days_30 %>% sum(na.rm=T),
        drop_days_60_count = drop_days_60 %>% sum(na.rm=T),
        drop_days_90_count = drop_days_90 %>% sum(na.rm=T),
        drop_days_90_180_count = drop_days_90_180 %>% sum(na.rm=T),
        drop_days_180_count = drop_days_180 %>% sum(na.rm=T),

        drop_days_30_ave = drop_days_30 %>% mean(na.rm=T),
        drop_days_60_ave = drop_days_60 %>% mean(na.rm=T),
        drop_days_90_ave = drop_days_90 %>% mean(na.rm=T),
        drop_days_90_180_ave = drop_days_90_180 %>% mean(na.rm=T),
        drop_days_180_ave = drop_days_180 %>% mean(na.rm=T)
      )  %>%
      suppressMessages() %>%
      ungroup()


    drops_drop_month <- dfx %>%
      filter(
        !is.na(date_drop)
      ) %>%
      group_by(..., drop_month, .drop = FALSE) %>%
      summarise(
        drops_drop_month = n()
      ) %>%
      suppressMessages() %>%
      ungroup()


    subout_subout_month <- dfx %>%
      filter(
        !is.na(date_subout)
      ) %>%
      group_by(..., subout_month, .drop = FALSE) %>%
      summarise(
        subout_subout_month = n()
      ) %>%
      suppressMessages() %>%
      ungroup()


    if( length(wlf:::cq(...)) == 0 ){

      summary_drop_sub <- full_join(
        drops_drop_month,
        subout_subout_month,
        by = join_by(drop_month == subout_month)
      ) %>%
        mutate(
          drops_drop_month = ifelse(is.na(drops_drop_month), 0, drops_drop_month),
          subout_subout_month = ifelse(is.na(subout_subout_month), 0, subout_subout_month)
        )

      df_summary <- full_join(
        df_summary,
        summary_drop_sub,
        by = join_by(signed_month == drop_month)
      )

    }else if( length(wlf:::cq(...)) == 1 ){

      aditional_args <- wlf:::cq(...)

      summary_drop_sub <- full_join(
        drops_drop_month,
        subout_subout_month,
        by = join_by(
          {{aditional_args}} == {{aditional_args}},
          drop_month == subout_month
        )
      ) %>%
        mutate(
          drops_drop_month = ifelse(is.na(drops_drop_month), 0, drops_drop_month),
          subout_subout_month = ifelse(is.na(subout_subout_month), 0, subout_subout_month)
        )

      df_summary <- full_join(
        df_summary,
        summary_drop_sub,
        by = join_by(
          {{aditional_args}} == {{aditional_args}},
          signed_month == drop_month
        )
      )

    }else{
      stop("drop join not programmed for, i.e., more than one field in ...")
    }

    df_summary <- df_summary %>%
      select(
        ...,
        signed_month,
        cases_signed,
        drops_signed_month, drops_drop_month,
        subout_signed_month, subout_subout_month,

        lit_cases_signed, lit_cases_converted,

        drop_days_30_count, drop_days_60_count, drop_days_90_count,
        drop_days_90_180_count, drop_days_180_count,

        drop_days_ave, drop_days_30_ave, drop_days_60_ave, drop_days_90_ave,
        drop_days_90_180_ave, drop_days_180_ave
      ) %>%
      mutate(
        drops_signed_month_perc = drops_signed_month / cases_signed,
        drops_drop_month_perc = drops_drop_month / cases_signed,
        subout_signed_month_perc = subout_signed_month / cases_signed,
        subout_subout_month_perc = subout_subout_month / cases_signed,
        lit_cases_signed_perc = lit_cases_signed / cases_signed,
        lit_cases_converted_perc = lit_cases_signed / cases_signed,

        drop_days_30_perc = drop_days_30_count / cases_signed,
        drop_days_60_perc = drop_days_60_count / cases_signed,
        drop_days_90_perc = drop_days_90_count / cases_signed,
        drop_days_90_180_perc = drop_days_90_180_count / cases_signed,
        drop_days_180_perc = drop_days_180_count / cases_signed
      )

    df_summary[is.na(df_summary)] <- NA
    df_summary[df_summary == Inf] <- NA

    df_summary <- df_summary %>%
      arrange(..., signed_month)


    return(df_summary)
  }


  drop_summary_year <- function(dfx, ...){

    df_summary <- dfx %>%
      group_by(..., signed_year, .drop = FALSE) %>%
      summarise(
        cases_signed = n(),
        drops_signed_year = sum(!is.na(date_drop)),
        subout_signed_year = sum(!is.na(date_subout)),
        drop_days_ave = drop_days %>% mean(na.rm=T),
      )  %>%
      suppressMessages() %>%
      ungroup()


    drops_drop_year <- dfx %>%
      filter(
        !is.na(date_drop)
      ) %>%
      group_by(..., drop_year, .drop = FALSE) %>%
      summarise(
        drops_drop_year = n()
      ) %>%
      suppressMessages() %>%
      ungroup()


    subout_subout_year <- dfx %>%
      filter(
        !is.na(date_subout)
      ) %>%
      group_by(..., subout_year, .drop = FALSE) %>%
      summarise(
        subout_subout_year = n()
      ) %>%
      suppressMessages() %>%
      ungroup()


    if( length(c(...)) == 0 ){

      summary_drop_sub <- full_join(
        drops_drop_year,
        subout_subout_year,
        by = join_by(
          drop_year == subout_year
        )
      ) %>%
        mutate(
          drops_drop_year = ifelse(is.na(drops_drop_year), 0, drops_drop_year),
          subout_subout_year = ifelse(is.na(subout_subout_year), 0, subout_subout_year)
        )

      df_summary <- full_join(
        df_summary,
        summary_drop_sub,
        by = join_by(
          signed_year == drop_year
        )
      )

    }else if( length(c(...)) == 1 ){

      aditional_args <- c(...)


      summary_drop_sub <- full_join(
        drops_drop_year,
        subout_subout_year,
        by = join_by(
          {{aditional_args}} == {{aditional_args}},
          drop_year == subout_year
        )
      ) %>%
        mutate(
          drops_drop_year = ifelse(is.na(drops_drop_year), 0, drops_drop_year),
          subout_subout_year = ifelse(is.na(subout_subout_year), 0, subout_subout_year)
        )

      df_summary <- full_join(
        df_summary,
        summary_drop_sub,
        by = join_by(
          {{aditional_args}} == {{aditional_args}},
          signed_year == drop_year
        )
      )

    }else if( length(c(...)) == 2 ){

      aditional_args <- c(...)

      aditional_args_1 <- aditional_args[[1]]
      aditional_args_2 <- aditional_args[[2]]

      summary_drop_sub <- full_join(
        drops_drop_year,
        subout_subout_year,
        by = join_by(
          {{aditional_args_1}} == {{aditional_args_1}},
          {{aditional_args_2}} == {{aditional_args_2}},
          drop_year == subout_year
        )
      ) %>%
        mutate(
          drops_drop_year = ifelse(is.na(drops_drop_year), 0, drops_drop_year),
          subout_subout_year = ifelse(is.na(subout_subout_year), 0, subout_subout_year)
        )

      df_summary <- full_join(
        df_summary,
        summary_drop_sub,
        by = join_by(
          {{aditional_args_1}} == {{aditional_args_1}},
          {{aditional_args_2}} == {{aditional_args_2}},
          signed_year == drop_year
        )
      )

    }else if( length(c(...)) == 3 ){

      aditional_args <- c(...)

      aditional_args_1 <- aditional_args[[1]]
      aditional_args_2 <- aditional_args[[2]]
      aditional_args_3 <- aditional_args[[3]]

      summary_drop_sub <- full_join(
        drops_drop_year,
        subout_subout_year,
        by = join_by(
          {{aditional_args_1}} == {{aditional_args_1}},
          {{aditional_args_2}} == {{aditional_args_2}},
          {{aditional_args_3}} == {{aditional_args_3}},
          drop_year == subout_year
        )
      ) %>%
        mutate(
          drops_drop_year = ifelse(is.na(drops_drop_year), 0, drops_drop_year),
          subout_subout_year = ifelse(is.na(subout_subout_year), 0, subout_subout_year)
        )

      df_summary <- full_join(
        df_summary,
        summary_drop_sub,
        by = join_by(
          {{aditional_args_1}} == {{aditional_args_1}},
          {{aditional_args_2}} == {{aditional_args_2}},
          {{aditional_args_3}} == {{aditional_args_3}},
          signed_year == drop_year
        )
      )

    }else{
      stop("drop join not programmed for, i.e., more than one field in ...")
    }

    df_summary <-
      df_summary %>%
      select(
        ...,
        signed_year,
        cases_signed,
        drops_signed_year, drops_drop_year,
        subout_signed_year, subout_subout_year
      ) %>%
      mutate(
        cases_signed = ifelse(is.na(cases_signed), 0, cases_signed),
        drops_signed_year = ifelse(is.na(drops_signed_year), 0, drops_signed_year),
        drops_drop_year = ifelse(is.na(drops_drop_year), 0, drops_drop_year),
        subout_signed_year = ifelse(is.na(subout_signed_year), 0, subout_signed_year),
        subout_subout_year = ifelse(is.na(subout_subout_year), 0, subout_subout_year),

        drops_signed_year_perc = drops_signed_year / cases_signed,
        drops_drop_year_perc = drops_drop_year / cases_signed,
        subout_signed_year_perc = subout_signed_year / cases_signed,
        subout_subout_year_perc = subout_subout_year / cases_signed
      )

    df_summary[is.na(df_summary)] <- NA
    df_summary[df_summary == Inf] <- NA


    df_summary <- df_summary %>%
      arrange(..., signed_year)


    return(df_summary)
  }


  drop_summary <- function(dfx, ..., .by_month = TRUE){
    if(.by_month){
      drop_summary_month(dfx, ...)
    }else if(!.by_month){
      drop_summary_year(dfx, ...)
    }
  }


  to_total <- function(df_sub, df_total){

    df_total %>%
      rename_col(
        .select = TRUE,
        signed_month = signed_month,
        cases_signed_total = cases_signed,
        drops_signed_month_total = drops_signed_month,
        drops_drop_month_total = drops_drop_month
      ) %>%
      left_join(
        df_sub,
        .,
        by = "signed_month"
      ) %>%
      mutate(
        drops_signed_month_perc_to_total = drops_signed_month / cases_signed_total,
        drops_drop_month_perc_to_total = drops_drop_month / cases_signed_total,
        drops_signed_month_perc_to_total_drops = drops_signed_month / drops_signed_month_total,
        drops_drop_month_perc_to_total_drops = drops_drop_month / drops_drop_month_total
      )
  }


  ######################

  df <- df %>% mutate(
    policy_limitR = policy_limit  %>% recode(
      "10/20" = "15-20K/3-40K",
      "15/30" = "15-20K/3-40K",
      "20/40" = "15-20K/3-40K",

      "25/50" = "25K/50-100K",
      "25/100" = "25K/50-100K",

      "30/60" = "30K/60K",
      "60" = "30K/60K",

      "$45,000/$90,000" = "50K/100K",
      "50/100" = "50K/100K",
      "50" = "50K/100K",
      "$85,000" = "50K/100K",

      "100/300" = "100K/2-300K",
      "100/200" = "100K/2-300K",
      "100" = "100K/2-300K",
      "150" = "100K/2-300K",

      "250/500" = "250K/500K",
      "250" = "250K/500K",
      "200" = "250K/500K",

      "300" = "300K/3-500K",
      "300/300" = "300K/3-500K",

      "500" = "500K/1M",
      "500/1mil" = "500K/1M",

      "600" = "big",

      "750" = "big",
      "1mil" = "big",
      "1mil/10mil" = "big",
      "1mil/3mil" = "big",
      "1mil/2mil" = "big",
      "1.25mil" = "big",
      "1.5mil" = "big",
      "2mil" = "big",
      "2.5mil" = "big",
      "3mil" = "big",
      "4mil" = "big",
      "5mil+" = "big",
      "5mil" = "big",
      "10mil" = "big",
      "15 Mil"  = "big",
      "15Mil" = "big",

      "Government Case" = "gov/self/com",
      "Self-insured" = "gov/self/com",
      "Commercial" = "gov/self/com",

      "No Hit" = "no hit/coverage",
      "No Coverage" = "no hit/coverage",

      .default = "other",
      .misssing = "missing"
    ),

    policy_limitR = ifelse(is.na(policy_limitR), "missing", policy_limitR),

    policy_limitR2 = policy_limit  %>% recode(
      "10/20" = "15-20K",
      "15/30" = "15-20K",
      "20/40" = "15-20K",

      "25/50" = "25-30K",
      "25/100" = "25-30K",

      "30/60" = "25-30K",

      "60" = "40-90K",
      "$45,000/$90,000" = "40-90K",
      "50/100" = "40-90K",
      "50" = "40-90K",
      "$85,000" = "40-90K",

      "100/300" = "100-700K",
      "100/200" = "100-700K",
      "100" = "100-700K",
      "150" = "100-700K",

      "250/500" = "100-700K",
      "250" = "100-700K",
      "200" = "100-700K",

      "300" = "100-700K",
      "300/300" = "100-700K",

      "500" = "100-700K",
      "500/1mil" = "100-700K",
      "600" = "100-700K",

      "750" = "big",
      "1mil" = "big",
      "1mil/10mil" = "big",
      "1mil/3mil" = "big",
      "1mil/2mil" = "big",
      "1.25mil" = "big",
      "1.5mil" = "big",
      "2mil" = "big",
      "2.5mil" = "big",
      "3mil" = "big",
      "4mil" = "big",
      "5mil+" = "big",
      "5mil" = "big",
      "10mil" = "big",
      "15 Mil"  = "big",
      "15Mil" = "big",

      "Government Case" = "gov/self/com",
      "Self-insured" = "gov/self/com",
      "Commercial" = "gov/self/com",

      "No Hit" = "no hit/coverage",
      "No Coverage" = "no hit/coverage",
      .default = "other",
      .misssing = "missing"
    ),

    policy_limitR2 = ifelse(is.na(policy_limitR2), "missing", policy_limitR2),

    case_severityR = case_severity  %>% recode(
      "Catastrophic Injury" = "catastrophic/death",
      "Death" = "catastrophic/death",

      "Soft Tissue Injury" = "soft tissue",
      "Substantial Injury" = "substantial",
      "Substantial Injury Major" = "substantial",
      "Substantial Injury Minor" = "substantial",
      .default = "other",
      .misssing = "missing"
    ),

    case_severityR = ifelse(is.na(case_severityR), "missing", case_severityR),

    case_typeR = case_type  %>% recode(
      "Automobile Accident" = "auto",
      "Dog Bite" = "dog",
      "Premises Liability" = "premises",
      "Wrongful Death" = "death",
      .default = "other",
      .misssing = "missing"
    ),

    case_typeR = ifelse(is.na(case_typeR), "missing", case_typeR)
  )

  ######################


  results <- list(
    df = df,
    summaries = list(),
    functions = list(
      drop_summary_month = drop_summary_month,
      drop_summary_year = drop_summary_year,
      drop_summary = drop_summary,
      to_total = to_total
    )
  )


  ######################

  # df %>% filter(practice_area %in% c("PI-Litigation", "PI-Pre-Litigation")) %>% select(case_typeR) %>% table

  # df %>% filter(practice_area %in% c("PI-Litigation", "PI-Pre-Litigation")) %>% select(case_severityR) %>% table

  # ALL PI

  output_filter <- rlang::quo(
      signed_month >= start_date &
      practice_area %in% c("PI-Litigation", "PI-Pre-Litigation")
  )

  results[["summaries"]][["pi_drop_lead"]] <- df %>% filter({{output_filter}} & lead_case) %>% drop_summary()
  results[["summaries"]][["pi_drop_lead_policy_limitR"]] <- df %>% filter({{output_filter}} & lead_case) %>% drop_summary(policy_limitR) %>% to_total(results[["summaries"]][["pi_drop_lead"]])
  results[["summaries"]][["pi_drop_lead_policy_limitR2"]] <- df %>% filter({{output_filter}} & lead_case) %>% drop_summary(policy_limitR2) %>% to_total(results[["summaries"]][["pi_drop_lead"]])
  results[["summaries"]][["pi_drop_lead_case_severityR"]] <- df %>% filter({{output_filter}} & lead_case) %>% drop_summary(case_severityR) %>% to_total(results[["summaries"]][["pi_drop_lead"]])
  results[["summaries"]][["pi_drop_lead_case_typeR"]] <- df %>% filter({{output_filter}} & lead_case) %>% drop_summary(case_typeR) %>% to_total(results[["summaries"]][["pi_drop_lead"]])

  results[["summaries"]][["pi_drop_all"]] <- df %>% filter({{output_filter}}) %>% drop_summary()
  results[["summaries"]][["pi_drop_all_policy_limitR"]] <- df %>% filter({{output_filter}}) %>% drop_summary(policy_limitR) %>% to_total(results[["summaries"]][["pi_drop_all"]])
  results[["summaries"]][["pi_drop_all_policy_limitR2"]] <- df %>% filter({{output_filter}}) %>% drop_summary(policy_limitR2) %>% to_total(results[["summaries"]][["pi_drop_all"]])
  results[["summaries"]][["pi_drop_all_case_severityR"]] <- df %>% filter({{output_filter}}) %>% drop_summary(case_severityR) %>% to_total(results[["summaries"]][["pi_drop_all"]])
  results[["summaries"]][["pi_drop_all_case_typeR"]] <- df %>% filter({{output_filter}}) %>% drop_summary(case_typeR) %>% to_total(results[["summaries"]][["pi_drop_all"]])


  # PI PRE-LIT

  output_filter <- rlang::quo(
    signed_month >= start_date &
      practice_area == "PI-Pre-Litigation"
  )

  results[["summaries"]][["pi_prelit_drop_lead"]] <- df %>% filter({{output_filter}} & lead_case) %>% drop_summary()
  results[["summaries"]][["pi_prelit_drop_lead_policy_limitR"]] <- df %>% filter({{output_filter}} & lead_case) %>% drop_summary(policy_limitR) %>% to_total(results[["summaries"]][["pi_prelit_drop_lead"]])
  results[["summaries"]][["pi_prelit_drop_lead_policy_limitR2"]] <- df %>% filter({{output_filter}} & lead_case) %>% drop_summary(policy_limitR2) %>% to_total(results[["summaries"]][["pi_prelit_drop_lead"]])
  results[["summaries"]][["pi_prelit_drop_lead_case_severityR"]] <- df %>% filter({{output_filter}} & lead_case) %>% drop_summary(case_severityR) %>% to_total(results[["summaries"]][["pi_prelit_drop_lead"]])
  results[["summaries"]][["pi_prelit_drop_lead_case_typeR"]] <- df %>% filter({{output_filter}} & lead_case) %>% drop_summary(case_typeR) %>% to_total(results[["summaries"]][["pi_prelit_drop_lead"]])

  results[["summaries"]][["pi_prelit_drop_all"]] <- df %>% filter({{output_filter}}) %>% drop_summary()
  results[["summaries"]][["pi_prelit_drop_all_policy_limitR"]] <- df %>% filter({{output_filter}}) %>% drop_summary(policy_limitR) %>% to_total(results[["summaries"]][["pi_prelit_drop_all"]])
  results[["summaries"]][["pi_prelit_drop_all_policy_limitR2"]] <- df %>% filter({{output_filter}}) %>% drop_summary(policy_limitR2) %>% to_total(results[["summaries"]][["pi_prelit_drop_all"]])
  results[["summaries"]][["pi_prelit_drop_all_case_severityR"]] <- df %>% filter({{output_filter}}) %>% drop_summary(case_severityR) %>% to_total(results[["summaries"]][["pi_prelit_drop_all"]])
  results[["summaries"]][["pi_prelit_drop_all_case_typeR"]] <- df %>% filter({{output_filter}}) %>% drop_summary(case_typeR) %>% to_total(results[["summaries"]][["pi_prelit_drop_all"]])


  # PI LIT

  output_filter <- rlang::quo(
    signed_month >= start_date &
      practice_area == "PI-Litigation"
  )

  results[["summaries"]][["pi_lit_drop_lead"]] <- df %>% filter({{output_filter}} & lead_case) %>% drop_summary()
  results[["summaries"]][["pi_lit_drop_lead_policy_limitR"]] <- df %>% filter({{output_filter}} & lead_case) %>% drop_summary(policy_limitR) %>% to_total(results[["summaries"]][["pi_lit_drop_lead"]])
  results[["summaries"]][["pi_lit_drop_lead_policy_limitR2"]] <- df %>% filter({{output_filter}} & lead_case) %>% drop_summary(policy_limitR2) %>% to_total(results[["summaries"]][["pi_lit_drop_lead"]])
  results[["summaries"]][["pi_lit_drop_lead_case_severityR"]] <- df %>% filter({{output_filter}} & lead_case) %>% drop_summary(case_severityR) %>% to_total(results[["summaries"]][["pi_lit_drop_lead"]])
  results[["summaries"]][["pi_lit_drop_lead_case_typeR"]] <- df %>% filter({{output_filter}} & lead_case) %>% drop_summary(case_typeR) %>% to_total(results[["summaries"]][["pi_lit_drop_lead"]])

  results[["summaries"]][["pi_lit_drop_all"]] <- df %>% filter({{output_filter}}) %>% drop_summary()
  results[["summaries"]][["pi_lit_drop_all_policy_limitR"]] <- df %>% filter({{output_filter}}) %>% drop_summary(policy_limitR) %>% to_total(results[["summaries"]][["pi_prelit_drop_all"]])
  results[["summaries"]][["pi_lit_drop_all_policy_limitR2"]] <- df %>% filter({{output_filter}}) %>% drop_summary(policy_limitR2) %>% to_total(results[["summaries"]][["pi_prelit_drop_all"]])
  results[["summaries"]][["pi_lit_drop_all_case_severityR"]] <- df %>% filter({{output_filter}}) %>% drop_summary(case_severityR) %>% to_total(results[["summaries"]][["pi_lit_drop_all"]])
  results[["summaries"]][["pi_lit_drop_all_case_typeR"]] <- df %>% filter({{output_filter}}) %>% drop_summary(case_typeR) %>% to_total(results[["summaries"]][["pi_lit_drop_all"]])


  # EMPLOYMENT

  output_filter <- rlang::quo(
    signed_month >= start_date &
      practice_area == "Employment"
  )

  results[["summaries"]][["employment_drop_lead"]] <- df %>% filter({{output_filter}} & lead_case) %>% drop_summary()
  results[["summaries"]][["employment_drop_all"]] <- df %>% filter({{output_filter}}) %>% drop_summary()



  # CA CONSUMER

  output_filter <- rlang::quo(
    signed_month >= start_date &
      practice_area == "Class-Action-Consumer"
  )

  results[["summaries"]][["ca_consumer_drop_lead"]] <- df %>% filter({{output_filter}} & lead_case) %>% drop_summary()
  results[["summaries"]][["ca_consumer_drop_all"]] <- df %>% filter({{output_filter}}) %>% drop_summary()


  # W&H

  output_filter <- rlang::quo(
    signed_month >= start_date &
      practice_area == "Class-Action-W&H"
  )

  results[["summaries"]][["ca_w&h_drop_lead"]] <- df %>% filter({{output_filter}} & lead_case) %>% drop_summary()
  results[["summaries"]][["ca_w&h_drop_all"]] <- df %>% filter({{output_filter}}) %>% drop_summary()





  if( add_lead_gen_source ){

    output_filter <- rlang::quo(
      signed_month >= start_date &
        practice_area %in% c("PI-Litigation", "PI-Pre-Litigation") &
        !is.na(lead_gen_source)
    )

    results[["summaries"]][["lead_gen_source_lead"]] <-
      df %>%
      filter(
        {{output_filter}} & lead_case
      ) %>%
      drop_summary(lead_gen_source, .by_month = FALSE) %>%
      arrange(signed_month) %>%
      tidyr::pivot_wider(
        names_from = "signed_month",
        values_from = c("cases_signed", "drops_signed_year", "drops_drop_year"),
        id_cols = "lead_gen_source",
        names_vary = "slowest"
      ) %>%
      arrange(lead_gen_source)



    results[["summaries"]][["lead_gen_source_all"]] <-
      df %>%
      filter(
        {{output_filter}}
      ) %>%
      drop_summary(lead_gen_source, .by_month = FALSE) %>%
      arrange(signed_month) %>%
      tidyr::pivot_wider(
        names_from = "signed_month",
        values_from = c("cases_signed", "drops_signed_year", "drops_drop_year"),
        id_cols = "lead_gen_source",
        names_vary = "slowest"
      ) %>%
      arrange(lead_gen_source)



    results[["summaries"]][["lead_gen_and_sub_source_lead"]] <-
      df %>%
      filter(
        {{output_filter}} & lead_case
      ) %>%
      drop_summary(lead_gen_source, sub_lead_gen, .by_month = FALSE) %>%
      arrange(signed_month) %>%
      tidyr::pivot_wider(
        names_from = "signed_month",
        values_from = c("cases_signed", "drops_signed_year", "drops_drop_year"),
        id_cols = c("lead_gen_source", "sub_lead_gen"),
        names_vary = "slowest"
      ) %>%
      arrange(lead_gen_source, sub_lead_gen)



    results[["summaries"]][["lead_gen_and_sub_source_all"]] <-
      df %>%
      filter(
        {{output_filter}}
      ) %>%
      drop_summary(lead_gen_source, sub_lead_gen, .by_month = FALSE) %>%
      arrange(signed_month) %>%
      tidyr::pivot_wider(
        names_from = "signed_month",
        values_from = c("cases_signed", "drops_signed_year", "drops_drop_year"),
        id_cols = c("lead_gen_source", "sub_lead_gen"),
        names_vary = "slowest"
      ) %>%
      arrange(lead_gen_source, sub_lead_gen)



    results[["summaries"]][["lead_gen_and_sub_source_type_lead"]] <-
      df %>%
      filter(
        {{output_filter}} & lead_case
      ) %>%
      drop_summary(lead_gen_source, sub_lead_gen, case_type, .by_month = FALSE) %>%
      arrange(signed_month) %>%
      tidyr::pivot_wider(
        names_from = "signed_month",
        values_from = c("cases_signed", "drops_signed_year", "drops_drop_year"),
        id_cols = c("lead_gen_source", "sub_lead_gen", "case_type"),
        names_vary = "slowest"
      ) %>%
      arrange(lead_gen_source, sub_lead_gen, case_type)



    results[["summaries"]][["lead_gen_and_sub_source_type_all"]] <-
      df %>%
      filter(
        {{output_filter}}
      ) %>%
      drop_summary(lead_gen_source, sub_lead_gen, case_type, .by_month = FALSE) %>%
      arrange(signed_month) %>%
      tidyr::pivot_wider(
        names_from = "signed_month",
        values_from = c("cases_signed", "drops_signed_year", "drops_drop_year"),
        id_cols = c("lead_gen_source", "sub_lead_gen", "case_type"),
        names_vary = "slowest"
      ) %>%
      arrange(lead_gen_source, sub_lead_gen, case_type)
  }



  return(results)
}

