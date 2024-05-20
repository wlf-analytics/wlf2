#' lit_recode_status
#' @description recode statsu
#' @export
lit_recode_status <- function(x){

  x <- x %>% rename_col_do_check("status", "litify_pm__Status__c")

  x %>%
    mutate(

      status = status %>% recode(
        "Dropped (No Lien)" = "dropped",
        "Dropped" = "dropped",
        "Dropped (Lien )" = "dropped",
        "Dropped In Pro Per (Lien)" = "dropped",
        "Dropped In Pro Per (No Lien)"  = "dropped",
        "Dropped (Lien)"  = "dropped",
        "Dropped MTBR (Lien)"  = "dropped",
        "Dropped MTBR (No Lien)"  = "dropped",

        "Pending Drop (Lien)" = "pending_drop",
        "Pending Drop (No Lien)" = "pending_drop",
        "Pending Drop In Pro Per (Lien)" = "pending_drop",
        "Pending Drop In Pro Per (No Lien) " = "pending_drop",
        "Pending Drop MTBR (Lien)" = "pending_drop",
        "Pending Drop" = "pending_drop",
        "Pending Drop In Pro Per (No Lien)" = "pending_drop",
        "Pending Drop MTBR (No Lien)" = "pending_drop",

        "Sub Out (Lien)" = "subout",
        "Sub Out (No Lien)" = "subout",
        "Subout" = "subout",

        "Pending Sub Out" = "pending_subout",
        "Pending Sub Out (Lien)" = "pending_subout",
        "Pending Sub Out (No Lien)" = "pending_subout",

        "Referral Initiated" = "referral_initiated",
        "Referral Rejected"  = "referral_rejected",
        "Referral Requested" = "referral_requested",
        "Referred Out" = "referred_out",
        "Settled" = "settled",
        "Liens" = "liens",

        "Litigation" = "litigation",
        "Pre-Litigation" = "pre-litigation",
        "Attorney Review" = "attorney_review",
        "Closed" = "closed"
      )

    )
}
