require(magrittr)


Stacomizkia_env<-new.env()

unlockEnvironment <- function (env) {
  return (new.env(parent=env))
}

Stacomizkia_env<-unlockEnvironment(Stacomizkia_env)

#' @param \dots additional arguments passed to the function
#' @return Nothing
#' @author Sebastien Grall \email{seinormigr.grall"at"gmail.com}
clean_up_envir <- function(ignore = NULL) {

  if (exists("Stacomizkia_env")) {
    miettes <- ls(envir = Stacomizkia_env)
    if (length(miettes) > 0) {
      miettes <- miettes[!miettes %in% c("datawd", "sch", "database_expected", "db_connection", ignore)]
      rm(list = miettes, envir = Stacomizkia_env)
    }
  } else {
    stop("Internal error, envir stacomi does not exist")
  }
  return(invisible(NULL))
}

col_names_operation = c("ope_identifiant", "ope_dic_identifiant", "ope_date_debut", "ope_date_fin", "ope_organisme", "ope_operateur", "ope_commentaires", "ope_org_code")
col_names_lot = c("lot_identifiant", "lot_ope_identifiant", "lot_tax_code", "lot_std_code", "lot_effectif", "lot_quantite", "lot_qte_code",
                  "lot_methode_obtention","lot_lot_identifiant","lot_dev_code","lot_commentaires","lot_org_code")

col_names_caract_lot = c("car_lot_identifiant","car_par_code","car_methode_obtention","car_val_identifiant","car_valeur_quantitatif","car_precision",
                         "car_commentaires","car_org_code")
