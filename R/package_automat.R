# ------------------------------------------------------------
# Auteur: Ahmadou Niass
# Numéro de téléphone: +221 77 798 36 96
# Email: ahmadouniass2@gmail.com
# Date de création : 28 mai 2024
# Description   : Dans ce script on utilise des fonctions de notre package créé pour l'automatisation
#                 du traitement des bases ehcvm

# ------------------------------------------------------------
library(haven)
library(readxl)
library(dplyr)


# Fonction pour lire les données et renommer les colonnes si nécessaire
read_and_prepare_data <- function(produit_path, conversion_path, produit) {
  produit_dta <- haven::read_dta(produit_path)
  conversion_data <- readxl::read_excel(conversion_path)
  names(produit_dta)[names(produit_dta) == paste0(produit,"__id")] <- "produitID"
  names(produit_dta)[names(produit_dta) == paste0("s07Bq03b_", produit)] <- "uniteID"
  names(produit_dta)[names(produit_dta) == paste0("s07Bq03c_", produit)] <- "tailleID"
  list(produit = produit_dta, conversion_data = conversion_data)
}


# Fonction pour faire le merge des données
merge_data <- function(produit_dta, conversion_data, produit) {
  conversion_selected <- conversion_data[c("produitID", "uniteID", "tailleID", "poids")]
  names(produit_dta)[names(produit_dta) == paste0(produit,"__id")] <- "produitID"
  produit_merge <- merge(produit_dta, conversion_selected, by = c("produitID", "uniteID", "tailleID"), all.x = TRUE)
  produit_merge <- produit_merge %>% filter(!is.na(produit_merge[[paste0("s07Bq07a_", produit)]]))

  produit_merge$poids <- as.numeric(produit_merge$poids)
  produit_merge[[paste0("s07Bq03a_", produit)]] <- as.numeric(produit_merge[[paste0("s07Bq03a_", produit)]])
  produit_merge$qte_cons_kg <- (produit_merge$poids * produit_merge[[paste0("s07Bq03a_", produit)]]) / 1000

  return(produit_merge)
}


# Fonction pour traiter les données d'achats
process_achats <- function(produit_merge, conversion_data, produit) {
  achats_cols <- c("produitID", paste0("s07Bq07a_", produit), paste0("s07Bq07b_", produit), paste0("s07Bq07c_", produit), paste0("s07Bq08_", produit))
  viandes_achats <- produit_merge[achats_cols]
  colnames(viandes_achats)[2:5] <- c("qte", "uniteID", "tailleID", "Valeur")

  viandes_achats <- merge(viandes_achats, conversion_data, by = c("produitID", "uniteID", "tailleID"), all.x = TRUE)
  viandes_achats$poids_ach_g <- as.numeric(viandes_achats$qte) * as.numeric(viandes_achats$poids)
  viandes_achats$pus_en_g <- as.numeric(viandes_achats$Valeur) / as.numeric(viandes_achats$poids_ach_g)

  viande_achats_fin <- viandes_achats %>%
    select(produitID, pus_en_g) %>%
    group_by(produitID) %>%
    mutate(pus_en_g = mean(pus_en_g, na.rm = TRUE)) %>%
    distinct()

  viande_mergeds <- merge(produit_merge, viande_achats_fin, by = "produitID", all.x = TRUE)
  viande_mergeds$Valeur[is.na(viande_mergeds$Valeur)] <- viande_mergeds$pus_en_g[is.na(viande_mergeds$Valeur)] * viande_mergeds$poids[is.na(viande_mergeds$Valeur)]

  return(viande_mergeds)
}

# Fonction pour importer et merger la base EHCVM MOD
merge_ehcvm <- function(produit_merge, ehcvm_path) {
  ehcvmmod <- haven::read_dta(ehcvm_path)
  base_fin_produit <- merge(produit_merge, ehcvmmod, by = "interview__key", all.x = TRUE)
  return(base_fin_produit)
}

# Fonction pour merger avec le tableau ANSD
merge_ansd <- function(base_fin_produit, ansd_path) {
  tableau_ansd <- readxl::read_excel(ansd_path)
  names(tableau_ansd)[names(tableau_ansd) == "Region_id"] <- "s00q01"
  base_fin_produit <- merge(base_fin_produit, tableau_ansd[c("Taille moyenne des ménages ordinaires", "s00q01")], by = "s00q01", all.x = TRUE)
  names(base_fin_produit)[names(base_fin_produit) == "Taille moyenne des ménages ordinaires"] <- "Taille_moyenne_menages"

  base_fin_produit$cons_moy_pers <- base_fin_produit$qte_cons_kg / base_fin_produit$Taille_moyenne_menages
  list(base_fin_produit = base_fin_produit, tableau_ansd = tableau_ansd)
}


# Fonction pour calculer les moyennes par région et milieu
calculate_means <- function(base_fin_produit, tableau_ansd) {
  base_fin_produit_reg <- base_fin_produit %>%
    group_by(s00q01) %>%
    summarise(cons_moy_pers = mean(cons_moy_pers, na.rm = TRUE))

  base_cons_moy_reg <- merge(base_fin_produit_reg, tableau_ansd[c("Région", "s00q01")], by = "s00q01", all.x = TRUE)

  base_fin_produit_mil <- base_fin_produit %>%
    group_by(s00q04) %>%
    summarise(cons_moy_pers = mean(cons_moy_pers, na.rm = TRUE))

  list(region_means = base_cons_moy_reg, milieu_means = base_fin_produit_mil)
}
