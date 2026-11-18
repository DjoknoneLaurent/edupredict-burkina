# EduPredict, Défi 1 : données du tableau de bord (lecture des sorties du pipeline, aucune valeur recalculée ni imputée).
# Toutes les jointures géographiques se font sur le code province (BFxxxx, limites COD-AB), jamais sur les noms.
suppressPackageStartupMessages({
  library(shiny); library(bslib); library(plotly); library(DT); library(jsonlite); library(dplyr); library(tidyr); library(htmltools)
})
invisible(Sys.setlocale("LC_CTYPE", if (.Platform$OS.type == "windows") "French_France.utf8" else "C.UTF-8"))

# Sur le PC, les données sont lues dans les dossiers du projet ; en ligne, dans le sous-dossier « donnees » copié avec l'application.
RACINE <- if (dir.exists(file.path(getwd(), "donnees"))) normalizePath(file.path(getwd(), "donnees")) else normalizePath(Sys.getenv("DCB26_RACINE", file.path(getwd(), "..", "..")), mustWork = TRUE)
chemin <- function(...) file.path(RACINE, ...)
M <- function(nom) chemin("04_analyse", "03_modelisation", paste0("DCB26_04-analyse_D1_scientist_", nom, ".csv"))
R2 <- function(nom) chemin("04_analyse", "02_analyses_multidimensionnelles", "resultats", paste0("DCB26_04-analyse_D1_scientist_", nom, ".csv"))
lire <- function(f, sep = ",") {
  if (!file.exists(f)) { warning("Fichier absent : ", basename(f)); return(NULL) }
  as.data.frame(readr::read_delim(f, delim = sep, locale = readr::locale(encoding = "UTF-8", decimal_mark = "."), show_col_types = FALSE,
                                  guess_max = 10000, name_repair = "minimal", progress = FALSE))
}
sans_accent <- function(x) toupper(iconv(x, from = "UTF-8", to = "ASCII//TRANSLIT"))

# Couleurs du drapeau du Burkina Faso : en-tête, bandeaux et éléments fixes de l'interface
DRAP_ROUGE <- "#EF2B2D"; DRAP_VERT <- "#009E49"; DRAP_JAUNE <- "#FCD116"
# Couleurs des données : tons de terre, de savane et d'indigo du Faso Dan Fani, contrastés et lisibles
ROUGE <- "#A8322A"; ORANGE <- "#C9692A"; VERT <- "#3F8A5F"; BLEU <- "#0B5D3B"; BLEU2 <- "#2F5D7C"; GRIS <- "#9A968C"
ENCRE <- "#1F2A24"; SABLE <- "#F6F3EC"
SEQ_RISQUE <- c("#FBF6EC", "#F3D9A8", "#E3A15A", "#BE5A2C", "#7A2418")       # du plus faible au plus fort
DIV_ECART <- c("#2F5D7C", "#A9C1D3", "#F7F3EA", "#E8B08A", "#A8322A")       # plus faible / moyenne / plus fort
SEQ_OUVERT <- c("#7A2418", "#BE5A2C", "#F3E4C4", "#8DBB9C", "#2F6B48")      # 30 % ... 70 % (sable) ... 110 %
COUL_STATUT <- c("Crise" = ROUGE, "Vigilance" = ORANGE, "Normal" = VERT)

# ---------------------------------------------------------------- base province x année
base <- lire(chemin("03_transformation", "01_bases_etude", "DCB26_03-transformation_D1_architect_base-analyse-province-annee_v1.csv"), ";")
ecoles <- lire(R2("acp-series-harmonisees-achevement-ecoles_v1"), ";")
REF <- base %>% distinct(code = code_province_old, province_norm, province = province_old, region = region_old) %>% arrange(province)
stopifnot(nrow(REF) == 45, !anyDuplicated(REF$code))
code_de_norm <- setNames(REF$code, REF$province_norm)
code_de_nom <- setNames(REF$code, sans_accent(REF$province))
code_de <- function(x) { k <- sans_accent(x); out <- unname(code_de_norm[k]); i <- is.na(out); out[i] <- unname(code_de_nom[k[i]]); out }   # nom de fichier -> code BFxxxx

CIBLES <- list(
  "Primaire" = c(Filles = "taux_abandon_cycle_F", "Garçons" = "taux_abandon_cycle_G", Ensemble = "taux_abandon_cycle_T"),
  "Post-primaire" = c(Filles = "pp_abandon_moy_6e_4e_F", "Garçons" = "pp_abandon_moy_6e_4e_G", Ensemble = "pp_abandon_moy_6e_4e_T"),
  "Secondaire" = c(Filles = "sec_abandon_moy_2nde_1ere_F", "Garçons" = "sec_abandon_moy_2nde_1ere_G", Ensemble = "sec_abandon_moy_2nde_1ere_T"))
CYCLES <- names(CIBLES)

panel <- base %>%
  transmute(code = code_province_old, annee = annee_scolaire, effectif = effectif_total_T, eleves_maitre = ratio_eleves_maitre,
            !!!setNames(lapply(unlist(CIBLES), as.name), unlist(CIBLES))) %>%
  left_join(ecoles %>% transmute(code = code_de_norm[Code_province], annee = Annee, ouverture = indice_maintien_harmonise), by = c("code", "annee"))
ANNEES <- sort(unique(panel$annee))
ANNEES_ABANDON <- sort(unique(panel$annee[!is.na(panel$taux_abandon_cycle_T)]))   # 2016-2017 à 2023-2024 : 2024-2025 n'est pas observée

# Statut d'alerte (seuil validé : 70 % d'écoles ouvertes ; vigilance : baisse de plus de 5 points en un an)
statut_annee <- function(a) {
  i <- match(a, ANNEES); o <- panel %>% filter(annee == a) %>% select(code, ouverture)
  o0 <- if (i > 1) panel %>% filter(annee == ANNEES[i - 1]) %>% select(code, o0 = ouverture) else tibble(code = o$code, o0 = NA)
  o %>% left_join(o0, by = "code") %>%
    mutate(statut = case_when(is.na(ouverture) ~ NA_character_, ouverture < 70 ~ "Crise", !is.na(o0) & ouverture - o0 < -5 ~ "Vigilance", TRUE ~ "Normal"))
}
PROV_CRISE <- unique(panel$code[!is.na(panel$ouverture) & panel$ouverture < 70])          # provinces passées sous 70 % au moins une fois (16)

# ---------------------------------------------------------------- sorties d'analyse
em <- lire(M("datamining-enfants-manquants_v1")) %>% mutate(code = code_de(province_norm), manquants = pmax(eleves_manquants_avec_croissance, 0))
dec <- lire(M("datamining-decomposition-enfants-manquants_v1")) %>% mutate(code = code_de(Province))
cah <- lire(M("classes-cah-provinces_v1")) %>% transmute(code = code_province, classe_cah, ive_structurel = IVE, risque_ive = Risque)
ive <- lire(M("ive-provinces-annees_v1")) %>% transmute(code = code_province, annee = annee_scolaire, ive = ive_annuel, ive_type = type_valeur)
lisa <- lire(R2("acp-autocorrelation-spatiale_v1"), ";") %>% transmute(code = code_province_old, lisa = LISA, lisa_robuste = Robuste_BH)
prev <- lire(M("previsions-2025-2026_v1"))
proj <- lire(M("projections-2026-2030_v1"))
couv <- lire(M("modelisation-phase6-couverture-intervalles_v1"))
retenus <- lire(M("modelisation-phase4-modeles-retenus_v1"))
cons <- lire(M("consensus-causes_v1"))
shap <- lire(M("modelisation-shap-post-primaire-filles_v1"))
fiches <- lire(M("datamining-fiches-actions_v5")) %>% mutate(code = code_de(Province))
survie <- lire(M("datamining-survie-temps-bascule_v1")) %>% mutate(code = code_de(province))
signaux <- lire(M("datamining-signaux-precurseurs_v1"))
resil <- lire(M("datamining-provinces-resilientes_v1")) %>% mutate(code = code_de(province_norm))
cas <- lire(M("datamining-cas-ecole-resilience_v1"))
regles <- lire(M("datamining-synthese_v1"))
seuils <- lire(M("datamining-validation-seuils_v1"))
cact <- lire(M("datamining-cout-action_v3"))
cinact <- lire(M("datamining-cout-total-inaction_v1"))
scen <- lire(M("datamining-scenario-contrefactuel_v1"))
optim <- lire(M("datamining-optimisation-synthese_v2"))
budget <- lire(M("datamining-optimisation-budget_v2")) %>% mutate(code = code_de(Province))
ens <- lire(M("datamining-enseignants-surplus-penurie_v1")) %>% mutate(code = code_de(Province))
cal <- lire(M("datamining-calendrier-execution_v3"))
demande <- lire(M("datamining-demande-menages_v1"))
versions <- lire(M("modelisation-versions-execution_v1"))
stopifnot(!anyNA(fiches$code), !anyNA(em$code), !anyNA(dec$code), !anyNA(survie$code), !anyNA(budget$code), !anyNA(ens$code), !anyNA(resil$code))

# ---------------------------------------------------------------- géométrie (COD-AB, 45 provinces)
geo <- fromJSON(chemin("03_transformation", "05_referentiels", "DCB26_03-transformation_D1_architect_provinces-45-geometrie-cartographie_v1.geojson"), simplifyVector = FALSE)
anneaux <- lapply(geo$features, function(f) {
  g <- f$geometry
  polys <- if (g$type == "Polygon") list(g$coordinates) else g$coordinates
  lapply(polys, function(p) { m <- do.call(rbind, lapply(p[[1]], unlist)); m[, 1:2, drop = FALSE] })
})
names(anneaux) <- vapply(geo$features, function(f) f$properties$code_province_old, "")
CENTRES <- do.call(rbind, lapply(geo$features, function(f) data.frame(code = f$properties$code_province_old, lon = f$properties$centre_lon, lat = f$properties$centre_lat)))
stopifnot(setequal(names(anneaux), REF$code))
CENTRES_REG <- CENTRES %>% mutate(region = REF$region[match(code, REF$code)]) %>% group_by(region) %>% summarise(lon = mean(lon), lat = mean(lat), .groups = "drop")

# ---------------------------------------------------------------- outils de mise en forme
nf <- function(x, d = 0) ifelse(is.na(x), "n.d.", formatC(x, format = "f", digits = d, big.mark = " ", decimal.mark = ","))
pct <- function(x, d = 1) ifelse(is.na(x), "n.d.", paste0(nf(x, d), " %"))
nom <- function(code) REF$province[match(code, REF$code)]
moy_ponderee <- function(x, w) { ok <- !is.na(x) & !is.na(w); if (!any(ok)) NA_real_ else sum(x[ok] * w[ok]) / sum(w[ok]) }

DT_FR <- list(search = "Rechercher :", lengthMenu = "Afficher _MENU_ lignes", info = "Lignes _START_ à _END_ sur _TOTAL_", infoEmpty = "Aucune ligne",
              zeroRecords = "Aucun résultat", paginate = list(previous = "Précédent", `next` = "Suivant"), emptyTable = "Aucune donnée")
tableau <- function(df, pageLength = 10, export = FALSE, ...) {       # export : boutons Shiny natifs (voir bouton_export), pas d'extension DT
  datatable(df, rownames = FALSE, style = "bootstrap4", class = "table table-sm table-hover",
            options = list(pageLength = pageLength, dom = if (nrow(df) > pageLength) "ftp" else "t", language = DT_FR, scrollX = TRUE, autoWidth = FALSE), ...)
}
bouton_export <- function(id) downloadButton(id, "CSV", class = "btn-sm btn-outline-secondary", icon = icon("download"))
export_csv <- function(f_donnees, nom) downloadHandler(filename = function() paste0("DCB26_", nom, ".csv"),
                                                       content = function(file) readr::write_excel_csv2(f_donnees(), file))
note <- function(...) div(class = "note-lecture", icon("circle-info"), span(...))

LIB_VAR <- c(pp_abandon_moy_6e_4e_G = "Abandon des garçons au post-primaire (année précédente)", pp_abandon_moy_6e_4e_F = "Abandon des filles au post-primaire (année précédente)",
             tbs_pp_f_publie = "TBS post-primaire des filles", ive_annuel = "Indice de vulnérabilité éducative", pp_redoublement_moy_6e_4e_T = "Redoublement au post-primaire",
             ipm2019_ipm_incidence = "Pauvreté multidimensionnelle 2019", pct_cantines = "Part des écoles avec cantine", taux_redoublement_cycle_F = "Redoublement des filles au primaire",
             indice_maintien_harmonise = "Écoles ouvertes (%)", tach_harmonise_gf = "Achèvement du primaire", ips_tbs_harmonise = "Parité filles/garçons du TBS",
             ratio_eleves_maitre = "Élèves par maître", tbs_harmonise_gf = "TBS primaire", tbs_sec_f_publie = "TBS secondaire des filles",
             reg_fies2021_insecurite_alim_moderee_severe = "Insécurité alimentaire (région)", reg_ehcvm2021_alphab_adultes = "Alphabétisation des adultes (région)")

# ---------------------------------------------------------------- facteurs propres à chaque territoire (modèle explicatif)
# Facteurs : associations robustes du consensus de cinq méthodes (au moins deux méthodes, même sens pour les deux sexes, années calmes et de crise).
# Poids : coefficient standardisé du modèle Ridge (bootstrap de 200 tirages de provinces), par cible.
# Contribution d'un facteur pour une province = coefficient x écart standardisé de la province à la moyenne de l'échantillon d'apprentissage,
# en points d'abandon au-dessus (+) ou au-dessous (-) d'une province moyenne. Entrées : valeurs observées en 2023-2024 (ligne 2024-2025 du jeu du modèle).
dfm <- lire(M("modelisation-df-model_v1"))
ridge <- lire(M("coefficients-ridge_v1"))
CIBLE_Y <- c("Primaire|Filles" = "taux_abandon_cycle_F", "Primaire|Garçons" = "taux_abandon_cycle_G", "Post-primaire|Filles" = "pp_abandon_moy_6e_4e_F",
             "Post-primaire|Garçons" = "pp_abandon_moy_6e_4e_G", "Secondaire|Filles" = "sec_abandon_moy_2nde_1ere_F", "Secondaire|Garçons" = "sec_abandon_moy_2nde_1ere_G")
FACT_ROB <- cons %>% filter(consensus == "Association robuste") %>% select(cycle, sexe, facteur, sens, nb_methodes) %>%
  inner_join(ridge %>% select(cycle, sexe, facteur, variable, beta = coefficient_standardise, ic_bas = ic95_bas, ic_haut = ic95_haut), by = c("cycle", "sexe", "facteur"))
contrib <- bind_rows(lapply(seq_len(nrow(FACT_ROB)), function(i) {
  f <- FACT_ROB[i, ]; y <- paste0("Y__", CIBLE_Y[[paste(f$cycle, f$sexe, sep = "|")]])
  appr <- dfm[!is.na(dfm[[y]]) & !is.na(dfm[[f$variable]]), f$variable]               # échantillon d'apprentissage de la cible
  mu <- mean(appr); sd0 <- sqrt(mean((appr - mu)^2))                                    # standardisation du modèle (écart-type de population)
  d <- dfm[dfm$annee_scolaire == "2024-2025", c("code_province_old", f$variable)]
  data.frame(code = d$code_province_old, cycle = f$cycle, sexe = f$sexe, facteur = f$facteur, sens = f$sens, nb_methodes = f$nb_methodes,
             valeur = d[[f$variable]], moyenne = mu, z = (d[[f$variable]] - mu) / sd0, beta = f$beta)
})) %>% mutate(contribution = beta * z, region = REF$region[match(code, REF$code)])
stopifnot(all(contrib$code %in% REF$code), nrow(contrib) == 45 * nrow(FACT_ROB))
LIB_COURT <- c("Pauvreté multidimensionnelle 2019" = "Pauvreté des ménages", "TBS primaire" = "Enfants scolarisés au primaire",
               "TBS post-primaire (sexe de la cible)" = "Jeunes scolarisés au post-primaire", "TBS secondaire (sexe de la cible)" = "Jeunes scolarisés au secondaire",
               "Achèvement du primaire" = "Élèves qui terminent le primaire", "Parité filles/garçons du TBS" = "Équilibre filles/garçons à l'école")
UNITE <- c("Pauvreté multidimensionnelle 2019" = " %", "Parité filles/garçons du TBS" = "")
facteurs_territoire <- function(cy, sx, niveau = c("province", "region")) {
  niveau <- match.arg(niveau); sxs <- if (sx == "Ensemble") c("Filles", "Garçons") else sx
  d <- contrib %>% filter(cycle == cy, sexe %in% sxs) %>% group_by(code, region, facteur, sens) %>%
    summarise(valeur = mean(valeur), moyenne = mean(moyenne), contribution = mean(contribution), .groups = "drop")
  if (niveau == "region") d <- d %>% group_by(region, facteur, sens) %>% summarise(valeur = mean(valeur, na.rm = TRUE), moyenne = mean(moyenne), contribution = mean(contribution, na.rm = TRUE), .groups = "drop")
  d
}
texte_facteurs <- function(d) {            # d : facteurs d'un territoire ; renvoie un bloc HTML pour l'infobulle
  d <- d[order(-d$contribution), ]
  dig <- ifelse(d$facteur == "Parité filles/garçons du TBS", 2, 0)
  lignes <- sprintf("%s %s : %s%s (moyenne nationale %s) → %s pt", ifelse(d$contribution >= 0, "▲", "▼"), LIB_COURT[d$facteur],
                    mapply(nf, d$valeur, dig), ifelse(is.na(UNITE[d$facteur]), " %", UNITE[d$facteur]), mapply(nf, d$moyenne, dig), sub("\\.", ",", sprintf("%+.1f", d$contribution)))
  paste0("<b>Ce qui pèse sur l'abandon ici</b> (▲ aggrave, ▼ atténue) :<br>", paste(lignes, collapse = "<br>"),
         "<br><b>Au total : ", sub("\\.", ",", sprintf("%+.1f", sum(d$contribution, na.rm = TRUE))), " points d'abandon</b> par rapport à une province moyenne")
}

# Libellés en langage courant
LIB_LISA <- c("Haut-Haut" = "au cœur d'une zone de provinces vulnérables", "Bas-Bas" = "au sein d'une zone de provinces peu vulnérables",
              "Haut-Bas" = "province vulnérable entourée de provinces moins vulnérables", "Bas-Haut" = "province moins vulnérable entourée de provinces vulnérables",
              "Non significatif" = "pas de regroupement géographique marqué")
LIB_LIEN <- c("Association robuste" = "Lien confirmé", "Association variable selon la période" = "Lien variable selon les années",
              "Association dépendante du modèle" = "Lien incertain", "Instable ou non confirmée" = "Lien non confirmé")
LIB_FACT_LONG <- function(f) { x <- gsub("TBS", "taux de scolarisation", f); x <- gsub("\\(sexe de la cible\\)", "(même sexe)", x); sub("^(.)", "\\U\\1", x, perl = TRUE) }

# ---------------------------------------------------------------- interprétation en langage courant (pour les décideurs)
NIVEAUX <- c("Primaire (CP1 à CM2)" = "Primaire", "Post-primaire (collège, 6e à 3e)" = "Post-primaire", "Secondaire (lycée, 2nde à Terminale)" = "Secondaire")
PUBLICS <- c("Filles et garçons" = "Ensemble", "Filles" = "Filles", "Garçons" = "Garçons")
nom_niveau <- function(cy) c("Primaire" = "au primaire", "Post-primaire" = "au collège (post-primaire)", "Secondaire" = "au lycée (secondaire)")[[cy]]
nom_public <- function(sx) c("Ensemble" = "pour les filles et les garçons", "Filles" = "pour les filles", "Garçons" = "pour les garçons")[[sx]]
# Phrase décrivant chaque facteur, avec la valeur du territoire et la moyenne nationale
phrase_facteur <- function(f, v, m) {
  r <- function(x) nf(x, 0)
  switch(f,
    "Pauvreté multidimensionnelle 2019" = sprintf("Pauvreté des familles : %s %% des habitants vivent dans la pauvreté, contre %s %% en moyenne nationale", r(v), r(m)),
    "TBS primaire" = sprintf("Accès à l'école primaire : %s enfants inscrits pour 100 enfants en âge d'y être, contre %s en moyenne%s", r(v), r(m), if (!is.na(v) && v > 100) " (plus de 100 car des enfants plus âgés sont aussi inscrits)" else ""),
    "TBS post-primaire (sexe de la cible)" = sprintf("Accès au collège : %s jeunes inscrits pour 100 jeunes en âge d'y être, contre %s en moyenne", r(v), r(m)),
    "TBS secondaire (sexe de la cible)" = sprintf("Accès au lycée : %s jeunes inscrits pour 100 jeunes en âge d'y être, contre %s en moyenne", r(v), r(m)),
    "Achèvement du primaire" = sprintf("Fin du primaire : %s enfants sur 100 vont jusqu'au bout du primaire, contre %s en moyenne", r(v), r(m)),
    "Parité filles/garçons du TBS" = sprintf("Place des filles à l'école : %s filles inscrites pour 100 garçons, contre %s en moyenne", r(100 * v), r(100 * m)),
    f)
}
# Traduction d'un nombre de points (pour 100 élèves) en nombre d'élèves, pour une lecture immédiate
eleves_sur_100 <- function(x) { x <- abs(x); if (x < 1) "moins d'un élève sur 100" else sprintf("environ %s élève%s sur 100", nf(x, 0), if (round(x) >= 2) "s" else "") }
QUALIF <- list("Pauvreté multidimensionnelle 2019" = c("une pauvreté plus forte qu'en moyenne", "une pauvreté plus faible qu'en moyenne"),
               "TBS primaire" = c("un accès à l'école primaire plus large qu'en moyenne", "un accès à l'école primaire plus faible qu'en moyenne"),
               "TBS post-primaire (sexe de la cible)" = c("un accès au collège plus large qu'en moyenne", "un accès au collège plus faible qu'en moyenne"),
               "TBS secondaire (sexe de la cible)" = c("un accès au lycée plus large qu'en moyenne", "un accès au lycée plus faible qu'en moyenne"),
               "Achèvement du primaire" = c("une fin du primaire plus fréquente qu'en moyenne", "une fin du primaire moins fréquente qu'en moyenne"),
               "Parité filles/garçons du TBS" = c("une place des filles à l'école plus grande qu'en moyenne", "une place des filles à l'école plus faible qu'en moyenne"))
qualif <- function(f, v, m) if (f %in% names(QUALIF) && !is.na(v) && !is.na(m)) QUALIF[[f]][if (v >= m) 1 else 2] else tolower(f)
pt <- function(x) paste0(nf(abs(x), 1), " point", if (abs(x) >= 2) "s" else "")
un_eleve_sur <- function(p) if (is.na(p)) "n.d." else if (p >= 10) sprintf("environ 1 élève sur %d", round(100 / p)) else eleves_sur_100(p)
# Piste d'action associée au facteur qui pèse le plus (formulée comme une piste, pas comme une certitude)
PISTE <- c("Pauvreté multidimensionnelle 2019" = "soutenir les familles pauvres pour qu'elles gardent leurs enfants à l'école (cantines, fournitures, appuis sociaux)",
           "TBS primaire" = "inscrire et réinscrire les enfants au primaire, en rouvrant ou en relocalisant les écoles",
           "TBS post-primaire (sexe de la cible)" = "faciliter le passage au collège : places disponibles, distance, accompagnement des familles",
           "TBS secondaire (sexe de la cible)" = "développer l'accès au lycée et l'accompagnement vers la fin du cycle",
           "Achèvement du primaire" = "aider les élèves à aller au bout du primaire : suivi des élèves fragiles, lutte contre le redoublement",
           "Parité filles/garçons du TBS" = "scolariser et maintenir davantage les filles : réinscription active, sensibilisation des familles")
pts <- function(x) sub("\\.", ",", sprintf("%+.1f", x))
interpretation_territoire <- function(d, titre, cy, sx, ouverture = NULL, rang_bascule = NA, codes = NULL, region = NULL) {
  # Texte de l'infobulle, articulé comme une interprétation de résultats : constat, « cela signifie que », facteurs de risque
  # et facteurs protecteurs chiffrés, « c'est-à-dire que », recommandation « étant donné que ».
  d <- d[order(-abs(d$contribution)), ]; tot <- sum(d$contribution, na.rm = TRUE)
  aggr <- d[d$contribution >= 0.5, ]; attn <- d[d$contribution <= -0.5, ]
  public <- paste0(nom_niveau(cy), ", ", nom_public(sx), ",")
  constat <- if (tot >= 1) sprintf("<b>Constat :</b> le risque d'abandon %s est <b>plus élevé que la moyenne nationale</b>, d'environ %s points. <b>Cela signifie que</b>, sur 100 élèves inscrits, %s de plus qu'ailleurs risquent de quitter l'école en cours d'année.",
                                   public, nf(tot, 1), sub("^environ ", "environ ", sub(" sur 100$", "", eleves_sur_100(tot))))
             else if (tot <= -1) sprintf("<b>Constat :</b> le risque d'abandon %s est <b>plus faible que la moyenne nationale</b>, d'environ %s points. <b>Cela signifie que</b>, sur 100 élèves inscrits, %s de moins qu'ailleurs risquent de quitter l'école en cours d'année.",
                                         public, nf(-tot, 1), sub(" sur 100$", "", eleves_sur_100(tot)))
             else sprintf("<b>Constat :</b> le risque d'abandon %s est <b>proche de la moyenne nationale</b>. <b>Cela signifie que</b> la province ne se distingue ni par un risque particulier, ni par une protection particulière.", public)
  crise <- !is.null(ouverture) && !is.na(ouverture) && ouverture < 70
  ouv <- if (!is.null(ouverture) && !is.na(ouverture)) {
    if (crise) sprintf("<br><b>Écoles :</b> seulement %s %% des écoles d'avant la crise fonctionnent en 2024-2025. C'est une <b>situation de crise</b> (seuil : 70 %%) : de nombreux enfants sont privés d'école et n'apparaissent plus dans le taux d'abandon.", nf(ouverture, 0))
    else if (ouverture < 80) sprintf("<br><b>Écoles :</b> %s %% des écoles d'avant la crise fonctionnent en 2024-2025, soit seulement %s points au-dessus du seuil de crise de 70 %%.", nf(ouverture, 0), nf(ouverture - 70, 0))
    else sprintf("<br><b>Écoles :</b> %s %% des écoles d'avant la crise fonctionnent en 2024-2025%s ; pas de crise d'ouverture.", nf(min(ouverture, 100), 0), if (ouverture > 100) " (de nouvelles écoles ont ouvert depuis)" else "") } else ""
  ligne <- function(x) sprintf("• %s. <b>%s %s d'abandon</b>, soit %s.", phrase_facteur(x$facteur, x$valeur, x$moyenne), if (x$contribution >= 0) "+" else "−", pt(x$contribution), eleves_sur_100(x$contribution))
  bloc_a <- if (nrow(aggr)) paste0("<br><b>Facteurs de risque</b> (ils augmentent l'abandon ici) :<br>", paste(vapply(seq_len(nrow(aggr)), function(i) ligne(aggr[i, ]), ""), collapse = "<br>")) else ""
  bloc_b <- if (nrow(attn)) paste0("<br><b>Facteurs protecteurs</b> (ils limitent l'abandon ici) :<br>", paste(vapply(seq_len(nrow(attn)), function(i) ligne(attn[i, ]), ""), collapse = "<br>")) else ""
  q1 <- function(x) qualif(x$facteur[1], x$valeur[1], x$moyenne[1])
  synthese <- if (nrow(aggr) && tot >= 1) sprintf("<br><b>C'est-à-dire que</b> l'abandon est plus élevé ici d'abord à cause d'%s.", sub("^un", "un", q1(aggr)))
              else if (nrow(attn) && tot <= -1) sprintf("<br><b>C'est-à-dire que</b> la province est plutôt protégée de l'abandon, d'abord grâce à %s.", q1(attn))
              else if (nrow(aggr) && nrow(attn)) "<br><b>C'est-à-dire que</b> facteurs de risque et facteurs protecteurs se compensent."
              else ""
  fp <- if (nrow(aggr)) list(facteur = aggr$facteur[1], valeur = aggr$valeur[1], moyenne = aggr$moyenne[1], effet = if (tot >= 1) aggr$contribution[1] else 0) else NULL
  reco <- if (is.null(codes)) "" else recommandations(codes, cy, sx, fp, region)
  paste0("<b>", titre, "</b><br>", constat, ouv, bloc_a, bloc_b, synthese, reco)
}

RANG_BASCULE <- setNames(rank(-survie$risque_annuel_bascule, ties.method = "first"), survie$code)   # provinces encore ouvertes, 1 = la plus exposée
rang_de <- function(c0) if (c0 %in% names(RANG_BASCULE)) RANG_BASCULE[[c0]] else NA

# Niveau d'alerte affiché (carte d'alerte, fiches) : la vigilance « À surveiller » s'ajoute aux statuts Crise / Vigilance / Normal
ST25 <- statut_annee("2024-2025")
LIB_ALERTE <- c(Crise = "Crise", Vigilance = "Vigilance", "À surveiller" = "À surveiller", Normal = "Normal")
COUL_ALERTE <- c(Crise = "#9E1B1B", Vigilance = "#D0662A", "À surveiller" = "#E8BD45", Normal = "#B7D3BF")   # carte : la couleur attire l'oeil vers l'urgence
BADGE_ALERTE <- c(Crise = "#9E1B1B", Vigilance = "#C2561F", "À surveiller" = "#E8BD45", Normal = "#3F8A5F")
TEXTE_BADGE <- c(Crise = "#FFFFFF", Vigilance = "#FFFFFF", "À surveiller" = "#3A2A00", Normal = "#FFFFFF")
statut_affiche <- function(st, c0) { s <- st$statut; if (!is.na(s) && s == "Normal" && (!is.na(rang_de(c0)) && rang_de(c0) <= 5 || st$ouverture < 80)) "À surveiller" else s }
source("fiche_pdf.R", local = TRUE, encoding = "UTF-8")

# ---------------------------------------------------------------- régression logistique : probabilité d'abandon et effet des facteurs
# Fichiers produits par 00_scripts/DCB26_regression_logistique_D1.py (régression logistique fractionnaire, écarts-types regroupés par province).
LG_P <- lire(M("logistique-probabilites-provinces_v1")); LG_R <- lire(M("logistique-probabilites-regions_v1"))
LG_E <- lire(M("logistique-effets-facteurs_v1")); LG_F <- lire(M("logistique-effets-facteurs-provinces_v1")); LG_C <- lire(M("logistique-criteres-succes_v1"))
LG_E$effet_etabli <- as.logical(LG_E$effet_etabli)
LG_MANQUE <- setdiff(REF$code, LG_P$code_province)   # provinces sans facteurs complets en 2023-2024
NOM_LG <- c("Pauvreté multidimensionnelle 2019" = "Pauvreté des ménages", "Parité filles/garçons du TBS" = "Équilibre filles-garçons à l'école primaire",
            "TBS secondaire (sexe de la cible)" = "Scolarisation au lycée", "Écoles ouvertes (% du niveau 2017-2019)" = "Écoles en fonctionnement",
            "Écoles avec cantine (%)" = "Écoles avec cantine", "Élèves par maître" = "Élèves par enseignant", "Alphabétisation des adultes 2021 (région)" = "Adultes sachant lire (région)",
            "Insécurité alimentaire 2021 (région)" = "Insécurité alimentaire (région)", "Redoublement primaire (sexe de la cible)" = "Redoublement au primaire",
            "Redoublement post-primaire" = "Redoublement au collège", "Redoublement secondaire" = "Redoublement au lycée")
UNITE_LG <- c("Pauvreté multidimensionnelle 2019" = "%s points de population pauvre en plus", "Parité filles/garçons du TBS" = "%s point d'indice de parité en plus (par ex. de 0,94 à 1,00)",
              "TBS secondaire (sexe de la cible)" = "%s points de scolarisation au lycée en plus", "Écoles ouvertes (% du niveau 2017-2019)" = "%s points d'écoles en fonctionnement en plus",
              "Écoles avec cantine (%)" = "%s points d'écoles avec cantine en plus", "Élèves par maître" = "%s élèves par enseignant en plus", "Alphabétisation des adultes 2021 (région)" = "%s points d'adultes sachant lire en plus",
              "Insécurité alimentaire 2021 (région)" = "%s points d'insécurité alimentaire en plus", "Redoublement primaire (sexe de la cible)" = "%s points de redoublement en plus",
              "Redoublement post-primaire" = "%s points de redoublement en plus", "Redoublement secondaire" = "%s points de redoublement en plus")
DEC_LG <- c("Parité filles/garçons du TBS" = 2, "Redoublement primaire (sexe de la cible)" = 1, "Redoublement post-primaire" = 1, "Redoublement secondaire" = 1)
hausse_lg <- function(fac, sd) sprintf(UNITE_LG[[fac]], nf(sd, if (fac %in% names(DEC_LG)) DEC_LG[[fac]] else 0))
sexes_de <- function(sx) if (sx == "Ensemble") c("Filles", "Garçons") else sx
# probabilité par province (Ensemble : moyenne filles et garçons)
lg_proba <- function(cy, sx) { d <- LG_P[LG_P$cycle == cy & LG_P$sexe %in% sexes_de(sx), ]
  v <- tapply(d$probabilite_abandon_pct, d$code_province, mean); setNames(as.numeric(v[REF$code]), REF$code) }
lg_proba_moy <- function(cy, sx) mean(LG_P$probabilite_province_moyenne_pct[LG_P$cycle == cy & LG_P$sexe %in% sexes_de(sx)])
lg_proba_region <- function(cy, sx) { d <- LG_R[LG_R$cycle == cy & LG_R$sexe %in% sexes_de(sx), ]; tapply(d$probabilite_abandon_pct, d$region, mean) }
# facteurs établis pour ce cycle (au moins un sexe concerné pour « Ensemble »)
lg_etablis <- function(cy, sx) unique(LG_E$facteur[LG_E$cycle == cy & LG_E$sexe %in% sexes_de(sx) & LG_E$effet_etabli])
# effet propre de chaque facteur établi dans une liste de provinces (points de probabilité ; moyenne si plusieurs)
lg_effets <- function(codes, cy, sx) {
  f <- lg_etablis(cy, sx); d <- LG_F[LG_F$code_province %in% codes & LG_F$cycle == cy & LG_F$sexe %in% sexes_de(sx) & LG_F$facteur %in% f, ]
  if (!nrow(d)) return(data.frame(facteur = character(), effet = numeric(), valeur = numeric(), moyenne = numeric()))
  a <- aggregate(cbind(effet = effet_points_probabilite, valeur = valeur_province, moyenne = moyenne_nationale) ~ facteur, data = d, FUN = mean)
  a[order(-abs(a$effet)), ] }
UNITE_VAL <- c("Pauvreté multidimensionnelle 2019" = "%s %% de la population pauvre", "Parité filles/garçons du TBS" = "indice de parité %s",
               "TBS secondaire (sexe de la cible)" = "%s %% des jeunes en âge d'aller au lycée y sont inscrits")
texte_proba <- function(titre, p, pm, eff, cy, sx, codes = NULL, region = NULL) {
  # Même articulation que les interprétations de la carte du risque : constat, « cela signifie que », facteurs de risque
  # et facteurs protecteurs chiffrés en points de pourcentage, « c'est-à-dire que ».
  ec <- p - pm
  compar <- if (abs(ec) < 1) "soit un niveau proche de la moyenne" else sprintf("soit %s points de %s que la moyenne", nf(abs(ec), 1), if (ec > 0) "plus" else "moins")
  tete <- sprintf("<b>%s</b><br><b>Constat :</b> %s, %s, la probabilité qu'un élève inscrit abandonne en cours d'année est de <b>%s</b>, contre %s pour une province aux caractéristiques moyennes. <b>Cela signifie qu'</b>%s risque de quitter l'école, %s.<br>",
                  titre, sub("^a", "a", nom_niveau(cy)), nom_public(sx), pct(p), pct(pm), un_eleve_sur(p), compar)
  if (!nrow(eff)) return(paste0(tete, "Aucun facteur n'a d'effet établi par le modèle pour ce niveau."))
  lig <- function(i) { f <- eff$facteur[i]; dv <- if (f %in% names(DEC_LG)) DEC_LG[[f]] else 0
    val <- if (f %in% names(UNITE_VAL)) sprintf(UNITE_VAL[[f]], nf(eff$valeur[i], dv)) else nf(eff$valeur[i], dv)
    suf <- c("Pauvreté multidimensionnelle 2019" = " %", "TBS secondaire (sexe de la cible)" = " %")
    sprintf("• %s : %s, contre %s%s en moyenne. <b>%s %s de pourcentage</b>, soit %s.", NOM_LG[[f]], val, nf(eff$moyenne[i], dv), if (f %in% names(suf)) suf[[f]] else "",
            if (eff$effet[i] >= 0) "+" else "−", pt(eff$effet[i]), eleves_sur_100(eff$effet[i])) }
  plus <- which(eff$effet >= 0.5); moins <- which(eff$effet <= -0.5)
  qf <- function(k) qualif(eff$facteur[k], eff$valeur[k], eff$moyenne[k])
  synth <- if (length(plus) && (!length(moins) || eff$effet[plus[1]] >= abs(eff$effet[moins[1]]))) sprintf("<b>C'est-à-dire que</b> la probabilité d'abandon est relevée ici d'abord par %s.", qf(plus[1]))
           else if (length(moins)) sprintf("<b>C'est-à-dire que</b> la probabilité d'abandon est abaissée ici d'abord par %s.", qf(moins[1]))
           else "<b>C'est-à-dire que</b> les facteurs établis sont ici proches de la moyenne nationale."
  if (ec >= 1 && !length(plus)) synth <- paste0(synth, " La probabilité reste au-dessus de la moyenne en raison d'autres caractéristiques, dont l'effet n'est pas établi séparément, et de la situation locale.")
  if (ec <= -1 && !length(moins)) synth <- paste0(synth, " La probabilité reste sous la moyenne en raison d'autres caractéristiques, dont l'effet n'est pas établi séparément, et de la situation locale.")
  paste0(tete,
         if (length(plus)) paste0("<b>Facteurs de risque</b> (ils augmentent la probabilité ici) :<br>", paste(sapply(plus, lig), collapse = "<br>"), "<br>") else "",
         if (length(moins)) paste0("<b>Facteurs protecteurs</b> (ils la diminuent ici) :<br>", paste(sapply(moins, lig), collapse = "<br>"), "<br>") else "",
         synth,
         if (is.null(codes)) "" else recommandations(codes, cy, sx, if (length(plus)) list(facteur = eff$facteur[plus[1]], valeur = eff$valeur[plus[1]], moyenne = eff$moyenne[plus[1]], effet = eff$effet[plus[1]]) else NULL, region),
         "<br><i>Liens observés entre provinces, pas des causes prouvées.</i>") }

# ---------------------------------------------------------------- recommandations affichées au survol (province ou région)
# Construites à partir des résultats : écoles en fonctionnement, exposition à une bascule, facteur de risque principal,
# écart filles-garçons observé, fiches actions. Au plus quatre recommandations, de la plus urgente à la moins urgente.
net_txt <- function(x) gsub(" : ", ", ", gsub("[\\s\\p{Z}]+(?=[,)]|$)", "", x, perl = TRUE), fixed = TRUE)
minus1 <- function(x) paste0(tolower(substr(x, 1, 1)), substring(x, 2))
recommandations <- function(codes, cy, sx, fac_principal = NULL, region = NULL) {
  st <- ST25[ST25$code %in% codes, ]; rec <- character()
  crise <- st$code[!is.na(st$ouverture) & st$ouverture < 70]
  alerte <- codes[vapply(codes, function(c0) { s <- statut_affiche(ST25[ST25$code == c0, ], c0); !is.na(s) && s %in% c("Vigilance", "À surveiller") }, TRUE)]
  dans <- function(cc) if (length(cc) == 1) paste0("dans la province de ", nom(cc)) else paste0("dans les provinces de ", paste(nom(cc), collapse = ", "))
  fi <- fiches[fiches$code %in% codes, ]
  fi <- fi[order(match(fi$Statut, c("Curatif", "Palliatif", "Préventif", "Transfert d'expérience")), fi$Priorité), ]
  detail <- function(types) { f <- fi[fi$Statut %in% types, ]; if (!nrow(f) || !is.null(region)) return("")
    sprintf(" <i>Fiche action : %s ; responsable : %s.</i>", minus1(net_txt(f$Échéance[1])), net_txt(f$`Responsable principal`[1])) }
  if (length(crise)) rec <- c(rec, if (is.null(region))
    sprintf("<b>Rouvrir les écoles sans attendre.</b> Étant donné que seulement %s %% des écoles fonctionnent, il faut rouvrir ou relocaliser les écoles fermées et offrir dès maintenant une solution d'attente (radio éducative, classes temporaires) aux enfants privés d'école.%s",
            nf(st$ouverture[st$code == crise[1]], 0), detail(c("Curatif", "Palliatif")))
    else sprintf("<b>Rouvrir les écoles en priorité</b> %s, où moins de 70 %% des écoles fonctionnent, et offrir une solution d'attente aux enfants privés d'école.", dans(crise)))
  if (length(alerte)) rec <- c(rec, if (is.null(region))
    { r <- rang_de(codes[1]); sprintf("<b>Prévenir la fermeture des écoles.</b> La province reste ouverte mais fragile%s : il faut mener une mission d'évaluation avant fin mars 2027 et tenir prêt un plan de continuité.%s",
                                     if (!is.na(r) && r <= 5) sprintf(" (rang %d des provinces les plus exposées à une bascule)", r) else "", detail("Préventif")) }
    else sprintf("<b>Prévenir la fermeture des écoles</b> %s, encore %s mais fragile%s : mission d'évaluation avant fin mars 2027 et plan de continuité prêt à déclencher.",
                 dans(alerte), if (length(alerte) == 1) "ouverte" else "ouvertes", if (length(alerte) == 1) "" else "s"))
  if (!is.null(fac_principal) && !is.na(fac_principal$effet) && fac_principal$effet >= 1 && fac_principal$facteur %in% names(PISTE))
    rec <- c(rec, sprintf("<b>Agir sur la cause principale.</b> Étant donné qu'%s pèse le plus sur l'abandon, il faut %s.", qualif(fac_principal$facteur, fac_principal$valeur, fac_principal$moyenne), PISTE[[fac_principal$facteur]]))
  ab <- panel[panel$annee == tail(ANNEES_ABANDON, 1) & panel$code %in% codes, ]
  tf <- moy_ponderee(ab[[CIBLES[[cy]][["Filles"]]]], ab$effectif); tg <- moy_ponderee(ab[[CIBLES[[cy]][["Garçons"]]]], ab$effectif)
  if (!is.na(tf) && !is.na(tg)) {
    if (tf - tg >= 2) rec <- c(rec, sprintf("<b>Retenir les filles à l'école.</b> Elles abandonnent davantage (%s contre %s chez les garçons en 2023-2024) : il faut réinscrire activement les filles et sensibiliser les familles, notamment contre le mariage précoce.", pct(tf), pct(tg)))
    else if (tg - tf >= 2) rec <- c(rec, sprintf("<b>Retenir les garçons à l'école.</b> Ils abandonnent davantage (%s contre %s chez les filles en 2023-2024) : il faut suivre individuellement les élèves en difficulté et réinscrire activement ceux qui sont partis.", pct(tg), pct(tf)))
  }
  autres <- fi[!(fi$Statut %in% c(if (length(crise)) c("Curatif", "Palliatif"), if (length(alerte)) "Préventif")), ]
  if (nrow(autres)) {
    if (is.null(region)) rec <- c(rec, sprintf(paste0(if (autres$Statut[1] == "Transfert d'expérience") "<b>Partager ce qui marche.</b>" else "<b>Mettre en œuvre la fiche action.</b>", " %s. <i>Échéance : %s ; responsable : %s.</i>"), net_txt(autres$`Action recommandée`[1]), minus1(net_txt(autres$Échéance[1])), net_txt(autres$`Responsable principal`[1])))
    else rec <- c(rec, sprintf("<b>Suivre les %d fiches actions</b> déjà établies %s (onglet « Plan d'action »).", nrow(fi), dans(unique(fi$code))))
  }
  if (!length(rec)) rec <- "<b>Maintenir le cap.</b> La situation est favorable : il suffit de suivre chaque année l'abandon, les écoles en fonctionnement et l'écart entre filles et garçons, pour réagir au premier signe de dégradation."
  rec <- head(rec, 4)
  paste0("<div class='reco'><div class='reco-titre'>Recommandations</div>", paste(sprintf("<div class='reco-item'><span class='reco-num'>%d</span><span>%s</span></div>", seq_along(rec), rec), collapse = ""), "</div>")
}
