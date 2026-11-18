# EduPredict, Défi 1 : tableau de bord d'aide à la décision contre l'abandon scolaire au Burkina Faso.
# Lancement : double-clic sur DCB26_lancer_tableau_de_bord_shiny.bat (dossier DCB26_D1), ou
#   Rscript -e "shiny::runApp('00_scripts/DCB26_tableau_de_bord_shiny_D1', launch.browser = TRUE)"
source("global.R", local = TRUE, encoding = "UTF-8")

# ============================================================================================ carte (polygones COD-AB, clic = fiche)
palette_continue <- function(v, couleurs, lo, hi) {
  f <- colorRamp(couleurs); x <- pmin(pmax((v - lo) / ifelse(hi > lo, hi - lo, 1), 0), 1)
  out <- rep("#E4E0D6", length(v)); ok <- !is.na(x); m <- f(x[ok]); out[ok] <- rgb(m[, 1], m[, 2], m[, 3], maxColorValue = 255); out
}
carte <- function(valeurs, libelle, type = c("continu", "categorie"), couleurs = SEQ_RISQUE, lo = NULL, hi = NULL,
                  categories = NULL, format_val = function(x) nf(x, 1), selection = NULL, source = "carte",
                  survol = NULL, titres = NULL, bord_interne = FALSE, consigne = "Cliquer pour afficher le détail sous la carte", echelle = TRUE, libelle_valeur = TRUE) {
  type <- match.arg(type); codes <- names(anneaux); v <- valeurs[codes]
  if (type == "continu") {
    lo <- if (is.null(lo)) min(v, na.rm = TRUE) else lo; hi <- if (is.null(hi)) max(v, na.rm = TRUE) else hi
    remp <- palette_continue(as.numeric(v), couleurs, lo, hi)
  } else remp <- ifelse(is.na(v), "#E4E0D6", unname(categories[as.character(v)]))
  p <- plot_ly(source = source)
  for (i in seq_along(codes)) {
    poly <- anneaux[[i]]; xy <- do.call(rbind, lapply(poly, function(m) rbind(m, c(NA, NA))))
    tt <- if (is.null(titres)) paste0("<b>", nom(codes[i]), "</b> (", REF$region[match(codes[i], REF$code)], ")") else titres[[codes[i]]]
    lab <- paste0(tt, if (libelle_valeur) paste0("<br>", libelle, " : ", if (type == "continu") format_val(as.numeric(v[i])) else ifelse(is.na(v[i]), "n.d.", as.character(v[i]))) else "",
                  if (!is.null(survol)) paste0(if (libelle_valeur) "<br><br>" else "", survol[[codes[i]]]) else "", if (nzchar(consigne)) paste0("<br><i>", consigne, "</i>") else "")
    p <- add_trace(p, x = xy[, 1], y = xy[, 2], type = "scatter", mode = "lines", fill = "toself", fillcolor = remp[i],
                   line = list(color = if (bord_interne) remp[i] else "white", width = if (bord_interne) 0.4 else 0.8), hoveron = "fills", text = lab, hoverinfo = "none",
                   customdata = codes[i], meta = paste0(codes[i], "|", REF$region[match(codes[i], REF$code)]), showlegend = FALSE)
  }
  if (!is.null(selection) && selection %in% codes) {
    xy <- do.call(rbind, lapply(anneaux[[selection]], function(m) rbind(m, c(NA, NA))))
    p <- add_trace(p, x = xy[, 1], y = xy[, 2], type = "scatter", mode = "lines", line = list(color = "#111111", width = 3), hoverinfo = "skip", showlegend = FALSE)
  }
  if (type == "continu" && echelle) {
    cs <- lapply(seq_along(couleurs), function(k) list((k - 1) / (length(couleurs) - 1), couleurs[k]))
    p <- add_trace(p, x = CENTRES$lon[1], y = CENTRES$lat[1], type = "scatter", mode = "markers", hoverinfo = "skip", showlegend = FALSE,
                   marker = list(size = 0.1, color = c(lo, hi), cmin = lo, cmax = hi, colorscale = cs,
                                 colorbar = list(title = list(text = libelle, font = list(size = 11, color = ENCRE), side = "right"), thickness = 12, len = 0.7, x = 1.01, xanchor = "left", outlinewidth = 0, tickfont = list(color = "#6B6A60")), showscale = TRUE, opacity = 0))
  } else {
    for (k in names(categories)) p <- add_trace(p, x = NA_real_, y = NA_real_, type = "scatter", mode = "markers", name = k, marker = list(symbol = "square", size = 12, color = categories[[k]]), hoverinfo = "skip")
  }
  layout(p, xaxis = list(visible = FALSE, scaleanchor = "y", scaleratio = 1), yaxis = list(visible = FALSE), margin = list(l = 0, r = if (type == "continu" && echelle) 78 else 0, t = 10, b = 0),
         legend = list(orientation = "h", x = 0, y = -0.02), paper_bgcolor = "rgba(0,0,0,0)", plot_bgcolor = "rgba(0,0,0,0)", dragmode = FALSE) %>%
    config(displayModeBar = FALSE, locale = "fr", doubleClick = FALSE) %>%
    htmlwidgets::onRender(sprintf("function(el, x) { window.eduCarte(el, {mode: '%s'}); }", if (grepl("region", source)) "region" else "province"))
}
style_graphe <- function(p, ytitre = NULL, xtitre = NULL) {
  layout(p, yaxis = list(title = if (is.null(ytitre)) "" else ytitre, gridcolor = "#ECEFF2", zeroline = FALSE), xaxis = list(title = if (is.null(xtitre)) "" else xtitre, gridcolor = "#F6F7F9"),
         legend = list(orientation = "h", y = -0.2), margin = list(l = 50, r = 10, t = 10, b = 40), hovermode = "x unified",
         font = list(family = "Segoe UI, Roboto, Arial", size = 12, color = ENCRE), separators = ", ", colorway = c(BLEU2, ORANGE, ROUGE, VERT),
         hoverlabel = list(bgcolor = "#FFFFFF", bordercolor = "#E6E0D3", font = list(color = ENCRE))) %>% config(displayModeBar = FALSE, locale = "fr")
}

# ============================================================================================ interface
theme <- bs_theme(version = 5, primary = BLEU, secondary = "#6B6A60", success = VERT, warning = ORANGE, danger = ROUGE, bg = "#FFFFFF", fg = ENCRE,
                  base_font = font_collection("Segoe UI", "Roboto", "Helvetica Neue", "Arial", "sans-serif"), "border-radius" = "0.5rem")
css <- "
:root { --drap-rouge: #EF2B2D; --drap-vert: #009E49; --drap-jaune: #FCD116; --encre: #1F2A24; --vert-profond: #0B5D3B; --sable: #F6F3EC; --trait: #E6E0D3; --doux: #6B6A60; }
body { background: var(--sable); color: var(--encre); }
/* en-tête aux couleurs du drapeau : bande rouge, liseré jaune, étoile */
.navbar { background: linear-gradient(90deg, #D9262A 0%, #EF2B2D 55%, #E52528 100%) !important; border-bottom: 4px solid var(--drap-vert); box-shadow: 0 2px 8px rgba(80,10,10,.18); }
.navbar .navbar-brand { color: #fff !important; font-weight: 700; letter-spacing: .2px; }
.navbar .navbar-brand .etoile { color: var(--drap-jaune); margin-right: 6px; filter: drop-shadow(0 0 1px rgba(0,0,0,.25)); }
.navbar .nav-link { color: rgba(255,255,255,.88) !important; font-weight: 500; border-bottom: 3px solid transparent; }
.navbar .nav-link:hover { color: #fff !important; }
.navbar .nav-link.active { color: #fff !important; font-weight: 700; border-bottom-color: var(--drap-jaune); }
.navbar .navbar-toggler { border-color: rgba(255,255,255,.6); }
/* bandeaux de section : vert du drapeau, étoile jaune */
.bandeau { position: relative; background: linear-gradient(100deg, #007A3D 0%, #009E49 70%, #12A955 100%); color: #fff; padding: 16px 22px 16px 58px; border-radius: .6rem; margin-bottom: 14px; box-shadow: 0 2px 6px rgba(0,60,30,.18); }
.bandeau::before { content: '\\2605'; position: absolute; left: 20px; top: 13px; font-size: 26px; color: var(--drap-jaune); text-shadow: 0 1px 2px rgba(0,0,0,.25); }
.bandeau::after { content: ''; position: absolute; left: 0; right: 0; bottom: 0; height: 4px; background: var(--drap-rouge); border-radius: 0 0 .6rem .6rem; }
.bandeau h4 { margin: 0 0 4px 0; font-weight: 700; } .bandeau p { margin: 0; opacity: .95; font-size: .95rem; }
/* chiffres clés */
.kpi.bslib-value-box { border: 1px solid var(--trait) !important; border-left: 6px solid var(--accent) !important; box-shadow: 0 1px 3px rgba(40,30,10,.06); }
.kpi .value-box-showcase { color: var(--accent); } .kpi .value-box-showcase svg { fill: var(--accent) !important; opacity: 1 !important; }
.kpi .value-box-value { color: var(--encre); font-weight: 700; } .kpi .value-box-title { color: var(--doux); font-weight: 600; } .kpi p { color: var(--doux); }
.kpi-rouge { --accent: #A8322A; } .kpi-ocre { --accent: #C9692A; } .kpi-indigo { --accent: #2F5D7C; } .kpi-vert { --accent: #3F8A5F; }
/* messages et lectures */
.message { background: #fff; border-left: 5px solid var(--vert-profond); padding: 10px 14px; margin-bottom: 8px; border-radius: 6px; font-size: .97rem; box-shadow: 0 1px 2px rgba(40,30,10,.05); }
.message.rouge { border-left-color: #A8322A; } .message.orange { border-left-color: #C9692A; } .message.vert { border-left-color: #3F8A5F; }
.lecture { font-size: .93rem; color: #33403A; margin: 2px 0 8px 0; } .lecture b { color: var(--vert-profond); }
.puce { display: inline-block; width: 11px; height: 11px; border-radius: 3px; margin: 0 4px 0 6px; vertical-align: -1px; }
.statut { display: inline-block; padding: 4px 12px; border-radius: 14px; font-weight: 700; font-size: .9rem; }
.card { border: 1px solid var(--trait); box-shadow: 0 1px 3px rgba(40,30,10,.05); }
.card-header { font-weight: 700; color: var(--vert-profond); background: #fff; border-bottom: 1px solid var(--trait); }
.bslib-value-box .value-box-title { font-size: .9rem; } .bslib-value-box .value-box-value { font-size: 1.8rem; }
.bslib-value-box .value-box-showcase { max-height: 70px; } .bslib-value-box .value-box-showcase svg { height: 42px !important; width: 42px !important; }
.bslib-value-box p { margin-bottom: 2px; }
.explication { font-size: .93rem; line-height: 1.55; } .explication b { color: var(--vert-profond); }
.fiche-action { border-left: 4px solid var(--vert-profond); padding: 10px 14px; margin-bottom: 10px; background: #FBFAF6; border-radius: 6px; font-size: .92rem; }
.choix .form-label, .choix label { font-weight: 700; color: var(--vert-profond); font-size: .9rem; }
.text-primary { color: var(--vert-profond) !important; }
.source { font-size: .8rem; color: var(--doux); }
table.dataTable { font-size: .88rem; }
table.dataTable thead th { color: var(--vert-profond); border-bottom: 2px solid var(--trait) !important; }
.btn-primary { background: var(--vert-profond); border-color: var(--vert-profond); } .btn-primary:hover { background: #084A2F; border-color: #084A2F; }
.btn-outline-primary { color: var(--vert-profond); border-color: var(--vert-profond); } .btn-outline-primary:hover { background: var(--vert-profond); color: #fff; }
/* cartes : encadré de survol placé à côté du territoire, bouton de retour */
.edu-bulle { position: fixed; z-index: 2000; display: none; max-width: 440px; background: #fff; color: var(--encre); border: 1px solid var(--trait); border-top: 4px solid var(--vert-profond);
  border-radius: 8px; box-shadow: 0 8px 24px rgba(30,25,10,.20); padding: 10px 13px; font-size: 12.5px; line-height: 1.45; pointer-events: auto; overflow-y: auto; user-select: text; cursor: auto; overscroll-behavior: contain; }
.edu-bulle.epingle { border-top-color: #A8322A; box-shadow: 0 10px 30px rgba(30,25,10,.28); padding-top: 0; }
.edu-barre { position: sticky; top: 0; z-index: 2; display: flex; align-items: center; justify-content: space-between; gap: 8px; background: #fff; margin: 0 -13px 6px -13px; padding: 6px 8px 6px 13px; border-bottom: 1px solid var(--trait); font-size: 11px; color: var(--doux); }
.edu-fermer { border: 1px solid var(--trait); background: #fff; color: #7A2418; border-radius: 50%; width: 24px; height: 24px; line-height: 20px; font-size: 18px; font-weight: 700; padding: 0; cursor: pointer; flex: 0 0 24px; }
.edu-fermer:hover { background: #A8322A; color: #fff; border-color: #A8322A; }
.edu-astuce { margin-top: 6px; padding-top: 5px; border-top: 1px dashed var(--trait); font-size: 11px; color: var(--doux); font-style: italic; }
.edu-bulle::-webkit-scrollbar { width: 8px; } .edu-bulle::-webkit-scrollbar-thumb { background: #D9CBB4; border-radius: 4px; }
.edu-bulle b { color: var(--vert-profond); }
.reco { margin-top: 7px; background: #FBF4EA; border-left: 4px solid #A8322A; border-radius: 6px; padding: 7px 9px 4px 9px; }
.reco-titre { font-weight: 700; color: #A8322A; text-transform: uppercase; letter-spacing: .4px; font-size: .78rem; margin-bottom: 4px; }
.reco-item { display: flex; gap: 7px; align-items: flex-start; margin-bottom: 5px; line-height: 1.42; }
.reco-num { flex: 0 0 18px; height: 18px; border-radius: 50%; background: #A8322A; color: #fff; font-weight: 700; font-size: .72rem; display: inline-flex; align-items: center; justify-content: center; margin-top: 1px; }
.reco b { color: #7A2418 !important; }
.commande { border-top: 4px solid var(--drap-jaune); }
.commande .irs--shiny .irs-bar { background: var(--vert-profond); border-color: var(--vert-profond); }
.curseur-annee .irs-single, .curseur-annee .irs-min, .curseur-annee .irs-max, .curseur-annee .irs-grid-text { display: none !important; }
.curseur-annee .form-group { margin-bottom: 0; }
.annee-choisie { background: var(--vert-profond); color: #fff; font-weight: 700; border-radius: 14px; padding: 2px 12px; font-size: .95rem; }
.curseur-annee { position: relative; } .curseur-annee .slider-animate-container { position: absolute; top: -2px; right: 0; margin: 0; }
.annees-axe { display: flex; justify-content: space-between; font-size: .78rem; color: var(--doux); margin: -14px 2px 6px 2px; }
.commande .irs--shiny .irs-handle { border-color: var(--vert-profond); }
.lecture-auto { display: inline-block; margin-top: 6px; font-size: .85rem; font-weight: 600; color: var(--vert-profond); border: 1px solid var(--vert-profond); border-radius: 14px; padding: 2px 10px; }
.edu-retour { position: absolute; top: 34px; right: 8px; z-index: 5; background: #fff; color: var(--vert-profond); border: 1px solid var(--vert-profond); border-radius: 16px; padding: 3px 12px; font-size: .82rem; font-weight: 600; box-shadow: 0 1px 4px rgba(0,0,0,.12); }
.edu-retour:hover { background: var(--vert-profond); color: #fff; }
.js-plotly-plot .nsewdrag { cursor: pointer !important; }
"
statut_badge <- function(s) {
  span(class = "statut", style = paste0("background:", ifelse(is.na(s), GRIS, BADGE_ALERTE[s]), ";color:", ifelse(is.na(s), "#fff", TEXTE_BADGE[s])), ifelse(is.na(s), "n.d.", c(Crise = "Crise", Vigilance = "Vigilance", "À surveiller" = "À surveiller", Normal = "Situation normale")[s])) }
ANNEE_AB <- tail(ANNEES_ABANDON, 1)                       # dernière année d'abandon observée (2023-2024)
CHOIX_PROV <- setNames(REF$code, paste0(REF$province, " (", REF$region, ")"))

ui <- page_navbar(
  id = "onglet", title = span(span(class = "etoile", HTML("&#9733;")), "EduPredict · Abandon scolaire au Burkina Faso"), theme = theme, fillable = FALSE, lang = "fr",
  header = tags$head(tags$style(HTML(css)), tags$script(src = "carte.js")),

  # ------------------------------------------------------------------------------------ 1. L'essentiel
  nav_panel("L'essentiel", value = "national", icon = icon("house"),
    div(class = "bandeau", h4("Ce qu'il faut retenir"), p("La situation de l'abandon scolaire en quatre chiffres et cinq messages, à partir des statistiques officielles de l'éducation.")),
    uiOutput("kpi"),
    uiOutput("messages"),
    layout_columns(col_widths = c(6, 6),
      card(card_header("Enfants qui ne sont plus à l'école primaire dans les provinces en crise"), uiOutput("lecture_manquants"), plotlyOutput("g_manquants", height = 290),
           div(class = "source", "Enfants en âge d'aller au primaire qui ne sont pas inscrits, comparés aux abandons enregistrés par les écoles. Estimation à partir des effectifs et de la population (DGESS, INSD).")),
      card(card_header("Taux d'abandon par niveau scolaire"), uiOutput("lecture_evol"), plotlyOutput("g_evol", height = 290),
           div(class = "source", "Part des élèves inscrits qui quittent l'école en cours d'année ou ne se réinscrivent pas l'année suivante. Source : annuaires statistiques de l'éducation (DGESS).")))),

  # ------------------------------------------------------------------------------------ 2. Carte du risque
  nav_panel("Carte du risque", value = "carte", icon = icon("map-location-dot"),
    div(class = "bandeau", h4("Où le risque d'abandon est-il le plus fort, et pourquoi ?"),
        p("Passez la souris sur une région ou une province : son explication s'affiche à côté ; amenez le curseur dessus pour la faire défiler ou la sélectionner. Un clic épingle l'explication, agrandit la carte sur ce territoire et affiche le détail sous la carte ; × ou Échap ferme l'explication, « Voir tout le pays » ou un double-clic ramène à la vue d'ensemble.")),
    card(div(class = "choix d-flex flex-wrap gap-3 align-items-end",
           selectInput("indic", "Que voulez-vous voir ?", width = "420px", choices = c(
             "Le risque d'abandon et ce qui l'explique" = "facteurs",
             "La carte d'alerte : où agir en premier" = "alerte",
             "Les écoles fermées par l'insécurité" = "ouverture",
             "Les enfants qui ne sont plus à l'école (primaire)" = "manquants",
             "Le taux d'abandon constaté (2023-2024)" = "abandon",
             "L'abandon attendu en 2025-2026" = "prevu",
             "Les écarts entre filles et garçons" = "ecart")),
           selectInput("cycle", "Niveau scolaire", choices = NIVEAUX, width = "300px"),
           radioButtons("sexe", "Public", choices = PUBLICS, inline = TRUE)),
         uiOutput("legende_carte"),
         layout_columns(col_widths = c(5, 7),
           div(h6(class = "text-primary fw-semibold", "Par région"), plotlyOutput("carte_region", height = 440)),
           div(h6(class = "text-primary fw-semibold", "Par province"), plotlyOutput("carte", height = 440)))),
    layout_columns(col_widths = c(7, 5),
      card(card_header(div(class = "d-flex justify-content-between align-items-center flex-wrap gap-2", textOutput("titre_detail", inline = TRUE),
                           uiOutput("bouton_fiche", inline = TRUE))),
           div(class = "explication", uiOutput("detail_texte"))),
      card(card_header(textOutput("titre_classement", inline = TRUE)), DTOutput("classement")))),

  # ------------------------------------------------------------------------------------ 2 bis. Probabilité d'abandon (régression logistique)
  nav_panel("Probabilités", value = "proba", icon = icon("percent"),
    div(class = "bandeau", h4("Quelle est la probabilité qu'un élève abandonne, et qu'est-ce qui la fait varier ?"),
        p("Probabilité qu'un élève inscrit quitte l'école en cours d'année, compte tenu des caractéristiques de sa province, estimée par une régression logistique sur les années 2016-2017 à 2023-2024. Passez la souris sur un territoire pour lire ce qui augmente ou diminue cette probabilité, et les recommandations ; amenez le curseur sur l'explication pour la faire défiler. Un clic l'épingle et agrandit la carte ; × ou Échap la ferme.")),
    card(div(class = "choix d-flex flex-wrap gap-3 align-items-end",
           selectInput("cycle_l", "Niveau scolaire", choices = NIVEAUX, width = "300px"),
           radioButtons("sexe_l", "Public", choices = PUBLICS, inline = TRUE)),
         uiOutput("l_retenir"),
         layout_columns(col_widths = c(5, 7),
           div(h6(class = "text-primary fw-semibold", "Par région"), plotlyOutput("l_carte_region", height = 440)),
           div(h6(class = "text-primary fw-semibold", "Par province"), plotlyOutput("l_carte", height = 440)))),
    layout_columns(col_widths = c(7, 5),
      card(card_header("Effet de chaque facteur sur la probabilité d'abandon"), uiOutput("l_lecture_effets"), DTOutput("l_effets"),
           div(class = "source", "« Non établi » : effet trop incertain ou instable, à ne pas utiliser pour décider. Rapport de cotes : par combien la cote d'abandon (probabilité d'abandonner divisée par celle de rester) est multipliée ; au-dessus de 1, plus d'abandon, en dessous, moins. Effet sur la probabilité : variation moyenne, en points de pourcentage. Intervalles à 95 % ; écarts-types robustes regroupés par province ; 500 tirages de provinces.")),
      card(card_header("Ce modèle est-il fiable ?"), uiOutput("l_fiabilite")))),

  # ------------------------------------------------------------------------------------ 3. Fiche province
  nav_panel("Fiches", value = "fiche", icon = icon("id-card"),
    card(div(class = "choix d-flex flex-wrap gap-3 align-items-end",
             selectInput("province", "Province", choices = CHOIX_PROV, selected = "BF5202", width = "340px"),
             div(class = "mb-3 d-flex gap-2", downloadButton("telecharger_fiche", "Fiche de la province (PDF, 1 page)", class = "btn-primary", icon = icon("file-pdf")),
                 downloadButton("telecharger_fiche_region", "Fiche de la région pour le directeur régional (PDF, 1 page)", class = "btn-outline-primary", icon = icon("file-pdf"))))),
    uiOutput("fiche_entete"),
    layout_columns(col_widths = c(6, 6),
      card(card_header("En bref"), div(class = "explication", uiOutput("fiche_enbref"))),
      card(card_header("Que faire ?"), uiOutput("f_actions"))),
    layout_columns(col_widths = c(6, 6),
      card(card_header("Taux d'abandon au fil des années"), uiOutput("lecture_f_abandon"), plotlyOutput("f_abandon", height = 280),
           div(class = "source", "Filles et garçons ensemble. Années 2016-2017 à 2023-2024 (dernière année connue).")),
      card(card_header("Écoles en fonctionnement au fil des années"), uiOutput("lecture_f_ouverture"), plotlyOutput("f_ouverture", height = 280),
           div(class = "source", "En % des écoles qui fonctionnaient avant la crise (2017-2019). Ligne rouge : seuil de crise de 70 %.")))),

  # ------------------------------------------------------------------------------------ 4. Anticiper
  nav_panel("Anticiper", value = "anticiper", icon = icon("binoculars"),
    div(class = "bandeau", h4("Que va-t-il se passer d'ici 2030, et où agir avant qu'il ne soit trop tard ?"),
        p("Faites glisser le curseur pour choisir l'année : la carte et la courbe d'évolution se mettent à jour. Choisissez un scénario pour voir ce qui change si la situation s'améliore ou se dégrade. Cliquez sur une province de la carte pour afficher son évolution.")),
    card(class = "commande",
      layout_columns(col_widths = c(5, 3, 4), fill = FALSE,
        div(class = "choix curseur-annee", div(class = "d-flex align-items-center gap-2", tags$label(class = "form-label mb-0", "Année scolaire de prévision :"), span(class = "annee-choisie", textOutput("an_txt", inline = TRUE))),
            sliderInput("an_p", NULL, min = 2025, max = 2030, value = 2026, step = 1, sep = "", ticks = TRUE, width = "100%",
                        animate = animationOptions(interval = 1600, loop = FALSE, playButton = tags$span(class = "lecture-auto", icon("play"), " Défiler les années"))),
            div(class = "annees-axe", lapply(2025:2030, function(y) span(paste0(y - 1, "-", substr(y, 3, 4)))))),
        div(class = "choix", radioButtons("scen_p", "Scénario (à partir de 2026-2027)", choices = c("Central : niveau habituel" = "Central", "Favorable" = "Optimiste", "Défavorable" = "Pessimiste"))),
        div(class = "choix", selectInput("cycle_p", "Niveau scolaire", choices = NIVEAUX, width = "100%"), radioButtons("sexe_p", "Public", choices = PUBLICS, inline = TRUE)))),
    layout_columns(col_widths = c(6, 6),
      card(card_header(textOutput("titre_carte_p", inline = TRUE)), uiOutput("lecture_prev"), plotlyOutput("a_prev_carte", height = 440),
           div(class = "source", "Prévision 2024-2026 : niveau habituel de chaque province (médiane de ses années observées), méthode qui a fait mieux que tous les modèles testés. Fourchette probable : 8 chances sur 10 que le taux réel s'y trouve, d'après les erreurs passées.")),
      card(card_header(div(class = "choix d-flex flex-wrap gap-2 align-items-center", span("Évolution jusqu'en 2030 :"),
                           selectInput("terr_p", NULL, choices = c("Burkina Faso (toutes provinces)" = "BF", setNames(REF$code[order(REF$province)], sort(REF$province))), width = "300px"))),
           uiOutput("lecture_traj"), plotlyOutput("a_traj", height = 360),
           div(class = "source", "Scénarios conditionnels, pas des prévisions certaines. Central : niveau habituel de la province. Favorable et défavorable : le plus bas et le plus haut de ses niveaux passés (avant la crise, toutes années, années de crise)."))),
    layout_columns(col_widths = c(7, 5),
      card(card_header(div(class = "d-flex justify-content-between align-items-center w-100", textOutput("titre_evol_p", inline = TRUE), bouton_export("dl_prev"))), plotlyOutput("a_prev_evol", height = 400)),
      card(card_header("Combien d'enfants privés d'école en 2030 ?"), uiOutput("lecture_manq"), plotlyOutput("a_manq", height = 300),
           div(class = "source", "Enfants qui seraient à l'école primaire sans la crise (écoles fermées). Baisse de 17 % en deux ans, observée pendant la campagne de retour à l'école ; ce n'est pas un scénario sans action."))),
    layout_columns(col_widths = c(7, 5),
      card(card_header("Provinces encore ouvertes à surveiller en priorité"), uiOutput("lecture_survie"), DTOutput("a_survie")),
      card(card_header("Les signaux d'alerte à suivre chaque trimestre"), uiOutput("a_signaux"))),
    card(card_header("En résumé, pour l'année et le scénario choisis"), uiOutput("a_kpi"))),

  # ------------------------------------------------------------------------------------ 5. Plan d'action
  nav_panel("Plan d'action", value = "plan", icon = icon("list-check"),
    div(class = "bandeau", h4("Que faire, où, quand, avec quels moyens ?"), p("Les actions recommandées, province par province, avec leur coût et leur calendrier.")),
    div(class = "lecture", HTML("<b>À retenir :</b> rouvrir les écoles dans les provinces en crise passe avant tout le reste ; en attendant, la radio éducative et les classes temporaires limitent les pertes. Dans les provinces encore ouvertes mais fragiles, une mission d'évaluation avant fin mars 2027 coûte peu et évite la bascule.")),
    card(card_header("Les grandes orientations"), tableOutput("p_matrice")),
    card(card_header(div(class = "d-flex justify-content-between align-items-center flex-wrap gap-2", span("Les actions par province"),
         checkboxGroupInput("statut_fiche", NULL, inline = TRUE, choices = c("Prévenir" = "Préventif", "Rouvrir" = "Curatif", "Scolariser autrement" = "Palliatif", "S'inspirer" = "Transfert d'expérience"),
                            selected = c("Préventif", "Curatif", "Palliatif", "Transfert d'expérience")))),
         div(class = "lecture", "Prévenir : provinces encore ouvertes mais fragiles. Rouvrir : provinces passées sous 70 % d'écoles ouvertes récemment. Scolariser autrement : provinces où les écoles sont fermées depuis plusieurs années. S'inspirer : provinces qui regagnent des élèves."),
         DTOutput("p_fiches")),
    layout_columns(col_widths = c(6, 6),
      card(card_header("Combien coûte l'action ?"), uiOutput("p_cout")),
      card(card_header("Où affecter les enseignants ?"), uiOutput("p_enseignants")))),

  # ------------------------------------------------------------------------------------ 6. Sources et méthode
  nav_panel("Méthode", value = "methode", icon = icon("book"),
    layout_columns(col_widths = c(6, 6),
      card(card_header("D'où viennent les chiffres ?"), uiOutput("m_donnees")),
      card(card_header("Ce qu'il faut garder en tête"), uiOutput("m_limites")))),
  nav_spacer()
)

# ============================================================================================ serveur
server <- function(input, output, session) {
  observeEvent(session$clientData$url_search, once = TRUE, ignoreNULL = FALSE, {
    q <- parseQueryString(session$clientData$url_search)
    if (!is.null(q$province) && q$province %in% REF$code) updateSelectInput(session, "province", selected = q$province)
    if (!is.null(q$indic)) updateSelectInput(session, "indic", selected = q$indic)
    if (!is.null(q$onglet)) nav_select("onglet", q$onglet)
  })
  sel <- reactive(input$province)
  ST25 <- statut_annee("2024-2025")

  # ---------------------------------------------------------------- 1. L'essentiel
  man <- function(a) sum(em$manquants[em$annee_scolaire == a & em$code %in% PROV_CRISE])
  output$kpi <- renderUI({
    ab <- panel %>% filter(annee == ANNEE_AB); tx <- moy_ponderee(ab$taux_abandon_cycle_T, ab$effectif)
    part <- cact[[grep("^Part du budget", names(cact))]][1]
    layout_columns(col_widths = c(3, 3, 3, 3), fill = FALSE,
      value_box(title = "Provinces en crise (2024-2025)", value = paste(sum(ST25$statut == "Crise", na.rm = TRUE), "sur 45"), showcase = icon("school-circle-xmark"), showcase_layout = "left center", theme = value_box_theme(bg = "#FFFFFF", fg = ENCRE), class = "kpi kpi-rouge",
                p(class = "small", "moins de 70 % de leurs écoles fonctionnent")),
      value_box(title = "Enfants privés d'école primaire (2024-2025)", value = nf(man("2024-2025")), showcase = icon("children"), showcase_layout = "left center", theme = value_box_theme(bg = "#FFFFFF", fg = ENCRE), class = "kpi kpi-ocre",
                p(class = "small", sprintf("%s %% en deux ans, pendant la campagne de retour à l'école", nf(100 * (man("2024-2025") / man("2022-2023") - 1), 0)))),
      value_box(title = "Taux d'abandon au primaire (2023-2024)", value = pct(tx), showcase = icon("person-walking-arrow-right"), showcase_layout = "left center", theme = value_box_theme(bg = "#FFFFFF", fg = ENCRE), class = "kpi kpi-indigo",
                p(class = "small", "il ne compte que les enfants encore inscrits")),
      value_box(title = "Coût des enseignants à recruter", value = paste0(nf(part, 1), " %"), showcase = icon("coins"), showcase_layout = "left center", theme = value_box_theme(bg = "#FFFFFF", fg = ENCRE), class = "kpi kpi-vert",
                p(class = "small", "du budget 2026 de l'éducation (8 187 enseignants)")))
  })
  output$messages <- renderUI({
    m24 <- man("2023-2024"); ab24 <- sum(em$abandons_comptes[em$annee_scolaire == "2023-2024" & em$code %in% PROV_CRISE], na.rm = TRUE)
    tagList(
      div(class = "message rouge", HTML(sprintf("<b>Le taux d'abandon ne montre qu'une petite partie du problème.</b> Dans les 16 provinces en crise, %s enfants n'étaient plus à l'école primaire en 2023-2024, alors que les écoles n'avaient enregistré que %s abandons. Quand une école ferme, ses élèves disparaissent des registres sans être comptés.", nf(m24), nf(ab24)))),
      div(class = "message orange", HTML("<b>Un seuil d'alerte clair : 70 % d'écoles ouvertes.</b> En dessous, la situation bascule en moins d'un an et aucune province n'est jamais remontée au-dessus. Il faut agir avant ce seuil.")),
      { al <- table(factor(vapply(ST25$code, function(c0) { s <- statut_affiche(ST25[ST25$code == c0, ], c0); if (is.na(s)) "Normal" else s }, ""), levels = names(LIB_ALERTE)))
        div(class = "message orange", HTML(sprintf("<b>Où agir en premier :</b> %d provinces en crise, %d en vigilance et %d à surveiller. La « carte d'alerte » de l'onglet « Carte du risque » les localise, et chaque province ou région dispose d'une fiche d'une page à imprimer.", al[["Crise"]], al[["Vigilance"]], al[["À surveiller"]]))) },
      div(class = "message", HTML("<b>Là où les enfants sont peu scolarisés et les familles pauvres, l'abandon est plus élevé.</b> Ces facteurs sont détaillés, province par province, dans l'onglet « Carte du risque ».")),
      div(class = "message vert", HTML("<b>Le plan est finançable.</b> Recruter les 8 187 enseignants nécessaires représente 2,3 % du budget 2026 de l'éducation ; la difficulté principale est la sécurité, pas le financement.")))
  })
  output$lecture_manquants <- renderUI(div(class = "lecture", HTML(sprintf("<b>À retenir :</b> le nombre d'enfants privés d'école a été multiplié par près de 4 entre 2019-2020 et 2022-2023, puis a baissé de %s %% en deux ans.",
                                                                         nf(100 * (1 - man("2024-2025") / man("2022-2023")), 0)))))
  output$g_manquants <- renderPlotly({
    s <- em %>% filter(code %in% PROV_CRISE) %>% group_by(annee_scolaire) %>% summarise(m = sum(manquants), ab = sum(abandons_comptes, na.rm = TRUE), .groups = "drop")
    plot_ly(s, x = ~annee_scolaire) %>% add_bars(y = ~round(m), name = "Enfants qui ne sont plus à l'école", marker = list(color = ROUGE), hovertemplate = "%{x} : %{y:,.0f} enfants<extra></extra>") %>%
      add_bars(y = ~round(ab), name = "Abandons enregistrés par les écoles", marker = list(color = BLEU2), hovertemplate = "%{x} : %{y:,.0f} abandons<extra></extra>") %>%
      style_graphe("Nombre d'enfants") %>% layout(barmode = "group", hovermode = "closest")
  })
  serie <- function(cy) { v <- CIBLES[[cy]][["Ensemble"]]
    panel %>% filter(annee %in% ANNEES_ABANDON) %>% group_by(annee) %>% summarise(val = if (cy == "Primaire") moy_ponderee(.data[[v]], effectif) else median(.data[[v]], na.rm = TRUE), .groups = "drop") }
  output$lecture_evol <- renderUI({ s <- serie("Primaire")
    div(class = "lecture", HTML(sprintf("<b>À retenir :</b> au primaire, l'abandon est passé de %s %% à %s %% au plus fort de la crise (2021-2022), puis est revenu à %s %% en 2023-2024 : une baisse en partie trompeuse, car les enfants des écoles fermées ne sont plus comptés.",
                                         nf(s$val[1], 0), nf(max(s$val), 0), nf(tail(s$val, 1), 0)))) })
  output$g_evol <- renderPlotly({
    p <- plot_ly()
    for (cy in CYCLES) { s <- serie(cy); p <- add_trace(p, x = s$annee, y = round(s$val, 1), type = "scatter", mode = "lines+markers", name = c(Primaire = "Primaire", "Post-primaire" = "Collège", Secondaire = "Lycée")[[cy]],
                                                     line = list(color = c(BLEU2, ORANGE, ROUGE)[match(cy, CYCLES)], width = 2.5), marker = list(color = c(BLEU2, ORANGE, ROUGE)[match(cy, CYCLES)], size = 6), hovertemplate = "%{x} : %{y} %<extra></extra>") }
    style_graphe(p, "Taux d'abandon (%)") %>% layout(hovermode = "closest")
  })

  # ---------------------------------------------------------------- 2. Carte du risque
  sx_carte <- reactive(input$sexe)
  valeurs_carte <- reactive({
    i <- input$indic; cy <- input$cycle; sx <- input$sexe
    switch(i,
      facteurs = { d <- facteurs_territoire(cy, sx) %>% group_by(code) %>% summarise(t = sum(contribution), .groups = "drop")
                   list(v = setNames(d$t, d$code), lib = "Écart à la moyenne (points d'abandon)", coul = DIV_ECART, lo = -15, hi = 15, f = function(x) paste(pts(x), "points")) },
      alerte = { st <- ST25; a <- sapply(st$code, function(c0) statut_affiche(st[st$code == c0, ], c0)); a <- LIB_ALERTE[a]
                 list(v = setNames(a, st$code), lib = "Niveau d'alerte", type = "categorie", f = function(x) as.character(x)) },
      ouverture = { o <- panel %>% filter(annee == "2024-2025"); list(v = setNames(pmin(o$ouverture, 110), o$code), lib = "Écoles en fonctionnement (%)", coul = SEQ_OUVERT, lo = 30, hi = 110, f = function(x) pct(x, 0)) },
      manquants = { e <- em %>% filter(annee_scolaire == "2024-2025"); list(v = setNames(e$manquants, e$code), lib = "Enfants qui ne sont plus à l'école", coul = SEQ_RISQUE, lo = 0, hi = NULL, f = function(x) nf(x)) },
      abandon = { b <- panel %>% filter(annee == ANNEE_AB); list(v = setNames(b[[CIBLES[[cy]][[sx]]]], b$code), lib = "Taux d'abandon (%)", coul = SEQ_RISQUE, lo = 0, hi = NULL, f = function(x) pct(x)) },
      ecart = { b <- panel %>% filter(annee == ANNEE_AB)
                list(v = setNames(b[[CIBLES[[cy]][["Filles"]]]] - b[[CIBLES[[cy]][["Garçons"]]]], b$code), lib = "Filles moins garçons (points)", coul = DIV_ECART, lo = -10, hi = 10, f = function(x) paste(pts(x), "points")) },
      prevu = { s2 <- if (sx == "Ensemble") c("Filles", "Garçons") else sx
                pv <- prev %>% filter(annee_scolaire == "2025-2026", cycle == cy, sexe %in% s2) %>% group_by(code_province) %>% summarise(t = mean(taux_prevu), .groups = "drop")
                list(v = setNames(pv$t, pv$code_province), lib = "Abandon attendu (%)", coul = SEQ_RISQUE, lo = 0, hi = NULL, f = function(x) pct(x)) })
  })
  texte_prov <- reactive({ d <- facteurs_territoire(input$cycle, input$sexe)
    sapply(REF$code, function(c0) interpretation_territoire(d[d$code == c0, ], paste0(nom(c0), " (", REF$region[REF$code == c0], ")"), input$cycle, input$sexe,
                                                          ST25$ouverture[ST25$code == c0], rang_de(c0), codes = c0), simplify = FALSE) })
  texte_reg <- reactive({ d <- facteurs_territoire(input$cycle, input$sexe, "region")
    sapply(unique(REF$region), function(r) { o <- mean(ST25$ouverture[REF$region[match(ST25$code, REF$code)] == r], na.rm = TRUE)
      interpretation_territoire(d[d$region == r, ], paste0("Région ", r, " (", sum(REF$region == r), " provinces)"), input$cycle, input$sexe, o, codes = REF$code[REF$region == r], region = r) }, simplify = FALSE) })
  ligne_valeur <- function(vc, x) if (input$indic == "facteurs") "" else paste0(vc$lib, " : <b>", vc$f(x), "</b><br>")
  avec_valeur <- function(txt, vl) if (!nzchar(vl)) txt else sub("</b><br>", paste0("</b><br>", vl), txt, fixed = TRUE)
  output$carte <- renderPlotly({
    vc <- valeurs_carte(); tp <- texte_prov()
    tp <- setNames(lapply(REF$code, function(c0) avec_valeur(tp[[c0]], ligne_valeur(vc, vc$v[c0]))), REF$code)
    tit <- setNames(lapply(REF$code, function(c0) ""), REF$code)
    if (identical(vc$type, "categorie")) return(carte(vc$v, vc$lib, "categorie", categories = COUL_ALERTE, survol = tp, titres = tit, consigne = "", libelle_valeur = FALSE))
    carte(vc$v, vc$lib, "continu", couleurs = vc$coul, lo = vc$lo, hi = if (is.null(vc$hi)) max(vc$v, na.rm = TRUE) else vc$hi, format_val = vc$f,
          survol = tp, titres = tit, consigne = "", libelle_valeur = FALSE)
  })
  output$carte_region <- renderPlotly({
    vc <- valeurs_carte(); reg <- REF$region[match(names(vc$v), REF$code)]
    if (identical(vc$type, "categorie")) vc <- list(v = setNames(as.numeric(vc$v != LIB_ALERTE[["Normal"]]), names(vc$v)), lib = "Provinces en alerte", coul = SEQ_RISQUE, lo = 0, hi = NULL, f = function(x) paste(nf(x, 0), "province(s) sur", sum(REF$region == REF$region[1])), somme = TRUE)
    v <- if (input$indic %in% c("manquants", "alerte")) tapply(vc$v, reg, sum, na.rm = TRUE) else tapply(vc$v, reg, mean, na.rm = TRUE)
    vp <- setNames(as.numeric(v[REF$region]), REF$code); tr <- texte_reg()
    tit <- setNames(lapply(REF$region, function(r) ""), REF$code)
    lv <- function(r) if (input$indic == "alerte") sprintf("Provinces en alerte (crise, vigilance ou à surveiller) : <b>%s sur %s</b><br>", nf(v[[r]], 0), sum(REF$region == r)) else ligne_valeur(vc, v[[r]])
    svp <- setNames(lapply(REF$region, function(r) avec_valeur(tr[[r]], lv(r))), REF$code)
    carte(vp, vc$lib, "continu", couleurs = vc$coul, lo = vc$lo, hi = if (is.null(vc$hi)) max(vp, na.rm = TRUE) else vc$hi, format_val = vc$f, source = "carte_region",
          survol = svp, titres = tit, bord_interne = TRUE, consigne = "", echelle = FALSE, libelle_valeur = FALSE) %>%
      add_annotations(data = CENTRES_REG, x = ~lon, y = ~lat, text = ~region, showarrow = FALSE, font = list(size = 9.5, color = ENCRE, family = "Segoe UI Semibold, Segoe UI, Roboto, Arial")) %>% layout(showlegend = FALSE)
  })
  # ---------------------------------------------------------------- 2 bis. Probabilité d'abandon
  l_textes <- reactive({ cy <- input$cycle_l; sx <- input$sexe_l; v <- lg_proba(cy, sx); pm <- lg_proba_moy(cy, sx)
    list(v = v, pm = pm, prov = setNames(lapply(REF$code, function(c0) texte_proba(paste0(nom(c0), " (", REF$region[REF$code == c0], ")"), v[[c0]], pm, lg_effets(c0, cy, sx), cy, sx, codes = c0)), REF$code),
         reg = sapply(unique(REF$region), function(r) texte_proba(paste0("Région ", r, " (", sum(REF$region == r), " provinces)"), lg_proba_region(cy, sx)[[r]], pm, lg_effets(REF$code[REF$region == r], cy, sx), cy, sx, codes = REF$code[REF$region == r], region = r), simplify = FALSE)) })
  output$l_carte <- renderPlotly({ t <- l_textes()
    carte(t$v, "Probabilité d'abandon (%)", "continu", couleurs = SEQ_RISQUE, lo = 0, hi = max(t$v, na.rm = TRUE), format_val = function(x) pct(x, 0), source = "proba",
          survol = t$prov, titres = setNames(lapply(REF$code, function(c0) ""), REF$code), consigne = "", libelle_valeur = FALSE) })
  output$l_carte_region <- renderPlotly({ t <- l_textes(); pr <- lg_proba_region(input$cycle_l, input$sexe_l)
    vp <- setNames(as.numeric(pr[REF$region]), REF$code)
    carte(vp, "Probabilité d'abandon (%)", "continu", couleurs = SEQ_RISQUE, lo = 0, hi = max(t$v, na.rm = TRUE), format_val = function(x) pct(x, 0), source = "proba_region",
          survol = setNames(lapply(REF$region, function(r) t$reg[[r]]), REF$code), titres = setNames(lapply(REF$region, function(r) ""), REF$code), bord_interne = TRUE, consigne = "", echelle = FALSE, libelle_valeur = FALSE) %>%
      add_annotations(data = CENTRES_REG, x = ~lon, y = ~lat, text = ~region, showarrow = FALSE, font = list(size = 9.5, color = ENCRE, family = "Segoe UI Semibold, Segoe UI, Roboto, Arial")) %>% layout(showlegend = FALSE) })
  output$l_retenir <- renderUI({ t <- l_textes(); pr <- sort(lg_proba_region(input$cycle_l, input$sexe_l), decreasing = TRUE); o <- sort(t$v, decreasing = TRUE)
    div(class = "lecture", HTML(sprintf("<b>À retenir :</b> %s, %s, la probabilité qu'un élève abandonne va de %s à %s selon la province ; une province aux caractéristiques moyennes est à %s. Régions les plus exposées : %s (%s) et %s (%s) ; provinces : %s, %s et %s.",
      sub("^a", "A", nom_niveau(input$cycle_l)), nom_public(input$sexe_l), pct(min(t$v, na.rm = TRUE), 0), pct(max(t$v, na.rm = TRUE), 0), pct(t$pm, 0),
      names(pr)[1], pct(pr[1], 0), names(pr)[2], pct(pr[2], 0), nom(names(o)[1]), nom(names(o)[2]), nom(names(o)[3])))) })
  l_tab <- reactive({ d <- LG_E[LG_E$cycle == input$cycle_l & LG_E$sexe %in% sexes_de(input$sexe_l), ]
    d$Public <- d$sexe; d <- d[order(!d$effet_etabli, -abs(d$effet_marginal_points)), ]; d })
  output$l_effets <- renderDT({ d <- l_tab()
    out <- data.frame(Facteur = NOM_LG[d$facteur], Public = d$Public, `Pour une hausse de` = mapply(hausse_lg, d$facteur, d$ecart_type_unite_naturelle),
                      `Rapport de cotes` = sprintf("× %s [%s ; %s]", nf(d$rapport_de_cotes, 2), nf(d$rc_ic95_bas, 2), nf(d$rc_ic95_haut, 2)),
                      `Effet sur la probabilité` = sprintf("%s%s point(s) [%s ; %s]", ifelse(d$effet_marginal_points >= 0, "+ ", "− "), nf(abs(d$effet_marginal_points), 1), nf(d$em_ic95_bas, 1), nf(d$em_ic95_haut, 1)),
                      `Fiabilité` = ifelse(d$effet_etabli, "Effet établi", "Non établi"), check.names = FALSE)
    if (input$sexe_l != "Ensemble") out$Public <- NULL
    tableau(out, pageLength = 12) %>% formatStyle("Fiabilité", color = styleEqual(c("Effet établi", "Non établi"), c(VERT, "#9A968C")), fontWeight = styleEqual("Effet établi", "bold")) })
  output$l_lecture_effets <- renderUI({ d <- l_tab(); e <- d[d$effet_etabli, ]
    if (!nrow(e)) return(div(class = "lecture", HTML("<b>À retenir :</b> aucun facteur n'a d'effet établi pour ce niveau et ce public.")))
    ph <- sapply(unique(e$facteur), function(f) { g <- e[e$facteur == f, ]
      qui <- if (input$sexe_l == "Ensemble" && nrow(g) == 1) paste0(" (", tolower(g$sexe), " seulement)") else ""
      sprintf("<b>%s</b>%s : pour %s, la probabilité d'abandonner %s de %s point(s) en moyenne (cote multipliée par %s)", NOM_LG[[f]], qui, hausse_lg(f, g$ecart_type_unite_naturelle[1]),
              if (g$effet_marginal_points[1] > 0) "augmente" else "diminue", paste(unique(nf(sort(abs(g$effet_marginal_points)), 1)), collapse = " à "), paste(unique(nf(sort(g$rapport_de_cotes), 2)), collapse = " à ")) })
    div(class = "lecture", HTML(paste0("<b>À retenir :</b><br>", paste0("• ", ph, collapse = "<br>"),
      "<br>Les autres facteurs (écoles en fonctionnement, cantines, élèves par enseignant, redoublement) n'ont pas d'effet établi sur l'abandon <b>des élèves inscrits</b> ; la fermeture des écoles pèse surtout en privant des enfants d'école (onglet « Carte du risque »)."))) })
  output$l_fiabilite <- renderUI({
    coul <- c(OUI = VERT, PARTIEL = ORANGE, NON = ROUGE); lib <- c(OUI = "Oui", PARTIEL = "En partie", NON = "Non")
    simple <- c(C1 = "Aucune information future utilisée", C2 = "Facteurs non redondants", C3 = "Variabilité entre provinces prise en compte", C4 = "Probabilités fidèles aux taux observés",
                C5 = "Testé sur des années et des provinces non vues", C6 = "Prévoit mieux que la tendance passée de la province", C7 = "Repère les provinces au-dessus de 20 % d'abandon",
                C8 = "Effets stables d'un échantillon à l'autre", C9 = "Concorde avec les analyses précédentes", C10 = "Pas d'erreur systématique")
    tagList(
      div(class = "lecture", HTML("<b>À retenir :</b> le modèle est fiable pour <b>décrire</b> la probabilité d'abandon et <b>comparer</b> l'effet des facteurs entre territoires. Il n'est <b>pas</b> fait pour prévoir l'année suivante : la prévision du tableau de bord reste la tendance passée de chaque province (onglet « Anticiper »).")),
      tags$table(class = "table table-sm", tags$tbody(lapply(seq_len(nrow(LG_C)), function(i) tags$tr(
        tags$td(simple[[LG_C$critere[i]]]), tags$td(style = paste0("font-weight:700;color:", coul[[LG_C$verdict[i]]], ";white-space:nowrap"), lib[[LG_C$verdict[i]]]))))),
      div(class = "source", "Dix critères fixés avant l'estimation ; détail dans le rapport de la régression logistique (04_analyse/03_modelisation)."))
  })

  cible <- reactiveVal(list(type = "province", id = NULL))
  observeEvent(input$clic_province, { if (input$clic_province %in% REF$code) { updateSelectInput(session, "province", selected = input$clic_province); cible(list(type = "province", id = input$clic_province)) } })
  observeEvent(input$clic_region, { if (input$clic_region %in% REF$region) cible(list(type = "region", id = input$clic_region)) })
  observeEvent(input$province, { if (identical(cible()$type, "province")) cible(list(type = "province", id = input$province)) })
  observeEvent(input$ouvrir_fiche, nav_select("onglet", "fiche"))
  output$bouton_fiche <- renderUI(if (identical(cible()$type, "region")) downloadButton("telecharger_fiche_region_carte", "Fiche de la région (PDF, 1 page)", class = "btn-sm btn-primary", icon = icon("file-pdf"))
                                  else actionButton("ouvrir_fiche", "Voir la fiche de la province", icon = icon("id-card"), class = "btn-sm btn-primary"))
  output$telecharger_fiche_region_carte <- downloadHandler(
    filename = function() paste0("DCB26_fiche-region_", gsub("[^A-Za-z]", "", iconv(cible()$id, "UTF-8", "ASCII//TRANSLIT")), ".pdf"),
    content = function(file) fiche_pdf(file, "region", cible()$id), contentType = "application/pdf")
  output$legende_carte <- renderUI({
    txt <- switch(input$indic,
      facteurs = "<b>Comment lire la carte :</b> brun-rouge = risque d'abandon plus élevé que dans une province moyenne, bleu = plus faible, sable = proche de la moyenne. Le calcul tient compte de la pauvreté des familles, de l'accès à l'école primaire, au collège et au lycée, de la fin du primaire et de la place des filles à l'école : les facteurs dont le lien avec l'abandon a été confirmé par nos analyses.",
      alerte = "<b>Comment lire la carte :</b> <span class='puce' style='background:#9E1B1B'></span><b>Crise</b> : moins de 70 % des écoles fonctionnent, agir tout de suite. <span class='puce' style='background:#D0662A'></span><b>Vigilance</b> : la province a perdu plus de 5 points d'écoles en un an. <span class='puce' style='background:#E8BD45'></span><b>À surveiller</b> : encore ouverte mais proche du seuil ou parmi les 5 plus exposées à une bascule. <span class='puce' style='background:#B7D3BF'></span><b>Normal</b> : pas d'alerte. Sur la carte des régions : nombre de provinces en alerte.",
      ouverture = "<b>Comment lire la carte :</b> part des écoles qui fonctionnent en 2024-2025, comparée à la période d'avant la crise (2017-2019). Tons brun-rouge : provinces sous 70 %, seuil à partir duquel la situation bascule ; tons verts : au-dessus.",
      manquants = "<b>Comment lire la carte :</b> enfants en âge d'aller au primaire qui ne sont plus inscrits en 2024-2025, du fait de la crise (estimation). Sur la carte des régions : total de la région.",
      abandon = "<b>Comment lire la carte :</b> part des élèves inscrits qui ont quitté l'école en 2023-2024, dernière année connue. Ce taux ne compte pas les enfants dont l'école a fermé.",
      prevu = "<b>Comment lire la carte :</b> abandon attendu en 2025-2026 si la province reste à son niveau habituel. Fiable au primaire, à prendre avec prudence au collège et au lycée.",
      ecart = "<b>Comment lire la carte :</b> brun-rouge = les filles abandonnent davantage que les garçons ; bleu = moins. En 2023-2024, dernière année connue.")
    div(class = "lecture", HTML(txt))
  })
  output$titre_detail <- renderText(if (identical(cible()$type, "region")) paste0("Pourquoi ce niveau de risque ? Région ", cible()$id) else paste0("Pourquoi ce niveau de risque ? ", nom(sel())))
  output$detail_texte <- renderUI({
    if (identical(cible()$type, "region")) { r <- cible()$id; return(HTML(texte_reg()[[r]])) }
    d <- facteurs_territoire(input$cycle, input$sexe); c0 <- sel()
    HTML(interpretation_territoire(d[d$code == c0, ], paste0(nom(c0), " (", REF$region[REF$code == c0], ")"), input$cycle, input$sexe, ST25$ouverture[ST25$code == c0], rang_de(c0), codes = c0))
  })
  output$titre_classement <- renderText({ vc <- valeurs_carte(); switch(input$indic, facteurs = "Les 10 provinces où le risque est le plus élevé", alerte = "Les provinces en alerte, des plus urgentes aux moins urgentes", ouverture = "Les 10 provinces avec le moins d'écoles en fonctionnement",
      manquants = "Les 10 provinces où le plus d'enfants ne sont plus à l'école", abandon = "Les 10 provinces au taux d'abandon le plus élevé", prevu = "Les 10 provinces à l'abandon attendu le plus élevé",
      ecart = "Les 10 provinces où les filles abandonnent le plus par rapport aux garçons") })
  classement_df <- reactive({ vc <- valeurs_carte()
    d <- data.frame(code = names(vc$v), val = unname(vc$v), stringsAsFactors = FALSE) %>% filter(!is.na(code), !is.na(val))
    if (input$indic == "alerte") { o <- ST25$ouverture[match(d$code, ST25$code)]
      return(d %>% mutate(rg = match(val, LIB_ALERTE), o = o) %>% filter(val != LIB_ALERTE[["Normal"]]) %>% arrange(rg, o) %>% select(code, val)) }
    d <- if (input$indic == "ouverture") d %>% arrange(val) else d %>% arrange(desc(val)); head(d, 10) })
  output$classement <- renderDT({ vc <- valeurs_carte(); d <- classement_df()
    df <- data.frame(Province = nom(d$code), Région = REF$region[match(d$code, REF$code)], v = vc$f(d$val)); names(df)[3] <- vc$lib
    tableau(df, pageLength = if (input$indic == "alerte") 25 else 10, selection = "single") })
  observeEvent(input$classement_rows_selected, updateSelectInput(session, "province", selected = classement_df()$code[input$classement_rows_selected]))

  # ---------------------------------------------------------------- 3. Fiche province
  infos <- reactive({
    c0 <- sel(); h <- panel %>% filter(code == c0) %>% arrange(annee)
    list(code = c0, nom = nom(c0), region = REF$region[REF$code == c0], h = h, st = ST25 %>% filter(code == c0),
         dec = dec %>% filter(code == c0), fiches = fiches %>% filter(code == c0) %>% arrange(Priorité), survie = survie %>% filter(code == c0),
         man = em %>% filter(code == c0) %>% arrange(annee_scolaire), ens = ens %>% filter(code == c0))
  })
  output$fiche_entete <- renderUI({ f <- infos()
    div(class = "bandeau d-flex justify-content-between align-items-center flex-wrap gap-3",
      div(h4(f$nom), p(paste0("Région ", f$region))),
      div(class = "text-end", statut_badge(statut_affiche(f$st, f$code)), p(style = "margin-top:6px", sprintf("%s %% des écoles fonctionnent en 2024-2025", nf(f$st$ouverture, 0))))) })
  enbref <- function(f) {
    a <- f$h %>% filter(annee == ANNEE_AB); o <- f$st$ouverture
    phr <- c()
    phr <- c(phr, if (!is.na(o) && o < 70) sprintf("<b>Situation de crise :</b> seulement %s %% des écoles d'avant la crise fonctionnent encore.", nf(o, 0))
                  else if (!is.na(f$st$statut) && f$st$statut == "Vigilance") sprintf("<b>Vigilance :</b> %s %% des écoles fonctionnent, mais la province a perdu plus de 5 points en un an.", nf(o, 0))
                  else if (!is.na(o) && o < 80) sprintf("<b>Attention :</b> %s %% des écoles d'avant la crise fonctionnent, soit seulement %s points au-dessus du seuil de crise de 70 %%.", nf(o, 0), nf(o - 70, 0))
                  else sprintf("<b>Écoles :</b> %s %% des écoles d'avant la crise fonctionnent ; pas de crise d'ouverture.", nf(o, 0)))
    m25 <- f$man$manquants[f$man$annee_scolaire == "2024-2025"]
    if (length(m25) && m25 > 0) phr <- c(phr, sprintf("<b>%s enfants</b> en âge d'aller au primaire ne sont plus à l'école (2024-2025).", nf(m25)))
    if (nrow(f$dec)) { pc <- f$dec$`Part des manquants dans le total hors école`
      phr <- c(phr, if (pc >= 60) "La plupart des enfants hors de l'école y allaient avant la crise : <b>la priorité est de les réinscrire</b>."
                    else if (pc < 50) "Beaucoup d'enfants n'étaient déjà pas scolarisés avant la crise : <b>il faut aussi une scolarisation initiale</b>, pas seulement une réouverture."
                    else "Les enfants hors de l'école sont pour moitié d'anciens élèves, pour moitié des enfants jamais scolarisés : <b>réinscrire et scolariser pour la première fois</b>.") }
    tx <- c(Primaire = a$taux_abandon_cycle_T, Collège = a$pp_abandon_moy_6e_4e_T, Lycée = a$sec_abandon_moy_2nde_1ere_T)
    nat <- sapply(CYCLES, function(cy) median((panel %>% filter(annee == ANNEE_AB))[[CIBLES[[cy]][["Ensemble"]]]], na.rm = TRUE))
    ec <- tx - nat; k <- which.max(ec)
    if (length(k) && !is.na(ec[k]) && ec[k] >= 2) phr <- c(phr, sprintf("En 2023-2024, l'abandon est particulièrement élevé <b>au %s</b> : %s %% des élèves, contre %s %% pour une province typique.", tolower(names(tx)[k]), nf(tx[k], 1), nf(nat[k], 1)))
    else if (any(!is.na(tx))) phr <- c(phr, "En 2023-2024, l'abandon des élèves inscrits reste proche ou en dessous du niveau national dans les trois niveaux scolaires.")
    gf <- a[[CIBLES[["Post-primaire"]][["Filles"]]]] - a[[CIBLES[["Post-primaire"]][["Garçons"]]]]
    if (!is.na(gf) && gf > 2) phr <- c(phr, sprintf("Au collège, <b>les filles abandonnent davantage que les garçons</b> (%s points de plus).", nf(gf, 1)))
    if (nrow(f$survie)) { rg <- match(f$code, survie$code[order(-survie$risque_annuel_bascule)])
      if (rg <= 5) phr <- c(phr, sprintf("<b>Province à surveiller :</b> elle fait partie des 5 provinces encore ouvertes les plus exposées à une bascule (rang %d).", rg)) }
    d <- facteurs_territoire("Primaire", "Ensemble"); d <- d[d$code == f$code, ]; d <- d[order(-d$contribution), ]
    if (nrow(d) && d$contribution[1] >= 1) phr <- c(phr, paste0("Ce qui pèse le plus sur l'abandon au primaire : ", tolower(substr(phrase_facteur(d$facteur[1], d$valeur[1], d$moyenne[1]), 1, 1)), substring(phrase_facteur(d$facteur[1], d$valeur[1], d$moyenne[1]), 2), "."))
    phr
  }
  output$fiche_enbref <- renderUI(tags$ul(lapply(enbref(infos()), function(x) tags$li(HTML(x)))))
  output$f_actions <- renderUI({
    fi <- infos()$fiches
    if (!nrow(fi)) return(div(class = "fiche-action", strong("Suivi courant"), p(style = "margin:4px 0", "Province ouverte et stable : suivre chaque année les écoles en fonctionnement, l'abandon et l'écart entre filles et garçons.")))
    lib <- c("Préventif" = "Prévenir", "Curatif" = "Rouvrir", "Palliatif" = "Scolariser autrement", "Transfert d'expérience" = "S'inspirer de l'expérience")
    fi <- fi[order(match(fi$Statut, c("Curatif", "Palliatif", "Préventif", "Transfert d'expérience")), fi$Priorité), ]
    ok <- function(x) !is.na(x) && nzchar(trimws(x)) && !toupper(trimws(x)) %in% c("NA", "N/A")
    net <- function(x) gsub("[\\s\\p{Z}]+(?=[,)]|$)", "", x, perl = TRUE)
    tagList(lapply(seq_len(nrow(fi)), function(i) { r <- fi[i, ]
      div(class = "fiche-action", strong(lib[[r$Statut]]), p(style = "margin:4px 0", r$`Action recommandée`),
        tags$ul(class = "small mb-0",
          tags$li(strong("Quand : "), r$Échéance), tags$li(strong("Qui : "), net(r$`Responsable principal`), if (ok(r$Partenaires)) paste0(", avec : ", net(r$Partenaires))),
          if (ok(r$`Public prioritaire`)) tags$li(strong("Pour qui : "), r$`Public prioritaire`),
          if (ok(r$`Cible filles`)) tags$li(strong("Pour les filles : "), r$`Cible filles`),
          if (ok(r$`Action rapide (6 mois)`)) tags$li(strong("Dans les 6 mois : "), r$`Action rapide (6 mois)`),
          if (ok(r$`Cible à 12 mois`)) tags$li(strong("Objectif à 12 mois : "), r$`Cible à 12 mois`),
          if (ok(r$`Déclencheur d'escalade`)) tags$li(strong("Alerte si : "), r$`Déclencheur d'escalade`)))
    }))
  })
  output$lecture_f_abandon <- renderUI({ a <- infos()$h %>% filter(annee == ANNEE_AB)
    div(class = "lecture", HTML(sprintf("En 2023-2024 : primaire %s, collège %s, lycée %s (filles et garçons).", pct(a$taux_abandon_cycle_T), pct(a$pp_abandon_moy_6e_4e_T), pct(a$sec_abandon_moy_2nde_1ere_T)))) })
  output$f_abandon <- renderPlotly({
    h <- infos()$h %>% filter(annee %in% ANNEES_ABANDON); p <- plot_ly()
    for (cy in CYCLES) { nm <- c(Primaire = "Primaire", "Post-primaire" = "Collège", Secondaire = "Lycée")[[cy]]
      p <- add_trace(p, x = h$annee, y = round(h[[CIBLES[[cy]][["Ensemble"]]]], 1), type = "scatter", mode = "lines+markers", name = nm,
                     line = list(color = c(BLEU2, ORANGE, ROUGE)[match(cy, CYCLES)], width = 2.5), marker = list(color = c(BLEU2, ORANGE, ROUGE)[match(cy, CYCLES)], size = 6), hovertemplate = paste0(nm, ", %{x} : %{y} %<extra></extra>")) }
    style_graphe(p, "Taux d'abandon (%)") %>% layout(hovermode = "closest")
  })
  output$lecture_f_ouverture <- renderUI({ h <- infos()$h; o <- h$ouverture
    i0 <- which.max(o); i1 <- length(o)
    div(class = "lecture", HTML(sprintf("Écoles en fonctionnement : %s %% en %s, %s %% en 2024-2025.", nf(o[i0], 0), h$annee[i0], nf(o[i1], 0)))) })
  output$f_ouverture <- renderPlotly({
    h <- infos()$h
    plot_ly(h, x = ~annee, y = ~round(ouverture, 1), type = "scatter", mode = "lines+markers", name = "Écoles en fonctionnement", line = list(color = BLEU, width = 3), hovertemplate = "%{x} : %{y} %<extra></extra>") %>%
      style_graphe("Écoles en fonctionnement (%)") %>%
      layout(hovermode = "closest", showlegend = FALSE, shapes = list(list(type = "line", xref = "paper", x0 = 0, x1 = 1, y0 = 70, y1 = 70, line = list(color = ROUGE, dash = "dash", width = 1.5))),
             annotations = list(list(xref = "paper", x = 0.01, y = 70, text = "seuil de crise : 70 %", showarrow = FALSE, yanchor = "bottom", font = list(size = 10, color = ROUGE))))
  })
  output$telecharger_fiche <- downloadHandler(
    filename = function() paste0("DCB26_fiche-province_", gsub("[^A-Za-z]", "", iconv(infos()$nom, "UTF-8", "ASCII//TRANSLIT")), ".pdf"),
    content = function(file) fiche_pdf(file, "province", sel()), contentType = "application/pdf")
  output$telecharger_fiche_region <- downloadHandler(
    filename = function() paste0("DCB26_fiche-region_", gsub("[^A-Za-z]", "", iconv(infos()$region, "UTF-8", "ASCII//TRANSLIT")), ".pdf"),
    content = function(file) fiche_pdf(file, "region", infos()$region), contentType = "application/pdf")
  # ---------------------------------------------------------------- 4. Anticiper
  # prévision 2025-2026 du niveau et du public choisis ; « Ensemble » = moyenne filles et garçons (sans fourchette, qui n'existe que par sexe)
  an_lib <- reactive(paste0(input$an_p - 1, "-", input$an_p))
  output$an_txt <- renderText(paste0(an_lib(), if (input$an_p <= 2026) " (prévision)" else paste0(" (", lib_scen(scen_eff()), ")")))
  scen_eff <- reactive(if (input$an_p <= 2026) "Central" else input$scen_p)   # les scénarios commencent en 2026-2027
  lib_scen <- function(sc) c(Central = "scénario central", Optimiste = "scénario favorable", Pessimiste = "scénario défavorable")[[sc]]
  # valeur attendue de l'année et du scénario choisis ; « Ensemble » = moyenne filles et garçons (sans fourchette, qui n'existe que par sexe)
  pv_sel <- reactive({ cy <- input$cycle_p; sx <- input$sexe_p; a <- an_lib(); sc <- scen_eff()
    src <- if (input$an_p <= 2026) prev %>% filter(annee_scolaire == a) %>% transmute(code_province, cycle, sexe, v = taux_prevu, b1 = borne_inf_80, b2 = borne_sup_80, fiable = statut_fiabilite == "fiable")
           else proj %>% filter(annee_scolaire == a, scenario == sc) %>% transmute(code_province, cycle, sexe, v = abandon_projete, b1 = borne_inf_80, b2 = borne_sup_80, fiable = FALSE)
    obs <- prev %>% filter(annee_scolaire == "2025-2026") %>% select(code_province, cycle, sexe, o = taux_observe_2023_2024)
    fia <- prev %>% filter(annee_scolaire == "2025-2026") %>% select(code_province, cycle, sexe, f0 = statut_fiabilite)
    d <- src %>% left_join(obs, by = c("code_province", "cycle", "sexe")) %>% left_join(fia, by = c("code_province", "cycle", "sexe")) %>%
      filter(cycle == cy, sexe %in% sexes_de(sx)) %>% group_by(code = code_province) %>%
      summarise(attendu = mean(v), constate = mean(o), bas = if (n() == 1) b1 else NA_real_, haut = if (n() == 1) b2 else NA_real_, fiable = all(f0 == "fiable"), .groups = "drop")
    d$nom <- nom(d$code); d$region <- REF$region[match(d$code, REF$code)]; d })
  observeEvent(input$clic_province, { if (identical(input$onglet, "anticiper") && input$clic_province %in% REF$code) updateSelectInput(session, "terr_p", selected = input$clic_province) })
  output$titre_carte_p <- renderText(paste0("Abandon attendu en ", an_lib(), if (input$an_p > 2026) paste0(" (", lib_scen(scen_eff()), ")") else " (prévision)", ", par province"))
  output$titre_evol_p <- renderText(paste0("Les 12 provinces les plus exposées : constaté en 2023-2024, attendu en ", an_lib()))
  output$a_kpi <- renderUI({ d <- pv_sel(); n20 <- sum(d$attendu > 20); s <- surveillance(); fi <- all(d$fiable)
    m24 <- sum(man("2024-2025")); sc <- as.data.frame(scen)
    bas <- sc$Enfants[grepl("même rythme", sc$Indicateur)][1]
    layout_columns(col_widths = c(3, 3, 3, 3), fill = FALSE,
      value_box(title = paste0("Provinces au-dessus de 20 % d'abandon attendues en ", an_lib()), value = paste(n20, "sur 45"), showcase = icon("triangle-exclamation"), showcase_layout = "left center",
                theme = value_box_theme(bg = "#FFFFFF", fg = ENCRE), class = "kpi kpi-rouge", p(class = "small", paste0(nom_niveau(input$cycle_p), ", ", nom_public(input$sexe_p), if (input$an_p > 2026) paste0(", ", lib_scen(scen_eff())) else ""))),
      value_box(title = "Provinces encore ouvertes à surveiller", value = nrow(s), showcase = icon("eye"), showcase_layout = "left center",
                theme = value_box_theme(bg = "#FFFFFF", fg = ENCRE), class = "kpi kpi-ocre", p(class = "small", "risque de fermeture massive des écoles")),
      value_box(title = "Enfants privés d'école primaire en 2030", value = paste0(nf(round(bas, -3) / 1000), " à ", nf(round(m24, -3) / 1000), " mille"), showcase = icon("children"), showcase_layout = "left center",
                theme = value_box_theme(bg = "#FFFFFF", fg = ENCRE), class = "kpi kpi-indigo", p(class = "small", "selon que la baisse récente se poursuit ou non")),
      if (input$an_p > 2026) value_box(title = "Nature du chiffre", value = "Scénario", showcase = icon("route"), showcase_layout = "left center", theme = value_box_theme(bg = "#FFFFFF", fg = ENCRE), class = "kpi kpi-ocre",
                                        p(class = "small", "projection conditionnelle, pas une prévision ; incertitude croissante avec l'horizon"))
      else value_box(title = "Fiabilité de la prévision", value = if (fi) "Bonne" else "Limitée", showcase = icon(if (fi) "circle-check" else "circle-exclamation"), showcase_layout = "left center",
                theme = value_box_theme(bg = "#FFFFFF", fg = ENCRE), class = if (fi) "kpi kpi-vert" else "kpi kpi-ocre",
                p(class = "small", if (fi) "par le passé, 8 à 9 provinces sur 10 dans la fourchette" else "par le passé, 5 à 8 provinces sur 10 dans la fourchette"))) })
  output$lecture_prev <- renderUI({ d <- pv_sel() %>% arrange(desc(attendu)); n20 <- sum(d$attendu > 20)
    hausse <- sum(d$attendu - d$constate >= 2); baisse <- sum(d$constate - d$attendu >= 2)
    note <- if (input$an_p > 2026 && scen_eff() == "Central") " Le scénario central garde le niveau habituel de chaque province : la carte est la même chaque année, seule l'incertitude grandit avec l'horizon. Choisissez « Favorable » ou « Défavorable » pour voir ce qui change."
            else if (input$an_p <= 2026 && input$scen_p != "Central") " Les scénarios commencent en 2026-2027 : pour cette année, la carte montre la prévision." else ""
    div(class = "lecture", HTML(sprintf("<b>À retenir :</b> en %s (%s), %s, %s, %s ; les plus exposées sont %s (%s), %s (%s) et %s (%s). Par rapport à 2023-2024, l'abandon attendu augmente nettement (2 points ou plus) dans %d provinces et baisse nettement dans %d.%s%s",
      an_lib(), if (input$an_p > 2026) lib_scen(scen_eff()) else "prévision", nom_niveau(input$cycle_p), nom_public(input$sexe_p),
      if (n20) sprintf("<b>%d province%s sur 45</b> dépasserai%s le seuil d'alerte de 20 %% d'abandon", n20, if (n20 > 1) "s" else "", if (n20 > 1) "ent" else "t") else "aucune province ne dépasserait le seuil d'alerte de 20 % d'abandon",
      d$nom[1], pct(d$attendu[1]), d$nom[2], pct(d$attendu[2]), d$nom[3], pct(d$attendu[3]), hausse, baisse, note,
      if (input$an_p > 2026) " Scénario conditionnel : à lire comme une hypothèse de travail, pas comme une prévision." else if (all(d$fiable)) " Prévision fiable par le passé à ce niveau." else " <b>Prudence :</b> à ce niveau, l'écart entre prévu et constaté a souvent été important ; utiliser ces chiffres pour classer les provinces plutôt que comme valeurs exactes."))) })
  output$a_prev_carte <- renderPlotly({ d <- pv_sel(); v <- setNames(d$attendu, d$code)[REF$code]
    tp <- setNames(lapply(REF$code, function(c0) { r <- d[d$code == c0, ]
      ec <- r$attendu - r$constate
      paste0("<b>", r$nom, " (", r$region, ")</b><br><b>Constat :</b> abandon attendu en ", an_lib(), if (input$an_p > 2026) paste0(" (", lib_scen(scen_eff()), ")") else "", " : <b>", pct(r$attendu), "</b>",
             if (!is.na(r$bas)) paste0(" (fourchette probable : ", nf(r$bas, 0), " à ", nf(r$haut, 0), " %)") else "", ", contre ", pct(r$constate), " constaté en 2023-2024.<br>",
             { u <- un_eleve_sur(r$attendu); if (grepl("^[aeiou]", u)) paste0("<b>Cela signifie qu'</b>", u) else paste0("<b>Cela signifie que</b> ", u) }, " risquerait de quitter l'école en cours d'année", if (abs(ec) >= 1) sprintf(", soit %s de %s qu'en 2023-2024.", pt(ec), if (ec > 0) "plus" else "moins") else ", un niveau proche de 2023-2024.",
             "<br>", if (r$attendu > 20) "<b style='color:#A8322A'>Au-dessus du seuil d'alerte de 20 % :</b> province à cibler en priorité pour le suivi des élèves et la réinscription." else "Sous le seuil d'alerte de 20 %.",
             if (!r$fiable) "<br><i>Prévision peu fiable à ce niveau : à lire comme un ordre de grandeur.</i>" else "") }), REF$code)
    carte(v, "Abandon attendu (%)", "continu", couleurs = SEQ_RISQUE, lo = 0, hi = 40, format_val = function(x) pct(x, 0), source = "prev",
          survol = tp, titres = setNames(lapply(REF$code, function(c0) ""), REF$code), consigne = "", libelle_valeur = FALSE) })
  output$a_prev_evol <- renderPlotly({ d <- pv_sel() %>% arrange(desc(attendu)) %>% head(12) %>% arrange(attendu)
    y <- factor(d$nom, levels = d$nom); p <- plot_ly()
    for (k in seq_len(nrow(d))) p <- add_segments(p, x = d$constate[k], xend = d$attendu[k], y = y[k], yend = y[k], line = list(color = "#CFC6B4", width = 3), showlegend = FALSE, hoverinfo = "skip")
    p %>% add_markers(x = d$constate, y = y, name = "Constaté 2023-2024", marker = list(color = "#9A968C", size = 10), hovertemplate = "%{y} : %{x:.1f} % constaté en 2023-2024<extra></extra>") %>%
      add_markers(x = d$attendu, y = y, name = paste("Attendu", an_lib()), marker = list(color = ifelse(d$attendu > 20, ROUGE, ORANGE), size = 12, line = list(color = "#fff", width = 1)),
                  hovertemplate = paste0("%{y} : %{x:.1f} % attendu en ", an_lib(), "<extra></extra>")) %>%
      style_graphe(NULL, "Abandon (%)") %>%
      layout(legend = list(orientation = "h", x = 0, y = 1.08), margin = list(l = 105, r = 10),
             shapes = list(list(type = "line", yref = "paper", y0 = 0, y1 = 1, x0 = 20, x1 = 20, line = list(color = ROUGE, dash = "dot", width = 1.5))),
             annotations = list(list(x = 20, y = 1.0, yref = "paper", text = "seuil d'alerte 20 %", showarrow = FALSE, xanchor = "left", font = list(size = 10, color = ROUGE)))) })
  output$dl_prev <- export_csv(function() prev %>% filter(annee_scolaire == "2025-2026") %>% transmute(Province = nom_province, Région = region, Niveau = cycle, Public = sexe,
                                        `Constaté 2023-2024 (%)` = round(taux_observe_2023_2024, 1), `Attendu 2025-2026 (%)` = round(taux_prevu, 1),
                                        `Fourchette basse (%)` = round(borne_inf_80, 1), `Fourchette haute (%)` = round(borne_sup_80, 1), Fiabilité = statut_fiabilite), "previsions-2025-2026")
  # trajectoire 2016-2030 : observé, prévision 2024-2026, scénarios 2026-2030
  traj <- reactive({ cy <- input$cycle_p; sx <- input$sexe_p; t0 <- input$terr_p
    codes <- if (t0 == "BF") REF$code else t0
    h <- panel %>% filter(code %in% codes, annee %in% ANNEES_ABANDON) %>% group_by(annee) %>% summarise(v = mean(.data[[CIBLES[[cy]][[sx]]]], na.rm = TRUE), .groups = "drop")
    pr <- prev %>% filter(code_province %in% codes, cycle == cy, sexe %in% sexes_de(sx)) %>% group_by(annee_scolaire) %>% summarise(v = mean(taux_prevu), .groups = "drop")
    sc <- proj %>% filter(code_province %in% codes, cycle == cy, sexe %in% sexes_de(sx)) %>% group_by(annee_scolaire, scenario) %>% summarise(v = mean(abandon_projete), .groups = "drop") %>%
      tidyr::pivot_wider(names_from = scenario, values_from = v)
    list(h = h, pr = pr, sc = sc, nom = if (t0 == "BF") "le Burkina Faso (moyenne des provinces)" else nom(t0)) })
  output$a_traj <- renderPlotly({ t <- traj(); h <- t$h; pr <- t$pr; sc <- t$sc
    der <- tail(h, 1); xs <- c(pr$annee_scolaire, sc$annee_scolaire)
    p <- plot_ly() %>%
      add_trace(x = c(sc$annee_scolaire, rev(sc$annee_scolaire)), y = c(sc$Pessimiste, rev(sc$Optimiste)), type = "scatter", mode = "none", fill = "toself", fillcolor = "rgba(201,105,42,0.16)",
                name = "Entre scénario favorable et défavorable", hoverinfo = "skip") %>%
      add_trace(x = h$annee, y = round(h$v, 1), type = "scatter", mode = "lines+markers", name = "Constaté", line = list(color = BLEU2, width = 2.5), marker = list(color = BLEU2, size = 6),
                hovertemplate = "%{x} : %{y} % constaté<extra></extra>") %>%
      add_trace(x = c(der$annee, pr$annee_scolaire), y = round(c(der$v, pr$v), 1), type = "scatter", mode = "lines+markers", name = "Prévu (2024-2026)", line = list(color = ORANGE, width = 2.5, dash = "dot"),
                marker = list(color = ORANGE, size = 7), hovertemplate = "%{x} : %{y} % prévu<extra></extra>") %>%
      add_trace(x = c(tail(pr$annee_scolaire, 1), sc$annee_scolaire), y = round(c(tail(pr$v, 1), sc$Central), 1), type = "scatter", mode = "lines", name = "Scénario central (2026-2030)",
                line = list(color = ROUGE, width = 2, dash = "dash"), hovertemplate = "%{x} : %{y} % (scénario central)<extra></extra>") %>%
      add_trace(x = sc$annee_scolaire, y = round(sc$Pessimiste, 1), type = "scatter", mode = "lines", name = "Défavorable", line = list(color = "rgba(168,50,42,0.5)", width = 1), hovertemplate = "%{x} : %{y} % (défavorable)<extra></extra>", showlegend = FALSE) %>%
      add_trace(x = sc$annee_scolaire, y = round(sc$Optimiste, 1), type = "scatter", mode = "lines", name = "Favorable", line = list(color = "rgba(63,138,95,0.6)", width = 1), hovertemplate = "%{x} : %{y} % (favorable)<extra></extra>", showlegend = FALSE)
    a <- an_lib(); se <- scen_eff()
    yv <- if (input$an_p <= 2026) pr$v[pr$annee_scolaire == a] else sc[[se]][sc$annee_scolaire == a]
    if (input$an_p > 2026 && se != "Central") p <- add_trace(p, x = sc$annee_scolaire, y = round(sc[[se]], 1), type = "scatter", mode = "lines", name = paste("Scénario", if (se == "Optimiste") "favorable" else "défavorable"),
                                                             line = list(color = if (se == "Optimiste") VERT else ROUGE, width = 3), hoverinfo = "skip")
    if (length(yv)) p <- add_markers(p, x = a, y = round(yv, 1), name = paste("Année choisie :", a), marker = list(size = 15, color = "#FCD116", line = list(color = ENCRE, width = 2)),
                                     hovertemplate = paste0(a, " : %{y} %<extra></extra>"))
    p %>% style_graphe(NULL, NULL) %>%
      layout(legend = list(orientation = "h", x = 0, y = 1.16), yaxis = list(title = "Abandon (%)", rangemode = "tozero"), margin = list(t = 60),
             xaxis = list(title = "", categoryorder = "array", categoryarray = c(ANNEES_ABANDON, xs), tickangle = -35),
             shapes = list(list(type = "rect", xref = "x", yref = "paper", x0 = tail(ANNEES_ABANDON, 1), x1 = tail(xs, 1), y0 = 0, y1 = 1, fillcolor = "rgba(246,243,236,0.6)", line = list(width = 0), layer = "below"),
                           list(type = "line", xref = "x", yref = "paper", x0 = an_lib(), x1 = an_lib(), y0 = 0, y1 = 1, line = list(color = "#C9A400", width = 1.5, dash = "dot"))),
             annotations = list(list(x = tail(xs, 1), y = 1.02, yref = "paper", text = "projections", showarrow = FALSE, xanchor = "right", font = list(size = 10, color = "#6B6A60")))) })
  output$lecture_traj <- renderUI({ t <- traj(); h <- t$h; sc <- t$sc; pc <- tail(sc$Central, 1)
    av <- mean(h$v[h$annee %in% c("2016-2017", "2017-2018", "2018-2019", "2019-2020")], na.rm = TRUE)
    pic <- h[which.max(h$v), ]
    a <- an_lib(); se <- scen_eff(); yv <- if (input$an_p <= 2026) t$pr$v[t$pr$annee_scolaire == a] else sc[[se]][sc$annee_scolaire == a]
    div(class = "lecture", HTML(sprintf("<b>En %s (%s) :</b> abandon attendu de <b>%s</b>. <b>À retenir :</b> pour %s, %s, %s, l'abandon serait d'environ <b>%s en 2029-2030</b> si la situation reste dans la moyenne des années passées, entre %s (scénario favorable) et %s (scénario défavorable). Avant la crise, il était de %s ; le plus haut niveau observé, %s en %s, coïncide avec les années de fermetures massives d'écoles.",
      a, if (input$an_p > 2026) lib_scen(se) else "prévision", pct(yv), t$nom, nom_niveau(input$cycle_p), nom_public(input$sexe_p), pct(pc), pct(tail(sc$Optimiste, 1)), pct(tail(sc$Pessimiste, 1)), pct(av), pct(pic$v), pic$annee))) })
  output$a_manq <- renderPlotly({ sc <- as.data.frame(scen); m24 <- sc$Enfants[sc$Scénario == "Observé"][1]
    bas <- sc$Enfants[grepl("même rythme", sc$Indicateur)][1]; pal <- sc$Enfants[grepl("radio", sc$Indicateur)][1]
    d <- data.frame(lib = c("2024-2025 (constaté)", "2030 si la baisse récente se poursuit", "2030 si rien ne bouge"), v = c(m24, bas, m24), coul = c(BLEU2, VERT, ROUGE))
    plot_ly(d, y = ~factor(lib, levels = rev(lib)), x = ~v, type = "bar", orientation = "h", marker = list(color = d$coul), text = ~paste0(nf(round(v, -3) / 1000), " mille"), textposition = "outside",
            hovertemplate = "%{y} : %{x:,.0f} enfants<extra></extra>") %>%
      style_graphe(NULL, "Enfants privés d'école primaire") %>%
      layout(margin = list(l = 215, r = 40), yaxis = list(title = ""), xaxis = list(range = c(0, max(d$v) * 1.3), tickformat = ",.0f"), separators = ", ") })
  output$lecture_manq <- renderUI({ sc <- as.data.frame(scen); m24 <- sc$Enfants[sc$Scénario == "Observé"][1]; bas <- sc$Enfants[grepl("même rythme", sc$Indicateur)][1]
    div(class = "lecture", HTML(sprintf("<b>À retenir :</b> %s enfants étaient privés d'école primaire en 2024-2025 du fait de la crise. Si la baisse des deux dernières années se poursuit, ils seraient encore environ <b>%s en 2030</b> : le retour à l'école ne se fera pas seul. Atteindre la moitié des enfants concernés (environ %s) par la radio éducative ou des classes temporaires est la cible d'attente proposée.",
                                        nf(m24), nf(round(bas, -3)), nf(round(sc$Enfants[grepl("radio", sc$Indicateur)][1], -3))))) })
  surveillance <- reactive({
    o24 <- panel %>% filter(annee == "2023-2024") %>% select(code, o24 = ouverture)
    survie %>% arrange(desc(risque_annuel_bascule)) %>% left_join(o24, by = "code") %>% rowwise() %>%
      mutate(raisons = paste(c(if (ecoles_ouvertes_2024_2025 < 80) sprintf("à %s points du seuil de crise", nf(ecoles_ouvertes_2024_2025 - 70, 0)),
                               if (!is.na(o24) && ecoles_ouvertes_2024_2025 - o24 < -5) sprintf("a perdu %s points d'écoles en un an", nf(o24 - ecoles_ouvertes_2024_2025, 0)),
                               if (part_voisines_en_crise > 0) sprintf("%s %% de ses voisines en crise", nf(100 * part_voisines_en_crise, 0))), collapse = " ; ")) %>% ungroup() %>%
      filter(nzchar(raisons)) %>% head(8) %>%
      transmute(code, Province = nom(code), Région = REF$region[match(code, REF$code)], `Écoles en fonctionnement` = ifelse(ecoles_ouvertes_2024_2025 >= 100, "Toutes", paste0(round(ecoles_ouvertes_2024_2025), " %")), `Pourquoi la surveiller` = raisons)
  })
  output$lecture_survie <- renderUI({ s <- surveillance()
    div(class = "lecture", HTML(sprintf("<b>À retenir :</b> %s et %s sont les provinces encore ouvertes les plus exposées à une fermeture massive d'écoles. Classement tiré de l'histoire des 16 provinces qui ont déjà basculé : il indique où regarder en priorité, pas une date.", s$Province[1], s$Province[2]))) })
  output$a_survie <- renderDT({
    d <- surveillance(); tableau(d %>% select(-code), pageLength = 10)
  })
  output$a_signaux <- renderUI(tagList(
    div(class = "lecture", "Dans les 16 provinces qui ont basculé, trois signaux sont apparus avant la fermeture massive des écoles. Les suivre chaque trimestre permet d'agir à temps :"),
    div(class = "message rouge", HTML("<b>1. L'abandon grimpe brutalement.</b> L'année précédant la bascule, il dépassait de 32 points celui des provinces restées stables.")),
    div(class = "message orange", HTML("<b>2. Les classes se remplissent.</b> Le nombre d'élèves par enseignant augmente dès deux ans avant la bascule.")),
    div(class = "message", HTML("<b>3. Des écoles ferment.</b> Une perte de plus de 5 points d'écoles en fonctionnement en un an doit déclencher une mission d'évaluation.")),
    div(class = "lecture", HTML("<b>Et les voisins :</b> une province voisine d'une province en crise a près de 4 fois plus de risques de basculer (14 % par an contre 4 %)."))))

  # ---------------------------------------------------------------- 5. Plan d'action
  output$p_matrice <- renderTable({
    gn <- dec %>% filter(`Part des manquants dans le total hors école` < 50) %>% pull(code)
    rs <- dec %>% filter(`Part des manquants dans le total hors école` >= 70) %>% pull(code)
    data.frame(
      `Situation` = c("Écoles fermées par l'insécurité", "Enfants qui n'ont jamais été à l'école", "Anciens élèves sortis de l'école avec la crise", "Filles qui quittent l'école", "Provinces encore ouvertes mais fragiles", "Capacités des directions régionales mal connues"),
      `Où` = c(paste(sum(ST25$statut == "Crise", na.rm = TRUE), "provinces en crise"), paste(nom(gn), collapse = ", "), paste(nom(rs), collapse = ", "), "Régions où le mariage précoce est fréquent", paste(nom(fiches$code[fiches$Statut == "Préventif"]), collapse = ", "), "Les 16 provinces du plan"),
      `Que faire` = c("Radio éducative, classes temporaires, accueil dans les provinces voisines", "Campagnes de première inscription, en même temps que la réouverture", "Réinscription active, en priorité des filles",
                      "Réinscription des filles et sensibilisation des familles contre le mariage précoce", "Mission d'évaluation et plan de continuité prêt à déclencher", "Audit des moyens avant le déploiement"),
      `Quand` = c("Dès décembre 2026", "Année scolaire 2027-2028", "Dans l'année", "Avant fin mars 2027", "Avant fin mars 2027", "Avant le déploiement"),
      `À savoir` = c("Solution d'attente, pas une scolarisation complète", "Il faut des places d'accueil", "Les enfants déplacés et ceux qui ont abandonné ne peuvent pas être distingués", "Lien observé entre régions, pas au niveau de chaque famille", "Risque de bascule faible mais réel", "Données absentes"),
      check.names = FALSE)
  }, striped = TRUE, hover = TRUE, width = "100%", spacing = "s")
  output$p_fiches <- renderDT({
    lib <- c("Préventif" = "Prévenir", "Curatif" = "Rouvrir", "Palliatif" = "Scolariser autrement", "Transfert d'expérience" = "S'inspirer")
    d <- fiches %>% filter(Statut %in% input$statut_fiche) %>% arrange(factor(Statut, levels = names(lib)), Priorité) %>%
      mutate(ordre = match(Statut, c("Curatif", "Palliatif", "Préventif", "Transfert d'expérience")), Qui = gsub("[\\s\\p{Z}]+(?=[,)]|$)", "", `Responsable principal`, perl = TRUE)) %>%
      group_by(ordre, Statut, `Action recommandée`, Échéance, Qui, `Cible à 12 mois`) %>% summarise(Provinces = paste(nom(code), collapse = ", "), n = n(), .groups = "drop") %>% arrange(ordre, desc(n)) %>%
      transmute(Type = unname(lib[Statut]), `Provinces concernées` = Provinces, `Action recommandée`, Quand = Échéance, Qui, `Objectif à 12 mois` = `Cible à 12 mois`)
    tableau(d, pageLength = 10)
  })
  output$p_cout <- renderUI({
    ci <- cinact
    tagList(
      div(class = "message vert", HTML("<b>Recruter les enseignants nécessaires :</b> 8 187 enseignants, environ <b>19,7 milliards FCFA par an</b>, soit 2,3 % du budget 2026 de l'éducation (environ 47 000 FCFA par enfant et par an).")),
      div(class = "message", HTML("<b>Réaffecter d'abord :</b> 578 enseignants en surnombre à Noumbiel, Léraba et Ioba peuvent être redéployés, avec des garanties de sécurité (7 % du besoin).")),
      div(class = "message orange", HTML("<b>Solution d'attente :</b> la radio éducative coûterait de l'ordre de 7,6 milliards FCFA (environ 16 700 FCFA par enfant), ordre de grandeur à confirmer.")),
      div(class = "message", HTML("<b>Salles de classe :</b> jusqu'à 107 milliards FCFA si toutes devaient être construites, une seule fois et à étaler ; le nombre d'écoles réellement détruites n'est pas connu.")),
      div(class = "message rouge", HTML(sprintf("<b>Coût de l'inaction :</b> entre %s et %s milliards FCFA de revenus que ces enfants perdront au cours de leur vie (estimation haute, en valeur d'aujourd'hui). Scolariser coûte 5 à 27 fois moins cher que ne rien faire.",
                                                nf(ci$`Total (milliards FCFA)`[1]), nf(ci$`Total (milliards FCFA)`[3])))))
  })
  output$p_enseignants <- renderUI({
    b <- budget %>% arrange(`Ordre logique hybride`) %>% head(6)
    tagList(div(class = "lecture", HTML("<b>Méthode retenue, en trois temps :</b>")),
      tags$ol(class = "explication", tags$li("Redéployer les 578 enseignants en surnombre vers les provinces en manque, là où la sécurité le permet."),
              tags$li(HTML(paste0("Recruter et affecter en priorité là où il manque le plus d'enfants à l'école : <b>", paste(nom(b$code), collapse = ", "), "</b>."))),
              tags$li("Traiter à part les classes surchargées (Loroum, Yagha, Séno, Bam : environ 1 263 enseignants) : cela améliore l'enseignement, mais n'ajoute pas d'enfants scolarisés.")),
      div(class = "source", "Calculé sur la base de 51 élèves par enseignant, la valeur médiane observée en 2024-2025 (aucune norme officielle n'a été trouvée)."))
  })

  # ---------------------------------------------------------------- 6. Sources et méthode
  output$m_donnees <- renderUI(tagList(tags$ul(class = "explication",
    tags$li("Statistiques officielles des 45 provinces, de 2015-2016 à 2024-2025 : annuaires de l'éducation (DGESS), projections de population et enquêtes auprès des ménages (INSD), personnes déplacées (CONASUR), limites administratives (COD-AB)."),
    tags$li("Chaque chiffre de la base a été comparé aux publications d'origine : 172 993 valeurs vérifiées, aucun écart."),
    tags$li("Les facteurs de risque affichés sur la carte sont ceux dont le lien avec l'abandon a été retrouvé par au moins deux méthodes d'analyse différentes, pour les filles comme pour les garçons, en période calme comme en période de crise. Leur poids vient d'un modèle de régression estimé sur 2016-2024."),
    tags$li("Les probabilités d'abandon (onglet « Probabilités ») viennent d'une régression logistique adaptée aux parts d'élèves qui abandonnent, estimée sur 2016-2024 avec des incertitudes calculées province par province. Elle a été jugée sur dix critères fixés à l'avance : fiable pour décrire et comparer, pas pour prévoir."),
    tags$li("Le seuil de 70 % d'écoles ouvertes est confirmé par trois méthodes indépendantes. Les prévisions reprennent le niveau habituel de chaque province : les méthodes plus complexes n'ont pas fait mieux."),
    tags$li("Toute la chaîne de calcul se relance par le fichier DCB26_lancer_pipeline.bat ; le détail figure dans la documentation."))))
  output$m_limites <- renderUI(tags$ul(class = "explication",
    tags$li("Les facteurs montrent ce qui accompagne l'abandon ; ils ne prouvent pas qu'en agir sur l'un fera baisser l'abandon."),
    tags$li("Le taux d'abandon ne compte pas les enfants dont l'école a fermé : c'est pourquoi les enfants qui ne sont plus à l'école sont suivis à part."),
    tags$li("Les enfants déplacés et ceux qui ont abandonné sur place ne peuvent pas être distingués."),
    tags$li("La pauvreté est mesurée en 2019 : elle a fortement augmenté au Sahel depuis, et baissé au Centre-Est."),
    tags$li("Les prévisions sont fiables au primaire, moins au collège et au lycée."),
    tags$li("La pauvreté pèse sur l'abandon dans les trois cycles ; mais les facteurs mesurés n'expliquent qu'une partie des écarts (20 à 30 %) : la situation locale compte beaucoup."),
    tags$li("Les coûts sont des ordres de grandeur ; les moyens des directions régionales ne sont pas documentés.")))
}

shinyApp(ui, server)
