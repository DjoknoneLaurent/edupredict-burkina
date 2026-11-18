# ============================================================================================
# Fiche d'une page (PDF A4) pour un directeur régional ou provincial
# Dessinée avec le paquet grid fourni avec R : aucune installation supplémentaire.
# fiche_pdf(fichier, "region", "Est")  ou  fiche_pdf(fichier, "province", "BF5302")
# ============================================================================================
library(grid)

.AB <- tail(ANNEES_ABANDON, 1)                     # dernière année d'abandon observée
.mm <- function(x) unit(x, "mm")

.texte <- function(txt, x, y, largeur, taille = 8.6, gras = FALSE, coul = "#222222", interligne = 1.32) {
  # écrit un paragraphe replié à la largeur (mm) ; renvoie la hauteur utilisée (mm)
  ncar <- floor(largeur / (taille * 0.158))
  lignes <- unlist(lapply(txt, function(t) strwrap(t, width = ncar)))
  h <- taille * 0.3528 * interligne
  for (k in seq_along(lignes)) grid.text(lignes[k], .mm(x), .mm(y - (k - 1) * h), just = c("left", "top"),
                                         gp = gpar(fontsize = taille, fontface = if (gras) "bold" else "plain", col = coul))
  length(lignes) * h
}
.puce <- function(txt, x, y, largeur, taille = 8.6, coul_puce = BLEU, gras_debut = NULL) {
  grid.circle(.mm(x + 1), .mm(y - 1.5), r = .mm(0.7), gp = gpar(fill = coul_puce, col = NA))
  h0 <- 0
  if (!is.null(gras_debut)) { h0 <- .texte(gras_debut, x + 3.5, y, largeur - 3.5, taille, gras = TRUE, coul = "#1B2A38") }
  h0 + .texte(txt, x + 3.5, y - h0, largeur - 3.5, taille)
}
.titre <- function(txt, y) {
  grid.rect(.mm(12), .mm(y), .mm(186), .mm(6.2), just = c("left", "top"), gp = gpar(fill = "#EAF3EC", col = NA))
  grid.rect(.mm(12), .mm(y), .mm(1.4), .mm(6.2), just = c("left", "top"), gp = gpar(fill = BLEU, col = NA))
  grid.text(txt, .mm(15.5), .mm(y - 3.1), just = c("left", "centre"), gp = gpar(fontsize = 10, fontface = "bold", col = BLEU))
  8.5
}
.pastille <- function(niv, x, y, l = 24) {
  grid.roundrect(.mm(x), .mm(y), .mm(l), .mm(4.6), r = unit(0.25, "snpc"), just = c("left", "top"), gp = gpar(fill = BADGE_ALERTE[[niv]], col = NA))
  grid.text(niv, .mm(x + l / 2), .mm(y - 2.3), gp = gpar(fontsize = 7.6, fontface = "bold", col = TEXTE_BADGE[[niv]]))
}
.kpi <- function(x, titre, valeur, note, coul) {
  grid.roundrect(.mm(x), .mm(259), .mm(44), .mm(21), r = unit(0.08, "snpc"), just = c("left", "top"), gp = gpar(fill = "white", col = "#D5DDE6", lwd = 0.8))
  grid.rect(.mm(x), .mm(259), .mm(1.6), .mm(21), just = c("left", "top"), gp = gpar(fill = coul, col = NA))
  grid.text(titre, .mm(x + 4), .mm(255.5), just = c("left", "top"), gp = gpar(fontsize = 7.2, col = "#5B6B7B"))
  grid.text(valeur, .mm(x + 4), .mm(250.6), just = c("left", "top"), gp = gpar(fontsize = 15, fontface = "bold", col = "#1B2A38"))
  grid.text(note, .mm(x + 4), .mm(242.2), just = c("left", "top"), gp = gpar(fontsize = 6.6, col = "#5B6B7B"))
}

# ---------------------------------------------------------------------------------- données
.donnees_fiche <- function(niveau, id) {
  codes <- if (niveau == "region") REF$code[REF$region == id] else id
  st <- ST25 %>% filter(code %in% codes)
  alerte <- vapply(codes, function(c0) { s <- statut_affiche(st[st$code == c0, ], c0); if (is.na(s)) "Normal" else s }, "")
  ab <- panel %>% filter(annee == .AB, code %in% codes)
  pv <- prev %>% filter(annee_scolaire == "2025-2026", cycle == "Primaire", code_province %in% codes) %>% group_by(code_province) %>% summarise(t = mean(taux_prevu), .groups = "drop")
  man <- em %>% filter(annee_scolaire == "2024-2025", code %in% codes)
  fi <- fiches %>% filter(code %in% codes) %>% arrange(Priorité)
  fac <- if (niveau == "region") facteurs_territoire("Primaire", "Ensemble", "region") %>% filter(region == id) else facteurs_territoire("Primaire", "Ensemble") %>% filter(code == id)
  list(codes = codes, st = st, alerte = alerte, ab = ab, pv = pv, man = man, fi = fi, fac = fac[order(-abs(fac$contribution)), ])
}

# ---------------------------------------------------------------------------------- fiche
fiche_pdf <- function(fichier, niveau = c("region", "province"), id) {
  niveau <- match.arg(niveau); D <- .donnees_fiche(niveau, id)
  region <- if (niveau == "region") id else REF$region[REF$code == id]
  titre <- if (niveau == "region") paste("Région", id) else paste0(nom(id), " (région ", region, ")")
  if (capabilities("cairo")) grDevices::cairo_pdf(fichier, width = 210 / 25.4, height = 297 / 25.4, family = "sans")
  else grDevices::pdf(fichier, width = 210 / 25.4, height = 297 / 25.4, encoding = "ISOLatin9")
  on.exit(grDevices::dev.off())
  grid.newpage()

  # en-tête
  grid.rect(0, .mm(297), .mm(210), .mm(32), just = c("left", "top"), gp = gpar(fill = DRAP_ROUGE, col = NA))
  grid.rect(0, .mm(265), .mm(210), .mm(1.6), just = c("left", "top"), gp = gpar(fill = DRAP_VERT, col = NA))
  grid.text("\u2605", .mm(12), .mm(285), just = c("left", "centre"), gp = gpar(fontsize = 17, col = DRAP_JAUNE))
  grid.text(titre, .mm(20), .mm(285), just = c("left", "centre"), gp = gpar(fontsize = 19, fontface = "bold", col = "white"))
  grid.text(if (niveau == "region") "Fiche d'aide à la décision pour la direction régionale de l'éducation" else "Fiche d'aide à la décision pour la direction provinciale de l'éducation",
            .mm(12), .mm(277), just = c("left", "centre"), gp = gpar(fontsize = 9.5, col = "#FFE9E9"))
  grid.text("Écoles : 2024-2025 · Abandon : 2023-2024 (dernière année connue)", .mm(12), .mm(271), just = c("left", "centre"), gp = gpar(fontsize = 7.6, col = "#FFD6D6"))
  niv_max <- names(LIB_ALERTE)[min(match(D$alerte, names(LIB_ALERTE)))]
  grid.text("Niveau d'alerte", .mm(198), .mm(287), just = c("right", "centre"), gp = gpar(fontsize = 7.6, col = "#FFE9E9"))
  grid.roundrect(.mm(169.3), .mm(284.2), .mm(29.4), .mm(6), r = unit(0.25, "snpc"), just = c("left", "top"), gp = gpar(fill = "white", col = NA))
  .pastille(niv_max, 170, 283.5, 28)

  # chiffres clés
  n_al <- sum(D$alerte != "Normal"); o <- mean(D$st$ouverture, na.rm = TRUE)
  txp <- moy_ponderee(D$ab$taux_abandon_cycle_T, D$ab$effectif)
  txf <- moy_ponderee(D$ab[[CIBLES[["Primaire"]][["Filles"]]]], D$ab$effectif); txg <- moy_ponderee(D$ab[[CIBLES[["Primaire"]][["Garçons"]]]], D$ab$effectif)
  .kpi(12, if (niveau == "region") "Provinces en alerte" else "Situation des écoles", if (niveau == "region") paste(n_al, "sur", length(D$codes)) else niv_max,
       if (niveau == "region") "crise, vigilance ou à surveiller" else c(Crise = "moins de 70 % des écoles ouvertes", Vigilance = "plus de 5 points perdus en un an", "À surveiller" = "proche du seuil ou très exposée", Normal = "pas d'alerte")[[niv_max]], BADGE_ALERTE[[niv_max]])
  .kpi(59, "Écoles en fonctionnement", paste0(nf(min(o, 100), 0), " %"), if (o > 100) "plus d'écoles qu'avant la crise" else "par rapport à avant la crise (2017-2019)", if (o < 70) ROUGE else if (o < 80) ORANGE else VERT)
  .kpi(106, "Enfants privés d'école primaire", nf(sum(D$man$manquants, na.rm = TRUE)), "2024-2025 (estimation)", ORANGE)
  .kpi(153, "Abandon au primaire", pct(txp), sprintf("filles %s ; garçons %s", pct(txf), pct(txg)), BLEU2)
  y <- 233

  # en bref
  y <- y - .titre("En bref", y)
  phr <- c()
  ncr <- sum(D$alerte == "Crise")
  if (niveau == "region") {
    phr <- c(phr, if (ncr > 0) sprintf("%d province(s) sur %d en crise : moins de 70 %% des écoles y fonctionnent (%s). La priorité est la réouverture et une solution d'attente pour les enfants privés d'école.",
                                      ncr, length(D$codes), paste(nom(D$codes[D$alerte == "Crise"]), collapse = ", "))
                  else "Aucune province en crise : toutes ont au moins 70 % de leurs écoles en fonctionnement.")
    sv <- D$codes[D$alerte %in% c("Vigilance", "À surveiller")]
    if (length(sv)) phr <- c(phr, sprintf("À surveiller de près : %s (proche du seuil de 70 %%, baisse récente ou forte exposition à une bascule).", paste(nom(sv), collapse = ", ")))
  } else {
    phr <- c(phr, if (o < 70) sprintf("Situation de crise : seulement %s %% des écoles d'avant la crise fonctionnent. Priorité : rouvrir ou relocaliser les écoles.", nf(o, 0))
                  else if (D$alerte[[1]] == "Vigilance") sprintf("Vigilance : %s %% des écoles d'avant la crise fonctionnent, mais la province en a perdu plus de 5 points en un an. Une mission d'évaluation s'impose.", nf(min(o, 100), 0))
                  else if (o < 80) sprintf("Attention : %s %% des écoles fonctionnent, soit seulement %s points au-dessus du seuil de crise de 70 %%.", nf(o, 0), nf(o - 70, 0))
                  else sprintf("%s %% des écoles d'avant la crise fonctionnent : pas de crise d'ouverture.", nf(min(o, 100), 0)))
    r <- rang_de(id); if (!is.na(r) && r <= 5) phr <- c(phr, sprintf("Province encore ouverte parmi les 5 plus exposées à une fermeture massive d'écoles (rang %d) : préparer un plan de continuité.", r))
  }
  if (!is.na(txf) && !is.na(txg) && abs(txf - txg) >= 1) phr <- c(phr, sprintf("Au primaire, les %s abandonnent davantage (%s points d'écart en 2023-2024).", if (txf > txg) "filles" else "garçons", nf(abs(txf - txg), 1)))
  abn <- panel %>% filter(annee == .AB) %>% group_by(code) %>% summarise(t = moy_ponderee(taux_abandon_cycle_T, effectif), .groups = "drop") %>% pull(t) %>% median(na.rm = TRUE)
  tot <- sum(D$fac$contribution); phr <- c(phr, if (tot <= -1 && !is.na(txp) && txp >= abn + 2) sprintf("Son abandon au primaire (%s) dépasse nettement celui d'une province typique (%s), alors que ses caractéristiques habituelles (pauvreté, accès à l'école) le placeraient sous la moyenne : des facteurs liés à la crise sont à examiner sur le terrain.", pct(txp), pct(abn))
                                         else if (tot >= 1) sprintf("Compte tenu de ses caractéristiques, le risque d'abandon au primaire y est plus élevé que la moyenne nationale (environ %s points).", nf(tot, 1))
                                         else if (tot <= -1) sprintf("Compte tenu de ses caractéristiques, le risque d'abandon au primaire y est plus faible que la moyenne nationale (environ %s points).", nf(-tot, 1))
                                         else "Compte tenu de ses caractéristiques, le risque d'abandon au primaire y est proche de la moyenne nationale.")
  for (p in phr) y <- y - .puce(p, 13, y, 184) - 0.8
  y <- y - 2

  # tableau des provinces (fiche région) ou historique (fiche province)
  if (niveau == "region") {
    y <- y - .titre("Les provinces de la région", y)
    col <- c(13, 45, 75, 105, 137, 172); ent <- c("Province", "Niveau d'alerte", "Écoles en fonct.", "Enfants hors école", "Abandon primaire 2023-24", "Prévu 2025-26")
    for (k in seq_along(ent)) grid.text(ent[k], .mm(col[k]), .mm(y), just = c("left", "top"), gp = gpar(fontsize = 7.4, fontface = "bold", col = "#5B6B7B"))
    y <- y - 5; ordre <- D$codes[order(match(D$alerte, names(LIB_ALERTE)), D$st$ouverture[match(D$codes, D$st$code)])]
    for (c0 in ordre) {
      grid.lines(.mm(c(12, 198)), .mm(c(y + 1, y + 1)), gp = gpar(col = "#E3E8EE", lwd = 0.6))
      ab0 <- D$ab$taux_abandon_cycle_T[D$ab$code == c0]; pv0 <- D$pv$t[D$pv$code_province == c0]; o0 <- D$st$ouverture[D$st$code == c0]
      v <- c(nom(c0), "", if (o0 >= 100) "Toutes" else paste0(nf(o0, 0), " %"), nf(D$man$manquants[D$man$code == c0]), pct(ab0), if (length(pv0)) pct(pv0) else "n.d.")
      for (k in c(1, 3:6)) grid.text(v[k], .mm(col[k]), .mm(y), just = c("left", "top"), gp = gpar(fontsize = 8.2, fontface = if (k == 1) "bold" else "plain", col = "#222222"))
      .pastille(D$alerte[[c0]], col[2], y + 0.6, 24); y <- y - 5.6
    }
    y <- y - 1.5
  } else {
    y <- y - .titre("Abandon des élèves inscrits, au fil des années (filles et garçons)", y)
    h <- panel %>% filter(code == id, annee %in% ANNEES_ABANDON) %>% arrange(annee)
    an <- intersect(c("2016-2017", "2018-2019", "2020-2021", "2021-2022", "2022-2023", .AB), h$annee)
    col <- 13 + c(0, 30 + 22 * (seq_along(an) - 1)); grid.text("Niveau", .mm(col[1]), .mm(y), just = c("left", "top"), gp = gpar(fontsize = 7.4, fontface = "bold", col = "#5B6B7B"))
    for (k in seq_along(an)) grid.text(an[k], .mm(col[k + 1]), .mm(y), just = c("left", "top"), gp = gpar(fontsize = 7.4, fontface = "bold", col = "#5B6B7B"))
    grid.text("Prévu 2025-26", .mm(col[length(col)] + 22), .mm(y), just = c("left", "top"), gp = gpar(fontsize = 7.4, fontface = "bold", col = "#5B6B7B"))
    y <- y - 5
    for (cy in CYCLES) {
      grid.lines(.mm(c(12, 198)), .mm(c(y + 1, y + 1)), gp = gpar(col = "#E3E8EE", lwd = 0.6))
      grid.text(c(Primaire = "Primaire", "Post-primaire" = "Collège", Secondaire = "Lycée")[[cy]], .mm(col[1]), .mm(y), just = c("left", "top"), gp = gpar(fontsize = 8.2, fontface = "bold"))
      for (k in seq_along(an)) grid.text(pct(h[[CIBLES[[cy]][["Ensemble"]]]][h$annee == an[k]]), .mm(col[k + 1]), .mm(y), just = c("left", "top"), gp = gpar(fontsize = 8.2))
      pv0 <- prev %>% filter(annee_scolaire == "2025-2026", cycle == cy, code_province == id) %>% pull(taux_prevu)
      grid.text(if (length(pv0)) pct(mean(pv0)) else "n.d.", .mm(col[length(col)] + 22), .mm(y), just = c("left", "top"), gp = gpar(fontsize = 8.2, col = BLEU))
      y <- y - 5.6
    }
    grid.lines(.mm(c(12, 198)), .mm(c(y + 1, y + 1)), gp = gpar(col = "#E3E8EE", lwd = 0.6))
    grid.text("Écoles ouvertes", .mm(col[1]), .mm(y), just = c("left", "top"), gp = gpar(fontsize = 8.2, fontface = "bold"))
    for (k in seq_along(an)) { o1 <- h$ouverture[h$annee == an[k]]; grid.text(if (length(o1) && !is.na(o1)) (if (o1 >= 100) "Toutes" else paste0(nf(o1, 0), " %")) else "n.d.", .mm(col[k + 1]), .mm(y), just = c("left", "top"), gp = gpar(fontsize = 8.2)) }
    y <- y - 5.6
    y <- y - .texte("Écoles ouvertes : part des écoles d'avant la crise (2017-2019) qui fonctionnent ; sous 70 %, la province est en crise.", 13, y - 0.5, 184, taille = 7, coul = "#5B6B7B") - 3
  }

  # facteurs
  y <- y - .titre("Ce qui pèse sur l'abandon au primaire, d'après nos modèles", y)
  aggr <- D$fac[D$fac$contribution >= 0.5, ]; attn <- D$fac[D$fac$contribution <= -0.5, ]
  eff <- function(c) sprintf("%s %s point%s d'abandon", if (c >= 0) "ajoute" else "retire", nf(abs(c), 1), if (abs(c) >= 2) "s" else "")
  if (nrow(aggr)) for (i in seq_len(nrow(aggr))) y <- y - .puce(paste0(phrase_facteur(aggr$facteur[i], aggr$valeur[i], aggr$moyenne[i]), " : ", eff(aggr$contribution[i]), "."), 13, y, 184, coul_puce = ROUGE) - 0.6
  if (nrow(attn)) for (i in seq_len(nrow(attn))) y <- y - .puce(paste0(phrase_facteur(attn$facteur[i], attn$valeur[i], attn$moyenne[i]), " : ", eff(attn$contribution[i]), "."), 13, y, 184, coul_puce = VERT) - 0.6
  if (!nrow(aggr) && !nrow(attn)) y <- y - .puce("Aucun facteur ne s'écarte nettement de la moyenne nationale.", 13, y, 184) - 0.6
  y <- y - .texte("Points rouges : facteurs qui augmentent le risque ici ; points verts : facteurs qui le réduisent. Ce sont des liens observés, pas des causes prouvées.", 16.5, y - 0.5, 180, taille = 7, coul = "#5B6B7B") - 2.5

  # actions
  y <- y - .titre("Que faire en priorité ?", y)
  lib <- c("Préventif" = "Prévenir", "Curatif" = "Rouvrir", "Palliatif" = "Scolariser autrement", "Transfert d'expérience" = "S'inspirer")
  if (nrow(D$fi)) {
    fi <- D$fi %>% arrange(factor(Statut, levels = c("Curatif", "Palliatif", "Préventif", "Transfert d'expérience")), Priorité)
    gr <- split(fi, paste(fi$Statut, fi$`Action recommandée`)); gr <- gr[order(sapply(gr, function(g) match(g$Statut[1], c("Curatif", "Palliatif", "Préventif", "Transfert d'expérience"))))]
    for (g in gr) {
      if (y < 40) break
      ou <- if (niveau == "region") paste0(" (", paste(nom(g$code), collapse = ", "), ")") else ""
      y <- y - .puce(paste0(g$`Action recommandée`[1], ou, ". Quand : ", tolower(substr(g$Échéance[1], 1, 1)), substring(g$Échéance[1], 2), ". Qui : ", gsub("[[:space:] ]+$", "", g$`Responsable principal`[1], perl = TRUE),
                             ". Objectif à 12 mois : ", tolower(substr(g$`Cible à 12 mois`[1], 1, 1)), substring(g$`Cible à 12 mois`[1], 2), "."), 13, y, 184, gras_debut = paste0(lib[[g$Statut[1]]], " :")) - 0.9
    }
  } else y <- y - .puce("Pas d'action spécifique prévue : suivi courant des écoles en fonctionnement, de l'abandon et de l'écart entre filles et garçons.", 13, y, 184) - 0.9
  y <- y - 1.5

  # signaux
  if (y > 36) {
    y <- y - .titre("Signaux d'alerte à suivre chaque trimestre", y)
    s3 <- c("une nouvelle baisse de plus de 5 points des écoles en fonctionnement, ou un passage sous 70 % ;",
            "une hausse brutale de l'abandon, ou des classes qui se remplissent (plus d'élèves par enseignant) ;",
            "une province voisine qui bascule en crise : le risque de bascule est alors près de 4 fois plus élevé.")
    for (p in s3) { if (y < 22) break; y <- y - .puce(p, 13, y, 184, taille = 8.2) - 0.4 }
  }

  # pied de page
  grid.lines(.mm(c(12, 198)), .mm(c(15, 15)), gp = gpar(col = "#C9D3DD", lwd = 0.6))
  .texte("Sources : annuaires statistiques de l'éducation (DGESS), INSD, CONASUR, COD-AB ; calculs du projet. Le taux d'abandon ne compte pas les enfants dont l'école a fermé : ils sont suivis à part (enfants privés d'école).",
         12, 13, 128, taille = 6.6, coul = "#5B6B7B")
  grid.text("Data Challenge Burkina 2026 · Défi 1", .mm(198), .mm(11.5), just = c("right", "centre"), gp = gpar(fontsize = 6.6, col = "#5B6B7B"))
  invisible(fichier)
}
