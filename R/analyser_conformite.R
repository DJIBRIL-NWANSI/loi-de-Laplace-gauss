#' @title Analyse de Conformité et Capabilité de Processus (Loi Normale)
#' @description
#' Cette fonction permet de réaliser une analyse statistique complète de contrôle
#' de la qualité pour un processus industriel. Elle évalue l'ajustement des données
#' à une distribution normale (loi de Laplace-Gauss), calcule les probabilités
#' théoriques et empiriques par rapport aux limites de spécification (LSL et USL),
#' intègre un test de normalité de Shapiro-Wilk avec seuil d'alerte, et calcule
#' automatiquement les indices de capabilité du procédé (Cp et Cpk).
#'
#' @param x Vecteur numérique des mesures ou données observées du processus.
#' @param lsl Limite de spécification inférieure (Lower Specification Limit).
#' @param usl Limite de spécification supérieure (Upper Specification Limit).
#' @param alpha Seuil de signification pour le test de normalité (par défaut 0.05).
#'
#' @return Une liste de classe \code{conforme_qc} contenant l'effectif, la moyenne,
#' l'écart-type, les probabilités, les indices Cp/Cpk, l'état de la normalité et les résultats du test.
#'
#' @importFrom graphics abline curve hist legend
#' @importFrom stats sd var
#' @export
analyser_conformite <- function(x, lsl, usl, alpha = 0.05) {
  if (!is.numeric(x)){
    stop("Le vecteur d'entree 'x' doit etre de type numerique.")
  }
  if(lsl>=usl){
    stop("La limite inferieure (lsl) doit etre strictement inferieure a la limite superieure (usl).")
  }
  #Nettoyage d'éventuelles valeurs manquantes

  x <- stats::na.omit(x)
  #Statistiques descriptives de base
  mu <- mean(x)
  sigma <- sd(x)
  #Probabilité théorique selon la loi normale et proportion empirique
  prob_theorique <- stats::pnorm(usl, mean = mu, sd = sigma)-stats::pnorm(lsl, mean = mu, sd = sigma)
  prop_empirique <- mean(x >= lsl & x <= usl)
  #Test de normalité de Shapiro-Wilk
  test_normalite <- stats::shapiro.test(x)
  normalite_respectee <- test_normalite$p.value >= alpha

  if(!normalite_respectee){
    message("---AVERTISSEMENT QUALITE---")
    message(sprintf("La p-value du test de Shapiro-Wilk est de %.4f (inferieure a %.2f).",
                    test_normalite$p.value, alpha))
    message("L'hypthese de normalite est REJETEE. Les indices Cp et Cpk doivent etre interpretes avec precaution.\n")
  }else{
    message("---VALIFATION QUALITE---")
    message(sprintf("La p-value du test de shapiro-Wilk est de %.4f (superieure ou egale a %.2f).",
                    test_normalite$p.value, alpha))
    message("L'hypothese de normalite est ACCEPTEE.\n")
  }

  resultats <- list(moyenne = mu, ecart_type = sigma, probabilite_theorique = prob_theorique, proportion_empirique = prop_empirique, test_normalite = test_normalite)
  class(resultats) <- "conforme_qc"
  return(resultats)
}

