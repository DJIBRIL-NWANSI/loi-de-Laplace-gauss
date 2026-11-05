#' @title Tracé du Graphique de Dispersion et de Conformité
#' @description
#' Cette fonction permet de tracer l'histogramme des données observées
#' ainsi que la courbe de distribution normale théorique de Laplace-Gauss,
#' en y superposant les limites de spécification (LSL et USL).
#'
#' @param x Vecteur numérique des mesures observées.
#' @param lsl Limite de spécification inférieure (Lower Specification Limit).
#' @param usl Limite de spécification supérieure (Upper Specification Limit).
#' @param main Titre principal du graphique (optionnel).
#'
#' @return Génère un graphique de contrôle dans la fenêtre graphique active.
#'
#' @importFrom graphics abline curve hist legend
#' @importFrom stats sd var
#' @export
tracer_dispersion_qc <- function(x, lsl, usl, main = "Analyse de dispersion - Loi de Laplace-Gauss"){
  if(!is.numeric(x)){
    stop("Le vecteur d'entree 'x' doit etre de type numerique.")
  }
  if(lsl>=usl){
    stop("La limite inferieure (lsl) doit etre strictement inferieure a la limite superieure (usl).")
  }

  x <- stats::na.omit(x)

  mu <- mean(x)
  sigma <- sd(x)


  #Calcul des indices de capabilité industrielle Cp et Cpk

  cp <- (usl-lsl)/(6*sigma)
  cpk_inf <- (mu-lsl)/(3*sigma)
  cpk_sup <- (usl-mu)/(3*sigma)
  cpk <- min(cpk_inf, cpk_sup)

  #Tracé graphique de l'histogramme et de la courbe en cloche

  hist(x, probability = TRUE, col = "aliceblue", border = "steelblue", main = "Controle Qualite : Loi de Laplace-Gauss", xlab = "Valeurs mesurees", ylab = "Densite de probabilite")
  #Ajout de la courbe théorique normale
  curve(stats::dnorm(x, mean = mu, sd = sigma), col = "darkblue", lwd = 2, add = TRUE)
  #Lignes verticales pour les limites
  abline(v = c(lsl,usl), col = "red", lty = 2, lwd = 2)
  abline(v = mu, col = "darkgreen", lty = 1, lwd = 2)

  legend("topright",
         legend = c("Courbe Normale", "Limites Usine (LSL/USL)", "Moyenne(mu)"),
         col = c("darkblue", "red", "darkgreen"),
         lwd = 2, lty = c(1,2,1), bty = "n")

  #Retour des métriques de dispersion et de capabiltés
  metriques <- list(etendue = max(x)-min(x),
                    variance = var(x),
                    Cp = cp,
                    Cpk = cpk
                    )
  return(metriques)
}
