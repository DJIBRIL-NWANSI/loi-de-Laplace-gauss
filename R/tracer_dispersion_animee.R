tracer_dispersion_animee <- function(x, lsl, usl, pas = 5){
  if (lsl >= usl) stop("La limite inferieure (lsl) doit etre strictement inferieure a la limite superieure(usl)")

  x <- stats::na.omit(x)
  n <- length(x)

  if (n < pas) stop("Le nombre de mesures est inferieure au pas specifie.")

  xlim_val <- c(min(x, lsl) - 1, max(x, usl) + 1)

  mu_total <- mean(x)
  sigma_total <- sd(x)
  max_densite <- stats::dnorm(mu_total, mean = mu_total, sd = sigma_total) * 1.3

  message("Generation de l'animation...")

  for (i in seq(pas, n, by = pas)) {
    sub_x <- x[1:i]
    mu <- mean(sub_x)
    sigma <- sd(sub_x)

    graphics::hist(sub_x, probability = TRUE, xlim = xlim_val, ylim = c(0, max_densite),
                   main = paste("Controle Qualite Temps Reel (", i, "/", n, "echantillons"),
                   xlab = "Valeurs mesurees", ylab = "Densite",
                   col = "lightblue", border = "white")

    if(!is.na(sigma) && sigma > 0){
      x_curve <- seq(xlim_val[1], xlim_val[2], length.out = 100)
      y_curve <- stats::dnorm(x_curve, mean = mu, sd = sigma)
      graphics::lines(x_curve, y_curve, col = "darkblue", lwd = 2)


    }

    graphics::abline(v = lsl, col = "red", lty = 2, lwd = 2)
    graphics::abline(v = usl, col = "red", lty = 2, lwd = 2)
    graphics::abline(v = mu, col = "green", lty = 1, lwd = 2)

    Sys.sleep(0.08)

  }
}
