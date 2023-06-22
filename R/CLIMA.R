#' Automatizacion de la clasificacion climatica de Koppen modificada por Enriqueta Garcia
#'
#'Regresa el, clima y cultivos que se pueden establecer en tal por su tipo de clima
#' @param temperaturas (vector) datos de la temperatura correspondiente a los doce meses y la temperatura anual
#' @param precipitaciones (vector) datos de la precipitacion correspondiente a los doce meses y la precipitacion anual
#'
#' @return una lista con el clima y cultivos
#' @export
Clima <- function(temperaturas, precipitaciones) {
  T <- temperaturas
  P <- precipitaciones

  clima_asignado <- FALSE

  if (min(T) > 18) {
    if (min(P) > 60) {
      clima <- "Af - Caliente húmedo con lluvias abundantes durante todo el año"
      clima_asignado <- TRUE
    } else {
      if (max(P[5:10]) >= 10 * min(P) & min(P) < 60) {
        clima <- "Aw - Caliente subhúmedo con lluvias en verano"
        clima_asignado <- TRUE
      } else {
        UPMS_plus <- -0.04 * sum(P[1:12]) + 100
        if (min(P) > UPMS_plus) {
          clima <- "Am - Caliente, Húmedo, con lluvias abundantes en verano, con influencia de monzón"
          clima_asignado <- TRUE
        } else {
          clima <- "Aw - Caliente subhúmedo con lluvias en verano"
          clima_asignado <- TRUE
        }
      }
    }
  } else {
    if (max(P[-13]) %in% P[5:10] & max(P[-13]) >= 10 * min(P)) {
      rh <- 2 * T[13] + 28
    } else {
      rh <- 2 * T[13] + 14
      if (rh * 100 > P[13]) {
        clima <- "B - Clima seco"
        re <- (2 * T[13] + 28) / 2
        if (re * 100 >= P[13]) {
          clima <- "Bs - Clima semiárido o estepario"
          clima_asignado <- TRUE
        } else {
          clima <- "Bw - Clima árido o desértico"
          clima_asignado <- TRUE
        }
      } else if (max(P[-13]) %in% P[c(11, 12, 1, 2, 3, 4)] & max(P[-13]) >= 3 * min(P)) {
        rh <- 2 * T[13]
      } else {
        rh <- 2 * T[13] + 14
        if (rh * 100 > P[13]) {
          clima <- "B - Clima seco"
          re <- (2 * T[13] + 14) / 2
          if (re * 100 >= P[13]) {
            clima <- "Bs - Clima semiárido o estepario"
            clima_asignado <- TRUE
          } else {
            clima <- "Bw - Clima árido o desértico"
            clima_asignado <- TRUE
          }
        }
      }
    }

    if (!clima_asignado) {
      clima <- "Cs - Templado subhúmedo con lluvias en invierno o Mediterráneo"
    }
  }

  if (clima == "Af - Caliente húmedo con lluvias abundantes durante todo el año") {
    cultivos <- c("Banana", "Mango", "Cacao")
  } else if (clima == "Aw - Caliente subhúmedo con lluvias en verano") {
    cultivos <- c("Maíz", "Frijol", "Calabaza")
  } else if (clima == "Am - Caliente, Húmedo, con lluvias abundantes en verano, con influencia de monzón") {
    cultivos <- c("Arroz", "Jengibre", "Pimienta")
  } else if (clima == "B - Clima seco") {
    cultivos <- c("Cactus", "Aloe vera", "Olivo")
  } else if (clima == "Bs - Clima semiárido o estepario") {
    cultivos <- c("Espárragos", "Viñedos", "Aceitunas")
  } else if (clima == "Bw - Clima árido o desértico") {
    cultivos <- c("Cactáceas", "Algodón", "Dátil")
  } else if (clima == "Cs - Templado subhúmedo con lluvias en invierno o Mediterráneo") {
    cultivos <- c("Papa", "Cebolla", "Zanahoria")
  } else {
    cultivos <- "Cultivos resistentes a sequía"
  }

  resultado <- list(clima, cultivos)
  return(resultado)
}
