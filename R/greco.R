TablaAnova<-function(Respuesta, Tratatamiento, Filas, Columnas, LetrasGR, data){
  # y^2/k^2
  sumtotalRESPUESTA <- sum(RESPUESTA)
  k <- nlevels(TRATAMIENTO)
  s <- sumtotalRESPUESTA^2 / k^2

  # Calculamos grados de libertad, suma de cuadrado, cuadrado medio, F y significancia
  # de tratamiento, filas, columnas, letras griegas, error y total

  # TRATAMIENTO
  gl_tto <- k - 1
  sum_tto <- tapply(RESPUESTA, INDEX = TRATAMIENTO, FUN = sum)
  n_tto <- tapply(RESPUESTA, INDEX = TRATAMIENTO, FUN = length)
  sc_tto <- sum(sum_tto^2 / n_tto) - s
  cm_tto <- sc_tto / gl_tto

  # FILAS
  gl_filas <- k - 1
  sum_filas <- tapply(RESPUESTA, INDEX = FILAS, FUN = sum)
  n_filas <- tapply(RESPUESTA, INDEX = FILAS, FUN = length)
  sc_filas <- sum(sum_filas^2 / n_filas) - s
  cm_filas <- sc_filas / gl_filas

  # COLUMNAS
  gl_columnas <- k - 1
  sum_columnas <- tapply(RESPUESTA, INDEX = COLUMNAS, FUN = sum)
  n_columnas <- tapply(RESPUESTA, INDEX = COLUMNAS, FUN = length)
  sc_columnas <- sum(sum_columnas^2 / n_columnas) - s
  cm_columnas <- sc_columnas / gl_columnas

  # LETRAS GRIEGAS
  gl_letrasgr <- k - 1
  sum_letrasgr <- tapply(RESPUESTA, INDEX = LETRASGR, FUN = sum)
  n_letrasgr <- tapply(RESPUESTA, INDEX = LETRASGR, FUN = length)
  sc_letrasgr <- sum(sum_letrasgr^2 / n_letrasgr) - s
  cm_letrasgr <- sc_letrasgr / gl_letrasgr

  # TOTAL
  gl_total <- k^2 - 1
  sc_total <- sum(RESPUESTA^2) - s

  # ERROR
  gl_error <- (k - 1) * (k - 3)
  sc_error <- sc_total - sc_tto - sc_filas - sc_columnas - sc_letrasgr
  cm_error <- sc_error / gl_error

  # F
  F_tto <- cm_tto / cm_error
  F_filas <- cm_filas / cm_error
  F_columnas <- cm_columnas / cm_error
  F_letrasgr <- cm_letrasgr / cm_error

  # SIGNIFICANCIA
  p_value_tto <- pf(F_tto, gl_tto, gl_error, lower.tail = FALSE)
  p_value_filas <- pf(F_filas, gl_filas, gl_error, lower.tail = FALSE)
  p_value_columnas <- pf(F_columnas, gl_columnas, gl_error, lower.tail = FALSE)
  p_value_letrasgr <- pf(F_letrasgr, gl_letrasgr, gl_error, lower.tail = FALSE)

  # CREAR EL DATAFRAME
  tabla <- data.frame(
    FUENTE = c("Tratamiento", "Filas", "Columnas", "Letras Griegas", "Error", "Total"),
    G_de_l = c(gl_tto, gl_filas, gl_columnas, gl_letrasgr, gl_error, gl_total),
    SC = c(sc_tto, sc_filas, sc_columnas, sc_letrasgr, sc_error, sc_total),
    CM = c(cm_tto, cm_filas, cm_columnas, cm_letrasgr, cm_error, NA),
    F = c(F_tto, F_filas, F_columnas, F_letrasgr, NA, NA),
    SIGNIFIC = c(p_value_tto, p_value_filas, p_value_columnas, p_value_letrasgr, NA, NA),
    check.names = FALSE
  )
  rownames(tabla) <- NULL
  anava <- format(tabla)
  anava[is.na(tabla)] <- ""

  return(anava)
}
