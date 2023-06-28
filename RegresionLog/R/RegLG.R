#' Regresión Logistica
#'
#' Realiza una regresión logistica
#'
#' @param xp (vector) muestra de datos de la variable predictora
#' @param x (vector) variable predictora
#' @param y (vector) muestra de la variable respuesta
#' @param clase_nombre nombre de la variable dependiente
#'
#' @return un data frame con las probabilidades requeridas
#' @export
#'
#' @examples
#'\dontrun{
#' #Directorio de trabajo
#' ruta <- "/Users/jg289/Downloads/dataset1.csv"
#'
#' #primer ejemplo
#' datos <- read.csv(ruta)
#' #Cargo la libreria
#' library(RegresionLog)
#' regl(substr(df$evi,1,100),df$evi,substr(df$presencia,1,100),"presencia")
#' }
regl <- function(xp,x, y, clase_nombre) {

  # Convertir los vectores x e y a tipo numérico
  x <- as.numeric(x)
  y <- as.numeric(y)

  # iteraciones y tasas de aprendizaje
  itr <- 1000
  tsa <- 0.001

  # Función para ajustar el modelo de regresión logística
  Bs <- function(xp, y, itr, tsa) {
    n <- length(y)
    B0 <- 0
    B1 <- 0
    for (iter in 1:itr) {
      ni <- B0 + B1 * x
      probabilidad <- 1 / (1 + exp(-ni))
      error <- y - probabilidad
      B0 <- B0 + tsa * sum(error) / n
      B1 <- B1 + tsa * sum(error * x) / n
    }
    modelo <- list(B0 = B0, B1 = B1)
    print(modelo)
    return(modelo)
  }

  # Ajustar el modelo de regresión logística
  modelo <- Bs(x, y, itr, tsa)

  # Función para calcular la probabilidad
  calcular_probabilidad <- function(x) {
    ni <- modelo$B0 + modelo$B1 * x
    probabilidad <- 1 / (1 + exp(-ni))
    return(probabilidad)
  }

  # Función para predecir la clase
  predict_clas <- function(x) {
    ni <- modelo$B0 + modelo$B1 * x
    probabilidad <- 1 / (1 + exp(-ni))
    clase <- ifelse(probabilidad >= 0.5, 1, 0)
    return(clase)
  }

  resultados <- data.frame(probabilidad = predict_clas(x))
  colnames(resultados)[1] <- clase_nombre
  return(resultados)
}
