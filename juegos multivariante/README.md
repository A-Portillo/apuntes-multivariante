# Data Arcade

Colección Shiny de seis juegos para introducir estadística multivariante mediante geometría, intuición y series competitivas. Los cuatro primeros reinterpretan las actividades de Di Iorio y Vantini; **Cuatro sombras** y **Vecinos en fuga** son juegos nuevos.

## Cómo funciona la partida

Al abrir la aplicación, el jugador elige un alias. La aplicación crea una semilla aleatoria exclusiva para esa sesión y genera todas sus rondas a partir de ella.

- No hay usuarios, base de datos ni servicios externos.
- El marcador existe únicamente en la memoria de la pestaña actual.
- Recargar la página o introducir otro alias comienza una sesión nueva.
- Cada ronda puntúa una sola vez.
- El cronómetro comienza al entrar en una ronda y no se reinicia al repetir un intento.
- El tiempo usa la misma regla en los seis juegos y puede descontar como máximo un 10%.
- Pedir la solución bloquea inmediatamente la ronda: no permite seguir jugando ni reiniciar y concede 0 puntos.
- La puntuación máxima de la colección es 25.000.

Esto permite desplegar la aplicación en el plan gratuito de shinyapps.io sin depender de Supabase, SQL ni almacenamiento persistente.

## Series de juegos

| Juego | Rondas | Variación entre rondas | Máximo |
|---|---:|---|---:|
| Centro perfecto | 4 | mezclas, anillo, arco y cruz con ruido gaussiano | 4.000 |
| Constelaciones | 4 | `k = 2, 3, 4, 5` en orden aleatorio | 4.000 |
| Dimensión perdida | 4 | nubes elípticas, curvas, ramificadas y con atípicos | 4.000 |
| Premonición | 4 | pendientes, varianza, heterocedasticidad y curvatura distintas | 4.000 |
| Cuatro sombras | 4 | círculo, cuadrado, triángulo y hexágono regular | 4.000 |
| Vecinos en fuga | 5 | 2, 5, 10, 20 y 50 dimensiones con paletas variables | 5.000 |

Los datasets se generan de nuevo al comenzar cada sesión. Todas las formas incorporan perturbaciones gaussianas, por lo que no se puede memorizar una coordenada o recta concreta.

### Regla común de tiempo

Primero se calcula la puntuación base por precisión, entre 0 y 1000. Al cerrar cualquier ronda se aplica exactamente el mismo multiplicador:

\[
P_{final}=\operatorname{redondear}\left[P_{base}\left(0.9+0.1e^{-t/45}\right)\right],
\]

donde \(t\) es el número de segundos desde que se abrió la ronda. El multiplicador vale 1 al comenzar y nunca baja de 0,9. Cambiar de pantalla o reiniciar la colocación no reinicia el reloj; solo lo hace el paso a una ronda nueva.

### k-means deliberadamente ambiguo

Las rondas de clustering incluyen grupos anisótropos y de tamaños distintos, solapamiento y observaciones puente. El objetivo no es adivinar etiquetas “verdaderas”, sino colocar `k` centroides que minimicen la inercia, que es exactamente el criterio utilizado por k-means.

### Alta dimensionalidad

El jugador debe recorrer obligatoriamente las cinco dimensiones. Cada ronda cambia el origen, sentido y pequeñas desviaciones de su paleta. Dentro de una columna la codificación sigue siendo coherente para poder comparar el objetivo y los candidatos, pero los colores no se pueden aprender de una ronda a otra.

Las reglas de los juegos añadidos están desarrolladas en [juegos_nuevos.md](juegos_nuevos.md).

## Ejecutar en local

Requiere R y tres paquetes de CRAN:

```r
install.packages(c("shiny", "bslib", "htmltools"))
shiny::runApp()
```

## Desplegar gratis en shinyapps.io

1. Crea una cuenta en shinyapps.io y copia el token mostrado en **Tokens**.
2. Ejecuta en R:

   ```r
   install.packages("rsconnect")

   rsconnect::setAccountInfo(
     name = "TU-CUENTA",
     token = "TU-TOKEN",
     secret = "TU-SECRETO"
   )

   rsconnect::deployApp(appName = "data-arcade")
   ```

3. Comparte la URL generada.

No es necesario configurar variables de entorno, ficheros de credenciales ni una base de datos. El plan gratuito puede suspender una instancia inactiva; al volver a abrir la aplicación se iniciará normalmente otra sesión.

## Dinámica de aula sugerida

1. Todos introducen un alias y comienzan al mismo tiempo.
2. Se fija un intervalo para completar la colección o una selección de juegos.
3. Cada estudiante enseña su marcador final o entrega una captura.
4. Se comparan estrategias antes de usar **Rendirse y ver solución**, ya que esa acción fija irrevocablemente la ronda en 0 puntos.
5. En `Vecinos en fuga`, se anotan los contrastes de 2 y 50 dimensiones para discutir la concentración de distancias.

El marcador no es una prueba inviolable: vive en una sesión Shiny y está pensado para motivación y conversación en el aula, no para calificación de alto impacto.

## Estructura

```text
app.R                  interfaz, recorridos y marcador de sesión
R/game_logic.R         generadores aleatorios y puntuaciones
R/plotting.R           visualizaciones de los juegos 2D
R/ui_helpers.R         componentes de interfaz
www/app.js             sólido 3D, navegación y temporizador
www/styles.css         diseño adaptable
tests/testthat/        pruebas estadísticas y de servidor
```

## Créditos

- Jacopo Di Iorio y Simone Vantini (2021), *How to Get Away With Statistics: Gamification of Multivariate Statistics*, DOI: [10.1080/26939169.2021.1997128](https://doi.org/10.1080/26939169.2021.1997128).
- Código de referencia de los cuatro juegos 2D: [JacopoDior/htgaws](https://github.com/JacopoDior/htgaws).

La interfaz, los generadores multirronda y los juegos nuevos de este repositorio son implementaciones nuevas.
