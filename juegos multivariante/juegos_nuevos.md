# Juegos nuevos

Este documento describe los dos juegos añadidos a los cuatro propuestos en el paper. Ambos producen una puntuación entre 0 y 1000; una puntuación mayor es mejor.

## 1. Cuatro sombras

### Concepto que introduce

Proyecciones, cambio de punto de vista y pérdida de información al reducir una figura tridimensional a dos dimensiones. Sirve como puente visual hacia PCA: una proyección puede conservar un aspecto del objeto y ocultar por completo otros.

### El objeto

El sólido no es un truco gráfico. Se define como la intersección

\[
S=\{(x,y,z): x^2+z^2\leq 1,\ -1\leq y\leq 1,\ |x|\leq(1-y)/2\}.
\]

La primera condición forma un cilindro y la última un prisma triangular. Sus proyecciones ortogonales son exactamente:

- un **círculo** al mirar a lo largo del eje `y`;
- un **cuadrado** al mirar a lo largo del eje `x`;
- un **triángulo** al mirar a lo largo del eje `z`.

La app dibuja su malla y su sombra en un `canvas`, sin depender de una librería 3D externa.

La cuarta ronda usa un segundo sólido conocido: un cubo proyectado a lo largo de una diagonal espacial produce una silueta de hexágono regular. Esta proyección clásica aparece documentada en [MathWorld](https://mathworld.wolfram.com/RegularHexagon.html); no se han añadido otras siluetas arbitrarias.

### Cómo se juega

1. La serie recorre círculo, cuadrado y triángulo con el sólido compuesto. La cuarta ronda cambia a un cubo y pide su proyección hexagonal clásica, observándolo a lo largo de una diagonal espacial.
2. El jugador arrastra el objeto para girarlo. La sombra actual aparece junto al objeto y el contorno discontinuo indica el objetivo.
3. Cuando ambas formas parecen coincidir, pulsa **Evaluar**.
4. Se puede seguir ajustando la orientación y evaluar otra vez antes de cerrar la ronda. Al cerrarla aparece la figura siguiente.

El doble clic o **Reiniciar orientación** devuelve el objeto a su posición inicial. El objetivo solo cambia después de puntuar la ronda actual.

### Puntuación

Sea \(\theta\) el menor ángulo, entre 0° y 90°, entre la dirección de la cámara y el eje que produce la silueta objetivo. La puntuación base es:

\[
P_{base}=\operatorname{redondear}\left[1000e^{-\theta/12}\right].
\]

Al cerrar la ronda se aplica la penalización temporal común descrita al final de este documento. Una orientación exacta produce 1000 puntos base.

### Discusión posterior

- ¿Cómo puede un objeto producir descripciones aparentemente incompatibles?
- ¿Qué información se pierde al pasar de 3D a 2D?
- ¿Por qué una proyección elegida deliberadamente puede ser útil a pesar de esa pérdida?

## 2. Vecinos en fuga

### Concepto que introduce

La **concentración de distancias**, una manifestación de la maldición de la dimensionalidad. En muchas distribuciones de alta dimensión, las distancias al vecino más próximo y al más lejano se vuelven relativamente parecidas. Por eso nociones como “cerca” y “lejos” pierden capacidad para discriminar y métodos basados en distancia —k-NN, clustering o búsqueda por similitud— pueden degradarse.

### Cómo se juega

1. La serie comienza con 2 dimensiones y avanza obligatoriamente por 5, 10, 20 y 50.
2. La fila **OBJETIVO** contiene un valor entre 0 y 1 en cada dimensión, codificado por color.
3. Las filas A–F son seis candidatos. Hay que elegir el que tenga menor distancia euclídea al objetivo usando **todas** las columnas.
4. Tras responder se muestran las seis distancias, la solución y el contraste relativo.
5. Cierra la ronda para avanzar a la siguiente dimensión. El resultado individual fluctúa, pero el contraste tiende a disminuir al aumentar la dimensión.

Los valores exactos aparecen al mantener el cursor encima. En cada ronda se aleatorizan el origen, sentido y desplazamientos de la paleta. La codificación se mantiene constante dentro de una columna para que la comparación siga siendo válida.

### Puntuación

Sea \(d_*\) la menor de las seis distancias y \(d_e\) la distancia del candidato elegido:

\[
P_{base}=\operatorname{redondear}\left[1000\frac{d_*}{d_e}\right].
\]

Elegir el vecino correcto produce la máxima exactitud. Elegir un candidato casi tan cercano como el óptimo conserva buena parte de la puntuación, precisamente porque en alta dimensión varias distancias pueden ser muy similares. Después se aplica la misma penalización temporal que en el resto de los juegos.

El indicador que aparece tras responder es

\[
C=\frac{d_{\max}-d_{\min}}{d_{\min}}.
\]

Un contraste pequeño significa que “el más lejano” y “el más cercano” están separados por poco en términos relativos.

### Discusión posterior

- ¿Ha sido más fácil reconocer el vecino en 2 o en 50 dimensiones?
- ¿Qué ocurre con el contraste al repetir varias rondas?
- Si casi todas las distancias son parecidas, ¿qué problemas puede tener k-means?
- ¿Ayudaría seleccionar variables o reducir dimensión antes de buscar vecinos?

## Marcador de sesión

Los dos juegos aportan, respectivamente, hasta 4.000 y 5.000 puntos al marcador. La semilla se crea al comenzar la sesión, de modo que cada jugador recibe datos y variaciones nuevas. No se guarda información fuera de la pestaña y revelar una solución en los juegos que lo permiten concede 0 puntos en esa ronda.

## Penalización temporal común

Todos los juegos muestran un cronómetro y aplican al cerrar la ronda el mismo multiplicador:

\[
P_{final}=\operatorname{redondear}\left[P_{base}\left(0.9+0.1e^{-t/45}\right)\right].
\]

El tiempo puede reducir como máximo un 10% de la puntuación base. Comienza al entrar en la ronda, continúa si se cambia de pantalla y no vuelve a cero al reiniciar un intento. Solo comienza un cronómetro nuevo al avanzar de ronda. En los cuatro juegos que ofrecen **Rendirse y ver solución**, pulsar ese botón fija irrevocablemente la ronda en 0 puntos y bloquea nuevos intentos antes de mostrar la solución.
