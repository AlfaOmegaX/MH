# Descripción del problema
Sean $e_1, \ldots, e_N$ ($N$ nº de datos) una muestra de objetos ya clasificados donde cada objeto pertenece a una de las clases $C_i, \ldots, C_M$ ($M$ nº de clases); y cada objeto $d_i$ tiene ascociado un vector $(x_{1_i}, \ldots, x_{n_i}) \in \mathbb{R}^n$ ($n$ nº de características/atributos); luego hacemos la asociación y representamos cada $e_i$ como el vector de atributos asociado.

Nuestro objetivo es obtener una función $f : \mathbb{R}^n \rightarrow \{C_1, \ldots, C_M\}$ llamada **Clasificador**, que asocie correctamente cada objeto $e_i$ con su clase correspondiente $C_j$.

Como ya conocemos a priori las clases existentes que queremos clasificar y sabemos la clase a la que pertenece cada objeto del conjunto de datos, estamos claramente en una situación de aplicar **aprendizaje supervisado** para obtener este clasificador que buscamos.

De entre las muchas técnicas usaremos el método **K-NN (k-nearest neighbors)**, que lo que hace es clasificar la clase de un punto $p$ según los k puntos más cercanos a $p$ con la distancia euclidea (junto a la Hamming si son atributos nominales pero asumimos que son todos atributos numéricos por la simplicidad).
Una vez encontrados estos puntos, se obtiene la clase que predomina en esos k puntos y se asigna esa clase al punto $p$. Obviamente para evitar problemas con la escala de cada atributo se suelen normalizar todas los atributos al intervalo [0, 1], así ninguna variable influye más que otra en la distancia.

Obviamente este clasificador tiene en cuenta todos los atributos, sin embargo sabemos que a la hora de clasificar objetos no tiene que darse el caso de que todas los atributos importen lo mismo a la hora de clasificar; es decir, hay atributos que pueden ser mas decisivos que otros para saber de que clase es el objeto (puede haber atributos que no influyan en nada o muy poco, atributos que realmente dependan de otros que ya estén incluidos...).

Por ello vamos a asignar un valor $w_i \in [0,1], \ i \in \{1, \ldots, n\}$ a cada carcterística y representamos los pesos como $W = \{w_1, \ldots, w_n\}$. Por tanto nuestro problema es en encontrar unos "buenos" pesos (más adelante se explica cual es el criterio de bondad), es decir el problema de **Aprendizaje de Pesos en Características (APC)**. Es decir ahora a la hora de calcular las distancias euclideas tendremos en cuenta el peso: $$d_e(e_1, e_2) = \sqrt{\sum_i w_i \cdot (e^i_i - e^i_2)^2}$$.

Para obtener nuestra solución a los algoritmos que van a buscar nuestra solución al problema necesitamos un conjunto de entrenamiento y tambien otro de prueba para evaluar como de buena es la solución obtenida. Para obtener estos conjuntos usaremos **k-fold cross validation**, que consiste en dividir el conjunto total de datos en k particiones disjuntas del mismo tamaño manteniendo la distribución de clases equilibrada (para que sea una muestra representativa del total), entonces utilizaremos el algoritmo k veces obteniendo k soluciones tomando cada vez una partición distinta como conjunto de prueba y agrupando el resto de particiones como conjunto de entrenamiento. Finalmente la calidad será la media de los resultados de las k soluciones.

En nuestro caso vamos a usar **1-NN** y **5-fold cross validation** (el 5 y 10-fold cross validation son los "mejores" para validar clasificadores asi que la elección es buena; por otro lado el usar 1-NN o cualquier otro k-NN habría que verlo repitiendo los resultados con distintas k y viendo cuales ofrecen mejores resultados).

La **función de evaluación** (como de buena es la solución obtenida) va a ser $$F(W) = \alpha \cdot tasa_{clas}(W) + (1-\alpha) \cdot tasa_{clas}(W)$$ donde $\alpha \in [0,1]$ es el que pondera cual de dos valores es más importante y:

  - $tasa_{clas} = 100 \cdot \dfrac{\text{instancias bien clasificadas en T}}{N}$ es la tasa de clasificación: indica el porcentaje de acierto del clasificador, la tasa de clases correctamente asignadas al conjunto de prueba T.
  Cuanto más alto se dice que el clasificador es mas **preciso**.

  - $tasa_{red} = 100 \cdot \dfrac{|\{w_i : w_i < 0,2\}|}{n}$ es la tasa de reducción: indica el nº de características descartadas, es decir las características que no influyen casi nada a la hora de clasificar porque tienen pesos muy cercanos a 0.
  Cuanto mas alto más **simple** es el clasificador.

En nuestro caso usaremos $\alpha = 0,5$ queriendo así obtener soluciones que sean buenas clasificando con el mínimo número de características posible.

# Descripción de la aplicación de los algoritmos empleados al problema

Antes de explicar los algoritmos realizados, describiré los tipos, métodos comunes... de todos los algoritmos. Como anotación, en algunas funciones no se especificarán algunas cosas completamente para evitar obscurecer el pseudocódigo; y en cosas menos importantes como la lectura de datos o creación de particiones no se incluye pseudocódigo al no ser el objetivo de la práctica.

## Distribución del codigo
El código se encuentra distribuido en diferentes módulos:

- `Base`: los tipos de estructura para todos los módulos y unas pocas funciones básicas
- `Main`: código principal, manejo general
- `Lectura`: para leer los datos del fichero y crear la estructura de datos
- `CrossV`: forma las particiones de CrossValidation
- `KNN`: implementa clasificador 1nn
- `Ejecutar`: ejecuta los algoritmos y formatea los resultados
- `Utils`: funciones auxiliares comunes
- `P1`: algoritmos de la P1
- `P2`: algoritmos de la P2

## Notación de pseudocódigo
He usado la explicación de Pablo Baeyens Fernández (disponible en [GitHub](https://github.com/mx-psi/metaheuristicas)) que también hizo las prácticas en Haskell y se entiende bastante bien para gente nueva a este tipo de notación:

Usaré notación parecida a la usada en Haskell para que se entiendan mejor las cosas aunque será una versión muy simplificada ya que hay cosas como para generar números aleatorios que se complican en Haskell que omitiré. Los argumentos se pasan a las funciones separados por espacios y el tipo de la función se indica después de su nombre seguido de dos dobles puntos `::`. Además para evitar poner paréntesis se puede cambiar por `$`, es decir `f(g(z * y))` equivale a `f $ g $ z * y`.

Aclaro que todo se "pasa" por valor, no por referencia ni por punteros, luego se asume como en C++ como si fuera todo paso por valor, por tanto el resultado de una operacion no se puede alterar.

Para mostrar el estilo del pseudocódigo incluyo un ejemplo de pseudocódigo para una función en C:

```C
int suma(int a, int b){
  int c = a + b;
  return c;
}
```
Y su equivalente en el estilo de pseudocódigo que voy a utilizar:

```haskell
suma :: Int → Int → Int
suma a b = c
  where c = a + b
```
Otra alternativa sería:

```haskell
suma :: Int → Int → Int
suma a b =
  let c = a + b
  in c
```

También explico algunas funciones que se usan bastante:

- `juntaCon f l1 l2 ... ln` toma n listas y una función de n argumentos y devuelve una lista tal que la posición `i` tiene el elemento `f l1_i l2_i ... ln_i`
- `map f [x1, ..., xn]` toma una función `f` y una lista `[x1, ..., xn]` y devuelve la lista `[f x1, ..., f xn]`
- `acumula (·) i [x1, ..., xn]` acumula los elementos de la lista usando la función `·`. Devuelve: `i·x1·x2···xn`.
- `\x1 x2 ... xn → expr` es una función lambda (sin nombre) que toma `x1 x2 ... xn` como argumentos y devuelve `expr`
- `repite n x` crea una lista de n copias de `x`.

## Representación de datos

- Un **atributo** se representa con `Double`.
- Un **vector de atributos** (`Punto`) se representa con `Vector Double`.
- Una **clase** (`Clase`) se representa con `String`.
- Un **dato** (`Dato`) se representa como una tupla `(Punto, Clase)`.
- Un **conjunto de datos** ( `Datos`) se representa como `[Dato]` (lista de `Dato`), pudiera ser un conjunto de entrenamiento, prueba, o cualquier agrupación de datos.
- Una **partición** (`Particion`) se representa como una tupla con el conjunto de entrenamiento y el de prueba `(Datos, Datos)`, se refiere a una de las agrupaciones obtenidas de aplicar el k-fold cross validation, no solo a una de las particiones en las que se dividen el conjunto total de datos.
- Un **conjunto de particiones** (`Particiones`) se representa como una `[Particion]`.
- Un **algoritmo** (`Algoritmo`) se representa como una función `Datos -> Pesos` que toma el conjunto de entrenamiento y devuelve la solución (los pesos).

Para la búsqueda local implemento un tipo extra llamado `Solucion` que encapsula la solución propiamente dicha (los pesos) y guarda su valor de la función objetivo (para evitar evaluarla de nuevo en comparaciones) y también el nº de vecinos que ha creado que se tendrá en cuenta para la condición de parada. Además hay otro tipo especial llamado `Estado a` que he tenido que realizar debido a como funciona Haskell, en términos muy sencillos lo que representa es una función que parte de un estado `s` a una tupla `(a,s)` es decir pasa de un estado a otro y devuelve un resultado, en este caso el estado siempre es fijo pero lo que devuelve puede variar según queramos.

Aqui el tipo `Solucion`:

```haskell
data Solucion = Solucion {
  getPesos :: Pesos,
  getFit :: Double,
  getNVecinos :: Int
}
```

Y el tipo `Estado a`:

```haskell
type Estado a = State (StdGen, Int) a
```
El estado será una tupla de un generador de nº aleatorios y un número que representará el nº de evaluaciones de la función objetivo a lo largo de la ejecución del algoritmo de búsqueda local. Tengo que llevar un generador como estado debido a la transparencia referencial de Haskell (una función siempre devuelve lo mismo con los mismos parámetros de entrada) por lo que para ir generando nº aleatorios un generador devuelve un nº aleatorio y un nuevo generador; en cualquier caso esa es la idea general, que tengo un estado **global** por decirlo de alguna manera al que puedo acceder, realizar cosas y devolver otro estado nuevo.


## Lectura del archivo .arff y normalización

La lectura del archivo es simple: primero se ignora todo hasta que se llega `@data`. Entonces para cada linea leo todos los valores excepto el último y creo un `Punto` con esos valores y con el último formo la `Clase` y con ambos ya tengo un `Dato`.

Paso un filtro para eliminar valores repetidos ya que entiendo que queremos entrenar el clasificador para que pueda clasificar valores **nuevos**, el hecho de dejar valores repetidos y que caigan en particiones distintas va a ocasionar que acierte siempre y realmente no nos dice nada nuevo.

A la hora de aplicar el algoritmo en conjuntos de datos no vistos si observamos un dato que es idéntico (en todos sus atributos) a un dato que ya tenemos clasificado en nuestra base de datos obviamente podemos decir que tienen la misma clase y ya hemos acabado. Si no fuese así entonces es que al menos existe un atributo desconocido que no hemos medido en ninguno de nuestros datos y nos encontrariamos un problema mucho mayor por lo que por simplificar la cuestión vamos a eliminar los repetidos para que no metan ruido.

Ahora se procede a normalizar los datos en el intervalo $[0,1]$ aplicando la función que ya se conoce. Si para un atributo la diferencia entre su maximo y su minimo es 0 se entiende entonces que el atributo es **constante** y por tanto no aporta nada a la hora de clasificar por lo que se normalizan a 0 todos sus valores.

Finalmente se aplica un `shuffle` para cambiar el orden a la hora de crear las particiones.

## Creación de particiones
Para crear las particiones queremos que mantegan una proporción equilibrada de clases, entonces primero dividimos los datos agrupándolos por su clase con `separarClases`. Al resultado se aplica un `shuffle` para  y pasamos el resultado a `eleccionCruzada` (k-fold cross validation) con $k = 5$.

La creación de las particiones es simple: para cada lista de datos de clase se divide en k particiones iguales junto al resto que va aparte, y después se juntan partición a partición y los restos se unen. Si quedasen restos entonces se mezclarían, se dividirían en k particiones y se juntarían partición a partición con el resultado anterior; hasta que o bien no quedasen o bien no se pudieran dividir entre k cuando entonces se repartirían uniformemente (uno a cada partición).

Este criterio intenta mantener un **equilibrio de clases** a la vez que mantiene que las k particiones tengan el **mismo tamaño** excepto restos.

## 1-NN
La clasificación k-nn con $k=1$ anteriormente explicada, implementada con la modificación **APC** para tener en cuenta los pesos. Toma el conjunto de prueba, el de entrenamiento y los pesos y devuelve el % de acierto clasificando los puntos del conjunto de prueba.

Para ello para cada punto del conjunto de prueba se le aplica `dist2P` con todos los puntos del conjunto de entrenamiento (excluyendo el punto al que se le está aplicando para los casos donde el conjunto de entrenamiento sea igual que el de prueba ~ **leave-one-out**, y como ya he quitado repetidos al procesar los datos no hay ningun problema al hacer esto) y obtenemos la clase del punto con menor distancia y el acierto será si coincide esta clase con la del conjunto de prueba.

```haskell
clas1nn :: Datos -> Datos -> Pesos -> Float
clas1nn train test pesos =
  let distanciasA p = map (\x -> dist2P x p pesos) (quita p train)
      aciertos = map (\p -> claseDe p == claseDe $ min $ distanciasA p) test
  in nAciertos aciertos / sizeOf aciertos
```

Cabe mencionar que como solo estamos buscando el mínimo de la distancia y no nos interesa su valor real, como $f(x) = \sqrt{x}, \ \forall x \in \mathbb{R}$ es una función creciente no afecta el hecho de calcular el máximo sin la raíz cuadrada (y nos ahorramos calculos para mejorar el tiempo de ejecucción):

```haskell
-- Distancia euclidea considerando pesos
dist2P :: Dato -> Dato -> Pesos -> Float
dist2P p1 p2 pesos = suma $ juntaCon (\x y w -> w * (y - x) * (y - x)) p1 p2 pesos
```

## Función objetivo
Como ya se ha explicado la función objetivo en la descripción del problema. En general se pasan la tasa de reducción y la tasa de acierto y tenemos que:

```haskell
-- Función objetivo
fEvaluacion :: Float -> Float -> Float -> Float
fEvaluacion alpha tAcier tRed = alpha * tAcier + (1 - alpha) * tRed
```

Existe una variante para cuando se quiere evaluar f sobre el conjunto de entrenamiento:

```haskell
evaluarF :: Datos -> Pesos -> Float
evaluarF datos pesos =
  let pReduccion = selecciona (< 0.2) pesos / sizeOf pesos
      pAcierto   = clas1nn datos datos (reducePesoss pesos)
  in fEvaluacion 0.5 pAcierto pReduccion
```

## Resultados de algoritmos
Tendremos una lista de distintos algoritmos que queremos aplicarles todos nuestras particiones y obtener los resultados, entonces para cada algoritmo y cada partición se ejecuta el algoritmo con esa partición que nos devuelve unos pesos y el tiempo tardado en obtenerlos. Vemos el porcentaje de reducción y reducimos los pesos para poder aplicar 1-NN y ya obtenemos el porcentaje de acierto y podemos sacar el valor de la función objetivo. Cuando tenemos las 5 particiones hacemos los valores medios y pasamos al siguiente algoritmo.

Finalmente se escribe en el archivo "nombreFichero_resultados.txt" los resultados de todos los algoritmos con las 5 particiones y los valores medios.
