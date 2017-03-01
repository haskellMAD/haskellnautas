---
title: Programación Aplicativa
author: pepegar
extract: Estamos acostumbrados a trabajar con mónadas pero, ¿estamos acostumbrados a trabajar con aplicativos?
---

> Este post es una traducción al castellano del que se puede encontrar en http://pepegar.github.io/2016/09/applicative-style-programming.html

Programación Aplicativa
=======================

En este post vamos a ver qué son los funtores aplicativos, un poco más
que los funtores, un poco menos que las mónadas. Y, si tenemos mónadas,
¿para qué queremos funtores? La respuesta es que al proveer menos
funcionalidad, los podremos usar en más ocasiones.

Este post intenta complementar la charla que dimos en HaskellMAD, puedes
ver los slides de la charla en este enlace

Los funtores aplicativos se introdujeron en [un
artículo](http://strictlypositive.org/Idiom.pdf) en [The Journal of
Functional
Programming](https://www.cambridge.org/core/journals/journal-of-functional-programming).

Lo que tenemos que entender es que los funtores aplicativos, como los
funtores, las mónadas o los monoides, son una *typeclass*. Ésta es su
definición:

``` {.haskell}
class Functor f => Applicative f where
    pure :: a -> f a
    (<*>) :: f (a -> b) -> f a -> f b
```

Muy bien, ¿y qué es lo que tenemos aquí? Básicamente podemos ver que
*Applicative* es una interfaz que declara dos funciones. La primera de
ellas `pure`, permite meter elementos dentro de nuestro contexto
aplicativo. La otra `<*>`, es una funcion infija que permite aplicar la
una funcion `a -> b` contenida en un contexto aplicativo a un valor `a`
contenido en el mismo contexto, dando como resultado un valor `f
b`. Después de el trabalenguas, una aclaración, podemos pensar en los
funtores aplicativos como aplicación de funciones dentro de un contexto.

Veamos un ejemplo sencillo del uso de Applicative.

``` {.haskell}
Prelude> let square = \ x -> x * x
Prelude> let plusTwo = \ x -> x + 2
Prelude> let functions = [square, plusTwo]
Prelude> let numbers = [1,2,3]
Prelude> functions <*> numbers
[1,4,9,3,4,5]
```

En el ejemplo vemos cómo las funciones contenidas dentro de la lista
`functions` son aplicadas a la lista `numbers` y el resultado es
concatenado. La librería estandard de Haskell nos ofrece instancias para
la mayoría de tipos de datos, y se importan automáticamente en
`Prelude`, por eso no necesitamos hacer ningún import para que el
ejemplo funcione.

De todos modos, no necesitamos ser genios para ver que una lista de
funciones no es el ejemplo más util del mundo. Veamos un ejemplo más
interesante:

Validación
----------

La validación de datos es un problema que todos, como programadores, nos
hemos encontrado. Tenemos datos que van a entrar a nuestro dominio, pero
antes de hacerlo queremos comprobar que son válidos. Al comprobar si un
valor de tipo `a` es válido, nuestros validadores devolverán el valor en
caso de que lo sea, o un error de tipo `Err` en caso de que no lo sea.
Dado que estos validadores devolverán un tipo u otro, vamos a usar el
tipo de datos `Either` de Haskell.

Para entenderlo un poco, `Either` es un tipo suma. Esto quiere decir que
los el número de valores posibles para un dato del tipo `Either a
b` es el número de valores posibles de `a`, mas el número de valores
posibles de `b`.

``` {.haskell}
data Person = Person {
  name :: String,
  age :: Int 
} deriving Show

data Err = String

validateName :: String -> Either Err String
validateName n = if (onlyLetters n) then Right n
                                    else Left "El nombre debe contener letras únicamente"

validateAge :: Int -> Either Err Int
validateAge a = if a > 18 then Right a
                          else Left "Tienes que ser mayor de 18 años"
```

En nuestro ejemplo, tenemos un tipo de datos \`Person\`, que representa
a una persona con nombre y edad. Y tenemos dos validadores, uno para
cada componente de este tipo de datos.

Ahora, con nuestros dos validadores, podemos crear una validación para
`Person` si usamos notación `do`:

``` {.haskell}
validatePersonM :: String -> Int -> Either String Person
validatePersonM n a = do
    vName <- validateName n
    vAge <- validateAge a
    return $ Person vName vAge
```

Lo que estamos haciendo aquí es muy sencillo. Dado que Either provee una
instancia de *Monad*, podemos utilizar la notación `do` para secuenciar
operaciones monádicas sobre este tipo de datos. Pero, hay una cosa que
debemos preguntarnos: ¿Realmente queremos que nuestras operaciones sean
secuenciales? Cuando la respuesta a esta pregunta sea *No*, sabemos que
estamos ante un claro caso de UIDM, o *uso indiscriminado de Monad*.

Para resolver nuestro *UIDM*, y sabiendo que `Either` aparte de proveer
una instancia de mónada, provee otra para `Applicative`, podemos cambiar
nuestras operaciones secuenciales a operaciones aplicativas.

``` {.haskell}
validatePersonA :: String -> String -> Either String Person
validatePersonA n l = Person <$> validateName n <*> validateLastName l
```

Pero, ¿cómo funciona esta función? Bueno, simplemente hay que entender
que el operador `<$>` es un alias *infijo* para el `fmap` de los
funtores. Es decir, la parte izquierda del operador es mapeada sobre el
contenido de nuestro funtor.

En esta sesión de GHCi lo podemos ver mejor:

``` {.haskell}
Prelude> :t Person
Person :: String -> String -> Person
Prelude> :t Person <$> validateName "pepe"
Person <$> validateName "pepe" :: Either String (String -> Person)
Prelude> :t Person <$> validateName "pepe" <*> validateLastName "Garcia"
Person <$> validateName "pepe" <*> validateLastName "Garcia" :: Either String Person
```

Creando nuestros propios aplicativos
------------------------------------

Para usar toda la magia de `Applicative` con el tipo de datos que
nosotros queramos, sólamente hay que dar una instancia de la typeclass:

``` {.haskell}
data Maybe a = Just a
             | Nothing


instance Applicative Maybe where
    pure = Just

    Just f  <*> m = fmap f m
    Nothing <*> _ = Nothing
```

Esto nos permitirá usar `Maybe` como un aplicativo, pero hay otros
casos, como por ejemplo, ¿cómo puedo hacer para usar toda la magia de
los aplicativos sobre un `ADT` que hayamos creado? En Haskell existe una
técnica para esto que se denomina *\~Free Applicatives\~*. En este caso,
*Free* se refiere a gratis, no a libre, y es así porque nos da una
instancia de `Applicative` gratuíta para nuestro ADT.

### Free Applicatives

Los `Free Applicative` son una abstracción sobre `Applicative`, y como
funcionan básicamente, es elevando a representaciones de tipos las
funciones que expone la typeclass de `Applicative`.

Puedes investigar más sobre la implementación [en
Github](https://github.com/ekmett/free), pero básicamente, los
`Free Applicative` se definen así:

``` {.haskell}
data Ap f a where
    Pure :: a -> Ap f a
    Ap   :: f a -> Ap f (a -> b) -> Ap f b

instance Functor (Ap f) where
  fmap f (Pure a)   = Pure (f a)
  fmap f (Ap x y)   = Ap x ((f .) <$> y)

instance Apply (Ap f) where
  Pure f <.> y = fmap f y
  Ap x y <.> z = Ap x (flip <$> y <.> z)

instance Applicative (Ap f) where
  pure = Pure
  Pure f <*> y = fmap f y
  Ap x y <*> z = Ap x (flip <$> y <*> z)
```

Ahora, con el constructor `Ap`, tenemos una instancia de `Applicative`
para cualquier tipo de datos `* -> *`!

Para intentar dar un ejemplo, vamos a modelar un blog mediante `Ap`.

``` {.haskell}
{-# LANGUAGE GADTs #-}

import Control.Applicative.Free
import Control.Applicative

type Id = String

data Author = Author {
    name :: String,
    lastName :: String
} deriving Show

data Post = Post {
    id :: Int,
    title :: String,
    content :: String,
    excerpt :: String
} deriving Show

data BlogF a where
    GetPost :: Id -> BlogF Post
    GetAuthor :: Id -> BlogF Author

type Blog a = Ap BlogF a
```

Lo más importante del ejemplo es entender que en este caso, cuando
definimos nuestro `GADT`, estamos definiendo nuestro *lenguaje*, o *DSL*
de operaciones de blog.

También, para no tener que estar lidiando con los constructores de datos
todo el rato, vamos a crear lo que se denomina como *smart
constructors*, o constructores inteligentes que nos eleven un valor del
tipo \`BlogF\` al tipo \`Blog\`.

Para hacer esto (elevar un valor a \`Ap\`) la librería que usamos nos da
una función llamada `liftAp`.

``` {.haskell}
getPost :: Id -> Blog Post
getPost id = liftAp $ GetPost id

getAuthor :: Id -> Blog Author
getAuthor id = liftAp $ GetAuthor id
```

Ahora lo más divertido, vamos a crear un programa que imprima una página
de nuestro blog!

``` {.haskell}
data Page = Page {
    post :: Post,
    author :: Author
} deriving Show

getPage :: Id -> Id -> Blog Page
getPage postId authorId = Page <$> getPost postId
                               <*> getAuthor authorId
```

¡Increíble! dado que no hay dependencias entre obtener un autor por id,
y obtener el post por id, podemos componer una página de manera
idiomática[^1]!

Pero hay una parte muy importante del blog que estamos olvidando, y
es ![](http://i.imgur.com/RadSf.jpg). Por ahora no estamos escribiendo
nada por la pantalla, ni yendo a la base de datos a obtener nuestros
posts.  Sólamente estamos creando nuevos valores y
devolviéndolos. Estamos creando un Árbol de Sintáxis Abstracta, o AST.

Para evaluar nuestro AST, tenemos que usar un concepto nuevo, llamado
Transformación Natural. Aunque tenga un nombre tan increíblemente
difícil, lo que tenemos que saber de las Transformaciones Naturales es
que son funciones con la siguiente forma:

``` {.haskell}
natTrans :: BlogF a -> IO a
```

Es decir que en vez de transformar el contenido, como hace `fmap` por
ejemplo, transforma el contenerdor, manteniendo el contenido. ¿Por qué
queremos interpretar a `IO`? porque en Haskell, todos los *side effects*
ocurren en IO.

Ahora, vamos a crear el intérprete para las operaciones `BlogF`!

``` {.haskell}
interpret :: BlogF a -> IO a
interpret (GetPost id)   = putStrLn ("getting post " ++ show id ++ " from DB")   *> pure $ Post id "this is the post" "content of the post" "excerpt"
interpret (GetAuthor id) = putStrLn ("getting author " ++ show id ++ " from DB") *> pure $ Author "Pepe" "García"
```

Y, finalmente, vamos a interpretar nuestros programa de renderización
del blog!

``` {.haskell}
main :: IO ()
main = do
    page <- runAp interpret $ getPage 1 1
    print page

-- Output:
-- getting post 1 from DB
-- getting author 1 from DB
-- Page {post = Post {id = 1, title = "this is the post", content = "content of the post", excerpt = "excerpt"}, author = Author {name = "Pepe", lastName = "Garc\237a"}}
```

`runAp` es una función de la libería `free` que toma un programa
basado en `FreeAp` y lo interpreta mediante una *Transformación
Natural*.

## Análisis estático

Una de las cosas más interesantes que podemos hacer con `AST`s
aplicativos, es análisis estático.  El análisis estático es una
técnica mediante la cual podemos sacar conclusiones de un programa sin
evaluarlo.

La razón por la que los aplicativos permiten análisis estático,
mientras que las mónadas no, es porque en los aplicativos no hay que
evaluar ninguna expresión para conocer el árbol sintáctico completo.
En cambio, como en las mónadas necesitamos evaluar las llamadas a
`>>=` para obtener la siguiente expresión.

Ahora imaginemos que queremos limitar el número de llamadas que
nuestro blog hace a la base de datos.  Dado que sabemos que en nuestro
intérprete todas las operaciones `GetAuthor` y `GetPost` se
interpretarán como una sentencia SQL, podemos simplemente contar el
número de operaciones de este tipo.

Cómo conseguimos esto?

``` {.haskell}
instance Monoid Int where
    mempty = 0
    mappend = (+)

countInstructions :: BlogF a -> Int
countInstructions _ = 1

main :: IO ()
main = do
    putStrLn "NUMBER OF REQUESTS TO THE DB:"
    print instructions
    where instructions = runAp_ countInstructions page
          page = getPage 1 1

-- Output:
--
-- NUMBER OF REQUESTS TO THE DB:
-- 2
```

`runAp_` es una versión modificada de `runAp` que toma como intérprete
una función:

```Haskell
fn :: Monoid b => f a -> b
```

Que aplica `mappend` a todos los `b` producidos en nuestro intérprete.

## Conclusiones

Hay que identificar cuándo usar Aplicativos.  Es un poco complicado al
principio, dado que no estamos habituados a trabajar con ellos, pero
como regla, podemos pensar que los aplicativos nos ayudan cuando
nuestras operaciones no dependen unas de las otras.


Footnotes
=========

[^1]: En un principio, los Applicative se denominaron Idioms.
