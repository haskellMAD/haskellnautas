---
title: Haskell con Katas: Introducción a CodeWars
author: merchucris-sof98-naldoco
extract: Las katas son un método óptimo para la iniciación y el perfeccionamiento de Haskell... ¡y viceversa!
---

### Bienvenida

**<a href="https://www.meetup.com/Haskell-MAD" target="_blank">HaskellMad</a>** tiene como una de sus actividades difundir la idea de que **Haskell** es un extraordinario lenguaje de programación, **divertido** y **provechoso** de aprender y utilizar. Y se nos ha ocurrido que Haskell podría ser una de las claves que permitan dar el salto de calidad desde un aprendizaje tradicional de "**hacer ejercicios de programación**" hacia un modelo mucho más ambicioso, eficaz y moderno basado en el sugerente concepto de "**kata de programación**", término aún poco preciso, que se intuye, pero que no se termina de concretar en la práctica.

Allí donde hay un reto de programación, puede y debe haber una kata, y Haskell va a ser nuestro aliado.

Con esto en mente, ya hemos realizado un **<a href="https://www.meetup.com/Haskell-MAD/events/249769977/" target="_blank">taller</a>** de la plataforma web **CodeWars**, tal vez la mejor plataforma web comunitaria de retos de programación, donde hemos mostrado los elementos que ofrece Haskell para lograr que lo que CodeWars identifica como "katas" hagan honor a su nombre, haciendo valer la gran **expresividad**, **abstracción** y **concisión** de nuestro lenguaje favorito.

Durante el taller se mostraron algunas **estrategias** y **técnicas** que con fluidez permiten trabajar y **repensar** el resultado obtenido por uno mismo, junto con el resto de soluciones del mismo problema aportados por otros programadores de manera provechosa y original.

Esta especie de juego que se hace con el código está diseñado para que afloren y destaquen una variedad de importantes **detalles abstractos** relacionados con la programación y ayuda a entender cómo programar con **elegancia** y **efectividad**.

Mientras preparamos los artículos con la metodología que se perfiló en el taller mencionado, vamos a publicar varios **artículos técnicos** con los mimbres con los que trabajaremos.  Este lo dedicamos a facilitar el uso de CodeWars.

Si te surge alguna **duda** con este artículo o estás especialmente **interesado** en este enfoque que relaciona **Haskell** y **katas**, te recomendamos encarecidamente que te des de alta en el **<a href="https://haskellnautas.herokuapp.com*" target="_blank">slack de Haskellnautas</a>**, y accedas al canal "**katas**" o al canal de entrada.  **Te esperamos**.

### CodeWars

En este primer artículo de la serie "**Katas de programación y Haskell**" vamos a presentar **<a href="https://www.codewars.com" target="_blank">CodeWars</a>**, web para programadores donde podemos localizar y seleccionar las "katas" que nos interesen, para tratar de resolverlas, interaccionar con otros programadores y hasta desarrollar nuevas propuestas de problemas que aumentarían el conjunto disponible a resolver.

En apenas cinco años desde su puesta en marcha, CodeWars actualmente ofrece **miles** de estos problemas para **Haskell** (y también para otros lenguajes).  Es un bello ejemplo de Web 3.0 donde "cualquier persona" está invitada a engrandecer y mejorar la plataforma de muy variadas maneras.  Cualquiera puede ser protagonista de crear y elevar contínuamente la **calidad de la documentación**, en especial de las instrucciones y ejemplos, así como de los **tests**, a los que se les sitúa en el primer plano que se merecen, invitando a que sean editados **a la vez** que se trabaje con el código de la solución.

Por tanto, desde la propia plataforma se puede, con un interfaz integrado, **editar tanto los test como la solución**, **compilar el código**, **ejecutarlo con los tests**, y obtener un posible informe de **errores** de compilación.  Y una vez verificado como aceptable, se permite **comparar la solución** propia con otras que ya ha sido aportadas anteriormente, una actividad que en próximos artículos demostraremos que puede resultar muy provechosa, mediante un procedimiento donde **Haskell brilla con luz propia**.

### Primeros pasos con CodeWars
Por decirlo del tirón, sería ir a la **web** de CodeWars, seleccionar **Haskell**, pasar una mínima **prueba**, crear un **usuario**, familiarizarte con el panel de control (**Dashboard**), ir al panel **Kata**, **buscar** la kata "Remove the minimum" (con la que trabajaremos en primer lugar), **visitar** la kata y finalmente tratar de resolverla (**Train**).

Lo vemos más detenidamente:

1) **Entrar en CodeWars**:

    * <a href="https://www.codewars.com" target="_blank">https://www.codewars.com</a>

Nota: En las imágenes resaltamos en *amarillo* los elementos que se destacan en cada descripción.

2) Registrarse como usuario nuevo. (**Sign Up**)

[//]: ![](/images/naldoco/00-CodeWarsSignUp-a.jpg)
<a href="/images/naldoco/00-CodeWarsSignUp-a.jpg">
  <img src="/images/naldoco/00-CodeWarsSignUp-a.jpg" alt="CodeWars sign up" width="640">
</a>

3) Antes de registrarte debes elegir un lenguaje y pasar una pequeña **prueba**.  Hacemos click en **Haskell**.

[//]: ![](/images/naldoco/00-CodeWarsSignUp-b.png)
<a href="/images/naldoco/00-CodeWarsSignUp-b.png">
  <img src="/images/naldoco/00-CodeWarsSignUp-b.png" alt="CodeWars sign up" width="640">
</a>

4) Esta será nuestra primera kata.  Se trata de **corregir el error** de la función que "multiplica dos enteros".  Cuando finalices, pulsa sobre "**Submit**".  Si no das con la solución, pulsa el botón rojo de "**TRY AGAIN**", y prueba otra vez.

    * Si sabes algo de Haskell no te resultará difícil conseguirlo. (Guiño, guiño)
    * Si tienes dificultades, **inténtalo un poco más**.  Merece la pena.
	    * Recuerda que Haskell tiene una **sintaxis muy sencilla**, similar a las **matemáticas** de la escuela.
	    * Si sigues perdido, pasa el ratón [por encima de este texto](* "Sobran 3 cosas").
	    * En último extremo, pulsa **<a href="/images/naldoco/00-CodeWarsSignUp-d.png" target="_blank">aquí</a>**.

[//]: ![](/images/naldoco/00-CodeWarsSignUp-c.png)
<a href="/images/naldoco/00-CodeWarsSignUp-c.png">
  <img src="/images/naldoco/00-CodeWarsSignUp-c.png" alt="CodeWars sign up" width="640">
</a>

5) ¡Ya tienes tu primera kata resuelta!  Ahora debes crear tu **identificador de usuario**.

* Si ya tienes cuenta en **GitHub**, selecciona la primera opción.
* Si lo prefieres, puedes crear un usuario **rellenando** estos campos y pulsando en **ENLIST**:
	* dirección de correo (**Email**)
	* nombre (**Username**)
	* clave de nueva creación que te inventes para entrar en CodeWars (**Password**)

[//]: ![](/images/naldoco/00-CodeWarsSignUp-e.png)
<a href="/images/naldoco/00-CodeWarsSignUp-e.png">
  <img src="/images/naldoco/00-CodeWarsSignUp-e.png" alt="CodeWars sign up" width="640">
</a>

6) Ahora cada vez que queramos entrar con nuestro usuario haremos un "**Log In**" (**Sign In**), lo que nos conducirá a nuestro propio panel de control o **Home** (**Dashboard**).  (<https://www.codewars.com/dashboard>).  Desde cualquier pantalla podemos volver al panel de control pulsando el botón rojo con esa especie de estrella oriental que vemos arriba a la izquierda.

[//]: ![](/images/naldoco/01-Dashboard-a.png)
<a href="/images/naldoco/01-Dashboard-a.png">
  <img src="/images/naldoco/01-Dashboard-a.png" alt="CodeWars sign up" width="640">
</a>

7) Debajo del **Dashboard** o **Home** tenemos el botón de **Kata**, lo pulsamos.

[//]: ![](/images/naldoco/02-Kata-a.png)
<a href="/images/naldoco/02-Kata-a.png">
  <img src="/images/naldoco/02-Kata-a.png" alt="CodeWars sign up" width="640">
</a>

8) Ahora estamos en el panel de las katas.  Vamos a utilizar el buscador de katas (**Search**).

[//]: ![](/images/naldoco/03-KataSearch-a.png)
<a href="/images/naldoco/03-KataSearch-a.png">
  <img src="/images/naldoco/03-KataSearch-a.png" alt="CodeWars sign up" width="640">
</a>

9) Buscamos una kata que contiene en el nombre "**remove minimum**".  Localizamos la kata "**Remove the minimum**".  **Pulsamos** sobre ella en el propio nombre.

[//]:  ![](/images/naldoco/04-KataSearch2-a.png)
<a href="/images/naldoco/04-KataSearch2-a.png">
  <img src="/images/naldoco/04-KataSearch2-a.png" alt="CodeWars sign up" width="640">
</a>

10) Ya hemos localizado una kata concreta.  Le echamos un vistazo, y como nos interesa, pulsamos sobre el botón azul "**TRAIN**".  Llegamos al IDE (Entorno de desarrollo integrado) Por fin podemos jugar con la kata.

Pues eso.  **¡A jugar!**

[//]: ![](/images/naldoco/05-KataSearch3-a.png)
<a href="/images/naldoco/05-KataSearch3-a.png">
  <img src="/images/naldoco/05-KataSearch3-a.png" alt="CodeWars sign up" width="640">
</a>

**¡Hasta la próxima entrega!**  (También nos puedes encontrar en <a href="https://haskellnautas.slack.com" target="_blank">https://haskellnautas.slack.com</a>).
