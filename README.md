# hakyll-haskellnautas

## Instalar

1. Clonar el repositorio `git clone git@github.com:haskellMAD/hakyll-haskellnautas.git`
2. Entrar en el repositorio `cd hakyll-haskellnautas`
3. Compilar el proyecto `stack build`

## Escribir un artículo

Añadir el artículo a `static/articulos`, tiene que tener como prefijo la fecha de publicación en formato `AAAA-MM-DD`. Los artículos se pueden escribir en cualquier formato soportado por Hakyll (mirad su documentación) pero por lo general utilizamos markdown.
Cada artículo tiene que compenzar con la siguiente cabecera:
```
---
title: <Título del artículo>
author: <nombre de usuario del autor>
extract: <Extracto de pocas líneas del artículo
---
```

### Compilando el HTML

Para que las páginas se vean correctamente en http://haskellnautas.xyz , hay que compilar el contenido a html.  Esto lo hacemos con el siguiente comando:

```bash
$ stack exec site clean # limpia los archivos cacheados
$ stack exec site build # construye el html
```
