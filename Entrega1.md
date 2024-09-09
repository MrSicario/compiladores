# Entrega 1

## Operaciones Unarias y Let-bindings

En un principio para mejorar la familiarización con el compilador, se implementaron operaciones
unarias y let-bindings sin utilizar transformación a ANF, lo cual resultó de forma exitosa pero
difícil de expandir a operaciones binarias o condicionales ya que hubiese requerido mucho código
"boiler-plate" para los multiples casos posibles.

## Implementación de ANF y refactorización del compilador

### ANF

Se decidió implementar una transformación a ANF siguiendo el repositorio que fue proveído como ejemplo,
por lo que se expandio el proyecto con el archivo `anf.ml`, que contiene las funciones necesarias para
transformar de `expr` a `aexpr` junto con utilidades para `aexpr -> string`.

Durante la refactorización del compilador para utilizar a-expr en lugar de expr, surgio un error de *"shadowing"* y captura de identificadores libres por lo que se implementó una función `scram : expr -> string -> string -> expr` para "desfigurar" los identificadores internamente y asegurar unicidad al momento de realizar la transformación a ANF.

La solución es bastante debil y es un elemento a mejorar a futuro del compilador pero se consideró suficiente para los objetivos de esta entrega.

### Refactorización del Compilador

Se refactorizó la función `compile` para que internamente transforme el argumento de `expr` a `aexpr` para mantener compatibilidad del código y evitar refactorizar otros modulos.

Las funciones `compile_aexpr` y `compile_cexpr` son funciones recursivas para generar la lista de instrucciones mediante pattern matching a partir de cualquier `aexpr` o `cexpr`, mientras que las funciones dedicadas a compilar `immexpr` son para casos particulares dependiendo si son argumentos o secuencias de instrucciones más complejas.

## Data type tagging y expresiones condicionales

Se agregaron datos de tipo booleano con tagging, utilizando el bit LSB como tag. Esto involucró adaptar las operaciones y el runtime para que usen la nueva representación. Luego de esto se agregaron los operadores booleanos correspondientes con short-circuit, utilizando saltos condicionales similar a `if`.

La expresión condicional `if` se implementó de forma estándar con saltos condicionales en assembly, utilizando labels generados con `gensym`. Tras la refactorización a ANF, se adaptó para que sea compatible con `compile_cexpr` siguiendo la convención de que ambas ramas dejan un resultado en el registro RAX.