# Entrega 0

Para agregar la función `simplify` utilicé pattern matching.

Intente usar pattern matching junto a funciones recursivas para extender tanto `parse.ml`como `interp.ml`
para extender el lenguaje.  
Esto requirio tambien extender `ast.ml` y `lib.ml` para agregar los tipos de datos
necesarios.  

Logre completar los requisitos de la tarea de:
- Agregar más operadores aritméticos
- Valores booleanos y operadores `if` y `and` con corto circuito
- Operadores `int -> int -> bool` `=` y `<`
- Funciones de primera clase
- Azúcar sintáctico de `let` y `let*`

Tambien implemente los tests necesarios para probar que funcionen las extensiones del lenguaje y la función `simplify`.

No me enfoque en probar s-expr que intencionalmente estén erroneas para el parser, y para probar los corto circuitos del interprete utilicé casos donde si se evaluara la otra rama causaría un error.