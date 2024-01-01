Lógica Computacional 2022-01
ADLG

En la función fnn trataba de hacer todo en esa misma funcion, asi que tome el consejo
del ayudante Daniel y la implemente junto con otra funcion auxiliar fnnax que lo 
que hace es aplicar lo de las notas, si tiene una foruma con dos negaciones estas se
eliminan y solo queda la formula. En resumen se aplican correctamente las leyes de la negacion.

La funcion distr la implemente como estan las leyes de la distributividad, en el caso 
donde reciviera una foruma del tipo ((P ^ P) v P) o la del tipo (P v (P ^ P)) ademas
implemente una funcion que nos dice si una formula tiene una Conjuncion o no, usada en
los otros casos de distr.

En las demas funciones ya fue un poco mas sencillo ya que para pasar a una forma
normal conjuntiva primero tenemos que pasar por la forma normal negativa, asi que en fnc
se le pasa una formula, a esa le aplicamos "si es necesario" distributividad y ya
despues la forma normal negativa. (Es necesario por la observacion de los casos de distr que se dio en Telegram).

Para la funcion ctolist necesite de una funcion auxiliar que dijera si una formula 
tenia literales, ya que si es una literal que la devolviera en una lista y si era
la clausula checar el caso del Or porque si es diferente simplemente devuelve esa formula.
La cuestion de los elementos repetidos se arregla con la operacion de union y la 
salida de la lista con una funcion auxiliar que la ordena.

Para la funcion fncC como recibe una formula normal conjuntiva se ve el caso del And
y devuelve una lista de clausulas de lo contrario la formula contenida en dos listas.

Y por ultimo para fncConj como recibe una formula y debe devolver un conjunto de clausulas
equivalentes solo se hace uso de la funcion fnc a la formula recibida.

