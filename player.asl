// Agente player en proyecto DeepRow.mas2j

/* Creencias y reglas iniciales */
// La profundidad máxima del árbol de juego a considerar. Afecta a la
// corrección y tiempo de ejecución del algoritmo: más profundidad aumenta
// la calidad de las jugadas, a costa de mayor tiempo de ejecución y consumo de
// memoria
profundidadArbolJuego(1). // En SWI-Prolog 8.0.2, estas mismas reglas se ejecutan en mucho menos tiempo que en Jason.
                          // Y Jason ni lo compensa con cortes verdes ;-(

// Las dimensiones del tablero
anchoTablero(8).
altoTablero(8).

// El valor devuelto por la heurística para señalar una victoria. Es el valor
// máximo posible que puede tomar la heurística
heuristicaVictoria(999999).
// El valor devuelto por la heurística para señalar una derrota. Es el valor
// mínimo posible que puede tomar la heurística
heuristicaDerrota(-999999).

// Cláusulas interfaz para obtener la casilla donde colocar una ficha para maximizar
// nuestra victoria o la del contrincante
mejorSiguienteCasilla(X, Y) :- minimax([[movimiento(X, Y, _)], _]).
peorSiguienteCasilla(X, Y) :- maximin([[movimiento(X, Y, _)], _]).

// Cláusula interfaz para obtener la jugada óptima a realizar, con su heurística asociada
minimax([Jugada, Heuristica]) :-
	profundidadArbolJuego(P) &
	minimax_impl([], [_, Heuristica], P, true) &
	mejorJugadaEnRespuestaAJugada([], Jugada) &
	// Borra datos en caché para partir de un estado limpio
	borrar_generarJugadasInmediatas_cacheado &
	borrar_jugadorGano_cacheado &
	.abolish(mejorJugadaEnRespuestaAJugada(_, _)).
// Cláusula interfaz para obtener la jugada óptima a realizar para perder el juego, con su heurística asociada
maximin([Jugada, Heuristica]) :-
	profundidadArbolJuego(P) &
	.asserta(haciendoMaximin) & // Para que no se tenga en cuenta la inversión del jugador que maximiza para generar jugadas
	minimax_impl([], [_, Heuristica], P, false) &
	mejorJugadaEnRespuestaAJugada([], Jugada) &
	.abolish(haciendoMaximin) &
	// Borra datos en caché para partir de un estado limpio
	borrar_generarJugadasInmediatas_cacheado &
	borrar_jugadorGano_cacheado &
	.abolish(mejorJugadaEnRespuestaAJugada(_, _)).
// Si la profundidad restante es cero, no generar hijos para este nodo,
// y considerar la heurística del nodo como la heurística de la jugada que representa
// (caso base)
minimax_impl(JugadaActual, [JugadaActual, Heuristica], 0, _) :-
	aplicarJugadaSoloSiNoAplicada(JugadaActual) &
	heuristica(JugadaActual, Heuristica) &
	deshacerJugadaSoloSiAplicada(JugadaActual).
// Si la profundidad restante no es cero, generar hijos para este nodo del árbol
// y considerar la heurística del nodo como la heurística máxima o mínima de las jugadas
// hijas
minimax_impl(JugadaActual, [JugadaActual, Heuristica], Profundidad, Maximizar) :- // true -> maximizar, mis jugadas; false -> minimizar, jugadas del oponente
	Profundidad > 0 & // Para no unificar con lo que debe atenderse por la primera regla
	misJugadasTeniendoCuentaMaximin(Maximizar, MisJugadas) &
	generarJugadasInmediatas_cacheado(JugadaActual, Jugadas, MisJugadas) & // Para reducir el tiempo de ejecución empleado por el backtracking
	Jugadas \== [] &
	negar(Maximizar, NuevoMaximizar) &
	minimaxVariasJugadas(Jugadas, JugadasYHeuristicas, Profundidad - 1, NuevoMaximizar) &
	heuristicaOptima(JugadasYHeuristicas, JugadaOptima, Heuristica, Maximizar) &
	.asserta(mejorJugadaEnRespuestaAJugada(JugadaActual, JugadaOptima)).
// Los nodos sin más jugadas posibles son terminales, y su heurística se calcula directamente
minimax_impl(JugadaActual, [JugadaActual, Heuristica], Profundidad, Maximizar) :-
	Profundidad > 0 & // Para no unificar con lo que debe atenderse por la primera regla
	misJugadasTeniendoCuentaMaximin(Maximizar, MisJugadas) &
	generarJugadasInmediatas_cacheado(JugadaActual, Jugadas, MisJugadas) & // Para reducir el tiempo de ejecución empleado por el backtracking
	Jugadas = [] &
	aplicarJugadaSoloSiNoAplicada(JugadaActual) &
	heuristica(JugadaActual, Heuristica) &
	deshacerJugadaSoloSiAplicada(JugadaActual).

// Devuelve el valor apropiado de MisJugadas para la generación de jugadas, teniendo en cuenta si se está ejecutando minimax o bien maximin.
// Esencialmente, estas cláusulas hacen que resultado = MisJugadas XOR haciendoMaximin.
misJugadasTeniendoCuentaMaximin(MisJugadas, true) :- (not MisJugadas & haciendoMaximin) | (MisJugadas & not haciendoMaximin).
misJugadasTeniendoCuentaMaximin(MisJugadas, false) :- (not MisJugadas & not haciendoMaximin) | (MisJugadas & haciendoMaximin).

// Si no hemos computado ya el resultado de generarJugadasInmediatas sobre los argumentos dados, hacerlo y guardarlo para reusarlo
generarJugadasInmediatas_cacheado(JugadaActual, Jugadas, MisJugadas) :-
	not generarJugadasInmediatas_(JugadaActual, Jugadas, MisJugadas) &
	generarJugadasInmediatas(JugadaActual, Jugadas, MisJugadas) &
	.assertz(generarJugadasInmediatas_(JugadaActual, Jugadas, MisJugadas)).
// Si ya tenemos el resultado en caché, sacarlo directamente de ahí, para no tener que volver a repetir los cálculos
generarJugadasInmediatas_cacheado(JugadaActual, Jugadas, MisJugadas) :- generarJugadasInmediatas_(JugadaActual, Jugadas, MisJugadas).
// Eliminar datos guardados en caché que pudieran quedar
borrar_generarJugadasInmediatas_cacheado :- .abolish(generarJugadasInmediatas_(_, _, _)).

// Si no tenemos jugadas a las que aplicar minimax, no hacerlo (caso base)
minimaxVariasJugadas([], [], _, _).
// Mientras queden jugadas a las que aplicar minimax, guardar los resultados de
// minimax en una lista
minimaxVariasJugadas([Jugada|Cdr], [[Jugada, Heuristica]|JugadasYHeuristicas], Profundidad, Maximizar) :-
	minimax_impl(Jugada, [Jugada, Heuristica], Profundidad, Maximizar) &
	minimaxVariasJugadas(Cdr, JugadasYHeuristicas, Profundidad, Maximizar).

// Cláusula interfaz para obtener la jugada que optimiza su valor de heurística
heuristicaOptima(JugadasYHeuristicas, JugadaOptima, HeuristicaOptima, Maximizar) :-
	heuristicaVictoria(MaxHeuristica) &
	heuristicaDerrota(MinHeuristica) &
	negar(Maximizar, MaximizarProc) &
	valorOptimo(MinHeuristica - 1, MaxHeuristica + 1, MaximizarProc, HeuristicaComp) & // Maximizar = true -> HeuristicaComp = MaxHeuristica
	heuristicaOptima_impl(JugadasYHeuristicas, JugadaOptima, HeuristicaOptima, Maximizar, _, HeuristicaComp).
// Si no hay más hijos que recorrer, la heurística optimizada final es la actual
heuristicaOptima_impl([], JugadaOptimaActual, HeuristicaOptimaActual, _, JugadaOptimaActual, HeuristicaOptimaActual).
// Si la heurística actual es una nueva óptima, entonces la nueva óptima actual debería de ser la actual
heuristicaOptima_impl([[JugadaActual, HeuristicaActual]|Cdr], JugadaOptima, HeuristicaOptima, Maximizar, _, HeuristicaOptimaActual) :-
	valorOptimo(HeuristicaActual, HeuristicaOptimaActual, Maximizar, HeuristicaComp) &
	HeuristicaOptimaActual \== HeuristicaComp &
	heuristicaOptima_impl(Cdr, JugadaOptima, HeuristicaOptima, Maximizar, JugadaActual, HeuristicaActual).
// Si la heurística actual no es una nueva óptima, entonces la nueva óptima actual debe de seguir como está
heuristicaOptima_impl([[_, HeuristicaActual]|Cdr], JugadaOptima, HeuristicaOptima, Maximizar, JugadaOptimaActual, HeuristicaOptimaActual) :-
	valorOptimo(HeuristicaActual, HeuristicaOptimaActual, Maximizar, HeuristicaComp) &
	HeuristicaOptimaActual = HeuristicaComp &
	heuristicaOptima_impl(Cdr, JugadaOptima, HeuristicaOptima, Maximizar, JugadaOptimaActual, HeuristicaOptimaActual).

// Cláusulas interfaz para generar las jugadas inmediatas a partir de una jugada que
// se considera ya hecha (aunque realmente no sea así)
// Si alguien ha ganado, no hay más jugadas posibles
generarJugadasInmediatas(JugadaHecha, [], _) :-
	aplicarJugadaSoloSiNoAplicada(JugadaHecha) &
	jugadorGano_cacheado(JugadaHecha, true, VictoriaMia) &
	jugadorGano_cacheado(JugadaHecha, false, VictoriaRival) &
	(VictoriaMia | VictoriaRival) &
	deshacerJugadaSoloSiAplicada(JugadaHecha).
// Si nadie gana, entonces generar jugadas
generarJugadasInmediatas(JugadaHecha, JugadasGeneradas, MisJugadas) :-
	aplicarJugadaSoloSiNoAplicada(JugadaHecha) & // Por si la regla anterior falló y no se ha deshecho nada
	jugadorGano_cacheado(JugadaHecha, true, VictoriaMia) &
	jugadorGano_cacheado(JugadaHecha, false, VictoriaRival) &
	not VictoriaMia & not VictoriaRival &
	generarJugadasInmediatas_impl(JugadaHecha, 0, 0, difListas(JugadasGeneradas, []), MisJugadas) &
	deshacerJugadaSoloSiAplicada(JugadaHecha).
// Si no hay un siguiente movimiento, no se pueden generar más jugadas, y por tanto
// las nuevas jugadas generadas se corresponden con la lista vacía
generarJugadasInmediatas_impl(JugadaHecha, SigX, SigY, difListas(JugadasGeneradas, JugadasGeneradas), MisJugadas) :-
	jugadaHecha(JugadaHecha) &
	not siguienteMovimiento(SigX, SigY, MisJugadas, _).
// Si hay un siguiente movimiento, entonces generar una nueva jugada con él,
// y añadirla a la lista
generarJugadasInmediatas_impl(JugadaHecha, SigX, SigY, JugadasGeneradas, MisJugadas) :-
	jugadaHecha(JugadaHecha) &
	siguienteMovimiento(SigX, SigY, MisJugadas, movimiento(X, Y, MiMovimiento)) &
	generarJugadasInmediatas_impl(JugadaHecha, X + 1, Y, difListas(InicioJugadasGeneradas, FinJugadasGeneradas), MisJugadas) &
	append_simple(JugadaHecha, [movimiento(X, Y, MiMovimiento)], NuevaJugada) & // No vamos a tener que iterar sobre muchas jugadas
	append_dl(difListas([NuevaJugada|Cdr3], Cdr3), difListas(InicioJugadasGeneradas, FinJugadasGeneradas), JugadasGeneradas).

// Podemos hacer un movimiento en la primera casilla libre que encontremos
siguienteMovimiento(X, Y, MiMovimiento, movimiento(X, Y, MiMovimiento)) :-
	anchoTablero(Ancho) & altoTablero(Alto) &
	X < Ancho & Y < Alto &
	tablero(X, Y, Id) & Id = 0.
// Si la casilla actual no está libre, analizar la siguiente en la coordenada X
siguienteMovimiento(X, Y, MiMovimiento, SigMovimiento) :-
	anchoTablero(Ancho) & altoTablero(Alto) &
	X < Ancho & Y < Alto &
	tablero(X, Y, Id) & Id \== 0 &
	siguienteMovimiento(X + 1, Y, MiMovimiento, SigMovimiento).
// Si la casilla actual está desbordada en X, analizar la siguiente en la coordenada Y,
// porque ya hemos agotado todas las coordenadas X de la Y anterior
siguienteMovimiento(X, Y, MiMovimiento, SigMovimiento) :-
	anchoTablero(Ancho) & altoTablero(Alto) &
	X == Ancho & Y < (Alto - 1) &
	siguienteMovimiento(0, Y + 1, MiMovimiento, SigMovimiento).

// Sin jugada que aplicar, no hacer nada (caso base)
aplicarJugada([]).
// Aplicar cada uno de los movimientos
aplicarJugada([Movimiento|Cdr]) :-
	aplicarMovimiento(Movimiento) &
	aplicarJugada(Cdr).

// Simula un movimiento en el tablero
aplicarMovimiento(movimiento(X, Y, SoyYoQuienHaceMov)) :-
	soyYoAIdentificadorJugador(SoyYoQuienHaceMov, Id) &
	.abolish(tablero(X, Y, 0)[source(percept)]) &
	.asserta(tablero(X, Y, Id)[source(percept)]).

// Aplica una jugada, solo si no se ha aplicado todavía, para evitar incongruencias.
// En caso de que ya esté aplicada, devuelve verdadero igualmente
aplicarJugadaSoloSiNoAplicada(Jugada) :-
	not jugadaHecha(Jugada) &
	aplicarJugada(Jugada) &
	.asserta(jugadaHecha(Jugada)).
aplicarJugadaSoloSiNoAplicada(Jugada) :- jugadaHecha(Jugada).

// Sin jugada que deshacer, no hacer nada (caso base)
deshacerJugada([]).
// Deshacer cada uno de los movimientos
deshacerJugada([Movimiento|Cdr]) :-
	deshacerMovimiento(Movimiento) &
	deshacerJugada(Cdr).

// Deshace la simulación de un movimiento en el tablero
deshacerMovimiento(movimiento(X, Y, SoyYoQuienHaceMov)) :-
	soyYoAIdentificadorJugador(SoyYoQuienHaceMov, Id) &
	.abolish(tablero(X, Y, Id)[source(percept)]) &
	.asserta(tablero(X, Y, 0)[source(percept)]).

// Deshace una jugada, solo si ya se ha aplicado, para evitar incongruencias.
// En caso de que no esté aplicada, devuelve verdadero igualmente
deshacerJugadaSoloSiAplicada(Jugada) :-
	jugadaHecha(Jugada) &
	deshacerJugada(Jugada) &
	.abolish(jugadaHecha(Jugada)).
deshacerJugadaSoloSiAplicada(Jugada) :- not jugadaHecha(Jugada).

// Predicados que obtiene la puntuación heurística del estado actual del tablero
// Si yo he ganado, la heurística será la máxima
heuristica(Jugada, Valor) :-
	jugadorGano_cacheado(Jugada, true, ValorVerdadGanado) &
	ValorVerdadGanado &
	heuristicaVictoria(Valor)/* &
	.print(Jugada, ": ", Valor)*/.
// Si el contrincante ha ganado, la heurística será la mínima
heuristica(Jugada, Valor) :-
	jugadorGano_cacheado(Jugada, false, ValorVerdadPerdido) &
	ValorVerdadPerdido &
	heuristicaDerrota(Valor)/* &
	.print(Jugada, ": ", Valor)*/.
// En otro caso (nadie ha ganado), la heurística se calculará en base a una función ponderada lineal
heuristica(Jugada, Valor) :-
	jugadorGano_cacheado(Jugada, true, ValorVerdadGanado) &
	jugadorGano_cacheado(Jugada, false, ValorVerdadPerdido) &
	not ValorVerdadGanado & not ValorVerdadPerdido &
	heuristicaPonderadaLineal(Valor)/* &
	.print(Jugada, ": ", Valor)*/.
// Para depuración de minimax
//heuristica(Jugada, Valor) :- .random(Valor) & .print(Jugada, ": ", Valor).

// Calcula una puntuación heurística a partir de características del tablero que se consideran positivas (y negativas)
heuristicaPonderadaLineal(2000 * CaracteristicaImpedirVictoria + 6 * CaracteristicaRaya3 + 5 * CaracteristicaImpedirRaya3 + 4 * CaracteristicaRaya2 + 3 * CaracteristicaImpedirRaya2 + CaracteristicaFichasCentro) :-
	caracteristicaImpedirRaya(CaracteristicaImpedirVictoria, 4) &
	caracteristicaRaya(true, CaracteristicaRaya3, 3) &
	caracteristicaImpedirRaya(CaracteristicaImpedirRaya3, 3) &
	caracteristicaRaya(true, CaracteristicaRaya2, 2) &
	caracteristicaImpedirRaya(CaracteristicaImpedirRaya2, 2) &
	caracteristicaFichasEnCentro(true, CaracteristicaFichasCentro).

// Cláusula interfaz que computa la característica de impedir la formación de una raya de N fichas del rival
caracteristicaImpedirRaya(CaracteristicaImpedirRaya, Fichas) :- caracteristicaImpedirRaya_impl(CaracteristicaImpedirRaya, Fichas, 0, 0, 0).
// Si en la posición actual hemos impedido una raya de N fichas, considerarlo para la característica
caracteristicaImpedirRaya_impl(CaracteristicaImpedirRaya, Fichas, X, Y, Acumulador) :-
	anchoTablero(Ancho) & altoTablero(Alto) &
	X < Ancho & Y < Alto &
	impidoRaya(true, X, Y, Fichas) &
	caracteristicaImpedirRaya_impl(CaracteristicaImpedirRaya, Fichas, X + 1, Y, Acumulador + 1).
// Si en la posición actual no hemos impedido una raya de N fichas, pero podemos seguir incrementando X, hacer eso
caracteristicaImpedirRaya_impl(CaracteristicaImpedirRaya, Fichas, X, Y, Acumulador) :-
	anchoTablero(Ancho) & altoTablero(Alto) &
	X < Ancho & Y < Alto &
	not impidoRaya(true, X, Y, Fichas) &
	caracteristicaImpedirRaya_impl(CaracteristicaImpedirRaya, Fichas, X + 1, Y, Acumulador).
// Si hemos agotado las posiciones X de la fila actual, ir con la siguiente fila
caracteristicaImpedirRaya_impl(CaracteristicaImpedirRaya, Fichas, X, Y, Acumulador) :-
	anchoTablero(Ancho) & altoTablero(Alto) &
	X == Ancho & Y < Alto &
	caracteristicaImpedirRaya_impl(CaracteristicaImpedirRaya, Fichas, 0, Y + 1, Acumulador).
// Si hemos llegado al final de las filas del tablero, es que hemos acabado, y el acumulador contiene
// el valor final de la característica
caracteristicaImpedirRaya_impl(CaracteristicaImpedirRaya, _, _, Y, CaracteristicaImpedirRaya) :-
	altoTablero(Alto) &
	Y >= Alto.

// Cláusula interfaz que computa la característica de formar una raya de N fichas 
caracteristicaRaya(Yo, CaracteristicaRaya, Fichas) :- caracteristicaRaya_impl(Yo, CaracteristicaRaya, Fichas, 0, 0, 0).
// Si en la posición actual hemos formado una raya de N fichas, considerarlo para la característica
caracteristicaRaya_impl(Yo, CaracteristicaRaya, Fichas, X, Y, Acumulador) :-
	anchoTablero(Ancho) & altoTablero(Alto) &
	X < Ancho & Y < Alto &
	raya(Yo, X, Y, Fichas) &
	caracteristicaRaya_impl(Yo, CaracteristicaRaya, Fichas, X + 1, Y, Acumulador + 1).
// Si en la posición actual no hemos formado una raya de N fichas, pero podemos seguir incrementando X, hacer eso
caracteristicaRaya_impl(Yo, CaracteristicaRaya, Fichas, X, Y, Acumulador) :-
	anchoTablero(Ancho) & altoTablero(Alto) &
	X < Ancho & Y < Alto &
	not raya(Yo, X, Y, Fichas) &
	caracteristicaRaya_impl(Yo, CaracteristicaRaya, Fichas, X + 1, Y, Acumulador).
// Si hemos agotado las posiciones X de la fila actual, ir con la siguiente fila
caracteristicaRaya_impl(Yo, CaracteristicaRaya, Fichas, X, Y, Acumulador) :-
	anchoTablero(Ancho) & altoTablero(Alto) &
	X == Ancho & Y < Alto &
	caracteristicaRaya_impl(Yo, CaracteristicaRaya, Fichas, 0, Y + 1, Acumulador).
// Si hemos llegado al final de las filas del tablero, es que hemos acabado, y el acumulador contiene
// el valor final de la característica
caracteristicaRaya_impl(_, CaracteristicaRaya, _, _, Y, CaracteristicaRaya) :-
	altoTablero(Alto) &
	Y >= Alto.

// Cláusula interfaz que computa la característica de tener fichas en las 4 posiciones centrales del tablero
caracteristicaFichasEnCentro(Yo, CaracteristicaFichasCentro1 + CaracteristicaFichasCentro2 + CaracteristicaFichasCentro3 + CaracteristicaFichasCentro4) :-
	caracteristicaFichasEnCentro_impl(Yo, CaracteristicaFichasCentro1, 3, 3) &
	caracteristicaFichasEnCentro_impl(Yo, CaracteristicaFichasCentro2, 4, 3) &
	caracteristicaFichasEnCentro_impl(Yo, CaracteristicaFichasCentro3, 3, 4) &
	caracteristicaFichasEnCentro_impl(Yo, CaracteristicaFichasCentro4, 4, 4).
// Si en la posición dada hay una ficha nuestra, la característica es favorable
caracteristicaFichasEnCentro_impl(Yo, 1, X, Y) :-
	soyYoAIdentificadorJugador(Yo, Id) &
	tablero(X, Y, Id).
// Si en la posición dada no hay una ficha, o no es nuestra, la característica es desfavorable
caracteristicaFichasEnCentro_impl(Yo, 0, X, Y) :-
	soyYoAIdentificadorJugador(Yo, MiId) &
	tablero(X, Y, Id) & Id \== MiId.

// Si ya hemos computado el resultado de jugadorGano para la jugada actual, reusarlo. En caso contrario, computarlo una vez
// por jugada
jugadorGano_cacheado(Jugada, Yo, ValorVerdad) :-
	not jugadorGano_(Jugada, Yo, ValorVerdad) &
	jugadorGano(Yo, ValorVerdad) & // El parámetro Jugada tan solo nos interesa para distinguir entre estados del tablero y garantizar coherencia de caché, no para computar si el jugador ha ganado
	.assertz(jugadorGano_(Jugada, Yo, ValorVerdad)).
jugadorGano_cacheado(Jugada, Yo, ValorVerdad) :- jugadorGano_(Jugada, Yo, ValorVerdad).
// Borra de la base de conocimiento reglas temporales, usadas para recordar resultados parciales
borrar_jugadorGano_cacheado :- .abolish(jugadorGano_(_, _, _)).

// Cláusula interfaz para comprobar si alguien ha ganado la partida o no. El segundo argumento existe para que se pueda guardar
// en caché el valor de salida de tal argumento, en vez de si se ha encontrado una solución o no.
// Diferencia principal entre esta regla y caracteristicaRaya: detiene la evaluación de rayas al encontrar la primera de 4,
// y no sigue hasta el final del tablero, por lo que es algo más eficiente
jugadorGano(Yo, ValorVerdad) :- jugadorGano_impl(0, 0, Yo, ValorVerdad).

// Ha ganado si partiendo de esta casilla hay 4 en raya en cualquiera de las direcciones posibles
jugadorGano_impl(X, Y, Yo, true) :-
	anchoTablero(Ancho) & altoTablero(Alto) &
	X < Ancho & Y < Alto &
	raya(Yo, X, Y, 4).
// Si no hay un 4 en raya en cualquiera de las direcciones posibles, y podemos ver si lo hay en la siguiente coordenada X,
// hacer tal comprobación
jugadorGano_impl(X, Y, Yo, ValorVerdad) :-
	anchoTablero(Ancho) & altoTablero(Alto) &
	X < Ancho & Y < Alto &
	not raya(Yo, X, Y, 4) & // Para evitar backtracking por aquí. Quizás sea buena idea quitarlo si eso no es problema
	jugadorGano_impl(X + 1, Y, Yo, ValorVerdad).
// Si no hay siguiente coordenada X para esta Y, probar con la siguiente Y, empezando otra vez en 0 en X
jugadorGano_impl(X, Y, Yo, ValorVerdad) :-
	anchoTablero(Ancho) & altoTablero(Alto) &
	X == Ancho & Y < Alto &
	jugadorGano_impl(0, Y + 1, Yo, ValorVerdad).
// Si acabamos de recorrer el tablero y no hemos determinado que alguien haya ganado, entonces
// es que no ha ganado
jugadorGano_impl(_, Y, _, false) :-
	altoTablero(Alto) &
	Y >= Alto.

// Regla que es cierta si y solo si un jugador ha hecho una raya de N fichas, considerando la posición (X, Y) como la ficha
// central de la hipotética raya
raya(Yo, X, Y, Fichas) :-
	fichaEn(Yo, X, Y) &
	(rayaEnDireccion(Yo, X, Y, Fichas - 1, 1, 0) |
	rayaEnDireccion(Yo, X, Y, Fichas - 1, 0, 1) |
	rayaEnDireccion(Yo, X, Y, Fichas - 1, 1, 1) |
	rayaEnDireccion(Yo, X, Y, Fichas - 1, -1, 1) |
	rayaEnDireccion(Yo, X, Y, Fichas - 1, 1, -1) |
	rayaEnDireccion(Yo, X, Y, Fichas - 1, -1, 0) |
	rayaEnDireccion(Yo, X, Y, Fichas - 1, 0, -1) |
	rayaEnDireccion(Yo, X, Y, Fichas - 1, -1, -1)).
// Cláusula interfaz para comprobar si, en el vector de dirección dado, hay N fichas más a partir de (X, Y) que pueden formar cuatro en raya
rayaEnDireccion(Yo, X, Y, Fichas, DX, DY) :- rayaEnDireccion_impl(Yo, X, Y, Fichas, 1, DX, DY).
// Una raya de una ficha siempre se cumple en nuestro caso
rayaEnDireccion_impl(_, _, _, 0, _, _, _).
// Ver si la raya en esta dirección se mantiene hasta agotar el número de fichas deseado
rayaEnDireccion_impl(Yo, X, Y, Fichas, FichasContadas, DX, DY) :-
	Fichas > 0 &
	fichaEn(Yo, X + DX * FichasContadas, Y + DY * FichasContadas) &
	rayaEnDireccion_impl(Yo, X, Y, Fichas - 1, FichasContadas + 1, DX, DY).

// Regla que es cierta si y solo si un jugador impide al otro hacer una raya de N fichas, considerando la posición (X, Y)
// como la ficha central de la raya del otro jugador (que debe de existir)
impidoRaya(Yo, X, Y, Fichas) :-
	negar(Yo, Otro) &
	fichaEn(Otro, X, Y) &
	(impidoRayaEnDireccion(Yo, X, Y, Fichas - 1, 1, 0) |
	impidoRayaEnDireccion(Yo, X, Y, Fichas - 1, 0, 1) |
	impidoRayaEnDireccion(Yo, X, Y, Fichas - 1, 1, 1) |
	impidoRayaEnDireccion(Yo, X, Y, Fichas - 1, -1, 1) |
	impidoRayaEnDireccion(Yo, X, Y, Fichas - 1, 1, -1) |
	impidoRayaEnDireccion(Yo, X, Y, Fichas - 1, -1, 0) |
	impidoRayaEnDireccion(Yo, X, Y, Fichas - 1, 0, -1) |
	impidoRayaEnDireccion(Yo, X, Y, Fichas - 1, -1, -1)).
// Cláusula interfaz para comprobar si, en el vector de dirección dado, impido una raya de N fichas a partir de (X, Y), colocando una
// a distancia N + 1
impidoRayaEnDireccion(Yo, X, Y, FichasRestantes, DX, DY) :-
	negar(Yo, Otro) &
	rayaEnDireccion(Otro, X, Y, FichasRestantes - 1, DX, DY) & // Solo se puede impedir una raya en la misma dirección
	fichaEn(Yo, X + DX * FichasRestantes, Y + DY * FichasRestantes).

// Regla que es cierta si y solo si la posición (X, Y) tiene una ficha mía o del otro jugador
fichaEn(Mia, X, Y) :-
	soyYoAIdentificadorJugador(Mia, Id) &
	tablero(X, Y, Id).

// Borra de la base de conocimiento reglas temporales, usadas para recordar resultados parciales
borrarCachesHeuristica :-
	.abolish(jugadorGano_(_, _, _)).

// Concatena dos listas expresadas como diferencias de listas.
// Esta operación es de complejidad O(1)
append_dl(difListas(Inicio1, Fin1), difListas(Fin1, Fin2), difListas(Inicio1, Fin2)).

// Concatena dos listas de manera trivial.
// Esta operación es de complejidad O(n), pero funciona en listas cerradas
append_simple([], L, L).
append_simple([Car|Cdr], L, [Car|R]) :- append_simple(Cdr, L, R).

// Obtiene la negación de un valor de verdad en un argumento, utilizando la negación por fallo disponible en Jason
negar(ValorVerdad, false) :- ValorVerdad.
negar(ValorVerdad, true) :- not ValorVerdad.

// Obtiene el valor mínimo de dos variables
valorMinimo(A, B, A) :- A < B.
valorMinimo(A, B, B) :- A >= B.

// Obtiene el valor máximo de dos variables
valorMaximo(A, B, B) :- B >= A.
valorMaximo(A, B, A) :- A > B.

// Obtiene el valor óptimo de entre los dados, que puede ser el máximo o el mínimo, dependiendo de un argumento que se pase
valorOptimo(A, B, true, Maximo) :- valorMaximo(A, B, Maximo).
valorOptimo(A, B, false, Minimo) :- valorMinimo(A, B, Minimo).

// Si el primer parámetro es verdadero, unifica Id con el ID del jugador actual,
// que es el mismo que se usa en los predicados de funtor tablero/3.
// Si el primer parámetro es falso, entonces unifica Id con el ID del otro jugador.
soyYoAIdentificadorJugador(true, Id) :-
	.my_name(Yo) &
	.delete("player", Yo, IdStr) &
	.term2string(Id, IdStr).
soyYoAIdentificadorJugador(false, 1 + (MiId mod 2)) :- soyYoAIdentificadorJugador(true, MiId).

/* Objetivos iniciales */

/* Planes */

/* Eventos de BC */
+estrategia(Estrategia)[source(percept)] : Estrategia = jugarAGanar | Estrategia = jugarAPerder.
+estrategia(Estrategia)[source(percept)] <- -estrategia(Estrategia)[source(percept)].

// Al ser nuestro turno según el entorno, hacer el mejor movimiento
+turno(Yo)[source(percept)] : .my_name(Yo) <-
	.wait(750); // Por si estamos recibiendo todavía percepciones del tablero

	// Dependiendo de la estrategia a emplear, escoger el mejor o peor movimiento
	// (desde nuestro punto de vista)
	?estrategia(Est);
	if (Est = jugarAGanar) {
		?mejorSiguienteCasilla(X, Y);
	} else {
		?peorSiguienteCasilla(X, Y);
	};

	// Realizar el movimiento a esa casilla
	put(X, Y).

// Descartar comunicaciones que lleguen de otros agentes, pues solo nos interesa
// lo que diga el entorno
+!kqml_received(Agente, _, _, _) : .my_name(Yo) & Agente \== Yo.
