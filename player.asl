// Agente player en proyecto DeepRow.mas2j

/* Creencias y reglas iniciales */
// La profundidad m�xima del �rbol de juego a considerar. Afecta a la
// correcci�n y tiempo de ejecuci�n del algoritmo: m�s profundidad aumenta
// la calidad de las jugadas, a costa de mayor tiempo de ejecuci�n y consumo de
// memoria
profundidadArbolJuego(1). // En SWI-Prolog, estas mismas reglas se ejecutan en mucho menos tiempo que en Jason. Y Jason ni lo compensa con cortes verdes ;-(

// Las dimensiones del tablero
anchoTablero(8).
altoTablero(8).

// El valor devuelto por la heur�stica para se�alar una victoria. Es el valor
// m�ximo posible que puede tomar la heur�stica
heuristicaVictoria(999999).
// El valor devuelto por la heur�stica para se�alar una derrota. Es el valor
// m�nimo posible que puede tomar la heur�stica
heuristicaDerrota(-999999).

// Cl�usulas interfaz para obtener la casilla donde colocar una ficha para maximizar
// nuestra victoria o la del contrincante
mejorSiguienteCasilla(X, Y) :- minimax([[movimiento(X, Y, _)], _]).
peorSiguienteCasilla(X, Y) :- maximin([[movimiento(X, Y, _)], _]).

// Cl�usula interfaz para obtener la jugada �ptima a realizar, con su heur�stica asociada
minimax(JugadaYHeuristica) :-
	profundidadArbolJuego(P) &
	minimax_impl([], JugadaYHeuristica, P, true) &
	// Borra datos en cach� para partir de un estado limpio
	borrar_generarJugadasInmediatas_cacheado &
	borrar_jugadorGano_cacheado &
	borrar_minimax_impl_cacheado.
// Cl�usula interfaz para obtener la jugada �ptima a realizar para perder el juego, con su heur�stica asociada
maximin(JugadaYHeuristica) :-
	profundidadArbolJuego(P) &
	.asserta(haciendoMaximin) & // Para que no se tenga en cuenta la inversi�n del jugador que maximiza para generar jugadas
	minimax_impl([], JugadaYHeuristica, P, false) &
	.abolish(haciendoMaximin) &
	// Borra datos en cach� para partir de un estado limpio
	borrar_generarJugadasInmediatas_cacheado &
	borrar_jugadorGano_cacheado &
	borrar_minimax_impl_cacheado.
// Si la profundidad restante es cero, no generar hijos para este nodo,
// y considerar la heur�stica del nodo como la heur�stica de la jugada que representa
// (caso base)
minimax_impl(JugadaActual, [JugadaActual, Heuristica], 0, _) :-
	aplicarJugada(JugadaActual) &
	heuristica(JugadaActual, Heuristica) &
	deshacerJugada(JugadaActual).
// Si la profundidad restante no es cero, generar hijos para este nodo del �rbol
// y considerar la heur�stica del nodo como la heur�stica m�xima o m�nima de las jugadas
// hijas
minimax_impl(JugadaActual, [JugadaOptima, Heuristica], Profundidad, Maximizar) :- // true -> maximizar, mis jugadas; false -> minimizar, jugadas del oponente
	Profundidad > 0 & // Para no unificar con lo que debe atenderse por la primera regla
	misJugadasTeniendoCuentaMaximin(Maximizar, MisJugadas) &
	generarJugadasInmediatas_cacheado(JugadaActual, Jugadas, MisJugadas) & // Para reducir el tiempo de ejecuci�n empleado por el backtracking
	Jugadas \== [] &
	negar(Maximizar, NuevoMaximizar) &
	minimaxVariasJugadas(Jugadas, Profundidad - 1, NuevoMaximizar, JugadaOptima, Heuristica).
// Los nodos sin m�s jugadas posibles son terminales, y su heur�stica se calcula directamente
minimax_impl(JugadaActual, [JugadaActual, Heuristica], Profundidad, Maximizar) :-
	Profundidad > 0 & // Para no unificar con lo que debe atenderse por la primera regla
	misJugadasTeniendoCuentaMaximin(Maximizar, MisJugadas) &
	generarJugadasInmediatas_cacheado(JugadaActual, Jugadas, MisJugadas) & // Para reducir el tiempo de ejecuci�n empleado por el backtracking
	Jugadas = [] &
	aplicarJugada(JugadaActual) &
	heuristica(JugadaActual, Heuristica) &
	deshacerJugada(JugadaActual).

// Devuelve el valor apropiado de MiJugada para la generaci�n de jugadas, teniendo en cuenta si se est� ejecutando minimax o bien maximin.
// Esencialmente, estas cl�usulas hacen que resultado = MiJugada XOR haciendoMaximin.
misJugadasTeniendoCuentaMaximin(MiJugada, true) :- (not MiJugada & haciendoMaximin) | (MiJugada & not haciendoMaximin).
misJugadasTeniendoCuentaMaximin(MiJugada, false) :- (not MiJugada & not haciendoMaximin) | (MiJugada & haciendoMaximin).

// Si no hemos computado ya el resultado de generarJugadasInmediatas sobre los argumentos dados, hacerlo y guardarlo para reusarlo
generarJugadasInmediatas_cacheado(JugadaActual, Jugadas, MisJugadas) :-
	not generarJugadasInmediatas_(JugadaActual, Jugadas, MisJugadas) &
	generarJugadasInmediatas(JugadaActual, Jugadas, MisJugadas) &
	.assertz(generarJugadasInmediatas_(JugadaActual, Jugadas, MisJugadas)).
// Si ya tenemos el resultado en cach�, sacarlo directamente de ah�, para no tener que volver a repetir los c�lculos
generarJugadasInmediatas_cacheado(JugadaActual, Jugadas, MisJugadas) :- generarJugadasInmediatas_(JugadaActual, Jugadas, MisJugadas).
// Eliminar datos guardados en cach� que pudieran quedar
borrar_generarJugadasInmediatas_cacheado :- .abolish(generarJugadasInmediatas_(_, _, _)).

// Cl�usula interfaz para calcular la jugada m�s �ptima seg�n minimax para el nivel de profundidad actual
minimaxVariasJugadas(Jugadas, Profundidad, Maximizar, JugadaOptima, Heuristica) :-
	heuristicaVictoria(MaxHeuristica) &
	heuristicaDerrota(MinHeuristica) &
	valorOptimo(MinHeuristica - 1, MaxHeuristica + 1, Maximizar, HeuristicaComp) & // Maximizar = true -> HeuristicaComp = MaxHeuristica
	minimaxVariasJugadas_impl(Jugadas, Profundidad, Maximizar, JugadaOptima, Heuristica, _, HeuristicaComp).
// Si solo nos queda una jugada a la que aplicar minimax, finalizar la recursividad seg�n sea una nueva �ptima o no.
// Establecemos el caso base en la lista de un elemento para evitar incongruencias en el backtracking de siguientes soluciones,
// y fallar directamente si nos pasan una lista de jugadas vac�a, cosa que no deber�a de ocurrir
minimaxVariasJugadas_impl([Jugada|Cdr], Profundidad, Maximizar, Jugada, HeuristicaActual, _, HeuristicaActual) :-
	Cdr = [] &
	negar(Maximizar, MaximizarAnterior) &
	minimax_impl_cacheado(Jugada, [_, HeuristicaComp], Profundidad, Maximizar) &
	valorOptimo(HeuristicaComp, HeuristicaActual, MaximizarAnterior, HeuristicaOptimaActual) &
	HeuristicaOptimaActual \== HeuristicaActual. // Nueva jugada �ptima
minimaxVariasJugadas_impl([Jugada|Cdr], Profundidad, Maximizar, JugadaOptimaActual, HeuristicaActual, JugadaOptimaActual, HeuristicaActual) :-
	Cdr = [] &
	negar(Maximizar, MaximizarAnterior) &
	minimax_impl_cacheado(Jugada, [_, HeuristicaComp], Profundidad, Maximizar) &
	valorOptimo(HeuristicaComp, HeuristicaActual, MaximizarAnterior, HeuristicaOptimaActual) &
	HeuristicaOptimaActual = HeuristicaActual. // No es nueva jugada �ptima
// Mientras quede m�s de una jugada a la que aplicar minimax, hacerlo
minimaxVariasJugadas_impl([Jugada|Cdr], Profundidad, Maximizar, JugadaOptima, Heuristica, _, HeuristicaActual) :-
	Cdr \== [] &
	negar(Maximizar, MaximizarAnterior) &
	minimax_impl_cacheado(Jugada, [_, HeuristicaComp], Profundidad, Maximizar) &
	valorOptimo(HeuristicaComp, HeuristicaActual, MaximizarAnterior, HeuristicaOptimaActual) &
	HeuristicaOptimaActual \== HeuristicaActual & // Nueva jugada �ptima
	minimaxVariasJugadas_impl(Cdr, Profundidad, Maximizar, JugadaOptima, Heuristica, Jugada, HeuristicaComp).
minimaxVariasJugadas_impl([Jugada|Cdr], Profundidad, Maximizar, JugadaOptima, Heuristica, JugadaOptimaActual, HeuristicaActual) :-
	Cdr \== [] &
	negar(Maximizar, MaximizarAnterior) &
	minimax_impl_cacheado(Jugada, [_, HeuristicaComp], Profundidad, Maximizar) &
	valorOptimo(HeuristicaComp, HeuristicaActual, MaximizarAnterior, HeuristicaOptimaActual) &
	HeuristicaOptimaActual = HeuristicaActual & // No es nueva jugada �ptima
	minimaxVariasJugadas_impl(Cdr, Profundidad, Maximizar, JugadaOptima, Heuristica, JugadaOptimaActual, HeuristicaActual).

// Si no hemos computado ya el resultado de minimax_impl sobre los argumentos dados, hacerlo y guardarlo para reusarlo
minimax_impl_cacheado(Jugada, JugadaYHeuristica, Profundidad, Maximizar) :-
	not minimax_impl_(Jugada, JugadaYHeuristica, Profundidad, Maximizar) &
	minimax_impl(Jugada, JugadaYHeuristica, Profundidad, Maximizar) &
	.assertz(minimax_impl_(Jugada, JugadaYHeuristica, Profundidad, Maximizar)).
// Si ya tenemos el resultado en cach�, sacarlo directamente de ah�, para no tener que volver a repetir los c�lculos
minimax_impl_cacheado(Jugada, JugadaYHeuristica, Profundidad, Maximizar) :- minimax_impl_(Jugada, JugadaYHeuristica, Profundidad, Maximizar).
// Eliminar datos guardados en cach� que pudieran quedar
borrar_minimax_impl_cacheado :- .abolish(minimax_impl_(_, _, _, _)).

// Cl�usula interfaz para generar las jugadas inmediatas a partir de una jugada que
// se considera ya hecha (aunque realmente no sea as�)
generarJugadasInmediatas(JugadaHecha, JugadasGeneradas, MisJugadas) :-
	aplicarJugada(JugadaHecha) &
	.asserta(jugadaHecha(JugadaHecha)) & // Para que el predicado generarJugadasInmediatas_impl pueda descartar unificaciones alternativas
	generarJugadasInmediatas_impl(JugadaHecha, 0, 0, difListas(JugadasGeneradas, []), MisJugadas) &
	deshacerJugada(JugadaHecha) &
	.abolish(jugadaHecha(JugadaHecha)).
// Si no hay un siguiente movimiento, no se pueden generar m�s jugadas, y por tanto
// las nuevas jugadas generadas se corresponden con la lista vac�a
generarJugadasInmediatas_impl(JugadaHecha, SigX, SigY, difListas(JugadasGeneradas, JugadasGeneradas), MisJugadas) :-
	jugadaHecha(JugadaHecha) &
	not siguienteMovimiento(SigX, SigY, MisJugadas, _).
// Si hay un siguiente movimiento, entonces generar una nueva jugada con �l,
// y a�adirla a la lista
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
// Si la casilla actual no est� libre, analizar la siguiente en la coordenada X
siguienteMovimiento(X, Y, MiMovimiento, SigMovimiento) :-
	anchoTablero(Ancho) & altoTablero(Alto) &
	X < Ancho & Y < Alto &
	tablero(X, Y, Id) & Id \== 0 &
	siguienteMovimiento(X + 1, Y, MiMovimiento, SigMovimiento).
// Si la casilla actual est� desbordada en X, analizar la siguiente en la coordenada Y,
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

// Sin jugada que deshacer, no hacer nada (caso base)
deshacerJugada([]).
// Deshacer cada uno de los movimientos
deshacerJugada([Movimiento|Cdr]) :-
	deshacerMovimiento(Movimiento) &
	deshacerJugada(Cdr).

// Deshace la simulaci�n de un movimiento en el tablero
deshacerMovimiento(movimiento(X, Y, SoyYoQuienHaceMov)) :-
	soyYoAIdentificadorJugador(SoyYoQuienHaceMov, Id) &
	.abolish(tablero(X, Y, Id)[source(percept)]) &
	.asserta(tablero(X, Y, 0)[source(percept)]).

// Predicados que obtiene la puntuaci�n heur�stica del estado actual del tablero
// Si yo he ganado, la heur�stica ser� la m�xima
heuristica(Jugada, Valor) :-
	jugadorGano_cacheado(Jugada, true, ValorVerdadGanado) &
	ValorVerdadGanado &
	heuristicaVictoria(Valor).
// Si el contrincante ha ganado, la heur�stica ser� la m�nima
heuristica(Jugada, Valor) :-
	jugadorGano_cacheado(Jugada, false, ValorVerdadPerdido) &
	ValorVerdadPerdido &
	heuristicaDerrota(Valor).
// En otro caso (nadie ha ganado), la heur�stica se calcular� en base a una funci�n ponderada lineal
heuristica(Jugada, Valor) :-
	jugadorGano_cacheado(Jugada, true, ValorVerdadGanado) &
	jugadorGano_cacheado(Jugada, false, ValorVerdadPerdido) &
	not ValorVerdadGanado & not ValorVerdadPerdido &
	heuristicaPonderadaLineal(Valor).

// Calcula una puntuaci�n heur�stica a partir de caracter�sticas del tablero que se consideran positivas (y negativas)
heuristicaPonderadaLineal(10000 * CaracteristicaImpedirVictoria + 180 * CaracteristicaImpedirRaya3 + 200 * CaracteristicaRaya3 + 80 * CaracteristicaRaya2 + 100 * CaracteristicaImpedirRaya2) :-
	caracteristicaImpedirRaya(CaracteristicaImpedirVictoria, 4) &
	caracteristicaRaya(true, CaracteristicaRaya3, 3) &
	caracteristicaImpedirRaya(CaracteristicaImpedirRaya3, 3) &
	caracteristicaRaya(true, CaracteristicaRaya2, 2) &
	caracteristicaImpedirRaya(CaracteristicaImpedirRaya2, 2).

// Cl�usula interfaz que computa la caracter�stica de impedir la formaci�n de una raya de N fichas del rival
caracteristicaImpedirRaya(CaracteristicaImpedirRaya, Fichas) :- caracteristicaImpedirRaya_impl(CaracteristicaImpedirRaya, Fichas, 0, 0, 0).
// Si en la posici�n actual hemos impedido una raya de N fichas, considerarlo para la caracter�stica
caracteristicaImpedirRaya_impl(CaracteristicaImpedirRaya, Fichas, X, Y, Acumulador) :-
	anchoTablero(Ancho) & altoTablero(Alto) &
	X < Ancho & Y < Alto &
	impidoRaya(true, X, Y, Fichas) &
	caracteristicaImpedirRaya_impl(CaracteristicaImpedirRaya, Fichas, X + 1, Y, Acumulador + 1).
// Si en la posici�n actual no hemos impedido una raya de N fichas, pero podemos seguir incrementando X, hacer eso
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
// el valor final de la caracter�stica
caracteristicaImpedirRaya_impl(CaracteristicaImpedirRaya, _, _, Y, CaracteristicaImpedirRaya) :-
	altoTablero(Alto) &
	Y >= Alto.

// Cl�usula interfaz que computa la caracter�stica de formar una raya de N fichas 
caracteristicaRaya(Yo, CaracteristicaRaya, Fichas) :- caracteristicaRaya_impl(Yo, CaracteristicaRaya, Fichas, 0, 0, 0).
// Si en la posici�n actual hemos formado una raya de N fichas, considerarlo para la caracter�stica
caracteristicaRaya_impl(Yo, CaracteristicaRaya, Fichas, X, Y, Acumulador) :-
	anchoTablero(Ancho) & altoTablero(Alto) &
	X < Ancho & Y < Alto &
	raya(Yo, X, Y, Fichas) &
	caracteristicaRaya_impl(Yo, CaracteristicaRaya, Fichas, X + 1, Y, Acumulador + 1).
// Si en la posici�n actual no hemos formado una raya de N fichas, pero podemos seguir incrementando X, hacer eso
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
// el valor final de la caracter�stica
caracteristicaRaya_impl(_, CaracteristicaRaya, _, _, Y, CaracteristicaRaya) :-
	altoTablero(Alto) &
	Y >= Alto.

// Si ya hemos computado el resultado de jugadorGano para la jugada actual, reusarlo. En caso contrario, computarlo una vez
// por jugada
jugadorGano_cacheado(Jugada, Yo, ValorVerdad) :-
	not jugadorGano_(Jugada, Yo, ValorVerdad) &
	jugadorGano(Yo, ValorVerdad) & // El par�metro Jugada tan solo nos interesa para distinguir entre estados del tablero y garantizar coherencia de cach�, no para computar si el jugador ha ganado
	.assertz(jugadorGano_(Jugada, Yo, ValorVerdad)).
jugadorGano_cacheado(Jugada, Yo, ValorVerdad) :- jugadorGano_(Jugada, Yo, ValorVerdad).
// Borra de la base de conocimiento reglas temporales, usadas para recordar resultados parciales
borrar_jugadorGano_cacheado :- .abolish(jugadorGano_(_, _, _)).

// Cl�usula interfaz para comprobar si alguien ha ganado la partida o no. El segundo argumento existe para que se pueda guardar
// en cach� el valor de salida de tal argumento, en vez de si se ha encontrado una soluci�n o no.
// Diferencia principal entre esta regla y caracteristicaRaya: detiene la evaluaci�n de rayas al encontrar la primera de 4,
// y no sigue hasta el final del tablero, por lo que es algo m�s eficiente
jugadorGano(Yo, ValorVerdad) :- jugadorGano_impl(0, 0, Yo, ValorVerdad).

// Ha ganado si partiendo de esta casilla hay 4 en raya en cualquiera de las direcciones posibles
jugadorGano_impl(X, Y, Yo, true) :-
	anchoTablero(Ancho) & altoTablero(Alto) &
	X < Ancho & Y < Alto &
	raya(Yo, X, Y, 4).
// Si no hay un 4 en raya en cualquiera de las direcciones posibles, y podemos ver si lo hay en la siguiente coordenada X,
// hacer tal comprobaci�n
jugadorGano_impl(X, Y, Yo, ValorVerdad) :-
	anchoTablero(Ancho) & altoTablero(Alto) &
	X < Ancho & Y < Alto &
	not raya(Yo, X, Y, 4) & // Para evitar backtracking por aqu�. Quiz�s sea buena idea quitarlo si eso no es problema
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

// Regla que es cierta si y solo si un jugador ha hecho una raya de N fichas, considerando la posici�n (X, Y) como la ficha
// central de la hipot�tica raya
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
// Cl�usula interfaz para comprobar si, en el vector de direcci�n dado, hay N fichas m�s a partir de (X, Y) que pueden formar cuatro en raya
rayaEnDireccion(Yo, X, Y, Fichas, DX, DY) :- rayaEnDireccion_impl(Yo, X, Y, Fichas, 1, DX, DY).
// Una raya de una ficha siempre se cumple en nuestro caso
rayaEnDireccion_impl(_, _, _, 0, _, _, _).
// Ver si la raya en esta direcci�n se mantiene hasta agotar el n�mero de fichas deseado
rayaEnDireccion_impl(Yo, X, Y, Fichas, FichasContadas, DX, DY) :-
	Fichas > 0 &
	fichaEn(Yo, X + DX * FichasContadas, Y + DY * FichasContadas) &
	rayaEnDireccion_impl(Yo, X, Y, Fichas - 1, FichasContadas + 1, DX, DY).

// Regla que es cierta si y solo si un jugador impide al otro hacer una raya de N fichas, considerando la posici�n (X, Y)
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
// Cl�usula interfaz para comprobar si, en el vector de direcci�n dado, impido una raya de N fichas a partir de (X, Y), colocando una
// a distancia N + 1
impidoRayaEnDireccion(Yo, X, Y, FichasRestantes, DX, DY) :-
	negar(Yo, Otro) &
	rayaEnDireccion(Otro, X, Y, FichasRestantes - 1, DX, DY) & // Solo se puede impedir una raya en la misma direcci�n
	fichaEn(Yo, X + DX * FichasRestantes, Y + DY * FichasRestantes).

// Regla que es cierta si y solo si la posici�n (X, Y) tiene una ficha m�a o del otro jugador
fichaEn(Mia, X, Y) :-
	soyYoAIdentificadorJugador(Mia, Id) &
	tablero(X, Y, Id).

// Borra de la base de conocimiento reglas temporales, usadas para recordar resultados parciales
borrarCachesHeuristica :-
	.abolish(jugadorGano_(_, _, _)).

// Concatena dos listas expresadas como diferencias de listas.
// Esta operaci�n es de complejidad O(1)
append_dl(difListas(Inicio1, Fin1), difListas(Fin1, Fin2), difListas(Inicio1, Fin2)).

// Concatena dos listas de manera trivial.
// Esta operaci�n es de complejidad O(n), pero funciona en listas cerradas
append_simple([], L, L).
append_simple([Car|Cdr], L, [Car|R]) :- append_simple(Cdr, L, R).

// Obtiene la negaci�n de un valor de verdad en un argumento, utilizando la negaci�n por fallo disponible en Jason
negar(ValorVerdad, false) :- ValorVerdad.
negar(ValorVerdad, true) :- not ValorVerdad.

// Obtiene el valor m�nimo de dos variables
valorMinimo(A, B, A) :- A < B.
valorMinimo(A, B, B) :- A >= B.

// Obtiene el valor m�ximo de dos variables
valorMaximo(A, B, B) :- B >= A.
valorMaximo(A, B, A) :- A > B.

// Obtiene el valor �ptimo de entre los dados, que puede ser el m�ximo o el m�nimo, dependiendo de un argumento que se pase
valorOptimo(A, B, true, Maximo) :- valorMaximo(A, B, Maximo).
valorOptimo(A, B, false, Minimo) :- valorMinimo(A, B, Minimo).

// Si el primer par�metro es verdadero, unifica Id con el ID del jugador actual,
// que es el mismo que se usa en los predicados de funtor tablero/3.
// Si el primer par�metro es falso, entonces unifica Id con el ID del otro jugador.
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

// Al ser nuestro turno seg�n el entorno, hacer el mejor movimiento
+turno(Yo)[source(percept)] : .my_name(Yo) <-
	.wait(1000); // Por si estamos recibiendo todav�a percepciones del tablero

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
