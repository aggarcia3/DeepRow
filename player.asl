// Agente player en proyecto DeepRow.mas2j

/* Creencias y reglas iniciales */
// La profundidad m�xima del �rbol de juego a considerar. Afecta a la
// correcci�n y tiempo de ejecuci�n del algoritmo: m�s profundidad aumenta
// la calidad de las jugadas, a costa de mayor tiempo de ejecuci�n y consumo de
// memoria
profundidadArbolJuego(2). // En SWI-Prolog, estas mismas reglas se ejecutan en mucho menos tiempo que en Jason ;-(

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
	borrar_generarJugadasInmediatas_cacheado. // Borra datos en cach� para partir de un estado limpio
// Cl�usula interfaz para obtener la jugada �ptima a realizar para perder el juego, con su heur�stica asociada
maximin(JugadaYHeuristica) :-
	profundidadArbolJuego(P) &
	.asserta(haciendoMaximin) & // Para que no se tenga en cuenta la inversi�n del jugador que maximiza para generar jugadas
	minimax_impl([], JugadaYHeuristica, P, false) &
	.abolish(haciendoMaximin) &
	borrar_generarJugadasInmediatas_cacheado. // Borra datos en cach� para partir de un estado limpio
// Si la profundidad restante es cero, no generar hijos para este nodo,
// y considerar la heur�stica del nodo como la heur�stica de la jugada que representa
// (caso base)
minimax_impl(JugadaActual, [JugadaActual, Heuristica], 0, _) :-
	aplicarJugada(JugadaActual) &
	heuristica(Heuristica) &
	deshacerJugada(JugadaActual).
// Si la profundidad restante no es cero, generar hijos para este nodo del �rbol
// y considerar la heur�stica del nodo como la heur�stica m�xima o m�nima de las jugadas
// hijas
minimax_impl(JugadaActual, [JugadaOptima, Heuristica], Profundidad, true) :- // true -> maximizar, mis jugadas
	misJugadasTeniendoCuentaMaximin(true, MisJugadas) &
	generarJugadasInmediatas_cacheado(JugadaActual, Jugadas, MisJugadas) & // Para reducir el tiempo de ejecuci�n por el backtracking
	Jugadas \== [] &
	Profundidad > 0 & // Para no unificar con lo que debe atenderse por minimax_impl(JugadaActual, [JugadaActual, Heuristica], 0, _)
	minimaxVariasJugadas(Jugadas, JugadasYHeuristicas, Profundidad - 1, false) &
	maximaHeuristica(JugadasYHeuristicas, JugadaOptima, Heuristica).
// Los nodos sin m�s jugadas posibles son terminales, y si heur�stica se calcula directamente
minimax_impl(JugadaActual, [JugadaActual, Heuristica], _, true) :- // true -> maximizar, mis jugadas
	misJugadasTeniendoCuentaMaximin(true, MisJugadas) &
	generarJugadasInmediatas_cacheado(JugadaActual, Jugadas, MisJugadas) & // Para reducir el tiempo de ejecuci�n por el backtracking
	Jugadas = [] &
	aplicarJugada(JugadaActual) &
	heuristica(JugadaActual, Heuristica) &
	deshacerJugada(JugadaActual).
minimax_impl(JugadaActual, [JugadaOptima, Heuristica], Profundidad, false) :- // false -> minimizar, jugadas del oponente
	misJugadasTeniendoCuentaMaximin(false, MisJugadas) &
	generarJugadasInmediatas_cacheado(JugadaActual, Jugadas, MisJugadas) & // Para reducir el tiempo de ejecuci�n por el backtracking
	Jugadas \== [] &
	Profundidad > 0 &
	minimaxVariasJugadas(Jugadas, JugadasYHeuristicas, Profundidad - 1, true) &
	minimaHeuristica(JugadasYHeuristicas, JugadaOptima, Heuristica).
// Los nodos sin m�s jugadas posibles son terminales, y si heur�stica se calcula directamente
minimax_impl(JugadaActual, [JugadaActual, Heuristica], _, false) :- // false -> minimizar, jugadas del oponente
	misJugadasTeniendoCuentaMaximin(false, MisJugadas) &
	generarJugadasInmediatas_cacheado(JugadaActual, Jugadas, MisJugadas) & // Para reducir el tiempo de ejecuci�n por el backtracking
	Jugadas = [] &
	aplicarJugada(JugadaActual) &
	heuristica(Heuristica) &
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

// Si no tenemos jugadas a las que aplicar minimax, no hacerlo (caso base)
minimaxVariasJugadas([], [], _, _).
// Mientras queden jugadas a las que aplicar minimax, guardar los resultados de
// minimax en una lista
minimaxVariasJugadas([Jugada|Cdr], [[Jugada, Heuristica]|JugadasYHeuristicas], Profundidad, Maximizar) :-
	minimax_impl(Jugada, [_, Heuristica], Profundidad, Maximizar) &
	minimaxVariasJugadas(Cdr, JugadasYHeuristicas, Profundidad, Maximizar).

// Cl�usula interfaz para obtener la jugada que minimiza su valor de heur�stica
minimaHeuristica(JugadasYHeuristicas, JugadaOptima, HeuristicaMinima) :-
	heuristicaVictoria(HeuristicaVictoria) &
	minimaHeuristica_impl(JugadasYHeuristicas, JugadaOptima, _, HeuristicaMinima, HeuristicaVictoria).
// Si no hay m�s hijos que recorrer, la heur�stica m�nima final es la actual
minimaHeuristica_impl([], OptimaActual, OptimaActual, MinimaActual, MinimaActual).
// Si la heur�stica actual es menor que la m�nima actual, entonces la nueva m�nima actual deber�a de ser la actual
minimaHeuristica_impl([[JugadaActual, HeuristicaActual]|Cdr], JugadaOptima, _, HeuristicaMinima, MinimaActual) :- HeuristicaActual < MinimaActual &
																												  minimaHeuristica_impl(Cdr, JugadaOptima, JugadaActual, HeuristicaMinima, HeuristicaActual).
// Si la heur�stica actual es mayor o igual que la m�nima actual, entonces la nueva m�nima actual debe de seguir como est�
minimaHeuristica_impl([[_, HeuristicaActual]|Cdr], JugadaOptima, OptimaActual, HeuristicaMinima, MinimaActual) :- HeuristicaActual >= MinimaActual &
																												  minimaHeuristica_impl(Cdr, JugadaOptima, OptimaActual, HeuristicaMinima, MinimaActual).

// Cl�usula interfaz para obtener la jugada que maximiza su valor de heur�stica
maximaHeuristica(JugadasYHeuristicas, JugadaOptima, HeuristicaMaxima) :-
	heuristicaDerrota(HeuristicaDerrota) &
	maximaHeuristica_impl(JugadasYHeuristicas, JugadaOptima, _, HeuristicaMaxima, HeuristicaDerrota).
// Si no hay m�s hijos que recorrer, la heur�stica m�xima es la actual
maximaHeuristica_impl([], OptimaActual, OptimaActual, MaximaActual, MaximaActual).
// Si la heur�stica actual es mayor que la m�xima actual, entonces la nueva m�xima actual deber�a de ser la actual
maximaHeuristica_impl([[JugadaActual, HeuristicaActual]|Cdr], JugadaOptima, _, HeuristicaMaxima, MaximaActual) :- HeuristicaActual > MaximaActual &
																												  maximaHeuristica_impl(Cdr, JugadaOptima, JugadaActual, HeuristicaMaxima, HeuristicaActual).
// Si la heur�stica actual es menor o igual que la m�xima actual, entonces la nueva m�xima actual debe de seguir como est�
maximaHeuristica_impl([[_, HeuristicaActual]|Cdr], JugadaOptima, OptimaActual, HeuristicaMaxima, MaximaActual) :- MaximaActual >= HeuristicaActual &
																												  maximaHeuristica_impl(Cdr, JugadaOptima, OptimaActual, HeuristicaMaxima, MaximaActual).

// Cl�usula interfaz para generar las jugadas inmediatas a partir de una jugada que
// se considera ya hecha (aunque realmente no sea as�)
generarJugadasInmediatas(JugadaHecha, JugadasGeneradas, MisJugadas) :-
	aplicarJugada(JugadaHecha) &
	.asserta(jugadaHecha(JugadaHecha)) & // Para que el predicado generarJugadasInmediatas_impl pueda descartar unificaciones alternativas (es curioso que tenga que estar haciendo esto en lugar de haber usado un corte, que es m�s eficiente, pero por razones filos�ficas no est� en AS)
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

// Concatena dos listas expresadas como diferencias de listas.
// Esta operaci�n es de complejidad O(1)
append_dl(difListas(Inicio1, Fin1), difListas(Fin1, Fin2), difListas(Inicio1, Fin2)).

// Concatena dos listas de manera trivial.
// Esta operaci�n es de complejidad O(n), pero funciona en listas cerradas
append_simple([], L, L).
append_simple([Car|Cdr], L, [Car|R]) :- append_simple(Cdr, L, R).

// Predicado que obtiene la puntuaci�n heur�stica del estado actual del tablero
// TODO: la implementaci�n final real de este predicado
heuristica(math.floor(-1000 + R * 2000)) :- .random(R).

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
