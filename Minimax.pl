% Idea intuitiva: ir generando un árbol de juego de profundidad limitada, y
% escoger en cada nivel la jugada que maximiza el beneficio propio (nos lleva
% más cerca a la victoria) y minimiza el beneficio ajeno (hace más difícil que
% el rival gane)

% Elemento del árbol de juego (Jugada): [ movimiento(X, Y, SoyYoQuienHaceMov), movimiento(X, Y, SoyYoQuienHaceMov), ... ]

% La profundidad máxima del árbol de juego a considerar. Afecta a la
% corrección y tiempo de ejecución del algoritmo: más profundidad aumenta
% la calidad de las jugadas, a costa de mayor tiempo de ejecución y consumo de
% memoria
profundidadArbolJuego(2).

% Las dimensiones del tablero
anchoTablero(8).
altoTablero(8).

% El valor devuelto por la heurística para señalar una victoria. Es el valor
% máximo posible que puede tomar la heurística
heuristicaVictoria(999999).
% El valor devuelto por la heurística para señalar una derrota. Es el valor
% mínimo posible que puede tomar la heurística
heuristicaDerrota(-999999).

% El estado actual del tablero. Se asume que el estado es completo
% y consistente
:- dynamic tablero/3.
tablero(0, 0, 0).
tablero(0, 1, 0).
tablero(0, 2, 0).
tablero(0, 3, 0).
tablero(0, 4, 0).
tablero(0, 5, 0).
tablero(0, 6, 0).
tablero(0, 7, 0).
tablero(1, 0, 0).
tablero(1, 1, 0).
tablero(1, 2, 0).
tablero(1, 3, 0).
tablero(1, 4, 0).
tablero(1, 5, 0).
tablero(1, 6, 0).
tablero(1, 7, 0).
tablero(2, 0, 0).
tablero(2, 1, 0).
tablero(2, 2, 0).
tablero(2, 3, 0).
tablero(2, 4, 0).
tablero(2, 5, 0).
tablero(2, 6, 0).
tablero(2, 7, 0).
tablero(3, 0, 0).
tablero(3, 1, 0).
tablero(3, 2, 0).
tablero(3, 3, 0).
tablero(3, 4, 0).
tablero(3, 5, 0).
tablero(3, 6, 0).
tablero(3, 7, 0).
tablero(4, 0, 0).
tablero(4, 1, 0).
tablero(4, 2, 0).
tablero(4, 3, 0).
tablero(4, 4, 0).
tablero(4, 5, 0).
tablero(4, 6, 0).
tablero(4, 7, 0).
tablero(5, 0, 0).
tablero(5, 1, 0).
tablero(5, 2, 0).
tablero(5, 3, 0).
tablero(5, 4, 0).
tablero(5, 5, 0).
tablero(5, 6, 0).
tablero(5, 7, 0).
tablero(6, 0, 0).
tablero(6, 1, 0).
tablero(6, 2, 0).
tablero(6, 3, 0).
tablero(6, 4, 0).
tablero(6, 5, 0).
tablero(6, 6, 0).
tablero(6, 7, 0).
tablero(7, 0, 0).
tablero(7, 1, 0).
tablero(7, 2, 0).
tablero(7, 3, 0).
tablero(7, 4, 0).
tablero(7, 5, 0).
tablero(7, 6, 0).
tablero(7, 7, 0).

% Cláusulas interfaz para obtener la casilla donde colocar una ficha para maximizar
% nuestra victoria o la del contrincante
mejorSiguienteCasilla(X, Y) :- minimax([[movimiento(X, Y, _)], _]).
peorSiguienteCasilla(X, Y) :- maximin([[movimiento(X, Y, _)], _]).

% Cláusula interfaz para obtener la jugada óptima a realizar, con su heurística asociada
minimax(JugadaYHeuristica) :-
	profundidadArbolJuego(P),
	minimax_impl([], JugadaYHeuristica, P, true),
	borrar_generarJugadasInmediatas_cacheado. % Borra datos en caché para partir de un estado limpio
:- dynamic haciendoMaximin/0.
% Cláusula interfaz para obtener la jugada óptima a realizar para perder el juego, con su heurística asociada
maximin(JugadaYHeuristica) :-
	profundidadArbolJuego(P),
	asserta(haciendoMaximin), % Para que no se tenga en cuenta la inversión del jugador que maximiza para generar jugadas
	minimax_impl([], JugadaYHeuristica, P, false),
	retract(haciendoMaximin),
	borrar_generarJugadasInmediatas_cacheado. % Borra datos en caché para partir de un estado limpio
% Si la profundidad restante es cero, no generar hijos para este nodo,
% y considerar la heurística del nodo como la heurística de la jugada que representa
% (caso base)
minimax_impl(JugadaActual, [JugadaActual, Heuristica], 0, _) :-
	aplicarJugada(JugadaActual),
	heuristica(Heuristica),
	deshacerJugada(JugadaActual).
% Si la profundidad restante no es cero, generar hijos para este nodo del árbol
% y considerar la heurística del nodo como la heurística máxima o mínima de las jugadas
% hijas
minimax_impl(JugadaActual, [JugadaOptima, Heuristica], Profundidad, true) :- % true -> maximizar, mis jugadas
	misJugadasTeniendoCuentaMaximin(true, MisJugadas),
	generarJugadasInmediatas_cacheado(JugadaActual, Jugadas, MisJugadas), % Para reducir el tiempo de ejecución por el backtracking
	Jugadas \= [],
	Profundidad > 0, % Para no unificar con lo que debe atenderse por minimax_impl(JugadaActual, [JugadaActual, Heuristica], 0, _)
	NuevaProfundidad is Profundidad - 1,
	minimaxVariasJugadas(Jugadas, JugadasYHeuristicas, NuevaProfundidad, false),
	maximaHeuristica(JugadasYHeuristicas, JugadaOptima, Heuristica).
% Los nodos sin más jugadas posibles son terminales, y su heurística se calcula directamente
minimax_impl(JugadaActual, [JugadaActual, Heuristica], _, true) :- % true -> maximizar, mis jugadas
	misJugadasTeniendoCuentaMaximin(true, MisJugadas),
	generarJugadasInmediatas_cacheado(JugadaActual, Jugadas, MisJugadas), % Para reducir el tiempo de ejecución por el backtracking
	Jugadas = [],
	aplicarJugada(JugadaActual),
	heuristica(Heuristica),
	deshacerJugada(JugadaActual).
minimax_impl(JugadaActual, [JugadaOptima, Heuristica], Profundidad, false) :- % false -> minimizar, jugadas del oponente
	misJugadasTeniendoCuentaMaximin(false, MisJugadas),
	generarJugadasInmediatas_cacheado(JugadaActual, Jugadas, MisJugadas), % Para reducir el tiempo de ejecución por el backtracking
	Jugadas \= [],
	Profundidad > 0,
	NuevaProfundidad is Profundidad - 1,
	minimaxVariasJugadas(Jugadas, JugadasYHeuristicas, NuevaProfundidad, true),
	minimaHeuristica(JugadasYHeuristicas, JugadaOptima, Heuristica).
% Los nodos sin más jugadas posibles son terminales, y su heurística se calcula directamente
minimax_impl(JugadaActual, [JugadaActual, Heuristica], _, false) :- % false -> minimizar, jugadas del oponente
	misJugadasTeniendoCuentaMaximin(false, MisJugadas),
	generarJugadasInmediatas_cacheado(JugadaActual, Jugadas, MisJugadas), % Para reducir el tiempo de ejecución por el backtracking
	Jugadas = [],
	aplicarJugada(JugadaActual),
	heuristica(Heuristica),
	deshacerJugada(JugadaActual).

% Devuelve el valor apropiado de MiJugada para la generación de jugadas, teniendo en cuenta si se está ejecutando minimax o bien maximin.
% Esencialmente, estas cláusulas hacen que resultado = MiJugada XOR haciendoMaximin.
misJugadasTeniendoCuentaMaximin(MiJugada, true) :- (not(MiJugada), haciendoMaximin); (MiJugada, not(haciendoMaximin)).
misJugadasTeniendoCuentaMaximin(MiJugada, false) :- (not(MiJugada), not(haciendoMaximin)); (MiJugada, haciendoMaximin).

:- dynamic generarJugadasInmediatas_/3.
% Si no hemos computado ya el resultado de generarJugadasInmediatas sobre los argumentos dados, hacerlo y guardarlo para reusarlo
generarJugadasInmediatas_cacheado(JugadaActual, Jugadas, MisJugadas) :-
	not(generarJugadasInmediatas_(JugadaActual, Jugadas, MisJugadas)),
	generarJugadasInmediatas(JugadaActual, Jugadas, MisJugadas),
	assertz(generarJugadasInmediatas_(JugadaActual, Jugadas, MisJugadas)).
% Si ya tenemos el resultado en caché, sacarlo directamente de ahí, para no tener que volver a repetir los cálculos
generarJugadasInmediatas_cacheado(JugadaActual, Jugadas, MisJugadas) :- generarJugadasInmediatas_(JugadaActual, Jugadas, MisJugadas).
% Eliminar datos guardados en caché que pudieran quedar
borrar_generarJugadasInmediatas_cacheado :- retractall(generarJugadasInmediatas_(_, _, _)).

% Si no tenemos jugadas a las que aplicar minimax, no hacerlo (caso base)
minimaxVariasJugadas([], [], _, _).
% Mientras queden jugadas a las que aplicar minimax, guardar los resultados de
% minimax en una lista
minimaxVariasJugadas([Jugada|Cdr], [[Jugada, Heuristica]|JugadasYHeuristicas], Profundidad, Maximizar) :-
	minimax_impl(Jugada, [_, Heuristica], Profundidad, Maximizar),
	minimaxVariasJugadas(Cdr, JugadasYHeuristicas, Profundidad, Maximizar).

% Cláusula interfaz para obtener la jugada que minimiza su valor de heurística
minimaHeuristica(JugadasYHeuristicas, JugadaOptima, HeuristicaMinima) :-
	heuristicaVictoria(HeuristicaVictoria),
	minimaHeuristica_impl(JugadasYHeuristicas, JugadaOptima, _, HeuristicaMinima, HeuristicaVictoria).
% Si no hay más hijos que recorrer, la heurística mínima final es la actual
minimaHeuristica_impl([], OptimaActual, OptimaActual, MinimaActual, MinimaActual).
% Si la heurística actual es menor que la mínima actual, entonces la nueva mínima actual debería de ser la actual
minimaHeuristica_impl([[JugadaActual, HeuristicaActual]|Cdr], JugadaOptima, _, HeuristicaMinima, MinimaActual) :- HeuristicaActual < MinimaActual,
																												  minimaHeuristica_impl(Cdr, JugadaOptima, JugadaActual, HeuristicaMinima, HeuristicaActual).
% Si la heurística actual es mayor o igual que la mínima actual, entonces la nueva mínima actual debe de seguir como está
minimaHeuristica_impl([[_, HeuristicaActual]|Cdr], JugadaOptima, OptimaActual, HeuristicaMinima, MinimaActual) :- HeuristicaActual >= MinimaActual,
																												  minimaHeuristica_impl(Cdr, JugadaOptima, OptimaActual, HeuristicaMinima, MinimaActual).

% Cláusula interfaz para obtener la jugada que maximiza su valor de heurística
maximaHeuristica(JugadasYHeuristicas, JugadaOptima, HeuristicaMaxima) :-
	heuristicaDerrota(HeuristicaDerrota),
	maximaHeuristica_impl(JugadasYHeuristicas, JugadaOptima, _, HeuristicaMaxima, HeuristicaDerrota).
% Si no hay más hijos que recorrer, la heurística máxima es la actual
maximaHeuristica_impl([], OptimaActual, OptimaActual, MaximaActual, MaximaActual).
% Si la heurística actual es mayor que la máxima actual, entonces la nueva máxima actual debería de ser la actual
maximaHeuristica_impl([[JugadaActual, HeuristicaActual]|Cdr], JugadaOptima, _, HeuristicaMaxima, MaximaActual) :- HeuristicaActual > MaximaActual,
																												  maximaHeuristica_impl(Cdr, JugadaOptima, JugadaActual, HeuristicaMaxima, HeuristicaActual).
% Si la heurística actual es menor o igual que la máxima actual, entonces la nueva máxima actual debe de seguir como está
maximaHeuristica_impl([[_, HeuristicaActual]|Cdr], JugadaOptima, OptimaActual, HeuristicaMaxima, MaximaActual) :- MaximaActual >= HeuristicaActual,
																												  maximaHeuristica_impl(Cdr, JugadaOptima, OptimaActual, HeuristicaMaxima, MaximaActual).

% Cláusula interfaz para generar las jugadas inmediatas a partir de una jugada que
% se considera ya hecha (aunque realmente no sea así)
generarJugadasInmediatas(JugadaHecha, JugadasGeneradas, MisJugadas) :-
	aplicarJugada(JugadaHecha),
	asserta(jugadaHecha(JugadaHecha)), % Para que el predicado generarJugadasInmediatas_impl pueda descartar unificaciones alternativas (es curioso que tenga que estar haciendo esto en lugar de haber usado un corte, que es más eficiente, pero por razones filosóficas no está en AS)
	generarJugadasInmediatas_impl(JugadaHecha, 0, 0, difListas(JugadasGeneradas, []), MisJugadas),
	deshacerJugada(JugadaHecha),
	retract(jugadaHecha(JugadaHecha)).
% Si no hay un siguiente movimiento, no se pueden generar más jugadas, y por tanto
% las nuevas jugadas generadas se corresponden con la lista vacía
generarJugadasInmediatas_impl(JugadaHecha, SigX, SigY, difListas(JugadasGeneradas, JugadasGeneradas), MisJugadas) :-
	jugadaHecha(JugadaHecha),
	not(siguienteMovimiento(SigX, SigY, MisJugadas, _)).
% Si hay un siguiente movimiento, entonces generar una nueva jugada con él,
% y añadirla a la lista
generarJugadasInmediatas_impl(JugadaHecha, SigX, SigY, JugadasGeneradas, MisJugadas) :-
	jugadaHecha(JugadaHecha),
	siguienteMovimiento(SigX, SigY, MisJugadas, movimiento(X, Y, MiMovimiento)),
	NuevoSigX is X + 1,
	generarJugadasInmediatas_impl(JugadaHecha, NuevoSigX, Y, difListas(InicioJugadasGeneradas, FinJugadasGeneradas), MisJugadas),
	append_simple(JugadaHecha, [movimiento(X, Y, MiMovimiento)], NuevaJugada), % No vamos a tener que iterar sobre muchas jugadas
	append_dl(difListas([NuevaJugada|Cdr3], Cdr3), difListas(InicioJugadasGeneradas, FinJugadasGeneradas), JugadasGeneradas).

% Podemos hacer un movimiento en la primera casilla libre que encontremos
siguienteMovimiento(X, Y, MiMovimiento, movimiento(X, Y, MiMovimiento)) :-
	anchoTablero(Ancho), altoTablero(Alto),
	X < Ancho, Y < Alto,
	tablero(X, Y, Id), Id = 0.
% Si la casilla actual no está libre, analizar la siguiente en la coordenada X
siguienteMovimiento(X, Y, MiMovimiento, SigMovimiento) :-
	anchoTablero(Ancho), altoTablero(Alto),
	X < Ancho, Y < Alto,
	tablero(X, Y, Id), Id \= 0,
	XSig is X + 1,
	siguienteMovimiento(XSig, Y, MiMovimiento, SigMovimiento).
% Si la casilla actual está desbordada en X, analizar la siguiente en la coordenada Y,
% porque ya hemos agotado todas las coordenadas X de la Y anterior
siguienteMovimiento(X, Y, MiMovimiento, SigMovimiento) :-
	anchoTablero(Ancho), altoTablero(Alto),
	AltoComp is Alto - 1,
	X == Ancho, Y < AltoComp,
	YSig is Y + 1,
	siguienteMovimiento(0, YSig, MiMovimiento, SigMovimiento).

% Sin jugada que aplicar, no hacer nada (caso base)
aplicarJugada([]).
% Aplicar cada uno de los movimientos
aplicarJugada([Movimiento|Cdr]) :-
	aplicarMovimiento(Movimiento),
	aplicarJugada(Cdr).

% Simula un movimiento en el tablero
aplicarMovimiento(movimiento(X, Y, SoyYoQuienHaceMov)) :-
	soyYoAIdentificadorJugador(SoyYoQuienHaceMov, Id),
	retract(tablero(X, Y, 0)), % abolish en Jason
	asserta(tablero(X, Y, Id)).

% Sin jugada que deshacer, no hacer nada (caso base)
deshacerJugada([]).
% Deshacer cada uno de los movimientos
deshacerJugada([Movimiento|Cdr]) :-
	deshacerMovimiento(Movimiento),
	deshacerJugada(Cdr).

% Deshace la simulación de un movimiento en el tablero
deshacerMovimiento(movimiento(X, Y, SoyYoQuienHaceMov)) :-
	soyYoAIdentificadorJugador(SoyYoQuienHaceMov, Id),
	retract(tablero(X, Y, Id)), % abolish en Jason
	asserta(tablero(X, Y, 0)).

% Concatena dos listas expresadas como diferencias de listas.
% Esta operación es de complejidad O(1)
append_dl(difListas(Inicio1, Fin1), difListas(Fin1, Fin2), difListas(Inicio1, Fin2)).

% Concatena dos listas de manera trivial.
% Esta operación es de complejidad O(n), pero funciona en listas cerradas
append_simple([], L, L).
append_simple([Car|Cdr], L, [Car|R]) :- append_simple(Cdr, L, R).

% Predicado que obtiene la puntuación heurística del estado actual del tablero
% TODO: la implementación final real de este predicado
heuristica(R) :- random(-2000, 2000, R).

% FIXME: predicados de prueba
soyYoAIdentificadorJugador(true, 1).
soyYoAIdentificadorJugador(false, 2).