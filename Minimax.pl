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
tablero(2, 1, 1).
tablero(2, 2, 0).
tablero(2, 3, 0).
tablero(2, 4, 0).
tablero(2, 5, 0).
tablero(2, 6, 0).
tablero(2, 7, 0).
tablero(3, 0, 0).
tablero(3, 1, 0).
tablero(3, 2, 2).
tablero(3, 3, 1).
tablero(3, 4, 1).
tablero(3, 5, 1).
tablero(3, 6, 2).
tablero(3, 7, 0).
tablero(4, 0, 0).
tablero(4, 1, 0).
tablero(4, 2, 0).
tablero(4, 3, 2).
tablero(4, 4, 0).
tablero(4, 5, 2).
tablero(4, 6, 0).
tablero(4, 7, 0).
tablero(5, 0, 0).
tablero(5, 1, 0).
tablero(5, 2, 0).
tablero(5, 3, 0).
tablero(5, 4, 2).
tablero(5, 5, 0).
tablero(5, 6, 0).
tablero(5, 7, 0).
tablero(6, 0, 0).
tablero(6, 1, 0).
tablero(6, 2, 0).
tablero(6, 3, 1).
tablero(6, 4, 0).
tablero(6, 5, 1).
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
:- dynamic mejorJugadaEnRespuestaAJugada/2.
minimax([Jugada, Heuristica]) :-
	profundidadArbolJuego(P),
	minimax_impl([], [_, Heuristica], P, true),
	mejorJugadaEnRespuestaAJugada([], Jugada),
	% Borra datos en caché para partir de un estado limpio
	borrar_generarJugadasInmediatas_cacheado,
	borrar_jugadorGano_cacheado,
	retractall(mejorJugadaEnRespuestaAJugada(_, _)).
:- dynamic haciendoMaximin/0.
% Cláusula interfaz para obtener la jugada óptima a realizar para perder el juego, con su heurística asociada
maximin([Jugada, Heuristica]) :-
	profundidadArbolJuego(P),
	asserta(haciendoMaximin), % Para que no se tenga en cuenta la inversión del jugador que maximiza para generar jugadas
	minimax_impl([], [_, Heuristica], P, false),
	mejorJugadaEnRespuestaAJugada([], Jugada),
	retract(haciendoMaximin),
	% Borra datos en caché para partir de un estado limpio
	borrar_generarJugadasInmediatas_cacheado,
	borrar_jugadorGano_cacheado,
	retractall(mejorJugadaEnRespuestaAJugada(_, _)).
% Si la profundidad restante es cero, no generar hijos para este nodo,
% y considerar la heurística del nodo como la heurística de la jugada que representa
% (caso base)
minimax_impl(JugadaActual, [JugadaActual, Heuristica], 0, _) :-
	aplicarJugadaSoloSiNoAplicada(JugadaActual),
	heuristica(JugadaActual, Heuristica),
	deshacerJugadaSoloSiAplicada(JugadaActual).
% Si la profundidad restante no es cero, generar hijos para este nodo del árbol
% y considerar la heurística del nodo como la heurística máxima o mínima de las jugadas
% hijas
minimax_impl(JugadaActual, [JugadaActual, Heuristica], Profundidad, Maximizar) :- % true -> maximizar, mis jugadas; false -> minimizar, jugadas del oponente
	Profundidad > 0, % Para no unificar con lo que debe atenderse por la primera regla
	misJugadasTeniendoCuentaMaximin(Maximizar, MisJugadas),
	generarJugadasInmediatas_cacheado(JugadaActual, Jugadas, MisJugadas), % Para reducir el tiempo de ejecución empleado por el backtracking
	Jugadas \= [],
	NuevaProfundidad is Profundidad - 1,
	negar(Maximizar, NuevoMaximizar),
	minimaxVariasJugadas(Jugadas, JugadasYHeuristicas, NuevaProfundidad, NuevoMaximizar),
	heuristicaOptima(JugadasYHeuristicas, JugadaOptima, Heuristica, Maximizar),
	asserta(mejorJugadaEnRespuestaAJugada(JugadaActual, JugadaOptima)).
% Los nodos sin más jugadas posibles son terminales, y su heurística se calcula directamente
minimax_impl(JugadaActual, [JugadaActual, Heuristica], Profundidad, Maximizar) :-
	Profundidad > 0, % Para no unificar con lo que debe atenderse por la primera regla
	misJugadasTeniendoCuentaMaximin(Maximizar, MisJugadas),
	generarJugadasInmediatas_cacheado(JugadaActual, Jugadas, MisJugadas), % Para reducir el tiempo de ejecución empleado por el backtracking
	Jugadas = [],
	aplicarJugadaSoloSiNoAplicada(JugadaActual),
	heuristica(JugadaActual, Heuristica),
	deshacerJugadaSoloSiAplicada(JugadaActual).

% Devuelve el valor apropiado de MisJugadas para la generación de jugadas, teniendo en cuenta si se está ejecutando minimax o bien maximin.
% Esencialmente, estas cláusulas hacen que resultado = MiJugada XOR haciendoMaximin.
misJugadasTeniendoCuentaMaximin(MisJugadas, true) :- (not(MisJugadas), haciendoMaximin); (MisJugadas, not(haciendoMaximin)).
misJugadasTeniendoCuentaMaximin(MisJugadas, false) :- (not(MisJugadas), not(haciendoMaximin)); (MisJugadas, haciendoMaximin).

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
	minimax_impl(Jugada, [Jugada, Heuristica], Profundidad, Maximizar),
	minimaxVariasJugadas(Cdr, JugadasYHeuristicas, Profundidad, Maximizar).

% Cláusula interfaz para obtener la jugada que optimiza su valor de heurística
heuristicaOptima(JugadasYHeuristicas, JugadaOptima, HeuristicaOptima, Maximizar) :-
	heuristicaVictoria(MaxHeuristica),
	MaxHeuristicaProc is MaxHeuristica + 1, % Para que la condición de igualdad en minimaxVariasJugadas_impl/7 no dé problemas con jugadas ganadoras
	heuristicaDerrota(MinHeuristica),
	MinHeuristicaProc is MinHeuristica - 1,
	negar(Maximizar, MaximizarProc),
	valorOptimo(MinHeuristicaProc, MaxHeuristicaProc, MaximizarProc, HeuristicaComp), % Maximizar = true -> HeuristicaComp = MaxHeuristica
	heuristicaOptima_impl(JugadasYHeuristicas, JugadaOptima, HeuristicaOptima, Maximizar, _, HeuristicaComp).
% Si no hay más hijos que recorrer, la heurística optimizada final es la actual
heuristicaOptima_impl([], JugadaOptimaActual, HeuristicaOptimaActual, _, JugadaOptimaActual, HeuristicaOptimaActual).
% Si la heurística actual es una nueva óptima, entonces la nueva óptima actual debería de ser la actual
heuristicaOptima_impl([[JugadaActual, HeuristicaActual]|Cdr], JugadaOptima, HeuristicaOptima, Maximizar, _, HeuristicaOptimaActual) :-
	valorOptimo(HeuristicaActual, HeuristicaOptimaActual, Maximizar, HeuristicaComp),
	HeuristicaOptimaActual \= HeuristicaComp,
	heuristicaOptima_impl(Cdr, JugadaOptima, HeuristicaOptima, Maximizar, JugadaActual, HeuristicaActual).
% Si la heurística actual no es una nueva óptima, entonces la nueva óptima actual debe de seguir como está
heuristicaOptima_impl([[_, HeuristicaActual]|Cdr], JugadaOptima, HeuristicaOptima, Maximizar, JugadaOptimaActual, HeuristicaOptimaActual) :-
	valorOptimo(HeuristicaActual, HeuristicaOptimaActual, Maximizar, HeuristicaComp),
	HeuristicaOptimaActual = HeuristicaComp,
	heuristicaOptima_impl(Cdr, JugadaOptima, HeuristicaOptima, Maximizar, JugadaOptimaActual, HeuristicaOptimaActual).

% Cláusulas interfaz para generar las jugadas inmediatas a partir de una jugada que
% se considera ya hecha (aunque realmente no sea así)
% Si alguien ha ganado, no hay más jugadas posibles
generarJugadasInmediatas(JugadaHecha, [], _) :-
	aplicarJugadaSoloSiNoAplicada(JugadaHecha),
	jugadorGano_cacheado(JugadaHecha, true, VictoriaMia),
	jugadorGano_cacheado(JugadaHecha, false, VictoriaRival),
	(VictoriaMia; VictoriaRival),
	deshacerJugadaSoloSiAplicada(JugadaHecha).
% Si nadie gana, entonces generar jugadas
generarJugadasInmediatas(JugadaHecha, JugadasGeneradas, MisJugadas) :-
	aplicarJugadaSoloSiNoAplicada(JugadaHecha), % Por si la regla anterior falló y no se ha deshecho nada
	jugadorGano_cacheado(JugadaHecha, true, VictoriaMia),
	jugadorGano_cacheado(JugadaHecha, false, VictoriaRival),
	not(VictoriaMia), not(VictoriaRival),
	generarJugadasInmediatas_impl(JugadaHecha, 0, 0, difListas(JugadasGeneradas, []), MisJugadas),
	deshacerJugadaSoloSiAplicada(JugadaHecha).
% Si no hay un siguiente movimiento, o alguien ha ganado, no se pueden generar más jugadas, y por tanto
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

:- dynamic jugadaHecha/1.
% Aplica una jugada, solo si no se ha aplicado todavía, para evitar incongruencias.
% En caso de que ya esté aplicada, devuelve verdadero igualmente
aplicarJugadaSoloSiNoAplicada(Jugada) :-
	not(jugadaHecha(Jugada)),
	aplicarJugada(Jugada),
	asserta(jugadaHecha(Jugada)).
aplicarJugadaSoloSiNoAplicada(Jugada) :- jugadaHecha(Jugada).

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

% Deshace una jugada, solo si ya se ha aplicado, para evitar incongruencias.
% En caso de que no esté aplicada, devuelve verdadero igualmente
deshacerJugadaSoloSiAplicada(Jugada) :-
	jugadaHecha(Jugada),
	deshacerJugada(Jugada),
	retract(jugadaHecha(Jugada)).
deshacerJugadaSoloSiAplicada(Jugada) :- not(jugadaHecha(Jugada)).

% Concatena dos listas expresadas como diferencias de listas.
% Esta operación es de complejidad O(1)
append_dl(difListas(Inicio1, Fin1), difListas(Fin1, Fin2), difListas(Inicio1, Fin2)).

% Concatena dos listas de manera trivial.
% Esta operación es de complejidad O(n), pero funciona en listas cerradas
append_simple([], L, L).
append_simple([Car|Cdr], L, [Car|R]) :- append_simple(Cdr, L, R).

% Obtiene el valor mínimo de dos variables
valorMinimo(A, B, A) :- A < B.
valorMinimo(A, B, B) :- A >= B.

% Obtiene el valor máximo de dos variables
valorMaximo(A, B, B) :- B >= A.
valorMaximo(A, B, A) :- A > B.

% Obtiene el valor óptimo de entre los dados, que puede ser el máximo o el mínimo, dependiendo de un argumento que se pase
valorOptimo(A, B, true, Maximo) :- valorMaximo(A, B, Maximo).
valorOptimo(A, B, false, Minimo) :- valorMinimo(A, B, Minimo).

soyYoAIdentificadorJugador(true, 1).
soyYoAIdentificadorJugador(false, 2).