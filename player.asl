// Agente player en proyecto DeepRow.mas2j

/* Creencias y reglas iniciales */
soyYoAIdentificadorJugador(true, Id) :-
	.my_name(Yo) &
	.delete("player", Yo, IdStr) &
	.term2string(Id, IdStr).
soyYoAIdentificadorJugador(false, 1 + (MiId mod 2)) :- soyYoAIdentificadorJugador(true, MiId).

/* Objetivos iniciales */

/* Planes */

/* Eventos de BC */
+tablero(X, Y, Jugador)[source(percept)] : X >= 0 & X <= 7 & Y >= 0 & Y <= 7 & (Jugador = 0 | Jugador = 1 | Jugador = 2) <-
	.print(X, ", ", Y, ", ", Jugador).
// Los estados del tablero que deliberemos tendrán prioridad sobre el recibido del entorno
+tablero(X, Y, Jugador)[source(self)] <-
	-tablero(X, Y, _)[source(percept)].
+tablero(X, Y, Jugador)[source(Agente)] <- -tablero(X, Y, Jugador)[source(Agente)]. // Descartar información recibida irrelevante

+estrategia(Estrategia)[source(percept)] : Estrategia = jugarAGanar | Estrategia = jugarAPerder <-
	.print(Estrategia).
+estrategia(Estrategia)[source(Agente)] <- -estrategia(Estrategia)[source(Agente)]. // Descartar información recibida irrelevante

+turno(Yo)[source(percept)] : .my_name(Yo) <-
	.wait(100); // Por si estamos recibiendo todavía percepciones del tablero
	.print(Yo);
	?soy(A);
	.print(A).
+turno(Otro)[source(Agente)] <- -turno(Otro)[source(Agente)]. // Descartar información recibida irrelevante
