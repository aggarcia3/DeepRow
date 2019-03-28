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
// Descartar datos inválidos
+tablero(X, Y, Jugador)[source(percept)] <- -tablero(X, Y, Jugador)[source(percept)].

+estrategia(Estrategia)[source(percept)] : Estrategia = jugarAGanar | Estrategia = jugarAPerder <-
	.print(Estrategia).

+turno(Yo)[source(percept)] : .my_name(Yo) <-
	.wait(100); // Por si estamos recibiendo todavía percepciones del tablero
	?soyYoAIdentificadorJugador(true, Id);
	.print(Id).

// Descartar comunicaciones que lleguen de otros agentes, pues solo nos interesa
// lo que diga el entorno
+!kqml_received(Agente, _, _, _) : .my_name(Yo) & Agente \== Yo.
