// Agente player en proyecto Practica2.mas2j

/* Creencias y reglas iniciales */
otrojugador(Jugando,Otrojugador):-.all_names(L)&L=L-Jugando& otrojugador2.
otrojugador2([Otrojugador|Tail],Otrojugador2):- Otrojugador2=Otrojugador.
estalibre(X,Y) :- tablero(X,Y,Jugador) & Jugador = 0.
esmio(X,Y) :- tablero(X,Y,Jugador) & .my_name(Jugando) & Jugador =Jugando .
mepertenece(X,Y,Jugando):- tablero(X,Y,Jugador) & Jugando=Jugador.
asignacionganadora(tablero(X,Y,Jugando),Valorjugada):-ganadora(X,Y,Jugando)&Valorjugada=Valorjugada+100000.
asignaciontier0ther(X,Y,Jugador):-tier0(X,Y,Jugador)&Valorjugada=Valorjugada+1.
asignaciontier0me(X,Y,Jugador):-tier0(X,Y,Jugador)&Valorjugada=Valorjugada+2.
asignaciontier1ther(X,Y,Jugador):-tier1(X,Y,Jugador)&Valorjugada=Valorjugada+3.
asignaciontier1me(X,Y,Jugador):-tier1(X,Y,Jugador)&Valorjugada=Valorjugada+4.
ganadora(X,Y,Jugador) :- ganadora2(X,Y,Jugador,1,0)|ganadora2(X,Y,Jugador,0,1) |ganadora2(X,Y,Jugador,1,1) |ganadora2(X,Y,Jugador,-1,1) |ganadora2(X,Y,Jugador,1,-1)|ganadora2(X,Y,Jugador,-1,0)|ganadora2(X,Y,Jugador,0,-1) |ganadora2(X,Y,Jugador,-1,-1).
ganadora2(X,Y,Jugador,D1,D2):- mepertenece(X + D1, Y +D2,Jugador)& mepertenece(X + D1*2, Y+ D2*2, Jugador)& mepertenece (X+D1*3, Y + D2*3,Jugador).
tier0(X,Y,Jugador):- tier0part2(X,Y,Jugador,0,1) |tier0part2(X,Y,Jugador,1,0) |tier0part2(X,Y,Jugador,1,1) |tier0part2(X,Y,Jugador,-1,1) |tier0part2(X,Y,Jugador,1,-1)|tier0part2(X,Y,Jugador,-1,0)|tier0part2(X,Y,Jugador,0,-1) |tier0part2(X,Y,Jugador,-1,-1).
tier0part2(X,Y,Jugando,D1,D2) :-  mepertenece(X + D1, Y +D2,Jugador).
tier1(X,Y,Jugador):- tier1part2(X,Y,Jugador,0,1) |tier1part2(X,Y,Jugador,1,0) |tier1part2(X,Y,Jugador,1,1) |tier1part2(X,Y,Jugador,-1,1) |tier1part2(X,Y,Jugador,1,-1)|tier1part2(X,Y,Jugador,-1,0)|tier1part2(X,Y,Jugador,0,-1) |tier1part2(X,Y,Jugador,-1,-1).
tier1part2(X,Y,Jugando,D1,D2) :-  mepertenece(X + D1, Y +D2,Jugador) & mepertenece(X + D1*2, Y + D2*2,Jugador).

/* Objetivos iniciales */


/* Planes */

/* Eventos de BC */
+tablero(X, Y, Jugador)[source(percept)] : X >= 0 & X <= 7 & Y >= 0 & Y <= 7 & (Jugador = 0 | Jugador = 1 | Jugador = 2) <-
	.print(X, ", ", Y, ", ", Jugador).
+tablero(X, Y, Jugador)[source(Agente)] <- -tablero(X, Y, Jugador)[source(Agente)]. // Descartar información recibida irrelevante

+estrategia(Estrategia)[source(percept)] : Estrategia = jugarAGanar | Estrategia = jugarAPerder <-
	.wait(95);
	.print(Estrategia).
+estrategia(Estrategia)[source(Agente)] <- -estrategia(Estrategia)[source(Agente)]. // Descartar información recibida irrelevante

+turno(Yo)[source(percept)] : .my_name(Yo) <-
	.wait(100); // Por si estamos recibiendo todavía percepciones del tablero
	.print(Yo).
	
+turno(Otro)[source(Agente)] <- -turno(Otro)[source(Agente)].// Descartar información recibida irrelevante
mainh(L=[movimiento(X,Y,Jugando)|Tail],Valortotal):-
	Valortotal=0&
	aplicarJugada(L)&
	heuristica(movimiento(X,Y,Jugando),Valorjugada)&
	Valortotal=Valortotal+Valorjugada&
	deshacerJugada(L)&
	mainh(Tail,Valortotal).

heuristica(tablero(X,Y,Jugando), Valorjugada):-
	Valorjugada=0&
	asignacionganadora(tablero(X,Y,Jugando,Valorjugada))&
	otrojugador(Jugando,Otrojugador)&
	asignacionganadora(tablero(X,Y,Otrojugador),Valorjugada)&
	tiercalculator(X,Y,Jugando,Valorjugada,Otrojugador).

tiercalculator(X,Y,Jugando,Valorjugada,Otrojugador):-
		asignaciontier0ther(X,Y,Otrojugador)|asignaciontier1ther(X,Y,Otrojugador)
		&asignaciontier0me(X,Y,Jugando)|asignaciontier1me(X,Y,Jugando).
