// Agente player en proyecto Practica2.mas2j

/* Creencias y reglas iniciales */
/*asignacion(X, Y, G, P, C==0) :- asignacion(X, Y, G, P, C==0) & asignacion(X, Y, G, P, C==0) & asignacion(X, Y, G, P, C==0) & asignacion(X, Y, G, P, C==0) & asignacion(X, Y, G, P, C==0) &asignacion(X, Y, G, P, C==0) &asignacion(X, Y, G, P, C==0) &asignacion(X, Y, G, P, C==0).  
asignacion(X, Y, G, P, C==1, Dir) :-  
asignacion(X, Y, G, P, C==2, Dir) :- */
estalibre(X,Y) :- tablero(X,Y,Jugador) & Jugador = 0.
esmio(X,Y) :- tablero(X,Y,Jugador) & Jugador = .myname.
noesmio(X,Y) :- tablero(X,Y,Jugador) & Jugador \= .myname.
mepertenece(X,Y,Jugando):- tablero(X,Y,Jugador) & Jugando=Jugador.
ganadora(X,Y,Jugador) :- ganadora2(X,Y,Jugador,L = [0 | 1]) |ganadora2(X,Y,Jugador,L = [1 | 0]) |ganadora2(X,Y,Jugador,L = [1 | 1]) |ganadora2(X,Y,Jugador,L = [-1 | 1]) |ganadora2(X,Y,Jugador,L = [1 | -1])|ganadora2(X,Y,Jugador,L = [-1 | 0])|ganadora2(X,Y,Jugador,L = [0 | -1]) |ganadora2(X,Y,Jugador,L = [-1 | -1])
ganadora2(X,Y,Jugador,L = [d1 | d2 ]):- mepertenece(X + d1, Y +d2,Jugador)& mepertenece(X + d1*2, Y+ d2*2, Jugador)& mepertenece (X+d1*3, Y + d2*3,Jugador).
tier0(X,Y,Jugador):- tier0part2(X,Y,Jugador,L = [0 | 1]) |tier0part2(X,Y,Jugador,L = [1 | 0]) |tier0part2(X,Y,Jugador,L = [1 | 1]) |tier0part2(X,Y,Jugador,L = [-1 | 1]) |tier0part2(X,Y,Jugador,L = [1 | -1])|tier0part2(X,Y,Jugador,L = [-1 | 0])|tier0part2(X,Y,Jugador,L = [0 | -1]) |tier0part2(X,Y,Jugador,L = [-1 | -1])
tier0part2(X,Y,Jugando,L = [d1 | d2 ]) :-  mepertenece(X + d1, Y +d2,Jugador).
tier1(X,Y,Jugador):- tier1part2(X,Y,Jugador,L = [0 | 1]) |tier1part2(X,Y,Jugador,L = [1 | 0]) |tier1part2(X,Y,Jugador,L = [1 | 1]) |tier1part2(X,Y,Jugador,L = [-1 | 1]) |tier1part2(X,Y,Jugador,L = [1 | -1])|tier1part2(X,Y,Jugador,L = [-1 | 0])|tier1part2(X,Y,Jugador,L = [0 | -1]) |tier1part2(X,Y,Jugador,L = [-1 | -1])
tier1part2(X,Y,Jugando,L = [d1 | d2 ]) :-  mepertenece(X + d1, Y +d2,Jugador) & mepertenece(X + d1*2, Y + d2*2,Jugador).
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
+!mainh(L,Valortotal){
	Valortotal=0.
	aplicarJugada(L).
	for(.member(movimiento(X,Y,Jugando),L){
	!heuristica(movimiento(X,Y,Jugando)Valorjugada);
	Valortotal=Valortotal+Valorjugada.
	}
	deshacerJugada(L).
}
+!heuristica(tablero(X,Y,Jugando), Valorjugada)<-
	Valorjugada=0.
	if(estalibre(X,Y){
		if(ganadora(X,Y,Jugando){
		Valorjugada=Valorjugada+100000;
		}elif(ganadora(X,Y,otrojugador){
		Valorjugada=Valorjugada+100000;
		}else{
		!tiercalculator(X,Y,Jugando,Valorjugada).
		}
	}
+!tiercalculator(X,Y,Jugando,Valorjugada)
		if(tier0(X,Y,otrojugador){
		Valorjugada=Valorjugada+1;
		}elif(tier1(X,Y,otorjugador){
		Valorjugada=Valorjugada+3;
		}
		if(tier0(X,Y,Jugando){
		Valorjugada=Valorjugada+2;
		}elif(tier1(X,Y,Jugando){
		Valorjugada=Valorjugada+4.
		}
