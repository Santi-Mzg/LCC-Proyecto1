:- module(proylcc,
	[
		join/4
	]).


/**
 * join(+Grid, +NumOfColumns, +Path, -RGrids)
 * RGrids es la lista de grillas representando el efecto, en etapas, de combinar las celdas del camino Path
 * en la grilla Grid, con número de columnas NumOfColumns. El número 0 representa que la celda está vacía.
*/

join(Grid, NumOfColumns, Path, RGrids):-
	RGrids = [RGrid1, RGrid2],
	combinePath(Grid, NumOfColumns, Path, RGrid1),
	fall(RGrid1, RGrid2).

% Caso base donde ya se recorrió toda la grilla.
/**
 * combinePath(+Grid, +NumOfColumns, +Path, -RGrid1)
 * RGrid1 es la grilla donde desaparecen (su valor es 0) las celdas del camino Path a excepción de la última 
 * cuyo nuevo valor va es la menor potencia de 2 mayor o igual a la sumatoria de los valores de todas las celdas del camino.
*/
combinePath(Grid, NumOfColumns, Path, RGrid1) :-
	Pos = 0,
	Val = 0,
	remove(Grid, NumOfColumns, Pos, Path, Val, FVal, RGrid),
	nextPowerOf2(FVal, 1, Res),
	changeElement(1, Res, RGrid, RGrid1).

/**
 * remove(+Grid, +NumOfColumns, +Pos, +Path, +Val, -FVal, -RGrid).
 * RGrid es la grilla donde desaparecen (su valor es 0) todas las celdas del camino Path a excepción de la última 
 * cuyo valor se iguala a 1 para marcarlo y diferenciarlo luego a la hora de definir su valor final. Pos es la posición actual en la lista Grid,
 * Val es la sumatoria de los valores de las celdas y FVal es el valor final de la sumatoria.
*/
remove(Grid, _NumOfColumns, _Pos, _Path, Val, FVal, RGrid):-
	Grid = [],
	RGrid = [],
	FVal is Val.

% Caso recursivo en donde para cada elemento de la grilla se hace 0 si forma parte del Path y se mantiene su valor en el caso contrario.
remove(Grid, NumOfColumns, Pos, Path, Val, FVal, RGrid):-
	removeAux(Grid, NumOfColumns, Pos, Path, ValN, RGrid),
	Grid = [_ | Gs],
	RGrid = [_ | Rs],
	PosSig is Pos+1,
	Sum is Val+ValN,
	remove(Gs, NumOfColumns, PosSig, Path, Sum, FVal, Rs).

/**
 * removeAux(+Grid, +NumOfColumns, +Pos, +Path, -Val, -RGrid).
 * Busca uno por uno en Grid si la celda en posición Pos pertenece al camino Path, en cuyo caso guarda su valor en Val y luego la iguala a 0 (celda vacía) en RGrid, a excepción de la última celda del camino, que guarda su valor en Val y la marca con 1 em RGrid.
*/
% Caso base donde la celda en cuestión no forma parte del Path y se mantiene igual.
removeAux(Grid, _NumOfColumns, _Pos, Path, Val, RGrid) :-
	Path = [],
	Val = 0,
	Grid = [G | _],
	RGrid = [G | _].

% Caso base donde la celda en cuestión es el último del Path y dónde debe modificarse su valor luego (se marca con 1), no hacerse 0.
removeAux(Grid, NumOfColumns, Pos, Path, Val, RGrid) :-
	Path = [[F, C]],
	Pos is F*NumOfColumns + C,
	Grid = [Val | _],
	RGrid = [1 | _]. % 1 representa la posición donde termina el Path y debe calcularse su nuevo valor.

% Caso base donde la celda en cuestión forma parte del Path y se cambia a 0.
removeAux(Grid, NumOfColumns, Pos, Path, Val, RGrid) :-
	Path = [[F, C] | Ps], % F=nro Fila y C=nro Columna.
	Ps \= [],
	Pos is F*NumOfColumns + C, % Este cálculo permite determinar si el square del Path es el square en cuestión en la posición Pos de la lista Grid.
	Grid = [Val | _],
	RGrid = [0 | _].

% Caso recursivo donde se recorren las celdas del Path para ver si alguno coincide con la celda en cuestión.
removeAux(Grid, NumOfColumns, Pos, Path, Val, RGrid) :-
	Path = [_ | Ps],
	removeAux(Grid, NumOfColumns, Pos, Ps, Val, RGrid). 
	
/**
 * nextPowerOf2(+FVal, -Res).
 * Res es la potencia P de 2 más chica tal que Res >= Fval.
*/
nextPowerOf2(FVal, P, Res) :-
	Res is 2^P,
	Res >= FVal.

nextPowerOf2(FVal, P, Res) :-
	PS is P+1,
	nextPowerOf2(FVal, PS, Res).

/**
 * changeElement(+E1, +E2, +L, -RL).
 * Cambia la primer aparición de E1 por E2 en la lista L obteniendose RL.
*/ 
changeElement(_, _, [], []).

changeElement(E1, E2, [E1|L], [E2|L]).

changeElement(E1, E2, [X|L], [X|LR]) :- 
	E1\=X, 
	changeElement(E1, E2, L, LR).


/**
*  fall(+Grid, -RGrid2).
*  RGrid2 es la grilla donde por 'gravedad' caen los bloques sobre los espacios vacíos (igualados a 0).
*  En los casos donde no quede un espacio libre en la primera fila se genera entonces una celda nueva con un valor aleatorio de los siguientes: 2, 4, 8, 16, 32 y 64.
*/
fall(Grid, RGrid2) :-
	splitGrid(Grid, Cols),
	Cols = [C | _],
	length(C, NumOfRows),
	fallOnColumns(Cols, NumOfRows, RCols),
	concatColumns(RCols, RGrid2).

/**
 * splitGrid(+Grid, -Columns).
 * Separa la grilla Grid en columnas, en una lista de listas (columnas) Columns.
*/ 
splitGrid(Grid, Columns) :-
	Grid = [],
	Columns = [[], [], [], [], []].

splitGrid(Grid, Columns) :-
	Grid = [G0, G1, G2, G3, G4 | Gs],
	Columns = [[G0 | C0s], [G1 | C1s], [G2 | C2s], [G3 | C3s], [G4 | C4s]],
	splitGrid(Gs, [C0s, C1s, C2s, C3s, C4s]).

/**
 * fallOnColumns(+Cols, +NumOfRows, -RCols).
 * Produce la caida por 'gravedad' en cada una de las columnas de la lista Cols. Generando nuevos elementos aleatorios cuando es necesario.
*/ 
fallOnColumns(Cols, _NumOfRows, RCols) :-
	Cols = [],
	RCols = [].

fallOnColumns(Cols, NumOfRows, RCols) :-
	Cols = [C | Cs],
	RCols = [RC | RCs],
	%findall(X, (member(X, C), X \= 0), Rta), % Se extraen los elementos en 0 y se compactan (caen) el resto de elementos de la columna.
	delete(C, 0, Rta),
	addRandoms(Rta, NumOfRows, RC), % Si es necesario genera nuevos elementos aleatorios al tope de la columna.
	fallOnColumns(Cs, NumOfRows, RCs).

/**
 * addRandoms(+Col, +NumOfRows, -RCol).
 * Ingresa la columna Col y si se tiene huecos libres en el tope (porque se eliminaron elementos y el resto cayó por 'gravedad')
 * entonces genera allí los elementos aleatorios necesarios, obteniendose RCol.
*/  
addRandoms(Col, NumOfRows, RCol) :-
	length(Col, Length),
	RandomsToAdd is NumOfRows - Length,
	Options = [2, 4, 8, 16, 32, 64],
	addRandomsAux(Col, RandomsToAdd, Options, RCol).

/**
 * addRandomsAux(+Col, +RandomsToAdd, +Options, -RCol).
 * Ingresa la columna Col y se agrega la cantidad RandomsToAdd de elementos nuevos aleatorios, elegidos de la lista Options, en el tope obteniendose RCol.
*/ 
addRandomsAux(Col, RandomsToAdd, _Options, RCol) :-
	RandomsToAdd = 0,
	RCol = Col.

addRandomsAux(Col, RandomsToAdd, Options, RCol) :-
	RandomsToAdd > 0,
	RCol = [R | Rs],
	random_member(R, Options),
	RandomsToAddN is RandomsToAdd - 1,
	addRandomsAux(Col, RandomsToAddN, Options, Rs).

/**
 * concatColumns(+Cols, -RGrid).
 * Concatena las columnas de la lista Cols en la grilla RGrid.
*/ 
concatColumns(Cols, RGrid) :-
	Cols = [[], [], [], [], []],
	RGrid = [].

concatColumns(Cols, RGrid) :-
	Cols = [[C0 | C0s], [C1 | C1s], [C2 | C2s], [C3 | C3s], [C4 | C4s]],
	RGrid = [C0, C1, C2, C3, C4 | Rs],
	concatColumns([C0s, C1s, C2s, C3s, C4s], Rs).

/*
	Informe: 
	Como funciona el juego.
	Prolog: explicar todo código prolog, razonamiento y funcionamiento, con ejemplos (imágenes)
	React: que info se pasa del cliente al servidor y como se actualizan los estados.

[64,4,64,32,16,64,8,16,2,32,2,4,64,64,2,2,4,32,16,4,16,4,16,16,16,16,64,2,32,32,64,2,64,32,64,32,2,64,32,4]
*/

boosterColapsarIguales(Grid, NumOfColumns, RGrids) :-
	Pos = 0,
	LMarcadosTemp = [],
	RGrids = [RGrid1, RGrid2],
	identificarGrupos(Grid, Grid, NumOfColumns, Pos, LMarcadosTemp, LMarcadosGlobal, RPaths),
	colapsarGrupos(Grid, NumOfColumns, RPaths, RGrid1),
	fall(RGrid1, RGrid2).

colapsarGrupos(Grid, NumOfColumns, RPaths, RGrid1) :-
	RPaths = [],
	RGrid1 = Grid.	

colapsarGrupos(Grid, NumOfColumns, RPaths, RGrid1) :-
	RPaths = [P | Ps],
	length(P, 1),
	colapsarGrupos(Grid, NumOfColumns, Ps, RGrid1).

colapsarGrupos(Grid, NumOfColumns, RPaths, RGrid1) :-
	RPaths = [P | Ps],
	length(P, Length),
	Length > 1,
	ordenarPath(P, RP),
	combinePath(Grid, NumOfColumns, RP, RGrid),
	colapsarGrupos(RGrid, NumOfColumns, Ps, RGrid1).

ordenarPath(Path, RPath) :-
	max_member(masAbajoYDerecha, RPos, Path),
	delete(Path, RPos, PathAux),
	insertar_final(RPos, PathAux, RPath).
	
masAbajoYDerecha([Fila1, Col1], [Fila2, Col2]) :-
	Val1 is Fila1 + Col1,
	Val2 is Fila2 + Col2,
	Val2 > Val1.

% Da prioridad a la posición de más abajo sobre la de más a la derecha cuando la suma de fila y columna es igual.
masAbajoYDerecha([Fila1, Col1], [Fila2, Col2]) :-
	Val1 is Fila1 + Col1,
	Val2 is Fila2 + Col2,
	Val2 = Val1,
	Fila2 > Fila1.

insertar_final(A, [], [A]).
insertar_final(A, [E|L1], [E|L2]) :-  insertar_final(A, L1, L2).

identificarGrupos(Grid, GridOrig, NumOfColumns, Pos, LMarcadosTemp, LMarcadosGlobal, RPaths) :-   
	Grid = [],
	RPaths = [],
	LMarcadosTemp = LMarcadosGlobal.
	
identificarGrupos(Grid, GridOrig, NumOfColumns, Pos, LMarcadosTemp, LMarcadosGlobal, RPaths) :- 
	Grid = [G | Gs],
	member(Pos, LMarcadosTemp),
	PosSig is Pos + 1,
	identificarGrupos(Gs, GridOrig, NumOfColumns, PosSig, LMarcadosTemp, LMarcadosGlobal, RPaths).

identificarGrupos(Grid, GridOrig, NumOfColumns, Pos, LMarcadosTemp, LMarcadosGlobal, RPaths) :- 
	Grid = [G | Gs],
	RPaths = [P | RPs],
	listaDeAdyacencia(Grid, GridOrig, NumOfColumns, Pos, LMarcadosTemp, LMarcadosGlobal, P),
	PosSig is Pos + 1,
	identificarGrupos(Gs, GridOrig, NumOfColumns, PosSig, LMarcadosGlobal, LMarcadosGlobalN, RPs).

%
listaDeAdyacencia(GridAct, GridOrig, NumOfColumns, Pos, LMarcadosTemp, LMarcadosGlobal, LAdy) :-
	GridAct = [G | Gs],
	LMarcadosTempN = [Pos | LMarcadosTemp],
	Fila is Pos // NumOfColumns,
	Col is Pos - Fila*NumOfColumns,
	LAdy = [[Fila, Col] | LAs],
	PosInicial = 0,
	listaDeAdyacenciaAux(G, GridOrig, GridOrig, NumOfColumns, Fila, Col, PosInicial, LMarcadosTempN, LMarcadosGlobal, LAs).

% No es adyacente y no quedan más adyacentes por recorrer.
listaDeAdyacenciaAux(E, Grid, GridOrig, NumOfColumns, FilaOrig, ColOrig, Pos, LMarcadosTemp, LMarcadosGlobal, LAdy) :-
	Grid = [],
	LAdy = [],
	LMarcadosGlobal = LMarcadosTemp.

% Si es adyacente, igual pero está marcado.
listaDeAdyacenciaAux(E, Grid, GridOrig, NumOfColumns, FilaOrig, ColOrig, Pos, LMarcadosTemp, LMarcadosGlobal, LAdy) :-
	Fila is Pos // NumOfColumns,
	Col is Pos - Fila*NumOfColumns,
	1 >= abs(Fila - FilaOrig),
	1 >= abs(Col - ColOrig),
	Grid = [E | Gs],
	member(Pos, LMarcadosTemp),
	PosSig is Pos + 1,
	listaDeAdyacenciaAux(E, Gs, GridOrig, NumOfColumns, FilaOrig, ColOrig, PosSig, LMarcadosTemp, LMarcadosGlobal, LAdy).

% Si es adyacente, igual y no está marcado.
listaDeAdyacenciaAux(E, Grid, GridOrig, NumOfColumns, FilaOrig, ColOrig, Pos, LMarcadosTemp, LMarcadosGlobal, LAdy) :-
	Fila is Pos // NumOfColumns,
	Col is Pos - Fila*NumOfColumns,
	1 >= abs(Fila - FilaOrig),
	1 >= abs(Col - ColOrig),
	Grid = [E | Gs],
	listaDeAdyacencia(Grid, GridOrig, NumOfColumns, Pos, LMarcadosTemp, LMarcadosGlobal, LAdyAux),
	append(LAdyAux, LAs, LAdy),
	PosSig is Pos + 1,
	listaDeAdyacenciaAux(E, Gs, GridOrig, NumOfColumns, FilaOrig, ColOrig, PosSig, LMarcadosGlobal, LMarcadosGlobalN, LAs).

% Si no es adyacente sigue recorriendo.
listaDeAdyacenciaAux(E, Grid, GridOrig, NumOfColumns, FilaOrig, ColOrig, Pos, LMarcadosTemp, LMarcadosGlobal, LAdy) :-
	Grid = [G | Gs],
	PosSig is Pos + 1,
	listaDeAdyacenciaAux(E, Gs, GridOrig, NumOfColumns, FilaOrig, ColOrig, PosSig, LMarcadosTemp, LMarcadosGlobal, LAdy).
