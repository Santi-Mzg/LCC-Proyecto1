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
	combinarPath(Grid, NumOfColumns, Path, RGrid1),
	caida(RGrid1, NumOfColumns, RGrid2).

% Caso base donde ya se recorrió toda la grilla.
/**
 * combinarPath(+Grid, +NumOfColumns, +Path, -RGrid1)
 * RGrid1 es la grilla donde desaparecen (su valor es 0) las celdas del camino Path a excepción de la última 
 * cuyo nuevo valor va es la menor potencia de 2 mayor o igual a la sumatoria de los valores de todas las celdas del camino.
*/
combinarPath(Grid, NumOfColumns, Path, RGrid1) :-
	Pos = 0,
	Val = 0,
	remover(Grid, NumOfColumns, Pos, Path, Val, FVal, RGrid),
	siguientePotenciaDe2(FVal, 1, Res),
	cambiarElemento(1, Res, RGrid, RGrid1).

/**
 * remover(+Grid, +NumOfColumns, +Pos, +Path, +Val, -FVal, -RGrid).
 * RGrid es la grilla donde desaparecen (su valor es 0) todas las celdas del camino Path a excepción de la última 
 * cuyo valor se iguala a 1 para marcarlo y diferenciarlo luego a la hora de definir su valor final. Pos es la posición actual en la lista Grid,
 * Val es la sumatoria de los valores de las celdas y FVal es el valor final de la sumatoria.
*/
remover(Grid, _NumOfColumns, _Pos, _Path, Val, FVal, RGrid):-
	Grid = [],
	RGrid = [],
	FVal is Val.

% Caso recursivo en donde para cada elemento de la grilla se hace 0 si forma parte del Path y se mantiene su valor en el caso contrario.
remover(Grid, NumOfColumns, Pos, Path, Val, FVal, RGrid):-
	removerAux(Grid, NumOfColumns, Pos, Path, ValN, RGrid),
	Grid = [_ | Gs],
	RGrid = [_ | Rs],
	PosSig is Pos+1,
	Sum is Val+ValN,
	remover(Gs, NumOfColumns, PosSig, Path, Sum, FVal, Rs).

/**
 * removerAux(+Grid, +NumOfColumns, +Pos, +Path, -Val, -RGrid).
 * Busca uno por uno en Grid si la celda en posición Pos pertenece al camino Path, en cuyo caso guarda su valor en Val y luego la iguala a 0 (celda vacía) en RGrid, a excepción de la última celda del camino, que guarda su valor en Val y la marca con 1 en RGrid.
*/
% Caso base donde la celda en cuestión no forma parte del Path y se mantiene igual.
removerAux(Grid, _NumOfColumns, _Pos, Path, Val, RGrid) :-
	Path = [],
	Val = 0,
	Grid = [G | _],
	RGrid = [G | _].

% Caso base donde la celda en cuestión es el último del Path y dónde debe modificarse su valor luego (se marca con 1), no hacerse 0.
removerAux(Grid, NumOfColumns, Pos, Path, Val, RGrid) :-
	Path = [[F, C]],
	Pos is F*NumOfColumns + C,
	Grid = [Val | _],
	RGrid = [1 | _]. % 1 representa la posición donde termina el Path y debe calcularse su nuevo valor.

% Caso base donde la celda en cuestión forma parte del Path y se cambia a 0.
removerAux(Grid, NumOfColumns, Pos, Path, Val, RGrid) :-
	Path = [[F, C] | Ps], % F=nro Fila y C=nro Columna.
	Ps \= [],
	Pos is F*NumOfColumns + C, % Este cálculo permite determinar si el square del Path es el square en cuestión en la posición Pos de la lista Grid.
	Grid = [Val | _],
	RGrid = [0 | _].

% Caso recursivo donde se recorren las celdas del Path para ver si alguno coincide con la celda en cuestión.
removerAux(Grid, NumOfColumns, Pos, Path, Val, RGrid) :-
	Path = [_ | Ps],
	removerAux(Grid, NumOfColumns, Pos, Ps, Val, RGrid). 
	
/**
 * siguientePotenciaDe2(+FVal, -Res).
 * Res es la potencia P de 2 más chica tal que Res >= Fval.
*/
siguientePotenciaDe2(FVal, P, Res) :-
	Res is 2^P,
	Res >= FVal,
	!.

siguientePotenciaDe2(FVal, P, Res) :-
	PS is P+1,
	siguientePotenciaDe2(FVal, PS, Res).

/**
 * cambiarElemento(+E1, +E2, +L, -RL).
 * Cambia la primera aparición de E1 por E2 en la lista L obteniendose RL.
*/ 
cambiarElemento(_, _, [], []).

cambiarElemento(E1, E2, [E1|L], [E2|L]).

cambiarElemento(E1, E2, [X|L], [X|LR]) :- 
	E1\=X, 
	cambiarElemento(E1, E2, L, LR).


/**
*  caida(+Grid, -RGrid2).
*  RGrid2 es la grilla donde por 'gravedad' caen los bloques sobre los espacios vacíos (igualados a 0).
*  En los casos donde quede un espacio libre en la primera fila se genera entonces un bloque nuevo con un valor aleatorio de los siguientes: 2, 4, 8, 16, 32 y 64.
*/
caida(Grid, NumOfColumns, RGrid2) :-
	separarEnColumnas(Grid, NumOfColumns, 0, Cols) ,
	Cols = [C | _],
	length(C, NumOfRows),
	caidaEnColumnas(Cols, NumOfRows, RCols),
	concatenarColumnas(RCols, NumOfColumns, NumOfRows, 0, RGrid2).

/**
 * separarEnColumnas(+Grid, -Columns).
 * Separa la grilla Grid en columnas, en una lista de listas (columnas) Columns.
*/ 
separarEnColumnas(_Grid, NumOfColumns, NCol, Columns) :-
	NCol is NumOfColumns,
	Columns = [].

separarEnColumnas(Grid, NumOfColumns, NCol, Columns) :-
	Columns = [C | Cs],
	Pos = 0,
	separarEnColumnasAux(Grid, NumOfColumns, NCol, Pos, C),
	NColSig is NCol + 1,
	separarEnColumnas(Grid, NumOfColumns, NColSig, Cs).

separarEnColumnasAux(Grid,_NumOfColumns, _NCol, _Pos, Col) :-
	Grid = [],
	Col = [].

separarEnColumnasAux(Grid, NumOfColumns, NCol, Pos, Col) :-
	Grid = [G | Gs],
	NCol is Pos mod NumOfColumns,
	Col = [G | Cs],
	PosSig is Pos + 1,
	separarEnColumnasAux(Gs, NumOfColumns, NCol, PosSig, Cs).

separarEnColumnasAux(Grid, NumOfColumns, NCol, Pos, Col) :-
	Grid = [_ | Gs],
	PosSig is Pos + 1,
	separarEnColumnasAux(Gs, NumOfColumns, NCol, PosSig, Col).

/**
 * caidaEnColumnas(+Cols, +NumOfRows, -RCols).
 * Produce la caida por 'gravedad' en cada una de las columnas de la lista Cols. Generando nuevos elementos aleatorios en cada columna cuando es necesario.
*/ 
caidaEnColumnas(Cols, _NumOfRows, RCols) :-
	Cols = [],
	RCols = [].

caidaEnColumnas(Cols, NumOfRows, RCols) :-
	Cols = [C | Cs],
	RCols = [RC | RCs],
	delete(C, 0, Rta),
	agregarRandoms(Rta, NumOfRows, RC), % Si es necesario genera nuevos elementos aleatorios al tope de la columna.
	caidaEnColumnas(Cs, NumOfRows, RCs).

/**
 * agregarRandoms(+Col, +NumOfRows, -RCol).
 * Ingresa la columna Col y si se tiene huecos libres en el tope (porque se eliminaron elementos y el resto se apiló)
 * entonces genera allí los elementos aleatorios necesarios para mantener la longitud NumOfRows, obteniendose RCol.
*/  
agregarRandoms(Col, NumOfRows, RCol) :-
	length(Col, Length),
	RandomsToAdd is NumOfRows - Length,
	Options = [2, 4, 8, 16, 32, 64],
	agregarRandomsAux(Col, RandomsToAdd, Options, RCol).

/**
 * agregarRandomsAux(+Col, +RandomsToAdd, +Options, -RCol).
 * Ingresa la columna Col y se agrega la cantidad RandomsToAdd de elementos nuevos aleatorios, elegidos de la lista Options, en el tope obteniendose RCol.
*/ 
agregarRandomsAux(Col, RandomsToAdd, _Options, RCol) :-
	RandomsToAdd = 0,
	RCol = Col.

agregarRandomsAux(Col, RandomsToAdd, Options, RCol) :-
	RandomsToAdd > 0,
	RCol = [R | Rs],
	random_member(R, Options),
	RandomsToAddN is RandomsToAdd - 1,
	agregarRandomsAux(Col, RandomsToAddN, Options, Rs).

/**
 * concatenarColumnas(+Cols, -RGrid).
 * Concatena las columnas de la lista Cols en la grilla RGrid.
*/ 
concatenarColumnas(_Cols, NumOfColumns, NumOfRows, NCol, RGrid) :-
	NCol is NumOfColumns,
	T is NumOfColumns*NumOfRows,
	length(RGrid, T).

concatenarColumnas(Cols, NumOfColumns, NumOfRows, NCol, RGrid) :-
	Cols = [C | Cs],
	Pos = 0,
	concatenarColumnasAux(C, NumOfColumns, NCol, Pos, RGrid),
	NColSig is NCol + 1,
	concatenarColumnas(Cs, NumOfColumns, NumOfRows, NColSig, RGrid).

concatenarColumnasAux(Col, _NumOfColumns, _NCol, _Pos, _RGrid) :-
	Col = [].
	
concatenarColumnasAux(Col, NumOfColumns, NCol, Pos, RGrid) :-
	Col = [C | Cs],
	NCol is Pos mod NumOfColumns,
	RGrid = [C | RGs],
	PosSig is Pos + 1,
	concatenarColumnasAux(Cs, NumOfColumns, NCol, PosSig, RGs).

concatenarColumnasAux(Col, NumOfColumns, NCol, Pos, RGrid) :-
	RGrid = [_ | RGs],
	PosSig is Pos + 1,
	concatenarColumnasAux(Col, NumOfColumns, NCol, PosSig, RGs).

/**
 * boosterColapsarIguales(+Grid, +NumOfColumns, -RGrids)
 * RGrids es la lista de grillas representando el efecto, en etapas, de combinar las celdas de todos los grupos de bloques adyacentes de igual valor
 * de la grilla Grid para luego caer por 'gravedad' el resto de bloques de la grilla y generandose nuevos bloques aleatorios. 
 * NumOfColumns es el número de columnas de la grilla Grid.
 *  
*/ 
boosterColapsarIguales(Grid, NumOfColumns, RGrids) :-
	RGrids = [RGrid1, RGrid2],
	identificarGrupos(Grid, NumOfColumns, RPaths),
	colapsarGrupos(Grid, NumOfColumns, RPaths, RGrid1),
	caida(RGrid1, NumOfColumns, RGrid2).

/**
 * identificarGrupos(+Grid, +NumOfColumns, -RPaths)
 * RPaths es la lista de todos los grupos de bloques adyacentes de igual valor en la grilla Grid con cantidad de columnas NumOfColumns.
*/ 
identificarGrupos(Grid, NumOfColumns, RPaths) :-
	Pos = 0,
	LMarcadosTemp = [],
	identificarGruposAux(Grid, Grid, NumOfColumns, Pos, LMarcadosTemp, _, RPaths).

% Se recorrieton todos los bloques de la grilla.
identificarGruposAux(Grid, _GridOrig, _NumOfColumns, _Pos, LMarcadosTemp, LMarcadosGlobal, RPaths) :-   
	Grid = [],
	RPaths = [],
	LMarcadosGlobal = LMarcadosTemp.

% El bloque en cuestión ya fue visitado y no es necesario buscarle grupo.
identificarGruposAux(Grid, GridOrig, NumOfColumns, Pos, LMarcadosTemp, LMarcadosGlobal, RPaths) :- 
	Grid = [_ | Gs],
	member(Pos, LMarcadosTemp),
	PosSig is Pos + 1,
	identificarGruposAux(Gs, GridOrig, NumOfColumns, PosSig, LMarcadosTemp, LMarcadosGlobal, RPaths).

% Se le busca grupo al bloque en cuestión.
identificarGruposAux(Grid, GridOrig, NumOfColumns, Pos, LMarcadosTemp, LMarcadosGlobal, RPaths) :- 
	Grid = [_ | Gs],
	RPaths = [P | RPs],
	buscarAdyacentes(Grid, GridOrig, NumOfColumns, Pos, LMarcadosTemp, LMarcadosGlobal, P),
	PosSig is Pos + 1,
	identificarGruposAux(Gs, GridOrig, NumOfColumns, PosSig, LMarcadosGlobal, _, RPs).

/**
 * buscarAdyacentes(+Grid, +GridOrig, +NumOfColumns, +Pos, +LMarcadosTemp, -LMarcadosGlobal, -LAdy)
 * Busca el grupo de bloques adyacentes e iguales LAdy partiendo de la posición Pos en la grilla GridOriginal correspondiente a la primera posición de la grilla Grid de número de columnas NumOfColums.
 * LMarcadosTemp es la lista de bloques visitados que se utiliza para chequear y LMarcadosGlobal es la lista de bloques visitados que se obtiene al finalizar.
*/ 
buscarAdyacentes(Grid, GridOrig, NumOfColumns, Pos, LMarcadosTemp, LMarcadosGlobal, LAdy) :-
	Grid = [G | _],
	LMarcadosTempN = [Pos | LMarcadosTemp],
	Fila is Pos // NumOfColumns,
	Col is Pos - Fila*NumOfColumns,
	LAdy = [[Fila, Col] | LAs],
	PosInicial = 0,
	buscarAdyacentesAux(G, GridOrig, GridOrig, NumOfColumns, Fila, Col, PosInicial, LMarcadosTempN, LMarcadosGlobal, LAs).

% No es adyacente y no quedan más adyacentes por recorrer.
buscarAdyacentesAux(_E, Grid, _GridOrig, NumOfColumns, FilaOrig, ColOrig, Pos, LMarcadosTemp, LMarcadosGlobal, LAdy) :-
	PosOrig is FilaOrig*NumOfColumns + ColOrig,
	(Pos > PosOrig + NumOfColumns + 1; Grid=[]), % Corta primero si se va del grupo de adyacentes o si se acaba la grilla.
	LAdy = [],
	LMarcadosGlobal = LMarcadosTemp.

% Si es adyacente, igual y no está marcado entonces busca sus adyacentes propios.
buscarAdyacentesAux(E, Grid, GridOrig, NumOfColumns, FilaOrig, ColOrig, Pos, LMarcadosTemp, LMarcadosGlobal, LAdy) :-
	Fila is Pos // NumOfColumns,
	Col is Pos - Fila*NumOfColumns,
	1 >= abs(Fila - FilaOrig),
	1 >= abs(Col - ColOrig),
	Grid = [E | Gs],
	\+ member(Pos, LMarcadosTemp), % Si no pertenece a la lista de marcados.
	buscarAdyacentes(Grid, GridOrig, NumOfColumns, Pos, LMarcadosTemp, LMarcadosGlobal, LAdyAux),
	append(LAdyAux, LAs, LAdy),
	PosSig is Pos + 1,
	buscarAdyacentesAux(E, Gs, GridOrig, NumOfColumns, FilaOrig, ColOrig, PosSig, LMarcadosGlobal, _, LAs).

% Sino sigue recorriendo.
buscarAdyacentesAux(E, Grid, GridOrig, NumOfColumns, FilaOrig, ColOrig, Pos, LMarcadosTemp, LMarcadosGlobal, LAdy) :-
	Grid = [_ | Gs],
	PosSig is Pos + 1,
	buscarAdyacentesAux(E, Gs, GridOrig, NumOfColumns, FilaOrig, ColOrig, PosSig, LMarcadosTemp, LMarcadosGlobal, LAdy).


/**
 * colapsarGrupos(+Grid, +NumOfColumns, +RPaths, -RGrid1)
 * RPaths es la lista de todos los grupos adyacentes e iguales que se colapsan y se suman en el último bloque de dicho grupo (el de más abajo a la derecha)
 * en la grilla Grid con número de columnas NumOfColumns obteniendose RGrid1.
*/ 
% Caso base donde se terminaron de colapsar todos lo grupos.
colapsarGrupos(Grid, _NumOfColumns, RPaths, RGrid1) :-
	RPaths = [],
	RGrid1 = Grid.	

% Caso base donde el grupo en cuestión tiene un solo bloque y no colapsa.
colapsarGrupos(Grid, NumOfColumns, RPaths, RGrid1) :-
	RPaths = [P | Ps],
	length(P, 1),
	colapsarGrupos(Grid, NumOfColumns, Ps, RGrid1).

% Caso recursivo donde colapsa cada grupo de longitud mayor a 1.
colapsarGrupos(Grid, NumOfColumns, RPaths, RGrid1) :-
	RPaths = [P | Ps],
	length(P, Length),
	Length > 1,
	ordenarPath(P, RP),
	combinarPath(Grid, NumOfColumns, RP, RGrid),
	colapsarGrupos(RGrid, NumOfColumns, Ps, RGrid1).

/**
 * ordenarPath(+Path, -RPath)
 * Ordena el grupo Path, que es una lista, colocando la posición más abajo y a la derecha del grupo al final de la lista. 
*/ 
ordenarPath(Path, RPath) :-
	max_member(masAbajoYDerecha, RPos, Path),
	delete(Path, RPos, PathAux),
	insertarFinal(RPos, PathAux, RPath).
	
/**
 * masAbajoYDerecha(+Pos1, +Pos2)
 * Es verdadero si la posicion Pos1 se halla más abajo y a la derecha en la grilla que la posición Pos2,
 * ambas expresadas en función de su fila y columna. 
 * La posición más abajo y a la derecha es aquella en la cual la suma de su número de fila y número de columna es mayor.
*/ 
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

/**
 * insertarFinal(+A, +L1, -L2)
 * Inserta el elemento A al final de la lista L1, resultando L2.
*/ 
insertarFinal(A, [], [A]).
insertarFinal(A, [E|L1], [E|L2]) :-  insertarFinal(A, L1, L2).


%[64,4,64,32,16, 64,8,16,2,32, 2,4,64,64,2, 2,4,32,16,4, 16,4,16,16,16 ,16,64,2,32,32, 64,2,64,32,64, 32,2,64,32,4]

/**
 * movidaMaxima(+Grid, +NumOfColumns, -MovidaMaxima)
 * 
*/ 
movidaMaxima(Grid, NumOfColumns, MovidaMaxima) :-
	identificarMovidasMaximas(Grid, Grid, NumOfColumns, 0, [], 0, MovidaMaximaInv, ValMejorMovida),
	reverse(MovidaMaximaInv, MovidaMaxima).

% Se recorrieton todos los bloques de la grilla.
identificarMovidasMaximas(Grid, _GridOrig, _NumOfColumns, _Pos, MejorMovidaActual, ValMejorMovidaActual, MejorMovida, ValMejorMovida) :-   
	Grid = [],
	MejorMovida = MejorMovidaActual,
	ValMejorMovida = ValMejorMovidaActual.

% Se le busca grupo al bloque en cuestión.
identificarMovidasMaximas(Grid, GridOrig, NumOfColumns, Pos, MejorMovidaActual, ValMejorMovidaActual, MejorMovida, ValMejorMovida) :- 
	Grid = [_ | Gs],
	LMarcadosTemp = [],
	MovidaActual = [],
	ValMovidaActual = 0,
	buscarMovidasEnPos(Grid, GridOrig, NumOfColumns, Pos, LMarcadosTemp, MovidaActual, ValMovidaActual, MejorMovidaActual, ValMejorMovidaActual, MejorMovidaTemp, ValMejorMovidaTemp), 
	PosSig is Pos + 1,
	identificarMovidasMaximas(Gs, GridOrig, NumOfColumns, PosSig, MejorMovidaTemp, ValMejorMovidaTemp, MejorMovida, ValMejorMovida).

/**
 * buscarMovidasEnPos(+Grids, +GridOrig, +NumOfColumns, +Pos, +LMarcadosTemp, -LMarcadosGlobal, -LAdy)
 * Busca el grupo de bloques adyacentes e iguales LAdy partiendo de la posición Pos en la grilla GridOriginal correspondiente a la primera posición de la grilla Grid de número de columnas NumOfColums.
 * LMarcadosTemp es la lista de bloques visitados que se utiliza para chequear y LMarcadosGlobal es la lista de bloques visitados que se obtiene al finalizar.
*/ 
buscarMovidasEnPos(Grid, GridOrig, NumOfColumns, Pos, LMarcadosTemp, MovidaActual, ValMovidaActual, MejorMovidaActual, ValMejorMovidaActual, MejorMovida, ValMejorMovida) :-
	Grid = [G | _],
	Fila is Pos // NumOfColumns,
	Col is Pos - Fila*NumOfColumns,
	MovidaActualN = [[Fila, Col] | MovidaActual],
	ValMovidaActualN is ValMovidaActual + G, 
	LMarcadosTempN = [Pos | LMarcadosTemp],
	PosInicial = 0,
	buscarMovidasEnPosAux(G, GridOrig, GridOrig, NumOfColumns, Fila, Col, PosInicial, LMarcadosTempN, MovidaActualN, ValMovidaActualN, MejorMovidaActual, ValMejorMovidaActual, MejorMovida, ValMejorMovida). 

% Se terminan de definir las listas de movidas y valores, y se le asigna valor 0 a los caminos de 1 bloque de largo para evitar su selección.
buscarMovidasEnPosAux(_E, Grid, _GridOrig, NumOfColumns, FilaOrig, ColOrig, Pos, _LMarcadosTemp, MovidaActual, ValMovidaActual, MejorMovidaActual, ValMejorMovidaActual, MejorMovida, ValMejorMovida) :-
	PosOrig is FilaOrig*NumOfColumns + ColOrig,
	(Pos > PosOrig + NumOfColumns + 1; Grid=[]), % Corta primero si se va del grupo de adyacentes o si se acaba la grilla.
	(length(MovidaActual, L), L < 2 ; 
	siguientePotenciaDe2(ValMovidaActual, 2, ValMovidaActualP),
	ValMovidaActualP =< ValMejorMovidaActual),
	!,
	ValMejorMovida = ValMejorMovidaActual,
	MejorMovida = MejorMovidaActual.

% Se agrega el movimiento a la lista de movidas y también el valor de esa movida a la lista de valores.
buscarMovidasEnPosAux(_E, Grid, _GridOrig, NumOfColumns, FilaOrig, ColOrig, Pos, _LMarcadosTemp, MovidaActual, ValMovidaActual, _MejorMovidaActual, ValMejorMovidaActual, MejorMovida, ValMejorMovida) :-
	PosOrig is FilaOrig*NumOfColumns + ColOrig,
	(Pos > PosOrig + NumOfColumns + 1; Grid=[]), % Corta primero si se va del grupo de adyacentes o si se acaba la grilla.
	siguientePotenciaDe2(ValMovidaActual, 2, ValMovidaActualP),
	ValMovidaActualP > ValMejorMovidaActual,
	!,
	ValMejorMovida = ValMovidaActualP,
	MejorMovida = MovidaActual.

% Si es adyacente, igual y no está marcado entonces busca sus adyacentes propios.
buscarMovidasEnPosAux(E, Grid, GridOrig, NumOfColumns, FilaOrig, ColOrig, Pos, LMarcadosTemp, MovidaActual, ValMovidaActual, MejorMovidaActual, ValMejorMovidaActual, MejorMovida, ValMejorMovida) :-
	Fila is Pos // NumOfColumns,
	Col is Pos - Fila*NumOfColumns,
	1 >= abs(Fila - FilaOrig),
	1 >= abs(Col - ColOrig),
	\+ member(Pos, LMarcadosTemp), % Si no pertenece a la lista de marcados.
	E2 is 2*E, % Siguiente potencia de 2.
	length(LMarcadosTemp, LengthPath),
	(Grid = [E | Gs]; (LengthPath > 1, Grid = [E2 | Gs])), % Si el adyacente es igual o, es la siguiente potencia de dos y además no es el segundo bloque del camino.
	!,
	buscarMovidasEnPos(Grid, GridOrig, NumOfColumns, Pos, LMarcadosTemp, MovidaActual, ValMovidaActual, MejorMovidaActual, ValMejorMovidaActual, MejorMovidaTemp, ValMejorMovidaTemp),
	PosSig is Pos + 1,
	buscarMovidasEnPosAux(E, Gs, GridOrig, NumOfColumns, FilaOrig, ColOrig, PosSig, LMarcadosTemp, MovidaActual, ValMovidaActual, MejorMovidaTemp, ValMejorMovidaTemp, MejorMovida, ValMejorMovida).

% Sino sigue recorriendo.
buscarMovidasEnPosAux(E, Grid, GridOrig, NumOfColumns, FilaOrig, ColOrig, Pos, LMarcadosTemp, MovidaActual, ValMovidaActual, MejorMovidaActual, ValMejorMovidaActual, MejorMovida, ValMejorMovida) :-
	Grid = [_ | Gs],
	PosSig is Pos + 1,
	buscarMovidasEnPosAux(E, Gs, GridOrig, NumOfColumns, FilaOrig, ColOrig, PosSig, LMarcadosTemp, MovidaActual, ValMovidaActual, MejorMovidaActual, ValMejorMovidaActual, MejorMovida, ValMejorMovida).

/**
 * maximosIgualesAdyacentes()
 * 
 * 
*/ 

maximosIgualesAdyacentes(Grid, NumOfColumns, MejorMovida) :-
	identificarMovidasMaximasAdyacente(Grid, Grid, NumOfColumns, 0, [], 0, MejorMovidaInv, ValMejorMovida),
	%seleccionarMovidaMaxima(RPaths, RVals, MovidaMaximaInv),
	reverse(MejorMovidaInv, MejorMovida).

% Se recorrieton todos los bloques de la grilla.
identificarMovidasMaximasAdyacente(Grid, _GridOrig, _NumOfColumns, _Pos, MejorMovidaActual, ValMejorMovidaActual, MejorMovida, ValMejorMovida) :-   
	Grid = [],
	MejorMovida = MejorMovidaActual,
	ValMejorMovida = ValMejorMovidaActual.

% Se le busca grupo al bloque en cuestión.
identificarMovidasMaximasAdyacente(Grid, GridOrig, NumOfColumns, Pos, MejorMovidaActual, ValMejorMovidaActual, MejorMovida, ValMejorMovida) :- 
	Grid = [_ | Gs],
	LMarcadosTemp = [],
	MovidaActual = [],
	ValMovidaActual = 0,
	buscarMovidasEnPosAdyacente(Grid, GridOrig, NumOfColumns, Pos, LMarcadosTemp, MovidaActual, ValMovidaActual, MejorMovidaActual, ValMejorMovidaActual, MejorMovidaTemp, ValMejorMovidaTemp), 
	PosSig is Pos + 1,
	identificarMovidasMaximasAdyacente(Gs, GridOrig, NumOfColumns, PosSig, MejorMovidaTemp, ValMejorMovidaTemp, MejorMovida, ValMejorMovida).

/**
 * buscarMovidasEnPos(+Grids, +GridOrig, +NumOfColumns, +Pos, +LMarcadosTemp, -LMarcadosGlobal, -LAdy)
 * Busca el grupo de bloques adyacentes e iguales LAdy partiendo de la posición Pos en la grilla GridOriginal correspondiente a la primera posición de la grilla Grid de número de columnas NumOfColums.
 * LMarcadosTemp es la lista de bloques visitados que se utiliza para chequear y LMarcadosGlobal es la lista de bloques visitados que se obtiene al finalizar.
*/ 
buscarMovidasEnPosAdyacente(Grid, GridOrig, NumOfColumns, Pos, LMarcadosTemp, MovidaActual, ValMovidaActual, MejorMovidaActual, ValMejorMovidaActual, MejorMovida, ValMejorMovida) :-
	Grid = [G | _],
	Fila is Pos // NumOfColumns,
	Col is Pos - Fila*NumOfColumns,
	MovidaActualN = [[Fila, Col] | MovidaActual],
	ValMovidaActualN is ValMovidaActual + G, 
	LMarcadosTempN = [Pos | LMarcadosTemp],
	PosInicial = 0,
	buscarMovidasEnPosAdyacenteAux(G, GridOrig, GridOrig, NumOfColumns, Fila, Col, PosInicial, LMarcadosTempN, MovidaActualN, ValMovidaActualN, MejorMovidaActual, ValMejorMovidaActual, MejorMovida, ValMejorMovida). 

% Se terminan de definir las listas de movidas y valores, y se le asigna valor 0 a los caminos de 1 bloque de largo para evitar su selección.
buscarMovidasEnPosAdyacenteAux(_E, Grid, _GridOrig, NumOfColumns, FilaOrig, ColOrig, Pos, _LMarcadosTemp, MovidaActual, ValMovidaActual, MejorMovidaActual, ValMejorMovidaActual, MejorMovida, ValMejorMovida) :-
	PosOrig is FilaOrig*NumOfColumns + ColOrig,
	(Pos > PosOrig + NumOfColumns + 1; Grid=[]), % Corta primero si se va del grupo de adyacentes o si se acaba la grilla.
	(length(MovidaActual, L), L < 2 ; 
	siguientePotenciaDe2(ValMovidaActual, 2, ValMovidaActualP),
	ValMovidaActualP =< ValMejorMovidaActual),
	!,
	ValMejorMovida = ValMejorMovidaActual,
	MejorMovida = MejorMovidaActual.

% Se agrega el movimiento a la lista de movidas y también el valor de esa movida a la lista de valores.
buscarMovidasEnPosAdyacenteAux(_E, Grid, GridOrig, NumOfColumns, FilaOrig, ColOrig, Pos, LMarcadosTemp, MovidaActual, ValMovidaActual, _MejorMovidaActual, ValMejorMovidaActual, MejorMovida, ValMejorMovida) :-
	PosOrig is FilaOrig*NumOfColumns + ColOrig,
	(Pos > PosOrig + NumOfColumns + 1; Grid=[]), % Corta primero si se va del grupo de adyacentes o si se acaba la grilla.
	siguientePotenciaDe2(ValMovidaActual, 2, ValMovidaActualP),
	ValMovidaActualP > ValMejorMovidaActual,
	!,
	buscarAdyacenteIgual(GridOrig, GridOrig, FilaOrig, ColOrig, 0, NumOfColumns, LMarcadosTemp, ValMovidaActualP),
	ValMejorMovida = ValMovidaActualP,
	MejorMovida = MovidaActual.

% Si es adyacente, igual y no está marcado entonces busca sus adyacentes propios.
buscarMovidasEnPosAdyacenteAux(E, Grid, GridOrig, NumOfColumns, FilaOrig, ColOrig, Pos, LMarcadosTemp, MovidaActual, ValMovidaActual, MejorMovidaActual, ValMejorMovidaActual, MejorMovida, ValMejorMovida) :-
	Fila is Pos // NumOfColumns,
	Col is Pos - Fila*NumOfColumns,
	1 >= abs(Fila - FilaOrig),
	1 >= abs(Col - ColOrig),
	\+ member(Pos, LMarcadosTemp), % Si no pertenece a la lista de marcados.
	E2 is 2*E, % Siguiente potencia de 2.
	length(LMarcadosTemp, LengthPath),
	(Grid = [E | Gs]; (LengthPath > 1, Grid = [E2 | Gs])), % Si el adyacente es igual o, es la siguiente potencia de dos y además no es el segundo bloque del camino.
	buscarMovidasEnPosAdyacente(Grid, GridOrig, NumOfColumns, Pos, LMarcadosTemp, MovidaActual, ValMovidaActual, MejorMovidaActual, ValMejorMovidaActual, MejorMovidaTemp, ValMejorMovidaTemp),
	PosSig is Pos + 1,
	buscarMovidasEnPosAdyacenteAux(E, Gs, GridOrig, NumOfColumns, FilaOrig, ColOrig, PosSig, LMarcadosTemp, MovidaActual, ValMovidaActual, MejorMovidaTemp, ValMejorMovidaTemp, MejorMovida, ValMejorMovida).

% Sino sigue recorriendo.
buscarMovidasEnPosAdyacenteAux(E, Grid, GridOrig, NumOfColumns, FilaOrig, ColOrig, Pos, LMarcadosTemp, MovidaActual, ValMovidaActual, MejorMovidaActual, ValMejorMovidaActual, MejorMovida, ValMejorMovida) :-
	Grid = [_ | Gs],
	PosSig is Pos + 1,
	buscarMovidasEnPosAdyacenteAux(E, Gs, GridOrig, NumOfColumns, FilaOrig, ColOrig, PosSig, LMarcadosTemp, MovidaActual, ValMovidaActual, MejorMovidaActual, ValMejorMovidaActual, MejorMovida, ValMejorMovida).


% Si el bloque de abajo pertenece a la movida, se vuelve a calcular los adyacentes para una posición debajo (ya que el bloque resultado se caería un bloque).
buscarAdyacenteIgual(Grid, GridOrig, FilaOrig, ColOrig, Pos, NumOfColumns, LMarcadosTemp, ValMovidaActual) :-
	PosOrig is FilaOrig*NumOfColumns + ColOrig,
	PosAbajo is PosOrig + NumOfColumns,
	member(PosAbajo, LMarcadosTemp), % Si la posición por debajo del bloque final de la movida pertenece a la movida.
	!,
	length(Grid, L),
	NumOfRows is L // NumOfColumns,
	FilaOrig < NumOfRows - 1,
	FilaOrigN is FilaOrig + 1,
	buscarAdyacenteIgual(GridOrig, GridOrig, FilaOrigN, ColOrig, 0, NumOfColumns, LMarcadosTemp, ValMovidaActual).


buscarAdyacenteIgual(Grid, GridOrig, FilaOrig, ColOrig, Pos, NumOfColumns, LMarcadosTemp, ValMovidaActual) :-
	Fila is Pos // NumOfColumns,
	Col is Pos - Fila*NumOfColumns,
	1 >= abs(Fila - FilaOrig),
	1 >= abs(Col - ColOrig),
	Grid = [ValMovidaActual | Gs],
	!.

buscarAdyacenteIgual(Grid, GridOrig, FilaOrig, ColOrig, Pos, NumOfColumns, LMarcadosTemp, ValMovidaActual) :-
	Fila is Pos // NumOfColumns,
	Col is Pos - Fila*NumOfColumns,
	1 >= abs(Fila - FilaOrig),
	1 >= abs(Col - ColOrig),
	member(Pos, LMarcadosTemp),
	LMarcadosTemp = [E | _],
	E \= Pos,
	Fila > 0,
	PosArriba is Pos - NumOfColumns,
	Cant = 1,
	buscarAdyacenteIgualAux(GridOrig, FilaOrig, ColOrig, PosArriba, Cant, NumOfColumns, LMarcadosTemp, ValMovidaActual).

buscarAdyacenteIgual(Grid, GridOrig, FilaOrig, ColOrig, Pos, NumOfColumns, LMarcadosTemp, ValMovidaActual) :-
	Grid = [_ | Gs],
	PosSig is Pos + 1,
	buscarAdyacenteIgual(Gs, GridOrig, FilaOrig, ColOrig, PosSig, NumOfColumns, LMarcadosTemp, ValMovidaActual).

%Casos al encontrar un adyacente que pertenece a la movida.

%Si el que está encima del encontrado es igual (al caer cant bloques quedaría adyacente).
buscarAdyacenteIgualAux(Grid, FilaOrig, ColOrig, Pos, Cant, NumOfColumns, LMarcadosTemp, ValMovidaActual) :-
	nth0(Pos, Grid, ValMovidaActual),
	Cant > 0,
	!.

%Si el primero por encima 
buscarAdyacenteIgualAux(Grid, FilaOrig, ColOrig, Pos, Cant, NumOfColumns, LMarcadosTemp, ValMovidaActual) :-
	\+ member(Pos, LMarcadosTemp),
	Fila is Pos // NumOfColumns,
	Fila > 0,
	Col is Pos - Fila*NumOfColumns,
	1 >= abs(Fila - FilaOrig),
	1 >= abs(Col - ColOrig),
	PosArriba is Pos - NumOfColumns,
	buscarAdyacenteIgualAux(Grid, FilaOrig, ColOrig, PosArriba, Cant, NumOfColumns, LMarcadosTemp, ValMovidaActual).

buscarAdyacenteIgualAux(Grid, FilaOrig, ColOrig, Pos, Cant, NumOfColumns, LMarcadosTemp, ValMovidaActual) :-
	\+ member(Pos, LMarcadosTemp),
	CantN is Cant - 1,
	CantN > 0,
	Fila is Pos // NumOfColumns,
	Fila > 0,
	PosArriba is Pos - NumOfColumns,
	buscarAdyacenteIgualAux(Grid, FilaOrig, ColOrig, PosArriba, CantN, NumOfColumns, LMarcadosTemp, ValMovidaActual).

buscarAdyacenteIgualAux(Grid, FilaOrig, ColOrig, Pos, Cant, NumOfColumns, LMarcadosTemp, ValMovidaActual) :-
	member(Pos, LMarcadosTemp),
	LMarcadosTemp = [E | _],
	E \= Pos,
	Fila is Pos // NumOfColumns,
	Fila > 0,
	Col is Pos - Fila*NumOfColumns,
	1 >= abs(Fila - FilaOrig),
	1 >= abs(Col - ColOrig),
	PosArriba is Pos - NumOfColumns,
	CantN is Cant + 1,
	buscarAdyacenteIgualAux(Grid, FilaOrig, ColOrig, PosArriba, CantN, NumOfColumns, LMarcadosTemp, ValMovidaActual).

buscarAdyacenteIgualAux(Grid, FilaOrig, ColOrig, Pos, Cant, NumOfColumns, LMarcadosTemp, ValMovidaActual) :-
	member(Pos, LMarcadosTemp),
	Fila is Pos // NumOfColumns,
	Fila > 0,
	PosArriba is Pos - NumOfColumns,
	buscarAdyacenteIgualAux(Grid, FilaOrig, ColOrig, PosArriba, Cant, NumOfColumns, LMarcadosTemp, ValMovidaActual).

