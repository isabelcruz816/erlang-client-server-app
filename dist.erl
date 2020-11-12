%% EJERCICIO DE CLASE # 16

-module(dist).
-export([inicio/0, crea_esclavo/1, para_esclavo/2, termina/0,
         maestro/0, esclavo/1]).

% CÓDIGO PARA EL MAESTRO

% proceso del maestro
maestro() ->
   process_flag(trap_exit, true),
   maestro(1, []).
% maestro recuerda el número del siguiente esclavo
% a crearse y la lista de esclavos existentes
maestro(N, Esclavos) ->
   receive
      {crea_esclavo, NNodo} ->
		 Nodo = nodo(NNodo),
	     monitor_node(Nodo, true),
         Pid = spawn(Nodo, dist, esclavo, [N]),
		 receive
		    {nodedown, Nodo} -> 
			   io:format("nodo ~w no existe~n", [NNodo]),
			   maestro(N, Esclavos)
			after 0 -> 
			   io:format("esclavo ~w creado en nodo ~w~n",
			             [N, NNodo]),
			   monitor_node(Nodo, false),
			   maestro(N+1, Esclavos++[{N, Pid}])
	     end;
	  {De, {mensaje, Mensaje, Nesclavo}} ->
	     case busca(Nesclavo, Esclavos) of
		    inexistente ->
			   De ! inexistente;
			Epid ->
			   Epid ! {mensaje, Mensaje},
			   De ! enviado
		  end,
		 maestro(N, Esclavos); 
	  {'EXIT', PID, _} -> 
		 maestro(N, elimina(PID, Esclavos));
      termina -> 
	     lists:map(fun({_, Epid}) -> 
		              Epid ! {mensaje, morir} end,
		           Esclavos)
   end.
   
% funciones auxiliares

% busca un nombre dentro de la lista de usuarios
busca(_, []) -> inexistente;
busca(N, [{N, PID}|_]) -> PID;
busca(N, [_|Resto]) -> busca(N, Resto).

% elimina el esclavo con determinado PID
elimina(_, []) -> [];
elimina(PID, [{_, PID}|Resto]) -> Resto;
elimina(PID, [Esclavo|Resto]) -> [Esclavo|elimina(PID, Resto)].

% FUNCIONES DE INTERFAZ DE USUARIO

% crea y registra el proceso del maestro con el alias "maestro"
inicio() ->
   register(maestro, spawn(dist, maestro, [])),
   'maestro creado'.
   
% pide al maestro crear un esclavo en un nodo distribuido
crea_esclavo(Nodo) ->
   {maestro, nodo(maestro)} ! {crea_esclavo, Nodo},
   ok.
   
% envia mensaje a esclavo a través del maestro
para_esclavo(Mensaje, Nesclavo) ->
   {maestro, nodo(maestro)} ! {self(), {mensaje, Mensaje, Nesclavo}},
   receive 
      inexistente -> 
	     io:format("El esclavo ~w no existe~n", [Nesclavo]);
	  enviado ->
	     {Mensaje, Nesclavo}
   end.
		
% termina el proceso maestro
termina() ->
   {maestro, nodo(maestro)} ! termina,
   'El maestro termino'.
   
% nombre corto del servidor (nombre@máquina)
nodo(Nombre) -> list_to_atom(atom_to_list(Nombre)++"@Air-de-Hector").

% CÓDIGO PARA ESCLAVOS

esclavo(N) ->
   receive
      {mensaje, morir} ->
	     io:format("El esclavo ~w ha muerto~n", [N]);
      {mensaje, M} ->
	     io:format("El esclavo ~w recibió el mensaje ~w~n",
		           [N, M]),
	     esclavo(N)
   end.