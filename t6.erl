% A01138741 Ana Isabel Cruz Ramos
% A01176866 Héctor Díaz
% A01338798 Edgar Rubén Salazar Lugo

% -------------------------------------------------------------------
%                              PROTOCOLO
% -------------------------------------------------------------------
% 1. Cambiar nombre de maquina

% ----------------- SISTEMA DISTRIBUIDO DE COMPRAS -----------------
% Socio: Nombre ?
% Producto: {PID, CantidadPedida}
% Listas: Productos, Pedidos, Socios
 
-module(t6).
-import(lists, [filter/2, foreach/2, map/2, delete/2]).
-export([
    tienda/0,
    tienda/5, 
    producto/2,
    elimina_producto/1,
    elimina_productos/2,
    registra_producto/2,
    modifica_producto/2,
    suscribir_socio/1, 
    elimina_socio/1, 
    elimina_el_socio/2,
    busca_socio/2,
    socio/1,
    crea_pedido/2, 
    acepta_pedido/2, 
    rechaza_pedido/2, 
    lista_existencias/0, 
    abre_tienda/0, 
    cierra_tienda/0,
    pedido/2,
    busca_pedido/2,
    eliminar_pedido/2,
    pedidos_en_proceso/0,
    pedidos_atendidos/0,
    busca_producto/2,
    lista_socios/0
]).

% -------------------------------------------------------------------
%                              SERVER
% -------------------------------------------------------------------
% Pedidos: [{Socio, ListaDeProductos}] . ListaDeProductos: [{Producto, Cantidad}]
% Productos : [ {Pid, Producto} ]
% Pedidos : [ {Pid, Numero, Socio} ]

tienda() ->
   process_flag(trap_exit, true),
   tienda(1, [], [], [], []).

tienda(N, Pedidos, Productos, Atendidos, Socios) ->
    receive
        {registra_producto, Producto, Cantidad} -> 
            io:format("Tienda ha recibido solicitud para registrar un producto. ~n"),
            %ProdNode = nodo(Producto),
            %monitor_node(ProdNode, true),
            ProductoID = spawn(t6, producto, [Producto, Cantidad]),
            ProductoID ! {crear, Productos},
            io:format("Producto ~w con cantidad ~w creado ~n", [Producto, Cantidad]),
            tienda(N, Pedidos,  [{ProductoID, Producto, Cantidad}]++Productos, Atendidos, Socios);
        {elimina_producto, Producto} ->
            case busca_producto(Producto, Productos) of
                inexistente ->
                    io:format("El producto no existe ~n", []);
                Pid ->
                    Pid ! elimina
            end,
            tienda(N, Pedidos, elimina_productos(Producto, Productos), Atendidos,  Socios);
        {modifica_producto, Producto, Cantidad} -> 
            case busca_producto(Producto, Productos) of
                inexistente ->
                    io:format("El producto no existe ~n", []);
                Pid ->
                    Pid ! {modifica, Cantidad}
            end,
            tienda(N, Pedidos, Productos, Atendidos, Socios);
        {lista_existencias} ->
            io:format("Productos en inventario: ~n", []),
            lists:foreach(fun({Pid, _, _}) -> Pid ! mostrar_info end, Productos),
            tienda(N, Pedidos, Productos, Atendidos, Socios);
        {suscribir_socio, Socio} ->
            So = nodo(Socio),
            monitor_node(So, true),
            Pid = spawn(So, t6, socio, [Socio]),
            receive
                {nodedown, So} -> 
                    tienda(N, Pedidos, Productos, Atendidos, Socios)
                after 0 -> 
                    monitor_node(So, false),
                    tienda(N, Pedidos, Productos, Atendidos ,Socios++[{Pid, Socio}])
	        end,
            tienda(N, Pedidos, Productos, Atendidos, Socios);
        {elimina_socio, Socio} ->
            case busca_socio(Socio, Socios) of
                inexistente ->
                    io:format("El socio no existe ~n", []);
                Pid ->
                    Pid ! elimina
            end,
            tienda(N, Pedidos, Productos, Atendidos, elimina_el_socio(Socio, Socios));
        {lista_socios} ->
            lists:foreach(fun({Pid, _}) -> Pid ! mostrar_info end, Socios),
            tienda(N, Pedidos, Productos, Atendidos, Socios);
        {crea_pedido, Socio, Pedido} ->
            PedidoID = spawn(t6, pedido, [Pedido, Socio]),
            PedidoID ! {crear, Productos},
            io:format("Pedido ~w creado ~n", [N]),
            tienda(N+1, Pedidos++[{PedidoID, N}], Productos, Atendidos, Socios);
        {rechaza_pedido, Socio, Pedido} ->
            PedidoID = busca_pedido(Pedido, Pedidos),
            PedidoID ! {rechazar, Productos},
            tienda(N,  eliminar_pedido(Pedido, Pedidos), Productos, Atendidos, Socios);
        {acepta_pedido, Socio, Pedido} ->
            io:format("Pedido ~w aceptado ~n", [N]),
            PedidoID = busca_pedido(Pedido, Pedidos),
            tienda(N,  eliminar_pedido(Pedido, Pedidos), Productos, Atendidos++[PedidoID], Socios);
        {pedidos_en_proceso} -> 
            io:format("Pedidos en proceso: ~n", []),
            lists:foreach(fun({Pid, N}) -> 
                io:format("--- Pedido # ~w ~n", [N]), 
                Pid ! mostrar_info 
                end, 
            Pedidos),
            tienda(N, Pedidos, Productos, Atendidos, Socios);
        {pedidos_atendidos} ->
            io:format("---Pedidos Atendidos: ~n"),
            lists:foreach(fun(N) -> N ! mostrar_info end, Atendidos),
            tienda(N, Pedidos, Productos, Atendidos, Socios);
        {cerrar} ->
            io:format("Cerrando tienda ~n")
            % lists:foreach(fun(X) -> cerrar ! X end, Pedidos),
            % lists:foreach(fun(X) -> cerrar ! X end, Productos),
            % lists:foreach(fun(X) -> cerrar ! X end, Socios)
    end.

% -------------------------------------------------------------------
%                          Proceso: Producto
% -------------------------------------------------------------------

producto(Nombre, Cantidad) ->
    receive
        mostrar_info ->
            io:format("Producto: ~w , Cantidad: ~w ~n", [Nombre, Cantidad]),
            producto(Nombre, Cantidad);
        elimina ->
            io:format("Producto ~w eliminado ~n", [Nombre]);
        {modifica, C} ->
            case C > 0 of 
                true -> 
                    producto(Nombre, Cantidad + C);
                false -> % checar que Cantidad mayor a C. Si sí, restamos, sino, nada
                    case Cantidad >= C of
                        true -> 
                             producto(Nombre, Cantidad + C);
                        false ->
                            io:format("ERROR: Resta mayor a cantidad ~n", []),
                            producto(Nombre, Cantidad)
                    end
            end
    end.


busca_producto(_, []) -> inexistente;
busca_producto(Producto, [{Pid, Producto, _}|_]) -> Pid;
busca_producto(Producto, [_|Resto]) -> busca_producto(Producto, Resto).

elimina_productos(_, []) -> [];
elimina_productos(Producto, [{_, Producto}|Resto]) -> Resto;
elimina_productos(Producto, [First|Resto]) -> [First|elimina_productos(Producto, Resto)].

% -------------------------------------------------------------------
%                          Proceso: Pedidos
% -------------------------------------------------------------------
% [{NombreProducto, CantidadDeseada}]
pedido(Pedido, Socio) -> 
    receive
        {crear, Productos} -> 
            lists:map(fun({Producto, Cantidad}) ->
                busca_producto(Producto, Productos) ! {modifica, Cantidad*-1} end, Pedido),
            pedido(Pedido, Socio);
        {rechazar, Productos} ->
            lists:map(fun({Producto, Cantidad}) ->
                busca_producto(Producto, Productos) ! {modifica, Cantidad} end, Pedido),
            pedido(Pedido, Socio);
        mostrar_info ->
            lists:foreach(fun({P, C}) -> io:format("Producto: ~w , Cantidad: ~w  ~n", [P, C]) end, Pedido),
            pedido(Pedido, Socio)
            
    end.

busca_pedido(_, []) -> inexistente;
busca_pedido(N, [{Pid, N}|_]) -> Pid;
busca_pedido(N, [_|Resto]) -> busca_pedido(N, Resto).

eliminar_pedido(_, []) -> [];
eliminar_pedido(Pedido, [{_, Pedido}|Resto]) -> Resto;
eliminar_pedido(Pedido, [First|Resto]) -> [First|eliminar_pedido(Pedido, Resto)].

crea_pedido(Socio, Pedido) ->
    {tienda, nodo(tienda) } ! {crea_pedido, Socio, Pedido}.
    
acepta_pedido(Socio, Pedido) ->
    {tienda, nodo(tienda) } ! {acepta_pedido, Socio, Pedido}.

rechaza_pedido(Socio, Pedido) ->
    {tienda, nodo(tienda) } ! {rechaza_pedido, Socio, Pedido}.
% -------------------------------------------------------------------
%                              SOCIO
% -------------------------------------------------------------------

socio(Socio)->
    receive
        elimina ->
            io:format("Socio ~w eliminado ~n", [socio]);
        mostrar_info ->
          io:format("Socio: ~w~n", [Socio])
    end.


busca_socio(_, []) -> inexistente;
busca_socio(Socio, [{Pid, Socio}|_]) -> Pid;
busca_socio(Socio, [_|Resto]) -> busca_socio(Socio, Resto).

elimina_el_socio(_, []) -> [];
elimina_el_socio(Socio, [{_, Socio}|Resto]) -> Resto;
elimina_el_socio(Socio, [First|Resto]) -> [First|elimina_el_socio(Socio, Resto)].


suscribir_socio(Socio) ->
    {tienda, nodo(tienda) } ! {suscribir_socio, Socio}.

elimina_socio(Socio) ->
    {tienda, nodo(tienda) } ! {elimina_socio, Socio}.


% -------------------------------------------------------------------
%                              INTERFAZ
% -------------------------------------------------------------------

abre_tienda() ->
    io:format("Proceso de tienda corriendo~n"),
    PID = spawn(t6, tienda, []),
    register(tienda, PID),
    link(PID),
    'Tienda abierta'.

cierra_tienda() ->
    {tienda, nodo(tienda)} ! cerrar.

% METODOS DE PRODUCTOS
registra_producto(Producto, Cantidad) ->
  {tienda, nodo(tienda)} ! {registra_producto, Producto, Cantidad},
  ok.

elimina_producto(Producto) ->
    {tienda, nodo(tienda)} ! {elimina_producto, Producto}.

modifica_producto(Producto,Cantidad) ->
    {tienda, nodo(tienda)} ! {modifica_producto, Producto, Cantidad}.

lista_existencias() -> 
    {tienda, nodo(tienda) } ! {lista_existencias}.

pedidos_en_proceso() ->
    {tienda, nodo(tienda) } ! {pedidos_en_proceso}.
    
pedidos_atendidos() ->
    {tienda, nodo(tienda)} ! {pedidos_atendidos}.

lista_socios() ->
  {tienda, nodo(tienda)} ! {lista_socios}.

% -------------------------------------------------------------------
%                               NODO
% -------------------------------------------------------------------
nodo(Nombre) -> list_to_atom(atom_to_list(Nombre)++"@CHITOXD").
