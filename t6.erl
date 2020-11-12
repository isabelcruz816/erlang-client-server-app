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
    tienda/3, 
    producto/2,
    elimina_producto/1,
    elimina_productos/2,
    registra_producto/2,
    modifica_producto/2,
    % suscribir_socio/1, 
    % elimina_socio/1, 
    % crea_pedido/2, 
    % acepta_pedido/2, 
    % rechaza_pedido/2, 
    lista_existencias/0, 
    abre_tienda/0, 
    cierra_tienda/0
    % nodo/1 pregunta asesoria
]).

% -------------------------------------------------------------------
%                              SERVER
% -------------------------------------------------------------------
% Pedidos: [{Socio, ListaDeProductos}] . ListaDeProductos: [{Producto, Cantidad}]
% Productos : [ {Pid, Producto} ]

tienda() ->
   process_flag(trap_exit, true), % pregunta asesoria
   tienda([], [], []).

tienda(Pedidos, Productos, Socios) ->
    receive
        {registra_producto, Producto, Cantidad} -> 
            ProdNode = nodo(Producto),
            monitor_node(ProdNode, true),
            Pid = spawn(ProdNode, t6, producto, [Producto, Cantidad]),
            receive
                {nodedown, ProdNode} -> 
                    tienda(Pedidos, Productos, Socios)
                after 0 -> 
                    monitor_node(ProdNode, false),
                    tienda(Pedidos, Productos++[{Pid, Producto}], Socios)
	          end;
        {elimina_producto, Producto} ->
            case busca_producto(Producto, Productos) of
                inexistente ->
                    io:format("El producto no existe ~n", []);
                Pid ->
                    Pid ! elimina
            end,
            tienda(Pedidos, elimina_productos(Producto, Productos), Socios);
        {modifica_producto, Producto, Cantidad} -> 
            case busca_producto(Producto, Productos) of
                inexistente ->
                    io:format("El producto no existe ~n", []);
                Pid ->
                    Pid ! {modifica, Cantidad}
            end,
            tienda(Pedidos, Productos, Socios);
        {lista_productos} ->
            io:format("Productos en inventario: ~n", []),
            lists:foreach(fun({Pid, _}) -> Pid ! mostrar_info end, Productos),
            tienda(Pedidos, Productos, Socios);
        {cerrar} ->
            io:format("Cerrando tienda ~n");
            % lists:foreach(fun(X) -> cerrar ! X end, Pedidos),
            % lists:foreach(fun(X) -> cerrar ! X end, Productos),
            % lists:foreach(fun(X) -> cerrar ! X end, Socios);
        {suscribir, Socio} ->
            io:format("Suscribiendo socio ~w ~n", [Socio]),
            tienda(Pedidos, Productos, Socios);
        {eliminar, Socio} ->
            io:format("Eliminando socio ~w ~n", [Socio]),
            tienda(Pedidos, Productos, Socios)
    end.

% -------------------------------------------------------------------
%                          Proceso: Producto
% -------------------------------------------------------------------

producto(Nombre, Cantidad) ->
    receive
        mostrar_info ->
            io:format("~w ~w ~n", [Nombre, Cantidad]),
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
busca_producto(Producto, [{Pid, _, Producto}|_]) -> Pid;
busca_producto(Producto, [_|Resto]) -> busca_producto(Producto, Resto).

elimina_productos(_, []) -> [];
elimina_productos(Producto, [{_, _, Producto}|Resto]) -> Resto;
elimina_productos(Producto, [First|Resto]) -> [First|elimina_productos(Producto, Resto)].


% -------------------------------------------------------------------
%                          Proceso: Pedidos
% -------------------------------------------------------------------

% -------------------------------------------------------------------
%                              CLIENT
% -------------------------------------------------------------------
% nombre de la maquina

% suscribir_socio
%   Socio es un atomo (nombre), id unico de socio
%   Caso 1: Ya existe ese ID, intentar de nuevo con otro nombre.
%   Caso 2: No existe, registrarlo.
% suscribir_socio(Socio) -> io:format(Socio).

% elimina_socio
%   Caso 1: No existe el socio, no hacer nada.
%   Caso 2: Si existe, eliminar todos sus pedidos no entregados 
%           y ajustar la informacion.
% elimina_socio(Socio) -> io:format(Socio).

% crea_pedido
% Tienda genera un numero de pedido y responde con la lista de pedido
% ajustada a las existencias de productos.
% crea_pedido(Socio, Productos) -> io:format(Socio, Productos).

% acepta_pedido(Socio, Pedido) -> io:format(Socio, Pedido).

%rechaza_pedido(Socio, Pedido) -> io:format(Socio, Pedido).

% lista_existencias
% Despliega una lista de productos
lista_existencias() -> 
    {tienda, nodo(tienda) } ! {lista_productos}.

% -------------------------------------------------------------------
%                              INTERFAZ
% -------------------------------------------------------------------
% Abrir la tienda
abre_tienda() ->
    io:format("Proceso de tienda corriendo"),
    register(tienda, spawn(t6, tienda, [])),
    'Tienda abierta'.

% Cerrar la tienda
cierra_tienda() ->
    {tienda, nodo(tienda)} ! cerrar.
 
registra_producto(Producto, Cantidad) ->
  {tienda, nodo(tienda)} ! {registra_producto, Producto, Cantidad},
  ok.

elimina_producto(Producto) ->
    {tienda, nodo(tienda)} ! {elimina_producto, Producto}.

modifica_producto(Producto,Cantidad) ->
    {tienda, nodo(tienda)} ! {modifica_producto, Producto, Cantidad}.

% Este cambia
nodo(Nombre) -> list_to_atom(atom_to_list(Nombre)++"@Isabels-MacBook-Pro").