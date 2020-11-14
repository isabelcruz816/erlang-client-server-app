
% Tarea 6
% A01138741 Ana Isabel Cruz Ramos
% A01176866 Héctor Díaz
% A01338798 Edgar Rubén Salazar Lugo

% -------------------------------------------------------------------
%                         PROTOCOLO DE PRUEBAS
% -------------------------------------------------------------------
% 1. En la linea, 364, cambiar el nombre la maquina.
% 2. Abrir una terminal
% 3. Correr el comando "erl -sname tienda"
% 4. Cambiar el nombre del archivo a "t6"
% 5. Cargar el codigo con el comando "c(t6)."
% 6. Abrir otra terminal y correr "erl -sname socio"
% 7. En la terminal del socio puede hacer las solicitudes a la tienda
% 8. Probar los comandos en las terminales del proceso correspondiente.
% 9. Y listo :)
%
% -------------------------------------------------------------------
%                         ESTRUCTURAS DE DATOS
% -------------------------------------------------------------------
% Pedidos: 
%   lista de pedidos en proceso
%   [{Pid, N}] en donde N es el numero del pedido     
% Productos: 
%   lista de productos en existencia
%   [{Nombre, Cantidad}]
% Atendidos: 
%   lista de pedidos atendidos
%   [Pid]
% Socios: 
%   lista de socios registrados
%   [{Pid, Nomnbre}]

% -------------------------------------------------------------------
%                        SISTEMA DISTRIBUIDO
% -------------------------------------------------------------------
-module(t6).
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
% tienda/0
% Llama la funcion tienda/5 y prende la bandera de salida.
tienda() ->
   process_flag(trap_exit, true),
   tienda(1, [], [], [], []).

% tienda/5
% Revisa los mensajes recibidos al proceso.
tienda(N, Pedidos, Productos, Atendidos, Socios) ->
    receive
        {registra_producto, Producto, Cantidad} -> 
            io:format("Solicitud para registrar un producto. ~n"),
            ProductoID = spawn(t6, producto, [Producto, Cantidad]),
            ProductoID ! {crear, Productos},
            io:format("Producto ~w con cantidad ~w creado ~n", 
                [Producto, Cantidad]),
            tienda(N, Pedidos,  [{ProductoID, Producto, Cantidad}]++Productos, 
                  Atendidos, Socios);
        {elimina_producto, Producto} ->
            case busca_producto(Producto, Productos) of
                inexistente ->
                    io:format("El producto no existe ~n", []);
                Pid ->
                    Pid ! elimina
            end,
            tienda(N, Pedidos, 
              elimina_productos(Producto, Productos), Atendidos,  Socios);
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
            tienda(N,  eliminar_pedido(Pedido, Pedidos), Productos, 
              Atendidos, Socios);
        {acepta_pedido, Socio, Pedido} ->
            io:format("Pedido ~w aceptado ~n", [N]),
            PedidoID = busca_pedido(Pedido, Pedidos),
            tienda(N,  eliminar_pedido(Pedido, Pedidos), Productos, 
              Atendidos++[PedidoID], Socios);
        {pedidos_en_proceso} -> 
            io:format("Pedidos en proceso: ~n", []),
            lists:foreach(fun({Pid, I}) -> 
                io:format("--- Pedido # ~w ~n", [I]), 
                Pid ! mostrar_info 
                end, 
            Pedidos),
            tienda(N, Pedidos, Productos, Atendidos, Socios);
        {pedidos_atendidos} ->
            io:format("---Pedidos Atendidos: ~n"),
            lists:foreach(fun(I) -> I ! mostrar_info end, Atendidos),
            tienda(N, Pedidos, Productos, Atendidos, Socios);
        cerrar ->
            io:format("Cerrando tienda ~n"),
            exit(whereis(tienda), kill)
    end.

% -------------------------------------------------------------------
%                          Proceso: Producto
% -------------------------------------------------------------------
% producto/2
% Recibe un producto y busca empatar con algun mensaje para ejecutar
% las instrucciones correspondientes a la solicitud.
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
                false -> % checar que Cantidad sea mayor a C. Si sí, restamos, sino, nada
                    case Cantidad >= C of
                        true -> 
                             producto(Nombre, Cantidad + C);
                        false ->
                            io:format("ERROR: Resta mayor a cantidad ~n", []),
                            producto(Nombre, Cantidad)
                    end
            end;
        {get_cantidad, De} ->
            De ! {Cantidad},
            producto(Nombre, Cantidad)
    end.

% busca_producto/2
% Busca un producto en la lista de Productos, si lo encuentra, regresa su Pid,
% si no lo encuetra regresa el atomo de inexistente.
busca_producto(_, []) -> inexistente;
busca_producto(Producto, [{Pid, Producto, _}|_]) -> Pid;
busca_producto(Producto, [_|Resto]) -> busca_producto(Producto, Resto).

% elimina_productos/2
% Elimina un producto de la lista de productos
elimina_productos(_, []) -> [];
elimina_productos(Producto, [{_, Producto}|Resto]) -> Resto;
elimina_productos(Producto, [First|Resto]) -> [First|elimina_productos(Producto, Resto)].

% -------------------------------------------------------------------
%                          Proceso: Pedidos
% -------------------------------------------------------------------
% pedido/2
% Recibe un pedido y busca empatar con algun mensaje para ejecutar
% las instrucciones correspondientes a la solicitud.
pedido(Pedido, Socio) -> 
    receive
        {crear, Productos} -> 
            lists:map(fun({Producto, Cantidad}) ->
                ProdID = busca_producto(Producto, Productos),
                ProdID ! {get_cantidad, self()},
                receive
                    {Existencia} ->
                        case Existencia >= Cantidad of 
                            true -> ProdID ! {modifica, Cantidad*-1};
                            false-> ProdID ! {modifica, Existencia*-1}
                        end
                end
            end, Pedido),
            pedido(Pedido, Socio);
        {rechazar, Productos} ->
            lists:map(fun({Producto, Cantidad}) ->
                busca_producto(Producto, Productos) ! {modifica, Cantidad} end, Pedido);
        mostrar_info ->
            lists:foreach(fun({P, C}) -> io:format("Producto: ~w , Cantidad: ~w  ~n", 
              [P, C]) end, Pedido),
            pedido(Pedido, Socio)
            
    end.

% busca_pedido/2
% Busca un pedido en la lista de Pedidos, si lo encuentra, regresa su Pid,
% si no lo encuetra regresa el atomo de inexistente.
busca_pedido(_, []) -> inexistente;
busca_pedido(N, [{Pid, N}|_]) -> Pid;
busca_pedido(N, [_|Resto]) -> busca_pedido(N, Resto).

% eliminar_pedido/2
% Elimina un pedido de la lista de Pedidos recursivamente.
eliminar_pedido(_, []) -> [];
eliminar_pedido(Pedido, [{_, Pedido}|Resto]) -> Resto;
eliminar_pedido(Pedido, [First|Resto]) -> [First|eliminar_pedido(Pedido, Resto)].

% crea_pedido/2
% Envia el mensaje de crea_pedido al servidor.
crea_pedido(Socio, Pedido) ->
    {tienda, nodo(tienda) } ! {crea_pedido, Socio, Pedido}.

% acepta_pedido/2
% Envia el mensaje de acepta_pedido al servidor.
acepta_pedido(Socio, Pedido) ->
    {tienda, nodo(tienda) } ! {acepta_pedido, Socio, Pedido}.

% rechaza_pedido/2
% Envia el mensaje de rechaza_pedido al servidor.
rechaza_pedido(Socio, Pedido) ->
    {tienda, nodo(tienda) } ! {rechaza_pedido, Socio, Pedido}.

% -------------------------------------------------------------------
%                            CLIENTE
% -------------------------------------------------------------------
% socio/1
% Recibe solicitudos para el socio y empata con el mensaje de 
% la solicitud
socio(Socio)->
    receive
        elimina ->
            io:format("Socio ~w eliminado ~n", [Socio]);
        mostrar_info ->
          io:format("Socio: ~w~n", [Socio]),
          socio(Socio)
    end.

% busca_socio/2
% Busca un socio en la lista de Socios, si lo encuentra, regresa su Pid,
% si no lo encuetra regresa el atomo de inexistente.
busca_socio(_, []) -> inexistente;
busca_socio(Socio, [{Pid, Socio}|_]) -> Pid;
busca_socio(Socio, [_|Resto]) -> busca_socio(Socio, Resto).

% elimina_el_socio/2
% Elimina un socio de la lista de socios.
elimina_el_socio(_, []) -> [];
elimina_el_socio(Socio, [{_, Socio}|Resto]) -> Resto;
elimina_el_socio(Socio, [First|Resto]) -> [First|elimina_el_socio(Socio, Resto)].

% suscribir_socio/1
% Envia el mensaje de suscribir_socio a la tienda.
suscribir_socio(Socio) ->
    {tienda, nodo(tienda) } ! {suscribir_socio, Socio}.

% elimina_socio/1
% Envia el mensaje de eliminar_socio a la tienda.
elimina_socio(Socio) ->
    {tienda, nodo(tienda) } ! {elimina_socio, Socio}.


% -------------------------------------------------------------------
%                              INTERFAZ
% -------------------------------------------------------------------
% abre_tienda/0
% Crea el proceso tienda, registra el proceso y hace el encadenamiento.
abre_tienda() ->
    io:format("Proceso de tienda corriendo~n"),
    PID = spawn(t6, tienda, []),
    register(tienda, PID),
    link(PID),
    'Tienda abierta'.

% cierra_tienda/0
% Envia el mensaje de cerrar a la tienda.
cierra_tienda() ->
    {tienda, nodo(tienda)} ! cerrar.

% ----------       METODOS DE PRODUCTOS      ---------- 

% registra_producto/2
% Envia el mensaje de registra_producto a la tienda.
registra_producto(Producto, Cantidad) ->
  {tienda, nodo(tienda)} ! {registra_producto, Producto, Cantidad},
  ok.

% elimiina_producto/1
% Envia el mensaje de elimina_producto a la tienda.
elimina_producto(Producto) ->
    {tienda, nodo(tienda)} ! {elimina_producto, Producto}.

% modifica_producto/2
% Envia el mensaje de modifica_producto a la tienda.
modifica_producto(Producto,Cantidad) ->
    {tienda, nodo(tienda)} ! {modifica_producto, Producto, Cantidad}.

% lista_existencias/0
% Envia el mensaje de lista_de_existencias a la tienda para desplegar
% la lista de productos.
lista_existencias() -> 
    {tienda, nodo(tienda) } ! {lista_existencias}.

% pedidos_en_proceso/0
% Envia el mensaje de pedidos_en_proceso a la tienda para desplegar
% la lista de pedidos en proceso.
pedidos_en_proceso() ->
    {tienda, nodo(tienda) } ! {pedidos_en_proceso}.

% pedidos_en_atendidos/0
% Envia el mensaje de pedidos_en_atendidos a la tienda para desplegar
% la lista de pedidos en proceso. 
pedidos_atendidos() ->
    {tienda, nodo(tienda)} ! {pedidos_atendidos}.

% lista_socios/0
% Envia el mensaje de lista_socios a la tienda para desplegar
% la lista de socios. 
lista_socios() ->
  {tienda, nodo(tienda)} ! {lista_socios}.

% -------------------------------------------------------------------
%                               NODO
% -------------------------------------------------------------------
% Se crea el nodo de la maquina
nodo(Nombre) -> list_to_atom(atom_to_list(Nombre)++"@Isabels-MacBook-Pro").