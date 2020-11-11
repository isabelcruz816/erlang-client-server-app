% A01138741 Ana Isabel Cruz Ramos
% A01176866 Héctor Díaz
% A01338798 Edgar Rubén Salazar Lugo

% ----------------- SISTEMA DISTRIBUIDO DE COMPRAS -----------------
% Socio: {Nombre, [Membresias]}
% Producto: {PID, CantidadPedida}
% Tienda: {TID, [Clientes], [Productos]}
% Listas: Productos, Existencias, Pedidos, Clientes, Tiendas ?
 
-module(t6).
-import(lists, [filter/2, foreach/2, map/2, delete/2]).
-export([suscribir_socio/1, elimina_socio/1, crea_pedido/2, 
  acepta_pedido/2, rechaza_pedido/2, lista_existencias/0]).

% -------------------------------------------------------------------
%                              SERVER
% -------------------------------------------------------------------

% -------------------------------------------------------------------
%                          Proceso: Producto
% -------------------------------------------------------------------

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
suscribir_socio(Socio) -> io:format(Socio).

% elimina_socio
%   Caso 1: No existe el socio, no hacer nada.
%   Caso 2: Si existe, eliminar todos sus pedidos no entregados 
%           y ajustar la informacion.
elimina_socio(Socio) -> io:format(Socio).

% crea_pedido
% Tienda genera un numero de pedido y responde con la lista de pedido
% ajustada a las existencias de productos.
crea_pedido(Socio, Productos) -> io:format(Socio, Productos).

acepta_pedido(Socio, Pedido) -> io:format(Socio, Pedido).

rechaza_pedido(Socio, Pedido) -> io:format(Socio, Pedido).

% lista_existencias
% Despliega una lista de productos
lista_existencias() -> io:format([]).