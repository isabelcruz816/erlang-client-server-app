-module(tienda).
-export([
    abre_tienda/0, 
    tienda/0, 
    cierra_tienda/0, 
    suscribir_socio/1, 
    elimina_socio/1, 
    lista_socios/0
]).

%hi isa caile al zoom glitch
% pazate el link https://itesm.zoom.us/j/84192045499 no abriste el

producto(Nombre, Cantidad) ->
    receive
        elimina ->
            io:format("Producto ~w eliminado ~ñ", [Nombre]);
        {modifica, NuevaCantidad} ->
            io:format("Producto ~w ahora tiene la cantidad de  ~ñ", [Nombre]);


tienda() ->
    receive
        cerrar ->
            io:format("Cerrando tienda ~n");
        {suscribir, Socio} ->
            io:format("Suscribiendo socio ~w ~n", [Socio]),
            tienda();
        {eliminar, Socio} ->
            io:format("Eliminando socio ~w ~n", [Socio]),
            tienda()
    end.

abre_tienda() ->
    register(tienda, spawn(tienda, tienda, [])).

cierra_tienda() ->
    tienda ! cerrar.

suscribir_socio(Socio) ->
    tienda ! {suscribir, Socio}.

elimina_socio(Socio) ->
    tienda ! {eliminar, Socio}.


lista_socios() ->
    io:format("Obteniendo lista de socios ~n", []).
