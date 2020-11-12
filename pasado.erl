% A01138741 Ana Isabel Cruz Ramos
% A01176866 Héctor Díaz
% A01338798 Edgar Rubén Salazar Lugo 

cd("C:/Users/edgar/OneDrive/Documentos/ITC/1. LENGUAJES/ERLANG").

-module(a01138741_A01176866_A01338798_Ejercicio16).
-export([inicio/0, 
    crea_esclavo/1, 
    servidor/2, 
    esclavo/1,
    para_esclavo/2,
    termina/0
    ]).


esclavo (Id) -> 
    receive
        morir ->
            io:format("El esclavo ~w ha muerto ~n", [Id]);
        {Mensaje} -> 
            case Mensaje == morir of
                true -> 
                    io:format("El esclavo ~w ha muerto ~n", [Id]);
                false -> 
                    io:format("El esclavo ~w recibió el mensaje ~w~n", [Id, Mensaje]),
                    esclavo(Id)
            end
    end.

servidor(Esclavos, Id) -> 
    receive
        { crear, Nodo } -> 
            io:format("Esclavo ~w creado en nodo ~w~n", [Id, Nodo]),
            P = spawn(a01138741_A01176866_A01338798_Ejercicio16, esclavo, [Id]),
            servidor(Esclavos ++ [{Id, P}], Id+1); 
        { para, EsclavoId, Mensaje } ->
            busca_esclavo(Esclavos, EsclavoId, Mensaje),
            servidor(Esclavos, Id);
        { termina } ->
            terminaEsclavos(Esclavos)
    end.    

busca_esclavo([{Id, P} | _], Id, Mensaje) ->
    P ! {Mensaje};
busca_esclavo([ _ | Resto],Id, Mensaje) ->
    busca_esclavo(Resto, Id, Mensaje).

terminaEsclavos([]) -> io:format("El maestro terminó ~n", []);
terminaEsclavos([{_, P} | Resto ]) ->
    P ! morir,
    terminaEsclavos(Resto).    

inicio() -> 
    register(master, spawn(a01138741_A01176866_A01338798_Ejercicio16, servidor, [[], 1]) ).

crea_esclavo (Nodo) ->
    master ! { crear, Nodo }.

para_esclavo(Mensaje, Id) ->
    master ! { para, Id, Mensaje}.

termina () -> 
    master ! { termina }.    