-export_type([ connection_type/0
             , protocol_type/0
             ]).

-type(connection_type() :: initial | reconnection).
-type(protocol_type() :: 4).


-record(diffusion_connection, { type :: connection_type()
                              , client_id :: binary()
                              , version :: protocol_type()}).

-record(diffusion_message, {type :: atom(),
                            headers :: list(binary()),
                            data :: list(binary())}).
