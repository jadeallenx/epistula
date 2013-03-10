%% @doc 
%% @author Mark Allen <mrallen1@yahoo.com>
%% @copyright Copyright (C) 2013 Mark Allen
%%
%% Copyright 2013 Mark Allen
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%   http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.

-module(epistula).
-author('Mark Allen <mrallen1@yahoo.com>').

-include("mime.hrl").

-export([
    new/0,
    new/1,
    new/3,
    msg/4,
    add_header/2,
    add_headers/2,
    update_header/2,
    delete_header/2,
    update_message_type/2,
    new_part/0,
    new_part/4,
    add_part/2,
    remove_last_part/2,
    add_plain_text_part/2,
    inline_file/2,
    inline_file/3,
    attach_file/2,
    attach_file/3
    ]
).

%% API

new() ->
    #mime_msg{boundary=generate_boundary()}.

-spec new( Type :: atom() ) -> #mime_msg{}.
new(Type) ->
    update_message_type(new(), Type).

-spec new( To :: string(), From :: string(), Subject :: string() ) -> #mime_msg{}.
new(To, From, Subject) ->
    add_headers(new(), [
        {"To", To},
        {"From", From},
        {"Subject", Subject} ]
    ).

-spec new( To :: string(), From :: string(), Subject :: string(), Body :: string() ) -> #mime_msg{}.
new(To, From, Subject, Body) ->
    M = new(To, From, Subject),
    add_plain_text_part(M, Body).

-spec add_header( Msg :: #mime_msg{}, Header :: { Key :: atom() | string(), Value :: string() } ) -> #mime_msg{}. 
add_header(Msg = #mime_msg{headers = H}, Header = {Key, _Value}) when is_list(Key) ->
    Msg#mime_msg{headers = lists:keystore(H, 1, Key, Header)};

add_header(Msg, _Header = {Key, Value}) when is_atom(Key) ->
    add_header(Msg, {atom_to_list(Key), Value}).

-spec add_headers( Msg :: #mime_msg{}, Headers :: [{ Key :: string(), Value :: string() }] ) -> #mime_msg{}.
add_headers(Msg = #mime_msg{headers = H}, Headers) when is_list(Headers)->
    Msg#mime_msg{headers = H ++ Headers}.

-spec update_header( Msg :: #mime_msg{}, Header :: { Key :: atom() | string(), Value :: string() } ) -> #mime_msg{}.
update_header(Msg, Header = {Key, _Value}) when is_list(Key) ->
    add_header(Msg, Header);

update_header(Msg, _Header = {Key, Value}) when is_atom(Key) ->
    add_header(Msg, {atom_to_list(Key), Value}).

-spec delete_header( Msg :: #mime_msg{}, Header :: { Key :: atom() | string(), Value :: string() } ) -> #mime_msg{}.
delete_header(Msg = #mime_msg{headers = H}, Header = {Key, _Value}) when is_list(Key) -> 
    Msg#mime_msg{headers = lists:keydelete(H, 1, Key, Header)};

delete_header(Msg, _Header = {Key, Value}) when is_atom(Key) ->
    delete_header(Msg, {atom_to_list(Key), Value}).
    
-spec update_message_type( Msg :: #mime_msg{}, Type :: atom() ) -> #mime_msg{}.
update_message_type(Msg, Type) ->
    Msg#mime_msg{type = Type}.

-spec new_part() -> #mime_part{}.
new_part() -> 
    #mime_part{}.

-spec new_part( 
    Encoding :: { EncodingType :: string(), MimeType :: string(), CharacterSet :: string() },
    Disposition :: atom(),
    Filename :: 'undefined' | string(),
    Data :: string() ) -> #mime_part{}.
new_part(Encoding, Disposition, Filename, Data) ->
    #mime_part{
        encoding = Encoding,
        disposition = Disposition,
        filename = Filename,
        data = Data}.

-spec add_part( Msg :: #mime_msg{}, Part :: #mime_part{} ) -> #mime_msg{}.
add_part(Msg = #mime_msg{parts = P}, Part) ->
    Msg#mime_msg{parts = P ++ [Part]}.

-spec remove_last_part( Msg :: #mime_msg{} ) -> #mime_msg{}.
remove_last_part(Msg = #mime_msg{parts = P}) ->
    Msg#mime_msg{parts = remove_part(P)}.
    
-spec add_plain_text_part( Msg :: #mime_msg{}, Body :: string() ) -> #mime_msg{}.
add_plain_text_part(Msg = #mime_msg{parts = P}, Body) -> 
    Msg#mime_msg{parts = P ++ [ #mime_part{ data = Body } ]}.
    



%% PRIVATE FUNCTIONS

generate_boundary() ->
    <<X:64/big-unsigned-integer>> = crypto:rand_bytes(8),
    "===========" ++ io_lib:format("~12.36.0B", [X]).

remove_part(P) ->
    I = length(P) - 1,
    lists:sublist(P, I).
