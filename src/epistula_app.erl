% @hidden
-module(epistula_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    ok = application:start(crypto),
    ok = application:start(esmtp),
    epistula_sup:start_link().

stop(_State) ->
    ok.
