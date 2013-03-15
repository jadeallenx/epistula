%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2009 Marc Worrell
%%
%% @doc
%% Encode a string according to RFC 2047 using quoted-printable.
%% Assumes UTF-8 as the character set.
%%
%% This module was modified from the original to support encoding only.
%%
%% Original source:
%% [https://raw.github.com/zotonic/zotonic/master/src/smtp/rfc2047.erl]
%% @end

-module(rfc2047).
-author("Marc Worrell <marc@worrell.nl>").

-export([encode/1]).

-spec encode( Word :: string() | binary() ) -> string().
% @doc Encode given string as in UTF-8 quoted printable format
encode(B) when is_binary(B) ->
	encode(binary_to_list(B));
encode([]) -> 
	[];
encode(Text) ->
    encode(Text, Text).

    %% Don't escape when all characters are ASCII printable
    encode([], Text) ->
        Text;
    encode([H|T], Text) when H >= 32 andalso H =< 126 andalso H /= $= ->
        encode(T, Text);
    encode(_, Text) ->
        "=?UTF-8?Q?" ++ encode(Text, [], 0) ++ "?=".

encode([], Acc, _WordLen) ->
    lists:reverse(Acc);
encode(T, Acc, WordLen) when WordLen >= 55 ->
    %% Make sure that the individual encoded words are not longer than 76 chars (including charset etc)
    encode(T, [$?,$Q,$?,$8,$-,$F,$T,$U,$?,$=,32,10,13,$=,$?|Acc], 0);
encode([C|T], Acc, WordLen) when C > 32 andalso C < 127 andalso C /= 32 
    andalso C /= $? andalso C /= $_ andalso C /= $= andalso C /= $. ->
    encode(T, [C|Acc], WordLen+1);
encode([C|T], Acc, WordLen) ->
    encode(T, [hex(C rem 16), hex(C div 16), $= | Acc], WordLen+3).

hex(N) when N >= 10 -> N + $A - 10;
hex(N) -> N + $0.
