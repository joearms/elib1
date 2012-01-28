%% Copyright (c) 2006-2009 Joe Armstrong
%% See MIT-LICENSE for licensing information.

%% JSON - RFC 4627 - for Erlang
%%---------------------------------------------------------------------------
%% Original Copyright notices
%% Copyright (c) 2007 Tony Garnock-Jones <tonyg@kcbbs.gen.nz>
%% Copyright (c) 2007 LShift Ltd. <query@lshift.net>
%%
%% Permission is hereby granted, free of charge, to any person
%% obtaining a copy of this software and associated documentation
%% files (the "Software"), to deal in the Software without
%% restriction, including without limitation the rights to use, copy,
%% modify, merge, publish, distribute, sublicense, and/or sell copies
%% of the Software, and to permit persons to whom the Software is
%% furnished to do so, subject to the following conditions:
%%
%% The above copyright notice and this permission notice shall be
%% included in all copies or substantial portions of the Software.
%%
%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
%% EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
%% MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
%% NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS
%% BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN
%% ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
%% CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
%% SOFTWARE.
%%---------------------------------------------------------------------------
%%
%% encode(val()) -> str()
%% decode(str()) -> {ok, val(), str()} | {error, Reason}
%%                  where Reason is usually far too much information
%%                  and should be ignored.
%%
%% Data type mapping as per Joe Armstrong's message
%% http://www.erlang.org/ml-archive/erlang-questions/200511/msg00193.html:
%%
%%	JSON Obj    = type obj()   = {obj, [{key(), val()}]}
%%	JSON Array  = type array() = [val()]
%%	JSON Number = type num()   = int() | float()
%%	JSON String = type str()   = bin()
%%	JSON true false null       = true, false null (atoms)
%%	With Type val() = obj() | array() | num() | str() | true | false
%%                      | null
%%
%% and key() being a str(). (Or a binary or atom, during JSON encoding.)
%%
%% I'm lenient in the following ways during parsing:
%%  - repeated commas in arrays and objects collapse to a single comma
%%  - any character =<32 is considered whitespace
%%  - leading zeros for numbers are accepted
%%  - we don't restrict the toplevel token to only object or array -
%%    any JSON value can be used at toplevel
%%
%% When serializing a string, if characters are found with codepoint
%% >127, we rely on the unicode encoder to build the proper byte
%% sequence for transmission. We still use the uXXXX escape for
%% control characters (other than the RFC-specified specially
%% recognised ones).
%%
%% decode/1 will autodetect the unicode encoding used, and any strings
%% returned in the result as binaries will contain UTF-8 encoded byte
%% sequences for codepoints >127. Object keys containing codepoints
%% >127 will be returned as lists of codepoints, rather than being
%% UTF-8 encoded. If you have already transformed the text to parse
%% into a list of unicode codepoints, perhaps by your own use of
%% unicode_decode/1, then use decode_noauto/1 to avoid redundant and
%% erroneous double-unicode-decoding.
%%
%% Similarly, encode/1 produces text that is already UTF-8 encoded. To
%% get raw codepoints, use encode_noauto/1 and encode_noauto/2. You
%% can use unicode_encode/1 to UTF-encode the results, if that's
%% appropriate for your application.

%% Modifed: Joe Armstrong

-module(elib1_rfc4627).

-export([mime_type/0, encode/1, decode/1]).
-export([encode_noauto/1, encode_noauto/2, decode_noauto/1]).
-export([unicode_decode/1, unicode_encode/1]).
-export([from_record/3, to_record/3]).
-export([hex_digit/1, digit_hex/1]).

mime_type() ->
    "application/json".

encode(X) ->
    unicode_encode({'utf-8', encode_noauto(X)}).

encode_noauto(X) ->
    lists:reverse(encode_noauto(X, [])).

encode_noauto(true, Acc) ->
    "eurt" ++ Acc;
encode_noauto(false, Acc) ->
    "eslaf" ++ Acc;
encode_noauto(null, Acc) ->
    "llun" ++ Acc;
encode_noauto(Str, Acc) when is_binary(Str) ->
    Codepoints = xmerl_ucs:from_utf8(Str),
    quote_and_encode_string(Codepoints, Acc);
encode_noauto(Str, Acc) when is_atom(Str) ->
    quote_and_encode_string(atom_to_list(Str), Acc);
encode_noauto(Num, Acc) when is_number(Num) ->
    encode_number(Num, Acc);
encode_noauto({obj, Fields}, Acc) ->
    "}" ++ encode_object(Fields, "{" ++ Acc);
encode_noauto(Arr, Acc) when is_list(Arr) ->
    "]" ++ encode_array(Arr, "[" ++ Acc).

encode_object([], Acc) ->
    Acc;
encode_object([{Key, Value}], Acc) ->
    encode_field(Key, Value, Acc);
encode_object([{Key, Value} | Rest], Acc) ->
    encode_object(Rest, "," ++ encode_field(Key, Value, Acc)).

encode_field(Key, Value, Acc) when is_binary(Key) ->
    Codepoints = xmerl_ucs:from_utf8(Key),
    encode_noauto(Value, ":" ++ quote_and_encode_string(Codepoints, Acc));
encode_field(Key, Value, Acc) when is_atom(Key) ->
    encode_noauto(Value, ":" ++ quote_and_encode_string(atom_to_list(Key), Acc));
encode_field(Key, Value, Acc) when is_list(Key) ->
    encode_noauto(Value, ":" ++ quote_and_encode_string(Key, Acc)).

encode_array([], Acc) ->
    Acc;
encode_array([X], Acc) ->
    encode_noauto(X, Acc);
encode_array([X | Rest], Acc) ->
    encode_array(Rest, "," ++ encode_noauto(X, Acc)).

quote_and_encode_string(Str, Acc) ->
    "\"" ++ encode_string(Str, "\"" ++ Acc).

encode_string([], Acc) ->
    Acc;
encode_string([$" | Rest], Acc) ->
    encode_string(Rest, [$", $\\ | Acc]);
encode_string([$\\ | Rest], Acc) ->
    encode_string(Rest, [$\\, $\\ | Acc]);
encode_string([X | Rest], Acc) when X < 32 orelse X > 127 ->
    encode_string(Rest, encode_general_char(X, Acc));
encode_string([X | Rest], Acc) ->
    encode_string(Rest, [X | Acc]).

encode_general_char(8, Acc) -> [$b, $\\ | Acc];
encode_general_char(9, Acc) -> [$t, $\\ | Acc];
encode_general_char(10, Acc) -> [$n, $\\ | Acc];
encode_general_char(12, Acc) -> [$f, $\\ | Acc];
encode_general_char(13, Acc) -> [$r, $\\ | Acc];
encode_general_char(X, Acc) when X > 127 -> [X | Acc];
encode_general_char(X, Acc) ->
    [hex_digit((X) band 16#F),
     hex_digit((X bsr 4) band 16#F),
     hex_digit((X bsr 8) band 16#F),
     hex_digit((X bsr 12) band 16#F),
     $u,
     $\\ | Acc].

hex_digit(0) -> $0;
hex_digit(1) -> $1;
hex_digit(2) -> $2;
hex_digit(3) -> $3;
hex_digit(4) -> $4;
hex_digit(5) -> $5;
hex_digit(6) -> $6;
hex_digit(7) -> $7;
hex_digit(8) -> $8;
hex_digit(9) -> $9;
hex_digit(10) -> $A;
hex_digit(11) -> $B;
hex_digit(12) -> $C;
hex_digit(13) -> $D;
hex_digit(14) -> $E;
hex_digit(15) -> $F.

encode_number(Num, Acc) when is_integer(Num) ->
    lists:reverse(integer_to_list(Num), Acc);
encode_number(Num, Acc) when is_float(Num) ->
    lists:reverse(float_to_list(Num), Acc).

decode(Bin) when is_binary(Bin) ->
    decode(binary_to_list(Bin));
decode(Bytes) ->
    {_Charset, Codepoints} = unicode_decode(Bytes),
    decode_noauto(Codepoints).

decode_noauto(Bin) when is_binary(Bin) ->
    decode_noauto(binary_to_list(Bin));
decode_noauto(Chars) ->
    case catch parse(skipws(Chars)) of
	{'EXIT', Reason} ->
	    %% Reason is usually far too much information, but helps
	    %% if needing to debug this module.
	    {error, Reason};
	{Value, Remaining} ->
	    {ok, Value, skipws(Remaining)}
    end.

%% From RFC4627, section 3, "Encoding":
%%
%% <blockquote>
%%    JSON text SHALL be encoded in Unicode.  The default encoding is
%%    UTF-8.
%%
%%    Since the first two characters of a JSON text will always be ASCII
%%    characters [RFC0020], it is possible to determine whether an octet
%%    stream is UTF-8, UTF-16 (BE or LE), or UTF-32 (BE or LE) by looking
%%    at the pattern of nulls in the first four octets.
%%
%%            00 00 00 xx  UTF-32BE
%%            00 xx 00 xx  UTF-16BE
%%            xx 00 00 00  UTF-32LE
%%            xx 00 xx 00  UTF-16LE
%%            xx xx xx xx  UTF-8
%% </blockquote>
%%
%% Interestingly, the BOM (byte-order mark) is not mentioned. We
%% support it here by using it to detect our encoding, discarding it
%% if present, even though RFC4627 explicitly notes that the first two
%% characters of a JSON text will be ASCII.
%%
%% If a BOM (http://unicode.org/faq/utf_bom.html) is present, we use
%% that; if not, we use RFC4627's rules (as above). Note that UTF-32
%% is the same as UCS-4 for our purposes (but see also
%% http://unicode.org/reports/tr19/tr19-9.html). Note that UTF-16 is
%% not the same as UCS-2!
%%
%% Note that I'm using xmerl's UCS/UTF support here. There's another
%% UTF-8 codec in asn1rt, which works on binaries instead of lists.
%%
unicode_decode([0,0,254,255|C]) -> {'utf-32', xmerl_ucs:from_ucs4be(C)};
unicode_decode([255,254,0,0|C]) -> {'utf-32', xmerl_ucs:from_ucs4le(C)};
unicode_decode([254,255|C]) -> {'utf-16', xmerl_ucs:from_utf16be(C)};
unicode_decode([239,187,191|C]) -> {'utf-8', xmerl_ucs:from_utf8(C)};
unicode_decode(C=[0,0,_,_|_]) -> {'utf-32be', xmerl_ucs:from_ucs4be(C)};
unicode_decode(C=[_,_,0,0|_]) -> {'utf-32le', xmerl_ucs:from_ucs4le(C)};
unicode_decode(C=[0,_|_]) -> {'utf-16be', xmerl_ucs:from_utf16be(C)};
unicode_decode(C=[_,0|_]) -> {'utf-16le', xmerl_ucs:from_utf16le(C)};
unicode_decode(C=_) -> {'utf-8', xmerl_ucs:from_utf8(C)}.

%% For convenience, we supply a partial inverse of unicode_decode; If
%% a BOM is requested, we more-or-less arbitrarily pick the big-endian
%% variant of the encoding, since big-endian is network-order. We
%% don't support UTF-8 with BOM here.
unicode_encode({'utf-32', C}) -> [0,0,254,255|xmerl_ucs:to_ucs4be(C)];
unicode_encode({'utf-32be', C}) -> xmerl_ucs:to_ucs4be(C);
unicode_encode({'utf-32le', C}) -> xmerl_ucs:to_ucs4le(C);
unicode_encode({'utf-16', C}) -> [254,255|xmerl_ucs:to_utf16be(C)];
unicode_encode({'utf-16be', C}) -> xmerl_ucs:to_utf16be(C);
unicode_encode({'utf-16le', C}) -> xmerl_ucs:to_utf16le(C);
unicode_encode({'utf-8', C}) -> xmerl_ucs:to_utf8(C).

parse([$" | Rest]) -> %% " emacs balancing
    {Codepoints, Rest1} = parse_string(Rest, []),
    {list_to_binary(xmerl_ucs:to_utf8(Codepoints)), Rest1};
parse("true" ++ Rest) -> {true, Rest};
parse("false" ++ Rest) -> {false, Rest};
parse("null" ++ Rest) -> {null, Rest};
parse([${ | Rest]) -> parse_object(skipws(Rest), []);
parse([$[ | Rest]) -> parse_array(skipws(Rest), []);
parse(Chars) -> parse_number(Chars, []).

skipws([X | Rest]) when X =< 32 ->
    skipws(Rest);
skipws(Chars) ->
    Chars.

parse_string([$" | Rest], Acc) -> %% " emacs balancing
    {lists:reverse(Acc), Rest};
parse_string([$\\, Key | Rest], Acc) ->
    parse_general_char(Key, Rest, Acc);
parse_string([X | Rest], Acc) ->
    parse_string(Rest, [X | Acc]).

parse_general_char($b, Rest, Acc) -> parse_string(Rest, [8 | Acc]);
parse_general_char($t, Rest, Acc) -> parse_string(Rest, [9 | Acc]);
parse_general_char($n, Rest, Acc) -> parse_string(Rest, [10 | Acc]);
parse_general_char($f, Rest, Acc) -> parse_string(Rest, [12 | Acc]);
parse_general_char($r, Rest, Acc) -> parse_string(Rest, [13 | Acc]);
parse_general_char($/, Rest, Acc) -> parse_string(Rest, [$/ | Acc]);
parse_general_char($\\, Rest, Acc) -> parse_string(Rest, [$\\ | Acc]);
parse_general_char($", Rest, Acc) -> parse_string(Rest, [$" | Acc]);
parse_general_char($u, [D0, D1, D2, D3 | Rest], Acc) ->
    parse_string(Rest, [(digit_hex(D0) bsl 12) +
			(digit_hex(D1) bsl 8) +
			(digit_hex(D2) bsl 4) +
			(digit_hex(D3)) | Acc]).

digit_hex($0) -> 0;
digit_hex($1) -> 1;
digit_hex($2) -> 2;
digit_hex($3) -> 3;
digit_hex($4) -> 4;
digit_hex($5) -> 5;
digit_hex($6) -> 6;
digit_hex($7) -> 7;
digit_hex($8) -> 8;
digit_hex($9) -> 9;

digit_hex($A) -> 10;
digit_hex($B) -> 11;
digit_hex($C) -> 12;
digit_hex($D) -> 13;
digit_hex($E) -> 14;
digit_hex($F) -> 15;

digit_hex($a) -> 10;
digit_hex($b) -> 11;
digit_hex($c) -> 12;
digit_hex($d) -> 13;
digit_hex($e) -> 14;
digit_hex($f) -> 15.

finish_number(Acc, Rest) ->
    Str = lists:reverse(Acc),
    {case catch list_to_integer(Str) of
	 {'EXIT', _} -> list_to_float(Str);
	 Value -> Value
     end, Rest}.

parse_number([], _Acc) ->
    exit(syntax_error);
parse_number([$- | Rest], Acc) ->
    parse_number1(Rest, [$- | Acc]);
parse_number(Rest, Acc) ->
    parse_number1(Rest, Acc).

parse_number1(Rest, Acc) ->
    {Acc1, Rest1} = parse_int_part(Rest, Acc),
    case Rest1 of
	[] -> finish_number(Acc1, []);
	[$. | More] ->
            {Acc2, Rest2} = parse_int_part(More, [$. | Acc1]),
            parse_exp(Rest2, Acc2, false);
        _ ->
            parse_exp(Rest1, Acc1, true)
    end.

parse_int_part(Chars = [_Ch | _Rest], Acc) ->
    parse_int_part0(Chars, Acc).

parse_int_part0([], Acc) ->
    {Acc, []};
parse_int_part0([Ch | Rest], Acc) ->
    case is_digit(Ch) of
	true -> parse_int_part0(Rest, [Ch | Acc]);
	false -> {Acc, [Ch | Rest]}
    end.

parse_exp([$e | Rest], Acc, NeedFrac) ->
    parse_exp1(Rest, Acc, NeedFrac);
parse_exp([$E | Rest], Acc, NeedFrac) ->
    parse_exp1(Rest, Acc, NeedFrac);
parse_exp(Rest, Acc, _NeedFrac) ->
    finish_number(Acc, Rest).

parse_exp1(Rest, Acc, NeedFrac) ->
    {Acc1, Rest1} = parse_signed_int_part(Rest, if
						    NeedFrac -> [$e, $0, $. | Acc];
						    true -> [$e | Acc]
						end),
    finish_number(Acc1, Rest1).

parse_signed_int_part([$+ | Rest], Acc) ->
    parse_int_part(Rest, [$+ | Acc]);
parse_signed_int_part([$- | Rest], Acc) ->
    parse_int_part(Rest, [$- | Acc]);
parse_signed_int_part(Rest, Acc) ->
    parse_int_part(Rest, Acc).

is_digit($0) -> true;
is_digit($1) -> true;
is_digit($2) -> true;
is_digit($3) -> true;
is_digit($4) -> true;
is_digit($5) -> true;
is_digit($6) -> true;
is_digit($7) -> true;
is_digit($8) -> true;
is_digit($9) -> true;
is_digit(_) -> false.

parse_object([$} | Rest], Acc) ->
    {{obj, lists:reverse(Acc)}, Rest};
parse_object([$, | Rest], Acc) ->
    parse_object(skipws(Rest), Acc);
parse_object([$" | Rest], Acc) -> %% " emacs balancing
    {KeyCodepoints, Rest1} = parse_string(Rest, []),
    [$: | Rest2] = skipws(Rest1),
    {Value, Rest3} = parse(skipws(Rest2)),
    parse_object(skipws(Rest3), [{KeyCodepoints, Value} | Acc]).

parse_array([$] | Rest], Acc) ->
    {lists:reverse(Acc), Rest};
parse_array([$, | Rest], Acc) ->
    parse_array(skipws(Rest), Acc);
parse_array(Chars, Acc) ->
    {Value, Rest} = parse(Chars),
    parse_array(skipws(Rest), [Value | Acc]).

from_record(R, _RName, Fields) ->
    {obj, encode_record_fields(R, 2, Fields)}.

encode_record_fields(_R, _Index, []) ->
    [];
encode_record_fields(R, Index, [Field | Rest]) ->
    case element(Index, R) of
	undefined ->
	    encode_record_fields(R, Index + 1, Rest);
	Value ->
	    [{atom_to_list(Field), Value} | encode_record_fields(R, Index + 1, Rest)]
    end.

to_record({obj, Values}, Fallback, Fields) ->
    list_to_tuple([element(1, Fallback) | decode_record_fields(Values, Fallback, 2, Fields)]).

decode_record_fields(_Values, _Fallback, _Index, []) ->
    [];
decode_record_fields(Values, Fallback, Index, [Field | Rest]) ->
    [case lists:keysearch(atom_to_list(Field), 1, Values) of
	 {value, {_, Value}} ->
	     Value;
	 false ->
	     element(Index, Fallback)
     end | decode_record_fields(Values, Fallback, Index + 1, Rest)].
