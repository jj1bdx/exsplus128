%% @author Kenji Rikitake <kenji.rikitake@acm.org>
%% @copyright 2014 Kenji Rikitake
%% @doc Xorshift128plus for Erlang
%% @end
%% (MIT License)
%% 
%% Copyright (c) 2014 Kenji Rikitake. All rights reserved.
%% 
%% Permission is hereby granted, free of charge, to any person obtaining a copy of
%% this software and associated documentation files (the "Software"), to deal in
%% the Software without restriction, including without limitation the rights to
%% use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies
%% of the Software, and to permit persons to whom the Software is furnished to do
%% so, subject to the following conditions:
%% 
%% The above copyright notice and this permission notice shall be included in all
%% copies or substantial portions of the Software.
%% 
%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
%% IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
%% FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
%% AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
%% LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
%% OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
%% SOFTWARE.
%% 

-module(exsplus).

-export([
     next_state/1,
     print_state/1,
     temper/1,
     test/0,
     test/2
 ]).

-export_type([state/0]).

%% @type uint64(). 64bit unsigned integer type.

-type uint64() :: 0..16#ffffffffffffffff.

-record(state, {s0 :: uint64(), s1 :: uint64()}).

%% @type state(). Internal state data type for exsplus.
%% Internally represented as the record <code>#state{}</code>,
%% of the 128bit seed.

-opaque state() :: #state{}.

-define(UINT64MASK, 16#ffffffffffffffff).

%% @doc Advance xorshift128plus state for one step.
%% Note: running temper function is required
%% to obtain the actual random number.

-spec next_state(state()) -> state().

next_state(R) ->
    S1 = R#state.s0,
    S0 = R#state.s1,
    S11 = (S1 bxor (S1 bsl 23)) band ?UINT64MASK,
    #state{s0 = S0, 
           s1 = S11 bxor S0 bxor (S11 bsr 17) bxor (S0 bsr 26)}.

%% @doc Generate 64bit unsigned integer from
%% the xorshift128plus internal state.

-spec temper(state()) -> uint64().

temper(R) ->
    (R#state.s0 + R#state.s1) band ?UINT64MASK.

-spec print_state(state()) -> ok.

print_state(R) ->
    io:format("s[0] = ~p s[1] = ~p~n",
               [R#state.s0, R#state.s1]).

-spec test(non_neg_integer(), state()) -> ok.

test(0, _) -> ok;
test(I, R) ->
    R1 = next_state(R),
    N = temper(R1),
    io:format("next = ~p ", [N]),
    print_state(R1),
    test(I - 1, R1).

-spec test() -> ok.

test() ->
    R = #state{s0 = 1234567890123456789, s1 = 9876543210987654321},
    print_state(R),  
    test(1000, R).

