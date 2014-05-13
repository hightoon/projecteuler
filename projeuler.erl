%% projeuler.erl
%% solutions for problems on projecteuler.net

-module(projeuler).
-compile(export_all).
-export([ pow_dig_sum/2
        , is_prime/1
        , get_nth_prime/2
        , sum_primes_under_n/1
        , factorial_digit_sum/1
        , max_digit_product/3
        , read_file/1
        , number_letter_count/1
        , trinum_over_N_factors/2
        ]).


pow_dig_sum(Base, Pow) ->
    lists:foldl(fun(X, Sum) -> X - 48 + Sum end, 0,
                integer_to_list(round(math:pow(Base, Pow)))).

all_factors(Num) when Num > 0 ->
    [X || X <- lists:seq(1, trunc(math:sqrt(Num))),
     (Num rem X == 0) and (X /= Num) and (X /= 1)].

is_prime(1) -> true;
is_prime(Num) ->
    case all_factors(Num) of
        [] -> true;
        _ -> false
    end.

get_nth_prime(1, Start) ->
    case is_prime(Start) of
        true -> Start;
        false -> get_nth_prime(1, Start+1)
    end;
get_nth_prime(Nth, Start) ->
    case is_prime(Start) of
        true -> get_nth_prime(Nth-1, Start+1);
        false -> get_nth_prime(Nth, Start+1)
    end.

sum_primes_under_n(N) ->
    lists:foldl(fun(X, Sum) -> X + Sum end, 0, [X || X <- lists:seq(1, N), is_prime(X) == true]).

factorial(N) -> lists:foldl(fun(X, Product) -> Product*X end, 1, [X || X <- lists:seq(1, N)]).

factorial_digit_sum(N) ->
    lists:foldl(fun(X, Sum) -> X - 48 + Sum end, 0, integer_to_list(factorial(N))).

digit_products(Digits, NumOfSerials, Pos) ->
    case NumOfSerials == length(Digits) - Pos + 1 of
        true -> [lists:foldl(fun(X, P) -> X*P end, 1, [X - 48 || X <- lists:sublist(Digits, Pos, NumOfSerials)])];
        false ->
          [lists:foldl(fun(X, P) -> X*P end, 1, [X - 48 || X <- lists:sublist(Digits, Pos, NumOfSerials)])] ++
          digit_products(Digits, NumOfSerials, Pos+1)
    end.

max_digit_product(Digits, NumOfSerials, Pos) -> lists:max(digit_products(Digits, NumOfSerials, Pos)).

read_file(Filename) ->
    case file:open(Filename, [read]) of
        {ok, Fd} -> sum_num_in_line(Fd, 0, file:read_line(Fd));
        {error, Reason} -> {error, Reason}
    end.

sum_num_in_line(Fd, Sum, {ok, Data}) ->
    sum_num_in_line(Fd, Sum + list_to_integer(lists:sublist(Data, 1, length(Data)-1)), file:read_line(Fd));
sum_num_in_line(Fd, Sum, eof) ->
    file:close(Fd),
    lists:sublist(integer_to_list(Sum), 1, 10);
sum_num_in_line(Fd, _Sum, {error, Reason}) ->
    file:close(Fd),
    {error, Reason}.

%% functions for "number letter counts" problem
number_count_under_twenty(N) ->
    case N of
        0 -> 0;
        1 -> length("one");
        2 -> length("two");
        3 -> length("Three");
        4 -> length("Four");
        5 -> length("Five");
        6 -> length("Six");
        7 -> length("Seven");
        8 -> length("Eight");
        9 -> length("Nine");
        10 -> length("Ten");
        11 -> length("eleven");
        12 -> length("twelve");
        13 -> length("thirteen");
        14 -> length("fourteen");
        15 -> length("fifteen");
        16 -> length("sixteen");
        17 -> length("seventeen");
        18 -> length("eighteen");
        19 -> length("nineteen")
    end.

tens_letter_count_over_twenty(N) ->
    case N of
        2 -> length("twenty");
        3 -> length("thirty");
        4 -> length("forty");
        5 -> length("fifty");
        6 -> length("sixty");
        7 -> length("seventy");
        8 -> length("eighty");
        9 -> length("ninety")
    end.

hundred_letter_count(N) -> number_count_under_twenty(N) + length("hundred") + length("and").

number_letter_count([]) -> 0;
number_letter_count([X|Xs]) ->
    if X < 20 -> number_count_under_twenty(X) + number_letter_count(Xs)
    ; X < 100 -> tens_letter_count_over_twenty(X div 10) +
                 number_count_under_twenty(X rem 10) +
                 number_letter_count(Xs)
    ; X == 1000 -> 11 + number_letter_count(Xs)
    ; X rem 100 < 20 -> hundred_letter_count(X div 100) +
                        number_count_under_twenty(X rem 100) +
                        number_letter_count(Xs)
    ; X rem 100 >= 20 -> hundred_letter_count(X div 100) +  tens_letter_count_over_twenty((X rem 100) div 10) +
                         number_count_under_twenty(X rem 10) + number_letter_count(Xs)
    end.

%% functions for handling problem 12
triangle_num(0) -> 0;
triangle_num(N) -> lists:foldl(fun (X, Sum) -> X + Sum end,
                               0,
                               lists:seq(1, N)).

%% another way to get triangle number
%% 1 + 2 + ... + N == N*(N+1)/2
triangle_num_alt(N) -> (N * (N+1)) bsr 1.

%% very inefficient
factors(N) -> [X || X <- lists:seq(1, (N+1) div 2), N rem X == 0] ++ [N].

trinum_over_N_factors(Start, N) ->
    NumOfFactor = length(all_factors(triangle_num_alt(Start))) * 2 + 2,
    if NumOfFactor > N -> triangle_num_alt(Start);
       true -> trinum_over_N_factors(Start+1, N)
    end.

alpha_value([]) -> 0;
alpha_value([X|Xs]) -> X - $A + 1 + alpha_value(Xs).
