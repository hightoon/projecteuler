-module(projeuler).
-export([is_prime/1, get_nth_prime/2, sum_primes_under_n/1, factorial_digit_sum/1]).


pow_dig_sum(Base, Pow) ->
    lists:foldl(fun(X, Sum) -> X - 48 + Sum end, 0, 
                integer_to_list(round(math:pow(Base, Pow)))).
                
all_factors(Num) when Num >= 4 -> [X || X <- lists:seq(2, round(math:sqrt(Num))), Num rem X == 0];
all_factors(1) -> [1];
all_factors(_) -> [].

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



