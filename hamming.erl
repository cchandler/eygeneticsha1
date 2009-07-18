

-module(hamming).

-export([hamming/2]).

% hamming(A,B) -> 
%     X = [[X] || X <- A],
%     Y = [[Y] || Y <- B],
%     hamming(X,Y).

hamming([],[]) ->
    0;
hamming([H1|T1],[H2|T2]) -> 
    case H1 == H2 of
        true ->
            % io:format("Something",[]),
            0 + hamming(T1,T2);
        _ ->
            1 + hamming(T1,T2)
    end.