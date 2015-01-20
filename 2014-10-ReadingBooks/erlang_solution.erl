-module(erlang_solution).
 
-export([find_most_used_words/1, find_pairs_consecutive_words/1]).
 
words(String) ->
  case  re:run(String, "\\w+", [global,{capture,first,list}]) of
      {match, Captures} ->
	  [string:to_lower(C) || [C] <-Captures];
      nomatch -> [] 
  end.
      
%% Build list of words in the order they appear
process_each_line(IoDevice, WordList) ->
    case io:get_line(IoDevice, "") of 
	eof->
	    file:close(IoDevice),
	    WordList;
	{error, Reason} ->
            file:close(IoDevice),
            throw(Reason);
        Data ->
            process_each_line(IoDevice, WordList ++ words(Data))
    end.


%% create list of words from file
create_word_list(Filename) ->
    case file:open(Filename, read) of
        {ok, IoDevice} ->
            process_each_line(IoDevice, []);
               
	{error, Reason} ->
			throw(Reason)
    end.


%% count occurrences of elements in list
count_items(List) ->
    Dict = lists:foldl(
                        fun(W, D) -> dict:update(W, fun(C) -> C + 1 end, 1, D) end, 
                        dict:new(), List),
    lists:reverse(lists:keysort(2, dict:to_list(Dict))).
    

%% create list of pairs of consecutive words
create_consecutive_pairs(List) ->
    create_consecutive_pairs(List,[]).
create_consecutive_pairs([X,Y|T],Acc) ->
    create_consecutive_pairs([Y|T],[{X,Y}|Acc]);
create_consecutive_pairs(_,Acc) ->
    lists:reverse(Acc).
    

%% Find the most frequently used words 
find_most_used_words(Filename) ->
      SortedWords = count_items(create_word_list(Filename)),
      io:format("~p~n",[lists:sublist(SortedWords,20)]).
	       
    
%% Find most frequently used pairs of consecutive words.
find_pairs_consecutive_words(Filename) -> 
    SortedConsecutive = count_items(create_consecutive_pairs(create_word_list(Filename))),
    io:format("~p~n",[lists:sublist(SortedConsecutive,20)]).
    
