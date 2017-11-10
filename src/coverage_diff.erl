-module(coverage_diff).

%% API exports
-export([main/1]).

-define(URL(X),"https://coveralls.io/builds/" ++ X).

-record(file_meta,{
  file_name = "",
  coverage = 0.0, % coverage in percents ((relevant/covered)*100)
  total = 0,      % total line numbers in file
  relevant = 0,   % number of relevant code lines (potentinaly covarable)
  covered = 0,    % number of covered lines
  noncovered = 0  % (relevant-covered)
}).

-record(diff_entry,{
  file_name = "",
  coverage1 = 0.0,  % coverage in build1
  coverage2 = 0.0,  % coverage in build2
  d_coverage = 0.0, % coverege delta between build1 and build2 (coverage1-coverage2)
  d_relevant = 0,   % relevant lines No delta between build1 and build2
  d_noncovered = 0  % noncovered lines No delta between build1 and build2
}).

%%====================================================================
%% API functions
%%====================================================================

%% escript Entry point
main(Args) ->
  {ok, _} = application:ensure_all_started(inets),
  {ok, _} = application:ensure_all_started(ssl),
  io:format("~p:main(~p)~n", [?MODULE, Args]),
  [Build1, Build2] = parse_arguments(Args),
  FileList1 = get_file_list(Build1),
  FileList2 = get_file_list(Build2),
  Coverage1 = get_coverage_for_build(Build1, FileList1),
  Coverage2 = get_coverage_for_build(Build2, FileList2),
  print_coverage_diff({Build1,Coverage1},{Build2,Coverage2}).


%%====================================================================
%% Internal functions
%%====================================================================

parse_arguments(["--md"|T]) ->
  put(markdown,true),
  parse_arguments(T);
parse_arguments(["-c",CacheDir|T]) ->
  put(cache_dir,CacheDir),
  parse_arguments(T);
parse_arguments([_,_]=Builds) ->
  load_cache(Builds),
  Builds;
parse_arguments(Args) ->
  io:format("invalid arguments:~n~8c~p~n", [$ ,Args]),
  io:format("correct usage:~n~8c~p", [$ ,?MODULE]),
  io:format(" [-c cache_dir] [--md] build1 build2~n"),
  erlang:halt(1).

%%%%%%%%%%%%%%%%%%%%
%% cache functions
%%%%%%%%%%%%%%%%%%%%
load_cache(Builds)->
  [begin
     put(BuildID,[]),
     case get(cache_dir) of
       undefined -> ok;
       CacheDir ->
         CacheFile = cache_file(CacheDir,BuildID),
         case file:consult(CacheFile) of
           {ok,Data} -> put(BuildID,Data);
           _ -> ok
         end
     end
   end || BuildID <- Builds].

read_cache(BuildID,Key) ->
  CacheData = get(BuildID),
  case lists:keyfind(Key,1,CacheData) of
    false -> undefined;
    {Key,Value} -> Value
  end.

store_cache(BuildID,Key,Data) ->
  case get(cache_dir) of
    undefined -> ok;
    CacheDir ->
      CacheFile = cache_file(CacheDir,BuildID),
      IOData = io_lib:format("~n{~p, ~p}.", [Key,Data]),
      ok = file:write_file(CacheFile, IOData, [append])
  end.

cache_file(CacheDir,BuildID) ->
  CacheDir ++ "/" ++ BuildID ++ ".cache".

%%%%%%%%%%%%%%%%%%%%
%% wrappers
%%%%%%%%%%%%%%%%%%%%
http_request(URL)->
  case httpc:request(URL) of
    {ok,Resp} -> Resp;
    {error, socket_closed_remotely} ->
      io:format("Waiting 5s...~n"),
      timer:sleep(5000),
      http_request(URL)
  end.

jsx_decode(Binary,ErrorInfo)->
  try
    jsx:decode(Binary)
  catch
    _:Err ->
      io:format("jsx:decode/1 failed:~n~8c~p~n~8c~p~n~8c~p~n",
                [$ ,Err,$ ,ErrorInfo,$ ,erlang:get_stacktrace()]),
      erlang:halt(1)
  end.

%%%%%%%%%%%%%%%%%%%%

get_file_list(BuildID) ->
  Key=file_list,
  case read_cache(BuildID,Key) of
    undefined ->
      URL = ?URL(BuildID) ++ "/source_files.json",
      {_, _, FileListBody} = http_request(URL),
      PreFileList = jsx_decode(unicode:characters_to_binary(FileListBody),BuildID),
      FileList = [unicode:characters_to_list(proplists:get_value(<<"name">>, F))
                  || F <- jsx:decode(proplists:get_value(<<"source_files">>, PreFileList))],
      SortedFileList = lists:usort(FileList),
      store_cache(BuildID, Key, SortedFileList), SortedFileList;
    Cache -> Cache
  end.

%%%%%%%%%%%%%%%%%%%%

get_coverage_for_build(BuildID, FileList) ->
    [get_coverage_for_file(BuildID,F) || F <- FileList].

get_coverage_for_file(BuildID, FileName) ->
  case read_cache(BuildID,FileName) of
    undefined ->
      URL = ?URL(BuildID) ++ "/source.json?filename=" ++ http_uri:encode(FileName),
      {_, _, CoverageBody} = http_request(URL),
      PerLineData = jsx_decode(list_to_binary(CoverageBody),{BuildID,FileName}),
      {Total, Relevant, Covered} = lists:foldl(
        fun (null, {Total, Relevant, Covered}) ->
              {Total + 1, Relevant, Covered};
            (0, {Total, Relevant, Covered}) ->
              {Total + 1, Relevant + 1, Covered};
            (Hits, {Total, Relevant, Covered}) when is_integer(Hits) ->
              {Total + 1, Relevant + 1, Covered + 1}
        end, {0, 0, 0}, PerLineData),
      PercentCovered = 100 * case Relevant of
                               0 -> 0.0;
                               _ -> Covered / Relevant
                             end,
      NonCovered = Relevant - Covered,
      FileMeta = #file_meta{ file_name=FileName, coverage=PercentCovered, total=Total,
                             relevant=Relevant, covered=Covered, noncovered=NonCovered},
      store_cache(BuildID, FileName, FileMeta), FileMeta;
    Cache -> Cache
  end.

%%%%%%%%%%%%%%%%%%%%
print_coverage_diff({Build1,Coverage1},{Build2,Coverage2}) ->
  io:format("~n~80c~n~n", [$=]),
  print_total_coverage(Build1, Coverage1),
  print_total_coverage(Build2, Coverage2),
  print_noncovered_count_diff(Coverage1, Coverage2),
  io:format("~n~80c~n~n", [$=]),
  print_detailed_report({Build1,Coverage1},{Build2,Coverage2}),
  io:format("~n~80c~n~n", [$=]).

print_total_coverage(Name, Coverage) ->
  {TR, TC} =
  lists:foldl(fun(#file_meta{relevant=R,covered=C}, {TR, TC}) -> {TR + R, TC + C} end,
              {0, 0}, Coverage),
  io:format("Total coverage for ~s: ~.3f%~n", [Name, (TC/TR)*100]).

noncovered_count(Coverage) ->
  lists:foldl(fun(#file_meta{noncovered=N}, Acc) -> Acc + N end,
              0, Coverage).

print_noncovered_count_diff(Coverage1, Coverage2) ->
  NonCovered1 = noncovered_count(Coverage1),
  NonCovered2 = noncovered_count(Coverage2),
  io:format("Noncovered lines count delta: ~p~n", [NonCovered1 - NonCovered2]).

%%%%%%%%%%%%%%%%%%%%
print_detailed_report({Build1,Coverage1},{Build2,Coverage2}) ->
  {UniqFiles1,UniqFiles2,DiffTable} = generate_diff_table(Coverage1, Coverage2),
  print_diff_table(Build1,Build2,DiffTable),
  print_uniq_files("New",Build1,UniqFiles1),
  print_uniq_files("Removed",Build2,UniqFiles2).


generate_diff_table(Coverage1, Coverage2) ->
  generate_diff_table({[],[],[]},Coverage1, Coverage2).

generate_diff_table({U1,U2,DiffTbl},[],[]) ->
  {U1,U2,lists:keysort(#diff_entry.d_coverage,DiffTbl)};
generate_diff_table({U1,U2,DiffTbl},[],C2) ->
  generate_diff_table({U1,U2 ++ C2,DiffTbl},[],[]);
generate_diff_table({U1,U2,DiffTbl},C1,[]) ->
  generate_diff_table({U1 ++ C1,U2,DiffTbl},[],[]);
generate_diff_table({U1,U2,DiffTbl},[E1|T1],[E2|T2]) ->
  case {E1#file_meta.file_name,E2#file_meta.file_name} of
    {F1,F2} when F1<F2->
       generate_diff_table({[E1|U1],U2,DiffTbl},T1,[E2|T2]);
    {F1,F2} when F1>F2->
       generate_diff_table({U1,[E2|U2],DiffTbl},[E1|T1],T2);
    {F,F} ->
       #file_meta{coverage=C1, noncovered=N1, relevant=R1} = E1,
       #file_meta{coverage=C2, noncovered=N2, relevant=R2} = E2,
       Diff=#diff_entry{ file_name=F, coverage1=C1, coverage2=C2,
                         d_coverage=(C1-C2), d_relevant=(R1-R2),
                         d_noncovered=(N1-N2)},
       generate_diff_table({U1,U2,[Diff|DiffTbl]},T1,T2)
  end.

%%%%%%%%%%%%%%%%%%%%
print_diff_table(Build1,Build2,CoverageDiffTable) ->
  %% filter out files with increased or remained the same coverage
  DiffTbl = [D || #diff_entry{d_coverage=Cd}=D <- CoverageDiffTable, Cd<0],
  case get(markdown) of
    true -> print_md_diff_table(Build1,Build2,DiffTbl);
    _    -> print_txt_diff_table(DiffTbl)
  end.

print_txt_diff_table(DiffTable)->
  DiffTbl = [ D#diff_entry{file_name=filename:basename(F)}
              || #diff_entry{file_name=F}=D <- DiffTable],
  print_table:print( "~i|~s|~.3f%|~.3f%|~.3f%|~p|~p",
                     "|File|Cov1 |Cov2 |dCov |dR|dN",
                     {r,l,r,r,r,r,r}, DiffTbl).

print_md_diff_table(Build1,Build2,DiffTable) ->
  io:format("coverage decrease analysis between [~s](~s) and [~s](~s)~n~n",
            [Build1,?URL(Build1),Build2,?URL(Build2)]),
  DiffTbl = [ begin
                C1MD=md_file_url("~.3f%",[C1],Build1,F),
                C2MD=md_file_url("~.3f%",[C2],Build2,F),
                {filename:basename(F), C1MD , C2MD, Cd, Rd, Nd}
              end || #diff_entry{ file_name=F, coverage1=C1, coverage2=C2, d_coverage=Cd,
                                  d_relevant=Rd, d_noncovered=Nd} <- DiffTable],
  print_table:print( "~s|~s|~s|~.3f%|~p|~p",
                     "File|Cov1|Cov2|dCov|dR|dN",
                     {n,n,n,n,n,n}, DiffTbl).

md_file_url(Format,Args,Build,File)->
  lists:flatten(io_lib:format( "[" ++ Format ++ "](~s/source?filename=~s)",
                               Args ++ [?URL(Build),http_uri:encode(File)])).

%%%%%%%%%%%%%%%%%%%%
print_uniq_files(_,_,[]) -> ok;
print_uniq_files(Label, BuildID, FileList) ->
  io:format("~n~80c~n~n", [$=]),
  case get(markdown) of
    true -> print_md_uniq_files(Label, BuildID, FileList);
    _    -> print_txt_uniq_files(Label,FileList)
  end.

print_txt_uniq_files(Label, FileList) ->
  Files = [ E#file_meta{file_name=filename:basename(F)}
            || #file_meta{file_name=F}=E <- FileList],
  print_table:print( "~i|~s|~.3f%|~i|~p|~i|~p",
                     "|"++Label++" files|Coverage||Rel||Non",
                     {r,l,r,r,r,r,r}, Files).

print_md_uniq_files(Label, BuildID, FileList) ->
  Files = [ {md_file_url("~s",[filename:basename(F)],BuildID,F),C,R,N}
            || #file_meta{file_name=F, coverage=C, relevant=R, noncovered=N} <- FileList],
  print_table:print( "~s|~.3f%|~p|~p",
                     Label++" files|Coverage|Rel|Non",
                     {n,n,n,n}, Files).

