-module(print_table).

%% API exports
-export([print/4]).

%%====================================================================
%% API functions
%%====================================================================
print(_,_,_,[]) -> ok;
print(Format,Header,Alignment,Table) when is_list(Format) ->
  print(list_to_tuple(string:split(Format,"|",all)),Header,Alignment,Table);
print(Format,Header,Alignment,Table) when is_list(Header) ->
  print(Format,list_to_tuple(string:split(Header,"|",all)),Alignment,Table);
print(Format,Header,Alignment,Table)
  when tuple_size(Header)=:=tuple_size(hd(Table)),
       tuple_size(Header)=:=tuple_size(Alignment),
       tuple_size(Header)=:=tuple_size(Format) ->
  FormattedTable = [Header | format_table(Format,Table)],
  FullAlignment = calculate_alignment(Alignment,FormattedTable),
  AlignedTable = align_table(FullAlignment,FormattedTable),
  FinalTable = remove_empty_columns(FullAlignment,AlignedTable),
  print_table(FinalTable).

%%====================================================================
%% Internal functions
%%====================================================================
fold_seq(Fun,InitialData,N)->
  lists:foldl(Fun,InitialData,lists:seq(1,N)).

format_table(Format,Table) ->
  N=tuple_size(Format),
  VerifiedTable=[Row || Row <- Table, tuple_size(Row)=:=N],
  FormatFun = fun(I,Tbl) ->
    [begin
      Fmt = element(I,Format),
      Field = element(I,Row),
      FormattedField = lists:flatten(io_lib:format(Fmt,[Field])),
      setelement(I,Row,FormattedField)
     end || Row <- Tbl]
  end,
  fold_seq(FormatFun,VerifiedTable,N).

calculate_alignment(Alignment,Table) ->
  N=tuple_size(hd(Table)),
  CalcAlignmentFun = fun(I,FullAlignment) ->
    ColumnAlignment = element(I,FullAlignment),
    ColumnWidth = lists:max([length(element(I,Row)) || Row <- Table]),
    setelement(I,FullAlignment,{ColumnAlignment,ColumnWidth})
  end,
  fold_seq(CalcAlignmentFun,Alignment,N).

align_table(Alignment,Table) ->
  N=tuple_size(Alignment),
  AlignFun = fun(I,Tbl) ->
    [begin
       Field = element(I,Row),
       {ColumnAlignment,ColumnWidth} = element(I,Alignment),
       WidthAlignment = ColumnWidth - length(Field),
       {RightAlignment,LeftAlignment} = calculate_field_alignment(ColumnAlignment,
                                                                  WidthAlignment),
       AlignedFiled = lists:duplicate(LeftAlignment,$ ) ++ Field ++
                      lists:duplicate(RightAlignment,$ ),
       setelement(I,Row,AlignedFiled)
     end || Row <- Tbl]
  end,
  fold_seq(AlignFun,Table,N).

calculate_field_alignment(ColumnAlingment, WidthAlignment) ->
  case ColumnAlingment of
    r -> {0,WidthAlignment};
    l -> {WidthAlignment,0};
    c ->
      RightAlignment = WidthAlignment div 2,
      LeftAlignment = WidthAlignment - RightAlignment,
      {RightAlignment,LeftAlignment}
  end.

remove_empty_columns(Alignment,Table) ->
  EmptyColumnsFun = fun(I,EmptyColumns) ->
    case element(I,Alignment) of
      {_,0} -> [I|EmptyColumns]; %% !columns listed in descending order
      {_,_} -> EmptyColumns
    end
  end,
  EmptyColumnsList = fold_seq(EmptyColumnsFun,[],tuple_size(Alignment)),
  RemoveColumnsFun = fun(I,Row) ->
    erlang:delete_element(I,Row)
  end,
  [lists:foldl(RemoveColumnsFun,Row,EmptyColumnsList) || Row <- Table].

print_table([Header|Table]) ->
  print_row(Header),
  print_delimiter(Header),
  [print_row(Row) || Row <- Table].

print_row(Row) ->
  io:format("|"),
  [io:format(" ~s |",[Field]) || Field<-tuple_to_list(Row)],
  io:format("~n").

print_delimiter(Row) ->
  io:format("|"),
  [begin
     ColumnWidth = length(Field) + 2,
     io:format("~" ++ integer_to_list(ColumnWidth) ++ "c|",[$-])
   end || Field<-tuple_to_list(Row)],
  io:format("~n").

