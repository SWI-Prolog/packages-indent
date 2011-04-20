:- module(prolog_indent,
	  [ indent/2			% +In, +Out
	  ]).
:- use_module(library(prolog_source)).

/** <module> Prolog indent utility

*/

%%	indent(+FileIn, -FileOut) is det.
%
%	Reformat Prolog source-code.

indent(FileIn, FileOut) :-
	setup_call_cleanup(open(FileIn, read, In),
			   indent_sf(In, FileOut),
			   close(In)).
indent_sf(In, FileOut) :-
	setup_call_cleanup(open(FileOut, write, Out),
			   indent_streams(In, Out),
			   close(Out)).

indent_streams(In, Out) :-
	read(In, Clause, Vars, Layout, Comment),
	indent(Clause, Vars, Layout, Comment, In, Out).

indent(end_of_file, _, _, _, _, _) :- !.
indent(Clause, Vars, Layout, Comment, In, Out) :-
	indent_1(Out, Clause, Vars, Layout, Comment),
	read(In, Cl2, V2, L2, Cmt2),
	indent(Cl2, V2, L2, Cmt2, In, Out).

indent_1(Out, Clause, Vars, Layout, Comment) :-
	arg(1, Layout, StartClause),
	leading_comments(Comment, StartClause, _RestComments, Out),
	bind_vars(Vars),
	portray_clause(Out, Clause).

leading_comments([], _, [], _) :- !.
leading_comments([Pos-Comment|Rest], StartClause, RestComments, Out) :-
	stream_position_data(char_count, Pos, StartComment),
	StartComment < StartClause, !,
	format(Out, '~s~n~n', [Comment]),
	leading_comments(Rest, StartClause, RestComments, Out).
leading_comments(Comments, _, Comments, _).


bind_vars([]).
bind_vars([Name=Var|T]) :-
	Var = '$VAR'(Name),
	bind_vars(T).

read(In, Clause, Vars, Layout, Comment) :-
	prolog_read_source_term(In, Clause, _Expanded,
				[ variable_names(Vars),
				  subterm_positions(Layout),
				  comments(Comment)
				]).
