:- module(prolog_indent,
	  [ indent/2			% +In, +Out
	  ]).
:- use_module(library(prolog_source)).
:- use_module(library(record)).

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

:- record
	clause(term,
	       variables,
	       layout,
	       comment).

indent_streams(In, Out) :-
	read_src(In, Clause),
	indent(Clause, In, Out).

indent(Clause, _, _) :-
	clause_term(Clause, end_of_file), !.
indent(Clause, In, Out) :-
	indent_1(Out, Clause),
	read_src(In, Cl2),
	indent(Cl2, In, Out).

indent_1(Out, Clause) :-
	clause_layout(Clause, Layout),
	arg(1, Layout, StartClause),
	clause_comment(Clause, Comment),
	leading_comments(Comment, StartClause, _RestComments, Out),
	clause_variables(Clause, Vars),
	bind_vars(Vars),
	clause_term(Clause, Term),
	portray_clause(Out, Term).

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

read_src(In, Clause) :-
	prolog_read_source_term(In, Term, _Expanded,
				[ variable_names(Vars),
				  subterm_positions(Layout),
				  comments(Comment)
				]),
	make_clause([ term(Term),
		      variables(Vars),
		      layout(Layout),
		      comment(Comment)
		    ], Clause).

