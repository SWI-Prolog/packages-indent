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
	read_predicate(Clause, Clauses, Next, In),
	indent_predicate(Out, Clauses),
	indent(Next, In, Out).

%%	read_predicate(+Clause0, -Clauses, -NextClause, +In) is det.
%
%	Read the next predicate from the source

read_predicate(Clause0, [Clause0|Rest], Next, In) :-
	read_src(In, Clause1),
	(   same_pred(Clause0, Clause1)
	->  read_predicate(Clause1, Rest, Next, In)
	;   Next = Clause1,
	    Rest = []
	).

same_pred(Clause1, Clause2) :-
	clause_pred(Clause1, PI1),
	clause_pred(Clause2, PI2),
	canonical_pi(PI1, PI),
	canonical_pi(PI2, PI).

clause_pred(Clause, PI) :-
	clause_term(Clause, Term),
	term_pi(Term, PI).

%%	indent_predicate(+Out, +Clauses) is det.
%
%	Indent a single predicate.

indent_predicate(Out, Clauses) :-
	maplist(indent_clause(Out), Clauses),
	format(Out, '~n', []).

indent_clause(Out, Clause) :-
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

%%	term_pi(+Term, -PI)

term_pi(Head :- _, PI) :- !,
	head_pi(Head, PI).
term_pi(Head --> _, PI) :- !,
	dcg_head_pi(Head, PI).
term_pi(Head, PI) :-
	head_pi(Head, PI).

head_pi(M:Head, M:PI) :- !,
	plain_head_pi(Head, PI).
head_pi(Head, PI) :-
	plain_head_pi(Head, PI).

dcg_head_pi(M:Head, M:PI) :-
	dcg_plain_head_pi(Head, PI).
dcg_head_pi(Head, PI) :-
	dcg_plain_head_pi(Head, PI).

plain_head_pi(Head, Name/Arity) :-
	functor(Head, Name, Arity).
dcg_plain_head_pi(Head, Name//Arity) :-
	functor(Head, Name, Arity).

canonical_pi(M:PI0, M:PI) :- !,
	canonical_pi(PI0, PI).
canonical_pi(Name//Arity0, Name/Arity) :- !,
	Arity is Arity0 + 2.
canonical_pi(PI, PI).






