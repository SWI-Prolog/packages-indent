:- module(prolog_indent,
	  [ indent/2			% +In, +Out
	  ]).
:- use_module(library(prolog_source)).
:- use_module(library(listing)).
:- use_module(library(record)).

/** <module> Prolog indent utility

This module implements reformatting of Prolog  source code comparable to
the Unix indent(1) utility for C.  It   operates  by reading the source,
using portray_clause/2 for formatting  the   result  and  re-inserts the
comments.

One of the design goals of this is module   is  to use the same code for
refactoring operations, such as changing   the arguments (order, delete,
add) of predicates. In that case we   read  the source, transform it and
write it out using the same layout logic.

@tbd	Reformat comments.  Maybe we can use the PlDoc Wiki parser here
	to extract the structure?
@tbd	Re-insert comments inside the clause.  This is probably the
	hardest part because the comment must be between the same two
	tokens as where it appeared originally.  We should also distingish
	comments aligned with the code and positioned right of the code.
@tbd	Preserve/insert special notation.  Some examples:

	    - Lists that must be written as "strings"
	    - Numbers written in different notations (including 0'x)
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

%%	leading_comments(+Comments, +StartClause, -InClauseComment, +Out)
%
%	Emit the comments that preceed the clause.
%
%	@param	StartClause is the character offset that starts the
%		clause
%	@tbd	(optionally) reformat the comments

leading_comments([], _, [], _) :- !.
leading_comments([Pos-Comment|Rest], StartClause, RestComments, Out) :-
	stream_position_data(char_count, Pos, StartComment),
	StartComment < StartClause, !,
	stream_position_data(line_position, Pos, LinePos),
	indent_to_column(Out, LinePos),
	format(Out, '~s~n~n', [Comment]),
	leading_comments(Rest, StartClause, RestComments, Out).
leading_comments(Comments, _, Comments, _).


bind_vars([]).
bind_vars([Name=Var|T]) :-
	Var = '$VAR'(Name),
	bind_vars(T).

%%	read_src(+In, -Clause) is det.
%
%	Read the next  clause  from  the   input.  Clause  is  a  clause
%	`record', containing the term, its   variables,  its layout info
%	and comment.

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

%%	indent_to_column(+Out, +Indent)
%
%	Indent to column Indent. Uses   the setting listing:tab_distance
%	to determine the mapping between tabs and spaces.

indent_to_column(Out, N) :-
	nl(Out),
	setting(listing:tab_distance, D),
	(   D =:= 0
	->  tab(Out, N)
	;   Tab is N // D,
	    Space is N mod D,
	    put_tabs(Out, Tab),
	    tab(Out, Space)
	).

put_tabs(Out, N) :-
	N > 0, !,
	put(Out, 0'\t),
	NN is N - 1,
	put_tabs(Out, NN).
put_tabs(_, _).





