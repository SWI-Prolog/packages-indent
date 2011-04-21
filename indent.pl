:- module(prolog_indent,
	  [ indent/2			% +In, +Out
	  ]).
:- use_module(library(prolog_source)).
:- use_module(library(listing)).
:- use_module(library(record)).
:- use_module(library(apply)).

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
	clause(term,			% The term itself
	       variables,		% its variables
	       input,			% source-stream
	       start,			% stream-position at start
	       layout,			% complete layout
	       comment).		% list of comments

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

indent_clause(Out, Clause0) :-
	leading_comments(Clause0, Clause1, Out),
	annotate_clause(Clause1, Clause),
	clause_variables(Clause, Vars),
	bind_vars(Vars),
	clause_term(Clause, Term),
	portray_clause(Out, Term, [portray_goal(indent_portray)]).

leading_comments(ClauseIn, ClauseOut, Out) :-
	clause_layout(ClauseIn, Layout),
	arg(1, Layout, StartClause),
	clause_comment(ClauseIn, Comment),
	leading_comments(Comment, StartClause, RestComments, Out),
	set_comment_of_clause(RestComments, ClauseIn, ClauseOut).


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

%%	annotate_clause(+ClauseIn, +In, -ClauseOut) is det.
%
%	Annotate ClauseIn with additional information such as particular
%	encodings of numbers or  the  use   of  strings.  This also adds
%	comments as annotations to the clause structure.

annotate_clause(ClauseIn, ClauseOut) :-
	clause_term(ClauseIn, Term),
	clause_layout(ClauseIn, Layout),
	clause_comment(ClauseIn, Comments),
	clause_source(ClauseIn, Source),
	annotate(Term, Layout, Comments, Source, TermOut),
	set_term_of_clause(TermOut, ClauseIn, ClauseOut).

annotate(String, Pos, _, Source, '$listing'(String, string(Text))) :-
	Pos = string_position(_,_), !,
	source_text(Source, Pos, Text).
annotate(Number, Pos, _, Source, '$listing'(Number, number(Text))) :-
	number(Number), !,
	source_text(Source, Pos, Text).
annotate(Primitive, _-_, _, _, Primitive) :- !.
annotate({}(Arg), brace_term_position(F,T,ArgPos), Comments, Source, TermOut) :-
	!,
	include(comment_in_range(F-T), Comments, EmbeddedComments),
	annotate(Arg, ArgPos, EmbeddedComments, Source, TermOut).
annotate(List, list_position(F,T,Elms,Tail), Comments, Source, ListOut) :- !,
	include(comment_in_range(F-T), Comments, EmbeddedComments),
	annotate_list(List, Elms, Tail, EmbeddedComments, Source, ListOut).
annotate(Term, term_position(F,T,_,_,ArgPos), Comments, Source, TermOut) :-
	functor(Term, Name, Arity),
	functor(TermOut, Name, Arity),
	include(comment_in_range(F-T), Comments, EmbeddedComments),
	annotate_args(1, Term, ArgPos, EmbeddedComments, Source, TermOut).

annotate_list([H|T], [PH|PT], TP, Comments, Source, [AH|AT]) :- !,
	annotate(H, PH, Comments, Source, AH),
	annotate_list(T, PT, TP, Comments, Source, AT).
annotate_list([], [], none, _, _, []) :- !.
annotate_list(Last, _, TP, Comments, Source, ALast) :-
	annotate(Last, TP, Comments, Source, ALast).

annotate_args(_, _, [], _, _, _) :- !.
annotate_args(I, Term, [PH|PT], Comments, Source, TermOut) :-
	arg(I, Term, A0),
	arg(I, TermOut, A),
	partition(comment_in_range(PH), Comments, A0Comments, RComments),
	annotate(A0, PH, A0Comments, Source, A1),
	(   PT = [PR|_]
	->  split_comments(RComments, PR, Now, RestComments)
	;   Now = RComments,
	    RestComments = []
	),
	tag_comment(Now, A1, A),
	succ(I, I2),
	annotate_args(I2, Term, PT, RestComments, Source, TermOut).

tag_comment([], Term, Term) :- !.
tag_comment(Comments, Term, '$comment'(Term, Comments)).

comment_in_range(Range, Pos-_) :-
	arg(1, Range, From),
	arg(2, Range, To),
	stream_position_data(char_count, Pos, Start),
	between(From, To, Start).

%%	split_comments(+Comments, +Pos, -Before, -After)
%
%	Split the set of Comments in a subset Before Pos and After Pos.

split_comments([], _, [], []).
split_comments([C0|CT], P, Before, After) :-
	C0 = Pos-_,
	stream_position_data(char_count, Pos, StartComment),
	arg(1, P, Here),
	(   StartComment < Here
	->  Before = [C0|BT],
	    split_comments(CT, P, BT, After)
	;   After = [C0|CT],
	    Before = []
	).


%%	source_text(+Source, +Pos, -Text:atom) is det.
%
%	Get the original source text for the range Pos.

source_text(Offset-Source, Pos, Text) :-
	arg(1, Pos, Start),
	arg(2, Pos, End),
	S is Start - Offset,
	L is End - Offset - S,
	sub_atom(Source, S, L, _, Text).

%%	clause_source(+Clause, -Source) is det.
%
%	Read the source-text for Clause from the original input
%
%	@param	Source is a term Offset-Text, where Offset is the
%		character position of the start and Text is an atom
%		representing the clause-text.

clause_source(ClauseIn, Start-Text) :-
	clause_input(ClauseIn, In),
	setup_call_cleanup(stream_property(In, position(Here)),
			   read_clause_text(ClauseIn, In, Start, Text),
			   set_stream_position(In, Here)).

read_clause_text(ClauseIn, In, StartCode, Text) :-
	clause_start(ClauseIn, Start),
	stream_position_data(char_count, Start, StartCode),
	clause_layout(ClauseIn, Layout),
	assertion(arg(1, Layout, StartCode)),
	arg(2, Layout, End),
	Count is End-StartCode,
	set_stream_position(In, Start),
	read_n_codes(Count, In, Codes),
	atom_codes(Text, Codes).

read_n_codes(N, In, [H|T]) :-
	succ(N2, N), !,
	get_code(In, H),
	read_n_codes(N2, In, T).
read_n_codes(_, _, []).

%%	indent_portray(+Term)
%
%	Use a local portray hook that   allows us to format annotations.
%	Note that for numbers,  strings,  etc.   we  can  choose between
%	emitting  the  original  token   or    generating   a   caonical
%	representation for the value.
%
%	@see '$put_quoted'/4 supports emitting escaped quoted strings.

:- public
	indent_portray/2.

indent_portray('$listing'(_String, string(Text)), _Options) :-
	write(Text).
indent_portray('$listing'(_Number, number(Text)), _Options) :-
	write(Text).
indent_portray('$comment'(Term, Comments), Options) :-
	memberchk(priority(Pri), Options),
	prolog_listing:pprint(current_output, Term, Pri, Options),
	print_comments(Comments).

print_comments([]).
print_comments([_Pos-Comment|T]) :-
	write(Comment),
	print_comments(T).


%%	read_src(+In, -Clause) is det.
%
%	Read the next  clause  from  the   input.  Clause  is  a  clause
%	`record', containing the term, its   variables,  its layout info
%	and comment.

read_src(In, Clause) :-
	prolog_read_source_term(In, Term, _Expanded,
				[ variable_names(Vars),
				  term_position(Start),
				  subterm_positions(Layout),
				  comments(Comment)
				]),
	make_clause([ term(Term),
		      variables(Vars),
		      input(In),
		      start(Start),
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





