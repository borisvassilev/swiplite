/* Copyright (C) 2024 Boris Vassilev <boris.vassilev@gmail.com>

This program is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program.  If not, see <https://www.gnu.org/licenses/>.
*/
:- module(test_sqlite, [test_sqlite/0]).
:- use_module(library(plunit)).
:- use_module(library(sqlite)).

test_sqlite :-
    run_tests([ connection,
                statement,
                evaluate ]).

:- begin_tests(connection).

test(open_nonvar, [error(uninstantiation_error(not_a_var))]) :-
    sqlite_open('foo.db', not_a_var, [memory(true)]).

test(open_default, [
        setup(( sqlite_open('foo.db', X, [mode(create)]),
                sqlite_close(X) )),
        cleanup(delete_file('foo.db')) ]) :-
    sqlite_open('foo.db', DB, []),
    sqlite_close(DB).

test(open_close_atom, [
        cleanup(delete_file('foo.db')) ]) :-
    sqlite_open('foo.db', DB, [mode(create)]),
    sqlite_close(DB).

test(open_close_string, [
        cleanup(delete_file('foo.db')) ]) :-
    sqlite_open("foo.db", DB, [mode(create)]),
    sqlite_close(DB).

test(open_close_codes, [
        cleanup(delete_file('foo.db')) ]) :-
    sqlite_open(`foo.db`, DB, [mode(create)]),
    sqlite_close(DB).

test(open_number, [error(type_error(text,42))]) :-
    sqlite_open(42, _, []).

test(open_term, [error(type_error(text, foo(bar)))]) :-
    sqlite_open(foo(bar), _, []).

create_noaccess_file(File) :-
    setup_call_cleanup(open(File, write, F), true, close(F)),
    chmod(File, -rwx).

test(open_no_permission, [
        setup(create_noaccess_file('locked.db')),
        error(permission_error(open, _, 'locked.db'), _),
        cleanup(delete_file('locked.db')) ]) :-
    sqlite_open('locked.db', _, []).

test(open_nonexistant_read, [
        error(permission_error(open,_,'foo.db'))]) :-
    sqlite_open('foo.db', _, [mode(read)]).

test(open_nonexistant_write, [
        error(permission_error(open,_,'foo.db'))]) :-
    sqlite_open('foo.db', _, [mode(write)]).

test(open_memory, [
        setup(\+ exists_file(foo)) ]) :-
    sqlite_open(foo, DB, [memory(true)]),
    sqlite_close(DB),
    \+ exists_file(foo).

test(open_bad_mode, [error(domain_error(_,bad_mode))] ) :-
    sqlite_open(foo, _, [mode(bad_mode)]).

test(open_bad_threaded, [error(domain_error(_,bad_threaded))] ) :-
    sqlite_open(foo, _, [threaded(bad_threaded)]).

test(close_not_connection) :-
    sqlite_open(foo, DB, [memory(true)]),
    catch(sqlite_close(some_atom),
        error(type_error('sqlite_connection', some_atom), _),
        sqlite_close(DB)).

test(prepare_nonvar, [
        setup(sqlite_open(foo, DB, [memory(true)])),
        cleanup(sqlite_close(DB)),
        error(uninstantiation_error(not_a_var))] ) :-
    sqlite_prepare(DB, "select 1;", not_a_var).

test(prepare_not_connection, [error(type_error(sqlite_connection,10))]) :-
    sqlite_prepare(10, "select 1;", _Stmt).

:- end_tests(connection).

:- begin_tests(statement).

test(prepare_not_text, [
        setup(sqlite_open(foo, DB, [memory(true)])),
        cleanup(sqlite_close(DB)),
        error(type_error(text,10)) ]) :-
    sqlite_prepare(DB, 10, _Stmt).

test(prepare_bad_stmt, [
        setup(sqlite_open(foo, DB, [memory(true)])),
        cleanup(sqlite_close(DB)),
        error(sqlite_error(sqlite_prepare,_,_,_), _) ]) :-
    sqlite_prepare(DB, "this isn't SQL", _Stmt).

test(prepare_badly_numbered, [
        setup(sqlite_open(foo, DB, [memory(true)])),
        cleanup(sqlite_close(DB)),
        error(swiplite_error(_,'anonymous or missing ?NNN parameter')) ]) :-
    sqlite_prepare(DB, 'select ?2,?3,?4', _).

test(finalize_not_a_blob, [
        setup(sqlite_open(foo, DB, [memory(true)])),
        cleanup(sqlite_close(DB)),
        error(type_error(sqlite_statement, foo)) ]) :-
    sqlite_finalize(foo).

test(finalize_not_a_statement, [
        setup(sqlite_open(foo, DB, [memory(true)])),
        cleanup(sqlite_close(DB)),
        error(type_error(sqlite_statement, DB)) ]) :-
    sqlite_finalize(DB).

test(finalize_finalized, [
        setup(sqlite_open(foo, DB, [memory(true)])),
        cleanup(sqlite_close(DB)),
        error(existence_error(sqlite_statement, Stmt)) ]) :-
    sqlite_prepare(DB, "select 1", Stmt),
    sqlite_finalize(Stmt),
    sqlite_finalize(Stmt).

test(bind_not_statement, [
        setup((
            sqlite_open(foo, DB, [memory(true)]),
            sqlite_prepare(DB, 'select ?1', S) )),
        cleanup((
            sqlite_finalize(S),
            sqlite_close(DB) )),
        error(type_error(_, DB)) ]) :-
    sqlite_bind(DB, bv(1)).

test(bind_closed_statement, [
        setup((
            sqlite_open(foo, DB, [memory(true)]),
            sqlite_prepare(DB, 'select ?1', S) )),
        cleanup((
            sqlite_close(DB) )),
        error(existence_error(_, S)) ]) :-
    sqlite_finalize(S),
    sqlite_bind(S, bv(1)).

test(bind, [
        setup((
            sqlite_open(foo, DB, [memory(true)]),
            sqlite_prepare(DB, 'select ?1,?2,?3,?4,?5,?6', S) )),
        cleanup((
            sqlite_finalize(S),
            sqlite_close(DB) )),
        true([
            SQL == 'select ?1,?2,?3,?4,?5,?6',
            SQLE == "select 1,2.2,NULL,'foo','bar','baz'" ]) ]) :-
    sqlite_bind(S, bv(1,2.2,[],"foo",bar,`baz`)),
    sqlite_sql(S, SQL),
    sqlite_expanded_sql(S, SQLE).

test(bind_numbered, [
        setup((
            sqlite_open(foo, DB, [memory(true)]),
            sqlite_prepare(DB, 'select ?2,?1,?1,?2', S) )),
        cleanup((
            sqlite_finalize(S),
            sqlite_close(DB) )),
        true([
            SQL == 'select ?2,?1,?1,?2',
            SQLE == "select 2.2,1,1,2.2" ]) ]) :-
    sqlite_bind(S, bv(1,2.2)),
    sqlite_sql(S, SQL),
    sqlite_expanded_sql(S, SQLE).

test(bind_unicode, [
        setup((
            sqlite_open(foo, DB, [memory(true)]),
            sqlite_prepare(DB, 'select ?1,?2,?3', S) )),
        cleanup((
            sqlite_finalize(S),
            sqlite_close(DB) )),
        true([
            Cols == cols('?1','?2','?3'),
            SQL == 'select ?1,?2,?3',
            SQLE == "select 'щип','щибиди дип','дип'" ]) ]) :-
    sqlite_column_names(S, Cols),
    sqlite_bind(S, bv(`щип`, 'щибиди дип', "дип")),
    sqlite_sql(S, SQL),
    sqlite_expanded_sql(S, SQLE).

test(bind_bad_bv_name, [
        setup((
            sqlite_open(foo, DB, [memory(true)]),
            sqlite_prepare(DB, 'select ?1', S) )),
        cleanup((
            sqlite_finalize(S),
            sqlite_close(DB) )),
        error(type_error(_,v(1))) ]) :-
    sqlite_bind(S, v(1)).

test(bind_too_few_bv, [
        setup((
            sqlite_open(foo, DB, [memory(true)]),
            sqlite_prepare(DB, 'select ?1,?2', S) )),
        cleanup((
            sqlite_finalize(S),
            sqlite_close(DB) )),
        error(type_error(_,bv(1))) ]) :-
    sqlite_bind(S, bv(1)).

test(bind_too_many_bv, [
        setup((
            sqlite_open(foo, DB, [memory(true)]),
            sqlite_prepare(DB, 'select ?1', S) )),
        cleanup((
            sqlite_finalize(S),
            sqlite_close(DB) )),
        error(type_error(_,bv(1,2))) ]) :-
    sqlite_bind(S, bv(1,2)).

test(bind_unsupported_type_large_int, [
        setup((
            sqlite_open(foo, DB, [memory(true)]),
            sqlite_prepare(DB, 'select ?1', S) )),
        cleanup((
            sqlite_finalize(S),
            sqlite_close(DB) )),
        forall( X is 2^63 ; X is -(2^63+1) ),
        error(representation_error(int64_t)) ]) :-
    sqlite_bind(S, bv(X)).

test(bind_unsupported_type_compound, [
        setup((
            sqlite_open(foo, DB, [memory(true)]),
            sqlite_prepare(DB, 'select ?1', S) )),
        cleanup((
            sqlite_finalize(S),
            sqlite_close(DB) )),
        error(type_error(_,1-2)) ]) :-
    sqlite_bind(S, bv(1-2)).

test(bind_unsupported_type_list, [
        setup((
            sqlite_open(foo, DB, [memory(true)]),
            sqlite_prepare(DB, 'select ?1', S) )),
        cleanup((
            sqlite_finalize(S),
            sqlite_close(DB) )),
        error(type_error(_,-1)) ]) :-
    sqlite_bind(S, bv([-1])).

test(bind_unsupported_type_blob, [
        setup((
            sqlite_open(foo, DB, [memory(true)]),
            sqlite_prepare(DB, 'select ?1', S) )),
        cleanup((
            sqlite_finalize(S),
            sqlite_close(DB) )),
        error(type_error(_,S)) ]) :-
    sqlite_bind(S, bv(S)).

test(bind_unsupported_type_dict, [
        setup((
            sqlite_open(foo, DB, [memory(true)]),
            sqlite_prepare(DB, 'select ?1', S) )),
        cleanup((
            sqlite_finalize(S),
            sqlite_close(DB) )),
        error(type_error(_,_{})) ]) :-
    sqlite_bind(S, bv(_{})).

test(bind_unsupported_type_rational, [
        setup((
            sqlite_open(foo, DB, [memory(true)]),
            sqlite_prepare(DB, 'select ?1', S) )),
        cleanup((
            sqlite_finalize(S),
            sqlite_close(DB) )),
        error(type_error(_,1r2)) ]) :-
    sqlite_bind(S, bv(1r2)).

test(colnames_unicode, [
        setup((
            sqlite_open(foo, DB, [memory(true)]),
            sqlite_prepare(DB, "select 'щип','щибиди дип','дип'", S) )),
        cleanup((
            sqlite_finalize(S),
            sqlite_close(DB) )),
        true([
            Cols == cols('\'щип\'','\'щибиди дип\'','\'дип\'')
             ]) ]) :-
    sqlite_column_names(S, Cols).

test(colnames_expr, [
        setup((
            sqlite_open(foo, DB, [memory(true)]),
            sqlite_prepare(DB, "select 1+2 as x, 2*3, substr('ana', 'banana')", S) )),
        cleanup((
            sqlite_finalize(S),
            sqlite_close(DB) )),
        true(Cols == cols(x,'2*3','substr(\'ana\', \'banana\')')) ]) :-
    sqlite_column_names(S, Cols).

test(colnames_none, [
        setup((
            sqlite_open(foo, DB, [memory(true)]),
            sqlite_prepare(DB, "Create table x ( y number )", S) )),
        cleanup((
            sqlite_finalize(S),
            sqlite_close(DB) )),
        true(Cols == cols()) ]) :-
    sqlite_column_names(S, Cols).

:- end_tests(statement).

:- begin_tests(evaluate).

test(create_insert, [
        setup((
            sqlite_open(foo, DB, [mode(create),memory(true)]),
            sqlite_prepare(DB, 'create table x ( y number )', S) )),
        cleanup((
            sqlite_finalize(S),
            sqlite_close(DB) )) ]) :-
    sqlite_eval(S),
    setup_call_cleanup(
        sqlite_prepare(DB, 'insert into x values ( 1 )', I),
        sqlite_eval(I),
        sqlite_finalize(I)).

test(select_noresult, [
        setup((
            sqlite_open(foo, DB, [memory(true)]),
            sqlite_prepare(DB, 'select 1', S) )),
        cleanup((
            sqlite_finalize(S),
            sqlite_close(DB) )),
        error(swiplite_error(_, 'non-empty result set'),_) ]) :-
    sqlite_eval(S).

test(select_nocols, [
        setup((
            sqlite_open(foo, DB, [memory(true)]),
            sqlite_prepare(DB, 'create table x ( y number )', S) )),
        cleanup((
            sqlite_finalize(S),
            sqlite_close(DB) )),
        error(swiplite_error(_, 'no columns in result set'),_) ]) :-
    sqlite_eval(S, _).

test(select_norows, [
        setup((
            sqlite_open(foo, DB, [memory(true)]),
            sqlite_prepare(DB, 'select 1 as x where x > 2', S) )),
        cleanup((
            sqlite_finalize(S),
            sqlite_close(DB) )),
        error(swiplite_error(_, 'no rows in result set'),_) ]) :-
    sqlite_eval(S, _).

test(select_one, [
        setup((
            sqlite_open(foo, DB, [memory(true)]),
            sqlite_prepare(DB, "select 1", S) )),
        cleanup((
            sqlite_finalize(S),
            sqlite_close(DB) )),
        true(R == row(1)) ]) :-
    sqlite_eval(S, R).

test(select_one_instantiated, [
        setup((
            sqlite_open(foo, DB, [memory(true)]),
            sqlite_prepare(DB, "select 1", S) )),
        cleanup((
            sqlite_finalize(S),
            sqlite_close(DB) )) ]) :-
    sqlite_eval(S, row(1)).

test(select_one_wrong, [
        setup((
            sqlite_open(foo, DB, [memory(true)]),
            sqlite_prepare(DB, "select 1", S) )),
        cleanup((
            sqlite_finalize(S),
            sqlite_close(DB) )),
        fail ]) :-
    sqlite_eval(S, row(2)).

test(select_one_cols, [
        setup((
            sqlite_open(foo, DB, [memory(true)]),
            sqlite_prepare(DB, "select NULL, 1, 2.2, '3,четири'", S) )),
        cleanup((
            sqlite_finalize(S),
            sqlite_close(DB) )),
        true(R == row([], 1, 2.2, "3,четири")) ]) :-
    sqlite_eval(S, R).

test(reeval, [
        setup((
            sqlite_open('foo.db', DB, [mode(create)]),
            sqlite_prepare(DB, "Create table x ( y number )", Create),
            sqlite_eval(Create),
            sqlite_finalize(Create),
            sqlite_prepare(DB, "Begin", Begin),
            sqlite_prepare(DB, "Insert into x values ( ?1 )", Insert),
            sqlite_prepare(DB, "End", End),
            sqlite_prepare(DB, "Select min(y), max(y) from x", Select) )),
        cleanup((
            maplist(sqlite_finalize, [Begin, Insert, End, Select]),
            sqlite_close(DB),
            delete_file('foo.db') )),
        true(R == row(1, 10 000)) ]) :-
    sqlite_eval(Begin),
    forall(between(1, 10 000, X),
        (   sqlite_bind(Insert, bv(X)),
            sqlite_eval(Insert)
        )),
    sqlite_eval(End),
    sqlite_eval(Select, R).

test(eval_insert_noresult, [
        setup((
            sqlite_open(foo, DB, [memory(true),mode(write)]),
            sqlite_prepare(DB, "Create table x ( y number )", Create),
            sqlite_eval(Create),
            sqlite_finalize(Create),
            sqlite_prepare(DB, "Insert into x values ( 6 )", Insert),
            sqlite_prepare(DB, "Select y from x", Select) )),
        cleanup((
            sqlite_finalize(Insert),
            sqlite_finalize(Select),
            sqlite_close(DB) )),
        true(Six == row(6)) ]) :-
    sqlite_eval(Insert),
    sqlite_eval(Select, Six).

test(select_some_double_use_reset, [
        setup((
            sqlite_open(foo, DB, [memory(true),mode(write)]),
            sqlite_prepare(DB, "Create table x ( y number )", Create),
            sqlite_eval(Create),
            sqlite_finalize(Create),
            sqlite_prepare(DB, "Insert into x values ( ?1 )", Insert),
            sqlite_prepare(DB,
                "Select y from x where y % 2 = ?1 order by y asc",
                Select) )),
        cleanup((
            sqlite_finalize(Insert),
            sqlite_finalize(Select),
            sqlite_close(DB) )),
        true([
            R1 == [row(2),row(4)],        N1 == 2, T1 == [],
            R2 == [row(1),row(3),row(5)], N2 == 3, T2 == [] ]) ]) :-
    forall(between(1,5,X),
        (   sqlite_bind(Insert, bv(X)),
            sqlite_eval(Insert)
        )),
    sqlite_bind(Select, bv(0)),
    sqlite_eval(Select, N1, R1, T1),
    sqlite_reset(Select),
    sqlite_bind(Select, bv(1)),
    sqlite_eval(Select, N2, R2, T2).

test(select_all_rows, [
        setup((
            sqlite_open(foo, DB, [memory(true),mode(write)]),
            sqlite_prepare(DB, "Create table x ( y number )", Create),
            sqlite_eval(Create),
            sqlite_finalize(Create),
            sqlite_prepare(DB, "Insert into x values ( ?1 )", Insert),
            sqlite_prepare(DB, "Select y from x order by y asc", Select) )),
        cleanup((
            sqlite_finalize(Insert),
            sqlite_finalize(Select),
            sqlite_close(DB) )),
        true([
            R == [row(2),row(3),row(4),row(5)],
            N == 4,
            R0 == [] ]) ]) :-
    forall(between(2,5,X),
        (   sqlite_bind(Insert, bv(X)),
            sqlite_eval(Insert)
        )),
    sqlite_eval(Select, N, R, R0).

test(select_more_rows, [
        setup((
            sqlite_open(foo, DB, [memory(true),mode(write)]),
            sqlite_prepare(DB, "Create table x ( y number )", Create),
            sqlite_eval(Create),
            sqlite_finalize(Create),
            sqlite_prepare(DB, "Insert into x values (1),(2),(3)", Insert),
            sqlite_prepare(DB, "Select y from x order by y asc", Select) )),
        cleanup((
            sqlite_finalize(Insert),
            sqlite_finalize(Select),
            sqlite_close(DB) )),
        true([
            R == [row(1),row(2),row(3)], R0 == [],
            R1 == [], T1 == [],
            N2 == 0, R2 == [], T2 == [] ]) ]) :-
    sqlite_eval(Insert),
    sqlite_eval(Select, 10, R, R0),
    sqlite_eval(Select, 10, R1, T1),
    sqlite_eval(Select, N2, R2, T2).

test(select_some_rows, [
        setup((
            sqlite_open(foo, DB, [memory(true),mode(write)]),
            sqlite_prepare(DB, "Create table x ( y number )", Create),
            sqlite_eval(Create),
            sqlite_finalize(Create),
            sqlite_prepare(DB, "Insert into x values ( ?1 )", Insert),
            sqlite_prepare(DB, "Select y from x order by y desc", Select) )),
        cleanup((
            sqlite_finalize(Insert),
            sqlite_finalize(Select),
            sqlite_close(DB) )),
        true([
            R == [row(50),row(49),row(48),row(47)|R1],
            R0 == [row(47)|R1],
            R2 == [row(46)],
            X == Y
            ]) ]) :-
    forall(between(2,50,X),
        (   sqlite_bind(Insert, bv(X)),
            sqlite_eval(Insert)
        )),
    sqlite_eval(Select, 3, R, R0),
    assertion(var(R0)),
    sqlite_eval(Select, 1, R0, R1),
    assertion(var(R1)),
    sqlite_eval(Select, 1, R2, []),
    sqlite_eval(Select, 0, X, Y).

:- end_tests(evaluate).