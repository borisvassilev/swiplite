/* Copyright (C) 2025 Boris Vassilev <boris.vassilev@gmail.com>

Permission is hereby granted, free of charge, to any person
obtaining a copy of this software and associated documentation
files (the "Software"), to deal in the Software without
restriction, including without limitation the rights to use,
copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the
Software is furnished to do so, subject to the following
conditions:

The above copyright notice and this permission notice shall be
included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES
OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
OTHER DEALINGS IN THE SOFTWARE.
*/
:- module(sqlite, [
            sqlite_version/1,
            sqlite_command/2,
            sqlite_query/3,
            sqlite_query_all/3,
            sqlite_schema/2,
            sqlite_open/3,
            sqlite_close/1,
            sqlite_prepare/3,
            sqlite_bind/2,
            sqlite_reset/1,
            sqlite_sql/2,
            sqlite_expanded_sql/2,
            sqlite_column_names/2,
            sqlite_finalize/1,
            sqlite_do/1,
            sqlite_one/2,
            sqlite_many/4,
            sqlite_row/2,
            sqlite_status/4,
            sqlite_db_status/5,
            sqlite_stmt_status/4 ] ).

/** <module> Prolog bindings for SQLite

This module provides partial access to the C-language interface
of SQLite.

It exposes the database connection object =sqlite3= and the
prepared statement object =sqlite3_stmt=, along with some of the
essential functions using these objects. Please refer to the
[SQLite documentation](https://www.sqlite.org/c3ref/intro.html)
and the implementation in =c/swiplite.c= when using this library.
To make it easier to find the relevant docs, I have tried to
consistently provide links.

Most of the predicates in this module are as close as possible in
naming and semantics to the corresponding functions in the C
interface. One exception is sqlite_bind/2, which converts values
from Prolog terms to corresponding
[SQLite column datatype](https://www.sqlite.org/flextypegood.html).
Similarly, sqlite_do/1, sqlite_one/2, and sqlite_many/4 wrap
the necessary calls to sqlite3_step() and convert the results of
=SELECT= queries to Prolog terms.

The database connection and prepared statement objects are
represented in SWI-Prolog as
[blobs](https://www.swi-prolog.org/pldoc/man?section=blob).
They are garbage collected, but
[finalizing a statement](https://www.sqlite.org/c3ref/finalize.html)
or [closing a database connection](https://www.sqlite.org/c3ref/close.html)
(and, alternatively, not doing it) have reprecussions, especially for
long-running programs. The code in this library uses exclusively the
=|*_v2|= versions of the SQLite C interface. In particular:

> The [sqlite3_close_v2() interface](https://www.sqlite.org/c3ref/close.html)
> is intended for use with host languages that are garbage
> collected, and where the order in which destructors are called is
> arbitrary.
*/

:- use_foreign_library(foreign(swiplite)).
:- use_module(library(dcg/basics)).

:- multifile prolog:error_message//1.

prolog:error_message(sqlite_error(Caller, Code, Str, Message)) -->
    [ '[~s] (~d) ~s - ~s'-[Caller, Code, Str, Message] ].
prolog:error_message(swiplite_error(Caller, Message)) -->
    [ '[~s] ~s'-[Caller, Message] ].

/** sqlite_version(-Version:atom) is det

Unify Version with the version of SQLite currently in use
*/
sqlite_version(V) :-
    setup_call_cleanup(sqlite_open('', DB, [memory(true)]),
        setup_call_cleanup(sqlite_prepare(DB, "select sqlite_version()", S),
            sqlite_one(S, row(V0)),
            sqlite_finalize(S)),
        sqlite_close(DB)),
    atom_string(V, V0).

/** sqlite_command(++Connection:blob, ++SQL:text) is det

Execute the command in SQL.

This assumes that the command is not a =|SELECT|= query and
it does not return any rows.

@arg Connection A database connection obtained with sqlite_open/3
@arg SQL The text of the command to be executed
*/
sqlite_command(DB, SQL) :-
    setup_call_cleanup(sqlite_prepare(DB, SQL, S),
        sqlite_do(S),
        sqlite_finalize(S)).

/** sqlite_query(++Connection:blob, ++SQL:text, -Row:row) is semidet

Get rows from the result set of the statement in SQL on backtracking.

If the result set is empty, fail.

@arg Connection A database connection obtained with sqlite_open/3
@arg SQL The text of the command to be executed
@arg Row Unified with row(Col1, Col2, ...) on backtracking
*/
sqlite_query(DB, SQL, Row) :-
    setup_call_cleanup(sqlite_prepare(DB, SQL, S),
        sqlite_row(S, Row),
        sqlite_finalize(S)).

/** sqlite_query_all(++Connection:blob, ++SQL:text, -Rows:list(row)) is semidet

Get all rows from the result set of the statement in SQL as a list in Rows.

@arg Connection A database connection obtained with sqlite_open/3
@arg SQL The text of the command to be executed
@arg Rows The list of rows with all rows in the result set
*/
sqlite_query_all(DB, SQL, Rs) :-
    setup_call_cleanup(sqlite_prepare(DB, SQL, S),
        sqlite_many(S, _, Rs, []),
        sqlite_finalize(S)).

/** sqlite_schema(++Connection:blob, -Schema:text) is det

Query the DB schema for that connection.

This is a convenience predicate returning the `sql` column of the table
`sqlite_schema`.

@arg Connection A database connection obtained with sqlite_open/3
@arg Schema A list of objects in the current schema

@see [The Schema Table](https://www.sqlite.org/schematab.html)
@see [=|PRAGMA table_list|=](https://www.sqlite.org/pragma.html#pragma_table_list)
@see [=|PRAGMA table_info|=](https://www.sqlite.org/pragma.html#pragma_table_info)
@see [=|PRAGMA table_xinfo|=](https://www.sqlite.org/pragma.html#pragma_table_xinfo)
*/
sqlite_schema(Connection, Schema) :-
    setup_call_cleanup(sqlite_prepare(Connection,
            "Select sql from sqlite_schema where sql is not null", S),
        sqlite_many(S, _, Schema, []),
        sqlite_finalize(S)).


:- predicate_options(sqlite_open/3, 3,
        [ mode(oneof([read,write,create])),
          memory(boolean),
          threaded(oneof([single,multi,serialized]))
        ]).
/** sqlite_open(++File:text, -Connection:blob, ++Options:list) is det

Open Connection to the database in File using Options

The options are used to set the =|flags|= argument in the call to
[=|sqlite3_open_v2()|=](https://www.sqlite.org/c3ref/open.html).
The following options are recognized:

    * mode(Mode)
      Determines how the database is opened:

      | *Value*          | *Corresponding flags*                          |
      | =read= (default) | =SQLITE_OPEN_READONLY=                         |
      | =write=          | =SQLITE_OPEN_READWRITE=                        |
      | =create=         | =|SQLITE_OPEN_READWRITE | SQLITE_OPEN_CREATE|= |

    * memory(Bool)
      Open as an in-memory database:

      | *Value*           | *Corresponding flags* |
      | =false= (default) | (empty)               |
      | =true=            | =SQLITE_OPEN_MEMORY=  |

    * threaded(Threaded)
      [Threading mode](https://www.sqlite.org/threadsafe.html)
      for this database connection:

      | *Value*            | *Corresponding flags*   |
      | =single= (default) | (empty)                 |
      | =multi=            | =SQLITE_OPEN_NOMUTEX=   |
      | =serialized==      | =SQLITE_OPEN_FULLMUTEX= |

    * foreign_keys(Bool)
      Enable [foreign keys](https://www.sqlite.org/foreignkeys.html)
      for this connection. Defaults to =|true|= and will enable foreign
      keys for this connection and throw an error if foreign keys are
      not supported. Setting this option to =|false|= is required
      if the used version of SQLite does not support foreign keys.

      | *Value*          | *Behaviour*                   |
      | =true= (default) | Enable foreign keys           |
      | =false=          | Do not check for foreign keys |

@arg File Relative path to the database file. Interpreted as UTF-8 string.
@arg Connection A blob with the database connection.
@arg Options A list of options

@see [`sqlite3_open_v2()`](https://www.sqlite.org/c3ref/open.html)
@see [Using SQLite in multi-threaded applications](https://www.sqlite.org/threadsafe.html)

@tbd Support all available =|SQLITE_OPEN_*|= flags.
*/

/** sqlite_close(++Connection:blob) is det

Close a Connection opened with sqlite_open/3

@arg Connection A database connection obtained with sqlite_open/3

@see [`sqlite3_close_v2()`](https://www.sqlite.org/c3ref/close.html)
*/

/** sqlite_prepare(++Connection:blob, ++SQL:text, -Statement:blob) is det

Compile Statement from the text in SQL using the database in Connection

The UTF-8 encoded text in SQL is parsed up to the first nul, or
up to the end of the first SQL statement. SQL parameters are
initially all set to =|NULL|=. Anonymous variables are not allowed.
If *|?|*_|NNN|_ parameters are used, they must be numbered
starting from 1, without any gaps.

@arg Connection A database connection obtained with sqlite_open/3
@arg SQL The UTF8-encoded text of the SQL as an atom, string,
         or list of codes

@see sqlite_bind/2
@see [SQL statement parameters in SQLite](https://www.sqlite.org/lang_expr.html#varparam)
@see [`sqlite3_bind_parameter_count()`](https://www.sqlite.org/c3ref/bind_parameter_count.html)
@tbd Do something with the rest of the text in SQL
*/

/** sqlite_finalize(++Statement:blob) is det

Delete a prepared statement

@arg Connection A database connection obtained with sqlite_open/3

@see [`sqlite3_finalize()`](https://www.sqlite.org/c3ref/finalize.html)
*/

/** sqlite_bind(++Statement:blob, ++Bind_values:bv) is det

Use Bind_values to set the variables in Statement

The term in Bind_values must be named "`bv`" (*|b|*ind *|v|*alues).
Use an empty list `[]` to set a variable to =|NULL|=.

~~~
?- sqlite_prepare(DB, "Select ?1, ?2", S),
   sqlite_bind(S, bv('a', [])),
   sqlite_expanded_sql(S, E).
E = "Select 'a', NULL".
~~~

Each term in the Bind_values argument is used to set the variable
with the same index in the SQL statement; both start counting at 1.

In addition to using the empty list to represent SQL =NULL=:
   * Atoms, strings, and code lists are converted to text;
   * Integers are converted to a 64-bit integer, if the value fits;
   * Floats are converted to a =double=.

@arg Statement A statement compiled with sqlite_prepare/3
@arg Bind_values A flat term with functor =|bv/<number of parameters>|=
@arg Statement A blob with the compiled statement

@see sqlite_prepare/3
@see sqlite_sql/2
@see sqlite_expanded_sql/2
@see [SQL statement parameters in SQLite](https://www.sqlite.org/lang_expr.html#varparam)
@tbd Support more types
*/

/** sqlite_reset(++Statement:blob) is det

Reset Statement

@arg Statement A statement compiled with sqlite_prepare/3

@see [`sqlite3_reset()`](https://www.sqlite.org/c3ref/reset.html)
*/

/** sqlite_sql(++Statement:blob, -SQL:atom) is det

Unify SQL with the UTF-8 text used to create the prepared statement

@arg Statement A statement compiled with sqlite_prepare/3
@arg SQL An atom with the original text of the statement

@see sqlite_prepare/3
@see sqlite_bind/2
@see [`sqlite3_sql()`](https://www.sqlite.org/c3ref/expanded_sql.html)
*/

/** sqlite_expanded_sql(++Statement:blob, -Expanded_SQL:string) is det

Retrieve the SQL statement with bind parameters expanded

@arg Statement A statement compiled with sqlite_prepare/3
@arg Expanded_SQL A string with the expanded statement

@see sqlite_prepare/3
@see sqlite_bind/2
@see [`sqlite3_expanded_sql()`](https://www.sqlite.org/c3ref/expanded_sql.html)
*/

/** sqlite_column_names(++Statement:blob, -Column_names:cols) is det

Retrieve the column names of a =|SELECT|= statement

For a =|SELECT|= statement, the result is a flat term
=|cols(column_1, column_2, ...)|=.

If the prepared statement does not have a result set with columns
in it, Column_names is unified with =|cols()|=.

@arg Statement A statement compiled with sqlite_prepare/3
@arg Column_names A flat term with functor =|cols/<number of columns>|=

@see [`sqlite3_column_name()`](https://www.sqlite.org/c3ref/column_name.html)
@see [`sqlite3_column_count()`](https://www.sqlite.org/c3ref/column_count.html)
*/

/** sqlite_do(++Statement:blob) is det

Evaluate a statement that has no results

For example, =|CREATE|= or =|INSERT|= statements must be
evaluated using sqlite_do/1, while a =|SELECT|= needs
either sqlite_one/2 or sqlite_many/4.

Statement is reset automatically upon success.

@error swiplite_error When the statement has results

@arg Statement A statement compiled with sqlite_prepare/3
*/

/** sqlite_one(++Statement:blob, Result:row) is det

Evaluate a =|SELECT|= statement with exactly one row in the result set

Statement is reset automatically upon success.

@error swiplite_error When the statement does not have exactly one result row

@arg Statement A =|SELECT|= statement compiled with sqlite_prepare/3
@arg Result A flat term with functor =|row/<number of columns>|=
*/

/** sqlite_many(++Statement:blob, ?N:nonneg, -R:list(row), ?T) is det

Evaluate a statement to collect results in the difference list R-T.

When N is a free variable, fetch all rows of the result set and
unify N with the number of rows.

Otherwise, fetch up to N result rows in R.

R and T form a difference list. When there are no more results
in the result set, T is unified with the empty list `[]`.

A statement evaluated with sqlite_many/4 *|must be|* explictly
reset using sqlite_reset/1 after all rows in the result set have
been fetched. Until it is reset, consecutive calls will unify N
with 0 and both R and T with the empty list `[]`.

@arg Statement A =|SELECT|= statement compiled with sqlite_prepare/3
@arg N Number of rows
@arg R Rows of the result set
@arg T Tail of R

@see [`sqlite3_column_count()`](https://www.sqlite.org/c3ref/column_count.html)
*/

/** sqlite_row(++Statement:blob, -Row:row) is semidet

Get rows from the result set of Statement on backtracking.

If the result set is empty, fail.

@arg Statement A =|SELECT|= statement compiled with sqlite_prepare/3
@arg Row A row in the result set
*/

/** sqlite_status(
        ++Op:sqlite_status_code,
        -Current:integer,
        -Highwater:integer,
        +Reset:boolean) is det

Query [SQLite runtime status](https://www.sqlite.org/c3ref/status.html).

The first argument is an atom that corresponds to one of the
[=|SQLITE_STATUS_|= codes](https://www.sqlite.org/c3ref/c_status_malloc_count.html),
with the =|SQLITE_STATUS_|= prefix dropped and in lowercase. For example,
=|memory_used|= or =|parser_stack|=.

@arg Op SQLite run-time status parameter
@arg Current is the current value of the parameter
@arg Highwater is the highest recorder value
@arg Reset When =|true|=, the Highwater value is reset after it is returned

@see sqlite_db_status/5
@see sqlite_stmt_status/4
@see [SQLite Runtime Status](https://www.sqlite.org/c3ref/status.html)
@see [Status Parameters](https://www.sqlite.org/c3ref/c_status_malloc_count.html)
*/

/** sqlite_db_status(
        ++DB:sqlite_connection,
        ++Op:sqlite_db_status_code,
        -Current:integer,
        -Highwater:integer,
        +Reset:boolean) is det

Query [SQLite database connection status](https://www.sqlite.org/c3ref/db_status.html).

The first argument is a database connection blob.
The second argument is an atom that corresponds to one of the
[=|SQLITE_DBSTATUS_|= codes](https://www.sqlite.org/c3ref/c_dbstatus_options.html),
with the =|SQLITE_DBSTATUS_|= prefix dropped and in lowercase. For example,
=|cache_used|= or =|deferred_fks|=.

@arg DB A database connection obtained with sqlite_open/3
@arg Op SQLite status parameter for database connections
@arg Current is the current value of the parameter
@arg Highwater is the highest recorder value
@arg Reset When =|true|=, the Highwater value is reset after it is returned

@see sqlite_status/4
@see sqlite_stmt_status/4
@see [Database Connection Status](https://www.sqlite.org/c3ref/db_status.html)
@see [Status Parameters for database connections](https://www.sqlite.org/c3ref/c_dbstatus_options.html)
*/

/** sqlite_stmt_status(
        ++DB:sqlite_statement,
        ++Op:sqlite_stmt_status_code,
        -Counter:integer,
        +Reset:boolean) is det

Query [SQLite prepared statement status](https://www.sqlite.org/c3ref/stmt_status.html).

The first argument is a prepared statement blob.
The second argument is an atom that corresponds to one of the
[=|SQLITE_STMTSTATUS_|= codes](https://www.sqlite.org/c3ref/c_stmtstatus_counter.html),
with the =|SQLITE_STMTSTATUS_|= prefix dropped and in lowercase. For example,
=|sort|= or =|reprepare|=.

@arg DB A prepared statement obtained with sqlite_prepare/3
@arg Op SQLite status parameter for prepared statement
@arg Counter is the current value of the corresponding counter
@arg Reset When =|true|=, the counter is reset after it is returned

@see sqlite_status/4
@see sqlite_db_status/5
@see [Prepared Statement Status](https://www.sqlite.org/c3ref/stmt_status.html)
@see [Status Parameters for prepared statements](https://www.sqlite.org/c3ref/c_stmtstatus_counter.html)
*/
