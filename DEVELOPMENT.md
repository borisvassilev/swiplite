# Development tips and tricks
This documents my own development workflow and is probably
useless to anyone else.

## (Re-)Installing from SWI-Prolog using pack_install/1

This example removes the installed pack, re-packs it, and re-installs
it, **without bumping version numbers and without
distributing the source code**, from the directory which _contains_
the swiplite directory:
```
$ rm -rf ~/.local/share/swi-prolog/pack/swiplite
$ tar -czf swiplite-0.1.tgz swiplite
$ swipl
?- pack_install('swiplite-0.1.tgz', [interactive(false)]).
?- use_module(library(sqlite)).
```

## Build without packing and installing
From inside the swiplite directory:
```
$ mkdir -p build \
    && ( cd build \
        && SWIPL_HOME_DIR="$(swipl --home)" cmake -G Ninja .. \
        && ninja )
```

After this, use `-p` to set the locations for foreign() and
library(), from inside the package home directory:
```
$ swipl -p foreign=build -p library=prolog
?- use_module(library(sqlite)).
?- use_module(test/test_sqlite).
?- run_tests.
```

... or alternatively,
```
$ swipl -p foreign=build -p library=prolog \
    -g 'use_module(test/test_sqlite),run_tests' \
    -t halt
```

This has been tested on MacOS and Linux, but of course the
`SWI_HOME_DIR` environment variable depends on where SWI-Prolog
is installed.

Yet another option is to have a file `debug.pl` that could look
like this:

```
:- doc_server(4000).
:- portray_text(true).

:- multifile user:file_search_path/2.

user:file_search_path(library, L) :- absolute_file_name(prolog, L).
user:file_search_path(foreign, L) :- absolute_file_name(build, L).

:- use_module(library(sqlite)).
:- use_module(test/test_sqlite).
```

If you now consult this file:
```
?- [debug].
```

you get a documentation server running at `http://localhost:4000/pldoc/`
and you can use the exported predicates, or run all tests with
```
?- run_tests.
```

## Evaluating SQLite statements
SQLite statements are evaluated by calling `sqlite3_step()` one
or more times. The return value distinguishes if there is a row
of result data or the statement is done: either `SQLITE_ROW` or
`SQLITE_DONE`.

`SELECT` statements are the only ones that have a result set.
From [the docs of sqlite3_column_count()](https://www.sqlite.org/c3ref/column_count.html):

> Return the number of columns in the result set returned by the prepared statement. If this routine returns 0, that means the prepared statement returns no data (for example an `UPDATE`). However, just because this routine returns a positive number does not mean that one or more rows of data will be returned. A `SELECT` statement will always have a positive `sqlite3_column_count()` but depending on the `WHERE` clause constraints and the table content, it might return no rows.

The interface provides four predicates for evaluating prepared
statements, wrapping the necessary calls to `sqlite3_step()`.
 * for statements without a result set, `sqlite_do/1`
 * for statements with exactly one result row, `sqlite_one/2`
 * for a list of result rows in the sequence in which `sqlite3_step()`
   fetches them, `sqlite_many/4`
 * to backtrack over rows in the result set, `sqlite_row/2`

The predicate `sqlite_do(Stmt)` throws an error if the prepared
statement *has any columns* in its result set. It evaluates the
statement and succeeds if the first `sqlite3_step()` invocation
returns `SQLITE_DONE`. The statement is reset and ready to be
evaluated again.

The predicate `sqlite_one(Stmt, Row)` throws an error if the
prepared statement *does not have columns* in its result set. It
expects exactly one row in the result set, and if there are no
rows or more than one row in the result set, it also throws an
error. When the predicate succeed, the statement is reset and
ready to be evaluated again.

The predicate `sqlite_many(Stmt, N, Rows, R0)` can be used to
fetch multiple rows from the result set as a list of Prolog
terms. It reads up to `N` rows in `Rows`. If there are no more
rows in the result, `R0 == []`. If `N` is a free variable, all
rows in the result will be fetched at once and `N` unified with
the total number of rows in the result set.

In addition, `sqlite_reset(Stmt)` must be used to reset a
statement after all rows in the result set have been stepped
through. It is not needed for statements evaluated with
`sqlite_eval/1` or `sqlite_eval/2` (calling it has no effect).

## The length argument in `*_nchars()`
I used the following function to figure out the
value of the "length" argument in `PL_get_nchars()`:
```
foreign_t
pl_text_length(term_t t, term_t n)
{
    char *s; size_t s_n;
    if (!PL_get_nchars(t, &s_n, &s,
                CVT_ATOM | CVT_STRING | CVT_LIST
                | CVT_EXCEPTION
                | BUF_STACK
                | REP_UTF8)
            || !PL_unify_uint64(n, s_n))
        return false;
    return true;
}
```

With this, I get:
```
?- text_length(foo, N).
N = 3.

?- text_length('foo\0bar', N).
N = 7.

?- text_length('foo\0bar\0', N).
N = 8.

?- text_length('уеиш', N).
N = 8.
```

So, it seems that the number is in bytes, and the terminating
null is not included in that count.
