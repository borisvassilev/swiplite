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
of result data or the statement is done.

I currently have the following design:
 * the client can say how many rows are expected: 0, 1, or more
 * rows are returned as a list of `r/<column_count>` terms
 * the result is a list with up to N elements and tail

The one-argument predicate `sqlite_eval(Stmt)` succeeds if
evaluating the prepared `Stmt` succeeds without returning any
rows. The statement is reset and ready to be evaluated again.

The two-argument `sqlite_eval(Stmt, Row)` succeeds if evaluating
`Stmt` succeeds and unifies `Row` with _the exactly one_ row of
results `row(V1, V2, ...)`. If there are further rows in the
result set, those are not fetched and an error is raised; if
the statement is done it is reset and ready to be evaluated
again.

The four-argument `sqlite_eval(Stmt, N, Rs)` reads up to `N` rows
in `R`. If there are no more rows in the result, `R0 == []`. If
`N` is a free variable all rows in the result will be fetched at
once and `N` unified with the total number of rows in the result
set.

In addition, `sqlite_reset(Stmt)` must be used to reset a
statement after all rows in the result set have been stepped
through. It is not needed for statements evaluated with
`sqlite_eval/1` or `sqlite_eval/2` (calling it has no effect).

