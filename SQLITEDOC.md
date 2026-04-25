# SQLite Specifics
SQLite, like all other relational database vendors, has a
specific set of features and limitations. Here we discuss how
those are exposed through `library(sqlite)`.

## Multi-threading
[SQLite can be used by a multi-threaded application](https://www.sqlite.org/threadsafe.html).

## Data types
[SQLite uses flexible typing](https://www.sqlite.org/flextypegood.html).
You can [declare the types](https://www.sqlite.org/datatype3.html)
of columns in the `CREATE TABLE` statement, and SQLite will
attempt to transform the input data to the preferred datatype of
the column. However, any value in SQLite can have any storage
class, regardless of the declared column type. In fact, column
type declarations are not mandatory and can be omitted. This is a
valid table declaration for a key-value store:

```
CREATE TABLE attribute(name TEXT PRIMARY KEY, value ) WITHOUT ROWID;
```

Values stored in the SQLite database have one of five _storage
classes_.

 * **NULL** is the storage class of the `NULL` value.
 * **INTEGER** holds a signed integer stored in 0, 1, 2, 3, 4,
   6, or 8 bytes, depending on the size. The smallest and largest
   values for an 8-byte signed integer are -9223372036854775808
   and 9223372036854775807.
 * **REAL** is for 8-byte (double precision) IEEE floating point
   number.
 * **TEXT** stores string in the database encoding (default UTF-8).
 * **BLOB** is a blob of data stored exactly as input.

The interface of `library(sqlite)` defines a two-way mapping of
Prolog types to SQLite. The purpose is to enable saving and
retrieving arbitrary Prolog terms to an SQLite database.
There are currently two possible ways to add values to the
SQLite database: using a literal in a SQL statement through
`sqlite_prepare/3`, or using a bind variables through
`sqlite_bind/2`. The only way to retrieve values is through
a `SELECT` statement with one of `sqlite_one/2`, `sqlite_many/4`,
and `sqlite_row/2`.

### Using a literal in an SQL statement
If we would prepare a statement and execute it:
```prolog
sqlite_prepare(DB,
    "INSERT INTO TABLE t VALUES ( 'foo', 12, 3.5 )", S),
sqlite_do(S)
```

In this case, the SQLite
[affinity rules](https://www.sqlite.org/datatype3.html#type_affinity)
apply. This means that for example, an integer literal outside of
the range for an 8-byte signed integer will be represented as a
REAL internally. The original integer value is lost and cannot be
retrieved.
```prolog
?- setup_call_cleanup(
       sqlite_open(foo, DB, [mode(write),memory(true)]),
       (   sqlite_command(DB, "create table kv ( k text primary key, v ) without rowid"),
           sqlite_command(DB, "insert into kv values ( 'foo', 9223372036854775808 )"),
           sqlite_query(DB, "select * from kv", Row)
       ),
       sqlite_close(DB)).
Row = row("foo", 9223372036854776000.0).
```

### Using a bind variable
In a prepared SQL statement with parameters, the Prolog term
used in the bind variable determines the conversion to a SQLite
value. This is the current mapping:

| Prolog term type | `sqlite3_bind_*` function |
|------------------|---------------------------|
| [] % empty list  | null                      |
| atom             | text                      |
| string           | text                      |
| list of codes    | text                      |
| integer          | int64 (throws on bigint)  |
| float            | double                    |
| var              | (throws type_error)       |
| compound         | (throws type_error)       |

### Returning column values to Prolog
SQLite column types as obtained with
[`sqlite3_column_type()`](https://www.sqlite.org/c3ref/column_blob.html)
are mapped to the following Prolog types, or an error is thrown:

| `sqlite3_column_type` | `PL_put_*` function |
|-----------------------|---------------------|
| INTEGER               | integer             |
| FLOAT                 | float               |
| TEXT                  | chars(PL_STRING)    |
| NULL                  | nil                 |
| BLOB                  | (type_error)        |

## Foreign keys
[Foreign keys in SQLite](https://www.sqlite.org/foreignkeys.html) must
be enabled explicitly. First, the library must be compiled with neither of
these two compile options defined:

 * `SQLITE_OMIT_FOREIGN_KEY`
 * `SQLITE_OMIT_TRIGGER`

In addition, for each connection, the application must enable foreign keys
with [PRAGMA `foreign_keys`](https://www.sqlite.org/pragma.html#pragma_foreign_keys).

The PRAGMA can be used to check if the currently used version of SQLite
supports foreign keys. This query:

```sql
PRAGMA foreign_keys;
```

... will **not return rows** if the SQLite version does **not support**
foreign keys. It will return 0 or 1 to indicate that foreign key constraints
are currently disabled (0) or enabled (1). It can be switched on and off
with:

```sql
PRAGMA foreign_keys = ON;
PRAGMA foreign_keys = OFF;
```

This library makes the following design decisions:

 * Foreign keys are required and enabled by default
 * If requested when obtaining a connection, foreign keys can be disabled
   for this connection. In that case, it is allowed to use an SQLite
   version that has been compiled without foreign key support
 * Trying to enable foreign keys for an SQLite version that **does not**
   support them will throw an error. In contrast, as of SQLite 3.50.4,
   on 2025-10-06, issuing `PRAGMA foreign_keys = ON;` on a database that
   does not support foreign keys silently succeeds.

## Schema
SQLite provides a couple of mechanisms for introspection. There
is [the schema table](https://www.sqlite.org/schematab.html);
there are also a few [SQLite pragmas](https://www.sqlite.org/pragma.html)
that return information on the tables in the schema:
 * [PRAGMA table_list](https://www.sqlite.org/pragma.html#pragma_table_list)
 * [PRAGMA table_info](https://www.sqlite.org/pragma.html#pragma_table_info)
 * [PRAGMA table_xinfo](https://www.sqlite.org/pragma.html#pragma_table_xinfo)

In addition, the convenience predicate `sqlite_schema/2` returns
the `sql` column of the `sqlite_schema` table.

SQLite `PRAGMA`s that have a result set can be accessed as if
they were Select statements.

## Database statistics
The library provides access to the following functions:
 * [`sqlite3_db_status()`](https://www.sqlite.org/c3ref/db_status.html)
 * [`sqlite3_status()`](https://www.sqlite.org/c3ref/status.html)
 * [`sqlite3_stmt_status()`](https://www.sqlite.org/c3ref/stmt_status.html)

Those are implemented in `sqlite_status/4`, `sqlite_db_status/5`, and
`sqlite_stmt_status/4`.

## Database configuration
There are now bindings for:

* `sqlite3_initialize()`: `sqlite_initialize/0`
* `sqlite3_shutdown()`: `sqlite_shutdown/0`

I find it [difficult to tell](https://www.sqlite.org/c3ref/initialize.html)
if my code is supposed to call these explicitly.

At some point I might need bindings for `sqlite3_config()` and
`sqlite3_db_config()`.
