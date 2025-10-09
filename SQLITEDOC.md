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
```
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
```
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

