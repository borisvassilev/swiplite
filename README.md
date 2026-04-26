# SQLite for SWI-Prolog
The purpose of swiplite is to enable using sqlite3
databases comfortably from SWI-Prolog, without sacrificing
efficiency.

The [`sqlite` module](prolog/sqlite.pl) provides low-level
access to the connection and statement objects.

In addition to this document, there is a discussion on the
[specifics of SQLite](SQLITEDOC.md) in comparison to other
relational databases. There is [another document](DEVELOPMENT.md)
summarizing information about the development process of the
library.

## Requirements
This has been developed and tested on Linux and Mac OS.

You might already have a decently recent version of SQLite
installed; you can also install it using the package manager
for your OS. On MacOS, it is strongly recommended to install
SQLite [from Homebrew](https://formulae.brew.sh/formula/sqlite)
and make sure this library is linked against it. The default
SQLite that comes with MacOS is somewhat out of date and seems
to be compiled with questionable flags, probably for backwards
compatibility.

Finally, since this package contains C code, you need CMake
and a C compiler.

## Installation
The pack has been already published. To get the latest release:
```
$ swipl pack install swiplite
```

... or from the top level:
```
?- pack_install(swiplite).
```

### Installing from source
The code is available in the public GitHub repository
<https://github.com/borisvassilev/swiplite>.
To install from the source, clone the repo and install
from the pack directory:
```
$ git clone https://github.com/borisvassilev/swiplite.git
$ cd swiplite
$ swipl pack install .
```

Alternatively, create an archive and install it locally:
```
$ git archive --output=swiplite-<version>.tgz <tree-ish>
$ swipl pack install swiplite-<version>.tgz
```

### Embedding the SQLite source
If you prefer to use the SQLite source provided on
[SQLite's download page](https://www.sqlite.org/download.html),
you can do it by first checking out its own branch,
`sqlite3-amalgamation`.
Once you add the two files you will find in the
[amalgamation](https://www.sqlite.org/amalgamation.html),
`sqlite3.c` and `sqlite3.h`, in the `c/` subdirectory,
you should see:
```
$ git status
On branch sqlite3-amalgamation
Your branch is up to date with 'origin/sqlite3-amalgamation'.

Untracked files:
  (use "git add <file>..." to include in what will be committed)
	c/sqlite3.c
	c/sqlite3.h

nothing added to commit but untracked files present (use "git add" to track)
```

You can now build and install the library from the local repo
as [described above](#installing-from-source).

### Smoke test
To check that you have installed the add-on correctly:
```
?- use_module(library(sqlite)).
true.

?- sqlite_version(V).
V = '3.53.0'.
```

The predicate `sqlite_version/1` opens an in-memory SQLite
database and queries `Select sqlite_version()`.

## Background
There are _many_ alternatives to using the `swiplite` pack.
Here is a list of questions I have asked myself _before_
I started working on it (and repeatedly ever since), along
with attempts at answers.

### Why SQL?
There is no good reason to use SQL; other than, it is the
standard interface to relational databases.

### Why a relational database?
... especially since you are using Prolog already?

Relational databases come with features that are not available
out of the box on SWI-Prolog:
  * Foreign key, unique, and check constraints;
  * Disk persistency.

### Why SQLite?
SQLite provides a full-featured relational database _almost_
for free, in terms of setup and resources needed.

### Why not ODBC?
SWI-Prolog provides a full-featured [ODBC interface](<https://www.swi-prolog.org/pldoc/doc_for?object=section(%27packages/odbc.html%27)>).
However, specifically in the case of SQLite, it requires a
[clumsy setup with extra dependencies](https://swi-prolog.discourse.group/t/connecting-to-sqlite3-with-odbc-choosing-a-database-file/6387?u=boris)
and no obvious benefit.

### Why not proSQLite?
[proSQLite](https://stoics.org.uk/~nicos/sware/prosqlite/)
is a mature SWI-Prolog interface to SQLite also available
[as a pack](https://www.swi-prolog.org/pack/list?p=prosqlite).
Unfortunately, I was not able to figure out how to use it to
create prepared statements with SQL parameters and bind values
to them.

### Why do you need prepared statements?
It is very likely that prepared statements are not _that_
important. On the other hand, there is the cautionary tale
of [Little Bobby Tables](https://xkcd.com/327/).

I have described [one bug and feature](SQLITEDOC.md), in
"Using a literal in an SQL statement": a Prolog integer that
does not fit in an 8-byte signed integer will be silently
converted to an SQLite REAL, and the original value will be
irreversibly lost. In contrast, if you use a prepared statement
as provided by this library, swiplite, and you attempt to
bind an integer that does not fit, you will get a run-time
error.

## Further work
This library has been used by me for about a year at this point
of time (2026-04-25) and it has served me well, for the very
limited purposes I have had. A couple of features I would
sometimes wish I would have had:

 * Transparent support for all data types in SWI-Prolog. As it
   is right now, you need to stringify large integers, rational
   numbers, compound terms, and so on, before inserting them in
   your SQLite database, and re-create them when you retrieve
   them.
 * An efficient transparent support for compound terms not based
   on stringifying.
 * A clean high-level interface. The main reason I have not
   provided it publicly is that I am not sure what might be
   generally useful to others and the work involved is too boring.

In addition, the CMakeLists.txt currently provided in this
library _happens to work_ for the two machines I have in daily use.
In the unlikely case that someone else wants to use this library
they might be able to propose improvements.
