# SQLite for SWI-Prolog
The purpose of swiplite is to enable using sqlite3
databases comfortably from SWI-Prolog, without sacrificing
efficiency.

The [`sqlite` module](prolog/sqlite.pl) provides low-level
access to the connection and statement objects.

## Requirements
This has been developed and tested on Linux and Mac OS.

You might already have a decently recent version of SQLite
installed; you can also install it using the package manager
for your OS.

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

You can use this method specifically if you want to build the
package using the SQLite amalgamation, currently found on its
own branch, `sqlite3-amalgamation`:
```
$ git archive --output=swiplite-<version>.tgz sqlite3-amalgamation
```

This last method uses `c/sqlite3.c` and `c/sqlite3.h`.

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
SWI-Prolog provides a full-features [ODBC interface](https://www.swi-prolog.org/pldoc/doc_for?object=section(%27packages/odbc.html%27)).
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
