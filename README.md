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
`sqlite3-amalgamation`, then dropping the two files
`sqlite3.c` and `sqlite3.h` in the `c/` subdirectory.

You can first check the diff:
```
$ git checkout sqlite3-amalgamation
Switched to branch 'sqlite3-amalgamation'
Your branch is up to date with 'origin/sqlite3-amalgamation'.

$ git diff main
diff --git a/CMakeLists.txt b/CMakeLists.txt

# Create the library as a CMake module
-add_library(swiplite MODULE c/swiplite.c)
+add_library(swiplite MODULE c/swiplite.c c/sqlite3.c)

-find_package(SQLite3 REQUIRED)
-target_link_libraries(swiplite PRIVATE ${SQLite3_LIBRARIES})
-target_include_directories(swiplite PRIVATE ${SQLite3_INCLUDE_DIRS})

diff --git a/c/swiplite.c b/c/swiplite.c

 #include <SWI-Prolog.h>
 #include <SWI-Stream.h>
-#include <sqlite3.h>
+#include "sqlite3.h"
```

Once you add the two files you will find in the
[amalgamation](https://www.sqlite.org/amalgamation.html)
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
V = '3.47.2'.
```

The predicate `sqlite_version/1` opens an in-memory SQLite
database and queries "`Select sqlite_version()`".

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
