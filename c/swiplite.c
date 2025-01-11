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
#include <SWI-Prolog.h>
#include <SWI-Stream.h>
#include <sqlite3.h>

/* Errors */
static int
sqlite_error(sqlite3 *db, const char *location)
{
    term_t e;
    return ( (e=PL_new_term_ref())
            && PL_unify_term(e,
                PL_FUNCTOR_CHARS, "error", 2,
                  PL_FUNCTOR_CHARS, "sqlite_error", 4,
                    PL_CHARS, location,
                    PL_INT, sqlite3_errcode(db),
                    PL_CHARS, sqlite3_errstr(sqlite3_errcode(db)),
                    PL_CHARS, sqlite3_errmsg(db),
                  PL_VARIABLE)
            && PL_raise_exception(e));
}

static int
sqlite_error_stmt(sqlite3_stmt *stmt, const char *location)
{
    return sqlite_error(sqlite3_db_handle(stmt), location);
}

static int
swiplite_error(const char *location, const char *message)
{
    term_t e;
    return ( (e=PL_new_term_ref())
            && PL_unify_term(e,
                PL_FUNCTOR_CHARS, "error", 2,
                  PL_FUNCTOR_CHARS, "swiplite_error", 2,
                    PL_CHARS, location,
                    PL_CHARS, message,
                  PL_VARIABLE)
            && PL_raise_exception(e));
}

/* Connection blob */
static int release_sqlite_connection(atom_t c);
static int write_sqlite_connection(IOSTREAM *s, atom_t c, int flags);

static PL_blob_t sqlite_connection_blob = {
    .magic = PL_BLOB_MAGIC,
    .flags = PL_BLOB_UNIQUE | PL_BLOB_NOCOPY,
    .name = "sqlite_connection",
    .release = release_sqlite_connection,
    .compare = 0,
    .write = write_sqlite_connection,
    .acquire = NULL,
    .save = 0,
    .load = 0
};

static int
release_sqlite_connection(atom_t c)
{
    sqlite3 *db = PL_blob_data(c, NULL, NULL);
    if (db && (SQLITE_OK == sqlite3_close_v2(db)))
        return true;
    return false;
}

static int
write_sqlite_connection(IOSTREAM *s, atom_t c, int flags)
{   (void) flags;
    sqlite3 *db = PL_blob_data(c, NULL, NULL);
    if (db) Sfprintf(s, "<sqlite_connection>(%p)", db);
    else Sfprintf(s, "<sqlite_connection>(closed)");
    return true;
}

/* Open connection */
static PL_option_t sqlite_open_options[] = {
    PL_OPTION("mode", OPT_ATOM),
    PL_OPTION("memory", OPT_BOOL),
    PL_OPTION("threaded", OPT_ATOM),
    PL_OPTIONS_END
};

static atom_t SQLITE_OPEN_mode_read;
static atom_t SQLITE_OPEN_mode_write;
static atom_t SQLITE_OPEN_mode_create;
static atom_t SQLITE_OPEN_threaded_single;
static atom_t SQLITE_OPEN_threaded_multi;
static atom_t SQLITE_OPEN_threaded_serialized;

static atom_t SWIPLITE_atom_bv;
static atom_t SWIPLITE_atom_row;
static atom_t SWIPLITE_atom_cols;

static int atom_as_term(atom_t a)
{
    term_t t = PL_new_term_ref();
    if (t && PL_put_atom(t, a))
        return t;
    return 0;
}

foreign_t
pl_sqlite_open(term_t db_name, term_t db_handle, term_t opts)
{
    if (!PL_is_variable(db_handle))
        return PL_uninstantiation_error(db_handle);

    char *name;
    if (!PL_get_chars(db_name, &name,
                CVT_ATOM | CVT_STRING | CVT_LIST
                | CVT_EXCEPTION
                | BUF_STACK
                | REP_UTF8))
        return false;

    atom_t mode = SQLITE_OPEN_mode_read;
    int memory = false;
    atom_t threaded = SQLITE_OPEN_threaded_single;
    if (!PL_scan_options(opts, OPT_ALL,
                "sqlite_open_options", sqlite_open_options,
                &mode, &memory, &threaded))
        return false;

    int flags = SQLITE_OPEN_EXRESCODE;

    if (mode == SQLITE_OPEN_mode_read) {
        flags |= SQLITE_OPEN_READONLY;
    }
    else if (mode == SQLITE_OPEN_mode_write) {
        flags |= SQLITE_OPEN_READWRITE;
    }
    else if (mode == SQLITE_OPEN_mode_create) {
        flags |= SQLITE_OPEN_READWRITE | SQLITE_OPEN_CREATE;
    }
    else return PL_domain_error("mode(read|write|create)",
            atom_as_term(mode));

    if (memory) flags |= SQLITE_OPEN_MEMORY;

    if (threaded == SQLITE_OPEN_threaded_single) ;
    else if (threaded == SQLITE_OPEN_threaded_multi) {
        flags |= SQLITE_OPEN_NOMUTEX;
    }
    else if (threaded == SQLITE_OPEN_threaded_serialized) {
        flags |= SQLITE_OPEN_FULLMUTEX;
    }
    else return PL_domain_error("threaded(single|multi|serialized)",
            atom_as_term(threaded));

    sqlite3 *db;
    if (SQLITE_OK != sqlite3_open_v2(name, &db, flags, NULL)) {
        PL_permission_error("open", sqlite3_errmsg(db), db_name);
        sqlite3_close(db);
        return false;
    }

    return PL_unify_blob(db_handle, db, sizeof(db),
            &sqlite_connection_blob);
}

foreign_t
pl_sqlite_close(term_t db_handle)
{
    size_t len;
    PL_blob_t *type;
    if (!PL_get_blob(db_handle, NULL, &len, &type)
            || type != &sqlite_connection_blob)
    return PL_type_error("sqlite_connection", db_handle);

    if (!len)
        return PL_existence_error("sqlite_connection", db_handle);

    atom_t a;
    return (PL_get_atom(db_handle, &a)
            && PL_free_blob(a));
}

/* Statement blob */
typedef enum stmt_state { /* SQLite statement evaluation state */
    STMT_READY = 0,
    STMT_BUSY,
    STMT_DONE
} stmt_state;

static const char * stmt_state_str(uint32_t i) {
    switch (i) {
        case STMT_READY: return "ready";
        case STMT_BUSY:  return "busy";
        case STMT_DONE:  return "done";
        default:         return "?";
    }
}

typedef struct stmt_data {
    atom_t        symbol;  /* stmt as blob */
    stmt_state    state;   /* evaluation state */
    sqlite3_stmt *stmt;    /* statement handle */
} stmt_data;

static void acquire_stmt(atom_t symbol);
static int  release_stmt(atom_t symbol);
static int  compare_stmt(atom_t a, atom_t b);
static int  write_stmt(IOSTREAM *s, atom_t symbol, int flags);

static PL_blob_t stmt_blob = {
    .magic = PL_BLOB_MAGIC,
    .flags = PL_BLOB_NOCOPY,
    .name  = "sqlite_stmt",
    .release = release_stmt,
    .compare = compare_stmt,
    .write   =   write_stmt,
    .acquire =  acquire_stmt,
    .save = 0,
    .load = 0
};

static void
acquire_stmt(atom_t symbol)
{
    stmt_data *sd = PL_blob_data(symbol, NULL, NULL);
    sd->symbol = symbol;
}

static int
compare_stmt(atom_t a, atom_t b)
{
    const stmt_data *sda = PL_blob_data(a, NULL, NULL);
    const stmt_data *sdb = PL_blob_data(b, NULL, NULL);

    return ( (sda->stmt > sdb->stmt) ? 1 :
             (sda->stmt < sdb->stmt) ? -1 : 0 );
}

static int
release_stmt(atom_t symbol)
{
    stmt_data *sd = PL_blob_data(symbol, NULL, NULL);
    if (sd) sqlite3_finalize(sd->stmt);
    PL_free(sd);
    return true;
}

static int
write_stmt(IOSTREAM *s, atom_t symbol, int flags)
{   (void) flags;
    stmt_data *sd = PL_blob_data(symbol, NULL, NULL);
    if (sd)
        Sfprintf(s, "<sqlite_statement>(%p,%s)",
                sd->stmt,
                stmt_state_str(sd->state));
    else Sfprintf(s, "<sqlite_statement>(finalized)");
    return true;
}

/* Prepare statements */
foreign_t
pl_sqlite_prepare(term_t db_handle, term_t sql_text, term_t stmt_handle)
{
    if (!PL_is_variable(stmt_handle))
        return PL_uninstantiation_error(stmt_handle);

    PL_blob_t *type;
    if (!PL_is_blob(db_handle, &type)
            || type != &sqlite_connection_blob)
        return PL_type_error("sqlite_connection", db_handle);

    sqlite3 *db;
    size_t blob_n;
    if (!PL_get_blob(db_handle, (void *)&db, &blob_n, NULL)
            || !blob_n)
        return PL_existence_error("sqlite_connection", db_handle);


    char *sql;
    size_t sql_len;
    if (!PL_get_nchars(sql_text, &sql_len, &sql,
                CVT_ATOM | CVT_STRING | CVT_LIST
                | CVT_EXCEPTION
                | BUF_STACK
                | REP_UTF8))
        return false;

    sqlite3_stmt *stmt;
    /* Take only the first statement and ignore any trailing SQL */
    if (SQLITE_OK != sqlite3_prepare_v2(db, sql, sql_len+1, &stmt, NULL))
        return sqlite_error(db, "sqlite_prepare");

/* Do not allow SQL parameter list with gaps or anonymous variables

    SELECT ?2, ?1; -- OK
    SELECT ?; -- BAD
    SELECT ?2, ?3; -- BAD

    The last example will report "3" as the bind parameter count
    but there is no value to bind at offset 1. If we allow it,
    this is what we would get:

    ?- sqlite_prepare(DB, "Select ?2, ?3", S),
       sqlite_bind(S, bv(1, 2, 3)),
       sqlite_expanded_sql(S, SQL).
    SQL = "Select 2, 3".

    Note that the first bind value is not used at all!
    */
    for (int i = 1; i <= sqlite3_bind_parameter_count(stmt); i++)
        if (!sqlite3_bind_parameter_name(stmt, i)) {
            sqlite3_finalize(stmt);
            return swiplite_error(
                    "sqlite_prepare_check_parameters",
                    "anonymous or missing ?NNN parameter");
        }

    stmt_data *sd = PL_malloc(sizeof(stmt_data));
    sd->state = STMT_READY;
    sd->stmt = stmt;
    return PL_unify_blob(stmt_handle, sd, sizeof(*sd),
            &stmt_blob);
}

/* Finalize statement */
foreign_t
pl_sqlite_finalize(term_t stmt_handle)
{
    size_t len;
    PL_blob_t *type;

    if (!PL_get_blob(stmt_handle, NULL, &len, &type)
            || type != &stmt_blob)
        return PL_type_error("sqlite_statement", stmt_handle);

    if (!len)
        return PL_existence_error("sqlite_statement", stmt_handle);

    atom_t s;
    return (PL_get_atom(stmt_handle, &s)
            && PL_free_blob(s));
}

/* Helper functions */
static int
stmt_from_handle(term_t stmt_handle, stmt_data **sd)
{
    atom_t s;
    size_t len;
    PL_blob_t *type;
    if (!PL_get_atom(stmt_handle, &s)
            || ((*sd = PL_blob_data(s, &len, &type))
                && type != &stmt_blob))
        return PL_type_error("sqlite_statement", stmt_handle);

    if (!len)
        return PL_existence_error("sqlite_statement", stmt_handle);

    return true;
}

/*
I used the following function to figure out the
value of the "length" argument in `PL_get_nchars()`:
```
foreign_t
pl_text_length(term_t t, term_t n)
{
    char *s;
    size_t s_n;
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
*/
int
bind_value(sqlite3_stmt *stmt, int i, term_t v)
{
    switch (PL_term_type(v)) {
        case PL_NIL:
            if (SQLITE_OK != sqlite3_bind_null(stmt, i))
                return sqlite_error_stmt(stmt, "sqlite_bind_null");
            break;
        case PL_ATOM:
        case PL_STRING:
        case PL_LIST_PAIR: {
            char *s; size_t n;
            if (!PL_get_nchars(v, &n, &s,
                        CVT_ATOM | CVT_STRING | CVT_LIST
                        | CVT_EXCEPTION
                        | BUF_STACK
                        | REP_UTF8 ))
                return false;
            /* we allow embedding NUL in the string
               see: https://sqlite.org/nulinstr.html */
            if (SQLITE_OK != sqlite3_bind_text(stmt, i, s, n,
                    SQLITE_TRANSIENT))
                return sqlite_error_stmt(stmt, "sqlite_bind_text");
            break; }

        case PL_INTEGER: {
            int64_t x;
            if (!PL_get_int64_ex(v, &x)) return false;
            if (SQLITE_OK != sqlite3_bind_int64(stmt, i, x))
                return sqlite_error_stmt(stmt, "sqlite_bind_int");
            break; }

        case PL_FLOAT: {
            double d;
            if (!PL_get_float_ex(v, &d)) return false;
            if (SQLITE_OK != sqlite3_bind_double(stmt, i, d))
                return sqlite_error_stmt(stmt, "sqlite_bind_float");
            break; }

        /* case PL_VARIABLE: not supported */
        default:
            return PL_type_error("[]|text|integer|float", v);
    }
    return true;
}

/* Bind values to prepared statement */
foreign_t
pl_sqlite_bind(term_t stmt_handle, term_t values)
{
    stmt_data *sd;
    if (!stmt_from_handle(stmt_handle, &sd))
        return false;

    if (SQLITE_OK != sqlite3_clear_bindings(sd->stmt))
        return sqlite_error_stmt(sd->stmt, "sqlite_clear_bindings");

    size_t pc = sqlite3_bind_parameter_count(sd->stmt);

    atom_t bv_name;
    size_t bv_arity;
    if (!PL_get_compound_name_arity(values, &bv_name, &bv_arity)
            || SWIPLITE_atom_bv != bv_name
            || pc != bv_arity)
        return PL_type_error("bv/<param_count>", values);

    for (size_t i = 1; i <= pc; i++) {
        term_t v;
        if (!(v = PL_new_term_ref())
                || !_PL_get_arg(i, values, v)
                || !bind_value(sd->stmt, i, v))
            return false;
    }
    return true;
}

/* Reset a statements */
foreign_t
pl_sqlite_reset(term_t stmt_handle)
{
    stmt_data *sd;
    if (!stmt_from_handle(stmt_handle, &sd))
        return false;

    if (SQLITE_OK != sqlite3_reset(sd->stmt))
        return sqlite_error_stmt(sd->stmt, "sqlite_reset");

    sd->state = STMT_READY;
    return true;
}

/* The original text of the statement */
foreign_t
pl_sqlite_sql(term_t stmt_handle, term_t sql)
{
    stmt_data *sd;
    if (!stmt_from_handle(stmt_handle, &sd))
        return false;

    const char *s = sqlite3_sql(sd->stmt);
    if (!s) return sqlite_error_stmt(sd->stmt, "sqlite_sql");

    /* using atom because I might want to cache the
       prepared statements */
    return PL_unify_chars(sql, PL_ATOM | REP_UTF8, -1, s);
}

/* The statement after binding variables to values */
foreign_t
pl_sqlite_expanded_sql(term_t stmt_handle, term_t sql)
{
    stmt_data *sd;
    if (!stmt_from_handle(stmt_handle, &sd))
        return false;

    char *s = sqlite3_expanded_sql(sd->stmt);
    if (!s) return sqlite_error_stmt(sd->stmt, "sqlite_expanded_sql");

    /* using string because sqlite string literals are
       single-quoted and are difficult to read when embedded
       within a Prolog atom */
    int r = PL_unify_chars(sql, PL_STRING | REP_UTF8, -1, s);
    sqlite3_free(s);
    return r;
}

/* Names of the columns in a Select statement */
foreign_t
pl_sqlite_column_names(term_t stmt_handle, term_t colnames)
{
    stmt_data *sd;
    if (!stmt_from_handle(stmt_handle, &sd))
        return false;

    int cc = sqlite3_column_count(sd->stmt);
    if (!cc)
        return PL_unify_compound(colnames,
                PL_new_functor(SWIPLITE_atom_cols, 0));

    term_t r, ra;
    if (!(r=PL_new_term_ref())
            || !(ra=PL_new_term_refs(cc)))
        return false;

    for (int i = 0; i < cc; i++)
        if (!PL_unify_chars(ra+i, PL_ATOM | REP_UTF8,
                    -1, sqlite3_column_name(sd->stmt, i)))
            return false;

    return (PL_cons_functor_v(r,
                PL_new_functor(SWIPLITE_atom_cols, cc), ra)
            && PL_unify(colnames, r));
}

foreign_t
pl_sqlite_eval_noresults(term_t stmt_handle)
{
    stmt_data *sd;
    if (!stmt_from_handle(stmt_handle, &sd)
            || STMT_READY != sd->state)
        return false;

    if (sqlite3_column_count(sd->stmt))
        return swiplite_error("sqlite_eval/1", "non-empty result set");

    int r = sqlite3_step(sd->stmt);
    if (SQLITE_DONE == r
            && SQLITE_OK == sqlite3_reset(sd->stmt))
        return true;

    return sqlite_error_stmt(sd->stmt, "sqlite_eval_no_result");
}

static int columns_to_terms(sqlite3_stmt *stmt, int nc, term_t v0)
{
    for (int i = 0; i < nc; i++) {
        term_t v = v0 + i;
        int r;
        switch (sqlite3_column_type(stmt, i)) {
            case SQLITE_INTEGER:
                r = PL_put_integer(v, sqlite3_column_int64(stmt, i));
                break;
            case SQLITE_FLOAT:
                r = PL_put_float(v, sqlite3_column_double(stmt, i));
                break;
            case SQLITE_TEXT:
                r = PL_put_chars(v, PL_STRING | REP_UTF8,
                        sqlite3_column_bytes(stmt, i),
                        (const char *)sqlite3_column_text(stmt, i));
                break;
            case SQLITE_NULL:
                r = PL_put_nil(v);
                break;
            /* case SQLITE_BLOB: */
            default:
                return PL_type_error("text|integer|float", v);
                break;
        }
        if (!r) return r;
    }
    return true;
}

foreign_t
pl_sqlite_eval_oneresult(term_t stmt_handle, term_t result)
{
    stmt_data *sd;
    if (!stmt_from_handle(stmt_handle, &sd)
            || STMT_READY != sd->state)
        return false;

    if (!sqlite3_column_count(sd->stmt))
        return swiplite_error("sqlite_eval/2", "no columns in result set");

    int step = sqlite3_step(sd->stmt);
    if (SQLITE_DONE == step)
        return swiplite_error("sqlite_eval/2", "no rows in result set");

    if (SQLITE_ROW != step)
        return sqlite_error_stmt(sd->stmt, "sqlite_eval_one_result");

    /* SQLITE_ROW == step */
    int dc = sqlite3_data_count(sd->stmt);
    if (0 == dc)
        return swiplite_error("sqlite_eval/2", "no columns in result row");

    term_t ra = PL_new_term_refs(dc);
    if (!ra || !columns_to_terms(sd->stmt, dc, ra))
        return false;

    step = sqlite3_step(sd->stmt);
    if (SQLITE_ROW == step)
        return swiplite_error("sqlite_eval/2", "additional rows in result set");
    if (SQLITE_DONE == step
            && SQLITE_OK == sqlite3_reset(sd->stmt)) {
        functor_t rf;
        term_t r;
        if (!(rf=PL_new_functor(SWIPLITE_atom_row, dc))
                || !(r=PL_new_term_ref())
                || !PL_cons_functor_v(r, rf, ra))
            return false;
        return PL_unify(result, r);
    }
    return sqlite_error_stmt(sd->stmt, "sqlite_eval_one_result_end");
}

foreign_t
pl_sqlite_eval_someresults(
        term_t stmt_handle,
        term_t n,
        term_t rows,
        term_t rows0)
{
    stmt_data *sd;
    if (!stmt_from_handle(stmt_handle, &sd))
        return false;

    /* "A SELECT statement will always have a positive
       sqlite3_column_count()..."
        - from the sqlite3_column_count() docs */
    if (!sqlite3_column_count(sd->stmt))
        return swiplite_error("sqlite_eval/4", "no columns in result set");

    int cc = sqlite3_column_count(sd->stmt);
    functor_t rf = PL_new_functor(SWIPLITE_atom_row, cc);
    if (!rf) return false;

    term_t tail, head;
    if (!(tail=PL_copy_term_ref(rows))
            || !(head=PL_new_term_ref()))
        return false;

    int vn;
    size_t nn = (size_t)-1;
    if ((vn=PL_is_variable(n))
            || PL_get_size_ex(n, &nn))
        ;

    int nrows = 0;
    int step = SQLITE_ERROR;
    while (nn) {
        if (STMT_DONE == sd->state) {
            if (!PL_unify_nil(rows0)) return false;
            break;
        }

        step = sqlite3_step(sd->stmt);
        sd->state = STMT_BUSY;

        if (SQLITE_DONE == step) {
            if (!PL_unify_nil(rows0)) return false;
            sd->state = STMT_DONE;
            break;
        }
        if (SQLITE_ROW == step && sqlite3_data_count(sd->stmt) == cc) {
            term_t r, ra;
            if (!(r=PL_new_term_ref())
                    || !(ra=PL_new_term_refs(cc))
                    || !columns_to_terms(sd->stmt, cc, ra)
                    || !PL_cons_functor_v(r, rf, ra)
                    || !PL_unify_list(tail, head, tail)
                    || !PL_unify(head, r))
                return false;
            PL_reset_term_refs(r);
            if (++nrows < nn) continue;
            else break;
        }
        /* Neither SQLITE_DONE nor SQLITE_ROW */
        return sqlite_error_stmt(sd->stmt, "sqlite_eval_many");
    }
    if (!PL_unify(rows0, tail)
            || (vn && !PL_unify_int64(n, nrows)))
        return false;

    return true;
}

install_t
install_swiplite()
{
    SQLITE_OPEN_mode_read = PL_new_atom("read");
    SQLITE_OPEN_mode_write = PL_new_atom("write");
    SQLITE_OPEN_mode_create = PL_new_atom("create");
    SQLITE_OPEN_threaded_single = PL_new_atom("single");
    SQLITE_OPEN_threaded_multi = PL_new_atom("multi");
    SQLITE_OPEN_threaded_serialized = PL_new_atom("serialized");

    SWIPLITE_atom_bv = PL_new_atom("bv");
    SWIPLITE_atom_row = PL_new_atom("row");
    SWIPLITE_atom_cols = PL_new_atom("cols");

    PL_register_foreign("sqlite_open", 3, pl_sqlite_open, 0);
    PL_register_foreign("sqlite_close", 1, pl_sqlite_close, 0);
    PL_register_foreign("sqlite_prepare", 3, pl_sqlite_prepare, 0);
    PL_register_foreign("sqlite_finalize", 1, pl_sqlite_finalize, 0);
    PL_register_foreign("sqlite_bind", 2, pl_sqlite_bind, 0);
    PL_register_foreign("sqlite_reset", 1, pl_sqlite_reset, 0);
    PL_register_foreign("sqlite_sql", 2, pl_sqlite_sql, 0);
    PL_register_foreign("sqlite_expanded_sql", 2, pl_sqlite_expanded_sql, 0);
    PL_register_foreign("sqlite_column_names", 2, pl_sqlite_column_names, 0);
    PL_register_foreign("sqlite_eval", 1, pl_sqlite_eval_noresults, 0);
    PL_register_foreign("sqlite_eval", 2, pl_sqlite_eval_oneresult, 0);
    PL_register_foreign("sqlite_eval", 4, pl_sqlite_eval_someresults, 0);
}
