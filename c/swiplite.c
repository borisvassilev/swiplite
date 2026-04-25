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
sqlite_error(sqlite3 *db, const char *more)
{
    term_t e;
    return ( (e=PL_new_term_ref())
            && PL_unify_term(e,
                PL_FUNCTOR_CHARS, "error", 2,
                  PL_FUNCTOR_CHARS, "sqlite_error", 4,
                    PL_CHARS, more,
                    PL_INT, sqlite3_errcode(db),
                    PL_CHARS, sqlite3_errstr(sqlite3_errcode(db)),
                    PL_CHARS, sqlite3_errmsg(db),
                  PL_VARIABLE)
            && PL_raise_exception(e));
}

static int
sqlite_error_stmt(sqlite3_stmt *stmt, const char *more)
{
    return sqlite_error(sqlite3_db_handle(stmt), more);
}

static int
swiplite_error(const char *error, const char *more)
{
    term_t e;
    return ( (e=PL_new_term_ref())
            && PL_unify_term(e,
                PL_FUNCTOR_CHARS, "error", 2,
                  PL_FUNCTOR_CHARS, "swiplite_error", 2,
                    PL_CHARS, error,
                    PL_CHARS, more,
                  PL_VARIABLE)
            && PL_raise_exception(e));
}

/* Initialize and shutdown */

foreign_t
pl_sqlite_initialize(void)
{
    int r = sqlite3_initialize();
    if (SQLITE_OK == r) return true;
    return swiplite_error("sqlite3_initialize", sqlite3_errstr(r));
}

foreign_t
pl_sqlite_shutdown(void)
{
    return (SQLITE_OK == sqlite3_shutdown());
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
    PL_OPTION("foreign_keys", OPT_BOOL),
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

static int
foreign_keys_callback(void *a, int argc, char **argv, char **azColName)
{
/* supposed to be called exactly once, with first argument pre-set to false */
    int supported = *(int *)a;
    if (false == supported) {
        *(int *)a = true;
        return 0;
    }
    return 1;
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
    int foreign_keys = true;
    atom_t threaded = SQLITE_OPEN_threaded_single;
    if (!PL_scan_options(opts, OPT_ALL,
                "sqlite_open_options", sqlite_open_options,
                &mode, &memory, &threaded, &foreign_keys))
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

    if (foreign_keys) {
        int foreign_keys_supported = false;
        char *err_str = 0;
        if (SQLITE_OK != sqlite3_exec(db,
                    "PRAGMA foreign_keys; PRAGMA foreign_keys = ON;",
                    foreign_keys_callback, &foreign_keys_supported,
                    &err_str)) {
            sqlite3_close(db);
            return swiplite_error(err_str, "foreign_keys(true)");
        }
        if (false == foreign_keys_supported) {
            sqlite3_close(db);
            return swiplite_error("Foreign keys not supported", name);
        }
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
static int db_from_handle(term_t db_handle, sqlite3 **db)
{
    PL_blob_t *type;
    if (!PL_is_blob(db_handle, &type)
            || type != &sqlite_connection_blob)
        return PL_type_error("sqlite_connection", db_handle);

    size_t blob_n;
    if (!PL_get_blob(db_handle, (void *)db, &blob_n, NULL)
            || !blob_n)
        return PL_existence_error("sqlite_connection", db_handle);

    return true;
}

static PL_option_t sqlite_prepare_options[] = {
    PL_OPTION("bind_parameter_count", OPT_TERM),
    PL_OPTION("rest", OPT_TERM),
    PL_OPTIONS_END
};

foreign_t
pl_sqlite_prepare(
        term_t db_handle, term_t sql_text,
        term_t stmt_handle,
        term_t opts)
{
    if (!PL_is_variable(stmt_handle))
        return PL_uninstantiation_error(stmt_handle);

    sqlite3 *db;
    if (!db_from_handle(db_handle, &db)) return false;

    char *sql;
    size_t sql_len;
    if (!PL_get_nchars(sql_text, &sql_len, &sql,
                CVT_ATOM | CVT_STRING | CVT_LIST
                | CVT_EXCEPTION
                | BUF_STACK
                | REP_UTF8))
        return false;

    term_t nbind = 0;
    term_t sql_rest = 0;
    if (!PL_scan_options(opts, OPT_ALL,
                "sqlite_prepare_options", sqlite_prepare_options,
                &nbind, &sql_rest))
        return false;

    sqlite3_stmt *stmt;
    if (sql_rest) {
        const char *rest;
        if (SQLITE_OK != sqlite3_prepare_v2(db, sql, sql_len+1, &stmt, &rest))
            return sqlite_error(db, "sqlite_prepare");
        if (!PL_unify_chars(sql_rest, PL_STRING | REP_UTF8, -1, rest))
            return false;
    } else {
        if (SQLITE_OK != sqlite3_prepare_v2(db, sql, sql_len+1, &stmt, NULL))
            return sqlite_error(db, "sqlite_prepare");
    }

    if (NULL == stmt) return false;

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
    int bind_parameter_count = sqlite3_bind_parameter_count(stmt);
    if (nbind) {
        if (!PL_unify_integer(nbind, bind_parameter_count))
            return false;
    }
    for (int i = 1; i <= bind_parameter_count; i++)
        if (!sqlite3_bind_parameter_name(stmt, i)) {
            sqlite3_finalize(stmt);
            return swiplite_error(
                    "anonymous or missing ?NNN parameter", "prepare");
        }

    stmt_data *sd = PL_malloc(sizeof(stmt_data));
    sd->state = STMT_READY;
    sd->stmt = stmt;
    return PL_unify_blob(stmt_handle, sd, sizeof(*sd), &stmt_blob);
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

int
bind_value(sqlite3_stmt *stmt, int i, term_t v)
{
    switch (PL_term_type(v)) {
        case PL_NIL:
            if (SQLITE_OK != sqlite3_bind_null(stmt, i))
                return sqlite_error_stmt(stmt, "bind_null");
            break;
        case PL_ATOM:
        case PL_STRING:
        case PL_LIST_PAIR: {
            char *s; size_t n;
            /* we allow embedding NUL in the string
               see: https://sqlite.org/nulinstr.html */
            if (!PL_get_nchars(v, &n, &s,
                        CVT_ATOM | CVT_STRING | CVT_LIST
                        | CVT_EXCEPTION
                        | BUF_STACK
                        | REP_UTF8 ))
                return false;
            if (SQLITE_OK != sqlite3_bind_text(stmt, i, s, n,
                    SQLITE_TRANSIENT))
                return sqlite_error_stmt(stmt, "bind_text");
            break; }

        case PL_INTEGER: {
            int64_t x;
            if (!PL_get_int64_ex(v, &x)) return false;
            if (SQLITE_OK != sqlite3_bind_int64(stmt, i, x))
                return sqlite_error_stmt(stmt, "bind_int");
            break; }

        case PL_FLOAT: {
            double d;
            if (!PL_get_float_ex(v, &d)) return false;
            if (SQLITE_OK != sqlite3_bind_double(stmt, i, d))
                return sqlite_error_stmt(stmt, "bind_float");
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
        return sqlite_error_stmt(sd->stmt, "clear_bindings");

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
        return sqlite_error_stmt(sd->stmt, "reset");

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
    if (!s) return sqlite_error_stmt(sd->stmt, "sql");

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
    if (!s) return sqlite_error_stmt(sd->stmt, "expanded_sql");

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
    if (!(r=PL_copy_term_ref(colnames))
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
        return swiplite_error("non-empty result set", "command");

    int r = sqlite3_step(sd->stmt);
    if (SQLITE_DONE == r
            && SQLITE_OK == sqlite3_reset(sd->stmt))
        return true;

    return sqlite_error_stmt(sd->stmt, "command");
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
        return swiplite_error("no columns in result set", "select_one");

    int step = sqlite3_step(sd->stmt);
    if (SQLITE_DONE == step)
        return swiplite_error("no rows in result set", "select_one");

    if (SQLITE_ROW != step)
        return sqlite_error_stmt(sd->stmt, "select_one");

    /* SQLITE_ROW == step */
    int dc = sqlite3_data_count(sd->stmt);
    if (0 >= dc)
        return swiplite_error("no columns in result row", "select_one");

    term_t ra = PL_new_term_refs(dc);
    if (!ra || !columns_to_terms(sd->stmt, dc, ra))
        return false;

    step = sqlite3_step(sd->stmt);
    if (SQLITE_ROW == step)
        return swiplite_error("additional rows in result set", "select_one");
    if (SQLITE_DONE == step
            && SQLITE_OK == sqlite3_reset(sd->stmt)) {
        functor_t rf;
        term_t r;
        if (!(rf=PL_new_functor(SWIPLITE_atom_row, dc))
                || !(r=PL_copy_term_ref(result))
                || !PL_cons_functor_v(r, rf, ra))
            return false;
        return PL_unify(result, r);
    }
    return sqlite_error_stmt(sd->stmt, "select_one");
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
    int cc = sqlite3_column_count(sd->stmt);
    if (!cc)
        return swiplite_error("no columns in result set", "select_many");

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
        return sqlite_error_stmt(sd->stmt, "select_many");
    }
    if (!PL_unify(rows0, tail)
            || (vn && !PL_unify_int64(n, nrows)))
        return false;

    return true;
}

typedef struct stmt_context {
    stmt_data *sd;
    int cc;
    functor_t rf;
} stmt_context;

foreign_t
pl_sqlite_eval_row(term_t stmt_handle, term_t row, control_t ctrl_handle)
{   stmt_context *sc;
    int step;

    switch (PL_foreign_control(ctrl_handle)) {
        case PL_FIRST_CALL: {
            stmt_data *sd;
            if (!stmt_from_handle(stmt_handle, &sd))
                return false;
            if (STMT_READY != sd->state)
                return swiplite_error("statement not ready",
                        stmt_state_str(sd->state));

            int cc = sqlite3_column_count(sd->stmt);
            if (0 >= cc)
                return swiplite_error("no columns in result set", "select_row");

            /* first step */
            step = sqlite3_step(sd->stmt);
            if (SQLITE_DONE == step
                    && SQLITE_OK == sqlite3_reset(sd->stmt)) {
                sd->state = STMT_READY;
                return false;
            }
            functor_t rf;
            if (SQLITE_ROW == step
                    && sqlite3_data_count(sd->stmt) == cc
                    && (rf=PL_new_functor(SWIPLITE_atom_row, cc)) ) {
                sd->state = STMT_BUSY;
                if (!(sc = malloc(sizeof *sc)))
                    return PL_resource_error("memory");
                sc->sd = sd;
                sc->cc = cc;
                sc->rf = rf;
                break;
            }
            return sqlite_error_stmt(sd->stmt, "select_row");
        }

        case PL_REDO:
            sc = PL_foreign_context_address(ctrl_handle);
            break;

        case PL_PRUNED:
            sc = PL_foreign_context_address(ctrl_handle);
            if (SQLITE_OK == sqlite3_reset(sc->sd->stmt))
                sc->sd->state = STMT_READY;
            PL_free(sc);
            return true;
    }

    term_t r, ra;
    if (!(r=PL_copy_term_ref(row))
            || !(ra=PL_new_term_refs(sc->cc))
            || !columns_to_terms(sc->sd->stmt, sc->cc, ra)
            || !PL_cons_functor_v(r, sc->rf, ra)
            || !PL_unify(row, r))
        return false;
    /* next step */
    step = sqlite3_step(sc->sd->stmt);
    if (SQLITE_ROW == step
            && sqlite3_data_count(sc->sd->stmt) == sc->cc)
        PL_retry_address(sc);

    if (SQLITE_DONE == step) {
        if (SQLITE_OK == sqlite3_reset(sc->sd->stmt))
            sc->sd->state = STMT_READY;
        PL_free(sc);
        return true;
    }

    return sqlite_error_stmt(sc->sd->stmt, "select_row_next");
}

static atom_t SQLITE_STATUS_memory_used;
static atom_t SQLITE_STATUS_pagecache_used;
static atom_t SQLITE_STATUS_pagecache_overflow;
static atom_t SQLITE_STATUS_malloc_size;
static atom_t SQLITE_STATUS_parser_stack;
static atom_t SQLITE_STATUS_pagecache_size;
static atom_t SQLITE_STATUS_malloc_count;

static int sqlite_status_code(atom_t a)
{
    if (SQLITE_STATUS_memory_used        == a) return SQLITE_STATUS_MEMORY_USED;
    if (SQLITE_STATUS_pagecache_used     == a) return SQLITE_STATUS_PAGECACHE_USED;
    if (SQLITE_STATUS_pagecache_overflow == a) return SQLITE_STATUS_PAGECACHE_OVERFLOW;
    if (SQLITE_STATUS_malloc_size        == a) return SQLITE_STATUS_MALLOC_SIZE;
    if (SQLITE_STATUS_parser_stack       == a) return SQLITE_STATUS_PARSER_STACK;
    if (SQLITE_STATUS_pagecache_size     == a) return SQLITE_STATUS_PAGECACHE_SIZE;
    if (SQLITE_STATUS_malloc_count       == a) return SQLITE_STATUS_MALLOC_COUNT;

    return -1;
}

foreign_t
pl_sqlite_status(term_t op, term_t current, term_t highwater, term_t reset)
{
    atom_t op_atom;
    if (!PL_get_atom_ex(op, &op_atom)) return false;

    int code = sqlite_status_code(op_atom);
    if (-1 == code)
        return PL_domain_error("SQLITE_STATUS_<code>", op);

    if (!PL_is_variable(current))
        return PL_uninstantiation_error(current);

    if (!PL_is_variable(highwater))
        return PL_uninstantiation_error(highwater);

    int r = false;
    if (!PL_get_bool_ex(reset, &r)) return false;

    sqlite_int64 c;
    sqlite_int64 h;

    int sqlite_r = sqlite3_status64(code, &c, &h, r);
    if (SQLITE_OK != sqlite_r)
        return swiplite_error("sqlite_status", sqlite3_errstr(sqlite_r));

    return (PL_unify_int64(current, c) && PL_unify_int64(highwater, h));
}

static atom_t SQLITE_DBSTATUS_lookaside_used;
static atom_t SQLITE_DBSTATUS_cache_used;
static atom_t SQLITE_DBSTATUS_schema_used;
static atom_t SQLITE_DBSTATUS_stmt_used;
static atom_t SQLITE_DBSTATUS_lookaside_hit;
static atom_t SQLITE_DBSTATUS_lookaside_miss_size;
static atom_t SQLITE_DBSTATUS_lookaside_miss_full;
static atom_t SQLITE_DBSTATUS_cache_hit;
static atom_t SQLITE_DBSTATUS_cache_miss;
static atom_t SQLITE_DBSTATUS_cache_write;
static atom_t SQLITE_DBSTATUS_deferred_fks;
static atom_t SQLITE_DBSTATUS_cache_used_shared;
static atom_t SQLITE_DBSTATUS_cache_spill;

static int sqlite_dbstatus_code(atom_t op)
{
    if (SQLITE_DBSTATUS_lookaside_used      == op) return SQLITE_DBSTATUS_LOOKASIDE_USED;
    if (SQLITE_DBSTATUS_cache_used          == op) return SQLITE_DBSTATUS_CACHE_USED;
    if (SQLITE_DBSTATUS_schema_used         == op) return SQLITE_DBSTATUS_SCHEMA_USED;
    if (SQLITE_DBSTATUS_stmt_used           == op) return SQLITE_DBSTATUS_STMT_USED;
    if (SQLITE_DBSTATUS_lookaside_hit       == op) return SQLITE_DBSTATUS_LOOKASIDE_HIT;
    if (SQLITE_DBSTATUS_lookaside_miss_size == op) return SQLITE_DBSTATUS_LOOKASIDE_MISS_SIZE;
    if (SQLITE_DBSTATUS_lookaside_miss_full == op) return SQLITE_DBSTATUS_LOOKASIDE_MISS_FULL;
    if (SQLITE_DBSTATUS_cache_hit           == op) return SQLITE_DBSTATUS_CACHE_HIT;
    if (SQLITE_DBSTATUS_cache_miss          == op) return SQLITE_DBSTATUS_CACHE_MISS;
    if (SQLITE_DBSTATUS_cache_write         == op) return SQLITE_DBSTATUS_CACHE_WRITE;
    if (SQLITE_DBSTATUS_deferred_fks        == op) return SQLITE_DBSTATUS_DEFERRED_FKS;
    if (SQLITE_DBSTATUS_cache_used_shared   == op) return SQLITE_DBSTATUS_CACHE_USED_SHARED;
    if (SQLITE_DBSTATUS_cache_spill         == op) return SQLITE_DBSTATUS_CACHE_SPILL;

    return -1;
}

foreign_t
pl_sqlite_db_status(term_t db_handle,
        term_t op, term_t current, term_t highwater, term_t reset)
{
    sqlite3 *db;
    if (!db_from_handle(db_handle, &db)) return false;

    atom_t op_atom;
    if (!PL_get_atom_ex(op, &op_atom)) return false;
    int code = sqlite_dbstatus_code(op_atom);
    if (-1 == code)
        return PL_domain_error("SQLITE_DBSTATUS_<code>", op);

    if (!PL_is_variable(current))
        return PL_uninstantiation_error(current);

    if (!PL_is_variable(highwater))
        return PL_uninstantiation_error(highwater);

    int r = false;
    if (!PL_get_bool_ex(reset, &r)) return false;

#if SQLITE_VERSION_NUMBER < 3051001
# define STATUS_TYPE int
# define STATUS_FETCH sqlite3_db_status
# define STATUS_UNIFY PL_unify_integer
#else
# define STATUS_TYPE sqlite_int64
# define STATUS_FETCH sqlite3_db_status64
# define STATUS_UNIFY PL_unify_int64
#endif
    STATUS_TYPE c;
    STATUS_TYPE h;

    int sqlite_r = STATUS_FETCH(db, code, &c, &h, r);
    if (SQLITE_OK != sqlite_r)
        return swiplite_error("sqlite_db_status", sqlite3_errstr(sqlite_r));

    return (STATUS_UNIFY(current, c) && STATUS_UNIFY(highwater, h));
#undef STATUS_TYPE
#undef STATUS_FETCH
#undef STATUS_UNIFY
}

static atom_t SQLITE_STMTSTATUS_fullscan_step;
static atom_t SQLITE_STMTSTATUS_sort;
static atom_t SQLITE_STMTSTATUS_autoindex;
static atom_t SQLITE_STMTSTATUS_vm_step;
static atom_t SQLITE_STMTSTATUS_reprepare;
static atom_t SQLITE_STMTSTATUS_run;
static atom_t SQLITE_STMTSTATUS_filter_miss;
static atom_t SQLITE_STMTSTATUS_filter_hit;
static atom_t SQLITE_STMTSTATUS_memused;

static int sqlite_stmtstatus_code(atom_t op)
{
    if (SQLITE_STMTSTATUS_fullscan_step == op) return SQLITE_STMTSTATUS_FULLSCAN_STEP;
    if (SQLITE_STMTSTATUS_sort          == op) return SQLITE_STMTSTATUS_SORT;
    if (SQLITE_STMTSTATUS_autoindex     == op) return SQLITE_STMTSTATUS_AUTOINDEX;
    if (SQLITE_STMTSTATUS_vm_step       == op) return SQLITE_STMTSTATUS_VM_STEP;
    if (SQLITE_STMTSTATUS_reprepare     == op) return SQLITE_STMTSTATUS_REPREPARE;
    if (SQLITE_STMTSTATUS_run           == op) return SQLITE_STMTSTATUS_RUN;
    if (SQLITE_STMTSTATUS_filter_miss   == op) return SQLITE_STMTSTATUS_FILTER_MISS;
    if (SQLITE_STMTSTATUS_filter_hit    == op) return SQLITE_STMTSTATUS_FILTER_HIT;
    if (SQLITE_STMTSTATUS_memused       == op) return SQLITE_STMTSTATUS_MEMUSED;

    return -1;
}

foreign_t
pl_sqlite_stmt_status(term_t stmt_handle, term_t op, term_t current, term_t reset)
{
    stmt_data *sd;
    if (!stmt_from_handle(stmt_handle, &sd))
        return false;

    atom_t op_atom;
    if (!PL_get_atom_ex(op, &op_atom)) return false;
    int code = sqlite_stmtstatus_code(op_atom);
    if (-1 == code)
        return PL_domain_error("SQLITE_STMTSTATUS_<code>", op);

    if (!PL_is_variable(current))
        return PL_uninstantiation_error(current);

    int r = false;
    if (!PL_get_bool_ex(reset, &r)) return false;

    int c = sqlite3_stmt_status(sd->stmt, code, r);

    return PL_unify_integer(current, c);
}

install_t
install_swiplite()
{
    SQLITE_OPEN_mode_read            = PL_new_atom("read");
    SQLITE_OPEN_mode_write           = PL_new_atom("write");
    SQLITE_OPEN_mode_create          = PL_new_atom("create");
    SQLITE_OPEN_threaded_single      = PL_new_atom("single");
    SQLITE_OPEN_threaded_multi       = PL_new_atom("multi");
    SQLITE_OPEN_threaded_serialized  = PL_new_atom("serialized");

    SQLITE_STATUS_memory_used        = PL_new_atom("memory_used");
    SQLITE_STATUS_pagecache_used     = PL_new_atom("pagecache_used");
    SQLITE_STATUS_pagecache_overflow = PL_new_atom("pagecache_overflow");
    SQLITE_STATUS_malloc_size        = PL_new_atom("malloc_size");
    SQLITE_STATUS_parser_stack       = PL_new_atom("parser_stack");
    SQLITE_STATUS_pagecache_size     = PL_new_atom("pagecache_size");
    SQLITE_STATUS_malloc_count       = PL_new_atom("malloc_count");

    SQLITE_DBSTATUS_lookaside_used      = PL_new_atom("lookaside_used");
    SQLITE_DBSTATUS_cache_used          = PL_new_atom("cache_used");
    SQLITE_DBSTATUS_schema_used         = PL_new_atom("schema_used");
    SQLITE_DBSTATUS_stmt_used           = PL_new_atom("stmt_used");
    SQLITE_DBSTATUS_lookaside_hit       = PL_new_atom("lookaside_hit");
    SQLITE_DBSTATUS_lookaside_miss_size = PL_new_atom("lookaside_miss_size");
    SQLITE_DBSTATUS_lookaside_miss_full = PL_new_atom("lookaside_miss_full");
    SQLITE_DBSTATUS_cache_hit           = PL_new_atom("cache_hit");
    SQLITE_DBSTATUS_cache_miss          = PL_new_atom("cache_miss");
    SQLITE_DBSTATUS_cache_write         = PL_new_atom("cache_write");
    SQLITE_DBSTATUS_deferred_fks        = PL_new_atom("deferred_fks");
    SQLITE_DBSTATUS_cache_used_shared   = PL_new_atom("cache_used_shared");
    SQLITE_DBSTATUS_cache_spill         = PL_new_atom("cache_spill");

    SQLITE_STMTSTATUS_fullscan_step     = PL_new_atom("fullscan_step");
    SQLITE_STMTSTATUS_sort              = PL_new_atom("sort");
    SQLITE_STMTSTATUS_autoindex         = PL_new_atom("autoindex");
    SQLITE_STMTSTATUS_vm_step           = PL_new_atom("vm_step");
    SQLITE_STMTSTATUS_reprepare         = PL_new_atom("reprepare");
    SQLITE_STMTSTATUS_run               = PL_new_atom("run");
    SQLITE_STMTSTATUS_filter_miss       = PL_new_atom("filter_miss");
    SQLITE_STMTSTATUS_filter_hit        = PL_new_atom("filter_hit");
    SQLITE_STMTSTATUS_memused           = PL_new_atom("memused");

    SWIPLITE_atom_bv = PL_new_atom("bv");
    SWIPLITE_atom_row = PL_new_atom("row");
    SWIPLITE_atom_cols = PL_new_atom("cols");

    PL_register_foreign("sqlite_initialize", 0, pl_sqlite_initialize, 0);
    PL_register_foreign("sqlite_shutdown", 0, pl_sqlite_shutdown, 0);
    PL_register_foreign("sqlite_open", 3, pl_sqlite_open, 0);
    PL_register_foreign("sqlite_close", 1, pl_sqlite_close, 0);
    PL_register_foreign("sqlite_prepare", 4, pl_sqlite_prepare, 0);
    PL_register_foreign("sqlite_finalize", 1, pl_sqlite_finalize, 0);
    PL_register_foreign("sqlite_bind", 2, pl_sqlite_bind, 0);
    PL_register_foreign("sqlite_reset", 1, pl_sqlite_reset, 0);
    PL_register_foreign("sqlite_sql", 2, pl_sqlite_sql, 0);
    PL_register_foreign("sqlite_expanded_sql", 2, pl_sqlite_expanded_sql, 0);
    PL_register_foreign("sqlite_column_names", 2, pl_sqlite_column_names, 0);
    PL_register_foreign("sqlite_do", 1, pl_sqlite_eval_noresults, 0);
    PL_register_foreign("sqlite_one", 2, pl_sqlite_eval_oneresult, 0);
    PL_register_foreign("sqlite_many", 4, pl_sqlite_eval_someresults, 0);
    PL_register_foreign("sqlite_row", 2, pl_sqlite_eval_row, PL_FA_NONDETERMINISTIC);
    PL_register_foreign("sqlite_status", 4, pl_sqlite_status, 0);
    PL_register_foreign("sqlite_db_status", 5, pl_sqlite_db_status, 0);
    PL_register_foreign("sqlite_stmt_status", 4, pl_sqlite_stmt_status, 0);
}
