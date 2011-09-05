/* Copyright (C) 2011 by Alexander Pantyukhov <alwx.main@gmail.com>
                         Dmitry Groshev       <lambdadmitry@gmail.com>
   Permission is hereby granted, free of charge, to any person obtaining a copy
   of this software and associated documentation files (the "Software"), to deal
   in the Software without restriction, including without limitation the rights
   to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
   copies of the Software, and to permit persons to whom the Software is
   furnished to do so, subject to the following conditions:

   The above copyright notice and this permission notice shall be included in
   all copies or substantial portions of the Software.

   THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
   IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
   FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
   AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
   LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
   OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
   THE SOFTWARE. */

#include <stdio.h>
#include <string.h>
#include <inttypes.h>
#include <netinet/in.h>
#include "erl_nif.h"

#if BITNESS == 12
#define ERL_MAKE_ELEM   enif_make_uint
#define ERL_GET_ELEM    enif_get_uint64
#define MAX_VAL         4096
#define EMPTY_VAL       UINT16_MAX
#define BITNESS_NAME    "small"
#define INIT_STR        ERL_NIF_INIT(ecirca_small, functions, &init, \
                                         NULL, NULL, NULL)
typedef uint16_t        elem_t;
#endif
#if BITNESS == 28
#define ERL_MAKE_ELEM   enif_make_uint
#define ERL_GET_ELEM    enif_get_uint64
#define MAX_VAL         134217728
#define EMPTY_VAL       UINT32_MAX
#define BITNESS_NAME    "medium"
#define INIT_STR        ERL_NIF_INIT(ecirca_medium, functions, &init, \
                                         NULL, NULL, NULL)
typedef uint32_t        elem_t;
#endif
#if BITNESS == 60
#define ERL_MAKE_ELEM   enif_make_uint64
#define ERL_GET_ELEM    enif_get_uint64
#define MAX_VAL         576460752303423487
#define EMPTY_VAL       UINT64_MAX
#define BITNESS_NAME    "large"
#define INIT_STR        ERL_NIF_INIT(ecirca_large, functions, &init, \
                                         NULL, NULL, NULL)
typedef uint64_t        elem_t;
#endif

#define ERL_MAKE_SIZE   enif_make_uint
#define ERL_GET_SIZE    enif_get_uint
#define MAX_SLICE       1000000
#define MAX_SIZE        1000000

#define ATOM_OK         enif_make_atom(env, "ok")
#define ATOM_ERROR      enif_make_atom(env, "error")
#define ATOM_OVERFLOW   enif_make_atom(env, "overflow")
#define BADARG          enif_make_badarg(env)
#define TUPLE2(A, B)    enif_make_tuple2(env, A, B)
#define ATOM(A)         enif_make_atom(env, A)


/* bigger vals will be represented as bigints in erlang VM */
/* TODO: add check for this value in all functions */

#define PUT_BUF(BUF, OFFSET, VAL) \
    *((typeof(VAL)*)(BUF + OFFSET)) = VAL; OFFSET += sizeof(VAL);
#define GET_BUF(BUF, OFFSET, VAL) \
    VAL = *((typeof(VAL)*)(BUF + OFFSET)); OFFSET += sizeof(VAL);

typedef uint32_t            length_t;
typedef uint32_t            count_t;
typedef unsigned short int  bool_t;

static const char emptystr[] = "empty";

/* data structures */
typedef enum {
    ecirca_last,
    ecirca_max,
    ecirca_min,
    ecirca_avg,
    ecirca_sum
} ecirca_type;

typedef struct {
    length_t     begin;
    elem_t      *circa;
    count_t     *count;
    length_t     size;
    bool_t       filled;
    ecirca_type  type;
} circactx;

ErlNifResourceType* circa_type;

/* additional functions */
static int set_type(char*, ecirca_type*);

/* get array index with respect to array bounds */
length_t
get_index(circactx * ctx, length_t i) {
    length_t index;

    if (i > ctx->begin) {
        index = ctx->size + ctx->begin - i;
    } else {
        index = ctx->begin - i;
    }
    return index;
}

/* ecirca destructor */
void
circactx_dtor(ErlNifEnv* env, void* obj) {
    circactx * ctx = (circactx *) obj;
    enif_free(ctx->circa);
}

/* creating resource type on load */
static int
init(ErlNifEnv* env, void** priv, ERL_NIF_TERM info) {
    int flags = ERL_NIF_RT_CREATE | ERL_NIF_RT_TAKEOVER;

    circa_type = enif_open_resource_type(env, NULL, "circa",
                                         circactx_dtor, flags, NULL);
    if (circa_type == NULL) return 1;
    return 0;
}

static ERL_NIF_TERM
new(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    circactx * ctx;
    ERL_NIF_TERM ret;
    length_t size;
    char typestr[5];
    ecirca_type type;

    if (argc != 2) {
        return BADARG;
    }
    if (!ERL_GET_SIZE(env, argv[0], &size)) {
        return BADARG;
    }
    if (size == 0) {
        return BADARG;
    }
    if (size > MAX_SIZE) {
        return TUPLE2(ATOM_ERROR, ATOM_OVERFLOW);
    }
    if (!enif_get_atom(env, argv[1], typestr, 5, ERL_NIF_LATIN1)) {
		return BADARG;
	}
    if (!set_type(typestr, &type)) {
        return BADARG;
    }

    ctx         = enif_alloc_resource(circa_type, sizeof(circactx));
    ctx->begin  = 0;
    ctx->circa  = enif_alloc(sizeof(elem_t) * size);
    ctx->size   = size;
    ctx->filled = 0;
    ctx->type   = type;

    memset(ctx->circa, 0xFF, sizeof(elem_t) * size);

    ret = enif_make_resource(env, ctx);
    enif_release_resource(ctx);

    return ret;
}

static ERL_NIF_TERM
push(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    circactx * ctx;
    char emptycmp[6];
    elem_t val;
    uint64_t val_big;

    if (argc != 2) {
        return BADARG;
    }
    if (!ERL_GET_ELEM(env, argv[1], &val_big)) {
        if (!enif_get_atom(env, argv[1], emptycmp, 6, ERL_NIF_LATIN1)) {
            return BADARG;
        } else {
            if (!strcmp(emptycmp, emptystr)) {
                val = EMPTY_VAL;
            } else {
                return BADARG;
            }
        }
    } else {
        if (val_big > MAX_VAL) {
            return TUPLE2(ATOM_ERROR, ATOM_OVERFLOW);
        }
        val = (elem_t) val_big;
    }
    if (!enif_get_resource(env, argv[0], circa_type, (void**) &ctx)) {
        return BADARG;
    }

    if (++ctx->begin >= ctx->size + 1) {
        ctx->begin  = 1;
        ctx->filled = 1;
    }

    ctx->circa[ctx->begin - 1] = val;

    return ATOM_OK;
}

static ERL_NIF_TERM
get(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    circactx * ctx;
    length_t i, idx;
    ERL_NIF_TERM ret;

    if (argc != 2) {
        return BADARG;
    }
    if (!ERL_GET_SIZE(env, argv[1], &i)) {
        return BADARG;
    }
    if (!enif_get_resource(env, argv[0], circa_type, (void**) &ctx)) {
        return BADARG;
    }

    if (i > ctx->size || i == 0) {
        return BADARG;
    }

    idx = get_index(ctx, i);
    if (ctx->circa[idx] == EMPTY_VAL) {
        ret = ATOM("empty");
    } else {
        ret = ERL_MAKE_ELEM(env, ctx->circa[idx]);
    }
    return TUPLE2(ATOM_OK, ret);
}

static ERL_NIF_TERM
set(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    circactx * ctx;
    char emptycmp[6];
    length_t i, idx;
    elem_t val, old_val, new_val;
    uint64_t val_big;

    if (argc != 3) {
        return BADARG;
    }
    if (!enif_get_resource(env, argv[0], circa_type, (void**) &ctx)) {
        return BADARG;
    }
    if (i > ctx->size || i == 0) {
        return BADARG;
    }
    if (!ERL_GET_SIZE(env, argv[1], &i)) {
        return BADARG;
    }
    if (!ERL_GET_ELEM(env, argv[2], &val_big)) {
        if (!enif_get_atom(env, argv[2], emptycmp, 6, ERL_NIF_LATIN1)) {
            return BADARG;
        } else {
            if (!strcmp(emptycmp, emptystr)) {
                val = EMPTY_VAL;
            } else {
                return BADARG;
            }
        }
    } else {
        if (val_big > MAX_VAL) {
           return TUPLE2(ATOM_OK, ATOM_OVERFLOW);
        }
        val = (elem_t) val_big;
    }

    idx = get_index(ctx, i);

    old_val = ctx->circa[idx];
    ctx->circa[idx] = val;
    new_val = ctx->circa[idx];

    return TUPLE2(ATOM_OK, TUPLE2(ERL_MAKE_ELEM(env, old_val),
                                  ERL_MAKE_ELEM(env, new_val)));
}

static ERL_NIF_TERM
update(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    circactx * ctx;
    char emptycmp[6];
    length_t i, idx;
    elem_t val, old_val, new_val;
    uint64_t val_big;

    if (argc != 3) {
        return BADARG;
    }
    if (!ERL_GET_SIZE(env, argv[1], &i)) {
        return BADARG;
    }
    if (!ERL_GET_ELEM(env, argv[2], &val_big)) {
        if (!enif_get_atom(env, argv[2], emptycmp, 6, ERL_NIF_LATIN1)) {
            return BADARG;
        } else {
            if (!strcmp(emptycmp, emptystr)) {
                val = EMPTY_VAL;
            } else {
                return BADARG;
            }
        }
    } else {
        if (val_big > MAX_VAL) {
            return TUPLE2(ATOM_ERROR, ATOM_OVERFLOW);
        }
        val = (elem_t) val_big;
    }
    if (!enif_get_resource(env, argv[0], circa_type, (void**) &ctx)) {
        return BADARG;
    }
    if (i > ctx->size || i == 0) {
        return BADARG;
    }

    idx = get_index(ctx, i);

    old_val = ctx->circa[idx];

    /* do something according to ecirca type */
    if (ctx->type == ecirca_last) {
        ctx->circa[idx] = val;
    }
    else if (val != EMPTY_VAL && ctx->circa[idx] != EMPTY_VAL) {
        if (ctx->type == ecirca_max) {

            if (val > ctx->circa[idx]) {
                ctx->circa[idx] = val;
            }
        }
        else if (ctx->type == ecirca_min) {
            if (val < ctx->circa[idx]) {
                ctx->circa[idx] = val;
            }
        }
        else if (ctx->type == ecirca_avg) {
            if ((ctx->circa[idx] + val) / 2 >= val &&
                (ctx->circa[idx] + val) / 2 >= ctx->circa[idx]) {
                ctx->circa[idx] = (ctx->circa[idx] + val) / 2;
            }
        }
        else if (ctx->type == ecirca_sum) {
            if (ctx->circa[idx] + val >= val &&
                ctx->circa[idx] + val >= ctx->circa[idx]) {
                ctx->circa[idx] = ctx->circa[idx] + val;
            }
        }
    }
    else {
        ctx->circa[idx] = val;
    }

    new_val = ctx->circa[idx];

    return TUPLE2(ATOM_OK, TUPLE2(ERL_MAKE_ELEM(env, old_val),
                                  ERL_MAKE_ELEM(env, new_val)));
}

static ERL_NIF_TERM
slice(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    circactx * ctx;
    length_t start, end, slicesize, idx, i, a;
    ERL_NIF_TERM * terms;
    ERL_NIF_TERM atom_empty;
    int incr;

    if (argc != 3) {
        return BADARG;
    }
    if (!ERL_GET_SIZE(env, argv[1], &start) ||
        !ERL_GET_SIZE(env, argv[2], &end)) {
        return BADARG;
    }
    if (!enif_get_resource(env, argv[0], circa_type, (void**) &ctx)) {
        return BADARG;
    }
    if (start > ctx->size || start == 0 ||
        end > ctx->size || end == 0) {
        return BADARG;
    }

    if (start > end) {
        incr = -1;
        slicesize = start - end + 1;
    } else {
        incr = 1;
        slicesize = end - start + 1;
    }

    if (slicesize > MAX_SLICE) {
        return TUPLE2(ATOM_ERROR, ATOM("slice_too_big"));
    }

    /* create slice */
    terms = enif_alloc(sizeof(ERL_NIF_TERM) * slicesize);

    atom_empty = ATOM("empty");

    for (a = 0, i = start; i != end + incr; i += incr) {
        idx = get_index(ctx, i);
        if (!ctx->filled && idx >= ctx->begin) {
            terms[a++] = atom_empty;
        } else {
            if (ctx->circa[idx] == EMPTY_VAL) {
                terms[a++] = atom_empty;
            } else {
                terms[a++] = ERL_MAKE_ELEM(env, ctx->circa[idx]);
            }
        }
    }

    return TUPLE2(ATOM_OK, enif_make_list_from_array(env, terms, slicesize));
}

/* getter function for size */
static ERL_NIF_TERM
size(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    circactx * ctx;

    if (argc != 1) {
        return enif_make_badarg(env);
    }
    if (!enif_get_resource(env, argv[0], circa_type, (void**) &ctx)) {
        return enif_make_badarg(env);
    }

    return TUPLE2(ATOM_OK, ERL_MAKE_SIZE(env, ctx->size));
}

/* getter functions for constants */
static ERL_NIF_TERM
max_size(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    if (argc != 0) {
        return BADARG;
    }

    return TUPLE2(ATOM_OK, ERL_MAKE_SIZE(env, MAX_SIZE));
}

static ERL_NIF_TERM
max_slice(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    if (argc != 0) {
        return BADARG;
    }

    return TUPLE2(ATOM_OK, ERL_MAKE_SIZE(env, MAX_SLICE));
}

static ERL_NIF_TERM
save(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    circactx* ctx;
    ERL_NIF_TERM ret;
    unsigned char* bin_data;
    length_t buflen, headerlen, i;

    if (argc != 1) {
        return BADARG;
    }
    if (!enif_get_resource(env, argv[0], circa_type, (void**) &ctx)) {
        return BADARG;
    }

    headerlen = (sizeof(length_t) +
                 sizeof(length_t) +
                 sizeof(bool_t) +
                 sizeof(ecirca_type) +
                 sizeof(unsigned int));
    buflen = (headerlen + sizeof(elem_t) * ctx->size);
    if (ctx->type == ecirca_avg) {
        buflen += sizeof(count_t) * ctx->size;
    }

    bin_data = enif_make_new_binary(env, buflen, &ret);

    /* format is size-begin-filled-type-value_type-circa-[count]*/
    memset(bin_data, 0x00, headerlen);

    i = 0;
    PUT_BUF(bin_data, i, ctx->size);
    PUT_BUF(bin_data, i, ctx->begin);
    PUT_BUF(bin_data, i, ctx->filled);
    PUT_BUF(bin_data, i, ctx->type);
    PUT_BUF(bin_data, i, BITNESS);

    memcpy(bin_data + i, ctx->circa, ctx->size * sizeof(elem_t));

    if (ctx->type == ecirca_avg) {
        i += ctx->size * sizeof(elem_t);
        memcpy(bin_data + i, ctx->count, ctx->size * sizeof(count_t));
    }

    return TUPLE2(ATOM_OK, ret);
}

static ERL_NIF_TERM
load(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    circactx* ctx;
    ErlNifBinary bin;
    ERL_NIF_TERM ret;
    length_t buflen, headerlen, i;
    unsigned int value_type;

    if (argc != 1) {
        return BADARG;
    }
    if (!enif_is_binary(env, argv[0])) {
        return BADARG;
    }
    if (!enif_inspect_binary(env, argv[0], &bin)) {
        return BADARG;
    };

    headerlen = (sizeof(length_t) +
                 sizeof(length_t) +
                 sizeof(bool_t) +
                 sizeof(ecirca_type) +
                 sizeof(unsigned int));

    if (bin.size < headerlen) {
        return TUPLE2(ATOM_ERROR, ATOM("bad_binary"));
    }

    /* format is size-begin-filled-type-circa-[count]*/
    i = 0;
    ctx = enif_alloc_resource(circa_type, sizeof(circactx));
    GET_BUF(bin.data, i, ctx->size);
    GET_BUF(bin.data, i, ctx->begin);
    GET_BUF(bin.data, i, ctx->filled);
    GET_BUF(bin.data, i, ctx->type);
    GET_BUF(bin.data, i, value_type);

    if (value_type != BITNESS) {
        return TUPLE2(ATOM_ERROR, ATOM("wrong_ecirca_value_type"));
    }

    buflen = (headerlen + sizeof(elem_t) * ctx->size);
    if (ctx->type == ecirca_avg) {
        buflen += sizeof(count_t) * ctx->size;
    }
    if (bin.size < buflen) {
        return TUPLE2(ATOM_ERROR, ATOM("bad_binary"));
    }
    if (ctx->size > MAX_SIZE) {
        return TUPLE2(ATOM_ERROR, ATOM("max_size"));
    }
    if (ctx->begin >= ctx->size ||
        ctx->type < ecirca_last || ctx->type > ecirca_sum) {
        return TUPLE2(ATOM_ERROR, ATOM("bad_binary"));
    }

    ctx->circa  = enif_alloc(sizeof(elem_t) * ctx->size);
    memcpy(ctx->circa, bin.data + i, ctx->size * sizeof(elem_t));
    if (ctx->type == ecirca_avg) {
        i += ctx->size * sizeof(elem_t);
        memcpy(ctx->count, bin.data + i, ctx->size * sizeof(count_t));
    }

    ret = enif_make_resource(env, ctx);
    enif_release_resource(ctx);

    return ret;
}

/* for setting ecirca type */
static int
set_type(char* str, ecirca_type* type) {
    if (strcmp(str, "max") == 0) {
        *type = ecirca_max; return 1;
    }
    else if (strcmp(str, "min") == 0) {
        *type = ecirca_min; return 1;
    }
    else if (strcmp(str, "avg") == 0) {
        *type = ecirca_avg; return 1;
    }
    else if (strcmp(str, "sum") == 0) {
        *type = ecirca_sum; return 1;
    }
    else if (strcmp(str, "last") == 0) {
        *type = ecirca_last; return 1;
    }
    return 0;
}

static ErlNifFunc functions[] =
{
    {"new",       2, new},
    {"push",      2, push},
    {"get",       2, get},
    {"set",       3, set},
    {"update",    3, update},
    {"slice",     3, slice},
    /* getter functions */
    {"size",      1, size},
    {"max_size",  0, max_size},
    {"max_slice", 0, max_slice},
    {"save",      1, save},
    {"load",      1, load}
};

INIT_STR
