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
#include "erl_nif.h"

#define ERL_MAKE_ELEM   enif_make_uint
#define ERL_MAKE_SIZE   enif_make_uint
#define ERL_GET_ELEM    enif_get_uint
#define ERL_GET_SIZE    enif_get_uint
#define MAX_SLICE       1000000
#define MAX_SIZE        1000000
#define EMPTY_VAL       UINT32_MAX

typedef uint32_t        elem_t;
typedef uint32_t        length_t;
typedef uint32_t        count_t;

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
    length_t                begin;
    elem_t                 *circa;
    count_t                *count;
    length_t                size;
    unsigned short int      filled;
    ecirca_type             type;
} circactx;

ErlNifResourceType* circa_type;

/* additional functions */;
static int set_type(char *);

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

    if (argc != 2) {
        return enif_make_badarg(env);
    }
    if (!ERL_GET_SIZE(env, argv[0], &size)) {
        return enif_make_badarg(env);
    }
    if (size == 0) {
        return enif_make_badarg(env);
    }
    if (size > MAX_SIZE) {
        return enif_make_tuple2(env, enif_make_atom(env, "error"),
                                     enif_make_atom(env, "max_size"));
    }
    if (!enif_get_atom(env, argv[1], typestr, 5, ERL_NIF_LATIN1)) {
		return enif_make_badarg(env);
	}

    ctx         = enif_alloc_resource(circa_type, sizeof(circactx));
    ctx->begin  = 0;
    ctx->circa  = enif_alloc(sizeof(elem_t) * size);
    ctx->size   = size;
    ctx->filled = 0;
    ctx->type   = set_type(typestr);

    memset(ctx->circa, 0xFF, sizeof(elem_t) * size);

    ret = enif_make_resource(env, ctx);
    enif_release_resource(ctx);

    return enif_make_tuple2(env, enif_make_atom(env, "ok"), ret);
}

static ERL_NIF_TERM
push(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    circactx * ctx;
    char emptycmp[6];
    elem_t val;

    if (argc != 2) {
        return enif_make_badarg(env);
    }
    if (!ERL_GET_ELEM(env, argv[1], &val)) {
        if (!enif_get_atom(env, argv[1], emptycmp, 6, ERL_NIF_LATIN1)) {
            return enif_make_badarg(env);
        } else {
            if (!strcmp(emptycmp, emptystr)) {
                val = EMPTY_VAL;
            } else {
                return enif_make_badarg(env);
            }
        }
    }
    if (!enif_get_resource(env, argv[0], circa_type, (void**) &ctx)) {
        return enif_make_badarg(env);
    }

    if (++ctx->begin >= ctx->size + 1) {
        ctx->begin  = 1;
        ctx->filled = 1;
    }

    ctx->circa[ctx->begin - 1] = val;

    return enif_make_tuple2(env, enif_make_atom(env, "ok"),
                                 enif_make_resource(env, ctx));
}

/*
static ERL_NIF_TERM
push_list(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    circactx * ctx;
    int listlen;

    if (argc != 2) {
        return enif_make_badarg(env);
    }
    if (!enif_get_resource(env, argv[0], circa_type, (void**) &ctx)) {
        return enif_make_badarg(env);
    }

}*/

static ERL_NIF_TERM
get(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    circactx * ctx;
    length_t i, idx;
    ERL_NIF_TERM ret;

    if (argc != 2) {
        return enif_make_badarg(env);
    }
    if (!ERL_GET_SIZE(env, argv[1], &i)) {
        return enif_make_badarg(env);
    }
    if (!enif_get_resource(env, argv[0], circa_type, (void**) &ctx)) {
        return enif_make_badarg(env);
    }

    if (i > ctx->size || i == 0) {
        return enif_make_badarg(env);
    }
    if (!ctx->filled && i > ctx->begin) {
        return enif_make_tuple2(env, enif_make_atom(env, "ok"),
                                     enif_make_atom(env, "empty"));
    }

    idx = get_index(ctx, i);
    if (ctx->circa[idx] == EMPTY_VAL) {
        ret = enif_make_atom(env, "empty");
    } else {
        ret = ERL_MAKE_ELEM(env, ctx->circa[idx]);
    }
    return enif_make_tuple2(env, enif_make_atom(env, "ok"), ret);
}

static ERL_NIF_TERM
set(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    circactx * ctx;
    char emptycmp[6];
    length_t i, idx;
    elem_t val;

    if (argc != 3) {
        return enif_make_badarg(env);
    }
    if (!ERL_GET_SIZE(env, argv[1], &i)) {
        return enif_make_badarg(env);
    }
    if (!ERL_GET_ELEM(env, argv[2], &val)) {
        if (!enif_get_atom(env, argv[2], emptycmp, 6, ERL_NIF_LATIN1)) {
            return enif_make_badarg(env);
        } else {
            if (!strcmp(emptycmp, emptystr)) {
                val = EMPTY_VAL;
            } else {
                return enif_make_badarg(env);
            }
        }
    } else {
        if (val == EMPTY_VAL) {
            return enif_make_tuple2(env, enif_make_atom(env, "error"),
                                         enif_make_atom(env, "overflow"));
        }
    }
    if (!enif_get_resource(env, argv[0], circa_type, (void**) &ctx)) {
        return enif_make_badarg(env);
    }
    if (i > ctx->size || i == 0) {
        return enif_make_badarg(env);
    }

    idx = get_index(ctx, i);
    ctx->circa[idx] = val;

    return enif_make_tuple2(env, enif_make_atom(env, "ok"),
                                 enif_make_resource(env, ctx));
}

static ERL_NIF_TERM
update(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    circactx * ctx;
    char emptycmp[6];
    length_t i, idx;
    elem_t val;

    if (argc != 3) {
        return enif_make_badarg(env);
    }
    if (!ERL_GET_SIZE(env, argv[1], &i)) {
        return enif_make_badarg(env);
    }
    if (!ERL_GET_ELEM(env, argv[2], &val)) {
        if (!enif_get_atom(env, argv[2], emptycmp, 6, ERL_NIF_LATIN1)) {
            return enif_make_badarg(env);
        } else {
            if (!strcmp(emptycmp, emptystr)) {
                val = EMPTY_VAL;
            } else {
                return enif_make_badarg(env);
            }
        }
    } else {
        if (val == EMPTY_VAL) {
            return enif_make_tuple2(env, enif_make_atom(env, "error"),
                                         enif_make_atom(env, "overflow"));
        }
    }
    if (!enif_get_resource(env, argv[0], circa_type, (void**) &ctx)) {
        return enif_make_badarg(env);
    }
    if (i > ctx->size || i == 0) {
        return enif_make_badarg(env);
    }

    idx = get_index(ctx, i);
    
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
        return enif_make_tuple2(env, enif_make_atom(env, "error"),
                                     enif_make_atom(env, "overflow"));      
    }
    
    return enif_make_tuple2(env, enif_make_atom(env, "ok"),
                                 enif_make_resource(env, ctx));
}

static ERL_NIF_TERM
slice(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    circactx * ctx;
    length_t start, end, slicesize, idx, i, a;
    ERL_NIF_TERM * terms;
    ERL_NIF_TERM atom_empty;
    int incr;

    if (argc != 3) {
        return enif_make_badarg(env);
    }
    if (!ERL_GET_SIZE(env, argv[1], &start) ||
        !ERL_GET_SIZE(env, argv[2], &end)) {
        return enif_make_badarg(env);
    }
    if (!enif_get_resource(env, argv[0], circa_type, (void**) &ctx)) {
        return enif_make_badarg(env);
    }
    if (start > ctx->size || start == 0 ||
        end > ctx->size || end == 0) {
        return enif_make_badarg(env);
    }

    if (start > end) {
        incr = -1;
        slicesize = start - end + 1;
    } else {
        incr = 1;
        slicesize = end - start + 1;
    }

    if (slicesize > MAX_SLICE) {
        return enif_make_tuple2(env, enif_make_atom(env, "error"),
                                     enif_make_atom(env, "slice_too_big"));
    }

    /* create slice */
    terms = enif_alloc(sizeof(ERL_NIF_TERM) * slicesize);

    atom_empty = enif_make_atom(env, "empty");

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

    return enif_make_tuple2(env, enif_make_atom(env, "ok"),
                            enif_make_list_from_array(env, terms, slicesize));
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

    return enif_make_tuple2(env, enif_make_atom(env, "ok"),
                                 ERL_MAKE_SIZE(env, ctx->size));
}

/* getter functions for constants */
static ERL_NIF_TERM
max_size(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    if (argc != 0) {
        return enif_make_badarg(env);
    }

    return enif_make_tuple2(env, enif_make_atom(env, "ok"),
                                 ERL_MAKE_SIZE(env, MAX_SIZE));
}

static ERL_NIF_TERM
max_slice(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    if (argc != 0) {
        return enif_make_badarg(env);
    }

    return enif_make_tuple2(env, enif_make_atom(env, "ok"),
                            ERL_MAKE_SIZE(env, MAX_SLICE));
}

static ERL_NIF_TERM
save(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    circactx* ctx;
    ERL_NIF_TERM ret;
    unsigned char* bin_data;
    length_t buflen, headerlen, i;

    if (argc != 1) {
        return enif_make_badarg(env);
    }
    if (!enif_get_resource(env, argv[0], circa_type, (void**) &ctx)) {
        return enif_make_badarg(env);
    }

    headerlen = (sizeof(length_t) +
                 sizeof(length_t) +
                 sizeof(unsigned short int) +
                 sizeof(ecirca_type));
    buflen =  (headerlen +
               sizeof(elem_t) * ctx->size);
    if (ctx->type == ecirca_avg) {
        buflen += sizeof(count_t) * ctx->size;
    }

    bin_data = enif_make_new_binary(env, buflen, &ret);

    /* format is size-begin-filled-type-circa-[count]*/
    memset(bin_data, 0x00, headerlen);

    i = 0;
    bin_data[i] = ctx->size;    i += sizeof(length_t);
    bin_data[i] = ctx->begin;   i += sizeof(length_t);
    bin_data[i] = ctx->filled;  i += sizeof(unsigned short int);
    bin_data[i] = ctx->type;    i += sizeof(ecirca_type);

    memcpy(bin_data + i, ctx->circa, ctx->size * sizeof(elem_t));

    if (ctx->type == ecirca_avg) {
        i += ctx->size * sizeof(elem_t);
        memcpy(bin_data + i, ctx->count, ctx->size * sizeof(count_t));
    }

    return enif_make_tuple2(env, enif_make_atom(env, "ok"), ret);
}

static ERL_NIF_TERM
load(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    circactx* ctx;
    ErlNifBinary bin;
    ERL_NIF_TERM ret;
    length_t buflen, headerlen, i;

    if (argc != 1) {
        return enif_make_badarg(env);
    }
    if (!enif_is_binary(env, argv[0])) {
        return enif_make_badarg(env);
    }
    if (!enif_inspect_binary(env, argv[0], &bin)) {
        return enif_make_badarg(env);
    };

    headerlen = (sizeof(length_t) +
                 sizeof(length_t) +
                 sizeof(unsigned short int) +
                 sizeof(ecirca_type));

    if (bin.size < headerlen) {
        return enif_make_tuple2(env, enif_make_atom(env, "error"),
                                enif_make_atom(env, "bad_binary"));
    }

    /* format is size-begin-filled-type-circa-[count]*/
    i = 0;
    ctx = enif_alloc_resource(circa_type, sizeof(circactx));
    ctx->size   = bin.data[i]; i += sizeof(length_t);
    ctx->begin  = bin.data[i]; i += sizeof(length_t);
    ctx->filled = bin.data[i]; i += sizeof(unsigned short int);
    ctx->type   = bin.data[i]; i += sizeof(ecirca_type);

    buflen =  (headerlen +
               sizeof(elem_t) * ctx->size);
    if (ctx->type == ecirca_avg) {
        buflen += sizeof(count_t) * ctx->size;
    }
    if (bin.size < buflen) {
        return enif_make_tuple2(env, enif_make_atom(env, "error"),
                                enif_make_atom(env, "bad_binary"));
    }
    if (ctx->size > MAX_SIZE) {
        return enif_make_tuple2(env, enif_make_atom(env, "error"),
                                enif_make_atom(env, "max_size"));
    }
    if (ctx->begin >= ctx->size ||
        ctx->type < ecirca_last || ctx->type > ecirca_sum) {
        return enif_make_tuple2(env, enif_make_atom(env, "error"),
                                enif_make_atom(env, "bad_binary"));
    }

    ctx->circa  = enif_alloc(sizeof(elem_t) * ctx->size);
    memcpy(ctx->circa, bin.data + i, ctx->size * sizeof(elem_t));
    if (ctx->type == ecirca_avg) {
        i += ctx->size * sizeof(elem_t);
        memcpy(ctx->count, bin.data + i, ctx->size * sizeof(count_t));
    }

    ret = enif_make_resource(env, ctx);
    enif_release_resource(ctx);

    return enif_make_tuple2(env, enif_make_atom(env, "ok"), ret);
}

/* for setting ecirca type */
static int
set_type(char *str) {
    if (strcmp(str, "max") == 0) {
        return ecirca_max;
    }
    else if (strcmp(str, "min") == 0) {
        return ecirca_min;
    }
    else if (strcmp(str, "avg") == 0) {
        return ecirca_avg;
    }
    else if (strcmp(str, "sum") == 0) {
        return ecirca_sum;
    }
    return ecirca_last;
}

static ErlNifFunc functions[] =
{
    {"new", 2, new},
    {"push", 2, push},
    {"get", 2, get},
    {"set", 3, set},
    {"update", 3, update},
    {"slice", 3, slice},
    /* getter functions */
    {"size", 1, size},
    {"max_size", 0, max_size},
    {"max_slice", 0, max_slice},
    {"save", 1, save},
    {"load", 1, load}
};

ERL_NIF_INIT(ecirca, functions, &init, NULL, NULL, NULL)
