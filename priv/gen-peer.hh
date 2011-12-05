#ifndef __defined_genpeer_hh
#define __defined_genpeer_hh

#ifndef NODE_CLASS
#   error NODE_CLASS undefined.
#endif

#ifndef MODULE
#    error MODULE undefined (check makefile).
#endif

#define STR(x) #x
#define TOSTR(x) STR(x)
const char * NODE_CLASS_NAME = TOSTR(NODE_CLASS);
const char * MODULE_NAME = TOSTR(MODULE);
#undef TOSTR
#undef STR

#include "gen-peer-abstract.hh"
#include <erl_nif.h>

/* ------------------------------------------------------------------- */
/* Utility functions                                                   */
/* ------------------------------------------------------------------- */

static
void destructor (ErlNifEnv *, void *obj)
{
    /* Environment is not used, since the instance is owned by erlang */
    GenPeer * p = static_cast<GenPeer *>(obj);
    p->~GenPeer();
}

static inline
ErlNifResourceType * open_resource (ErlNifEnv *env,
                                    ErlNifResourceFlags flags)
{
    return enif_open_resource_type(env, MODULE_NAME, NODE_CLASS_NAME,
                                   destructor, flags, NULL);
}

static
GenPeer & unpack (ErlNifEnv *env, ERL_NIF_TERM pack)
{
    GenPeer *instance;
    ErlNifResourceType *type;

    type = static_cast<ErlNifResourceType *>(enif_priv_data(env));
    enif_get_resource(env, pack, type, (void **) &instance);

    return *instance;
}

static inline
ERL_NIF_TERM build_error (ErlNifEnv *env, const char *msg)
{
    return enif_make_tuple2(
                env, enif_make_atom(env, "error"),
                enif_make_string(env, msg, ERL_NIF_LATIN1)
           );
}

static inline
ERL_NIF_TERM build_ok (ErlNifEnv *env, ERL_NIF_TERM ret)
{
    return enif_make_tuple2(env, enif_make_atom(env, "ok"), ret);
}

/* ------------------------------------------------------------------- */
/* Interface with dynamic loading                                      */
/* ------------------------------------------------------------------- */

/* TODO: remove output on cerr when stuff is ready */

static
int load (ErlNifEnv *env, void **priv_data, ERL_NIF_TERM load_info)
{
    ErlNifResourceType *res = open_resource(env, ERL_NIF_RT_CREATE);
    *priv_data = static_cast<void *>(res);
    std::cerr << "Loading phase achieved" << std::endl;
    return 0;
}

static
int reload (ErlNifEnv* env, void** priv_data, ERL_NIF_TERM load_info)
{
    ErlNifResourceFlags flags = static_cast<ErlNifResourceFlags>(
        ERL_NIF_RT_CREATE | ERL_NIF_RT_TAKEOVER
    );

    ErlNifResourceType *res = open_resource(env, flags);
    *priv_data = static_cast<void *>(res);
    std::cerr << "Reload" << std::endl;
    return 0;
}

static
int upgrade (ErlNifEnv* env, void** priv_data, void** old_priv_data,
             ERL_NIF_TERM load_info)
{
    std::cerr << "Upgrade" << std::endl;
    *priv_data = *old_priv_data;
    return 0;
}

void unload (ErlNifEnv* env, void* priv_data)
{
    std::cerr << "Unload" << std::endl;
}

/* ------------------------------------------------------------------- */
/* Gateways                                                            */
/* ------------------------------------------------------------------- */

static
ERL_NIF_TERM init_gw (ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    int arity;
    const ERL_NIF_TERM *items;
    void * memchunk;
    ErlNifResourceType *type;

    /* XXX Assumption here: One single argument, which is a tuple */
    if (!enif_get_tuple(env, argv[0], &arity, &items)) {
        return enif_make_badarg(env);
    }

    std::cerr << "Init parametrized with " << arity << " params."
              << std::endl;

    type = static_cast<ErlNifResourceType *>(enif_priv_data(env));
    memchunk = enif_alloc_resource(type, sizeof(NODE_CLASS));
    GenPeer &instance = * new (memchunk) NODE_CLASS();

    try {
        instance.init(env, items, static_cast<size_t>(arity));
        ERL_NIF_TERM ret = enif_make_resource(env, memchunk);
        enif_release_resource(memchunk);
        return build_ok(env, ret);
    } catch (GenPeerError &err) {
        destructor(env, memchunk);
        return build_error(env, err.what());
    }
}

static
ERL_NIF_TERM handle_message_gw (ErlNifEnv *env, int argc,
                                const ERL_NIF_TERM argv[])
{
    ERL_NIF_TERM res = argv[argc - 1];

    try {
        unpack(env, res).handle_message(env, argv[0], argv[1]);
        return build_ok(env, res);
    } catch (GenPeerError &err) {
        return build_error(env, err.what());
    }
}

static
ERL_NIF_TERM handle_introduction_gw (ErlNifEnv *env, int argc,
                                     const ERL_NIF_TERM argv[])
{
    ERL_NIF_TERM res = argv[argc - 1];

    try {
        unpack(env, res).handle_introduction(env, argv[0], argv[1]);
        return build_ok(env, res);
    } catch (GenPeerError &err) {
        return build_error(env, err.what());
    }
}

static
ERL_NIF_TERM handle_info_gw (ErlNifEnv *env, int argc,
                             const ERL_NIF_TERM argv[])
{
    ERL_NIF_TERM res = argv[argc - 1];

    try {
        unpack(env, res).handle_info(env, argv[0]);
        return build_ok(env, res);
    } catch (GenPeerError &err) {
        return build_error(env, err.what());
    }
}

static
ERL_NIF_TERM handle_beacon_gw (ErlNifEnv *env, int argc,
                               const ERL_NIF_TERM argv[])
{
    ERL_NIF_TERM res = argv[argc - 1];

    try {
        unpack(env, res).handle_beacon(env);
        return build_ok(env, res);
    } catch (GenPeerError &err) {
        return build_error(env, err.what());
    }
}

/* A little hack: I need to support genericity of this module, and the
 * macro will expand my MODULE variable into a string. I don't want that.
 */
#define ERL_NIF_INIT_HACK(module, funcs, load, reload, upgrade, unload) \
        ERL_NIF_INIT(module, funcs, load, reload, upgrade, unload)

extern "C" {

    static ErlNifFunc funcs [] = {
        {"init", 1, init_gw},
        {"handle_message", 3, handle_message_gw},
        {"handle_introduction", 3, handle_introduction_gw},
        {"handle_info", 2, handle_info_gw},
        {"handle_beacon", 1, handle_beacon_gw}
    };

    ERL_NIF_INIT_HACK(MODULE, funcs, load, reload, upgrade, unload)

}

#endif // __defined_genpeer_hh
