#ifndef __defined_genpeerabstract_hh
#define __defined_genpeerabstract_hh

#include <string>
#include <map>
#include <stdexcept>

#include <erl_nif.h>

class GenPeerError : public std::runtime_error {
    public:
        explicit GenPeerError (const std::string &msg)
            : std::runtime_error(msg) {}
};

class GenPeer {

    public:

        virtual ~GenPeer() {};

        /* ----------------------------------------------------------- */
        /* Handler for events                                          */
        /* ----------------------------------------------------------- */

        virtual void init (ErlNifEnv *env, const ERL_NIF_TERM params[],
                           size_t nparams) = 0;

        virtual void handle_message (ErlNifEnv *env,
                                     ERL_NIF_TERM from,
                                     ERL_NIF_TERM msg) = 0;

        virtual void handle_introduction (ErlNifEnv *env,
                                          ERL_NIF_TERM from,
                                          ERL_NIF_TERM node) = 0;

        virtual void handle_info (ErlNifEnv *env, ERL_NIF_TERM info) = 0;

        virtual void handle_beacon (ErlNifEnv *env) = 0;

};

#endif // __defined_genpeerabstract_hh

