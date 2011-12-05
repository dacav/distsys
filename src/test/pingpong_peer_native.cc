#include <iostream>
#include <gen-peer-abstract.hh>

class PingPongNode : public GenPeer {

    void init (ErlNifEnv *env, const ERL_NIF_TERM *, //params[]
               size_t nparams)
    {
        std::cerr << "Initialized with " << nparams << "Params"
                  << std::endl;
    }

    void handle_message (ErlNifEnv *env, ERL_NIF_TERM, // from,
                         ERL_NIF_TERM) // msg)
    {
        std::cerr << "Got message" << std::endl;
    }

    void handle_introduction (ErlNifEnv *env, ERL_NIF_TERM, // from,
                              ERL_NIF_TERM) // node)
    {
        std::cerr << "Got introduction" << std::endl;
    }

    void handle_info (ErlNifEnv *env, ERL_NIF_TERM) //info)
    {
        std::cerr << "Got info" << std::endl;
    }

    void handle_beacon (ErlNifEnv *env)
    {
        std::cerr << "beacon" << std::endl;
    }

};

#define NODE_CLASS PingPongNode

#include <gen-peer.hh>
