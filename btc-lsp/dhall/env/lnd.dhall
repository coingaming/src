let G = ../Global.dhall

let Bitcoind = ../Service/Bitcoind.dhall

let Lnd = ../Service/Lnd.dhall

let bitcoindHost = G.unOwner G.Owner.Bitcoind

let networkScheme = G.unNetworkScheme G.NetworkScheme.Tcp

in  ''
    export BITCOIN_DEFAULTCHANCONFS="1"
    export BITCOIN_RPCUSER="${Bitcoind.rpcUser}"
    export BITCOIN_RPCPASS="${Bitcoind.rpcPass}"
    export BITCOIN_ZMQPUBRAWBLOCK="${networkScheme}://${bitcoindHost}:${G.unPort
                                                                          Bitcoind.zmqPubRawBlockPort}"
    export BITCOIN_ZMQPUBRAWTX="${networkScheme}://${bitcoindHost}:${G.unPort
                                                                       Bitcoind.zmqPubRawTxPort}"
    export LND_GRPC_PORT="${G.unPort Lnd.grpcPort}"
    export LND_P2P_PORT="${G.unPort Lnd.p2pPort}"
    export LND_REST_PORT="${G.unPort Lnd.restPort}"
    export LND_WALLETPASS="${Lnd.walletPass}"
    ''
