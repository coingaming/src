let Integration = ../Service/Integration.dhall

let Lsp = ../Service/Lsp.dhall

let G = ../Global.dhall

let lndHost = G.unOwner G.Owner.Lnd

let network = G.BitcoinNetwork.RegTest

let Lnd = ../Service/Lnd.dhall

let Postgres = ../Service/Postgres.dhall

in  ''
    export ${Integration.env.integrationGrpcClientEnv}='{
      "host":"${lndHost}",
      "port":${G.unPort Lsp.grpcPort},
      "sig_header_name":"sig-bin",
      "compress_mode":"Compressed"
    }'
    export ${Integration.env.integrationBitcoindEnv}='${Integration.mkIntegrationBitcoindEnv
                                                          network}'
    export ${Integration.env.integrationGrpcServerEnv}='${Integration.mkIntegrationGrpcServerEnv}'
    export ${Integration.env.integrationLndEnv}='${Integration.mkIntegrationLndEnv
                                                     network}'
    export ${Integration.env.integrationLndEnv2}='${Integration.mkIntegrationLndEnv
                                                      network}'
    export ${Integration.env.integrationElectrsEnv}='{
      "host":"electrs",
      "port":"80"
    }'
    export ${Integration.env.integrationLibpqConnStr}='${Postgres.mkConnStr
                                                           network}'
    export ${Integration.env.integrationLndP2pHost}="${Lnd.mkHost network}"
    export ${Integration.env.integrationLndP2pPort}="${G.unPort Lnd.p2pPort}"
    export ${Integration.env.integrationLogEnv}="${Lsp.logEnv}"
    export ${Integration.env.integrationLogFormat}="${Lsp.logFormat}"
    export ${Integration.env.integrationLogSeverity}="${Lsp.logSeverity}"
    export ${Integration.env.integrationLogVerbosity}="${Lsp.logVerbosity}"
    export ${Integration.env.integrationMinChanCapMsat}="20000000"
    ''