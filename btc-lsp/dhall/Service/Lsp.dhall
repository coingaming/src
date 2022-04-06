let P = ../Prelude/Import.dhall

let G = ../Global.dhall

let K = ../Kubernetes/Import.dhall

let Service = ../Kubernetes/Service.dhall

let Deployment = ../Kubernetes/Deployment.dhall

let Bitcoind = ./Bitcoind.dhall

let Lnd = ./Lnd.dhall

let Postgres = ./Postgres.dhall

let owner = G.unOwner G.Owner.Lsp

let tlsCert = ../../build/secrets/lsp/tls.cert as Text ? G.todo

let tlsKey = ../../build/secrets/lsp/tls.key as Text ? G.todo

let logEnv = "test"

let logFormat = "Bracket"

let logVerbosity = "V3"

let logSeverity = "DebugS"

let minChanSize = 40000000

let msatPerByte = 1000

let grpcPort
    : G.Port
    = { unPort = 8443 }

let env =
      { lspLogEnv = "LSP_LOG_ENV"
      , lspLogFormat = "LSP_LOG_FORMAT"
      , lspLogVerbosity = "LSP_LOG_VERBOSITY"
      , lspLogSeverity = "LSP_LOG_SEVERITY"
      , lspLndP2pHost = "LSP_LND_P2P_HOST"
      , lspLndP2pPort = "LSP_LND_P2P_PORT"
      , lspLibpqConnStr = "LSP_LIBPQ_CONN_STR"
      , lspLndEnv = "LSP_LND_ENV"
      , lspGrpcServerEnv = "LSP_GRPC_SERVER_ENV"
      , lspBitcoindEnv = "LSP_BITCOIND_ENV"
      , lspMinChanCapMsat = "LSP_MIN_CHAN_CAP_MSAT"
      , lspMsatPerByte = "LSP_MSAT_PER_BYTE"
      }

let mkLspLndEnv
    : G.BitcoinNetwork → P.JSON.Type
    = λ(net : G.BitcoinNetwork) →
        P.JSON.object
          ( toMap
              { lnd_wallet_password = P.JSON.string (Lnd.mkWalletPass net)
              , lnd_tls_cert = P.JSON.string Lnd.tlsCert
              , lnd_hex_macaroon = P.JSON.string Lnd.hexMacaroon
              , lnd_host = P.JSON.string (G.unOwner G.Owner.Lnd)
              , lnd_port = P.JSON.natural Lnd.grpcPort.unPort
              }
          )

let mkLspBitcoindEnv
    : G.BitcoinNetwork → P.JSON.Type
    = λ(net : G.BitcoinNetwork) →
        P.JSON.object
          ( toMap
              { host =
                  P.JSON.string
                    "${G.unNetworkScheme
                         G.NetworkScheme.Http}://${G.unOwner
                                                     G.Owner.Bitcoind}:${G.unPort
                                                                           ( Bitcoind.mkRpcPort
                                                                               net
                                                                           )}"
              , username = P.JSON.string (Bitcoind.mkRpcUser net)
              , password = P.JSON.string (Bitcoind.mkRpcPass net)
              }
          )

let mkLspGrpcServerEnv
    : P.JSON.Type
    = P.JSON.object
        ( toMap
            { port = P.JSON.natural grpcPort.unPort
            , sig_verify = P.JSON.bool True
            , sig_header_name = P.JSON.string "sig-bin"
            , tls_cert = P.JSON.string tlsCert
            , tls_key = P.JSON.string tlsKey
            }
        )

let mkMsatPerByte
    : G.BitcoinNetwork → Text
    = λ(net : G.BitcoinNetwork) →
        merge
          { MainNet = "", TestNet = "", RegTest = Natural/show msatPerByte }
          net

let ports
    : List Natural
    = G.unPorts [ grpcPort ]

let mkEnv
    : G.BitcoinNetwork → P.Map.Type Text Text
    = λ(net : G.BitcoinNetwork) →
        [ { mapKey = env.lspLogEnv, mapValue = logEnv }
        , { mapKey = env.lspLogFormat, mapValue = logFormat }
        , { mapKey = env.lspLogVerbosity, mapValue = logVerbosity }
        , { mapKey = env.lspLogSeverity, mapValue = logSeverity }
        , { mapKey = env.lspLndP2pPort, mapValue = G.unPort Lnd.p2pPort }
        , { mapKey = env.lspLndP2pHost, mapValue = Lnd.mkDomain net }
        , { mapKey = env.lspLibpqConnStr, mapValue = Postgres.mkConnStr net }
        , { mapKey = env.lspGrpcServerEnv
          , mapValue = "'${P.JSON.render mkLspGrpcServerEnv}'"
          }
        , { mapKey = env.lspLndEnv
          , mapValue = "'${P.JSON.render (mkLspLndEnv net)}'"
          }
        , { mapKey = env.lspBitcoindEnv
          , mapValue = "'${P.JSON.render (mkLspBitcoindEnv net)}'"
          }
        , { mapKey = env.lspMsatPerByte, mapValue = mkMsatPerByte net }
        , { mapKey = env.lspMinChanCapMsat
          , mapValue = Natural/show minChanSize
          }
        ]

let mkServiceType
    : G.BitcoinNetwork → Service.ServiceType
    = λ(net : G.BitcoinNetwork) →
        merge
          { MainNet = Service.ServiceType.LoadBalancer
          , TestNet = Service.ServiceType.LoadBalancer
          , RegTest = Service.ServiceType.ClusterIP
          }
          net

let mkServiceAnnotations
    : G.BitcoinNetwork → Optional (List { mapKey : Text, mapValue : Text })
    = λ(net : G.BitcoinNetwork) →
        merge
          { MainNet = Service.mkAnnotations Service.CloudProvider.Aws owner
          , TestNet =
              Service.mkAnnotations Service.CloudProvider.DigitalOcean owner
          , RegTest = None (List { mapKey : Text, mapValue : Text })
          }
          net

let mkService
    : G.BitcoinNetwork → K.Service.Type
    = λ(net : G.BitcoinNetwork) →
        Service.mkService
          owner
          (mkServiceAnnotations net)
          (mkServiceType net)
          (Service.mkPorts ports)

let mkContainerImage
    : G.BitcoinNetwork → Text
    = λ(net : G.BitcoinNetwork) →
        merge
          { MainNet = "ghcr.io/coingaming/btc-lsp:v0.1.17"
          , TestNet = "ghcr.io/coingaming/btc-lsp:v0.1.17"
          , RegTest = ../../build/docker-image-btc-lsp.txt as Text ? G.todo
          }
          net

let configMapEnv
    : List Text
    = [ env.lspLogEnv
      , env.lspLogFormat
      , env.lspLogVerbosity
      , env.lspLogSeverity
      , env.lspLndP2pHost
      , env.lspLndP2pPort
      , env.lspMinChanCapMsat
      , env.lspMsatPerByte
      ]

let secretEnv
    : List Text
    = [ env.lspLibpqConnStr
      , env.lspLndEnv
      , env.lspGrpcServerEnv
      , env.lspBitcoindEnv
      ]

let mkContainerEnv =
        Deployment.mkEnv Deployment.EnvVarType.ConfigMap owner configMapEnv
      # Deployment.mkEnv Deployment.EnvVarType.Secret owner secretEnv

let mkContainer
    : Text → G.BitcoinNetwork → K.Container.Type
    = λ(name : Text) →
      λ(net : G.BitcoinNetwork) →
        K.Container::{
        , name
        , image = Some (mkContainerImage net)
        , env = Some mkContainerEnv
        , ports = Some (Deployment.mkContainerPorts ports)
        }

let mkDeployment
    : G.BitcoinNetwork → K.Deployment.Type
    = λ(net : G.BitcoinNetwork) →
        Deployment.mkDeployment
          owner
          (None K.DeploymentStrategy.Type)
          [ mkContainer owner net ]
          (None (List K.Volume.Type))

in  { mkEnv, configMapEnv, secretEnv, mkService, mkDeployment }
