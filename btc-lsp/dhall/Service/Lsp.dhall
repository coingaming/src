let P = ../Prelude/Import.dhall

let G = ../Global.dhall

let C = ../CloudProvider.dhall

let S = ../Service.dhall

let K = ../Kubernetes/Import.dhall

let Service = ../Kubernetes/Service.dhall

let Ingress = ../Kubernetes/Ingress.dhall

let Deployment = ../Kubernetes/Deployment.dhall

let Bitcoind = ./Bitcoind.dhall

let Lnd = ./Lnd.dhall

let Postgres = ./Postgres.dhall

let owner = G.unOwner G.Owner.Lsp

let domainName = ../../build/secrets/lnd/domainname.txt as Text ? G.todo

let cloudProvider = Some C.ProviderType.DigitalOcean

let logEnv = "test"

let logFormat = "Bracket"

let logVerbosity = "V3"

let logSeverity = "DebugS"

let minChanSize = 40000000

let msatPerByte = 1000

let grpcPort
    : G.Port
    = { unPort = 8443 }

let yesodPort
    : G.Port
    = { unPort = 3000 }

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
      , lspYesodHost = "LSP_YESOD_HOST"
      , lspYesodPort = "LSP_YESOD_PORT"
      }

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
      , env.lspYesodHost
      , env.lspYesodPort
      ]

let secretEnv
    : List Text
    = [ env.lspLibpqConnStr
      , env.lspLndEnv
      , env.lspGrpcServerEnv
      , env.lspBitcoindEnv
      ]

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
            , encryption =
                P.JSON.string (G.unEncryption G.Encryption.UnEncrypted)
            }
        )

let mkLspGrpcClientEnv
    : P.JSON.Type
    = P.JSON.object
        ( toMap
            { host = P.JSON.string (G.unOwner G.Owner.Lsp)
            , port = P.JSON.natural grpcPort.unPort
            , sig_header_name = P.JSON.string "sig-bin"
            , compress_mode = P.JSON.string "Compressed"
            }
        )

let mkMsatPerByte
    : G.BitcoinNetwork → Text
    = λ(net : G.BitcoinNetwork) →
        merge
          { MainNet = "", TestNet = "", RegTest = Natural/show msatPerByte }
          net

let mkLndEnv
    : G.BitcoinNetwork → G.Owner → P.JSON.Type
    = λ(net : G.BitcoinNetwork) →
      λ(owner : G.Owner) →
        P.JSON.object
          ( toMap
              { lnd_wallet_password = P.JSON.string (Lnd.mkWalletPass net)
              , lnd_tls_cert = P.JSON.string (Lnd.mkTlsCert owner)
              , lnd_hex_macaroon = P.JSON.string (Lnd.mkHexMacaroon owner)
              , lnd_host = P.JSON.string (G.unOwner owner)
              , lnd_port = P.JSON.natural Lnd.grpcPort.unPort
              }
          )

let ports
    : List Natural
    = G.unPorts [ grpcPort, yesodPort ]

let mkDomain
    : G.BitcoinNetwork → Text
    = λ(net : G.BitcoinNetwork) →
        merge
          { MainNet = domainName, TestNet = domainName, RegTest = owner }
          net

let mkEnv
    : G.BitcoinNetwork → P.Map.Type Text Text
    = λ(net : G.BitcoinNetwork) →
        [ { mapKey = env.lspLogEnv, mapValue = logEnv }
        , { mapKey = env.lspLogFormat, mapValue = logFormat }
        , { mapKey = env.lspLogVerbosity, mapValue = logVerbosity }
        , { mapKey = env.lspLogSeverity, mapValue = logSeverity }
        , { mapKey = env.lspLndP2pPort, mapValue = G.unPort Lnd.p2pPort }
        , { mapKey = env.lspLndP2pHost, mapValue = Lnd.mkDomainName net }
        , { mapKey = env.lspLibpqConnStr, mapValue = Postgres.mkConnStr net }
        , { mapKey = env.lspGrpcServerEnv
          , mapValue = "'${P.JSON.render mkLspGrpcServerEnv}'"
          }
        , { mapKey = env.lspLndEnv
          , mapValue = "'${P.JSON.render (mkLndEnv net G.Owner.Lnd)}'"
          }
        , { mapKey = env.lspBitcoindEnv
          , mapValue = "'${P.JSON.render (mkLspBitcoindEnv net)}'"
          }
        , { mapKey = env.lspMsatPerByte, mapValue = mkMsatPerByte net }
        , { mapKey = env.lspMinChanCapMsat
          , mapValue = Natural/show minChanSize
          }
        , { mapKey = env.lspYesodHost, mapValue = "*" }
        , { mapKey = env.lspYesodPort, mapValue = G.unPort yesodPort }
        ]

let mkSetupEnv
    : G.Owner → Text
    = λ(owner : G.Owner) →
        let ownerText = G.unOwner owner

        in  ''
            #!/bin/bash

            set -e

            THIS_DIR="$(dirname "$(realpath "$0")")"

            . "$THIS_DIR/export-${ownerText}-env.sh"

            echo "==> Setting up env for ${ownerText}"

            (
              kubectl create configmap ${ownerText} \${S.concatSetupEnv
                                                         configMapEnv}
            ) || true

            (
              kubectl create secret generic ${ownerText} \${S.concatSetupEnv
                                                              secretEnv}
            ) || true
            ''

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
    : G.BitcoinNetwork →
      Optional C.ProviderType →
        Optional (P.Map.Type Text Text)
    = λ(net : G.BitcoinNetwork) →
      λ(cloudProvider : Optional C.ProviderType) →
        let annotations =
              P.Optional.concatMap
                C.ProviderType
                (P.Map.Type Text Text)
                ( λ(cloudProvider : C.ProviderType) →
                    merge
                      { Aws = Some
                        [ { mapKey =
                              "service.beta.kubernetes.io/aws-load-balancer-proxy-protocol"
                          , mapValue = "*"
                          }
                        , { mapKey =
                              "service.beta.kubernetes.io/aws-load-balancer-ssl-cert"
                          , mapValue = ../../build/certarn.txt as Text ? G.todo
                          }
                        , { mapKey =
                              "service.beta.kubernetes.io/aws-load-balancer-ssl-ports"
                          , mapValue = Natural/show grpcPort.unPort
                          }
                        ]
                      , DigitalOcean = Some
                        [ { mapKey =
                              "kubernetes.digitalocean.com/load-balancer-id"
                          , mapValue = "${owner}-lb"
                          }
                        ]
                      }
                      cloudProvider
                )
                cloudProvider

        in  S.mkServiceAnnotations net annotations cloudProvider

let mkService
    : G.BitcoinNetwork → Optional C.ProviderType → K.Service.Type
    = λ(net : G.BitcoinNetwork) →
      λ(cloudProvider : Optional C.ProviderType) →
        Service.mkService
          owner
          (mkServiceAnnotations net cloudProvider)
          (mkServiceType net)
          (Service.mkPorts ports)

let mkIngress
    : G.BitcoinNetwork → K.Ingress.Type
    = λ(net : G.BitcoinNetwork) →
        let certArn = ../../build/certarn.txt as Text ? G.todo

        in  Ingress.mkIngress
              owner
              (S.mkIngressAnnotations net cloudProvider certArn)
              (mkDomain net)
              yesodPort.unPort
              (S.mkIngressClassName cloudProvider)
              (None (List K.IngressTLS.Type))

let mkContainerImage
    : G.BitcoinNetwork → Text
    = λ(net : G.BitcoinNetwork) →
        merge
          { MainNet = "ghcr.io/coingaming/btc-lsp:v0.1.27"
          , TestNet = "ghcr.io/coingaming/btc-lsp:v0.1.27"
          , RegTest = ../../build/docker-image-btc-lsp.txt as Text ? G.todo
          }
          net

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

in  { mkEnv
    , mkSetupEnv
    , configMapEnv
    , grpcPort
    , secretEnv
    , mkService
    , mkIngress
    , mkDeployment
    , mkLspGrpcClientEnv
    , mkLspBitcoindEnv
    , mkLspGrpcServerEnv
    , logEnv
    , logFormat
    , logSeverity
    , logVerbosity
    , mkLndEnv
    }
