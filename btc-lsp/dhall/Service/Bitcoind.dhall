let P = ../Prelude/Import.dhall

let G = ../Global.dhall

let C = ../CloudProvider.dhall

let S = ../Service.dhall

let K = ../Kubernetes/Import.dhall

let Service = ../Kubernetes/Service.dhall

let Volume = ../Kubernetes/Volume.dhall

let Deployment = ../Kubernetes/Deployment.dhall

let owner = G.unOwner G.Owner.Bitcoind

let image = "heathmont/bitcoind:v0.22.0-neutrino"

let secureRpcUser = ../../build/secrets/bitcoind/rpcuser.txt as Text ? G.todo

let secureRpcPass = ../../build/secrets/bitcoind/rpcpass.txt as Text ? G.todo

let zmqPubRawBlockPort
    : G.Port
    = { unPort = 39703 }

let zmqPubRawTxPort
    : G.Port
    = { unPort = 39704 }

let env =
      { configFromEnv = "CONFIG_FROM_ENV"
      , disableWallet = "DISABLEWALLET"
      , prune = "PRUNE"
      , regTest = "REGTEST"
      , rpcAllowIp = "RPCALLOWIP"
      , rpcBind = "RPCBIND"
      , rpcPort = "RPCPORT"
      , p2pPort = "PORT"
      , server = "SERVER"
      , testNet = "TESTNET"
      , txIndex = "TXINDEX"
      , zmqPubRawBlock = "ZMQPUBRAWBLOCK"
      , zmqPubRawTx = "ZMQPUBRAWTX"
      , blockFilterIndex = "BLOCKFILTERINDEX"
      , peerBlockFilters = "PEERBLOCKFILTERS"
      , rpcUser = "RPCUSER"
      , rpcPassword = "RPCPASSWORD"
      }

let configMapEnv
    : List Text
    = [ env.configFromEnv
      , env.disableWallet
      , env.prune
      , env.regTest
      , env.rpcAllowIp
      , env.rpcBind
      , env.rpcPort
      , env.p2pPort
      , env.server
      , env.testNet
      , env.txIndex
      , env.zmqPubRawBlock
      , env.zmqPubRawTx
      , env.blockFilterIndex
      , env.peerBlockFilters
      ]

let secretEnv
    : List Text
    = [ env.rpcUser, env.rpcPassword ]

let mkRpcUser
    : G.BitcoinNetwork → Text
    = λ(net : G.BitcoinNetwork) →
        merge
          { MainNet = secureRpcUser
          , TestNet = secureRpcUser
          , RegTest = "bitcoinrpc"
          }
          net

let mkRpcPass
    : G.BitcoinNetwork → Text
    = λ(net : G.BitcoinNetwork) →
        merge
          { MainNet = secureRpcPass
          , TestNet = secureRpcPass
          , RegTest = G.defaultPass
          }
          net

let mkRpcPort
    : G.BitcoinNetwork → G.Port
    = λ(net : G.BitcoinNetwork) →
        merge
          { MainNet.unPort = 8332
          , TestNet.unPort = 18332
          , RegTest.unPort = 18332
          }
          net

let mkP2pPort
    : G.BitcoinNetwork → G.Port
    = λ(net : G.BitcoinNetwork) →
        merge
          { MainNet.unPort = 8333
          , TestNet.unPort = 18333
          , RegTest.unPort = 18444
          }
          net

let mkRegTest
    : G.BitcoinNetwork → Text
    = λ(net : G.BitcoinNetwork) →
        merge { MainNet = "0", TestNet = "0", RegTest = "1" } net

let mkTestNet
    : G.BitcoinNetwork → Text
    = λ(net : G.BitcoinNetwork) →
        merge { MainNet = "0", TestNet = "1", RegTest = "0" } net

let mkEnv
    : G.BitcoinNetwork → P.Map.Type Text Text
    = λ(net : G.BitcoinNetwork) →
        let networkScheme = G.unNetworkScheme G.NetworkScheme.Tcp

        let rpcPort = G.unPort (mkRpcPort net)

        in  [ { mapKey = env.configFromEnv, mapValue = "true" }
            , { mapKey = env.disableWallet, mapValue = "0" }
            , { mapKey = env.prune, mapValue = "0" }
            , { mapKey = env.rpcAllowIp, mapValue = "0.0.0.0/0" }
            , { mapKey = env.server, mapValue = "1" }
            , { mapKey = env.txIndex, mapValue = "1" }
            , { mapKey = env.zmqPubRawBlock
              , mapValue =
                  "${networkScheme}://0.0.0.0:${G.unPort zmqPubRawBlockPort}"
              }
            , { mapKey = env.zmqPubRawTx
              , mapValue =
                  "${networkScheme}://0.0.0.0:${G.unPort zmqPubRawTxPort}"
              }
            , { mapKey = env.blockFilterIndex, mapValue = "1" }
            , { mapKey = env.peerBlockFilters, mapValue = "1" }
            , { mapKey = env.rpcBind, mapValue = ":${rpcPort}" }
            , { mapKey = env.rpcPort, mapValue = rpcPort }
            , { mapKey = env.p2pPort, mapValue = G.unPort (mkP2pPort net) }
            , { mapKey = env.regTest, mapValue = mkRegTest net }
            , { mapKey = env.testNet, mapValue = mkTestNet net }
            , { mapKey = env.rpcUser, mapValue = mkRpcUser net }
            , { mapKey = env.rpcPassword, mapValue = mkRpcPass net }
            ]

let mkSetupEnv
    : G.Owner → Text
    = λ(owner : G.Owner) →
        let ownerText = G.unOwner owner

        in  ''
            #!/bin/bash

            set -e

            THIS_DIR="$(dirname "$(realpath "$0")")"

            echo "==> Setting up env for ${ownerText}"

            . "$THIS_DIR/export-${ownerText}-env.sh"

            (
              kubectl create configmap ${ownerText} \${S.concatSetupEnv
                                                         configMapEnv}
            ) || true

            (
              kubectl create secret generic ${ownerText} \${S.concatSetupEnv
                                                              secretEnv}
            ) || true
            ''

let mkPorts
    : G.BitcoinNetwork → List Natural
    = λ(net : G.BitcoinNetwork) →
        G.unPorts
          [ zmqPubRawBlockPort, zmqPubRawTxPort, mkRpcPort net, mkP2pPort net ]

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
                      { Aws = None (P.Map.Type Text Text)
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
          (Service.mkPorts (mkPorts net))

let mkVolumeSize
    : G.BitcoinNetwork → Volume.Size.Type
    = λ(net : G.BitcoinNetwork) →
        merge
          { MainNet = Volume.Size::{
            , capacity = 1
            , suffix = Volume.Suffix.Terabyte
            }
          , TestNet = Volume.Size::{
            , capacity = 100
            , suffix = Volume.Suffix.Gigabyte
            }
          , RegTest = Volume.Size::{
            , capacity = 100
            , suffix = Volume.Suffix.Megabyte
            }
          }
          net

let mkPersistentVolumeClaim
    : G.BitcoinNetwork → K.PersistentVolumeClaim.Type
    = λ(net : G.BitcoinNetwork) →
        Volume.mkPersistentVolumeClaim owner (mkVolumeSize net)

let mkContainerEnv =
        Deployment.mkEnv Deployment.EnvVarType.ConfigMap owner configMapEnv
      # Deployment.mkEnv Deployment.EnvVarType.Secret owner secretEnv

let mkContainer
    : Text → G.BitcoinNetwork → K.Container.Type
    = λ(name : Text) →
      λ(net : G.BitcoinNetwork) →
        K.Container::{
        , name
        , image = Some image
        , args = Some [ "-fallbackfee=0.0002" ]
        , env = Some mkContainerEnv
        , ports = Some (Deployment.mkContainerPorts (mkPorts net))
        , volumeMounts = Some
          [ Deployment.mkVolumeMount owner "/bitcoin/.bitcoin" ]
        }

let mkDeployment
    : G.BitcoinNetwork → K.Deployment.Type
    = λ(net : G.BitcoinNetwork) →
        Deployment.mkDeployment
          owner
          (Some K.DeploymentStrategy::{ type = Some "Recreate" })
          [ mkContainer owner net ]
          (Some [ Deployment.mkVolume owner ])

in  { zmqPubRawBlockPort
    , zmqPubRawTxPort
    , mkEnv
    , mkSetupEnv
    , mkRpcUser
    , mkRpcPass
    , mkRpcPort
    , mkService
    , mkPersistentVolumeClaim
    , mkDeployment
    }
