let P = ../Prelude/Import.dhall

let K = ../Kubernetes/Import.dhall

let G = ../Global.dhall

let Service = ../Kubernetes/Service.dhall

let Ingress = ../Kubernetes/Ingress.dhall

let Deployment = ../Kubernetes/Deployment.dhall

let owner = G.unOwner G.Owner.Rtl

let image = "heathmont/rtl:9c8d7d6"

let tcpPort
    : G.Port
    = { unPort = 3000 }

let ports
    : List Natural
    = G.unPort [ tcpPort ]

let mkServiceType
    : G.BitcoinNetwork → Service.ServiceType
    = λ(net : G.BitcoinNetwork) →
        merge
          { MainNet = Service.ServiceType.LoadBalancer
          , TestNet = Service.ServiceType.LoadBalancer
          , RegTest = Service.ServiceType.ClusterIP
          }
          net

let mkService
    : G.BitcoinNetwork → K.Service.Type
    = λ(net : G.BitcoinNetwork) →
        Service.mkService owner (mkServiceType net) (Service.mkPorts ports)

let mkHost
    : G.BitcoinNetwork → Text
    = λ(net : G.BitcoinNetwork) →
        merge
          { MainNet = "rtl.coins.io"
          , TestNet = "testnet-rtl.coins.io"
          , RegTest = owner
          }
          net

let mkTls
    : G.BitcoinNetwork → Optional K.IngressTLS.Type
    = λ(net : G.BitcoinNetwork) →
        let tls = Some (Ingress.mkTls (mkHost net) "${owner}-tls")

        in  merge
              { MainNet = tls, TestNet = tls, RegTest = None K.IngressTLS.Type }
              net

let mkIngress
    : G.BitcoinNetwork → K.Ingress.Type
    = λ(net : G.BitcoinNetwork) →
        let tlsList : Optional (List K.IngressTLS.Type) =
              P.Optional.map
                K.IngressTLS.Type
                (List K.IngressTLS.Type)
                (λ(tls : K.IngressTLS.Type) → [ tls ])
                (mkTls net)

        in  Ingress.mkIngress owner (mkHost net) tcpPort.unPort tlsList

let configMapEnv
    : List Text
    = [ "CONFIG_FROM_ENV" ]

let secretEnv
    : List Text
    = [ "RTL_CONFIG_JSON", "RTL_CONFIG_NODES_JSON" ]

let env =
        Deployment.mkEnv Deployment.EnvVarType.ConfigMap owner configMapEnv
      # Deployment.mkEnv Deployment.EnvVarType.Secret owner secretEnv

let mkContainer
    : Text → G.BitcoinNetwork → K.Container.Type
    = λ(name : Text) →
      λ(net : G.BitcoinNetwork) →
        K.Container::{
        , name
        , image = Some image
        , env = Some env
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

in  { tcpPort, mkService, mkDeployment, mkIngress }
