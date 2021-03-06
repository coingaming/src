let G = ../Global.dhall

let C = ../CloudProvider.dhall

let K = ../Kubernetes/Import.dhall

let Lnd = ../Service/Lnd.dhall

let network = G.BitcoinNetwork.RegTest

let owner = G.Owner.Lnd

in  { apiVersion = "v1"
    , kind = "List"
    , items =
      [ K.Resource.Service (Lnd.mkService network owner (None C.ProviderType))
      , K.Resource.PersistentVolumeClaim
          (Lnd.mkPersistentVolumeClaim network owner)
      , K.Resource.Deployment (Lnd.mkDeployment network owner)
      ]
    }
