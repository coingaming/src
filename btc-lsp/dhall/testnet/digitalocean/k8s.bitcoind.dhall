let G = ../Global.dhall

let K = ../Kubernetes/Import.dhall

let Bitcoind = ../Service/Bitcoind.dhall

let network = G.BitcoinNetwork.TestNet

let cloudProvider = G.CloudProvider.DigitalOcean

in  { apiVersion = "v1"
    , kind = "List"
    , items =
      [ K.Resource.Service (Bitcoind.mkService network cloudProvider)
      , K.Resource.PersistentVolumeClaim
          (Bitcoind.mkPersistentVolumeClaim network)
      , K.Resource.Deployment (Bitcoind.mkDeployment network)
      ]
    }
