let G = ../Global.dhall

let K = ../Kubernetes/Import.dhall

let Bitcoind = ../Service/Bitcoind.dhall

let network = G.BitcoinNetwork.MainNet

in 
{ apiVersion = "v1"
, kind = "List"
, items = 
  [
    K.Resource.Service (Bitcoind.mkService network)
    , K.Resource.PersistentVolumeClaim (Bitcoind.mkPersistentVolumeClaim network)
    , K.Resource.Deployment (Bitcoind.mkDeployment network)
  ]
}
