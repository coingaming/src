let G = ../Global.dhall

let K = ../Kubernetes/Import.dhall

let Lsp = ../Service/Lsp.dhall

let network = G.BitcoinNetwork.RegTest

in  { apiVersion = "v1"
    , kind = "List"
    , items =
      [ K.Resource.Service (Lsp.mkService network)
      , K.Resource.Deployment (Lsp.mkDeployment network)
      ]
    }
