let G = ../Global.dhall

let Rtl = ../Service/Rtl.dhall

let network = G.BitcoinNetwork.RegTest

in  G.concatExportEnv (Rtl.mkEnv network)
