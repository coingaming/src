{ port ? 9735
, rpcport ? 10009
, restport ? 8080
, dataDir ? "./tmp"
, macaroonDir
, name
, writeText
, writeShellScriptBin
, symlinkJoin
, runCommand
, openssl
, ps
, lnd
, expect
}:
let
  lndconf = writeText "lnd.conf" ''
    [Bitcoin]

    bitcoin.active=1
    bitcoin.regtest=1
    bitcoin.node=bitcoind

    [Bitcoind]
    bitcoind.dir=${workDir}/bitcoind_alice
    bitcoind.rpchost=127.0.0.1
    bitcoind.rpcuser=developer
    bitcoind.rpcpass=developer
    bitcoind.zmqpubrawblock=tcp://127.0.0.1:28332
    bitcoind.zmqpubrawtx=tcp://127.0.0.1:28333

    [protocol]

    protocol.wumbo-channels=true

    [Application Options]
    maxpendingchannels=50
    listen=0.0.0.0:${toString port}
    rpclisten=localhost:${toString rpcport}
    restlisten=0.0.0.0:${toString restport}
    debuglevel=debug,PEER=debug
  '';
  serviceName = "lnd-${name}";
  workDir = "${dataDir}/${serviceName}";
  tlscert = runCommand "LNDTLS"
    {
      buildInputs = [ openssl ];
    } ''
    mkdir $out
    TLS_KEY="tls.key"
    TLS_CERT="tls.cert"

    echo "subjectAltName=IP:127.0.0.1,DNS:localhost,DNS:127.0.0.1,DNS:${serviceName}" > subjectAltName

    openssl ecparam -genkey -name prime256v1 -out "$TLS_KEY"
    openssl req -new -sha256 -key "$TLS_KEY" \
      -out csr.csr -subj "/CN=${serviceName}/O=${serviceName}"
    openssl x509 -req -in csr.csr \
      -sha256 -days 36500 \
      -extfile ./subjectAltName \
      -signkey "$TLS_KEY" -out "$TLS_CERT"
    cp $TLS_KEY $out
    cp $TLS_CERT $out
    rm csr.csr
    rm ./subjectAltName
  '';
  cli = writeShellScriptBin "lncli" ''
    exec ${lnd}/bin/lncli -n regtest --rpcserver 127.0.0.1:${toString rpcport} --lnddir=${workDir} "$@"
  '';
  setup = writeShellScriptBin "setup" ''
    mkdir -p "${workDir}"
    cp -f ${lndconf} ${workDir}/lnd.conf
    cp -f ${tlscert}/* ${workDir}/
    mkdir -p ${workDir}/data/chain/bitcoin/regtest
    cp --no-preserve=mode,ownership ${macaroonDir}/*macaroon* ${workDir}/data/chain/bitcoin/regtest
  '';
  start = writeShellScriptBin "start" ''
    ${lnd}/bin/lnd --lnddir=${workDir} --bitcoin.defaultchanconfs=1  > ${workDir}/stdout.log 2>&1 &
    ${ps}/bin/ps aux | grep lnd
    echo "$!" > ${workDir}/lnd.pid
    disown $(cat ${workDir}/lnd.pid)
    echo "Lnd ${name} started $!"
  '';
  init = writeShellScriptBin "init" ''

    createWallet() {
    ${expect}/bin/expect <<- EOF
      spawn sh -c "${cli}/bin/lncli create";
      expect "Input wallet password: ";
      send "developer\r";
      expect "Confirm password: ";
      send "developer\r";
      expect "Do you have";
      send "n\r";
      expect "Input your passphrase if ";
      send "\r";
      expect "lnd successfully initialized!";
      sleep 2;
      interact;
    EOF
    }

    ( echo "${serviceName} ==> Checking Lnd wallet" && \
      ${cli}/bin/lncli getinfo ) || \
    ( echo "${serviceName} ==> Unlocking Lnd wallet" && \
      echo developer | ${cli}/bin/lncli unlock --stdin ) || \
    ( echo "${serviceName} ==> Creating Lnd wallet" && \
      createWallet ) || \
    ( echo "${serviceName} ==> INIT ERROR" && \
      exit 1 )
  '';
  stop = writeShellScriptBin "stop" ''
    lnd_pid=`cat ${workDir}/lnd.pid`
    echo "Stoping lnd ${name} $lnd_pid"
    timeout 5 ${cli}/bin/lncli stop
    kill -9 "$lnd_pid"
    rm -rf ${workDir}
  '';
  up = writeShellScriptBin "up" ''
    ( kill -0 `cat ${workDir}/lnd.pid` && \
      echo "==> ${serviceName} is still running" ) || \
    ( ${setup}/bin/setup && \
      ${start}/bin/start && \
      ${init}/bin/init )
  '';
  down = writeShellScriptBin "down" ''
    ${stop}/bin/stop
  '';
in {
  inherit up down tlscert cli;
}
