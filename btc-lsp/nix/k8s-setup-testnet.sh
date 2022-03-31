#!/bin/bash

set -e

THIS_DIR="$(dirname "$(realpath "$0")")"
BUILD_DIR="$THIS_DIR/../build"
BITCOIN_NETWORK="testnet"

# Service-specific files are here
BITCOIND_PATH="$BUILD_DIR/bitcoind"
LND_PATH="$BUILD_DIR/lnd"
RTL_PATH="$BUILD_DIR/rtl"
LSP_PATH="$BUILD_DIR/lsp"
POSTGRES_PATH="$BUILD_DIR/postgres"

# Save all LetsEncrypt stuff here
LETSENCRYPT_DIR="$BUILD_DIR/letsencrypt"

# Save domain name here
LND_DOMAIN_PATH="$LND_PATH/domain.txt"
RTL_DOMAIN_PATH="$RTL_PATH/domain.txt"

cleanBuildDir () {
  echo "Deleting everything in $BUILD_DIR"
  rm -rf "$BUILD_DIR" && mkdir -p "$BUILD_DIR"

  echo "==> Generating new credentials"
  sh "$THIS_DIR/hm-shell-docker.sh" \
    --mini \
    "--run './nix/ns-gen-creds.sh'"
}

confirmAction () {
  local ASK="$1"
  local ACTION="$2"

  while true; do
    read -p "$ASK (y/N) ? " CONFIRM
    case "$CONFIRM" in
      [Yy]* ) eval "$ACTION"; break;;
      [Nn]* ) break;;
      * ) echo "Please answer yes or no.";;
    esac
  done
}

isInstalled () {
  local COMMAND_NAME="$1"

  if ! command -v "$COMMAND_NAME" &> /dev/null; then
    echo "Please install \"$COMMAND_NAME\" before continuing"
    exit 1;
  fi
}

getLetsEncryptCert () {
  echo "Requesting cert for $DOMAIN_NAME..."
  certbot certonly \
    -d "*.$DOMAIN_NAME" \
    --agree-tos \
    --manual \
    --preferred-challenges dns \
    --register-unsafely-without-email \
    --config-dir "$LETSENCRYPT_DIR/etc" \
    --work-dir "$LETSENCRYPT_DIR/lib" \
    --logs-dir "$LETSENCRYPT_DIR/log"
}

copyLetsEncryptCert () {
  local COPY_PATH="$1"
  local DOMAIN_DIR="$LETSENCRYPT_DIR/etc/live/$DOMAIN_NAME"

  echo "Copying files generated by LetsEncrypt to $COPY_PATH"
  cp "$DOMAIN_DIR/cert.pem" "$COPY_PATH/tls.cert"
  cp "$DOMAIN_DIR/privkey.pem" "$COPY_PATH/tls.key"
}

fileExistNotEmpty () {
  local FILEPATH="$1"

  if [ -f "$FILEPATH" ]; then
    if [ ! -s "$FILEPATH" ]; then
      echo "$FILEPATH is empty"
      exit 1;
    fi
  else
    echo "$FILEPATH does not exist"
    exit 1;
  fi
}

setupLetsEncryptCert() {
  isInstalled certbot && \
  mkdir -p "$LETSENCRYPT_DIR" && \
  getLetsEncryptCert && \
  copyLetsEncryptCert "$RTL_PATH" && \
  copyLetsEncryptCert "$LSP_PATH"
}

createKubernetesCluster () {
  local K8S_CLUSTER_NAME="$1"

  echo "Creating \"$K8S_CLUSTER_NAME\" k8s cluster on DigitalOcean..."
  doctl kubernetes cluster create "$K8S_CLUSTER_NAME" \
    --count 3 \
    --region ams3 \
    --1-clicks ingress-nginx \
    --size s-4vcpu-8gb
}

deleteKubernetesCluster () {
  local K8S_CLUSTER_NAME="$1"

  echo "Deleting \"$K8S_CLUSTER_NAME\" k8s cluster from DigitalOcean..."
  doctl kubernetes cluster delete "$K8S_CLUSTER_NAME" --dangerous
}

setupKubernetesCluster () {
  isInstalled doctl && doctl account get

  local K8S_CLUSTER_NAME="$1"

  if doctl kubernetes cluster get "$K8S_CLUSTER_NAME"; then
    confirmAction \
    "==> Delete existing \"$K8S_CLUSTER_NAME\" k8s cluster and create a new one?" \
    "deleteKubernetesCluster $K8S_CLUSTER_NAME && createKubernetesCluster $K8S_CLUSTER_NAME"
  else
    confirmAction \
    "==> Create new \"$K8S_CLUSTER_NAME\" k8s cluster?" \
    "createKubernetesCluster $K8S_CLUSTER_NAME"
  fi
}

getPostgresInstanceId () {
  local PG_INSTANCE_NAME="$1"

  doctl databases list --no-header | grep "$PG_INSTANCE_NAME" | awk '{print $1}'
}

createPostgresInstance () {
  local PG_INSTANCE_NAME="$1"

  echo "Creating \"$PG_INSTANCE_NAME\" database instance on DigitalOcean..."
  doctl databases create "$PG_INSTANCE_NAME" \
    --engine pg \
    --region ams3 \
    --size db-s-1vcpu-1gb
}

deletePostgresInstance () {
  local PG_INSTANCE_NAME="$1"

  echo "Deleting \"$PG_INSTANCE_NAME\" database instance from DigitalOcean..."
  doctl databases delete `getPostgresInstanceId $PG_INSTANCE_NAME`
}

writePostgresURI() {
  local PG_INSTANCE_NAME="$1"
  local PG_INSTANCE_ID=`getPostgresInstanceId $1`
  local PG_URI=`doctl databases connection $PG_INSTANCE_ID --format URI --no-header`
  local PG_CONN_PATH="$POSTGRES_PATH/conn.txt"

  echo "==> Saving pg connection details"
  mkdir -p "$POSTGRES_PATH"

  echo -n "$PG_URI" > "$PG_CONN_PATH"
  echo "Saved connection details to $PG_CONN_PATH"
}

setupPostgresInstance () {
  isInstalled doctl && doctl account get

  local PG_INSTANCE_NAME="$1"
  local PG_INSTANCE_ID=`getPostgresInstanceId $PG_INSTANCE_NAME`

  if [ -n "$PG_INSTANCE_ID" ]; then
    confirmAction \
    "==> Delete existing \"$PG_INSTANCE_NAME\" database instance and create a new one?" \
    "deletePostgresInstance $PG_INSTANCE_NAME && createPostgresInstance $PG_INSTANCE_NAME && writePostgresURI $PG_INSTANCE_NAME"
  else
    confirmAction \
    "==> Create new \"$PG_INSTANCE_NAME\" database instance?" \
    "createPostgresInstance $PG_INSTANCE_NAME && writePostgresURI $PG_INSTANCE_NAME"
  fi
}

confirmAction \
"==> Clean up previous build?" \
"cleanBuildDir"

if [ ! -f "$LND_DOMAIN_PATH" ] || [ ! -f "$RTL_DOMAIN_PATH" ]; then
  echo "==> Domain name must be set before continuing"
  read -p "Input your domain name: " "DOMAIN_NAME"

  echo "Saving $DOMAIN_NAME to $LND_DOMAIN_PATH"
  echo -n "$DOMAIN_NAME" > "$LND_DOMAIN_PATH"

  echo "Saving $DOMAIN_NAME to $RTL_DOMAIN_PATH"
  echo -n "$DOMAIN_NAME" > "$RTL_DOMAIN_PATH"
fi

confirmAction \
"==> Setup LetsEncrypt certificate?" \
"setupLetsEncryptCert"

echo "==> Checking that all required files exist and are not empty"
fileExistNotEmpty "$BITCOIND_PATH/rpcuser.txt" 
fileExistNotEmpty "$BITCOIND_PATH/rpcpass.txt"

fileExistNotEmpty "$LND_PATH/walletpassword.txt" 

fileExistNotEmpty "$RTL_PATH/tls.cert" 
fileExistNotEmpty "$RTL_PATH/tls.key"
fileExistNotEmpty "$RTL_PATH/multipass.txt"

fileExistNotEmpty "$LSP_PATH/tls.cert" 
fileExistNotEmpty "$LSP_PATH/tls.key"
echo "All files are OK."

setupKubernetesCluster "lsp-$BITCOIN_NETWORK"
setupPostgresInstance "lsp-$BITCOIN_NETWORK"

echo "==> Checking that postgres connection details are saved"
fileExistNotEmpty "$POSTGRES_PATH/conn.txt" 
echo "Connection details are OK."

echo "==> Partial dhall"
sh "$THIS_DIR/hm-shell-docker.sh" --mini \
   "--run './nix/ns-inline-creds.sh && ./nix/ns-dhall-compile.sh $BITCOIN_NETWORK'"

echo "==> Configuring environment for containers"
sh "$THIS_DIR/k8s-setup-env.sh"

echo "==> Deploying k8s resources"
sh "$THIS_DIR/k8s-deploy.sh" "bitcoind lnd"

echo "==> Waiting until containers are ready"
sh "$THIS_DIR/k8s-wait.sh" "bitcoind lnd"

echo "==> Partial spin"
sh "$THIS_DIR/k8s-lazy-init-unlock.sh"
sleep 20

echo "==> Exporting creds from running pods"
sh "$THIS_DIR/k8s-export-creds.sh"

echo "==> Full dhall"
sh "$THIS_DIR/hm-shell-docker.sh" --mini \
   "--run './nix/ns-inline-creds.sh && ./nix/ns-dhall-compile.sh $BITCOIN_NETWORK'"

echo "==> Configuring environment for containers"
sh "$THIS_DIR/k8s-setup-env.sh"

echo "==> Deploying additional k8s resources"
sh "$THIS_DIR/k8s-deploy.sh" "rtl lsp"

echo "==> Waiting until containers are ready"
sh "$THIS_DIR/k8s-wait.sh" "rtl lsp"

echo "==> Setup for $BITCOIN_NETWORK has been completed!"
