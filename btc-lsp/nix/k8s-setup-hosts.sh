#!/bin/sh

THIS_DIR="$(dirname "$(realpath "$0")")"
LOCALHOST=127.0.0.1
HOSTS_FILE=/etc/hosts

. "$THIS_DIR/k8s-export-env.sh"

CLUSTER_IP=`minikube ip --profile=$MINIKUBE_PROFILE`

if [ `uname -s` = "Darwin" ]; then
  IP_ADDRESS="$LOCALHOST"
else
  IP_ADDRESS="$CLUSTER_IP"
fi

setup () {
  ENTRY="$IP_ADDRESS $1"
  echo "Adding $ENTRY to $HOSTS_FILE"
  grep -q "$ENTRY" /etc/hosts || echo "$ENTRY" >> $HOSTS_FILE
}

setup "postgres"
setup "bitcoind"

for OWNER in lsp alice bob; do
  setup "lnd-$OWNER"
done

setup "rtl"
setup "btc-lsp"