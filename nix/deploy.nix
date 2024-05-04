{ pkgs }:
pkgs.writeShellScriptBin "deployScript" ''
    #!/usr/bin/env bash
    # This script is used to deploy NixOS configurations to multiple targets
    # It reads a list of deploy targets from a yaml file and builds and deploys
    export PATH=$PATH:${pkgs.jq}/bin

    while getopts ":ps" opt; do
      case $opt in
        p)
          export DEPLOY_TARGETS=$(cat $PROD_TARGETS)
          echo "Deploying to production"
          ;;
        s)
          export DEPLOY_TARGETS=$(cat $STAGING_TARGETS)
          echo "Deploying to staging"
          ;;
      esac
    done

    if [[ -z "$DEPLOY_TARGETS" ]]; then
      echo "No deploy targets found"
      exit 1
    fi

    if ! test -f $DEPLOY_SSH_KEY; then
      echo "No deploy test key found at $DEPLOY_SSH_KEY"
      exit 1
    fi


    set -x

    # Build each of the deploy targets
    # If one fails to build, the script will exit 1 and the deployment will not continue
    jq -c '.[]' <<<"$DEPLOY_TARGETS" | while read target; do
      nix_target=$(echo $target | jq -r '.nix_target')
      echo "Building $nix_target"
      nixos-rebuild build --flake .#$nix_target
      build_status=$?
      if [ $build_status -ne 0 ]; then
        echo "Error building $nix_target"
        exit 1
      fi
    done

    # Attempt to deploy each of the deploy targets
    # If one fails to deploy, the script will continue to deploy the remaining targets
    deploy_failed=false
    jq -c '.[]' <<<"$DEPLOY_TARGETS" | while read target; do
      target_name=$(echo $target | jq -r '.name')
      nix_target=$(echo $target | jq -r '.nix_target')
      target_host=$(echo $target | jq -r '.host')
      build_host=$(echo $target | jq -r '.build_host')
      user=$(echo $target | jq -r '.user')
      pub_key_file=$(mktemp)
      echo "$target_host $(echo $target | jq -r '.pub_key')" > $pub_key_file
      echo "" >> $pub_key_file

      echo "Deploying to $target_name"
      export NIX_SSHOPTS="-i $DEPLOY_SSH_KEY -o UserKnownHostsFile=$pub_key_file"
      echo $nix_target

      # Deploy the target, 
      nixos-rebuild switch --flake .#$nix_target --use-remote-sudo --target-host "$user@$target_host"
      switch_status=$?
      if [ $switch_status -ne 0 ]; then
        echo "Error deploying to $target_name"
        deploy_failed=true # Mark that a deploy failed so the script can exit 1 at the end but continue to deploy the remaining targets
      fi
      echo "finished deploying to $target_name"
    done

    if [[ $deploy_failed == "true" ]]; then
      echo "deploy failed"
      exit 1
    else
      echo "deploy finished successfully"
      exit 0
    fi
  ''
