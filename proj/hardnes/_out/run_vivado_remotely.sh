#!/bin/sh

PROJECT_NAME="nexys_a7_100t"

PROJECT_FILES="$PROJECT_NAME.v $PROJECT_NAME.tcl $PROJECT_NAME.xdc"
VIVADO_COMMAND="vivado -mode batch -source $PROJECT_NAME.tcl"
COPY_FILES_ON_COMPLETION="$PROJECT_NAME.bit $PROJECT_NAME.timing.rpt vivado.log"

if [ $# -ne 1 ]; then
  echo "Usage: $0 <user@server_ip>"
  exit 1
fi

server_arg="$1"
socket_file="/tmp/ssh_socket_$$"

# Cleanup function
cleanup() {
  ssh -S "$socket_file" -O exit "$server_arg" 2> /dev/null
}

echo "Connecting to server, you may be prompted to authenticate..."

# Create master SSH connection in background after auth
ssh -fNM -S "$socket_file" "$server_arg"

# Set cleanup to run on exit
trap cleanup EXIT

echo "Checking for vivado on server..."

# Check if vivado is available on server
if ! vivado_path=$(ssh -S "$socket_file" "$server_arg" "which vivado"); then
  echo "Error: vivado not found on server"
  exit 1
fi

echo "Vivado path: $vivado_path"

# Create temp directory on server
temp_dir=$(ssh -S "$socket_file" "$server_arg" "mktemp -d")
echo "Server temp directory: $temp_dir"

echo "Copying project files to server..."

# Copy project files
scp -o "ControlPath=$socket_file" $PROJECT_FILES "$server_arg:$temp_dir/"

echo "Running vivado build..."

# Run vivado command
if ! ssh -S "$socket_file" "$server_arg" "cd $temp_dir && $VIVADO_COMMAND"; then
  echo "Vivado build failed, but continuing to copy files..."
fi

echo "Copying build output files..."

# Copy files that exist
for file in $COPY_FILES_ON_COMPLETION; do
  if ssh -S "$socket_file" "$server_arg" "test -f $temp_dir/$file"; then
    echo "Copying $file"
    scp -o "ControlPath=$socket_file" "$server_arg:$temp_dir/$file" .
  fi
done

echo "Build completed, files copied back, SSH connection closed"
