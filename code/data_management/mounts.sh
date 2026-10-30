# Run at ushift-alfa!

# Mount TML validations from IMPT at /data/biclar-parking/raw
sudo mount --bind /data/TML/validacoes/ /data/biclar-parking/raw/TML_validacoes

# Mount ushift@ist /afs/.ist.utl.pt/groups/ushift/web/content/biclar 
# (used to make data available at website) to match /data/biclar-parking/dashboard_data
sudo sed -i 's/#user_allow_other/user_allow_other/' /etc/fuse.conf

sudo sshfs ist1108284@sigma.ist.utl.pt:/afs/.ist.utl.pt/groups/ushift/web/content/biclar \
  /data/biclar-parking/dashboard_data \
  -o allow_other,ServerAliveInterval=15,ServerAliveCountMax=3

ls -ld /data/biclar-parking/dashboard_data

# unmount
# sudo fusermount3 -u /data/biclar-parking/dashboard_data 