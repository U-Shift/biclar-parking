# Run at ushift-alfa!

# Mount TML validations from IMPT at /data/biclar-parking/raw
sudo mount --bind /data/TML/validacoes/ /data/biclar-parking/raw/TML_validacoes

# Mount ushift@ist /afs/.ist.utl.pt/groups/ushift/web/content/biclar 
# (used to make data available at website) to match /data/biclar-parking/dashboard_data
#
# NOTE: On sigma.ist.utl.pt, $HOME is inside AFS (/afs/.ist.utl.pt/users/...), which is NOT readable
# before Kerberos/AFS authentication completes. Therefore, the Kerberos keytab must live on
# ushift-alfa (not on sigma), and ushift-alfa uses GSSAPI (Kerberos) over SSH to authenticate
# and delegate renewable credentials to sigma automatically.
#
# --- ONE-TIME SETUP ON ushift-alfa ---
# 1. Install Kerberos client tools & sshfs (if not installed):
#    sudo apt-get update && sudo apt-get install -y krb5-user kstart sshfs
#
# 2. Create the Kerberos keytab on ushift-alfa (stores encrypted key, no plaintext password):
#    sudo ktutil
#    ktutil:  addent -password -p ist1108284@IST.UTL.PT -k 1 -e aes256-cts-hmac-sha1-96
#    Password for ist1108284@IST.UTL.PT: <enter your IST password>
#    ktutil:  wkt /etc/ist1108284.keytab
#    ktutil:  quit
#    sudo chmod 600 /etc/ist1108284.keytab
#
# 3. Ensure /etc/fuse.conf allows other users:
sudo sed -i 's/#user_allow_other/user_allow_other/' /etc/fuse.conf

# 4. Obtain Kerberos ticket from keytab and mount:
#    NOTE: Must connect to a specific node (sigma01.ist.utl.pt), NOT the round-robin alias
#    (sigma.ist.utl.pt), because Kerberos host principals in IST.UTL.PT are registered per node
#    (host/sigma01.ist.utl.pt@IST.UTL.PT). Using sigma.ist.utl.pt causes GSSAPI to fail and ask
#    for a password!
sudo kinit -l 10h -k -t /etc/ist1108284.keytab ist1108284@IST.UTL.PT

sudo umount -l /data/biclar-parking/dashboard_data 2>/dev/null || true
sudo sshfs ist1108284@sigma01.ist.utl.pt:/afs/.ist.utl.pt/groups/ushift/web/content/biclar \
  /data/biclar-parking/dashboard_data \
  -o allow_other,reconnect,ServerAliveInterval=15,ServerAliveCountMax=3,StrictHostKeyChecking=no,GSSAPIAuthentication=yes,GSSAPIDelegateCredentials=yes

# 5. Keep the Kerberos ticket and remote AFS PAG token alive via cron (every 2 hours):
sudo tee /etc/cron.d/ist-sshfs > /dev/null << 'EOF'
SHELL=/bin/bash
PATH=/usr/local/sbin:/usr/local/bin:/sbin:/bin:/usr/sbin:/usr/bin
0 */2 * * * root kinit -l 10h -k -t /etc/ist1108284.keytab ist1108284@IST.UTL.PT && { umount -l /data/biclar-parking/dashboard_data 2>/dev/null || true; sleep 1; sshfs ist1108284@sigma01.ist.utl.pt:/afs/.ist.utl.pt/groups/ushift/web/content/biclar /data/biclar-parking/dashboard_data -o allow_other,reconnect,ServerAliveInterval=15,ServerAliveCountMax=3,StrictHostKeyChecking=no,GSSAPIAuthentication=yes,GSSAPIDelegateCredentials=yes; }
EOF
sudo chmod 644 /etc/cron.d/ist-sshfs

df -h /data/biclar-parking/dashboard_data

# unmount
# sudo umount -l /data/biclar-parking/dashboard_data