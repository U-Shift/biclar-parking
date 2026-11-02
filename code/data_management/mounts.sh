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

# 4. Obtain/renew Kerberos ticket from keytab and mount (and keep renewing in background):
#    Note: An active sshfs session on sigma also needs its remote AFS token refreshed before
#    the ~10h token lifetime expires. Adding a 6-hour cron entry in /etc/cron.d/ist-sshfs
#    ensures both the local Kerberos ticket and the remote AFS token on sigma stay fresh forever:
#
#    cat << 'EOF' | sudo tee /etc/cron.d/ist-sshfs
#    0 */6 * * * root kinit -k -t /etc/ist1108284.keytab ist1108284@IST.UTL.PT && ssh -o GSSAPIAuthentication=yes -o GSSAPIDelegateCredentials=yes ist1108284@sigma.ist.utl.pt "aklog" || (fusermount3 -uz /data/biclar-parking/dashboard_data; sshfs ist1108284@sigma.ist.utl.pt:/afs/.ist.utl.pt/groups/ushift/web/content/biclar /data/biclar-parking/dashboard_data -o allow_other,reconnect,ServerAliveInterval=15,ServerAliveCountMax=3,GSSAPIAuthentication=yes,GSSAPIDelegateCredentials=yes)
#    EOF

sudo kinit -k -t /etc/ist1108284.keytab ist1108284@IST.UTL.PT

sudo sshfs ist1108284@sigma.ist.utl.pt:/afs/.ist.utl.pt/groups/ushift/web/content/biclar \
  /data/biclar-parking/dashboard_data \
  -o allow_other,reconnect,ServerAliveInterval=15,ServerAliveCountMax=3,GSSAPIAuthentication=yes,GSSAPIDelegateCredentials=yes

ls -ld /data/biclar-parking/dashboard_data

# unmount
# sudo fusermount3 -u /data/biclar-parking/dashboard_data