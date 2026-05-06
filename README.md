# biclaR Parking 🚲🅿️

This repository aims to document the data treatment and processing steps for the biclaR Parking project.

## Data access set up 

Data should not be synced with GitHub. Instead, we use `/data/biclar-parking` folder at `ushift@alfa` server, a shared folder accessible to all users at the server.

### At RStudio server

```bash
# Adjust the location below to the location of the repository on your personal folder
$ cd ~/Repos/biclar-parking
$ mount --bind /data/biclar-parking ./data
```

### Locally

To access the data at your local machine (outside `ushift@alfa`), you must use tailscale. Refer to https://tailscale.com/docs/install for installation instructions.

Once installed and activated, you can mount the data at your local machine using sshfs.

```bash
# Adjust the location below to the location of the repository on your local machine
$ cd ~/Repos/biclar-parking
# Adjust the username to your user
$ sshfs username@alfa:/data/biclar-parking ./data
```
