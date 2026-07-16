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
# Open your repository directory
# Adjust the location below to the location of the repository on your local machine
$ cd ~/Repos/biclar-parking
# Create a data directory inside your repo
mkdir data
# Adjust the username to your user
$ sshfs username@ushift-alfa:/data/biclar-parking ./data
```

## Code structure and data management

Code should be stored in [`/code`](./code) directory.

Scripts should be named using the convention `XX_name_camel_case.R`, with `XX` a sequential number and a brief but explanatory enough name, written in camel case.

[`00_data_load.R`](./code/00_data_load.R) and [`99_results_load.R`](./code/99_results_load.R) are special scripts for loading data and results, respectively. You should extend them if in any of your scripts you generate new base data or results. Make sure to add only the output of your scripts to these files.

Data should be stored in the [`/data`](./data) directory, with subdirectories for each data category. If there is not a subfolder for the category you are working with, create one. Mind that this folder is not synced with Git. Refer to [Data access set up](#Data access set up) on instructions on how to sync this folder with the remote server.

[`/useful_data`](./useful_data) is an alternative folder to store data, synced with Git, but should only be used for contextual base data (with small size) that should be shared publicly to enable results reproducibility/interpretability. For instance, CSV tables with geometries ids and associated metadata. 

## Base geo layers

Base geographic layers for analysis are: grid, parishes, municipalities. Their geometries are stored at [`/data/geo`](./data/geo) directory.

Each is identified by an id column, named with the first letter of the layer type. For instance, the id for a grid cell is called `gid`, for a parish `pid`, and for a municipality `mid`.

Metadata for each is stored in [`/useful_data`](./useful_data), associating, to each layer, id, the corresponding name, relations with other layers and NUTS2 (`nid`).

