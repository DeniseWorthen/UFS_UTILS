# cpldgrid_gen

# Introduction

The gen_fixgrid program and associated script related functions create
the files required for Fix and IC files for the coupled model

This document is part of the <a href="../index.html">UFS_UTILS
documentation</a>.

The cpld_gengrid program is part of the
[UFS_UTILS](https://github.com/ufs-community/UFS_UTILS) project.


| Filename                | Description                            | Function |
| :----:                  | :----:                                 | :----:   |
| tripole.mx025.nc        | master grid file                       | Creating all subsequent grid or mapping files |
| grid_cice_NEMS_mx025.nc | the CICE grid file                     | used at runtime by CICE6 |
| kmtu_cice_NEMS_mx025.nc | the CICE mask file                     | used at runtime by CICE6 |
| mesh.mx025.nc           | the ocean and ice mesh file            | used at runtime by CICE6, MOM6, and CMEPS |
| C384.mx025.tile[1-6].nc | the mapped ocean mask on the ATM tiles | used to create ATM ICs consistent with the fractional grid |