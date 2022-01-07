# cpldgrid_gen

# Introduction

The gen_fixgrid program and associated script related functions create
the files required for Fix and IC files for the coupled model

This document is part of the <a href="../index.html">UFS_UTILS
documentation</a>.

The cpld_gengrid program is part of the
[UFS_UTILS](https://github.com/ufs-community/UFS_UTILS) project.


| Filename                | Description                            | Function |
| :---------------------- | :------------------------------------- | :------- |
| tripole.mx025.nc        | master grid file                       | Creating all subsequent grid or mapping files |
| grid_cice_NEMS_mx025.nc | the CICE grid file                     | used at runtime by CICE6 |
| kmtu_cice_NEMS_mx025.nc | the CICE mask file                     | used at runtime by CICE6 |
| mesh.mx025.nc           | the ocean and ice mesh file            | used at runtime by CICE6, MOM6, and CMEPS |
| C384.mx025.tile[1-6].nc | the mapped ocean mask on the ATM tiles | used to create ATM ICs consistent with the fractional grid |



| Filename                                                                  | Function     |
| :------------------------------------------------------------------------ | :----------- |
| tripole.mx025.[Cu][Cv][Bu].to.Ct.bilinear.nc                              | the ESMF weights for mapping OCN or ICE output fields from the various stagger locations on the tripole grid to the center (Ct) stagger location on the tripole grid using bilinear mapping |
| tripole.mx025.Ct.to.rect.[destination resolution].[bilinear][conserve].nc | the ESMF weights for mapping variables on the center (Ct) stagger location on the tripole grid to a rectilinear grid with *destination resolution* using either bilinear or conservative mapping | 


| Filename                                                                  | Function     |
| :------------------------------------------------------------------------ | :----------- |
| tripole.mx025.Ct.to.mx[destination resolution].Ct.neareststod.nc          | the ESMF weights for mapping the 1/4 CICE ICs to a tripole *destination resolution* using nearest source-to-destination mapping |


| Filename                                                                  | Function     |
| :------------------------------------------------------------------------ | :----------- |
| ufs.[Default filename].nc                                                 | Topo-edits required for UFS application. These are appended to the existing default topo edits file and implemented at run time with the parameter flag ``ALLOW_LANDMASK_CHANGES=true``. All files produced by the *run_gridgen.sh* will be consistent  with this run-time land mask |