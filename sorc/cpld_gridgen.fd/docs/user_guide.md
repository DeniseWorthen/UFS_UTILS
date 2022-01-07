# cpldgrid_gen

# Introduction

The cpld_gengrid program and associated script related functions create the files required for Fix and IC files for the coupled model.

This document is part of the <a href="../index.html">UFS_UTILS
documentation</a>.

The cpld_gengrid program is part of the
[UFS_UTILS](https://github.com/ufs-community/UFS_UTILS) project.

## Creating Fix and IC files required for the Coupled Model

For the UFS coupled model application S2S or S2SW, the following fix files are required:

- The CICE6 grid and mask file

- The mesh file for the desired OCN/ICE resolution, which is identical for MOM6 and CICE6.

- The mapped ocean mask on the FV3 tiles

- The ESMF regridding weights required to create the CICE6 IC from CPC (SIS2) reanalysis.

- The ESMF regridding weights required to remap the CICE6 or MOM6 output from tripole grid to a rectilinear grid (optional).

Since MOM6 creates the model grid at runtime (including adjusting the land mask, if required), the required files for CICE and UFSAtm must be created in a pre-processing step using only the MOM6 supergrid, topography and land mask files as input. This allows the mapped ocean mask (used for creating of the ATM ICs) and the CICE6 grid and mask files to be consistent with the run-time configuration of MOM6.

## Terminology

MOM6 uses an Arakawa C grid staggering. Within cpld_gridgen, these are referred to as "stagger" locations, and named as follows:

                 Bu────Cv─────Bu
                 │            │
                 │            │
                 Cu    Ct     Cu
                 │            │
                 │            │
                 Bu────Cv─────Bu



## Generating the grid files

The cpld_gridgen program and associated script related functions perform the following tasks:

1. reads the MOM6 supergrid and ocean mask file and optionally creates the required *topo_edits* file if the land mask for MOM6 is to be changed at runtime.
2. creates a master grid file containing all stagger locations of the grid fully defined
3. creates the CICE6 grid variables and writes the required CICE6 grid file
4. creates a SCRIP file for the center stagger (tracer) grid points and a second SCRIP file also containing the land mask
5. creates the ESMF conservative regridding weights to map the ocean mask to the FV3 tiles and write the mapped mask to 6 tile files
6. creates the ESMF regridding weights to map the 1/4 CICE6 restart file to a lower resolution tripole grid 
7. optionally calls a routine to generate ESMF regridding weights to map the tripole grid to a set of rectilinear grids
8. uses the command line tool *ESMF_Scrip2Unstruct* to generate the ocean mesh from the SCRIP file containing the land mask (item 4)
9. uses the NCO command line tool to generate the CICE6 land mask file from the CICE6 grid file

## The generated files

The exact list of files produced by the *run_gridgen.sh* script will vary depending on several factors. For example, if the *DO_POSTWGHTS* is true, then a SCRIP format file will be produced for each rectilinear destination grid desired and a file containing the regridding weights to map from the center (Ct, tracer) stagger point to the rectilinear grid will also be written. If an OCN/ICE grid resolution less than 1/4 degree is chosen, then a file containing regridding weights from the 1/4 degree grid to a lower resolution grid will also be written. Note also that multiple intermediate SCRIP format files may be produced depending on the options chosen.

* Executing the script for the 1/4 deg OCN/ICE resolution will result in the following files being produced in the output location:

| Filename                | Description                            | Function |
| :---------------------- | :------------------------------------- | :------- |
| tripole.mx025.nc        | master grid file                       | Creating all subsequent grid or mapping files |
| grid_cice_NEMS_mx025.nc | the CICE grid file                     | used at runtime by CICE6 |
| kmtu_cice_NEMS_mx025.nc | the CICE mask file                     | used at runtime by CICE6 |
| mesh.mx025.nc           | the ocean and ice mesh file            | used at runtime by CICE6, MOM6, and CMEPS |
| C384.mx025.tile[1-6].nc | the mapped ocean mask on the ATM tiles | used to create ATM ICs consistent with the fractional grid |

* If the optional post-weights are generated, the following files will be produced in the output location: 

| Filename                                                                  | Function     |
| :------------------------------------------------------------------------ | :----------- |
| tripole.mx025.[Cu][Cv][Bu].to.Ct.bilinear.nc                              | the ESMF weights for mapping OCN or ICE output fields <br>                                                                                from the various stagger locations on the tripole grid <br>                                                                               to the center (Ct) stagger location on the tripole grid <br>
                                                                              using bilinear mapping |
| tripole.mx025.Ct.to.rect.[destination resolution].[bilinear][conserve].nc | the ESMF weights for mapping variables on the center <br>                                                                                 (Ct) stagger location on the tripole grid to a <br>                                                                                       rectilinear grid with [destination resolution] using <br>
                                                                              either bilinear or conservative mapping | 
                                                                              
* If a resolution other than 1/4 degree is used in *run_gridgen.sh*, the following file will be produced in the output location: 

| Filename                                                                  | Function     |
| :------------------------------------------------------------------------ | :----------- |
| tripole.mx025.Ct.to.mx[destination resolution].Ct.neareststod.nc          | the ESMF weights for mapping the 1/4 CICE ICs to a <br>                                                                                   tripole [destination resolution] using nearest <br>
                                                                              source-to-destination mapping |
                                                                              
* If run-time land mask changes for MOM6 are requested, the following file will be produced in the output location:

| Filename                                                                  | Function     |
| :------------------------------------------------------------------------ | :----------- |
| ufs.[Default filename].nc                                                 | Topo-edits required for UFS application. These are <br>
                                                                              appended to the existing default topo edits file and <br>                                                                                 implemented at run time with the parameter flag <br>                                                                                      ``ALLOW_LANDMASK_CHANGES=true``. All files produced by <br>
                                                                              the *cpld_gridgen.sh* will be consistent  with this <br>                                                                                  run-time land mask |



















<table>
<caption id="multi_row">Output files for 1/4 deg</caption>
<tr><th>File name                      <th>Description                              <th>Function
<tr><td row=1>tripole.mx025.nc         <td>master grid file                         <td>Creating all subsequent grid or mapping files
<tr><td row=2>grid_cice_NEMS_mx025.nc  <td>the CICE grid file                       <td>used at runtime by CICE6
<tr><td row=3>kmtu_cice_NEMS_mx025.nc  <td>the CICE mask file                       <td>used at runtime by CICE6
<tr><td row=4>mesh.mx025.nc            <td>the ocean and ice mesh file              <td>used at runtime by CICE6, MOM6, and CMEPS
<tr><td row=5>C384.mx025.tile[1-6].nc  <td>the mapped ocean mask on the ATM tiles   <td>used to create ATM ICs consistent with the <br>                                                                                           fractional grid
</table>
    
    
<table>
<caption id="multi_row">Optional post-weights files for 1/4deg</caption>
<tr><th>File name                                                                       <th>Function
<tr><td row=1>tripole.mx025.[Cu][Cv][Bu].to.Ct.bilinear.nc                              <td>the ESMF weights for mapping OCN or ICE <br>                                                                                              output fields from the various stagger <br>                                                                                               locations on the tripole grid to the <br>                                                                                                 center (Ct) stagger location on the <br>
                                                                                            tripole grid using bilinear mapping
<tr><td row=2>tripole.mx025.Ct.to.rect.[destination resolution].[bilinear][conserve].nc <td>the ESMF weights for mapping variables on <br>                                                                                        the center (Ct) stagger location on the <br>                                                                                              tripole grid to a rectilinear grid with <br>                                                                                              *destination resolution* using either <br>                                                                                                bilinear or conservative mapping
</table>
    
<table>
<caption id="multi_row">Output files for CICE6 IC creation at tripole destination resolution</caption>
<tr><th>File name                                                               <th>Function
<tr><td row=1>tripole.mx025.Ct.to.mx[destination resolution].Ct.neareststod.nc  <td>the ESMF weights for mapping the 1/4 CICE ICs to <br>
                                                                                    a tripole *destination resolution* using nearest <br>                                                                                     source-to-destination mapping
</table>
    
<table>
<caption id="multi_row">Output files for run-time modification of MOM6 land mask</caption>
<tr><th>File name                       <th>Function
<tr><td row=1>ufs.[Default filename].nc <td>Topo-edits required for UFS application. These are appended to the existing default topo <br>                                             edits file and implemented at run time with the parameter flag <br>                                                                       ``ALLOW_LANDMASK_CHANGES=true``. All files produced by the *run_gridgen.sh* will be <br>                                                  consistent with this run-time land mask.
</table>
