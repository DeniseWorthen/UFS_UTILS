#!/bin/bash
set -eux

APRUN=${APRUN:-"srun"}

#ATMRES=C96
#OCNRES=100
#WAVRES=global_270k

ATMRES=C1152
OCNRES=025
WAVRES=uglo_15km

#URSA
#wavdir=/scratch4/NAGAPE/epic/role-epic/UFS-WM_RT/NEMSfv3gfs/input-data-20250507/WW3_input_data_20250807
fv3dir=/scratch3/NCEPDEV/global/role.glopara/fix/orog/20240917
icedir=/scratch3/NCEPDEV/global/role.glopara/fix/cice/20240416
wavdir=/scratch3/NCEPDEV/global/role.glopara/fix/wave/20250508

#GAEA
#wavdir=/gpfs/f6/drsa-precip3/world-shared/role.glopara/fix/wave/20250508
#wavdir=/gpfs/f6/bil-fire8/world-shared/role.epic/UFS-WM_RT/NEMSfv3gfs/input-data-20250507/WW3_input_data_20250807
#fv3dir=/gpfs/f6/drsa-precip3/world-shared/role.glopara/fix/orog/20240917
#icedir=/gpfs/f6/drsa-precip3/world-shared/role.glopara/fix/cice/20240416

focnmesh=$icedir/${OCNRES}/'mesh.mx'${OCNRES}'.nc'
fwavmesh=$wavdir/'mesh.'${WAVRES}'.nc'
fmosaic=$fv3dir/${ATMRES}/${ATMRES}'_mosaic.nc'
ftilepath=$fv3dir/${ATMRES}

defaultopts=' --src_loc center --dst_loc center --weight_only --no_log '
#defaultopts=' --src_loc center --dst_loc center --weight_only '
#defaultopts=' --src_loc center --dst_loc center --no_log --checkFlag '


#for exp in a2o_bilin a2o_patch a2w_bilin; do
#for exp in w2o o2w a2o_bilin; do

for exp in a2o_bilin a2o_patch a2w_bilin w2o o2w a2o_bilin; do
    case $exp in
        w2o)
            mapindex=bilnr_nstod
            ftag='map.'${WAVRES}'.to.mx'${OCNRES}'.'$mapindex'.nc'
            mapping='-m bilinear -p none --extrap_method neareststod '
            opts='-s '${fwavmesh}' -d '${focnmesh}' -w '${ftag}'  '${mapping}
            ;;
        o2w)
            mapindex=bilnr_nstod
            ftag='map.mx'${OCNRES}'.to.'${WAVRES}'.'$mapindex'.nc'
            mapping='-m bilinear -p none --extrap_method neareststod '
            opts='-s '${focnmesh}' -d '${fwavmesh}' -w '${ftag}'  '${mapping}
            ;;
        a2o_bilin)
            mapindex=bilnr
            ftag='map.'${ATMRES}'.to.mx'${OCNRES}'.'$mapindex'.nc'
            mapping='-m bilinear -p all '
            opts='-s '${fmosaic}' --tilefile_path '${ftilepath}' -d '${focnmesh}' -w '${ftag}'  '${mapping}
            ;;
        a2o_patch)
            mapindex=patch_uv3d
            ftag='map.'${ATMRES}'.to.mx'${OCNRES}'.'$mapindex'.nc'
            mapping='-m patch -p all '
	    opts='-s '${fmosaic}' --tilefile_path '${ftilepath}' -d '${focnmesh}' -w '${ftag}'  '${mapping}
            ;;
        a2w_bilin)
            mapindex=bilnr
            ftag='map.'${ATMRES}'.to.'${WAVRES}'.'$mapindex'.nc'
            mapping='-m bilinear -p none '
            opts='-s '${fmosaic}' --tilefile_path '${ftilepath}' -d '${fwavmesh}' -w '${ftag}'  '${mapping}
	    ;;
    esac

    ${APRUN} ESMF_RegridWeightGen ${opts} ${defaultopts}

done

#FDIMS=${NX}x${NY}
#FDST=${OUTPUT_DIR}/datm.${FDIMS}.SCRIP.nc
#if [ $N2S == .true. ]; then
#    ncremap -g ${FDST} -G ttl='DATM grid '${FDIMS}#latlon=${NY},${NX}#lon_typ=grn_ctr#lat_typ=gss#lat_drc=n2s
#else
#    ncremap -g ${FDST} -G ttl='DATM grid '${FDIMS}#latlon=${NY},${NX}#lon_typ=grn_ctr#lat_typ=gss
#fi

#FSRC=${OUTPUT_DIR}/datm.${FDIMS}.SCRIP.nc
#FDST=${OUTPUT_DIR}/mesh.datm.${FDIMS}.nc
#$APRUN -n 1 ESMF_Scrip2Unstruct ${FSRC} ${FDST} 0
