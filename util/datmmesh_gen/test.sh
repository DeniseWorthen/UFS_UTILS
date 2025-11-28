#!/bin/bash
set -eux

APRUN=${APRUN:-"srun"}

# Parse command-line arguments
if [ $# -lt 1 ] || [ $# -gt 3 ]; then
    echo "Usage: $0 <ATMRES> [OCNRES] [WAVRES]" >&2
    exit 1
fi

ATMRES=$1
OCNRES=${2:-}
WAVRES=${3:-}

#orog_ver
#ice_ver
#wav_ver
#datm_ver

if [ $machine = "ursa" ]; then
    FIX_DIR="/scratch3/NCEPDEV/global/role.glopara/fix"
elif [ $machine = "jet" ]; then
    FIX_DIR="/lfs5/HFIP/hfv3gfs/glopara/FIX/fix"
elif [ $machine = "orion" -o $machine = "hercules" ]; then
    FIX_DIR="/work2/noaa/global/role-global/fix"
elif [ $machine = "wcoss2" ]; then
    FIX_DIR="/lfs/h2/emc/global/noscrub/emc.global/FIX/fix"
elif [ $machine = "gaeac6" ]; then
    FIX_DIR="/gpfs/f6/drsa-precip3/world-shared/role.glopara/fix"
fi


#URSA
fv3dir=/scratch3/NCEPDEV/global/role.glopara/fix/orog/20240917
datmdir=/scratch4/NAGAPE/epic/role-epic/UFS-WM_RT/NEMSfv3gfs/input-data-20251015/DATM_CDEPS
icedir=/scratch4/NCEPDEV/stmp/Denise.Worthen/CPLD_GRIDGEN/BASELINE
#icedir=/scratch3/NCEPDEV/global/role.glopara/fix/cice/20240416
wavdir=/scratch3/NCEPDEV/global/role.glopara/fix/wave/20250508

#GAEA
#wavdir=/gpfs/f6/drsa-precip3/world-shared/role.glopara/fix/wave/20250508
#wavdir=/gpfs/f6/bil-fire8/world-shared/role.epic/UFS-WM_RT/NEMSfv3gfs/input-data-20250507/WW3_input_data_20250807
#fv3dir=/gpfs/f6/drsa-precip3/world-shared/role.glopara/fix/orog/20240917
#icedir=/gpfs/f6/drsa-precip3/world-shared/role.glopara/fix/cice/20240416

# Set ATM mesh based on ATMRES
if [[ $ATMRES == C* ]]; then
    # FV3 cube-sphere grid
    fmosaic="${fv3dir}/${ATMRES}/${ATMRES}_mosaic.nc"
    ftilepath="${fv3dir}/${ATMRES}"
    fatmmesh=""
else
    # DATM unstructured mesh
    fatmmesh="${datmdir}/mesh.datm.${ATMRES}.nc"
    fmosaic=""
    ftilepath=""
fi

# Set ocean mesh if OCNRES is provided
if [ -n "${OCNRES}" ]; then
    focnmesh=$icedir/${OCNRES}/'mesh.mx'${OCNRES}'.nc'
fi

# Set wave mesh if WAVRES is provided
if [ -n "${WAVRES}" ]; then
    fwavmesh="${wavdir}/mesh.${WAVRES}.nc"
fi

# Set srcopt based on ATM mesh type
if [ -n "${fmosaic}" ] && [ -n "${ftilepath}" ]; then
    srcopt="-s ${fmosaic} --tilefile_path ${ftilepath}"
elif [ -n "${fatmmesh}" ]; then
    srcopt="-s ${fatmmesh}"
else
    echo "Error: no ATM grid specified (set fmosaic+ftilepath or fatmmesh)" >&2
    exit 1
fi

defaultopts=' --src_loc center --dst_loc center --weight_only --no_log'
#defaultopts=' --src_loc center --dst_loc center --checkFlag '

for exp in a2o_bilin a2o_consf a2o_patch a2w_bilin w2o o2w; do
    # Skip ocean-related mappings if OCNRES not provided
    if [ -z "${OCNRES}" ] && [[ $exp == *o* ]]; then
        continue
    fi

    # Skip wave-related mappings if WAVRES not provided
    if [ -z "${WAVRES}" ] && [[ $exp == *w* ]]; then
        continue
    fi

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
            opts="${srcopt} -d ${focnmesh} -w ${ftag} ${mapping}"
            ;;
        a2o_consf)
            mapindex=consf
            ftag='map.'${ATMRES}'.to.mx'${OCNRES}'.'$mapindex'.nc'
            mapping='-m conserve --norm_type fracarea '
            opts="${srcopt} -d ${focnmesh} -w ${ftag} ${mapping}"
            ;;
        a2o_patch)
            mapindex='patch'
            ftag='map.'${ATMRES}'.to.mx'${OCNRES}'.'$mapindex'.nc'
            mapping='-m patch -p all '
	    opts="${srcopt} -d ${focnmesh} -w ${ftag} ${mapping}"
            ;;
        a2w_bilin)
            mapindex=bilnr
            ftag='map.'${ATMRES}'.to.'${WAVRES}'.'$mapindex'.nc'
            mapping='-m bilinear -p none '
	    opts="${srcopt} -d ${fwavmesh} -w ${ftag} ${mapping}"
	    ;;
    esac

    ${APRUN} ESMF_RegridWeightGen "${opts}" "${defaultopts}"

done
