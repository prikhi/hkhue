#!/usr/bin/env bash
#
# Gradually brighten & increase color temperature to ease waking up.
# Starts at pure red & gets to 6500K in ~60 minutes.

echo "[$(date +%T)] Starting wake up sequence."

echo "[$(date +%T)] Turning on red lights at lowest brightness."
hkhue set --on --wait --color 255,0,0 --brightness 1

TEMP_RAMP=(
  # KELVIN  BRIGHTN MINUTES
    2000    10      5
    2500    15      5
    3000    30      7
    3500    60      7
    4250    80      10
    5000    100     10
    6500    100     15
)
RAMP_LEN=${#TEMP_RAMP[@]}

for (( INDEX=0; INDEX < RAMP_LEN; INDEX=INDEX+3 )); do
    COLOR_TEMP="${TEMP_RAMP[${INDEX}]}"
    BRIGHTNESS="${TEMP_RAMP[$((INDEX+1))]}"
    MINUTES="${TEMP_RAMP[$((INDEX+2))]}"
    TRANSITION_TIME="$(( 10 * 60 * MINUTES))"
    hkhue set --wait \
        -k "${COLOR_TEMP}" \
        -b "${BRIGHTNESS}" \
        -t "${TRANSITION_TIME}"
    echo "[$(date +%T)] Reached ${COLOR_TEMP}K and ${BRIGHTNESS}% brightness" \
         "in ${MINUTES} minute(s)."
done

echo "[$(date +%T)] Wake up sequence complete."
