#!/bin/bash
# Das Erste HD via FritzBox 6660 SAT>IP (DVB-C)
# Freq 386 MHz (from FritzBox M3U)
# Full PID list from FritzBox; flatten-editions handles multi-program TS

mpv --profile=dvbc \
  --deinterlace \
  "rtsp://192.168.178.1:554/?avm=1&freq=386&bw=8&msys=dvbc&mtype=256qam&sr=6900&specinv=1&pids=0,16,17,18,20,5100,5101,5102,5103,5104,5105,5107"
