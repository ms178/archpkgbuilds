#!/bin/bash
# SAT.1 via FritzBox 6660 SAT>IP (DVB-C)
# Freq 410 MHz (from FritzBox M3U)
# PIDs: 7140=PMT, 7142=Video, 7143/7144=Audio, 7148/7149=Subs/Teletext

mpv --profile=dvbc \
  --deinterlace \
  "rtsp://192.168.178.1:554/?avm=1&freq=410&bw=8&msys=dvbc&mtype=256qam&sr=6900&specinv=1&pids=0,16,17,18,20,7140,7142,7143,7144,7148,7149"
