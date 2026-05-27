#!/bin/bash
# Phoenix HD via FritzBox 6660 SAT>IP (DVB-C)
# Freq 474 MHz — PIDs filtered to Phoenix HD only
#   106 = PMT
#   581 = Video (H.264)
#   582 = Audio 1 (German, AC3/AAC)
#   583 = Audio 2 (German AD)
#   584 = Audio 3
#   586 = Subtitles (DVB)
#   588 = Teletext

mpv --profile=dvbc \
  --deinterlace \
  "rtsp://192.168.178.1:554/?avm=1&freq=474&bw=8&msys=dvbc&mtype=256qam&sr=6900&specinv=1&pids=0,106,581,582,583,584,586,588"
