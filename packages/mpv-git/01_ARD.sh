#!/bin/bash
# ARD / Das Erste HD via FritzBox 6660 SAT>IP (DVB-C)
# Freq 330 MHz — PIDs filtered to Das Erste HD only
#   5100 = PMT
#   5101 = Video (H.264)
#   5102 = Audio 1 (German, AC3/AAC)
#   5103 = Audio 2 (German AD)
#   5104 = Audio 3
#   5105 = Subtitles (DVB)
#   5107 = Teletext

mpv --profile=dvbc \
  --deinterlace \
  "rtsp://192.168.178.1:554/?avm=1&freq=330&bw=8&msys=dvbc&mtype=256qam&sr=6900&specinv=1&pids=0,5100,5101,5102,5103,5104,5105,5107"
