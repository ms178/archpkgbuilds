#!/bin/bash
mpv "rtsp://192.168.178.1:554/?avm=1&freq=330&bw=8&msys=dvbc&mtype=256qam&sr=6900&specinv=1&pids=0,16,17,18,20,5100,5101,1170,1176,2171,2172,5102,5103,5104,5105,5107" --rtsp-transport=udp --deinterlace
