#!/bin/bash
mpv "rtsp://192.168.178.1:554/?avm=1&freq=474&bw=8&msys=dvbc&mtype=256qam&sr=6900&specinv=1&pids=0,16,17,18,20,106,581,582,583,584,586,588,2171" --rtsp-transport=udp --deinterlace
