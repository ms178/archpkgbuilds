#!/bin/bash
mpv "rtsp://192.168.178.1:554/?avm=1&freq=466&bw=8&msys=dvbc&mtype=256qam&sr=6900&specinv=1&pids=0,16,17,18,20,7620,7622,7623,7624,7628,7629" --rtsp-transport=udp --deinterlace
