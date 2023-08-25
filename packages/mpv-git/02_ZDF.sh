#!/bin/bash
mpv "rtsp://192.168.178.1:554/?avm=1&freq=450&bw=8&msys=dvbc&mtype=256qam&sr=6900&specinv=1&pids=0,16,17,18,20,6100,6110,6120,6121,6123,6130,6131,6170" --rtsp-transport=udp --deinterlace
