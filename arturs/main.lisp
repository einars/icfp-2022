(ql:quickload "png-read")

(png-read:image-data
 (png-read:read-png-file "../problems/01.png"))

