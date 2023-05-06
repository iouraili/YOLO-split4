import argparse
import os
import cv2
from pathlib import Path

parser = argparse.ArgumentParser()
parser.add_argument('--image', required=True, help='Path to the image')
parser.add_argument('--outdir', required=True, help='Path to output directory')
args = vars(parser.parse_args())

if (os.path.exists(args["outdir"])):
    print("Directory already exists. Placing tiles there.")
else:
    print("Directory does not exist; creating output directory.")
    os.makedirs(args["outdir"])

file_name = Path(args["image"]).stem

image = cv2.imread(args["image"])
(h, w) = image.shape[:2]
#cv2.imshow('Original', image)

(cX, cY) = (w // 2, h // 2)

topLeft = image[0:cY, 0:cX]
topRight = image[0:cY, cX:w]
bottomLeft = image[cY:h, 0:cX]
bottomRight = image[cY:h, cX:w]

cv2.imwrite(args["outdir"] + "/" + file_name + "a.tif", topLeft)
cv2.imwrite(args["outdir"] + "/" + file_name + "b.tif", topRight)
cv2.imwrite(args["outdir"] + "/" + file_name + "c.tif", bottomLeft)
cv2.imwrite(args["outdir"] + "/" + file_name + "d.tif", bottomRight)
