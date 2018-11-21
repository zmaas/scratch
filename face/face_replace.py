#!/usr/bin/python
# face_replace.py
# Usage: python face_replace.py <image_file> [face index]

import sys
import cv2
from PIL import Image
from PIL import ImageOps
import random

IMAGEPATH = sys.argv[1]
CASCPATH = "haarcascade_frontalface_default.xml"


def replaceFaces(img=IMAGEPATH, casc=CASCPATH):
    """Replace all identified faces with the ones from a picture."""
    # Create the haar cascade
    face_cascade = cv2.CascadeClassifier(casc)
    print("Imported Cascade.")

    # Read the image
    image = cv2.imread(img)
    print("Imported Image: ", img)
    print(image.shape)

    gray = cv2.cvtColor(image, cv2.COLOR_BGR2GRAY)
    print("Converted to Greyscale")

    # Detect faces in the image
    faces = face_cascade.detectMultiScale(
        image, scaleFactor=1.1, minNeighbors=5, minSize=(30, 30))

    print("Found {0} faces!".format(len(faces)))

    # Draw a rectangle around the faces
    for (x, y, w, h) in faces:
        cv2.rectangle(image, (x, y), (x + w, y + h), (0, 255, 0), 2)

    im = Image.open(IMAGEPATH)
    im2 = Image.open("head.png")

    for (x, y, w, h) in faces:
        newim = im2.resize((int(1.7 * w), int(1.7 * h)))
        if random.randint(0, 1):
            newim = ImageOps.mirror(newim)
        if random.randint(0, 1):
            newim = newim.rotate(random.randint(-45, 45))
        im.paste(newim, (x - w // 2, y - h // 2), newim)

    im.save("testout.jpg")


def main():
    """Main function"""
    replaceFaces()


if __name__ == "__main__":
    main()
