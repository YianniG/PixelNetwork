# PixelNetwork
Exploratory work with Pixel Networks in Haskell.

Linking pixels to each other, creating a network where each pixel knows what pixels it is connected to.
Using Haskell's lazy eval, I aim to create a network where each pixel is initialised in terms of another (and therefore itself),
  this creates an infinite data structure where the entire network is inherently encoded in each individual pixel.
