module Sample where


class BinaryLife a where
    binaryAlive :: a
    binaryDead :: a
    
data Sample a = Sample [[a]]

fromSample (Sample a) = a
