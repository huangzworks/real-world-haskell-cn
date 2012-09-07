-- file: ch03/Distinction.hs
a = ("Porpoise", "Grey")
b = ("Table", "Oak")

data Cetacean = Cetacean String String
data Furniture = Furniture String String

c = Cetacean "Porpoise" "Grey"
d = Furniture "Table" "Oak"
