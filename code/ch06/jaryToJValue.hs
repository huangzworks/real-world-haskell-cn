-- file: ch06/jaryToJValue.hs
jaryToJValue = JArray . JAry . map toJValue . fromJAry