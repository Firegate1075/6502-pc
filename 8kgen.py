file = open("./output", "wb")

data = [0] * 8192

for i in range(8192):
	data[i] = (255-i )% 256

dataByteArray = bytearray(data)

file.write(dataByteArray)
