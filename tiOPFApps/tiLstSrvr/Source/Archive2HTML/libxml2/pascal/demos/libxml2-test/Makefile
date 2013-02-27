PROJECT=TestParseFile
DCC=dcc
EXECUTABLE=$(PROJECT)
DCCOPTS=-Q -U../../src

all: test

./$(EXECUTABLE): $(PROJECT).dpr
	$(DCC) $(DCCOPTS) $(PROJECT).dpr

clean:
	rm -f *.dcu

test: ./$(EXECUTABLE)
	./$(EXECUTABLE) test.xml
