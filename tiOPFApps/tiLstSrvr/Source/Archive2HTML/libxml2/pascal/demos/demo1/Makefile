DCC=dcc
DCCOPTS=-Q -U../../src

all: test

./Demo1: Demo1.dpr
	$(DCC) $(DCCOPTS) Demo1.dpr

./transform: transform.dpr
	$(DCC) $(DCCOPTS) transform.dpr

clean:
	rm -f *.dcu 17-3.html

distclean: clean
	rm -f *.exe

testDemo1: ./Demo1 ./transform
	./Demo1 calServer.xml '//*'

testtransform: ./transform
	./transform 17-1.xml 17-2.xsl 17-3.html

test: testDemo1 testtransform
