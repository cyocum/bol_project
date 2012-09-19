all:
	g++ -Wall -o skmeans -I /usr/local/include -I /usr/include/libxml2 main.cpp -lmlpack -larmadillo -lxml2
	ocamlfind opt -package pcre,camomile -linkpkg -o combine combine.ml

clean:
	rm *.o *.cm* *.o *.csv *.txt combine skmeans
