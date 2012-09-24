all:
	ocamlfind opt -package pcre,camomile -nodynlink -linkpkg -o vectorize util.ml vectorize.ml

profile: 
	ocamlfind opt -p -package pcre,camomile -nodynlink -linkpkg -o vectorize util.ml vectorize.ml	

clean:
	rm *.cm* *.o util.cm* vectorize.cm* *.csv *.txt *.mat vectorize *~