all:
	make on
on: 
	ghc -O2 -i./lib -i./oldnorse --make ./oldnorse/Main.hs -o morpho_on
cleanLex:
	ghc -O2 -i./lib --make tools/Clean.hs -o tools/cleanLex
trie:
	gcc -Wall -O3 ./tools/trie.c -o ./tools/trie
.PHONY : clean
clean:
	rm -f ./lib/*.o ./lib/*.hi *~ ./lib/*~ ./oldnorse/*.o ./oldnorse/*.hi ./oldnorse/*~  
