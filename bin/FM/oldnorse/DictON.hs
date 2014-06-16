{-
    Functional Morphology: Latin internal dictionary
    Copyright (C) 2004  Author: Markus Forsberg

    This program is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 2 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program; if not, write to the Free Software
    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
-}

module DictON where

import BuildON
import Dictionary
import TypesON

oldnorseDict :: Dictionary
oldnorseDict = dictionary $ nouns ++ verbs
	
nouns = [
    d1heimr "heimr",
	d1songr "söngr",
	d1kottr "köttr",
	d1barn "barn",
	d1nidr "niðr",
	d2sjar "sjár",
	d2sker "sker",
	d1smjor "smjör",
	d1kvadi "kvæði",
	d1hirdir "hirðir",
	d1nal "nál",
	d1stod "stöð",
	d1ben "ben",
	d1heidr "heiðr",
	d1gygr "gýgr",
	d1stadr "staðr",
	d1gestr "gestr",
	d1naud "nauð",
    d2holl "höll",
    d1ylgr "ylgr",
    d1fingr "fingr",
    d2vetr "vetr",
    d1fotr "fótr",
    d1bok "bók",
    d2vik "vík",
    d2flo "fló",
    d1kyr "kýr",
    d1fadir "faðir",
    d2dottir "dóttir",
    d1gefandi "gefandi",
    d1timi "tími",
    d2auga "auga",
    d1bryti "bryti",
    d1vokvi "vökvi",
    d1tunga "tunga",
    d1brynja "brynja",
    d2kirkja "kirkja",
    d1volva "völva",
    d1elli "elli",
    d2iski "iski",
    d1gorsimi "görsimi"
    ] 	

verbs = [
    d1elska "elska",
    d1stodva "stöðva",
    d1stodva "herja",
    d1spa "spá",
    d1krefja "krefja",
    d1lyja "lýja",
    d1bita "bíta"
    ]