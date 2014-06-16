{-
    Functional Morphology: Latin Dictionary definitions
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
module BuildON where

import RulesON
import AttrON
import TypesON
import Dictionary
import General

-----------------------------------------------
-- Interface functions for Nouns.
----------------------------------------------

d1heimr :: DictForm -> Entry
d1heimr = masculine . decl1heimr

d1songr :: DictForm -> Entry
d1songr = masculine . decl1songr

d1kottr :: DictForm -> Entry
d1kottr = masculine . decl1kottr

d1barn :: DictForm -> Entry
d1barn = neuter . decl1barn

d2sker :: DictForm -> Entry
d2sker = neuter . decl2sker

d1nidr :: DictForm -> Entry
d1nidr = masculine . decl1nidr

d2sjar :: DictForm -> Entry
d2sjar = masculine . decl2sjar

d1smjor :: DictForm -> Entry
d1smjor = neuter . decl1smjor

d1kvadi :: DictForm -> Entry
d1kvadi = neuter . decl1kvadi

d1hirdir :: DictForm -> Entry
d1hirdir = masculine . decl1hirdir

d1nal :: DictForm -> Entry
d1nal = feminine . decl1nal

d1stod :: DictForm -> Entry
d1stod = feminine . decl1stod

d1ben :: DictForm -> Entry
d1ben = feminine . decl1ben

d1heidr :: DictForm -> Entry
d1heidr = feminine . decl1heidr

d1gygr :: DictForm -> Entry
d1gygr = feminine . decl1gygr

d1stadr :: DictForm -> Entry
d1stadr = masculine . decl1stadr

d1gestr :: DictForm -> Entry
d1gestr = masculine . decl1gestr

d1naud :: DictForm -> Entry
d1naud = feminine . decl1naud

d2holl :: DictForm -> Entry
d2holl = feminine . decl2holl

d1ylgr :: DictForm -> Entry
d1ylgr = feminine. decl1ylgr

d1fingr :: DictForm -> Entry
d1fingr = masculine . decl1fingr

d2vetr :: DictForm -> Entry
d2vetr = masculine . decl2vetr

d1fotr :: DictForm -> Entry
d1fotr = masculine . decl1fotr

d1bok :: DictForm -> Entry
d1bok = feminine . decl1bok

d2vik :: DictForm -> Entry
d2vik = feminine . decl2vik

d2flo :: DictForm -> Entry
d2flo = feminine . decl2flo

d1kyr :: DictForm -> Entry
d1kyr = feminine . decl1kyr

d1fadir :: DictForm -> Entry
d1fadir = masculine . decl1fadir

d2dottir :: DictForm -> Entry
d2dottir = feminine . decl2dottir

d1gefandi :: DictForm -> Entry
d1gefandi = masculine . decl1gefandi

d1timi :: DictForm -> Entry
d1timi = masculine . decl1timi

d2auga :: DictForm -> Entry
d2auga = neuter . decl2auga

d1bryti :: DictForm -> Entry
d1bryti = masculine . decl1bryti

d1vokvi :: DictForm -> Entry
d1vokvi = masculine . decl1vokvi

d1tunga :: DictForm -> Entry
d1tunga = feminine . decl1tunga

d1brynja :: DictForm -> Entry
d1brynja = feminine . decl1brynja

d2kirkja :: DictForm -> Entry
d2kirkja = feminine . decl2kirkja

d1volva :: DictForm -> Entry
d1volva = feminine . decl1volva

d1elli :: DictForm -> Entry
d1elli = feminine . decl1elli

d2iski :: DictForm -> Entry
d2iski = feminine . decl2iski

d1gorsimi :: DictForm -> Entry
d1gorsimi = feminine . decl1gorsimi


d1elska :: DictForm -> Entry
d1elska = entry . decl1elska

d1stodva :: DictForm -> Entry
d1stodva = entry . decl1stodva

d1spa :: DictForm -> Entry
d1spa = entry . decl1spa

d1krefja :: DictForm -> Entry
d1krefja = entry . decl1krefja

d1lyja :: DictForm -> Entry
d1lyja = entry . decl1lyja

d1bita :: DictForm -> Entry
d1bita = entry . decl1bita


noun :: Noun -> Gender -> Entry
noun n g = entryI n [prValue g]

feminine :: Noun -> Entry
feminine n = noun n Feminine

masculine :: Noun -> Entry
masculine n = noun n Masculine

neuter :: Noun -> Entry
neuter n = noun n Neuter

-- Dictionary instances

instance Dict NounForm      where category _ = "Noun"
instance Dict VerbForm      where category _ = "Verb"