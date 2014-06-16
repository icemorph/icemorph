{-
    Functional Morphology: Latin type system
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


module TypesON where

import General
import Invariant

-- Parameters for Old Norse morphology

data Case = Nominative | Accusative | Genitive | Dative
 deriving (Show,Eq,Enum,Ord,Bounded)
 
data Number = Singular | Plural
 deriving (Show,Eq,Enum,Ord,Bounded)

data Gender = Feminine | Masculine | Neuter
 deriving (Show,Eq,Enum,Ord,Bounded) 
 
data Person = P1 | P2 | P3   
  deriving (Show,Eq,Enum,Ord,Bounded) 

--data Mood = Indicative | Optative
--  deriving (Show,Eq,Enum,Ord,Bounded)  
 
data Voice = Active | Middle
  deriving (Show,Eq,Enum,Ord,Bounded) 
  
data Tense = Present | Past
  deriving (Show,Eq,Enum,Ord,Bounded) 
  

instance Param Case where values = enum 
instance Param Number where values = enum 
instance Param Gender where values = enum       
instance Param Person where values = enum          
instance Param Voice where values = enum
instance Param Tense where values = enum

 
-- Old Norse nouns


data NounForm = NounForm Number Case deriving (Show,Eq,Ord)

instance Param NounForm where
    values = [NounForm n c | n <- values , c <- values]
    prValue (NounForm n c) = unwords $ [prValue n, prValue c]

type Noun = NounForm -> Str


-- Old Norse verbs 
  
   
data VerbForm =   
    Infinitive Voice |
    PresentParticiple Voice |
    PastParticiple Voice |
    Impera2sg Voice |
    Impera1pl Voice |
    Impera2pl Voice |
    Indicative Voice Number Person Tense |
    Optative Voice Number Person Tense
  deriving (Show,Eq,Ord)   
   
instance Param VerbForm where
    values = 
      [Infinitive v | v <- values] ++
      [PresentParticiple v | v <- values] ++
      [PastParticiple v | v <- values] ++
      [Impera2sg v | v <- values] ++
      [Impera1pl v | v <- values] ++
      [Impera2pl v | v <- values] ++
      [Indicative v n p t | 
        t <- values,  
        p <- values,
        n <- values, 
        v <- values] ++
      [Optative v n p t | 
        t <- values,  
        p <- values,
        n <- values, 
        v <- values]

type Verb = VerbForm -> Str