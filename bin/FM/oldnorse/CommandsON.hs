{-
    Functional Morphology: Latin command definitions
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

module CommandsON where

import BuildON
import Frontend

commands = 
 [
  ("d1heimr", ["heimr"], app1 d1heimr),
  ("d1songr", ["söngr"], app1 d1songr),
  ("d1kottr", ["köttr"], app1 d1kottr),
  ("d1barn", ["barn"], app1 d1barn),
  ("d2sker", ["sker"], app1 d2sker),
  ("d1nidr", ["niðr"], app1 d1nidr),
  ("d2sjar", ["sjár"], app1 d2sjar),  
  ("d1smjor", ["smjör"], app1 d1smjor),
  ("d1kvadi", ["kvæði"], app1 d1kvadi),
  ("d1nal", ["nál"], app1 d1nal),
  ("d1stod", ["stöð"], app1 d1stod),
  ("d1ben", ["ben"], app1 d1ben),
  ("d1heidr", ["heiðr"], app1 d1heidr),
  ("d1gygr", ["gýgr"], app1 d1gygr),
  ("d1stadr", ["staðr"], app1 d1stadr),
  ("d1gestr", ["gestr"], app1 d1gestr),  
  ("d1naud", ["nauð"], app1 d1naud), 
  ("d2holl", ["höll"], app1 d2holl),
  ("d1ylgr", ["ylgr"], app1 d1ylgr),
  ("d1fingr", ["fingr"], app1 d1fingr),
  ("d2vetr", ["vetr"], app1 d2vetr),
  ("d1fotr", ["fótr"], app1 d1fotr),
  ("d1bok", ["bók"], app1 d1bok),
  ("d2vik", ["vík"], app1 d2vik),
  ("d2flo", ["fló"], app1 d2flo),
  ("d1kyr", ["kýr"], app1 d1kyr),
  ("d1fadir", ["faðir"], app1 d1fadir),
  ("d2dottir", ["dóttir"], app1 d2dottir),
  ("d1gefandi", ["gefandi"], app1 d1gefandi),
  ("d1timi", ["tími"], app1 d1timi),
  ("d2auga", ["auga"], app1 d2auga),
  ("d1bryti", ["bryti"], app1 d1bryti),
  ("d1vokvi", ["vökvi"], app1 d1vokvi),
  ("d1tunga", ["tunga"], app1 d1tunga),
  ("d1brynja", ["brynja"], app1 d1brynja),
  ("d2kirkja", ["kirkja"], app1 d2kirkja),
  ("d1volva", ["völva"], app1 d1volva),
  ("d1elli", ["elli"], app1 d1elli),
  ("d2iski", ["iski"], app1 d2iski),
  ("d1gorsimi", ["görsimi"], app1 d1gorsimi),
  ("d1elska", ["elska"], app1 d1elska),
  ("d1stodva", ["stöðva"], app1 d1stodva),
  ("d1spa", ["spá"], app1 d1spa),
  ("d1krefja", ["krefja"], app1 d1krefja),
  ("d1lyja", ["lýja"], app1 d1lyja),
  ("d1bita", ["bíta"], app1 d1bita)
 ]
 

