{-
    Functional Morphology: Latin paradigm definitions
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

module RulesON where

import TypesON
import General

{- Interface functions. -}

type DictForm = String
type Stem     = String



-------------------------------------------------------
-- first declension
-------------------------------------------------------


{- noun masc u-stem    -}

decl1kottr :: DictForm -> Noun
decl1kottr köttr (NounForm n c) = 
    mkStr $ 
      case n of
        Singular -> case c of
          Nominative -> köttr
          Genitive -> kattar
          Dative -> ketti
          Accusative -> kött
        Plural -> case c of
          Nominative -> kettir
          Genitive -> katta
          Dative -> köttum
          Accusative -> köttu
  where
    kött = (tk 1 köttr)
    kattar = (revertToStem kött) ++ "ar"
    ketti = i_mutation (revertToStem kött) ++ "i"
    kettir = i_mutation (revertToStem kött) ++ "ir"
    katta = revertToStem kött ++ "a"
    köttum = u_mutation (revertToStem kött) ++ "um"
    köttu = u_mutation (revertToStem kött) ++ "u"
    
    
{- noun masc a-stem    -}
    
decl1heimr :: DictForm -> Noun
decl1heimr heimr (NounForm n c) =
    mkStr $
      case n of
        Singular -> case c of
          Nominative -> prefix ++ lexeme
          Accusative -> heim
          Genitive -> heims
          Dative -> heimi
        Plural -> case c of
          Nominative -> heimar
          Accusative -> heima
          Genitive -> heima
          Dative -> heimum
  where
    (prefix, lexeme) = splitCompound heimr
    root = (tk 1 lexeme)
    heim = prefix ++ root
    heims = prefix ++ root ++ "s"
    heimi = prefix ++ (syncope root ++ "i")
    heimar = prefix ++ (syncope root ++ "ar")
    heima = prefix ++ (syncope root ++ "a")
    heimum = prefix ++ (u_mutation (syncope root) ++ "um")    
    
    
{- noun masc ja-stem -}
    
decl1nidr :: DictForm -> Noun
decl1nidr niðr (NounForm n c) =
    mkStr $
      case n of
        Singular -> case c of
          Nominative -> prefix ++ lexeme
          Accusative -> nið
          Genitive -> niðs
          Dative -> nið
        Plural -> case c of
          Nominative -> niðjar
          Accusative -> niðja
          Genitive -> niðja
          Dative -> niðjum
  where
    (prefix, lexeme) = splitCompound niðr
    root = (tk 1 lexeme)
    nið = prefix ++ root
    niðs = prefix ++ root ++ "s"
    niðjar = prefix ++ (syncope root ++ "jar")
    niðja = prefix ++ (syncope root ++ "ja")
    niðjum = prefix ++ (u_mutation (syncope root) ++ "jum")        
    

{- noun masc ia-stem -}

decl1hirdir :: DictForm -> Noun
decl1hirdir hirðir (NounForm n c) =
    mkStr $
      case n of
        Singular -> case c of
          Nominative -> prefix ++ lexeme
          Accusative -> hirði
          Genitive -> hirðis
          Dative -> hirði
        Plural -> case c of
          Nominative -> hirðar
          Accusative -> hirða
          Genitive -> hirða
          Dative -> hirðum
  where
    (prefix, lexeme) = splitCompound hirðir
    root = (tk 2 lexeme)
    hirði = prefix ++ root ++ "i"
    hirðis = prefix ++ root ++ "is"
    hirðar = prefix ++ (syncope root ++ "ar")
    hirða = prefix ++ (syncope root ++ "a")
    hirðum = prefix ++ (u_mutation (syncope root) ++ "um")    

    
{- noun masc wa-stem -}
    
decl1songr :: DictForm -> Noun
decl1songr söngr (NounForm n c) =
    mkStr $
      case n of
        Singular -> case c of
          Nominative -> prefix ++ lexeme
          Accusative -> söng
          Genitive -> söngs
          Dative -> söngvi
        Plural -> case c of
          Nominative -> söngvar
          Accusative -> söngva
          Genitive -> söngva
          Dative -> söngvum
  where
    (prefix, lexeme) = splitCompound söngr
    root = (tk 1 lexeme)
    söng = prefix ++ root
    söngs = prefix ++ root ++ "s"
    söngvi = prefix ++ (syncope root ++ "vi")
    söngvar = prefix ++ (syncope root ++ "var")
    söngva = prefix ++ (syncope root ++ "va")
    söngvum = prefix ++ (u_mutation (syncope root) ++ "vum")    

decl2sjar :: String -> Noun
decl2sjar sjár nf =  
    except (decl1songr sjár) 
           [(NounForm Singular Genitive, sjávar)] nf
      where 
        (prefix, lexeme) = splitCompound sjár
        root = (tk 1 lexeme)      
        sjávar = prefix ++ (syncope root ++ "var")

        
{- noun masc i-stem -}        
        
decl1stadr :: DictForm -> Noun
decl1stadr staðr (NounForm n c) =
    mkStr $
      case n of
        Singular -> case c of
          Nominative -> prefix ++ lexeme
          Accusative -> stað
          Genitive -> staðar
          Dative -> stað
        Plural -> case c of
          Nominative -> staðir
          Accusative -> staði
          Genitive -> staða
          Dative -> staðum
  where
    (prefix, lexeme) = splitCompound staðr
    root = (tk 1 lexeme)
    stað = prefix ++ root
    staðar = prefix ++ root ++ "ar"
    staðir = prefix ++ (syncope root ++ "ir")
    staði = prefix ++ (syncope root ++ "i")
    staða = prefix ++ (syncope root ++ "a")
    staðum = prefix ++ (u_mutation (syncope root) ++ "um")    
    
decl1gestr :: DictForm -> Noun
decl1gestr gestr (NounForm n c) =
    mkStr $
      case n of
        Singular -> case c of
          Nominative -> prefix ++ lexeme
          Accusative -> gest
          Genitive -> gests
          Dative -> gest
        Plural -> case c of
          Nominative -> gestir
          Accusative -> gesti
          Genitive -> gesta
          Dative -> gestum
  where
    (prefix, lexeme) = splitCompound gestr
    root = (tk 1 lexeme)
    gest = prefix ++ root
    gests = prefix ++ root ++ "s"
    gestir = prefix ++ (syncope root ++ "ir")
    gesti = prefix ++ (syncope root ++ "i")
    gesta = prefix ++ syncope root ++ velarJ root ++ "a"
    gestum = prefix ++ u_mutation (syncope root) ++ velarJ root ++ "um"   
 
 
{- noun masc c-stem -}
 
decl1fingr :: DictForm -> Noun
decl1fingr fingr (NounForm n c) =
    mkStr $
      case n of
        Singular -> case c of
          Nominative -> prefix ++ lexeme
          Accusative -> prefix ++ lexeme
          Genitive -> fingrs
          Dative -> fingri
        Plural -> case c of
          Nominative -> prefix ++ lexeme
          Accusative -> prefix ++ lexeme
          Genitive -> fingra
          Dative -> fingrum
  where
    (prefix, lexeme) = splitCompound fingr
    fingrs = prefix ++ lexeme ++ "s"
    fingri = prefix ++ syncope lexeme ++ "i"
    fingra = prefix ++ syncope lexeme ++ "a"
    fingrum = prefix ++ u_mutation (syncope lexeme) ++ "um"
    
decl2vetr :: String -> Noun
decl2vetr vetr nf =  
    except (decl1fingr vetr) 
           [(NounForm Singular Genitive, vetrar)] nf
      where 
        (prefix, lexeme) = splitCompound vetr      
        vetrar = prefix ++ syncope lexeme ++ "ar"

decl1fotr :: DictForm -> Noun
decl1fotr fótr (NounForm n c) =
    mkStr $
      case n of
        Singular -> case c of
          Nominative -> prefix ++ lexeme
          Accusative -> prefix ++ root
          Genitive -> fótar
          Dative -> fóti
        Plural -> case c of
          Nominative -> prefix ++ lexeme
          Accusative -> prefix ++ lexeme
          Genitive -> fóta
          Dative -> fótum
  where
    (prefix, lexeme) = splitCompound fótr
    root = tk 1 lexeme
    fótar = prefix ++ syncope root ++ "ar"
    fóti = prefix ++ syncope root ++ "i"
    fóta = prefix ++ syncope root ++ "a"
    fótum = prefix ++ u_mutation (syncope root) ++ "um"
    
    
{- noun masc r-stem -}

decl1fadir :: DictForm -> Noun
decl1fadir faðir (NounForm n c) =
    mkStr $
      case n of
        Singular -> case c of
          Nominative -> prefix ++ lexeme
          Accusative -> faður
          Genitive -> faður
          Dative -> faður
        Plural -> case c of
          Nominative -> feðr
          Accusative -> feðr
          Genitive -> feðra
          Dative -> feðrum
  where
    (prefix, lexeme) = splitCompound faðir
    root = tk 2 lexeme
    faður = prefix ++ syncope root ++ "ur"
    feðr = prefix ++ i_mutation (syncope root) ++ "r"
    feðra = prefix ++ i_mutation (syncope root) ++ "ra"
    feðrum = prefix ++ i_mutation (syncope root) ++ "rum"
   

{- noun masc nd-stem -}

decl1gefandi :: DictForm -> Noun
decl1gefandi gefandi (NounForm n c) =
    mkStr $
      case n of
        Singular -> case c of
          Nominative -> prefix ++ lexeme
          Accusative -> gefanda
          Genitive -> gefanda
          Dative -> gefanda
        Plural -> case c of
          Nominative -> gefendr
          Accusative -> gefendr
          Genitive -> gefanda
          Dative -> geföndum
  where
    (prefix, lexeme) = splitCompound gefandi
    root = tk 1 lexeme
    gefanda = prefix ++ syncope root ++ "a"
    gefendr = prefix ++ i_mutation (syncope root) ++ "r"
    geföndum = prefix ++ u_mutation (syncope root) ++ "um"


{- noun masc an-stem -}

decl1timi :: DictForm -> Noun
decl1timi tími (NounForm n c) =
    mkStr $
      case n of
        Singular -> case c of
          Nominative -> prefix ++ lexeme
          Accusative -> tíma
          Genitive -> tíma
          Dative -> tíma
        Plural -> case c of
          Nominative -> tímar
          Accusative -> tíma
          Genitive -> tíma
          Dative -> tímum
  where
    (prefix, lexeme) = splitCompound tími
    root = tk 1 lexeme
    tíma = prefix ++ syncope root ++ "a"
    tímar = prefix ++ syncope root ++ "ar"
    tímum = prefix ++ u_mutation (syncope root) ++ "um"
   

{- noun masc jan-stem -}

decl1bryti :: DictForm -> Noun
decl1bryti bryti (NounForm n c) =
    mkStr $
      case n of
        Singular -> case c of
          Nominative -> prefix ++ lexeme
          Accusative -> brytja
          Genitive -> brytja
          Dative -> brytja
        Plural -> case c of
          Nominative -> brytjar
          Accusative -> brytja
          Genitive -> brytja
          Dative -> brytjum
  where
    (prefix, lexeme) = splitCompound bryti
    root = tk 1 lexeme
    brytja = prefix ++ syncope root ++ "ja"
    brytjar = prefix ++ syncope root ++ "jar"
    brytjum = prefix ++ u_mutation (syncope root) ++ "jum"


{- noun masc wan-stem -}

decl1vokvi :: DictForm -> Noun
decl1vokvi vökvi (NounForm n c) =
    mkStr $
      case n of
        Singular -> case c of
          Nominative -> prefix ++ lexeme
          Accusative -> vökva
          Genitive -> vökva
          Dative -> vökva
        Plural -> case c of
          Nominative -> vökvar
          Accusative -> vökva
          Genitive -> vökva
          Dative -> vökum
  where
    (prefix, lexeme) = splitCompound vökvi
    root = tk 2 lexeme
    vökva = prefix ++ syncope root ++ "va"
    vökvar = prefix ++ syncope root ++ "var"
    vökum = prefix ++ u_mutation (syncope root) ++ "um"   
   
   
   

{- noun neut a-stem    -}

decl1barn :: DictForm -> Noun
decl1barn barn (NounForm n c) =
    mkStr $
      case n of
        Singular -> case c of
          Nominative -> prefix ++ lexeme
          Accusative -> prefix ++ lexeme
          Genitive -> barns
          Dative -> barni
        Plural -> case c of
          Nominative -> barn
          Accusative -> barn
          Genitive -> barna
          Dative -> börnum
  where
    (prefix, lexeme) = splitCompound barn
    root = lexeme
    barns = prefix ++ root ++ "s"
    barni = prefix ++ (syncope root ++ "i")
    barna = prefix ++ (syncope root ++ "a")
    börnum = prefix ++ (u_mutation (syncope root) ++ "um")        
    

{- noun neut ja-stem -}

decl2sker :: String -> Noun
decl2sker sker nf =  
    except (decl1barn sker) 
           [(NounForm Plural Genitive, skerja),
            (NounForm Plural Dative, skerjum)] nf
      where 
        (prefix, lexeme) = splitCompound sker
        root = lexeme      
        skerja = prefix ++ (syncope root ++ "ja")    
        skerjum = prefix ++ (u_mutation (syncope root) ++ "jum")    


{- noun neut ia-stem -}

decl1kvadi :: DictForm -> Noun
decl1kvadi kvæði (NounForm n c) =
    mkStr $
      case n of
        Singular -> case c of
          Nominative -> prefix ++ lexeme
          Accusative -> prefix ++ lexeme
          Genitive -> prefix ++ lexeme ++ "s"
          Dative -> prefix ++ lexeme
        Plural -> case c of
          Nominative -> prefix ++ lexeme
          Accusative -> prefix ++ lexeme
          Genitive -> kvæða
          Dative -> kvæðum
  where
    (prefix, lexeme) = splitCompound kvæði
    root = (tk 1 lexeme)
    kvæða = prefix ++ syncope root ++ velarJ root ++ "a"
    kvæðum = prefix ++ u_mutation (syncope root) ++ velarJ root ++ "um"
    
    
{- noun neut wa-stem -}

decl1smjor smjör (NounForm n c) =
    mkStr $
      case n of
        Singular -> case c of
          Nominative -> prefix ++ lexeme
          Accusative -> prefix ++ lexeme
          Genitive -> smjörs
          Dative -> smjörvi
        Plural -> case c of
          Nominative -> smjör
          Accusative -> smjör
          Genitive -> smjörva
          Dative -> smjörum
  where
    (prefix, lexeme) = splitCompound smjör
    root = lexeme
    smjörs = prefix ++ root ++ "s"
    smjörvi = prefix ++ (syncope root ++ "vi")
    smjörva = prefix ++ (syncope root ++ "va")
    smjörum = prefix ++ (u_mutation (syncope root) ++ "um")    


{- noun neut an-stem -}

decl2auga :: String -> Noun
decl2auga auga nf =  
    except (decl1timi auga) 
           [(NounForm Plural Genitive, augna)] nf
      where 
        (prefix, lexeme) = splitCompound auga
        root = tk 1 lexeme      
        augna = prefix ++ syncope root ++ "na"    
   
    
{- noun fem o-stem -}

decl1nal :: DictForm -> Noun
decl1nal nál (NounForm n c) =
    mkStr $
      case n of
        Singular -> case c of
          Nominative -> prefix ++ lexeme
          Accusative -> prefix ++ lexeme
          Genitive -> nálar
          Dative -> prefix ++ lexeme
        Plural -> case c of
          Nominative -> nálar
          Accusative -> nálar
          Genitive -> nála
          Dative -> nálum
  where
    (prefix, lexeme) = splitCompound nál
    root = lexeme
    nálar = prefix ++ (syncope root ++ "ar")
    nála = prefix ++ (syncope root ++ "a")
    nálum = prefix ++ (u_mutation (syncope root) ++ "um")    


{- noun fem wo-stem -}

decl1stod :: DictForm -> Noun
decl1stod stöð (NounForm n c) =
    mkStr $
      case n of
        Singular -> case c of
          Nominative -> prefix ++ lexeme
          Accusative -> prefix ++ lexeme
          Genitive -> stöðvar
          Dative -> prefix ++ lexeme
        Plural -> case c of
          Nominative -> stöðvar
          Accusative -> stöðvar
          Genitive -> stöðva
          Dative -> stöðum
  where
    (prefix, lexeme) = splitCompound stöð
    root = lexeme
    stöðvar = prefix ++ (syncope root ++ "var")
    stöðva = prefix ++ (syncope root ++ "va")
    stöðum = prefix ++ (u_mutation (syncope root) ++ "um")        
    
    
{- noun fem jo-stem -}

decl1ben :: DictForm -> Noun
decl1ben ben (NounForm n c) =
    mkStr $
      case n of
        Singular -> case c of
          Nominative -> prefix ++ lexeme
          Accusative -> prefix ++ lexeme
          Genitive -> benjar
          Dative -> prefix ++ lexeme
        Plural -> case c of
          Nominative -> benjar
          Accusative -> benjar
          Genitive -> benja
          Dative -> benjum
  where
    (prefix, lexeme) = splitCompound ben
    root = lexeme
    benjar = prefix ++ (syncope root ++ "jar")
    benja = prefix ++ (syncope root ++ "ja")
    benjum = prefix ++ (u_mutation (syncope root) ++ "jum")        


{- noun fem io-stem -}    
    
decl1heidr :: DictForm -> Noun
decl1heidr heiðr (NounForm n c) =
    mkStr $
      case n of
        Singular -> case c of
          Nominative -> prefix ++ lexeme
          Accusative -> heiði
          Genitive -> heiðar
          Dative -> heiði
        Plural -> case c of
          Nominative -> heiðar
          Accusative -> heiðar
          Genitive -> heiða
          Dative -> heiðum
  where
    (prefix, lexeme) = splitCompound heiðr
    root = (tk 1 lexeme)
    heiði = prefix ++ (syncope root ++ "i")
    heiðar = prefix ++ (syncope root ++ "ar")
    heiða = prefix ++ (syncope root ++ "a")    
    heiðum = prefix ++ (u_mutation (syncope root) ++ "um")    

decl1gygr :: DictForm -> Noun
decl1gygr gýgr (NounForm n c) =
    mkStr $
      case n of
        Singular -> case c of
          Nominative -> prefix ++ lexeme
          Accusative -> gýgi
          Genitive -> gýgjar
          Dative -> gýgi
        Plural -> case c of
          Nominative -> gýgjar
          Accusative -> gýgjar
          Genitive -> gýgja
          Dative -> gýgjum
  where
    (prefix, lexeme) = splitCompound gýgr
    root = (tk 1 lexeme)
    gýgi = prefix ++ (syncope root ++ "i")
    gýgjar = prefix ++ (syncope root ++ "jar")
    gýgja = prefix ++ (syncope root ++ "ja")    
    gýgjum = prefix ++ (u_mutation (syncope root) ++ "jum")

    
{- noun fem i-stem -}

decl1naud :: DictForm -> Noun
decl1naud nauð (NounForm n c) =
    mkStr $
      case n of
        Singular -> case c of
          Nominative -> prefix ++ lexeme
          Accusative -> prefix ++ lexeme
          Genitive -> nauðar
          Dative -> prefix ++ lexeme
        Plural -> case c of
          Nominative -> nauðir
          Accusative -> nauðir
          Genitive -> nauða
          Dative -> nauðum
  where
    (prefix, lexeme) = splitCompound nauð
    nauðir = prefix ++ revertToStem lexeme ++ "ir"
    nauðar = prefix ++ revertToStem lexeme ++ "ar"
    nauða = prefix ++ revertToStem lexeme ++ "a"
    nauðum = prefix ++ u_mutation (syncope lexeme) ++ "um"

decl2holl :: String -> Noun
decl2holl höll nf =  
    except (decl1naud höll) 
           [(NounForm Singular Dative, höllu)] nf
      where 
        (prefix, lexeme) = splitCompound höll  
        höllu = prefix ++ u_mutation (syncope lexeme) ++ "u"    


decl1ylgr :: DictForm -> Noun
decl1ylgr ylgr (NounForm n c) =
    mkStr $
      case n of
        Singular -> case c of
          Nominative -> prefix ++ lexeme
          Accusative -> ylgi
          Genitive -> ylgjar
          Dative -> ylgi
        Plural -> case c of
          Nominative -> ylgir
          Accusative -> ylgir
          Genitive -> ylgja
          Dative -> ylgjum
  where
    (prefix, lexeme) = splitCompound ylgr
    root = tk 1 lexeme
    ylgi = prefix ++ revertToStem root ++ "i"
    ylgjar = prefix ++ revertToStem root ++ velarJ root ++ "ar"
    ylgir = prefix ++ revertToStem root ++ "ir"
    ylgja = prefix ++ revertToStem root ++ velarJ root ++ "a"
    ylgjum = prefix ++ u_mutation (syncope root) ++ velarJ root ++ "um"       

    
{- noun feminine c-stem -}    
    
decl1bok :: DictForm -> Noun
decl1bok bók (NounForm n c) =
    mkStr $
      case n of
        Singular -> case c of
          Nominative -> prefix ++ lexeme
          Accusative -> prefix ++ lexeme
          Genitive -> bókar
          Dative -> prefix ++ lexeme
        Plural -> case c of
          Nominative -> bœkr
          Accusative -> bœkr
          Genitive -> bóka
          Dative -> bókum
  where
    (prefix, lexeme) = splitCompound bók
    bókar = prefix ++ syncope lexeme ++ "ar"
    bœkr = prefix ++ i_mutation lexeme ++ "r"
    bóka = prefix ++ syncope lexeme ++ "a"
    bókum = prefix ++ u_mutation (syncope lexeme) ++ "um"
    
decl2vik :: String -> Noun
decl2vik vík nf =  
    except (decl1bok vík) 
           [(NounForm Singular Genitive, víkr)] nf
      where 
        (prefix, lexeme) = splitCompound vík  
        víkr = prefix ++ lexeme ++ "r"    
        
decl2flo :: String -> Noun
decl2flo fló nf =  
    except (decl2vik fló) 
           [(NounForm Plural Genitive, fló),
            (NounForm Plural Dative, flóm)] nf
      where 
        (prefix, lexeme) = splitCompound fló  
        flóm = prefix ++ lexeme ++ "m"                
  

decl1kyr :: DictForm -> Noun
decl1kyr kýr (NounForm n c) =
    mkStr $
      case n of
        Singular -> case c of
          Nominative -> prefix ++ lexeme
          Accusative -> ký
          Genitive -> prefix ++ lexeme
          Dative -> ký
        Plural -> case c of
          Nominative -> kiumr
          Accusative -> kiumr
          Genitive -> ký
          Dative -> kým
  where
    (prefix, lexeme) = splitCompound kýr
    root = tk 1 lexeme
    ký = prefix ++ root
    kiumr = prefix ++ i_mutation root ++ "r"
    kým = prefix ++ root ++ "m"


{- noun feminine r-stem -}

decl2dottir :: String -> Noun
decl2dottir dóttir nf =  
    except (decl1fadir dóttir) 
           [] nf
    
 
{- noun feminine on-stem -}

decl1tunga :: DictForm -> Noun
decl1tunga tunga (NounForm n c) =
    mkStr $
      case n of
        Singular -> case c of
          Nominative -> prefix ++ lexeme
          Accusative -> tungu
          Genitive -> tungu
          Dative -> tungu
        Plural -> case c of
          Nominative -> tungur
          Accusative -> tungur
          Genitive -> tungna
          Dative -> tungum
  where
    (prefix, lexeme) = splitCompound tunga
    root = tk 1 lexeme
    tungu = prefix ++ syncope root ++ "u"
    tungur = prefix ++ syncope root ++ "ur"
    tungna = prefix ++ syncope root ++ "na"   
    tungum = prefix ++ syncope root ++ "um"
    
    
{- noun feminine jon-stem -}    
    
decl1brynja :: DictForm -> Noun
decl1brynja brynja (NounForm n c) =
    mkStr $
      case n of
        Singular -> case c of
          Nominative -> prefix ++ lexeme
          Accusative -> brynju
          Genitive -> brynju
          Dative -> brynju
        Plural -> case c of
          Nominative -> brynjur
          Accusative -> brynjur
          Genitive -> prefix ++ lexeme
          Dative -> brynjum
  where
    (prefix, lexeme) = splitCompound brynja
    root = tk 2 lexeme
    brynju = prefix ++ syncope root ++ "ju"
    brynjur = prefix ++ syncope root ++ "jur"
    brynjum = prefix ++ syncope root ++ "jum"   
    
decl2kirkja :: String -> Noun
decl2kirkja kirkja nf =  
    except (decl1brynja kirkja) 
           [(NounForm Plural Genitive, kirkna)] nf
      where 
        (prefix, lexeme) = splitCompound kirkja
        root = tk 2 lexeme        
        kirkna = prefix ++ syncope root ++ "na"    
    
    
{- noun feminine won-stem -}

decl1volva :: DictForm -> Noun
decl1volva völva (NounForm n c) =
    mkStr $
      case n of
        Singular -> case c of
          Nominative -> prefix ++ lexeme
          Accusative -> völu
          Genitive -> völu
          Dative -> völu
        Plural -> case c of
          Nominative -> völur
          Accusative -> völur
          Genitive -> prefix ++ lexeme
          Dative -> völum
  where
    (prefix, lexeme) = splitCompound völva
    root = tk 2 lexeme
    völu = prefix ++ syncope root ++ "u"
    völur = prefix ++ syncope root ++ "ur"
    völum = prefix ++ syncope root ++ "um"     


{- noun feminine in-stem -}    
    
decl1elli :: DictForm -> Noun
decl1elli elli (NounForm n c) =
    mkStr $
      case n of
        Singular -> case c of
          Nominative -> prefix ++ lexeme
          Accusative -> prefix ++ lexeme
          Genitive -> prefix ++ lexeme
          Dative -> prefix ++ lexeme
        Plural -> case c of
          Nominative -> na
          Accusative -> na
          Genitive -> na
          Dative -> na
  where
    (prefix, lexeme) = splitCompound elli
    na = "n/a"     

decl2iski :: String -> Noun
decl2iski iski nf =  
    except (decl1elli iski) 
           [(NounForm Singular Genitive, iskjar)] nf
      where 
        (prefix, lexeme) = splitCompound iski
        root = tk 1 lexeme        
        iskjar = prefix ++ syncope root ++ "jar"        
    
decl1gorsimi :: DictForm -> Noun
decl1gorsimi görsimi (NounForm n c) =
    mkStr $
      case n of
        Singular -> case c of
          Nominative -> prefix ++ lexeme
          Accusative -> prefix ++ lexeme
          Genitive -> görsimjar
          Dative -> prefix ++ lexeme
        Plural -> case c of
          Nominative -> görsimar
          Accusative -> görsimar
          Genitive -> görsima
          Dative -> görsimum
  where
    (prefix, lexeme) = splitCompound görsimi
    root = tk 1 lexeme
    görsimjar = prefix ++ syncope root ++ "jar"
    görsimar = prefix ++ syncope root ++ "ar"
    görsima = prefix ++ syncope root ++ "a"
    görsimum = prefix ++ u_mutation (syncope root) ++ "um"

    
    
    
    
    
    
        
syncope :: String -> String
syncope man = synced
    where
      (pre, middle, post) = findSyncopeEnding man
      (fL, rest) = firstLetter man
      synced
       | post == "il" && not (isVowelString middle) = pre ++ middle ++ "l"
       | post == "in" && not (isVowelString middle) = pre ++ middle ++ "n"
       | post == "ul" && not (isVowelString middle) = pre ++ middle ++ "l"
       | post == "un" && not (isVowelString middle) = pre ++ middle ++ "n"
{-       | post == "ar" && not (isVowelString middle) && not (isUCConsonant fL) = pre ++ middle ++ "r" -}
       | post == "ar" && not (isVowelString middle) = pre ++ middle ++ "r"       
       | post == "an" && not (isVowelString middle) = pre ++ middle ++ "n"
       | otherwise = pre ++ middle ++ post

findSyncopeEnding :: String -> (String, String, String) 
findSyncopeEnding premiddlepost = (reverse erp, middle, reverse tsop)
    where
      (tsop, elddimerp) = splitAt 2 $ reverse premiddlepost
      (middle, erp) = splitAt 1 $ elddimerp

isVowelString :: String -> Bool
isVowelString a = elem a ["a", "e", "i", "o", "u", "y", "ö", "ó", "ø", "á", "í"]

isVowel c = elem c "aeiouyöóøáí"

excisionConsonant :: (String, String) -> String 
excisionConsonant (beg, end) = be ++ delCons ge ++ nd
    where
      (be, ge, nd) = lastFirst (beg, end)
      delCons x = case x of
        "nn" -> "n"
        "rr" -> "r"
        _    -> x

lastFirst :: (String, String) -> (String, String, String) 
lastFirst (stem, end) = (ste, me, nd)
    where
      (ste, m) = lastLetter stem
      (e, nd) = firstLetter end
      me = m ++ e

lastLetter :: String -> (String, String) 
lastLetter be = (reverse b, e)
    where
      (e, b) = splitAt 1 $ reverse be 

firstLetter :: String -> (String, String) 
firstLetter be = (b, e)
    where
      (b, e) = splitAt 1 $ be

u_mutation :: String -> String
u_mutation man = m ++ mkUm a ++ n
    where
      (m,a,n) = findStemVowel man
      mkUm v = case v of
        "a" -> "ö"
        _   -> v
        
i_mutation :: String -> String
i_mutation man = m ++ mkUm a ++ n
    where
      (m,a,n) = findStemVowel man
      mkUm v = case v of
        "a" -> "e"
        "ó" -> "œ"
        _   -> v        
  
reverseIMutation :: String -> String
reverseIMutation man = m ++ mkUm a ++ n
    where
      (m,a,n) = findStemVowel man
      mkUm v = case v of
        "e" -> "a"
        "œ" -> "ó"
        _   -> v        

changeStemVowel :: (String, String) -> String
changeStemVowel (man,new) = m ++ new ++ n
    where
      (m,a,n) = findStemVowel man

        
revertToStem :: String -> String
revertToStem man = m ++ mkUm a ++ n
    where
      (m,a,n) = findStemVowel man
      mkUm v = case v of
        "ö" -> "a"
        _   -> v        
      
findStemVowel :: String -> (String, String, String)
findStemVowel sprick = (reverse rps, reverse i, reverse kc)
    where
      (kc, irps) = break isVowel $ reverse sprick
      (i, rps) = span isVowel $ irps


findSyncopVowel :: String -> (String, String, String)
findSyncopVowel sprick = (reverse rps, reverse i, reverse kc)
    where
      (kc, irps) = splitAt 1 $ reverse sprick
      (i, rps) = span isVowel $ irps 
      
splitCompound :: String -> (String, String)
splitCompound word = (prefix, reverse lexeme)
    where
      (lexeme, prehyp) = break ('-'==) $ reverse word
      (prefix, hyphen) = break ('-'==) $ reverse prehyp
      
velarJ :: String -> String
velarJ root = addJ lletter
    where
      (rest, lletter) = lastLetter root
      addJ j = if isVelar lletter then "j" else ""

isVelar :: String -> Bool
isVelar a = elem a ["g", "k"]


-- VERBS


dropV :: String -> String
dropV stem = (ste ++ delV m)
    where 
      (ste,m) = lastLetter stem
      delV x = case x of
        "v" -> ""
        _   -> x
        
dropJ :: String -> String
dropJ stem = (ste ++ dropJ m)
    where 
      (ste,m) = lastLetter stem
      dropJ x = case x of
        "j" -> ""
        _   -> x        
        
getPreteriteRoot :: String -> String
getPreteriteRoot stem = (ste ++ pretsuf m)
    where 
      (ste,m) = lastLetter stem
      pretsuf x = case x of
        "m" -> "md"
        "n" -> "nd"
        "t" -> "tt"
        "s" -> "st"
        "p" -> "pt"
        "ð" -> "dd"
        _   -> x ++ "ð"     
        


{- weak class I    -}

decl1elska :: String -> Verb
decl1elska elska vf =
  case vf of
   Infinitive v ->
     mkStr $
       case v of 
         Active -> lexeme
         Middle -> lexeme ++ "sk"
   PresentParticiple v ->
     mkStr $
       case v of
         Active -> lexeme ++ "ndi"
         Middle -> lexeme ++ "ndisk"
   PastParticiple v ->
     mkStr $
       case v of
         Active -> lexeme ++ "ðr"
         Middle -> lexeme ++ "zk"    
   Impera2sg v ->
     mkStr $
       case v of
         Active -> lexeme
         Middle -> lexeme ++ "sk"    
   Impera1pl v ->
     mkStr $
       case v of
         Active -> dropV(u_mutation (syncope root)) ++ "um"
         Middle -> dropV(u_mutation (syncope root)) ++ "umk"    
   Impera2pl v ->
     mkStr $
       case v of
         Active -> dropJ(root) ++ "ið"
         Middle -> dropJ(root) ++ "izk"             
   Indicative v n p t ->
     mkStr $
       case v of
         Active ->
           case t of 
             Present ->
               case n of
                 Singular ->
                   case p of 
                     P1 -> lexeme
                     P2 -> lexeme ++ "r"
                     P3 -> lexeme ++ "r"
                 Plural ->
                   case p of
                     P1 -> dropV(u_mutation (syncope root)) ++ "um"
                     P2 -> dropJ(root) ++ "ið"
                     P3 -> lexeme      
             Past ->
               case n of
                 Singular ->
                   case p of 
                     P1 -> lexeme ++ "ða"
                     P2 -> lexeme ++ "ðir"
                     P3 -> lexeme ++ "ði"
                 Plural ->
                   case p of
                     P1 -> dropV(u_mutation (syncope root)) ++ "uðum"
                     P2 -> dropV(u_mutation (syncope root)) ++ "uðuð"
                     P3 -> dropV(u_mutation (syncope root)) ++ "uðu"
         Middle ->
           case t of 
             Present ->
               case n of
                 Singular ->
                   case p of 
                     P1 -> dropV(u_mutation (syncope root)) ++ "umk"
                     P2 -> lexeme ++ "sk"
                     P3 -> lexeme ++ "sk"
                 Plural ->
                   case p of
                     P1 -> dropV(u_mutation (syncope root)) ++ "umk"
                     P2 -> dropJ(root) ++ "izk"
                     P3 -> lexeme ++ "sk"    
             Past ->
               case n of
                 Singular ->
                   case p of 
                     P1 -> dropV(u_mutation (syncope root)) ++ "uðumk"
                     P2 -> lexeme ++ "ðisk"
                     P3 -> lexeme ++ "ðisk"
                 Plural ->
                   case p of
                     P1 -> dropV(u_mutation (syncope root)) ++ "uðumk"
                     P2 -> dropV(u_mutation (syncope root)) ++ "uðuzk"
                     P3 -> dropV(u_mutation (syncope root)) ++ "uðusk"
   Optative v n p t ->
     mkStr $
       case v of
         Active ->
           case t of 
             Present ->
               case n of
                 Singular ->
                   case p of 
                     P1 -> lexeme
                     P2 -> dropJ(root) ++ "ir"
                     P3 -> dropJ(root) ++ "i"
                 Plural ->
                   case p of
                     P1 -> dropJ(root) ++ "im"
                     P2 -> dropJ(root) ++ "ið"
                     P3 -> dropJ(root) ++ "i"     
             Past ->
               case n of
                 Singular ->
                   case p of 
                     P1 -> lexeme ++ "ða"
                     P2 -> lexeme ++ "ðir"
                     P3 -> lexeme ++ "ði"
                 Plural ->
                   case p of
                     P1 -> lexeme ++ "ðim"
                     P2 -> lexeme ++ "ðið"
                     P3 -> lexeme ++ "ði"
         Middle ->
           case t of 
             Present ->
               case n of
                 Singular ->
                   case p of 
                     P1 -> dropV(u_mutation (syncope root)) ++ "umk"
                     P2 -> dropJ(root) ++ "isk"
                     P3 -> dropJ(root) ++ "isk"
                 Plural ->
                   case p of
                     P1 -> dropJ(root) ++ "imk"
                     P2 -> dropJ(root) ++ "izk"
                     P3 -> dropJ(root) ++ "isk"    
             Past ->
               case n of
                 Singular ->
                   case p of 
                     P1 -> dropV(u_mutation (syncope root)) ++ "uðumk"
                     P2 -> lexeme ++ "ðisk"
                     P3 -> lexeme ++ "ðisk"
                 Plural ->
                   case p of
                     P1 -> lexeme ++ "ðimk"
                     P2 -> lexeme ++ "ðizk"
                     P3 -> lexeme ++ "ðisk"
  where
    lexeme = elska
    root = tk 1 lexeme

decl1stodva :: String -> Verb
decl1stodva stöðva vf =
    except (decl1elska stöðva)
           [(Indicative Middle Singular P1 Present, na),
            (Indicative Middle Singular P1 Past, na),
            (Indicative Middle Singular P2 Present, na),
            (Indicative Middle Singular P2 Past, na),
            (Indicative Middle Singular P3 Present, na),
            (Indicative Middle Singular P3 Past, na),
            (Indicative Middle Plural P1 Present, na),
            (Indicative Middle Plural P1 Past, na),
            (Indicative Middle Plural P2 Present, na),
            (Indicative Middle Plural P2 Past, na),
            (Indicative Middle Plural P3 Present, na),
            (Indicative Middle Plural P3 Past, na),
            (Optative Middle Singular P1 Present, na),
            (Optative Middle Singular P1 Past, na),
            (Optative Middle Singular P2 Present, na),
            (Optative Middle Singular P2 Past, na),
            (Optative Middle Singular P3 Present, na),
            (Optative Middle Singular P3 Past, na),
            (Optative Middle Plural P1 Present, na),
            (Optative Middle Plural P1 Past, na),
            (Optative Middle Plural P2 Present, na),
            (Optative Middle Plural P2 Past, na),
            (Optative Middle Plural P3 Present, na),
            (Optative Middle Plural P3 Past, na)] vf
       where
         na = "n/a"
         
decl1spa :: String -> Verb
decl1spa spá vf =
    except (decl1elska spá) 
           [(Impera2pl Middle, lexeme ++ "ið"),
            (Impera1pl Active, lexeme ++ "m"),
            (Impera1pl Middle, lexeme ++ "mk"),
            (Indicative Active Plural P1 Present, lexeme ++ "m"),
            (Indicative Active Plural P1 Past, lexeme ++ "ðum"),
            (Indicative Active Plural P2 Past, lexeme ++ "ðuð"),
            (Indicative Active Plural P3 Past, lexeme ++ "ðu"),
            (Indicative Middle Singular P1 Present, lexeme ++ "mk"),
            (Indicative Middle Plural P1 Present, lexeme ++ "mk"),
            (Indicative Middle Singular P1 Past, lexeme ++ "ðumk"),
            (Indicative Middle Plural P1 Past, lexeme ++ "ðumk"),
            (Indicative Middle Plural P2 Past, lexeme ++ "ðuzk"),
            (Indicative Middle Plural P3 Past, lexeme ++ "ðusk"),
            (Optative Middle Singular P1 Present, lexeme ++ "mk"),
            (Optative Middle Singular P1 Past, lexeme ++ "ðumk")] vf
       where
         lexeme = spá

         
{- weak class II verbs -}         

decl1krefja :: String -> Verb
decl1krefja krefja vf =
  case vf of
   Infinitive v ->
     mkStr $
       case v of 
         Active -> lexeme
         Middle -> root ++ "sk"
   PresentParticiple v ->
     mkStr $
       case v of
         Active -> root ++ "ndi"
         Middle -> root ++ "ndisk"
   PastParticiple v ->
     mkStr $
       case v of
         Active -> root_pret_reverse_i ++ "r"
         Middle -> root ++ "zk"    
   Impera2sg v ->
     mkStr $
       case v of
         Active -> root
         Middle -> root ++ "sk"    
   Impera1pl v ->
     mkStr $
       case v of
         Active -> dropV(u_mutation (syncope root)) ++ "um"
         Middle -> dropV(u_mutation (syncope root)) ++ "umk"    
   Impera2pl v ->
     mkStr $
       case v of
         Active -> dropJ(root) ++ "ið"
         Middle -> dropJ(root) ++ "izk"             
   Indicative v n p t ->
     mkStr $
       case v of
         Active ->
           case t of 
             Present ->
               case n of
                 Singular ->
                   case p of 
                     P1 -> root
                     P2 -> root ++ "r"
                     P3 -> root ++ "r"
                 Plural ->
                   case p of
                     P1 -> dropV(u_mutation (syncope root)) ++ "jum"
                     P2 -> dropJ(root) ++ "ið"
                     P3 -> lexeme     
             Past ->
               case n of
                 Singular ->
                   case p of 
                     P1 -> root_pret_reverse_i ++ "a"
                     P2 -> root_pret_reverse_i ++ "ir"
                     P3 -> root_pret_reverse_i ++ "i"
                 Plural ->
                   case p of
                     P1 -> dropV(u_mutation (syncope root_pret_reverse_i)) ++ "um"
                     P2 -> dropV(u_mutation (syncope root_pret_reverse_i)) ++ "uð"
                     P3 -> dropV(u_mutation (syncope root_pret_reverse_i)) ++ "u"
         Middle ->
           case t of 
             Present ->
               case n of
                 Singular ->
                   case p of 
                     P1 -> dropV(u_mutation (syncope root)) ++ "jumk"
                     P2 -> root ++ "sk"
                     P3 -> lexeme ++ "sk"
                 Plural ->
                   case p of
                     P1 -> dropV(u_mutation (syncope root)) ++ "jumk"
                     P2 -> dropJ(root) ++ "izk"
                     P3 -> lexeme ++ "sk"    
             Past ->
               case n of
                 Singular ->
                   case p of 
                     P1 -> dropV(u_mutation (syncope root_pret_reverse_i)) ++ "umk"
                     P2 -> root_pret ++ "isk"
                     P3 -> root_pret ++ "isk"
                 Plural ->
                   case p of
                     P1 -> dropV(u_mutation (syncope root_pret_reverse_i)) ++ "umk"
                     P2 -> dropV(u_mutation (syncope root_pret_reverse_i)) ++ "uzk"
                     P3 -> dropV(u_mutation (syncope root_pret_reverse_i)) ++ "usk"
   Optative v n p t ->
     mkStr $
       case v of
         Active ->
           case t of 
             Present ->
               case n of
                 Singular ->
                   case p of 
                     P1 -> lexeme
                     P2 -> dropJ(root) ++ "ir"
                     P3 -> dropJ(root) ++ "i"
                 Plural ->
                   case p of
                     P1 -> dropJ(root) ++ "im"
                     P2 -> dropJ(root) ++ "ið"
                     P3 -> dropJ(root) ++ "i"     
             Past ->
               case n of
                 Singular ->
                   case p of 
                     P1 -> root_pret ++ "a"
                     P2 -> root_pret ++ "ir"
                     P3 -> root_pret ++ "i"
                 Plural ->
                   case p of
                     P1 -> root_pret ++ "im"
                     P2 -> root_pret ++ "ið"
                     P3 -> root_pret ++ "i"
         Middle ->
           case t of 
             Present ->
               case n of
                 Singular ->
                   case p of 
                     P1 -> dropV(u_mutation (syncope root)) ++ "jumk"
                     P2 -> dropJ(root) ++ "isk"
                     P3 -> dropJ(root) ++ "isk"
                 Plural ->
                   case p of
                     P1 -> dropJ(root) ++ "imk"
                     P2 -> dropJ(root) ++ "izk"
                     P3 -> dropJ(root) ++ "isk"    
             Past ->
               case n of
                 Singular ->
                   case p of 
                     P1 -> dropV(u_mutation (syncope root_pret)) ++ "umk"
                     P2 -> root_pret ++ "isk"
                     P3 -> root_pret ++ "isk"
                 Plural ->
                   case p of
                     P1 -> root_pret ++ "imk"
                     P2 -> root_pret ++ "izk"
                     P3 -> root_pret ++ "isk"
  where
    lexeme = krefja
    root = tk 2 lexeme
    root_pret = getPreteriteRoot(root)
    root_pret_reverse_i = reverseIMutation(root_pret)


decl1lyja :: String -> Verb
decl1lyja lýja vf =
    except (decl1krefja lýja)
           [(Indicative Middle Singular P1 Present, na),
            (Indicative Middle Singular P1 Past, na),
            (Indicative Middle Singular P2 Present, na),
            (Indicative Middle Singular P2 Past, na),
            (Indicative Middle Singular P3 Present, na),
            (Indicative Middle Singular P3 Past, na),
            (Indicative Middle Plural P1 Present, na),
            (Indicative Middle Plural P1 Past, na),
            (Indicative Middle Plural P2 Present, na),
            (Indicative Middle Plural P2 Past, na),
            (Indicative Middle Plural P3 Present, na),
            (Indicative Middle Plural P3 Past, na),
            (Optative Middle Singular P1 Present, na),
            (Optative Middle Singular P1 Past, na),
            (Optative Middle Singular P2 Present, na),
            (Optative Middle Singular P2 Past, na),
            (Optative Middle Singular P3 Present, na),
            (Optative Middle Singular P3 Past, na),
            (Optative Middle Plural P1 Present, na),
            (Optative Middle Plural P1 Past, na),
            (Optative Middle Plural P2 Present, na),
            (Optative Middle Plural P2 Past, na),
            (Optative Middle Plural P3 Present, na),
            (Optative Middle Plural P3 Past, na)] vf
       where
         na = "n/a"
         
         
{- strong class I verbs -}

decl1bita :: String -> Verb
decl1bita bíta vf =
  case vf of
   Infinitive v ->
     mkStr $
       case v of 
         Active -> lexeme
         Middle -> lexeme ++ "sk"
   PresentParticiple v ->
     mkStr $
       case v of
         Active -> lexeme ++ "ndi"
         Middle -> lexeme ++ "ndisk"
   PastParticiple v ->
     mkStr $
       case v of
         Active -> lexeme ++ "ðr"
         Middle -> lexeme ++ "zk"    
   Impera2sg v ->
     mkStr $
       case v of
         Active -> lexeme
         Middle -> lexeme ++ "sk"    
   Impera1pl v ->
     mkStr $
       case v of
         Active -> dropV(u_mutation (syncope root)) ++ "um"
         Middle -> dropV(u_mutation (syncope root)) ++ "umk"    
   Impera2pl v ->
     mkStr $
       case v of
         Active -> dropJ(root) ++ "ið"
         Middle -> dropJ(root) ++ "izk"             
   Indicative v n p t ->
     mkStr $
       case v of
         Active ->
           case t of 
             Present ->
               case n of
                 Singular ->
                   case p of 
                     P1 -> root
                     P2 -> root ++ "r"
                     P3 -> root ++ "r"
                 Plural ->
                   case p of
                     P1 -> dropV(u_mutation (syncope root)) ++ "um"
                     P2 -> dropJ(root) ++ "ið"
                     P3 -> lexeme      
             Past ->
               case n of
                 Singular ->
                   case p of 
                     P1 -> root_pastsg
                     P2 -> root_pastsg ++ "ðir"
                     P3 -> root_pastsg
                 Plural ->
                   case p of
                     P1 -> dropV(u_mutation (syncope root_pastpl)) ++ "um"
                     P2 -> dropV(u_mutation (syncope root_pastpl)) ++ "uð"
                     P3 -> dropV(u_mutation (syncope root_pastpl)) ++ "u"
         Middle ->
           case t of 
             Present ->
               case n of
                 Singular ->
                   case p of 
                     P1 -> dropV(u_mutation (syncope root)) ++ "umk"
                     P2 -> lexeme ++ "zk"
                     P3 -> lexeme ++ "zk"
                 Plural ->
                   case p of
                     P1 -> dropV(u_mutation (syncope root)) ++ "umk"
                     P2 -> dropJ(root) ++ "izk"
                     P3 -> lexeme ++ "sk"    
             Past ->
               case n of
                 Singular ->
                   case p of 
                     P1 -> dropV(u_mutation (syncope root_pastpl)) ++ "umk"
                     P2 -> root_pastsg ++ "zk"
                     P3 -> root_pastsg ++ "zk"
                 Plural ->
                   case p of
                     P1 -> dropV(u_mutation (syncope root_pastpl)) ++ "umk"
                     P2 -> dropV(u_mutation (syncope root_pastpl)) ++ "uzk"
                     P3 -> dropV(u_mutation (syncope root_pastpl)) ++ "usk"
   Optative v n p t ->
     mkStr $
       case v of
         Active ->
           case t of 
             Present ->
               case n of
                 Singular ->
                   case p of 
                     P1 -> lexeme
                     P2 -> dropJ(root) ++ "ir"
                     P3 -> dropJ(root) ++ "i"
                 Plural ->
                   case p of
                     P1 -> dropJ(root) ++ "im"
                     P2 -> dropJ(root) ++ "ið"
                     P3 -> dropJ(root) ++ "i"     
             Past ->
               case n of
                 Singular ->
                   case p of 
                     P1 -> root_pastpl ++ "a"
                     P2 -> root_pastpl ++ "ir"
                     P3 -> root_pastpl ++ "i"
                 Plural ->
                   case p of
                     P1 -> root_pastpl ++ "im"
                     P2 -> root_pastpl ++ "ið"
                     P3 -> root_pastpl ++ "i"
         Middle ->
           case t of 
             Present ->
               case n of
                 Singular ->
                   case p of 
                     P1 -> dropV(u_mutation (syncope root)) ++ "umk"
                     P2 -> dropJ(root) ++ "isk"
                     P3 -> dropJ(root) ++ "isk"
                 Plural ->
                   case p of
                     P1 -> dropJ(root) ++ "imk"
                     P2 -> dropJ(root) ++ "izk"
                     P3 -> dropJ(root) ++ "isk"    
             Past ->
               case n of
                 Singular ->
                   case p of 
                     P1 -> dropV(u_mutation (syncope root_pastpl)) ++ "umk"
                     P2 -> root_pastpl ++ "isk"
                     P3 -> root_pastpl ++ "isk"
                 Plural ->
                   case p of
                     P1 -> root_pastpl ++ "imk"
                     P2 -> root_pastpl ++ "izk"
                     P3 -> root_pastpl ++ "isk"
  where
    lexeme = bíta
    root = tk 1 lexeme
    root_pastsg = changeStemVowel(root,"ei")
    root_pastpl = changeStemVowel(root,"i")
         