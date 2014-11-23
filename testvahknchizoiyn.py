import unittest
import re

class Tests(unittest.TestCase):
    def test_T_is_tor(self):
        p = Parser()
        result = p.parse("T")
        self.assertEqual("tor", result)
        
    def test_K_is_kah(self):
        p = Parser()
        result = p.parse("K")
        self.assertEqual("kah", result)

    def test_R_is_rar(self):
        p = Parser()
        result = p.parse("R")
        self.assertEqual("rar", result)

    def test_KP_is_koo(self):
        p = Parser()
        result = p.parse("KP")
        self.assertEqual("koo", result)

    def test_VB_is_var(self):
        p = Parser()
        result = p.parse("VB")
        self.assertEqual("var", result)

    def test_LL_is_laiylaiy(self):
        p = Parser()
        result = p.parse("LL")
        self.assertEqual("laiylaiy", result)

    def test_KK_is_kahkah(self):
        p = Parser()
        result = p.parse("KK")
        self.assertEqual("kahkah", result)

    def test_LM_is_loo(self):
        p = Parser()
        result = p.parse("LM")
        self.assertEqual("loo", result)

    def test_ML_is_maiy(self):
        p = Parser()
        result = p.parse("ML")
        self.assertEqual("maiy", result)

    def test_U_is_saruhr(self):
        p = Parser()
        result = p.parse("U")
        self.assertEqual("saruhr", result)

    def test_AR_is_jarrar(self):
        p = Parser()
        result = p.parse("AR")
        self.assertEqual("jarrar", result)

    def test_RA_is_jarrar(self):
        p = Parser()
        result = p.parse("RA")
        self.assertEqual("rarehr", result)

    def test_DFA_is_deeehr(self):
        p = Parser()
        result = p.parse("DFA")
        self.assertEqual("deeehr", result)

    def test_UR_is_sartor(self):
        p = Parser()
        result = p.parse("UT")
        self.assertEqual("sartor", result)

    def test_Rn_is_rarn(self):
        p = Parser()
        result = p.parse("Rn")
        self.assertEqual("rarn", result)

    def test_PSqMi_ZT_Tw_is_puhngmoongah_zor_torw(self):
        p = Parser()
        result = p.parse("PSqMi ZT Tw")
        self.assertEqual("puhngmoongah zor torw", result)

    def test_K_Cwy_RTn_YVBwy_Kha_Ran_Thw(self):
        p = Parser()
        result = p.parse("K Cwy RTn YVBwy Kha Ran Thw")
        self.assertEqual("kah chiwya rorn yavarwya khahwa rarwan thorw", result)

class Parser():
    def parse(self, input):
        newword = re.compile("^$|\\s+")
        output = ""
        logograms = {
            "K":"kah",
            "R":"rar",
            "T":"tor",
            "P":"poo",
            "S":"suh",
            "D":"deu",
            "F":"fee",
            "G":"gih",
            "H":"hai",
            "J":"jeh",
            "L":"laiy",
            "Z":"zoiy",
            "V":"vah",
            "B":"bar",
            "N":"nor",
            "M":"moo",
        }
        duallogograms = {
            "A":"jarehr",
            "U":"saruhr",
            "O":"paroor",
        }
        postfixideograms = {
            "k":"k",
            "d":"th",
            "a":"wa",
            "n":"n",
            "m":"m",
            "w":"w",
            "l":"l",
            "g":"ngi",
            "t":"t",
            "q":"ng",
            "y":"ya",
            "Y":"ya",
            "c":"chi",
            "C":"chi",
            "o":"ngol",
            "b":"ngay",
            "i":"ngah",
            "r":"ngor",
        }
        infixideograms = {
            "h":"h",
            "x":"l"
        }
        for C in input:
            matchcase = False
            dualmatchcase = False
            endmatchcase = False
            lenmatch = 0
            endlenmatch = 0
            for entries in logograms.values():
                if output[-len(entries):] == entries:
                    matchcase = True
                    lenmatch = len(entries)   
            for entries in duallogograms.values():
                if output[-len(entries):] == entries:
                    dualmatchcase = True
                    lenmatch = len(entries)
                    
            for entries in logograms.values():
                if output[-len(entries[1:]):] == entries[1:]:
                    endmatchcase = True
                    endlenmatch = len(entries[1:])
                    
            if C in logograms:
                if dualmatchcase:
                    output = output[:3-lenmatch]
                    output += logograms[C]       
                elif logograms[C] == output[:lenmatch]:
                    output += logograms[C]    
                elif matchcase == False:
                    output += logograms[C]            
                else:
                    output = output[:1-lenmatch]
                    output += logograms[C][1:]           
            elif C in duallogograms:
                if not newword.match(output[-1:]):
                    output += duallogograms[C][3:]
                else:
                    output += duallogograms[C]
            elif C in postfixideograms:
                output += postfixideograms[C]
            elif C in infixideograms:
                if endmatchcase:
                    output = output[:-endlenmatch] + infixideograms[C] + output[-endlenmatch:]
            elif C == " ":
                output += C
        return output

if __name__ == '__main__': unittest.main()
