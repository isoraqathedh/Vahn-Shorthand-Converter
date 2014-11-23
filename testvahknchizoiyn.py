import unittest
import re

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

    def test_Ag_is_jarngi(self):
        p = Parser()
        result = p.parse("Ag")
        self.assertEqual("jarngi", result)

class Parser():
    def parse(self, input):
        newword = re.compile("^$|\\s+")  # Regex Query to determine if a string ends in whitespace or null
        output = ""  # By Default, with no input, no output should be given.
        logograms = {  # A list all characters which combine through portmanteau from the first letter
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
        duallogograms = {  # A list of all characters which combine through portmanteau from the first 3 letters
            "A":"jarehr",
            "U":"saruhr",
            "O":"paroor",
        }
        postfixideograms = {  # A list of all characters which combine at the end of the word, or at their position of being typed
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
        infixideograms = {  # A list of all characters which combine after the initial of a three character cluster
            "h":"h",
            "x":"l",
        }
        insideslash = False
        for C in input:  # Process input one character at a time
            if not insideslash:
                matchcase = False  # Set all Default Values
                dualmatchcase = False
                endmatchcase = False
                lenmatch = 0
                endlenmatch = 0
                for entries in logograms.values():  # Determine if there is a complete logogram at the end of the output
                    if output[-len(entries):] == entries:
                        matchcase = True
                        lenmatch = len(entries)   
                for entries in duallogograms.values():  # Determine if there is a complete dual logogram at the end of the output
                    if output[-len(entries):] == entries:
                        dualmatchcase = True
                        lenmatch = len(entries)
                for entries in logograms.values():  # Determine if there is the second half a logogram at the end of the output
                    if output[-len(entries[1:]):] == entries[1:]:
                        endmatchcase = True
                        endlenmatch = len(entries[1:])
                if C == "/":  # If the user enters a slash notation section preserve it without conversion
                    if dualmatchcase:
                        output = output[:3-lenmatch]
                    output += C
                    insideslash = True
                elif C in logograms:  # If the current character is assigned to a logogram
                    if dualmatchcase:  # If there is a dual logogram on the end of the output remove it's latter half and add the inputted logogram
                        output = output[:3-lenmatch]
                        output += logograms[C]       
                    elif logograms[C] == output[:lenmatch]:  # If there is a duplicate of the current logogram at the end of the output add another instance of it
                        output += logograms[C]    
                    elif matchcase == False:  # If there is no logogram at the end of the output add the currently requested output string
                        output += logograms[C]            
                    else:  # If there is a logogram on the end of the output strip all letters but it's initial and combine all letter but the initial of the requested output string
                        output = output[:1-lenmatch]
                        output += logograms[C][1:]           
                elif C in duallogograms:  # If the current character is assigned to a dual logogram
                    if not newword.match(output[-1:]):  # If the current output does not end in a new word (whitespace or null character) add the end of the dual logogram
                        output += duallogograms[C][3:]
                    else:  # If the current output is a new word add the entire logogram
                        output += duallogograms[C] 
                elif C in postfixideograms:  # If the current character is assigned to a postfixed ideogram append it to the output, overwritting a dual logogram if necessary
                    if dualmatchcase:
                        output = output[:3-lenmatch]
                        output += postfixideograms[C]
                    else:
                        output += postfixideograms[C]
                elif C in infixideograms:  # If the current character is assigned to an infix ideogram add it in after the inital of the last logogram cluster, if this is not the case report an error
                    if endmatchcase:
                        output = output[:-endlenmatch] + infixideograms[C] + output[-endlenmatch:]
                    else:
                        output = " INVALID USE OF 'h' "
                elif C == " " or C == "~":  # If the user enters a space allow it to go through as a space.
                    output += C
            else:
                output += C
                if C == "/":
                    insideslash = False
        return output

if __name__ == '__main__': unittest.main()

