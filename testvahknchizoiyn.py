import unittest
import re

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

