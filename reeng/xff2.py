#!/usr/bin/env python
import sys
from struct import unpack

def xfferr (msg):
    sys.stderr.write (msg)
    sys.exit (1)

class XFF:
    """XFF"""

    def __init__ (self, file, pos):
        self.pos = pos

        file.seek (pos)
        buf = file.read (0x70)
        D = unpack ("<28I", buf)

        if not buf[0:4] in ["xff\x00", "xff2"]:
            xfferr ("%08x: invalid primary signature %s %x" % (pos, buf[0:4], D[0]))

        if D[1] <> 0:
            xfferr ("%08x: expected zero got %#x" % (pos + 4, D[1]))

        if D[2] <> 0:
            xfferr ("%08x: expected zero got %#x" % (pos + 4, D[2]))

        self.count_0C = D[3]
        self.xxxxx_10 = D[4]
        self.xff_size = D[5]
        #print D[5]
        self.xxxxx_18 = D[6]
        self.count_1C = D[7]
        self.xxxxx_20 = D[8]
        self.symcount = D[9]
        self.xxxxx_28 = D[10]
        self.xxxxx_2C = D[11]
        self.xxxxx_30 = D[12]
        self.xxxxx_34 = D[13]
        self.count_38 = D[14]
        self.xxxxx_3C = D[15]
        self.seccount = D[16]
        self.xxxxx_44 = D[17]
        self.xxxxx_48 = D[18]
        self.off_sign = D[19]

        self.off_50_count_1C = D[20]
        self.off_symbols1    = D[21]
        self.off_symstrtab   = D[22]
        self.off_sections    = D[23]
        self.off_symbols2    = D[24]
        self.off_64_count_38 = D[25]
        self.off_secnameoffs = D[26]
        self.off_secstrtab   = D[27]

        ### Header[0]
        file.seek (self.off_50_count_1C + self.pos)
        self.data_50_count_1C = file.read (self.count_1C * 4)

        ### Header[1,2,4]
        file.seek (self.off_symbols1 + self.pos)
        buf = file.read (self.symcount*4*4)
        L = unpack ("<%dI" % (self.symcount*4), buf)

        file.seek (self.off_symbols2 + self.pos)
        buf = file.read (self.symcount*4)
        LL = unpack ("<%dI" % self.symcount, buf)

        maxoff = 0
        symbols = []
        for i in range (self.symcount):
            Ls = L[i*4:(i+1)*4]
            maxoff = max (maxoff, Ls[0])
            symbols.append (Ls + (LL[i],))

        file.seek (self.off_symstrtab + self.pos)
        symstrtab = file.read (maxoff + 512)

        self.symbols = []
        for sym in symbols:
            s = symstrtab[sym[0]:]
            name = s[:s.find ("\x00")]
            self.symbols.append ((name, sym[0], sym[1], sym[2], sym[3]))

        ### Header[5]
        file.seek (self.off_64_count_38 + self.pos)
        buf = file.read (self.count_38 * 7 * 4)
        self.hdr5_data = []
        for i in range (self.count_38):
            L = unpack ("<7I", buf[i*7*4:(i+1)*7*4])
            self.hdr5_data.append (L)

        ### Header[3,6,7]
        file.seek (self.off_secnameoffs + self.pos)
        buf = file.read (self.seccount * 4)
        offs = unpack ("<%dI" % self.seccount, buf)
        maxoff = max (offs)

        file.seek (self.off_secstrtab + self.pos)
        secstrtab = file.read (maxoff + 512)

        if self.off_secnameoffs + self.seccount * 4 <> self.off_sections:
            raise "Oh fuck", self.off_secnameoffs + self.seccount * 4, self.off_sections

        file.seek (self.off_sections + self.pos)
        buf = file.read (self.seccount * 8 * 4)
        L = unpack ("<%dI" % (self.seccount*8), buf)

        self.sections = []
        for i in range (self.seccount):
            Ls = L[i*8:(i+1)*8]
#            L5Cs = L5C[i*8:(i+1)*8]
            s = secstrtab[offs[i]:]
            name = s[:s.find ("\x00")]
            self.sections.append ((name, Ls))

    def extr(self, file, sectnr, path):
        f = open (path, "wb")
        sect = self.sections[sectnr]
        file.seek (sect[1][7])
        buf = file.read (sect[1][2])
        f.write (buf)
        f.close ()

    def pr (self, x):
        print "count_0C = %d (some kind of section count)" % self.count_0C
        print "xxxxx_10 = %08x" % self.xxxxx_10
        print "xff_size = %d" % self.xff_size
        print "xxxxx_18 = %08x" % self.xxxxx_18
        print "count_1C = %d" % self.count_1C
        print "xxxxx_20 = %08x" % self.xxxxx_20
        print "symcount = %d" % self.symcount
        print "xxxxx_28 = %08x" % self.xxxxx_28
        print "xxxxx_2C = %08x" % self.xxxxx_2C
        print "xxxxx_30 = %08x" % self.xxxxx_30
        print "xxxxx_34 = %08x" % self.xxxxx_34
        print "count_38 = %d" % self.count_38
        print "xxxxx_3C = %08x" % self.xxxxx_3C
        print "seccount = %d" % self.seccount
        print "xxxxx_44 = %08x" % self.xxxxx_44
        print "xxxxx_48 = %08x" % self.xxxxx_48

        print
        print "off_sign    = %08x %d" % (self.off_sign, self.off_sign)
        print "50_count_1C = %08x" % self.off_50_count_1C
        print "symbols1    = %08x" % self.off_symbols1
        print "symstrtab   = %08x" % self.off_symstrtab
        print "sections    = %08x" % self.off_sections
        print "symbols2    = %08x" % self.off_symbols2
        print "64_count_38 = %08x" % self.off_64_count_38
        print "secnameoffs = %08x" % self.off_secnameoffs
        print "secstrtab   = %08x" % self.off_secstrtab

        if x:
            i = 0
            print "\nSymbols"
            for sym in self.symbols:
                print " [%d] %s" % (i, sym[0])
                print "  %08x" % sym[1]
                print "  %08x" % sym[2]
                print "  %08x" % sym[3]
                print "  %08x" % sym[4]
                i = i + 1

            i = 0
            print "\nHdr0"
            x = unpack (("<%dI" % self.count_1C), self.data_50_count_1C)
            for i in range (self.count_1C):
                print " [%5d] %08x %d" % (i, x[i], x[i])
                i = i + 1

            i = 0
            print "\nHdr5"
            for x in self.hdr5_data:
                print " [%d]" % i
                for v in x:
                    print "  %08x" % v
                i = i + 1

        print "\nSections"
        i = 0
        for sec in self.sections:
            print " [%d] %s" % (i, sec[0])
            for j in range (8):
                if j == 2:
                    print "  %08x %d size" % (sec[1][j], sec[1][j])
                elif j == 7:
                    print "  %08x (%08x)" % (sec[1][j], sec[1][j] + self.off_sign)
                else:
                    print "  %08x" % sec[1][j]
            i = i + 1

if __name__ == "__main__":
    f = open (sys.argv[1], "rb")
    print sys.argv[1]
    x = XFF (f, 0*0x3fff3edb)
    x.pr (True)
##    x.extr (f, 4, "vutext")

##f.seek (0x4e2)
##s = f.read (0x73e-0x4e2-1)
##print len (s.split ("\x00"))

##f.seek (0x430a50)
##s = f.read (0x00431742-0x430a50-1)
##print len (s.split ("\x00"))
