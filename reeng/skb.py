#!/usr/bin/python
from struct import unpack
import xff2, sys, math

def cstr (s, p):
    return s[p:p+s[p:].find ("\x00")]

def subs (s, p, c):
    return s[p:p+c]

minmax = [["none", "none", -1e9, 1e9],
          ["none", "none", -1e9, 1e9],
          ["none", "none", -1e9, 1e9],
          ["none", "none", -1e9, 1e9],
          ["none", "none", -1e9, 1e9],
          ["none", "none", -1e9, 1e9],
          ["none", "none", -1e9, 1e9],
          ["none", "none", -1e9, 1e9],
          ["none", "none", -1e9, 1e9]]

def prminmax ():
    j = 0
    for (nmax, nmin, vmax, vmin) in minmax:
        print "[%d] min % f %-15s" % (j, vmin, nmin),
        print "max % f %s" % (vmax, nmax)
        j += 1

def magn (F):
    return math.sqrt (reduce (lambda a,b:b*b+a, F,0))
##    x = F[0]
##    y = F[1]
##    z = F[2]
##    return math.sqrt (x*x+y*y+z*z)

class SKB:
    def rt (self, pos, t):
        buf = self.data[pos:pos+64]
        D1 = unpack ("<3I", buf[0:12])
        F1 = unpack ("<9f", buf[12:48])
        if t:
            H1 = unpack ("<6H", buf[48:60])
        else:
            H1 = unpack ("<3i", buf[48:60])
        F2 = unpack ("<f", buf[60:64])
        return (D1, F1, H1, F2)

    def rt1 (self, data, tup):
        t1 = self.rt (tup[0], False)
        (D, F, _, _) = t1
        name = cstr (self.data[self.strtab1:], D[0])
        for j in range (9):
            if F[j] > minmax[j][2]:
                minmax[j][2] = F[j]
                minmax[j][0] = name
            if F[j] < minmax[j][3]:
                minmax[j][3] = F[j]
                minmax[j][1] = name

        self.t1.append (t1)

    def rt2 (self, n):
        pos = self.off2 + n*64
        self.t2.append (self.rt (pos, True))

    def __init__ (self, file, xff):
        rodata = xff.sections[1]
        self.rodata = rodata
        self.roff = rodata[1][7]
        file.seek (self.roff)
        data = file.read ()#rodata[1][2])
        self.data = data

        print "hdr at %08x" % (xff.off_sign + self.roff)
        hdr = data[xff.off_sign:]

        D1 = unpack ("<4I", hdr[:16])
        D2 = unpack ("<3I", hdr[32:44])

        self.one = D1[0]
        self.count1 = D1[1]
        self.off1 = D1[2]
        self.strtab1 = D1[3]

        self.count2 = D2[0]
        self.off2 = D2[1]
        self.strtab2 = D2[2]

        self.data1 = []
        pos = 44
        print "%d" % len (data)
        for i in range (self.count1 + 3):
            tup = unpack ("<2I", hdr[pos:pos+8])
            self.data1.append (tup)
            pos += 8

        self.Unk1 = unpack ("<2I4fI", subs (data, self.data1[self.count1][0], 7*4))
        self.Unk2 = unpack ("<I", subs (data, self.data1[self.count1 + 1][0], 4))[0]
        self.Unk3 = unpack ("<I", subs (data, self.data1[self.count1 + 2][0], 4))[0]

        self.t1 = []
        for i in range (self.count1):
            self.rt1 (data, self.data1[i])

        self.t2 = []
        for i in range (self.count2):
            self.rt2 (i)

        self.names = []
        for i in range (self.count1):
            tup = unpack ("<II", hdr[pos:pos+8])
            if tup[1] <> 0:
                raise "Whoopsie daisy", hex (pos + self.roff)
            name = cstr (data, tup[0] + self.strtab1)
            self.names.append (name)
            pos += 8

    def prt (self, t, taboff, defval):
        i = 0
        print
        for e in t:
            D1, F1, H1, F2 = e
            print "[%2d] %s at %08x" % (i,
                                        cstr (self.data[taboff:], D1[0]),
                                        self.data1[i][0] + self.roff)

            if D1[1] <> defval:
                print "!D1[1] %08x (%d)" % (D1[1], D1[1])
            if D1[2] <> defval:
                print "!D1[2] %08x (%d)" % (D1[2], D1[2])

            print " V0    (% f, % f, % f)" % (F1[0], F1[1], F1[2])
            print " V1    (% f, % f, % f)" % (F1[3], F1[4], F1[5])
            print " V2    (% f, % f, % f)" % (F1[6], F1[7], F1[8])

            l13 = magn (F1[1:4])
#            l57 = magn (F1[5:8])
#            l68 = magn (F1[6:])
            l58 = magn (F1[5:])
            print " %f %f" % (l13, l58)

            if defval == 0:
                for j in range (6):
                    print " H1[%d] %04x (%d)" % (j, H1[j], H1[j])
            else:
                for j in range (3):
                    print " H1[%d] % d" % (j, H1[j])

            if F2[0] <> 0.0:
                print "!F2    % f" % F2[0]
            print
            i += 1

    def pr (self):
        print "one    %d" % self.one

        print "count1  %d" % self.count1
        print "off1    %08x" % (self.off1 + self.roff)
        print "strtab1 %08x" % (self.strtab1 + self.roff)

        print "count2  %d" % self.count2
        print "off2    %08x" % (self.off2 + self.roff)
        print "strtab2 %08x" % (self.strtab2 + self.roff)

        print "Unk1[0] %08x(%08x) %d" % (self.Unk1[0],
                                         self.Unk1[0] + self.roff,
                                         self.Unk1[0])
        print "    [1] %08x %d" % (self.Unk1[1], self.Unk1[1])
        for j in range (4):
            print "    [%d] % f" % (j+2, self.Unk1[j+2])
        print "    [6] %08x(%08x) %d" % (self.Unk1[6],
                                         self.Unk1[6] + self.roff,
                                         self.Unk1[6])

#        for f in self.F4:
#            print f

        i = 0
        for elem1 in self.data1:
            off = elem1[0] + self.roff
            if i < len (self.names):
                name = self.names[i]
            else:
                name = ""
            print "[%2d] %-15s %08x(%08x), %08x(%d)" % (i, name,
                                                        elem1[0], off, elem1[1], elem1[1])
            i += 1

        self.prt (self.t1, self.strtab1, 0xffffffff)
        self.prt (self.t2, self.strtab2, 0x00000000)

print sys.argv[1]
f = open (sys.argv[1], "rb")
x = xff2.XFF (f, 0)
s = SKB (f, x)
s.pr ()
prminmax ()
