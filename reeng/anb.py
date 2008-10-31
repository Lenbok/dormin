#!/usr/bin/python
from struct import unpack
import xff2, sys, math

def cstr (s, p):
    return s[p:p+s[p:].find ("\x00")]

def sub1 (s, p, n):
    return s[p:p+n]

def sub4 (s, p, n):
    return s[p:p+n*4]

def magn (F):
    return math.sqrt (reduce (lambda a,b:b*b+a, F,0))

def bits (b, l):
    return reduce(lambda s,v:str(v)+s,[(b>>i)&1for i in range(l)],"")

def bits1 (d, s, l):
    return reduce(lambda s,v:str(v)+s,[(d>>(i+s))&1for i in range(l)],"")

def bit9 (d, s, l):
    if l == 9:
        lut = [-256, +128, +64, +32, +16, +8, +4, +2, +1]
        a = 0
        for i in range (l):
            if d & (1 << (s+i)):
                a += lut[8-i]
    else:
        lut = [-16, +8, +4, +2, +1]
        a = 0
        for i in range (l):
            if d & (1 << (s+i)):
                a += lut[4-i]
    return a+(1<<(l-1))
#    return (a + (1 << (l-1))) / float (1 << l)
#   return (a) / float (1 << (l))


def bytes(D):
    b0 = (D >> 24) & 0xff
    b1 = (D >> 16) & 0xff
    b2 = (D >> 8) & 0xff
    b3 = (D) & 0xff
    return (b0, b1, b2, b3)

def signb (b):
    if b & 0x80:
        b -= 0x100
    return b

def signbs (D):
    return map (signb, bytes (D))

def flts (D):
    sbs = signbs (D)
    return map (lambda b: b, sbs)

def strfs (F):
    return "[" + ", ".join (map (lambda f: "% f" % f, F)) + "]"

class ANB:
    def readsome (self):
        data = self.data[self.xff.off_sign+17*4:]
        self.some = []
        x = self.xff.off_sign+17*4
        print "hdr at %08x" % (x + self.roff)
        while True:
            D = unpack ("<2I", data[:8])
            if D[1] <> 0x102:
                print "%08x %08x" % (D[0], D[1])
                break
            data = data[8:]
            self.some.append ((x + self.roff, D[0]))
            x += 8

        print "hdr at %08x" % (x + self.roff)
        self.some1 = []
        for _ in self.some:
            D = unpack ("<2I", data[:8])
            if D[1] <> 0:
                print "%08x %08x" % (D[0], D[1])
                break
            data = data[8:]
            self.some1.append ((x + self.roff, D[0]))
            x += 8

    def __init__ (self, file, xff):
        self.xff = xff
        rodata = xff.sections[1]
        self.rodata = rodata
        self.roff = rodata[1][7]
        file.seek (self.roff)
        data = file.read ()#rodata[1][2])
        self.data = data
        self.D = unpack ("<17I", data[self.xff.off_sign:self.xff.off_sign+17*4])

        self.bone_count = self.D[5]
        self.boneD = unpack ("<%dI" % self.bone_count,
                             sub4 (data, self.D[4], self.bone_count))
        self.boneB = unpack ("<%dB" % (self.bone_count + 3),
                             sub1 (data, self.D[2], self.bone_count + 3))
        self.readsome ()

#        F = unpack ("<4f", data[0x13c:0x14c])
#        print "magn %f" % magn (F)

    def bone (self, n):
        b = self.boneB[n]
        d = self.boneD[n]
        data = self.data
        incr = 0
        print "_"*70
        print "[%2d] %d %08x %08x" % (n, b, d, d + self.roff)
        if b == 0:
            F = unpack ("<4f", data[d:d+16])
            print " magn % f" % magn (F)
            incr = 16
            i = 0
            for f in F:
                print " (%d) % 13.13f (% e)" % (i, f, f)
                i += 1
        elif b == 13:
            F = unpack ("<5f", data[d:d+20])
            print " magn % f" % magn (F[2:])
            i = 0
            for f in F:
                print " (%d) % 13.13f (% e)" % (i, f, f)
                i += 1
            incr = 20
        elif b == 12:
            F = unpack ("<6f", data[d:d+24])
            for i, f in enumerate (F):
                print " (%d) % 13.13f (% e)" % (i, f, f)
            incr = 24
        elif b == 2:
            raise "MOO"
        else:
            F = unpack ("<8f", data[d:d+32])
            print " magn % f" % magn (F[:])
            for i, f in enumerate (F):
                if f == 0.0:
                    sys.stderr.write ("fuck it %d\n" % n)
                print " (%d) % 13.13f (% e)" % (i, f, f)
            f = math.fabs (F[4]) + math.fabs (F[5]) + math.fabs (F[6]) + math.fabs (F[7])
            print " add % f" % f
            incr = 32

        print " end at %08x %08x" % (d+incr, d+incr+self.roff)
        if b:
            buf = data[d+incr:d+incr+4]
            d1 = unpack ("<I", buf)[0]
            print " %08x %08x %d" % (d1, d1 + self.roff, d1)
            count = self.D[1] & 0xffff
            pos = d1
        else:
            pos = d+incr

        if b == 13:
            subd = data[d1:d1+count*2]
            D = unpack ("<%dH" % count, subd)
            for i in range (count):
                print "  [%2d] %04x %d" % (i, D[i], D[i])
            pos += count*2
        elif b == 12:
            subd = data[d1:d1+count*6]
            for i in range (count):
                data = unpack ("<3H", subd[:6])
                subd = subd[6:]
                print "  [%2d] %04x %04x %04x" % (i, data[0], data[1], data[2])
            pos += count*6
        elif b == 6 or b == 3 or b == 4 or b == 5:
            if False:
                subd = data[d1:d1+count*4]
                D = unpack ("<%dI" % count, subd)
                for i in range (count):
                    d = D[i]
                    w = d & ((1<<9)-1)
                    z = (d>>9) & ((1<<9)-1)
                    y = (d>>18) & ((1<<9)-1)
                    x = (d>>27)
                    if b == 3:
                        starts = [27, 18, 9, 0]
                        lengths = [5, 9, 9, 9]
                    elif b == 4:
                        starts = [23, 18, 9, 0]
                        lengths = [9, 5, 9, 9]
                    elif b == 5:
                        starts = [23, 14, 9, 0]
                        lengths = [9, 9, 5, 9]
                    elif b == 6:
                        starts = [23, 14, 5, 0]
                        lengths = [9, 9, 9, 5]

    ##                print "  [%2d] %08x %10d (%s %s %s %s)" % (i, D[i], D[i],
    ##                                                           bits (x, 5),
    ##                                                           bits (y, 9),
    ##                                                           bits (z, 9),
    ##                                                           bits (w, 9))
    #                print "                           %s" % bits (D[i], 32)
    #                print "  [%2d] %08x %10d (% 4d, % 4d, % 4d, % 4d) (%s %s %s %s)" % \
                    x = bit9 (D[i], starts[0], lengths[0])
                    y = bit9 (D[i], starts[1], lengths[1])
                    z = bit9 (D[i], starts[2], lengths[2])
                    w = bit9 (D[i], starts[3], lengths[3])

    #                print "  [%2d] %08x %10d (% f, % f, % f, % f: %f) (%s %s %s %s)" % \
                    print "  [%2d] %08x %10d (%4d, %4d, %4d, %4d) (%s %s %s %s)" % \
                          (i, D[i], D[i], x, y, z, w,
                           bits1 (D[i], starts[0], lengths[0]),
                           bits1 (D[i], starts[1], lengths[1]),
                           bits1 (D[i], starts[2], lengths[2]),
                           bits1 (D[i], starts[3], lengths[3]))
            pos += count*4
        elif b == 0:
            pass
        else:
            subd = data[d1:d1+count*4]
            D = unpack ("<%dI" % count, subd)
            F = unpack ("<%df" % count, subd)
            for i in range (count):
                print "  [%2d] %08x %f %d" % (i, D[i], F[i], D[i])
            pos += count*4

        print " end2 at %08x %08x" % (pos, pos + self.roff)

    def proff (self, n, s):
        d = self.D[n]
        print "D[%2d] %08x %08x %d (%s)" % (n, d, d+self.roff, d, s)

    def pr (self):
        print "roff %08x" % self.roff
        print "off_sign %08x" % self.xff.off_sign
        print "at %08x" % (self.roff + self.xff.off_sign)
        print "D[ 0] %08x" % self.D[0]
        print "D[ 1] %08x" % self.D[1]
        self.proff (2, "per bone byte")
        for i in range (3, 4):
            d = self.D[i]
            print "D[%2d] %08x %08x %d" % (i, d, d+self.roff, d)

        self.proff (4, "per bone dword")
        print "D[ 5] %d (bone count)" % self.D[5]

        if self.xff.off_sign != self.D[10]:
            print "D[10] !!! %08x %d" % (self.D[10], self.D[10])

#        print "%d" % (self.D[10] - self.D[7])

        for i in range (6, 9) + range (10, 17):
            d = self.D[i]
            print "D[%2d] %08x %08x %d" % (i, d, d+self.roff, d)

        for i in range (self.bone_count):
            d = self.boneD[i]
            if i == self.bone_count - 1:
                diff = "????"
            else:
                diff = "%4d" % (self.boneD[i+1] - d)
            print "boneD[%2d] %08x %08x %4d(%s)" % (i, d, d + self.roff, d, diff),
            b = self.boneB[i]
            print "FLAG %3d %02x" % (b, b)

        for i in range (len (self.some)):
            (off, d) = self.some[i]
            x = unpack ('<I', self.data[d:d+4])[0]
            print "[%2d] %08x %08x (%04x) |" % (i, d, d + self.roff, x),
            (off, d) = self.some1[i]
            print "[%2d] %08x %08x" % (i, d, d + self.roff)

        for i in range (self.bone_count):
            self.bone (i)

print sys.argv[1]
f = open (sys.argv[1], "rb")
x = xff2.XFF (f, 0)
a = ANB (f, x)
a.pr ()
