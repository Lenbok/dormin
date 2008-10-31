#!/usr/bin/env python
import xff2, sys, math
from struct import unpack

def cstr (s, p):
    return s[p:p+s[p:].find ("\x00")]

class SRF:
    def __init__ (self, ropos, pos, n, data):
        self.n = n
        self.ropos = ropos
        spos = pos + n * 288
        self.spos = spos
        srf = data[spos:spos+288]
        if srf[:4] <> "SRF\x00":
            raise "sourface %d at %08x, bad signature" % (n, spos + ropos), srf[:4]

        self.D04_10 = unpack ("<3I", srf[4:0x10])
        self.tricount = self.D04_10[0]
        self.stripcount = self.D04_10[1]
        self.nameoff = self.D04_10[2]
        self.hdr1 = []
        for i in range (3):
            stru = unpack ("<4BfII", srf[0x10+i*16:0x10+(i+1)*16])
            self.hdr1.append (stru)

        self.hdr2 = unpack ("<I2f5I", srf[0x40:0x60])
        self.hdr3 = unpack ("<48I", srf[0x60:288])
        self.rodata = data

    def pr (self):
        print " [%d] %s (%08x)" % (self.n, cstr (self.rodata, self.nameoff),
                                   self.ropos + self.spos)
        print "   triangle count %d" % self.tricount
        print "   strip    count %d" % self.stripcount
#        print " %s" % cstr (self.rodata, self.D04_10[2])
#        print " %08x %d" % (self.D04_10[2], self.D04_10[2])
#        print " | %08x %d" % (self.D04_10[2] + self.ropos, self.D04_10[2] + self.ropos)

        i = 0
        for stru in self.hdr1:
            print "   Stru %d" % i
            print "     %d, %d, %d, %d" % (stru[0], stru[1], stru[2], stru[3])
            print "     %f" % stru[4]
            print "     %08x %d" % (stru[5], stru[5])
            print "     %08x %d" % (stru[6], stru[6])
            i += 1

        j = 0
        print "   Hdr2"
        for v in self.hdr2:
            if j == 1 or j == 2:
                print "    [%d] % f" % (j, v)
            else:
                print "    [%d] %08x %d" % (j, v, v)
            j += 1

        if 1==1:
            print " Hdr3"
            j = 0
            for v in self.hdr3:
                print "  [%d] %08x %d" % (j, v, v)
                j += 1

class HDR3:
    def __init__ (self, pos, data):
        buf = data[pos:pos+32]
        D = unpack ("<8I", buf)
        self.size = D[0]
        self.unk_D_04 = D[1]
        self.surf = D[2]
        self.unk_D_0C = D[3]
        self.offs = D[4]
        self.tricount = D[5]
        self.stripcount = D[6]
        self.unk_D_1C = D[7]

    def pr (self, ropos):
        print "  at %08x size %d (ends at %08x)" % (self.offs + ropos,
                                                    self.size,
                                                    self.offs + self.size + ropos)
        print "  surface        %d" % self.surf
        print "  triangle count %d" % self.tricount
        print "  strip    count %d" % self.stripcount
        if self.unk_D_04:
            print "  !!! unknown at 04 = %08x %d" % (self.unk_D_04, self.unk_D_04)
        print "  unknown at 0C = %08x %d" % (self.unk_D_0C, self.unk_D_0C)
        if self.unk_D_1C:
            print "  !!! unknown at 1c = %08x %d" % (self.unk_D_1C, self.unk_D_1C)

class NMO:
    def geom (self, ropos, count):
        pos = 0
        for surf in range (count):
            print
            print "Surface %d %08x:" % (surf, pos + ropos)

            buf = self.data
            bytes = unpack ("<12B", buf[:12])
            if bytes[2] <> 0 or bytes[6] <> 0 or bytes[10] <> 0:
                raise "Unexpected sequence at %08x %s" % (ropos + pos,
                                                          buf[:12].encode ("hex"))
            for b in bytes:
                print " %03x" % b,
            print
            for b in bytes:
                print " %3d" % b,
            print

            pos += 12

            simple = True
            while simple:
                print
                print " %08x:" % (pos + ropos)
                bytes = unpack ("<4B", buf[pos:pos+4])

                index = bytes[0]
                mask = bytes[1]
                count = bytes[2]
                kind = bytes[3]

                if kind == 0x6c:
                    print "="*80
                print " %d %02x count=%d %02x" % (index, mask, count, kind),
                if kind >> 4 <> 0 and mask <> 0x80:
                    print " (NON 80 MASK)"
                else:
                    print

                pos += 4
                if kind == 0x6c:
                    for i in range (count):
                        if index == 0:
                            t = unpack ("<BBH3I",buf[pos:pos+16])
                            print "     (%04x(%d) %02x %d)" % (t[2], t[2],
                                                              t[1], t[0])
                            print " %08x %d" % (t[3], t[3])
                            print " %08x %d" % (t[4], t[4])
                            print " %08x %d" % (t[5], t[5])
                            pos += 16
                        else:
                            t = unpack ("<3fI",buf[pos:pos+16])
                            print " | %f" % t[0]
                            print " | %f" % t[1]
                            print " | %f" % t[2]
                            print " %d" % (t[3])
                            pos += 16

                elif kind == 0x68:
                    for i in range (count):
                        F = unpack ("<3f", buf[pos:pos+12])
                        if index == 1:
                            print " v % f, % f, % f" % (F[0], F[1], F[2])
                        else:
                            Fn = math.sqrt (F[0]*F[0] + F[1]*F[1] + F[2]*F[2])
                            print " n % f, % f, % f -> %f" % (F[0], F[1], F[2], Fn)
                        pos += 12

                elif kind == 0x6d:
                    for i in range (count):
                        h = unpack ("<4h", buf[pos:pos+8])
                        H = unpack ("<4H", buf[pos:pos+8])
                        #print buf[pos:pos+8].encode ("hex")
                        print " %04x %04x %04x %04x | % 5d % 5d % 5d % 5d" % (H[0], H[1],
                                                                              H[2], H[3],
                                                                              h[0], h[1],
                                                                              h[2], h[3])
                        pos += 8

                elif kind == 0x05:
                    if mask != 0:
                        raise "5 with non zero mask?"

                elif kind == 0x6e:
                    for i in range (count):
                        B = unpack ("<4B", buf[pos:pos+4])
                        print " %02x %02x %02x %02x (%3d, %3d, %3d, %3d)" % (B[0], B[1],
                                                                             B[2], B[3],
                                                                             B[0], B[1],
                                                                             B[2], B[3])
                        pos += 4

                elif kind == 0x17:
                    if mask <> 0 or count <> 0 or index <> 0:
                        raise "0x17 with invalid stuff"

                elif kind == 0x65:
                    for i in range (count):
                        h = unpack ("<2h", buf[pos:pos+4])
                        H = unpack ("<2H", buf[pos:pos+4])
                        print " %04x %04x | % 5d % 5d" % (H[0], H[1], h[0], h[1])
                        pos += 4

                elif kind == 0x00 and index == 0x00 and mask == 0x00 and count == 0x00:
                    pos = (pos + 15) & ~15
                    print " POS %08x" % (pos+ropos)
                    simple = False

                else:
                    raise "Unknown kind", hex (kind)

    def __init__ (self, file, xff):
        rodata = xff.sections[1]
        self.rodata = rodata
        file.seek (rodata[1][7])
        data = file.read (rodata[1][2])
        self.data = data
        hdr = data[xff.off_sign:][:0x80]
        if hdr[:4] <> "NMO\x00":
            raise "bad signature", hdr[:4]

        self.D7 = unpack ("<7I", hdr[4:0x20])
        self.F4 = unpack ("<4f", hdr[0x20:0x30])
        self.D4 = unpack ("<4I", hdr[0x20:0x30])

        self.hdrs = []
        for i in range (5):
            s = hdr[0x30+i*0x10:0x30+(i+1)*0x10]
            u = unpack ("<4I", s)
            self.hdrs.append (u)

        self.data0 = data[self.hdrs[0][0]:self.hdrs[0][0]+144]

        self.texs = []
        for i in range (self.hdrs[1][1]):
            pos = self.hdrs[1][0] + i*32
            tdata = data[pos:pos+32]
            if tdata[:4] <> "TEX\x00":
                raise "bad tex object %d at %08x" % (i, pos), tdata[:4]
            stru = unpack ("<5I2H2H", tdata[4:0x20])
            nameoff = stru[0]
            name = cstr (data, nameoff)
            tex = (name,) + stru[1:]
            self.texs.append (tex)

        self.srfs = []
        for i in range (self.hdrs[2][1]):
            self.srfs.append (SRF (rodata[1][7], self.hdrs[2][0], i, data))

        self.hdr3 = []
        for i in range (self.hdrs[3][1]):
            pos = self.hdrs[3][0] + i*32
#            stru = unpack ("<8I", data[pos:pos+32])
            self.hdr3.append (HDR3 (pos, data))

        try:
            self.geom (rodata[1][7], self.hdrs[3][1] )
        except:
            print "Unexpected error:", sys.exc_info()
            pass

    def pr (self):
        i = 0
        for d in self.D7:
            print " [%d] %08x %d" % (i, d, d)
            i = i + 1

        i = 0
        for f in self.F4:
            print " [%d] %08x % f" % (i, self.D4[i], f)
            i = i + 1

        for i in range (5):
            u = self.hdrs[i]
            print "[%d]" % i
            print " %08x (%08x) offset" % (u[0], u[0] + self.rodata[1][7])
            print " %08x %d count" % (u[1], u[1])
            print " %08x" % u[2]
            print " %08x" % u[3]
            #if i == 0:
            #    print " %08x" % (u[0] + 144)
            if i == 4:
                print "Name = ",
                print cstr (self.data, u[0])

        print "Header 3"
        for i in range (len (self.hdr3)):
            hdr3 = self.hdr3[i]
            print " [%d]" % i,
            hdr3.pr (self.rodata[1][7])
##            v = self.hdr3[i]
##            for j in range (len (v)):
##                vv = v[j]
##                if j == 4:
##                    vvv = vv + self.rodata[1][7]
##                    print "  [%d] %08x %d %08x %d" % (j, vv, vv, vvv, vvv)
##                elif j == 0:
##                    vvv = v[4] + vv + self.rodata[1][7]
##                    print "  [%d] %08x %d %08x %d" % (j, vv, vv, vvv, vvv)
##                else:
##                    print "  [%d] %08x %d" % (j, vv, vv)

        print "Textures"
        nr = 0
        for tex in self.texs:
            print " [%4d] %s" % (nr, tex[0])
            for i in range (6):
                print "  %08x %d" % (tex[i+1], tex[i+1])
            print "  %dx%d" % (tex[7], tex[8])
            nr = nr + 1

        print "Sourfaces"
        for srf in self.srfs:
            srf.pr ()
            print

print sys.argv[1]
f = open (sys.argv[1], "rb")
x = xff2.XFF (f, 0)
n = NMO (f, x)
n.pr ()
#x.pr (False)
#rodata = x.sections[1]
