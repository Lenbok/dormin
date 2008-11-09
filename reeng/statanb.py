#!/usr/bin/env python

maxd = [(-1e9, "none", -1, -1) for i in range (8)]
mind = [( 1e9, "none", -1, -1) for i in range (8)]

import sys, math, os
from struct import unpack

class Continue:
    pass

def magn (F):
    return math.sqrt (reduce (lambda a,b:b*b+a, F,0))

def bitstr (b, l):
    # boy, am i proud for this...
    return reduce(lambda s,v:"01"[v]+s,[(b>>i)&1 for i in range(l)],"")

def makelut (i):
    bitindex = 32
    lut = []
    for j in range (4):
        bitlen = [9,5][j == i]
        bitindex -= bitlen
        lut.append ((bitindex, bitlen))
    return lut

bitlut = [makelut (i) for i in range (4)]

def kalms (val, len):
    a = 0
    if len == 9:
        lut = [-256, +128, +64, +32, +16, +8, +4, +2, +1]
        for i in range (len):
            if val & (1 << i):
                a += lut[8-i]
    else:
        lut = [-16, +8, +4, +2, +1]
        for i in range (len):
            if val & (1 << i):
                a += lut[4-i]

    return a

def notkalms (v, len):
    return v

kalms=notkalms

def fkalms (v, len):
    a = kalms (v, len)
    x = (a + 256) / 512.0
    y = math.acos (x) * 57.2957795
    return y

class ANB:
    bones = []

    def __init__ (self, file):
        def r32 (pos):
            file.seek (pos)
            buf = file.read (4)
            return unpack ("<I", buf)[0]

        hdrpos = r32 (19*4)
        tmppos = r32 (0x50 + 3*4)
        secpos = r32 (tmppos + 15*4)

        file.seek (secpos)
        data = file.read ()

        def r32 (pos):
            return unpack ("<I", data[pos:pos+4])[0]
        def r16 (pos):
            return unpack ("<H", data[pos:pos+2])[0]
        def r8 (pos):
            return unpack ("B", data[pos:pos+1])[0]

        bonecount = r32 (hdrpos + 20)

        tmp = r32 (hdrpos + 16)
        offs = unpack ("<%dI" % bonecount, data[tmp:tmp + bonecount*4])

        tmp = r32 (hdrpos + 8)
        flags = unpack ("%dB" % bonecount, data[tmp:tmp + bonecount])

        posecount = r16 (hdrpos + 4)

        def rbone (index):
            bonedata = data[offs[index]:]

            flag = flags[index]

            if flag == 0:
                quaternion = unpack ("<4f", bonedata[:16])
                bone = (quaternion, None)

            elif flag == 12:
                floats = unpack ("<6f", bonedata[:24])
                offset = unpack ("<I", bonedata[24:28])[0]
                subdata = data[offset:]
                payload = map (lambda k: unpack ("<3H", subdata[k*6:(k+1)*6]),
                               range (posecount))
                bone = (floats, payload)

            elif flag == 13:
                floats = unpack ("<5f", bonedata[:20])
                offset = unpack ("<I", bonedata[20:24])[0]
                subdata = data[offset:offset+posecount*2]
                payload = unpack ("<%dH" % posecount, subdata)
                bone = (floats, payload)

            else:                       # 3,4,5,6
                floats = unpack ("<8f", bonedata[:32])
                offset = unpack ("<I", bonedata[32:36])[0]
                subdata = data[offset:offset+posecount*4]
                payload = unpack ("<%dI" % posecount, subdata)
                bone = (floats, payload)

            return (flag, bone)

        self.bones = map (rbone, range (bonecount))
        self.posecount = posecount

    def zerotest (self, path):
        for i, (flag, (floats, payload)) in enumerate (self.bones):
            if flag in [3,4,5,6]:
                global maxd, mind

                for i in range (8):
                    if floats[i] > maxd[i][0]:
                        maxd[i] = (floats[i], path, i, flag)
                    if floats[i] < mind[i][0]:
                        mind[i] = (floats[i], path, i, flag)

    def twentytest (self, path):
        for i, (flag, (floats, payload)) in enumerate (self.bones):
            if flag == 12:
                global maxd, mind

                for i in range (6):
                    if floats[i] > maxd[i][0]:
                        maxd[i] = (floats[i], path, i, flag)
                    if floats[i] < mind[i][0]:
                        mind[i] = (floats[i], path, i, flag)

    def thirtytest (self, path):
        for i, (flag, (floats, payload)) in enumerate (self.bones):
            if flag == 13:
                global maxd, mind

                for i in range (5):
                    if floats[i] > maxd[i][0]:
                        maxd[i] = (floats[i], path, i, flag)
                    if floats[i] < mind[i][0]:
                        mind[i] = (floats[i], path, i, flag)

    def showbone (self, index, bone, below):
        (flag, (floats, payload)) = bone
        print "[%2d] %d bone%d" % (index, flag, index)
        for i, f in enumerate (floats):
            print " (%d) % .13f (% e)" % (i, f, f)

        if flag == 0:
            print "magnitude %f" % magn (floats)

        elif flag == 12:
            print
            for i, (x,y,z) in enumerate (payload):
                print " (%3d) %04x %04x %04x" % (i, x, y, z)

        elif flag == 13:
            print "degrees1(?) % f" % (180.0 * floats[0] / math.pi)
            print "degrees2(?) % f" % (180.0 * floats[1] / math.pi)
            print "magnitude %f" % magn (floats[2:])
            print
            for i, h in enumerate (payload):
                print " (%2d) %04x" % (i, h)

        else:                       # 3,4,5,6
            s1 = sum (floats[:4])
            s2 = sum (floats[4:])
            s3 = s1 + s2
#            print "% .13f %e" % (magn (floats[3:6]), magn (floats[3:6]))
#            print "% .13f % .13f % .13f\n% e, % e, % e" % (s1, s2, s3,
#                                                           s1, s2, s3)
            print
            lut = bitlut[flag - 3]
            for i, d in enumerate (payload):
                l = [((d >> b[0]) & ((1 << b[1]) - 1), b[1]) for b in lut]
                (x,xl), (y,yl), (z,zl), (w,wl) = l
                xs = bitstr (x, xl)
                ys = bitstr (y, yl)
                zs = bitstr (z, zl)
                ws = bitstr (w, wl)

                print " (%2d) %08x %s %s %s %s" % (i, d, xs, ys, zs, ws),
                which = 1
                if below:
                    print
                    print " "*14,
                    print "%*d %*d %*d %*d" % (xl, kalms (x, xl),
                                               yl, kalms (y, yl),
                                               zl, kalms (z, zl),
                                               wl, kalms (w, wl))
                elif which == 1:
                    print "% 4d % 4d % 4d % 4d" % (kalms (x, xl),
                                                   kalms (y, yl),
                                                   kalms (z, zl),
                                                   kalms (w, wl))
                elif which == 2:
                    fl = map (lambda (v, l): fkalms (v, l), l)
                    fl[0] = fl[0] * floats[4] + floats[0]
                    fl[1] = fl[1] * floats[5] + floats[1]
                    fl[2] = fl[2] * floats[6] + floats[2]
                    fl[3] = fl[3] * floats[7] + floats[3]
                    print "% f % f % f % f (%f)" % (fl[0], fl[1], fl[2], fl[3],
                                                    magn (fl))
                elif which == 0:
                    print
                else:
                    print "% f % f % f % f" % (fkalms2 (x, xl, floats),
                                               fkalms2 (y, yl, floats),
                                               fkalms2 (z, zl, floats),
                                               fkalms2 (w, wl, floats))

    def show (self, below = False):
        print "flags = %s" % str ([b[0] for b in self.bones])
        print "poses = %d" % self.posecount
        print
        for index, bone in enumerate (self.bones):
            self.showbone (index, bone, below)
            print '-'*70

def main ():
    def process (path):
        print path
        file = open (path, "rb")
        anb = ANB (file)
#        anb.zerotest (path)
#        anb.twentytest (path)
        anb.show ()
        file.close ()

    if len (sys.argv) < 2:
        if sys.stdin.isatty ():
            sys.stderr.write ("path please")
            sys.exit (1)
        else:
            for line in sys.stdin:
                path = line[:-1]
                if True:
                    file = open (path, "rb")
                    anb = ANB (file)
#                    anb.zerotest (path)
                    anb.twentytest (path)
#                    anb.thirtytest (path)
                    file.close ()
                else:
                    process (path)
    else:
        # shamelessly stolen from git
        (r,w) = os.pipe ()
        pid = os.fork ()
        if pid == 0:
            os.dup2 (w, 1)
            os.dup2 (w, 2)
            os.close (r)
            os.close (w)
            for path in sys.argv[1:]:
                process (path)
            return

        os.dup2 (r, 0)
        os.close (r)
        os.close (w)
        os.execl ("/bin/sh", "sh", "-c", os.getenv ("PAGER", default="less"))

if __name__ == "__main__":
    main ()
    def pr (i):
        (v, path, index, flag) = maxd[i]
        print "max %d: % f %s %d %d" % (i, v, path, index, flag)

        (v, path, index, flag) = mind[i]
        print "min %d: % f %s %d %d" % (i, v, path, index, flag)

    for i in range (8):
        pr (i)
