static void vneg (float *res, float *v)
{
    res[0] = -v[0];
    res[1] = -v[1];
    res[2] = -v[2];
}

static void vcopy (float *res, float *v)
{
    *res++ = *v++;
    *res++ = *v++;
    *res++ = *v++;
}

static void vaddto (float *v1, float *v2)
{
    v1[0] += v2[0];
    v1[1] += v2[1];
    v1[2] += v2[2];
}

static void vadd (float *res, float *v1, float *v2)
{
    res[0] = v1[0] + v2[0];
    res[1] = v1[1] + v2[1];
    res[2] = v1[2] + v2[2];
}

static void vsub (float *res, float *v1, float *v2)
{
    res[0] = v1[0] - v2[0];
    res[1] = v1[1] - v2[1];
    res[2] = v1[2] - v2[2];
}

static void qconjugate (float *res, float *q)
{
    vneg (res, q);
    res[3] = q[3];
}

static float qmagnitude (float *q)
{
    return sqrt (q[0]*q[0]+q[1]*q[1]+q[2]*q[2]+q[3]*q[3]);
}

static void qapply (float *res, float *q, float *v)
{
    float a = -q[3];
    float b = q[0];
    float c = q[1];
    float d = q[2];
    float v1 = v[0];
    float v2 = v[1];
    float v3 = v[2];
    float t2, t3, t4, t5, t6, t7, t8, t9, t10;
    t2 =   a*b;
    t3 =   a*c;
    t4 =   a*d;
    t5 =  -b*b;
    t6 =   b*c;
    t7 =   b*d;
    t8 =  -c*c;
    t9 =   c*d;
    t10 = -d*d;
    res[0] = 2*( (t8 + t10)*v1 + (t6 -  t4)*v2 + (t3 + t7)*v3 ) + v1;
    res[1] = 2*( (t4 +  t6)*v1 + (t5 + t10)*v2 + (t9 - t2)*v3 ) + v2;
    res[2] = 2*( (t7 -  t3)*v1 + (t2 +  t9)*v2 + (t5 + t8)*v3 ) + v3;
}

static void qscale (float *res, float *q, float scale)
{
    *res++ = *q++ * scale;
    *res++ = *q++ * scale;
    *res++ = *q++ * scale;
    *res++ = *q++ * scale;
}

static void qcompose (float *res, float *q1, float *q2)
{
    res[0] = q1[3]*q2[0] + q1[0]*q2[3] + q1[1]*q2[2] - q1[2]*q2[1];
    res[1] = q1[3]*q2[1] + q1[1]*q2[3] + q1[2]*q2[0] - q1[0]*q2[2];
    res[2] = q1[3]*q2[2] + q1[2]*q2[3] + q1[0]*q2[1] - q1[1]*q2[0];
    res[3] = q1[3]*q2[3] - q1[0]*q2[0] - q1[1]*q2[1] - q1[2]*q2[2];
}

static void q2matrix (float *mat, float *q, float *v)
{
    float X = q[0];
    float Y = q[1];
    float Z = q[2];
    float W = q[3];

    float xx      = X * X;
    float xy      = X * Y;
    float xz      = X * Z;
    float xw      = X * W;

    float yy      = Y * Y;
    float yz      = Y * Z;
    float yw      = Y * W;

    float zz      = Z * Z;
    float zw      = Z * W;

    mat[0]  = 1 - 2 * ( yy + zz );
    mat[1]  =     2 * ( xy - zw );
    mat[2]  =     2 * ( xz + yw );

    mat[4]  =     2 * ( xy + zw );
    mat[5]  = 1 - 2 * ( xx + zz );
    mat[6]  =     2 * ( yz - xw );

    mat[8]  =     2 * ( xz - yw );
    mat[9]  =     2 * ( yz + xw );
    mat[10] = 1 - 2 * ( xx + yy );

    mat[3] = v[0];
    mat[7] = v[1];
    mat[11] = v[2];
}

static void mscale (float *res, float *m, float s)
{
    int i;
    for (i = 0; i < 12; ++i) *res++ = *m++ * s;
}

static void mapply_to_point (float *res, float *m, float *v)
{
    float x = v[0];
    float y = v[1];
    float z = v[2];
    res[0] = x*m[0] + y*m[4] + z*m[8] + m[3];
    res[1] = x*m[1] + y*m[5] + z*m[9] + m[7];
    res[2] = x*m[2] + y*m[6] + z*m[10] + m[11];
}

static void mapply_to_vector (float *res, float *m, float *v)
{
    float x = v[0];
    float y = v[1];
    float z = v[2];
    res[0] = x*m[0] + y*m[4] + z*m[8];
    res[1] = x*m[1] + y*m[5] + z*m[9];
    res[2] = x*m[2] + y*m[6] + z*m[10];
}
