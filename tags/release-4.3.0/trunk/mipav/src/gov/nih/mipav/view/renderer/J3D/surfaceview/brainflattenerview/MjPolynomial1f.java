package gov.nih.mipav.view.renderer.J3D.surfaceview.brainflattenerview;


/**
 * Limited implementation of a floating-point polynomial of 1 variable.
 */
public class MjPolynomial1f {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** DOCUMENT ME! */
    private static final float ms_fInvLog2 = 1.0f / (float) Math.log(2.0f);

    /** DOCUMENT ME! */
    private static final float ms_fLog10 = (float) Math.log(10.0f);

    /** DOCUMENT ME! */
    private static final float ms_fThird = 1.0f / 3.0f;

    /** DOCUMENT ME! */
    private static final float ms_fSqrt3 = (float) Math.sqrt(3.0f);

    /** DOCUMENT ME! */
    private static final float ms_fTwentySeventh = 1.0f / 27.0f;

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** DOCUMENT ME! */
    protected float[] m_afCoeff;

    /** DOCUMENT ME! */
    protected int m_iDegree;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Creates a new MjPolynomial1f object.
     */
    public MjPolynomial1f() {
        m_iDegree = -1;
        m_afCoeff = null;
    }

    /**
     * Creates a new MjPolynomial1f object.
     *
     * @param  iDegree  DOCUMENT ME!
     */
    public MjPolynomial1f(int iDegree) {

        if (iDegree >= 0) {
            m_iDegree = iDegree;
            m_afCoeff = new float[m_iDegree + 1];
        } else {

            // default creation
            m_iDegree = -1;
            m_afCoeff = null;
        }
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * Returns Float which is null if no root is found; otherwise contains the value of the root.
     *
     * @param   fXMin            DOCUMENT ME!
     * @param   fXMax            DOCUMENT ME!
     * @param   iDigitsAccuracy  DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    public Float bisection(float fXMin, float fXMax, int iDigitsAccuracy) {
        float fP0 = eval(fXMin);
        float fP1 = eval(fXMax);

        // check for endpoint zeros
        if (Math.abs(fP0) <= MjMathf.EPSILON) {
            return new Float(fXMin);
        }

        if (Math.abs(fP1) <= MjMathf.EPSILON) {
            return new Float(fXMax);
        }

        if ((fP0 * fP1) > 0.0f) {
            return null;
        }

        // determine number of iterations to get 'digits' accuracy.
        float fTmp0 = (float) Math.log(fXMax - fXMin);
        float fTmp1 = ((float) iDigitsAccuracy) * ms_fLog10;
        float fArg = (fTmp0 + fTmp1) * ms_fInvLog2;
        int iMaxIter = (int) Math.ceil(fArg);

        float fRoot = 0.0f;

        for (int i = 0; i < iMaxIter; i++) {
            fRoot = 0.5f * (fXMin + fXMax);

            float fP = eval(fRoot);

            if (Math.abs(fP) <= MjMathf.EPSILON) {
                return new Float(fRoot);
            }

            if ((fP * fP0) < 0.0f) {
                fXMax = fRoot;
                fP1 = fP;
            } else {
                fXMin = fRoot;
                fP0 = fP;
            }
        }

        return new Float(fRoot);
    }

    /**
     * evaluate this polynomial for the specified value.
     *
     * @param   fT  DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    public float eval(float fT) {
        assert (m_iDegree >= 0);

        float fResult = m_afCoeff[m_iDegree];

        for (int i = m_iDegree - 1; i >= 0; i--) {
            fResult *= fT;
            fResult += m_afCoeff[i];
        }

        return fResult;
    }

    /**
     * DOCUMENT ME!
     *
     * @param   i  DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    public float getCoeff(int i) {
        assert ((0 <= i) && (i <= m_iDegree));

        return m_afCoeff[i];
    }

    /**
     * DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    public int getDegree() {
        return m_iDegree;
    }

    /**
     * return new instance which is the derivative of this instance.
     *
     * @return  DOCUMENT ME!
     */
    public MjPolynomial1f getDerivative() {
        MjPolynomial1f kDeriv;

        if (m_iDegree > 0) {
            kDeriv = new MjPolynomial1f(m_iDegree - 1);

            for (int i0 = 0, i1 = 1; i0 < m_iDegree; i0++, i1++) {
                kDeriv.m_afCoeff[i0] = i1 * m_afCoeff[i1];
            }
        } else if (m_iDegree == 1) {
            kDeriv = new MjPolynomial1f(0);
            kDeriv.m_afCoeff[0] = 0.0f;
        } else {
            kDeriv = new MjPolynomial1f(); // invalid in, invalid out
        }

        return kDeriv;
    }

    /**
     * Called by computeRadius, finds the root of the <= 8 degree polynomial through bisection method:
     *
     * @return  DOCUMENT ME!
     */
    public float getRootBisection() {

        /* Bound a root near zero and apply bisection to find it. */
        float fTMin = 0.0f, fFMin = eval(fTMin);
        float fTMax = 1.0f, fFMax = eval(fTMax);
        assert ((fFMin > 0.0f) && (fFMax < 0.0f));

        /* Determine the number of iterations to get 6 of accuracy. */
        float fTmp0 = (float) Math.log(fTMax - fTMin);
        float fTmp1 = 6.0f * ms_fLog10;
        float fArg = (fTmp0 + fTmp1) * ms_fInvLog2;
        int iMaxIter = (int) Math.ceil(fArg);

        /* Do the bisection. */
        float fTMid, fFMid;
        fTMid = 0.5f * (fTMin + fTMax);
        fFMid = eval(fTMid);

        for (int i = 0; i < iMaxIter; i++) {
            fTMid = 0.5f * (fTMin + fTMax);
            fFMid = eval(fTMid);

            float fProduct = fFMid * fFMin;

            if (fProduct < 0.0f) {
                fTMax = fTMid;
                fFMax = fFMid;
            } else {
                fTMin = fTMid;
                fFMin = fFMid;
            }
        }

        float fRadius = (float) Math.sqrt((double) fTMid);

        return fRadius;
    }

    /**
     * afRoot should have length 2; returns number roots found, which are returned in the afRoot array.
     *
     * @param   fXMin            DOCUMENT ME!
     * @param   fXMax            DOCUMENT ME!
     * @param   afRoot           DOCUMENT ME!
     * @param   iDigitsAccuracy  DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    public int getRootsOnInterval(float fXMin, float fXMax, float[] afRoot, int iDigitsAccuracy) {
        int iCount;
        Float kRoot;

        if (m_iDegree == 1) {
            kRoot = bisection(fXMin, fXMax, iDigitsAccuracy);

            if (null != kRoot) {
                iCount = 1;
                afRoot[0] = kRoot.floatValue();
            } else {
                iCount = 0;
            }

            return iCount;
        }

        // get roots of derivative polynomial
        MjPolynomial1f kDeriv = getDerivative();
        iCount = kDeriv.getRootsOnInterval(fXMin, fXMax, afRoot, iDigitsAccuracy);

        int iNewCount = 0;
        float[] afNewRoot = new float[iCount + 1];

        if (iCount > 0) {

            // find root on [xmin,root[0]]
            kRoot = bisection(fXMin, afRoot[0], iDigitsAccuracy);

            if (null != kRoot) {
                afNewRoot[iNewCount++] = kRoot.floatValue();
            }

            // find root on [root[i],root[i+1]] for 0 <= i <= count-2
            for (int i = 0; i <= (iCount - 2); i++) {
                kRoot = bisection(afRoot[i], afRoot[i + 1], iDigitsAccuracy);

                if (null != kRoot) {
                    afNewRoot[iNewCount++] = kRoot.floatValue();
                }
            }

            // find root on [root[count-1],xmax]
            kRoot = bisection(afRoot[iCount - 1], fXMax, iDigitsAccuracy);

            if (null != kRoot) {
                afNewRoot[iNewCount++] = kRoot.floatValue();
            }
        } else {

            // polynomial is monotone on [xmin,xmax], has at most one root
            kRoot = bisection(fXMin, fXMax, iDigitsAccuracy);

            if (null != kRoot) {
                afNewRoot[iNewCount++] = kRoot.floatValue();
            }
        }

        // copy to old buffer
        if (iNewCount > 0) {
            iCount = 1;
            afRoot[0] = afNewRoot[0];

            for (int i = 1; i < iNewCount; i++) {

                if (Math.abs(afNewRoot[i] - afNewRoot[i - 1]) > MjMathf.EPSILON) {
                    afRoot[iCount++] = afNewRoot[i];
                }
            }
        } else {
            iCount = 0;
        }

        return iCount;
    }

    /**
     * set this polynomial to the product of two polynomials.
     *
     * @param  kPoly1  DOCUMENT ME!
     * @param  kPoly2  DOCUMENT ME!
     */
    public void mul(MjPolynomial1f kPoly1, MjPolynomial1f kPoly2) {
        assert ((kPoly1.m_iDegree >= 0) && (kPoly2.m_iDegree >= 0));

        MjPolynomial1f kProd = new MjPolynomial1f(kPoly1.m_iDegree + kPoly2.m_iDegree);

        for (int i = 0; i <= kProd.m_iDegree; i++) {
            kProd.m_afCoeff[i] = 0.0f;
        }

        for (int i1 = 0; i1 <= kPoly1.m_iDegree; i1++) {

            for (int i2 = 0; i2 <= kPoly2.m_iDegree; i2++) {
                kProd.m_afCoeff[i1 + i2] += kPoly1.m_afCoeff[i1] * kPoly2.m_afCoeff[i2];
            }
        }

        set(kProd);
    }

    /**
     * afRoot should have length 3 which is used to return the real roots found; returns 0 if no root can be found,
     * otherwise returns the number of real roots found (1 or 3);
     *
     * @param   afRoot  DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    public int rootsDegree3(float[] afRoot) {

        // compute real roots to x^3+c[2]*x^2+c[1]*x+c[0] = 0
        if (m_iDegree != 3) {
            return 0;
        }

        // make polynomial monic
        float[] afCoeff = { m_afCoeff[0], m_afCoeff[1], m_afCoeff[2] };

        if (m_afCoeff[3] != 1.0f) {
            float fInv = 1.0f / m_afCoeff[3];
            afCoeff[0] *= fInv;
            afCoeff[1] *= fInv;
            afCoeff[2] *= fInv;
        }

        // convert to y^3+a*y+b = 0 by x = y-c[2]/3 and
        float fA = ms_fThird * ((3.0f * afCoeff[1]) - (afCoeff[2] * afCoeff[2]));
        float fB = ms_fTwentySeventh *
                       ((2.0f * afCoeff[2] * afCoeff[2] * afCoeff[2]) - (9.0f * afCoeff[1] * afCoeff[2]) +
                            (27.0f * afCoeff[0]));
        float fOffset = ms_fThird * afCoeff[2];

        float fDiscr = (0.25f * fB * fB) + (ms_fTwentySeventh * fA * fA * fA);

        if (Math.abs(fDiscr) <= MjMathf.EPSILON) {
            fDiscr = 0.0f;
        }

        int iCount;
        float fHalfB = 0.5f * fB;

        if (fDiscr > 0.0f) // 1 real, 2 complex roots
        {
            fDiscr = (float) Math.sqrt(fDiscr);

            float fTemp = -fHalfB + fDiscr;

            if (fTemp >= 0.0f) {
                afRoot[0] = (float) Math.pow(fTemp, ms_fThird);
            } else {
                afRoot[0] = -(float) Math.pow(-fTemp, ms_fThird);
            }

            fTemp = -fHalfB - fDiscr;

            if (fTemp >= 0.0f) {
                afRoot[0] += Math.pow(fTemp, ms_fThird);
            } else {
                afRoot[0] -= Math.pow(-fTemp, ms_fThird);
            }

            afRoot[0] -= fOffset;
            iCount = 1;
        } else if (fDiscr < 0.0f) {
            float fDist = (float) Math.sqrt(-ms_fThird * fA);
            float fAngle = ms_fThird * (float) Math.atan2(Math.sqrt(-fDiscr), -fHalfB);
            float fCos = (float) Math.cos(fAngle);
            float fSin = (float) Math.sin(fAngle);
            afRoot[0] = (2.0f * fDist * fCos) - fOffset;
            afRoot[1] = (-fDist * (fCos + (ms_fSqrt3 * fSin))) - fOffset;
            afRoot[2] = (-fDist * (fCos - (ms_fSqrt3 * fSin))) - fOffset;
            iCount = 3;
        } else {
            float fTemp;

            if (fHalfB >= 0.0f) {
                fTemp = -(float) Math.pow(fHalfB, ms_fThird);
            } else {
                fTemp = (float) Math.pow(-fHalfB, ms_fThird);
            }

            afRoot[0] = (2.0f * fTemp) - fOffset;
            afRoot[1] = -fTemp - fOffset;
            afRoot[2] = afRoot[1];
            iCount = 3;
        }

        return iCount;
    }

    /**
     * sets this instance to the scalar product with itself.
     *
     * @param  fScalar  DOCUMENT ME!
     */
    public void scale(float fScalar) {
        assert (m_iDegree >= 0);

        for (int i = 0; i <= m_iDegree; i++) {
            m_afCoeff[i] *= fScalar;
        }
    }

    /**
     * set this polynomial to have the same coefficients as those in the specified polynomial.
     *
     * @param  kPoly  DOCUMENT ME!
     */
    public void set(MjPolynomial1f kPoly) {
        m_iDegree = kPoly.m_iDegree;
        m_afCoeff = new float[m_iDegree + 1];
        System.arraycopy(kPoly.m_afCoeff, 0, this.m_afCoeff, 0, m_iDegree + 1);
    }

    /**
     * DOCUMENT ME!
     *
     * @param  i       DOCUMENT ME!
     * @param  fValue  DOCUMENT ME!
     */
    public void setCoeff(int i, float fValue) {
        assert ((0 <= i) && (i <= m_iDegree));
        m_afCoeff[i] = fValue;
    }

    /**
     * sets this instance to the difference of two polynomial instances.
     *
     * @param  kPoly1  DOCUMENT ME!
     * @param  kPoly2  DOCUMENT ME!
     */
    public void sub(MjPolynomial1f kPoly1, MjPolynomial1f kPoly2) {
        assert ((kPoly1.m_iDegree >= 0) && (kPoly2.m_iDegree >= 0));

        MjPolynomial1f kDiff;

        if (kPoly1.m_iDegree > kPoly2.m_iDegree) {
            kDiff = new MjPolynomial1f(kPoly1.m_iDegree);

            for (int i = 0; i <= kPoly2.m_iDegree; i++) {
                kDiff.m_afCoeff[i] = kPoly1.m_afCoeff[i] - kPoly2.m_afCoeff[i];
            }

            for (int i = kPoly2.m_iDegree + 1; i <= kPoly1.m_iDegree; i++) {
                kDiff.m_afCoeff[i] = kPoly1.m_afCoeff[i];
            }
        } else {
            kDiff = new MjPolynomial1f(kPoly2.m_iDegree);

            for (int i = 0; i <= kPoly1.m_iDegree; i++) {
                kDiff.m_afCoeff[i] = kPoly1.m_afCoeff[i] - kPoly2.m_afCoeff[i];
            }

            for (int i = kPoly1.m_iDegree + 1; i <= kPoly2.m_iDegree; i++) {
                kDiff.m_afCoeff[i] = -kPoly2.m_afCoeff[i];
            }
        }

        set(kDiff);
    }
}
