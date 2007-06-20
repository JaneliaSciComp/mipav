// Geometric Tools, Inc.
// http://www.geometrictools.com
// Copyright (c) 1998-2006.  All Rights Reserved
//
// The Wild Magic Version 4 Foundation Library source code is supplied
// under the terms of the license agreement
//     http://www.geometrictools.com/License/Wm4FoundationLicense.pdf
// and may not be copied or disclosed except in accordance with the terms
// of that agreement.
//
// Version: 4.0.0 (2006/06/28)

package gov.nih.mipav.view.WildMagic.LibFoundation.Mathematics;

/** The linear component is represented as P+t*D where P is the component
 * origin and D is a unit-length direction vector.  The user must ensure
 * that the direction vector satisfies this condition.  The t-intervals
 * for lines, rays, segments, points, or the empty set are described
 * later.
 */
public abstract class LinCompf
{

    /** component type */
    public enum ComponentType
    {
        CT_EMPTY (0),
        CT_POINT (1),
        CT_SEGMENT (2),
        CT_RAY (3),
        CT_LINE (4);
        private int m_iValue;
        ComponentType( int iValue ) { m_iValue = iValue; }
        public int Value() { return m_iValue; }

    };

    public ComponentType GetType ()
    {
        return m_iType;
    }

    /** The interval of restriction for t, as defined above.  The function
     * SetInterval(min,max) sets the t-interval; it handles all possible
     * inputs according to the following scheme:
     *   CT_LINE:
     *     [-MAX_REAL,MAX_REAL]
     *   CT_RAY:
     *     [min,MAX_REAL], where min is finite
     *     [-MAX_REAL,max], where max is finite
     *   CT_SEGMENT:
     *     [min,max], where min and max are finite with min < max
     *   CT_POINT:
     *     [min,max], where min and max are finite with min = max
     *   CT_EMPTY:
     *     [min,max], where min > max or min = max = MAX_REAL or
     *                min = max = -MAX_REAL
     */
    public void SetInterval (float fMin, float fMax)
    {
        m_iType = GetTypeFromInterval(fMin,fMax);
        m_fMin = fMin;
        m_fMax = fMax;
    }


    /** Determine the type of an interval without having to create an instance
     * of a LinComp object.
     */
    public static ComponentType GetTypeFromInterval (float fMin, float fMax)
    {
        if (fMin < fMax)
        {
            if (fMax == Float.MAX_VALUE)
            {
                if (fMin == Float.MIN_VALUE)
                {
                    return ComponentType.CT_LINE;
                }
                else
                {
                    return ComponentType.CT_RAY;
                }
            }
            else
            {
                if (fMin == Float.MIN_VALUE)
                {
                    return ComponentType.CT_RAY;
                }
                else
                {
                    return ComponentType.CT_SEGMENT;
                }
            }
        }
        else if (fMin == fMax)
        {
            if (fMin != Float.MIN_VALUE && fMax != Float.MAX_VALUE)
            {
                return ComponentType.CT_POINT;
            }
        }
        
        return ComponentType.CT_EMPTY;
    }


    /** The canonical intervals are [-MAX_REAL,MAX_REAL] for a line;
     * [0,MAX_REAL] for a ray; [-e,e] for a segment, where e > 0; [0,0] for
     * a point, and [MAX_REAL,-MAX_REAL] for the empty set.  If the interval
     * is [min,max], the adjustments are as follows.
     * 
     * CT_RAY:  If max is MAX_REAL and if min is not zero, then P is modified
     * to P' = P+min*D so that the ray is represented by P'+t*D for t >= 0.
     * If min is -MAX_REAL and max is finite, then the origin and direction
     * are modified to P' = P+max*D and D' = -D.
     *
     * CT_SEGMENT:  If min is not -max, then P is modified to
     * P' = P + ((min+max)/2)*D and the extent is e' = (max-min)/2.
     *
     * CT_POINT:  If min is not zero, the P is modified to P' = P+min*D.
     *
     * CT_EMPTY:  Set max to -MAX_REAL and min to MAX_REAL.
     *
     * The first function is virtual since the updates are dependent on the
     * dimension of the vector space.
     */
    public abstract void MakeCanonical ();

    public boolean IsCanonical ()
    {
        if (m_iType == ComponentType.CT_RAY)
        {
            return m_fMin == (float)0.0 && m_fMax == Float.MAX_VALUE;
        }

        if (m_iType == ComponentType.CT_SEGMENT)
        {
            return m_fMin == -m_fMax;
        }

        if (m_iType == ComponentType.CT_POINT)
        {
            return m_fMin == (float)0.0; 
        }

        if (m_iType == ComponentType.CT_EMPTY)
        {
            return m_fMin == Float.MAX_VALUE
                && m_fMax == Float.MIN_VALUE;
        }

        // m_iType == CT_LINE
        return true;
    }

    /** access the interval [min,max] */
    public float GetMin ()
    {
        return m_fMin;
    }

    public float GetMax ()
    {
        return m_fMax;
    }

    /** Determine if the specified parameter is in the interval. */
    public boolean Contains (float fParam)
    {
        return m_fMin <= fParam && fParam <= m_fMax;
    }

    protected LinCompf ()  // default is CT_NONE
    {
        m_iType = ComponentType.CT_EMPTY;
        m_fMin = Float.MAX_VALUE;
        m_fMax = Float.MIN_VALUE;
    }

    /** component type */
    protected ComponentType m_iType;

    /** the interval of restriction for t */
    protected float m_fMin, m_fMax;
}
