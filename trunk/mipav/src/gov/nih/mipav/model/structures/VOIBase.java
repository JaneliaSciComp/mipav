package gov.nih.mipav.model.structures;

import gov.nih.mipav.MipavMath;
import gov.nih.mipav.model.file.FileInfoBase;
import gov.nih.mipav.view.dialogs.JDialogVOIStatistics;
import gov.nih.mipav.view.renderer.WildMagic.Render.VolumeImage;
import gov.nih.mipav.view.renderer.WildMagic.Render.VolumeVOI;

import java.awt.event.KeyEvent;
import java.util.BitSet;
import java.util.Vector;


import WildMagic.LibFoundation.Approximation.ApprPlaneFit3f;
import WildMagic.LibFoundation.Distance.DistanceVector3Segment3;
import WildMagic.LibFoundation.Mathematics.ColorRGB;
import WildMagic.LibFoundation.Mathematics.ColorRGBA;
import WildMagic.LibFoundation.Mathematics.Plane3f;
import WildMagic.LibFoundation.Mathematics.Segment3f;
import WildMagic.LibFoundation.Mathematics.Vector3f;



/**
 * Base which holds the functions common to both Contour, Line and Point type VOI. Abstract class.
 *
 * @version  0.1 Jul 22, 1998
 * @author   Matthew McAuliffe
 * @see      VOILine
 * @see      VOIContour
 *
 *           <p>$Logfile: /mipav/src/gov/nih/mipav/model/structures/VOIBase.java $ $Revision: 56 $ $Date: 2/24/06 3:34p
 *           $</p>
 */
public abstract class VOIBase extends Vector<Vector3f> {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** Use serialVersionUID for interoperability. */
    private static final long serialVersionUID = -3872480526036224600L;

    /** Used in determining if all points on the contour have the same x-value */
    public static final int XPLANE = 1;

    /** Used in determining if all points on the contour have the same y-value */
    public static final int YPLANE = 2;

    /** Used in determining if all points on the contour have the same z-value */
    public static final int ZPLANE = 4;

    /** If the points on the contour are not on the x,y, or z-plane. */
    public static final int NOT_A_PLANE = 0;

    /**
     * Used in places which usually remember an index into a point vector. Indicates that the index should not be used.
     */
    public static final int NOT_A_POINT = -99;

    /** The up arrow key. */
    protected static final int UP = KeyEvent.VK_UP;

    /** The left arrow key. */
    protected static final int LEFT = KeyEvent.VK_LEFT;

    /** The down arrow key. */
    protected static final int DOWN = KeyEvent.VK_DOWN;

    /** The right arrow key. */
    protected static final int RIGHT = KeyEvent.VK_RIGHT;

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /**
     * Flag indicating whether or not a VOI is active (selected). If the VOI is selected then the flag is true else it
     * is false
     */
    protected boolean active;

    /** Stores the geometric center of the contour of a VOI. */
    protected Vector3f gcPt = new Vector3f(0, 0, 0);

    /** Stores the black and white center of mass of the contour of a VOI. */
    protected Vector3f cenMassPt = new Vector3f(0, 0, 0);

    /** Stores the red center of mass of the contour of a VOI. */
    protected Vector3f cenMassPtR = new Vector3f(0, 0, 0);

    /** Stores the green center of mass of the contour of a VOI. */
    protected Vector3f cenMassPtG = new Vector3f(0, 0, 0);

    /** Stores the blue center of mass of the contour of a VOI. */
    protected Vector3f cenMassPtB = new Vector3f(0, 0, 0);

    /** Flag that indicates if the VOI is movable. */
    protected boolean fixed = false;

    /** Label (Name) for this member of the VOI. */
    protected String label = null;

    /** Keeps track of the last near point value (for keyboard moving of individual pts. */
    protected int lastPoint = 0;

    /** Flag used to indicate if the cursor is near a point of the VOI member. */
    protected int nearPoint = NOT_A_POINT;

    /** Reference to the containing VOI object. */
    protected VOI voiGroup;

    /**
     * If doGeometricCenterLabel = true and if active == false and if closed =
     * true, execute drawGeometricCenterLabel when in drawSelf
     */
    private boolean doGeometricCenterLabel = false;

    /** Set to true if the contour bounding-box needs updating. */
    protected boolean m_bUpdateBounds = true;    
    protected boolean m_bUpdateGeometricCenter = true;
    /** Set to true if the contour plane needs updating. */
    protected boolean m_bUpdatePlane = true;

    /** VOI type of this contour. */
    protected int m_iVOIType;
    /** Anchor point. */
    protected int m_iAnchorIndex = -1;
    /** True when this is a closed contour. */
    protected boolean closed = true;
    /** True is this is a split-line contour. */
    protected boolean m_bSplit = false;

    /** Current plane of this contour. */
    protected int m_iPlane = NOT_A_PLANE;
    /** Current contour bounding-box. */
    protected Vector3f[] m_akImageMinMax = new Vector3f[]{ new Vector3f(), new Vector3f() };    
    /** Contour color. */
    protected ColorRGBA m_kColor = new ColorRGBA();

    /** Data structure for drawing this contour in the GPU VolumeRenderer */
    protected VolumeVOI m_kVolumeVOI;   

    /**
     * Number of pixels in the array used in graphing intensity along the
     * boundary.
     */
    private int numPixels;

    /**
     * Default Constructor. Initializes the Vector<> : start and the amount to increment the vector by.,
     */
    public VOIBase() {
        super(20, 10);
        active = false;
        fixed = false;
    }

    /**
     * Constructor sets the fixed and closed flags.
     * @param bFixed true if this contour cannot be changed.
     * @param bClosed true if this contour is closed.
     */
    public VOIBase( boolean bFixed, boolean bClosed  )
    {
        this();
        lastPoint = size() - 1;
        fixed = bFixed;
        closed = bClosed;
        setClosed(closed);
    }

    /**
     * Constructor, sets the fixed and closed flags and positions.
     * @param bFixed true if this contour cannot be changed.
     * @param bClosed true if this contour is closed.
     * @param kIn positions.
     */
    public VOIBase( boolean bFixed, boolean bClosed, Vector<Vector3f> kIn )
    {
        this();
        if ( kIn != null )
        {
            for ( int i = 0; i < kIn.size(); i++ )
            {
                Vector3f kPos = new Vector3f( kIn.get(i) );
                add( kPos );
            }
        }
        lastPoint = size() - 1;
        fixed = bFixed;
        closed = bClosed;
        setClosed(closed);
    }

    /**
     * Copies the input VOIBase into a new VOIBase object.
     * @param kBase
     */
    public VOIBase( VOIBase kBase ) {
        super(20, 10);
        this.active = kBase.active;
        this.gcPt.Copy(kBase.gcPt);
        this.cenMassPt.Copy(kBase.cenMassPt);
        this.cenMassPtR.Copy(kBase.cenMassPtR);
        this.cenMassPtG.Copy(kBase.cenMassPtG);
        this.cenMassPtB.Copy(kBase.cenMassPtB);
        this.closed = kBase.closed;
        this.fixed = kBase.fixed;
        if ( kBase.label != null )
        {
            this.label = new String(kBase.label);
        }
        this.lastPoint = kBase.lastPoint;
        this.nearPoint = kBase.nearPoint;
        this.voiGroup = kBase.voiGroup;

        for ( int i = 0; i < kBase.size(); i++ )
        {
            add( new Vector3f(kBase.get(i)) );
        } 

        this.m_iVOIType = kBase.m_iVOIType;
        this.m_iAnchorIndex = kBase.m_iAnchorIndex;

        this.m_kVolumeVOI = null;     
    }


    /**
     * Copies the input contour and changes it's slice +/-
     * @param kBase
     * @param iPropDir
     */
    public VOIBase( VOIBase kBase, int iPropDir )
    {
        this(kBase);
        int iPlane = kBase.getPlane();
        if ( iPlane != NOT_A_PLANE )
        {
            removeAllElements();
            Vector3f kProp = new Vector3f();
            if ( (iPlane & XPLANE) == XPLANE )
            {
                kProp.Set( iPropDir, 0, 0 );
            }
            if ( (iPlane & YPLANE) == YPLANE )
            {
                kProp.Set( 0, iPropDir, 0 );
            }
            if ( (iPlane & ZPLANE) == ZPLANE )
            {
                kProp.Set( 0, 0, iPropDir );
            }
            for (int i = 0; i < kBase.size(); i++) {
                Vector3f pt = new Vector3f();
                pt.Add( kBase.elementAt(i), kProp );
                this.addElement(pt);
            }
        }
    }


    /**
     * Copies the input VOIBase, transformed by the input TransMatrix.
     * @param kBase VOI to copy.
     * @param tMatrix transformation
     */
    public VOIBase( VOIBase kBase, TransMatrix tMatrix )
    {
        this(kBase);
        removeAllElements();
        for (int i = 0; i < kBase.size(); i++) {
            Vector3f pt = new Vector3f();
            tMatrix.transformAsPoint3Df(kBase.elementAt(i), pt);
            this.addElement(pt);
        }
    }

    /**
     * Adds a point to the curve.
     *
     * @param  x  x coordinate of point
     * @param  y  y coordinate of point
     * @param  z  z coordinate of point
     */
    public void addElement(float x, float y, float z) {
        addElement(new Vector3f(x, y, z));
    }

    /**
     * Adds a point to the curve.
     *
     * @param  x  x coordinate of point
     * @param  y  y coordinate of point
     * @param  z  z coordinate of point
     */
    public void addElement(int x, int y, int z) {
        addElement(new Vector3f(x, y, z));
    }


    /**
     * Calculates the area of contour using vector cross product method - fast !!
     * 
     * @return returns the area
     */
    public float area() {
        float result = 0;
        Vector3f prevVector = new Vector3f();
        Vector3f currentVector = new Vector3f();
        Vector3f cross = new Vector3f();
        if (size() >= 3) {
            prevVector.Sub(get(1), get(0));

            for (int i = 2; i < size(); i++) {
                currentVector.Sub(get(i), get(0));
                cross.Cross(currentVector, prevVector);
                result += cross.Length() * 0.5;
                prevVector = currentVector;
            }

            // if result is negative then points are ordered clockwise
            // if result is positive then points are ordered counter-clockwise
            if (result < 0) {
                result = -result;
            }
        }

        // System.out.println("Contour Area = " + result);
        return result;
    }


    /**
     * Calculated the total intensity contained within this contour.
     * @param kImage
     * @return total intensity.
     */
    public float calcIntensity(ModelImage kImage) {
        numPixels = 0;
        Vector3f[] kBounds = getImageBoundingBox();
        float sum = 0;
        for ( int z = (int)kBounds[0].Z; z <= (int)kBounds[1].Z; z++ )
        {
            for ( int y = (int)kBounds[0].Y; y <= (int)kBounds[1].Y; y++ )
            {
                for ( int x = (int)kBounds[0].X; x <= (int)kBounds[1].X; x++ )
                {
                    if ( contains( x, y, z ) )
                    {
                        sum += kImage.getFloat(x,y,z);
                        numPixels++;
                    }
                }
            }
        }
        return sum;
    }
    
    public Vector<Float> calcIntensity( ModelImage kImage, float ignoreMin, float ignoreMax, int rangeFlag)
    {
        Vector<Float> values = new Vector<Float>();
        Vector3f[] kBounds = getImageBoundingBox();
        float fVal = 0;
        for ( int z = (int)kBounds[0].Z; z <= (int)kBounds[1].Z; z++ )
        {
            for ( int y = (int)kBounds[0].Y; y <= (int)kBounds[1].Y; y++ )
            {
                for ( int x = (int)kBounds[0].X; x <= (int)kBounds[1].X; x++ )
                {
                    if ( contains( x, y, z ) )
                    {
                        fVal = kImage.getFloat(x,y,z);
                        if ( !inRange( ignoreMin, ignoreMax, fVal, rangeFlag ) )
                        {
                            values.add( new Float(fVal) );
                        }
                    }
                }
            }
        }
        return values;
    }
    
    private boolean inRange(float ignoreMin, float ignoreMax, float num, int rangeFlag) {

        if (rangeFlag == JDialogVOIStatistics.NO_RANGE) {
            return false;
        } else if (rangeFlag == JDialogVOIStatistics.BETWEEN) {

            if ((num >= ignoreMin) && (num <= ignoreMax)) {
                return true;
            }
            return false;
        } else if (rangeFlag == JDialogVOIStatistics.OUTSIDE) {

            if ((num <= ignoreMin) || (num >= ignoreMax)) {
                return true;
            }
            return false;
        } else {

            if ((num >= ignoreMin) && (num <= ignoreMax)) {
                System.out.println(" min  = " + ignoreMax + " max = " + ignoreMax);

                return true;
            }
            return false;
        }

    }

    /**
     * Calculates the total intensity contained within this contour, equal to of greater than the threshold.
     * @param kImage
     * @param threshold
     * @return
     */
    public float calcIntensityThreshold(ModelImage kImage, float threshold) {
        numPixels = 0;
        Vector3f[] kBounds = getImageBoundingBox();
        float sum = 0;
        for ( int z = (int)kBounds[0].Z; z <= (int)kBounds[1].Z; z++ )
        {
            for ( int y = (int)kBounds[0].Y; y <= (int)kBounds[1].Y; y++ )
            {
                for ( int x = (int)kBounds[0].X; x <= (int)kBounds[1].X; x++ )
                {
                    if ( contains( x, y, z ) )
                    {
                        float val = kImage.getFloat(x,y,z);
                        if ( val >= threshold )
                        {
                            sum += val;
                            numPixels++;
                        }
                    }
                }
            }
        }
        return sum;
    }

    /**
     * Calculates the total intensity contained within this contour for the input color channel.
     * @param kImage
     * @param RGorB (Red = 1; Green = 2; Blue = 3)
     * @return
     */
    public Vector<ColorRGB> calcRGBIntensity(ModelImage kImage, float ignoreMin, float ignoreMax, int rangeFlag) {
        Vector<ColorRGB> values = new Vector<ColorRGB>();
        float r, g, b;
        Vector3f[] kBounds = getImageBoundingBox();
        for ( int z = (int)kBounds[0].Z; z <= (int)kBounds[1].Z; z++ )
        {
            for ( int y = (int)kBounds[0].Y; y <= (int)kBounds[1].Y; y++ )
            {
                for ( int x = (int)kBounds[0].X; x <= (int)kBounds[1].X; x++ )
                {
                    if ( contains( x, y, z ) )
                    {
                        r = kImage.getFloatC(x,y,z,1);
                        g = kImage.getFloatC(x,y,z,2);
                        b = kImage.getFloatC(x,y,z,3);
                        values.add( new ColorRGB(r,g,b) );
                    }
                }
            }
        }
        return values;
    }

    /**
     * Calculates the total intensity contained within this contour for the input color channel.
     * @param kImage
     * @param RGorB (Red = 1; Green = 2; Blue = 3)
     * @return
     */
    public float calcRGBIntensity(ModelImage kImage, int RGorB) {
        numPixels = 0;
        Vector3f[] kBounds = getImageBoundingBox();
        float sum = 0;
        for ( int z = (int)kBounds[0].Z; z <= (int)kBounds[1].Z; z++ )
        {
            for ( int y = (int)kBounds[0].Y; y <= (int)kBounds[1].Y; y++ )
            {
                for ( int x = (int)kBounds[0].X; x <= (int)kBounds[1].X; x++ )
                {
                    if ( contains( x, y, z ) )
                    {
                        sum += kImage.getFloatC(x,y,z,RGorB+1);
                        numPixels++;
                    }
                }
            }
        }
        return sum;
    }

    /**
     * Calculates the total intensity contained within this contour for the input color channel,
     * that is greater than or equal to the input threshold value.
     * @param kImage
     * @param RGorB (Red = 1; Green = 2; Blue = 3)
     * @param threshold
     * @return
     */
    public float calcRGBIntensityThreshold(ModelImage kImage, int RGorB, float threshold) {
        numPixels = 0;
        Vector3f[] kBounds = getImageBoundingBox();
        float sum = 0;
        for ( int z = (int)kBounds[0].Z; z <= (int)kBounds[1].Z; z++ )
        {
            for ( int y = (int)kBounds[0].Y; y <= (int)kBounds[1].Y; y++ )
            {
                for ( int x = (int)kBounds[0].X; x <= (int)kBounds[1].X; x++ )
                {
                    if ( contains( x, y, z ) )
                    {
                        float val = kImage.getFloatC(x,y,z,RGorB+1);
                        if ( val >= threshold )
                        {
                            sum += val;
                            numPixels++;
                        }
                    }
                }
            }
        }
        return sum;
    }

    /* (non-Javadoc)
     * @see java.util.Vector#clone()
     */
    public abstract VOIBase clone();

    /**
     * Returns true if the input iX, iY is contained within this contour.  
     * The z-value of the contour is ignored.
     * @param iX
     * @param iY
     * @return
     */
    public boolean contains(float iX, float iY)
    {
        boolean isInside = false;
        float fX = iX + 0.49f;
        float fY = iY + 0.49f;

        int iLast = size() -1;
        for ( int i = 0; i < size(); i++ )
        {
            Vector3f kPos = get(i);
            Vector3f kPosL = get(iLast);

            if (((kPosL.Y <= fY) && (fY < kPos.Y) && (areaTwice(kPos.X, kPos.Y, kPosL.X, kPosL.Y, fX, fY) >= 0)) ||
                    ((kPos.Y <= fY) && (fY < kPosL.Y) && (areaTwice(kPosL.X, kPosL.Y, kPos.X, kPos.Y, fX, fY) >= 0))) {
                isInside = !isInside;
            }

            iLast = i;
        }
        return isInside;
    }

    /**
     * Returns true if the input iX,iY,iZ is contained within this contour.
     * @param iX
     * @param iY
     * @param iZ
     * @return
     */
    public boolean contains(float iX, float iY, float iZ )
    {
        int iPlane = getPlane();
        if ( iPlane == NOT_A_PLANE )
        {
            return false; //containsPlaneFit( iX, iY, iZ );
        }
        boolean isInside = false;
        float fX = iX + 0.49f;
        float fY = iY + 0.49f;
        float fZ = iZ + 0.49f;

        if ( iPlane == XPLANE )
        {
            int iLast = size() -1;
            for ( int i = 0; i < size(); i++ )
            {
                Vector3f kPos = elementAt(i);
                Vector3f kPosL = elementAt(iLast);
                if ( isInside( kPosL.Y, kPosL.Z, kPos.Y, kPos.Z, fY, fZ ) )
                { 
                    isInside = !isInside; 
                }  
                iLast = i;
            }
        }
        else if ( iPlane == YPLANE )
        {
            int iLast = size() -1;
            for ( int i = 0; i < size(); i++ )
            {
                Vector3f kPos = elementAt(i);
                Vector3f kPosL = elementAt(iLast);
                if ( isInside( kPosL.X, kPosL.Z, kPos.X, kPos.Z, fX, fZ ) )
                { 
                    isInside = !isInside; 
                }  
                iLast = i;
            }
        }
        else
        {
            int iLast = size() -1;
            for ( int i = 0; i < size(); i++ )
            {
                Vector3f kPos = elementAt(i);
                Vector3f kPosL = elementAt(iLast);
                if ( isInside( kPosL.X, kPosL.Y, kPos.X, kPos.Y, fX, fY ) )
                { 
                    isInside = !isInside; 
                }  
                iLast = i;
            }
        }
        /*
        int iLast = size() -1;
        for ( int i = 0; i < size(); i++ )
        {
            Vector3f kPos = get(i);
            Vector3f kPosL = get(iLast);

            switch ( iPlane )
            {
            case XPLANE: 
                if ( isInside( kPosL.Y, kPosL.Z, kPos.Y, kPos.Z, fY, fZ ) )
                { 
                    isInside = !isInside; 
                }  
                break;
            case YPLANE:
                if ( isInside( kPosL.X, kPosL.Z, kPos.X, kPos.Z, fX, fZ ) )
                { 
                    isInside = !isInside; 
                }  
                break;
            case ZPLANE:
                if ( isInside( kPosL.X, kPosL.Y, kPos.X, kPos.Y, fX, fY ) )
                { 
                    isInside = !isInside; 
                }  
                break;
            }
            iLast = i;
        }
        */
        return isInside;
    }



    /**
     * Creates the VolumeVOI data structure for rendering this contour in the GPU VolumeRenderer
     * @param kRenderer
     * @param kDrawable
     * @param kCamera
     * @param kVolumeImage
     * @param kTranslate
     * @return
     */
    public VolumeVOI createVolumeVOI(VolumeImage kVolumeImage, Vector3f kTranslate)
    {
        m_kVolumeVOI = new VolumeVOI( kVolumeImage, kTranslate, this, m_kColor );
        return m_kVolumeVOI;
    }

    /**
     * Cycles through the active points on the curve.
     * @param  keyCode  int arrow key (up/down/left/right)
     */
    public void cycleActivePt(int keyCode) {

        if ((lastPoint >= 0) && (lastPoint < this.size()))
        {
            int index = lastPoint;

            switch (keyCode) {

            case UP:
            case RIGHT:
                index++;
                break;

            case DOWN:
            case LEFT:
                index--;
                break;
            }

            if (index < 0) {
                index = this.size() - 1;
            } else if (index > (size() - 1)) {
                index = 0;
            }

            lastPoint = index;
        }
    }

    /**
     * Deletes the specified position on the curve.
     * @param iPos
     */
    public void delete( int iPos )
    {
        remove(iPos);
        lastPoint = Math.max( 0, iPos - 1 );  
        update();
    }

    /**
     * Deletes local data members.
     */
    public void dispose()
    {
        gcPt = null;
        cenMassPt = null;
        cenMassPtR = null;
        cenMassPtG = null;
        cenMassPtB = null;
        label = null;
        voiGroup = null;
        m_akImageMinMax[0] = null;
        m_akImageMinMax[1] = null;
        m_akImageMinMax = null;
        m_kColor = null;
        m_kVolumeVOI = null;
    }

    /**
     * Returns the smallest distance from the input point to this contour.
     * @param iX
     * @param iY
     * @param iZ
     * @return
     */
    public float distanceToVOI(int iX, int iY, int iZ )
    {
        float fMin = Float.MAX_VALUE;
        Vector3f kVOIPoint = new Vector3f(iX, iY, iZ );
        for (int i = 0; i < (size() - 1); i++)
        {
            Vector3f kPos0 = get(i);
            Vector3f kPos1 = get(i+1);
            
            float fDist = Float.MAX_VALUE;
            if ( kVOIPoint.equals(kPos0) ) {
                return 0.0f;
            } else if ( kVOIPoint.equals(kPos1) ) {
                return 0.0f;
            } else if (kPos0.equals(kPos1) ) {
                // 2 contour points coincide - do nothing
            }
            else
            {
                Vector3f kDir = new Vector3f();
                kDir.Sub( kPos1, kPos0 );
                float fLength = kDir.Normalize();
                Segment3f kSegment = new Segment3f(kPos0, kDir, fLength);
                DistanceVector3Segment3 kDist = new DistanceVector3Segment3( kVOIPoint, kSegment );
                fDist = kDist.Get();
            }
            if ( fDist < fMin )
            {
                fMin = fDist;
                nearPoint = i;
            }
        }

        Vector3f kPos0 = get(size() - 1);
        Vector3f kPos1 = get(0);
        
        if ( kVOIPoint.equals(kPos0) ) {
            return 0.0f;
        } else if ( kVOIPoint.equals(kPos1) ) {
            return 0.0f;
        } else if (kPos0.equals(kPos1) ) {
            // 2 contour points coincide - do nothing
        }
        else
        {           
            Vector3f kDir = new Vector3f();
            kDir.Sub( kPos1, kPos0 );
            float fLength = kDir.Normalize();
            Segment3f kSegment = new Segment3f(kPos0, kDir, fLength);
            DistanceVector3Segment3 kDist = new DistanceVector3Segment3( kVOIPoint, kSegment );
            float fDist = kDist.Get();
            if ( fDist < fMin )
            {
                fMin = fDist;
                nearPoint = size()-1;
            }
        }
        return fMin;
    }

    /**
     * Exports the float arrays of the points of the curve.
     *
     * @param  x  array of x coordinates
     * @param  y  array of y coordinates
     * @param  z  array of z coordinates
     */
    public void exportArrays(float[] x, float[] y, float[] z) {
        int i;

        if ((x == null) || (y == null)) {
            return;
        }

        if ((x.length != y.length) || (x.length < size())) {
            return;
        }

        for (i = 0; i < this.size(); i++) {
            x[i] = elementAt(i).X;
            y[i] = elementAt(i).Y;
            z[i] = elementAt(i).Z;
        }
    }

    /**
     * Exports the arrays of the points of the curve in int array format.
     *
     * @param  x  array of x coordinates
     * @param  y  array of y coordinates
     * @param  z  array of z coordinates
     */
    public void exportArrays(int[] x, int[] y, int[] z) {
        int i;

        if ((x == null) || (y == null)) {
            return;
        }

        if ((x.length != y.length) || (x.length < size())) {
            return;
        }

        for (i = 0; i < size(); i++) {
            x[i] = MipavMath.round(elementAt(i).X);
            y[i] = MipavMath.round(elementAt(i).Y);
            z[i] = MipavMath.round(elementAt(i).Z);
        }
    }

    /**
     * Finds the position/intensity along a line VOI.
     *
     * @param   position     Vector that is filled with the distance along the line in millimeters for example
     * @param   intensity    the corresponding intensities along the line
     *
     * @return  the number of points in the position and intensity array that have valid data.
     */
    public int findPositionAndIntensity(ModelImage kImage, Vector<Vector3f> positions, Vector<ColorRGB> colors )
    {
        for ( int i = 0; i < size()-1; i++ )
        {
            Vector3f kStart = elementAt(i);
            Vector3f kEnd = elementAt(i+1);

            findPositionAndIntensity( kStart, kEnd, kImage, positions, colors );

            if( (i < size() - 2) && (positions.size() > 0) )
            {
                positions.remove(positions.size()-1);
                colors.remove(colors.size()-1);
            }
        }
        if ( closed )
        {
            positions.remove(positions.size()-1);
            colors.remove(colors.size()-1);

            Vector3f kStart = lastElement();
            Vector3f kEnd = firstElement();
            findPositionAndIntensity( kStart, kEnd, kImage, positions, colors );       

            if ( positions.size() > 0 )
            {
                positions.remove(positions.size()-1);
                colors.remove(colors.size()-1);
            }
        }

        return positions.size();
    }

    /**
     * Finds the position/intensity along a line VOI.
     *
     * @param   position     Vector that is filled with the distance along the line in millimeters for example
     * @param   intensity    the corresponding intensities along the line
     *
     * @return  the number of points in the position and intensity array that have valid data.
     */
    public void findPositionAndIntensity(Vector3f kStart, Vector3f kEnd,
            ModelImage kImage, Vector<Vector3f> positions, Vector<ColorRGB> colors)
    {              
        Vector3f kDiff = new Vector3f();
        kDiff.Sub( kEnd, kStart );

        double xDist = Math.abs(kDiff.X);
        double yDist = Math.abs(kDiff.Y);
        double zDist = Math.abs(kDiff.Z);

        double max = Math.max(xDist, Math.max(yDist,zDist));
        double xInc = ((kDiff.X) / (1.0 * max));
        double yInc = ((kDiff.Y) / (1.0 * max));
        double zInc = ((kDiff.Z) / (1.0 * max));

        int xD = kImage.getExtents()[0];
        int yD = kImage.getExtents()[1];
        int zD = kImage.getExtents().length > 3 ? kImage.getExtents()[2] : 1;

        Vector3f kStep = new Vector3f(kStart);

        double totalDistance = positions.size() == 0 ? 0 : positions.lastElement().Z;
        for (int i = 0; i < max; i++ )
        {            
            if ( i == max -1 )
            {
                kStep.Copy(kEnd);
            }
            kDiff.Sub( kStep, kStart );
            double subDistance = MipavMath.distance( kStep, kStart, kImage.getResolutions(0) );          

            int indexZ = Math.min(Math.round(kStep.Z), zD - 1);
            int indexY = Math.min(Math.round(kStep.Y), yD - 1);
            int indexX = Math.min(Math.round(kStep.X), xD - 1);
            int index = (indexZ * yD * xD) + (indexY * xD) + indexX;

            positions.add( new Vector3f( indexX, indexY, (float)(totalDistance + subDistance))) ;
            if ( kImage.isColorImage() )
            {
                colors.add( new ColorRGB(kImage.getFloat(index * 4 + 1),
                        kImage.getFloat(index * 4 + 2),
                        kImage.getFloat(index * 4 + 3) ) );
            }
            else
            {
                colors.add( new ColorRGB(kImage.getFloat(index), 0, 0 ) );
            }
            kStep.X += xInc;
            kStep.Y += yInc;
            kStep.Z += zInc;
        }
    }

    /**
     * Gets the Vector3f of the active point.
     * @return  Vector3f the active point's Vector3f
     */    
    public Vector3f getActivePt() {
        Vector3f pt = null;
        if ((lastPoint >= 0) && (lastPoint < this.size()))
        {
            pt = elementAt(lastPoint);
        }
        return pt;
    }


    /**
     * Returns the anchor point.
     * @return
     */
    public int getAnchor()
    {
        return m_iAnchorIndex;
    }

    /**
     * Calculates the bounds of the contour.
     * 
     * @param x
     *            two element array where x[0] = min extent of the Contour and
     *            x[1] = max extent of the Contour in the x dimension
     * @param y
     *            two element array where y[0] = min extent of the Contour and
     *            y[1] = max extent of the Contour in the y dimension
     * @param z
     *            two element array where z[0] = min extent of the Contour and
     *            z[1] = max extent of the Contour in the z dimension
     */
    public void getBounds(float[] x, float[] y, float[] z) {
        getImageBoundingBox();
        x[0] = m_akImageMinMax[0].X;
        x[1] = m_akImageMinMax[1].X;
        y[0] = m_akImageMinMax[0].Y;
        y[1] = m_akImageMinMax[1].Y;
        z[0] = m_akImageMinMax[0].Z;
        z[1] = m_akImageMinMax[1].Z;
    }
    /**
     * Calculates the bounds of the contour.
     * 
     * @param x
     *            two element array where x[0] = min extent of the Contour and
     *            x[1] = max extent of the Contour in the x dimension
     * @param y
     *            two element array where y[0] = min extent of the Contour and
     *            y[1] = max extent of the Contour in the y dimension
     * @param z
     *            two element array where z[0] = min extent of the Contour and
     *            z[1] = max extent of the Contour in the z dimension
     */
    public void getBounds(int[] x, int[] y, int[] z) {
        getImageBoundingBox();
        x[0] = MipavMath.round(m_akImageMinMax[0].X);
        x[1] = MipavMath.round(m_akImageMinMax[1].X);
        y[0] = MipavMath.round(m_akImageMinMax[0].Y);
        y[1] = MipavMath.round(m_akImageMinMax[1].Y);
        z[0] = MipavMath.round(m_akImageMinMax[0].Z);
        z[1] = MipavMath.round(m_akImageMinMax[1].Z);
    }

    /**
     * Gets the center of mass of the contour.
     * @return returns the center of mass
     */
    public Vector3f getCenterOfMass(ModelImage kImage) {
        getCenterOfMass(kImage, cenMassPt, 0);
        return cenMassPt;
    }

    /**
     * Gets the center of mass of the contour for the image and color-channel c.
     * @param kImage
     * @param centerPt
     * @param c
     */
    public void getCenterOfMass(ModelImage kImage, Vector3f centerPt, int c) {
        Vector3f[] kBounds = getImageBoundingBox();
        float sum = 0;
        double sumX = 0.0;
        double sumY = 0.0;
        double sumZ = 0.0;
        float val;
        for ( int z = (int)kBounds[0].Z; z <= (int)kBounds[1].Z; z++ )
        {
            for ( int y = (int)kBounds[0].Y; y <= (int)kBounds[1].Y; y++ )
            {
                for ( int x = (int)kBounds[0].X; x <= (int)kBounds[1].X; x++ )
                {
                    if ( contains( x, y, z ) )
                    {
                        if ( kImage.isColorImage() )
                        {
                            val = kImage.getFloatC(x,y,z,c);
                        }
                        else
                        {
                            val = kImage.getFloat(x,y,z);                            
                        }
                        sumX += x * val;
                        sumY += y * val;
                        sumZ += z * val;
                        sum += val;
                    }
                }
            }
        }
        centerPt.X = MipavMath.round(sumX / sum);
        centerPt.Y = MipavMath.round(sumY / sum);
        centerPt.Z = MipavMath.round(sumZ / sum);
    }

    /**
     * Gets the blue center of mass of the contour.
     * @return returns the center of mass
     */
    public Vector3f getCenterOfMassB(ModelImage kImage) {
        getCenterOfMass(kImage, cenMassPtB, 3);
        return cenMassPtB;
    }

    /**
     * Gets the green center of mass of the contour.
     * @return returns the center of mass
     */
    public Vector3f getCenterOfMassG(ModelImage kImage) {
        getCenterOfMass(kImage, cenMassPtG, 2);
        return cenMassPtG;
    }

    /**
     * Gets the red center of mass of the contour.
     * @return returns the center of mass
     */
    public Vector3f getCenterOfMassR(ModelImage kImage) {
        getCenterOfMass(kImage, cenMassPtR, 1);
        return cenMassPtR;
    }

    /**
     * Returns the contour ID: the index of this contour into the VOI.
     * @return
     */
    public int getContourID()
    {
        if ( voiGroup != null )
        {
            return voiGroup.getCurves().indexOf(this);
        }
        return -1;
    }

    /**
     * If doGeometricCenterLabel = true and active == false and closed = true,
     * execute drawGeometricCenterLabel when in drawSelf
     * 
     * @return doGeometricCenterLabel
     */
    public boolean getDoGeometricCenterLabel() {
        return this.doGeometricCenterLabel;
    }

    /**
     * Gets the geometric center of the contour.
     * 
     * @return returns the geometric center
     */
    public Vector3f getGeometricCenter() {

        if ( !m_bUpdateGeometricCenter )
        {
            return gcPt;
        }  
        Vector3f[] kBounds = getImageBoundingBox();
        int nPts = 0;
        Vector3f kSum = new Vector3f();
        for ( int z = (int)kBounds[0].Z; z <= (int)kBounds[1].Z; z++ )
        {
            for ( int y = (int)kBounds[0].Y; y <= (int)kBounds[1].Y; y++ )
            {
                for ( int x = (int)kBounds[0].X; x <= (int)kBounds[1].X; x++ )
                {
                    if ( contains( x, y, z ) )
                    {
                        nPts++;
                        //kSum.X += x;
                        //kSum.Y += y;
                        //kSum.Z += z;
                        kSum.Add( x, y, z );
                    }
                }
            }
        }
        gcPt.X = MipavMath.round( kSum.X / nPts);
        gcPt.Y = MipavMath.round( kSum.Y / nPts);
        gcPt.Z = MipavMath.round( kSum.Z / nPts);        
        m_bUpdateGeometricCenter = false;
        return gcPt;
    }

    /**
     * Returns the VOI object that contains this contour.
     * @return the VOI object that contains this contour.
     */
    public VOI getGroup()
    {
        return voiGroup;
    }

    /**
     * Returns the contour bounding-box.
     * @return
     */
    public Vector3f[] getImageBoundingBox()
    {
        if ( m_bUpdateBounds )
        {
            for ( int i = 0; i < size(); i++ )
            {
                Vector3f kVolumePt = elementAt(i);
                if ( i == 0 )
                {
                    m_akImageMinMax[0].Copy(kVolumePt);
                    m_akImageMinMax[1].Copy(kVolumePt);
                }
                m_akImageMinMax[0].Min( kVolumePt );
                m_akImageMinMax[1].Max( kVolumePt );
            }
            m_bUpdateBounds = false;
        }
        return m_akImageMinMax;
    }

    /**
     * Gets the label of the VOI.
     *
     * @return  label as a String
     */
    public String getLabel() {
        if ( label == null && voiGroup != null )
        {
            label = String.valueOf( voiGroup.getCurves().indexOf(this) );
        }
        return label;
    }

    /**
     * Accessor that returns the number of points used in the most recent
     * intensity calculation of this contour.
     * 
     * @return the number of points used in the most recent intensity
     *         calculation
     */
    public int getLastNumPixels() {
        return numPixels;
    }


    /**
     * Returns the total length of this contour, based on the input resolutions.
     * @param resolutions
     * @return
     */
    public double getLengthPtToPt(float[] resolutions) {
        double totalLength = 0;
        for (int i = 0; i < size()-1; i++) {
            totalLength += MipavMath.distance(get(i), get(i+1), resolutions);
        }

        if (closed == true) {
            totalLength += MipavMath.distance(lastElement(), firstElement(), resolutions);
        }

        return totalLength;
    }

    /**
     * Returns the length of this contour between the two positions along the contour.
     * Based on the input resolutions and units. 
     * @param iPos0
     * @param iPos1
     * @param afResolutions
     * @param aiUnits
     * @return
     */
    public String getLengthString(int iPos0, int iPos1, float[] afResolutions, int[] aiUnits)
    {
        if ( iPos0 >= size() || iPos1 >= size() )
        {
            return null;
        }
        double length = MipavMath.distance( get(iPos0), get(iPos1), afResolutions );

        String tmpString = String.valueOf(length);
        int i = tmpString.indexOf('.');

        if (tmpString.length() >= (i + 3)) {
            tmpString = tmpString.substring(0, i + 3);
        }

        tmpString = tmpString + " " + FileInfoBase.getUnitsOfMeasureAbbrevStr(aiUnits[0]);
        return tmpString;
    }

    /**
     * Gets the VOI's name... need this here to display in drawSelf
     *
     * @return  String
     */
    public String getName() {
        if ( voiGroup != null )
        {
            return voiGroup.getName();
        }
        return null;
    }

    /**
     * Returns the point on the curve nearest the cursor, calculated by nearLine, etc.
     * @return point on the curve nearest the cursor.
     */
    public int getNearPoint()
    {
        return nearPoint;
    }


    /**
     * Determines if the points on the contour lie on either the x,y,or z-planes. Or not on any plane.
     * @return
     */
    public int getPlane()
    {
        if ( !m_bUpdatePlane )
        {
            return m_iPlane;
        }
        m_bUpdatePlane = false;
        boolean bXSame = true, bYSame = true, bZSame = true;
        for ( int i = 0; i < size() -1; i++ )
        {
            Vector3f kPos1 = elementAt(i);
            Vector3f kPos2 = elementAt(i+1);
            if ( kPos1.X != kPos2.X )
            {
                bXSame = false;
            }
            if ( kPos1.Y != kPos2.Y )
            {
                bYSame = false;
            }
            if ( kPos1.Z != kPos2.Z )
            {
                bZSame = false;
            }
        }
        m_iPlane = NOT_A_PLANE;
        if ( bXSame )
        {
            m_iPlane |= XPLANE;
        }
        if ( bYSame )
        {
            m_iPlane |= YPLANE;
        }
        if ( bZSame )
        {
            m_iPlane |= ZPLANE;
        }
        return m_iPlane;
    }
    /**
     * Returns the last selected point.
     * @return
     */
    public int getSelectedPoint()
    {
        return lastPoint;
    }

    /**
     * Returns the total lenght of this contour as a string. 
     * Based on the input resolutions and units.
     * @param afResolutions
     * @param aiUnits
     * @return
     */
    public String getTotalLengthString(float[] afResolutions, int[] aiUnits)
    {
        double length = getLengthPtToPt(afResolutions);

        String tmpString = String.valueOf(length);
        int i = tmpString.indexOf('.');

        if (tmpString.length() >= (i + 3)) {
            tmpString = tmpString.substring(0, i + 3);
        }

        tmpString = tmpString + " " + FileInfoBase.getUnitsOfMeasureAbbrevStr(aiUnits[0]);
        return tmpString;

    }

    /**
     * Returns the type of this contour.
     * @return
     */
    public int getType()
    {
        return m_iVOIType;
    }

    /**
     * Returns the VolumeVOI data member used to draw this contour in the GPU VOlumeRenderer.
     * @return
     */
    public VolumeVOI getVolumeVOI() 
    {
        return m_kVolumeVOI;
    }

    /**
     * Imports new position values into this contour.
     * @param x
     * @param y
     * @param z
     * @param n
     */
    public void importArrays(float[] x, float[] y, float[] z, int n) {
        this.removeAllElements();
        for ( int i = 0; i < Math.min(Math.min(x.length,y.length),Math.min(z.length,n)); i++ )
        {
            add( new Vector3f( x[i], y[i], z[i] ) );
        }
    }

    /**
     * Imports new position values into this contour.
     * @param x
     * @param y
     * @param z
     * @param n
     */
    public void importArrays(int[] x, int[] y, int[] z, int n) {
        this.removeAllElements();
        for ( int i = 0; i < Math.min(Math.min(x.length,y.length),Math.min(z.length,n)); i++ )
        {
            add( new Vector3f( x[i], y[i], z[i] ) );
        }        
    }


    /**
     * Imports new position values into this contour.
     * @param pt
     */
    public void importPoints(Vector3f[] pt) {
        this.removeAllElements();
        for ( int i = 0; i < pt.length; i++ )
        {
            add( new Vector3f( pt[i] ) );
        }        
    }

    /**
     * Accessor to flag that indicates if an VOI is active.
     *
     * @return  flag indicating if an VOI is active
     */
    public boolean isActive() {
        return active;
    }


    /**
     * Flag used to indicate type of contour: true = closed contour (i.e.
     * contour end points are connected) false = open contour
     * 
     * @return flag whether the voi contour is closed.
     */
    public boolean isClosed()
    {
        return closed;
    }


    /**
     * Accessor to flag that indicates if an VOI is fixed (movable).
     *
     * @return  flag indicating if an VOI is fixed
     */
    public boolean isFixed() {
        return fixed;
    }

    /**
     * Returns true if this is a split-line contour.
     * @return
     */
    public boolean isSplit()
    {
        return m_bSplit;
    }



    /**
     * Returns true if the input position is near the outline of this contour.
     * @param iX
     * @param iY
     * @param iZ
     * @return
     */
    public boolean nearLine(int iX, int iY, int iZ )
    {
        return nearLine(iX,iY,iZ,3);
    }

    /**
     * Returns true if the input position is near the outline of this contour.
     * Based on the input tolerance.
     * @param iX
     * @param iY
     * @param iZ
     * @param tol
     * @return
     */
    public boolean nearLine(int iX, int iY, int iZ, int tol )
    {
        if ( distanceToVOI( iX, iY, iZ ) < tol )
        {
            return true;
        }
        return false;
    }

    public boolean nearPoint( int iX, int iY, int iZ) {

        Vector3f kVOIPoint = new Vector3f(iX, iY, iZ );
        for ( int i = 0; i < size(); i++ )
        {
            Vector3f kPos = get(i);
            Vector3f kDiff = new Vector3f();
            kDiff.Sub( kPos, kVOIPoint );
            if ( (Math.abs( kDiff.X ) < 3) &&  (Math.abs( kDiff.Y ) < 3) && (Math.abs( kDiff.Z ) < 3) )
            {
                setNearPoint(i);
                return true;
            }
        }
        return false;
    }

    /**
     * Fits a plane to the points in this contour and returns the plane.
     * @return
     */
    public Plane3f planeFit()
    {
        return ApprPlaneFit3f.OrthogonalPlaneFit3( size(), this );
    }


    /**
     * Sets flag to indicate whether or not VOI is active.
     *
     * @param  active  flag sets the activity status of a flag
     */
    public void setActive(boolean active) {
        this.active = active;
    }

    /**
     * Sets the anchor point. 
     */
    public void setAnchor()
    {
        m_iAnchorIndex = size()-1;
    }

    /**
     * Sets this contour as open or closed.
     * @param bClosed
     */
    public void setClosed( boolean bClosed )
    {
        closed = bClosed;
        if ( closed )
        {
            m_iVOIType = VOI.CONTOUR;
        }
        else
        {
            m_iVOIType = VOI.POLYLINE;
        }
    }   

    /**
     * If doGeometricCenterLabel = true and active == false and closed = true,
     * execute drawGeometricCenterLabel when in drawSelf
     * 
     * @param doGeometricCenterLabel
     */
    public void setDoGeometricCenterLabel(boolean doGeometricCenterLabel) {
        this.doGeometricCenterLabel = doGeometricCenterLabel;
    }

    /**
     * Sets flag to indicate whether or not VOI is fixed.
     *
     * @param  fixed  flag sets the fixed status of a flag
     */
    public void setFixed(boolean fixed) {
        this.fixed = fixed;
    }

    /**
     * Sets the VOI object that contains this contour.
     * @param kGroup VOI container for this contour.
     */
    public void setGroup( VOI kGroup )
    {
        voiGroup = kGroup;
    }

    /**
     * Sets the label of the VOI.
     *
     * @param  str  the label
     */
    public void setLabel(String str) {

        if (str == null) {
            label = null;
        } else {
            label = new String(str);
        }
    }

    /**
     * Sets the near point.
     * @param i
     */
    public void setNearPoint( int i )
    {
        nearPoint = i;
    }


    /**
     * Sets the selected point.
     * @param i
     */
    public void setSelectedPoint( int i )
    {
        lastPoint = i;
        nearPoint = i;
    }    


    /**
     * Sets the split flag.
     * @param bSplit
     */
    public void setSplit(boolean bSplit)
    {
        m_bSplit = bSplit;;
    }

    /**
     * If the points of this contour all exist on either the x,y,z plane, 
     * then the value of that plane is returned.  If the points are not on a plane,
     * -1 is returned.
     * @return
     */
    public int slice()
    {
        int iPlane = getPlane();

        if ( (iPlane&XPLANE) == XPLANE )
        {
            return (int)get(0).X;
        }
        if ( (iPlane&YPLANE) == YPLANE )
        {
            return (int)get(0).Y;
        }
        if ( (iPlane&ZPLANE) == ZPLANE )
        {
            return (int)get(0).Z;
        }
        return -1;
    }

    /**
     * Removes collinear points (or near collinear) in the contour. If the
     * perpendicular distance from the middle point to the line defined by the
     * 1st and 3rd point is small than the middle point is removed.
     * 
     * @param constraint
     *            factor that controls the number of points removed. A larger
     *            constraint removes more points 0.50 typical - removes most
     *            "almost/and collinear" points 0.10 - removes only "collinear"
     *            points
     * @param tFlag
     *            if true, trim adjacient points
     */
    public void trimPoints(double constraint, boolean tFlag) {
        int i;
        boolean flag = true;
        int end;

        if (size() <= 5) {
            return;
        }

        if (tFlag == true) {
            end = size();

            for (i = 0; i < (end - 1); i++) {
                if (MipavMath.distance( elementAt(i), elementAt(i+1) ) <= 1.5) {
                    removeElementAt(i + 1); // remove adjacient points
                    end = size();
                    i = i - 1;
                }

                if (size() <= 5) {
                    return;
                }
            }
        }

        while (flag == true) {
            flag = false;
            end = size();

            if (size() <= 5) {
                return;
            }

            for (i = 0; (i < (end - 2)) && (end > 5); i++) {

                Vector3f kPos0 = get(i);
                Vector3f kPos1 = get(i + 1);
                Vector3f kPos2 = get(i + 2);

                Vector3f kDir = new Vector3f();
                kDir.Sub( kPos2, kPos0 );
                float fLength = kDir.Normalize();
                Segment3f kSegment = new Segment3f(kPos0, kDir, fLength);
                DistanceVector3Segment3 kDist = new DistanceVector3Segment3( kPos1, kSegment );
                float fDist = kDist.Get();
                if ( fDist < constraint )
                {
                    removeElementAt(i + 1);
                    end = size();
                    i = i - 1;
                    flag = true;
                }
            }
        }
    }



    /**
     * The contour has changed, set the update flags so statistics are recalculated when needed.
     */
    public void update()
    {
        m_bUpdateBounds = true;
        m_bUpdatePlane = true;
        m_bUpdateGeometricCenter = true;

        if ( m_kVolumeVOI != null )
        {
            m_kVolumeVOI.setVOI(this);
        }
    }



    /**
     * The contour has changed, set the update flags so statistics are recalculated when needed.
     */
    public void update(Vector3f kTranslate)
    {
        gcPt.Add(kTranslate);
        m_akImageMinMax[0].Add(kTranslate);
        m_akImageMinMax[1].Add(kTranslate);
        if ( m_kVolumeVOI != null )
        {
            m_kVolumeVOI.setVOI(this);
        }
    }




    /**
     * Set the color of this contour.
     * @param kColor
     */
    public void update( ColorRGBA kColor )
    {
        m_kColor.Copy(kColor);
        if ( m_kVolumeVOI != null )
        {
            m_kVolumeVOI.update( kColor );
        }
    }


    /**
     * Calculates twice the area (cross product of two vectors) of a triangle
     * given three points. This is a private function only called by the
     * function "contains".
     * 
     * @param ptAx
     *            x-coordinate of the first point of the triangle
     * @param ptAy
     *            y-coordinate of the first point of the triangle
     * @param ptBx
     *            x-coordinate of the second point of the triangle
     * @param ptBy
     *            y-coordinate of the second point of the triangle
     * @param ptCx
     *            x-coordinate of the third point of the triangle
     * @param ptCy
     *            y-coordinate of the third point of the triangle
     * 
     * @return twice the area of the triangle if CCw or -2*area if CW
     */
    protected float areaTwice(float ptAx, float ptAy, float ptBx, float ptBy, float ptCx, float ptCy) {
        return (((ptAx - ptCx) * (ptBy - ptCy)) - ((ptAy - ptCy) * (ptBx - ptCx)));
    }

    /**
     * Returns true if fX,fY is 'inside' the line specified by fX1,fY1 -> fX2,fY2.
     * @param fX1
     * @param fY1
     * @param fX2
     * @param fY2
     * @param fX
     * @param fY
     * @return
     */
    private boolean isInside( float fX1, float fY1, float fX2, float fY2, float fX, float fY )
    {
        if ( !(((fY1 <= fY) && (fY < fY2)) || ((fY2 <= fY) && (fY < fY1))) )
            return false;
        return ( ((fY1 <= fY) && (fY < fY2) && (areaTwice(fX2, fY2, fX1, fY1, fX, fY) >= 0)) ||
                ((fY2 <= fY) && (fY < fY1) && (areaTwice(fX1, fY1, fX2, fY2, fX, fY) >= 0)) );
    }

    public void fillVolume( BitSet kMask, int xDim, int yDim, boolean XOR, int polarity )
    {
        if ( size() == 0 )
        {
            return;
        }
        getImageBoundingBox();
        if ( m_iPlane == ZPLANE )
        {
            int iXMin = (int)(m_akImageMinMax[0].X);
            int iXMax = (int)(m_akImageMinMax[1].X);

            int iYMin = (int)(m_akImageMinMax[0].Y);
            int iYMax = (int)(m_akImageMinMax[1].Y);

            int[][] aaiCrossingPoints = new int[iXMax - iXMin + 1][];
            int[] aiNumCrossings = new int[iXMax - iXMin + 1];

            for (int i = 0; i < (iXMax - iXMin + 1); i++) {
                aaiCrossingPoints[i] = new int[size()+2];
            }

            outlineRegion(aaiCrossingPoints, aiNumCrossings, iXMin, iXMax);
            fill(aaiCrossingPoints, aiNumCrossings, iXMin, iYMin, iXMax, iYMax, (int)elementAt(0).Z, 
                    kMask, xDim, yDim, XOR, polarity );
        }
        else if ( m_iPlane == XPLANE )
        {

            int iYMin = (int)(m_akImageMinMax[0].Y);
            int iYMax = (int)(m_akImageMinMax[1].Y);
            
            int iZMin = (int)(m_akImageMinMax[0].Z);
            int iZMax = (int)(m_akImageMinMax[1].Z);

            int[][] aaiCrossingPoints = new int[iYMax - iYMin + 1][];
            int[] aiNumCrossings = new int[iYMax - iYMin + 1];

            for (int i = 0; i < (iYMax - iYMin + 1); i++) {
                aaiCrossingPoints[i] = new int[size()+2];
            }

            outlineRegion_X(aaiCrossingPoints, aiNumCrossings, iYMin, iYMax);
            fill_X(aaiCrossingPoints, aiNumCrossings, iYMin, iZMin, iYMax, iZMax, (int)elementAt(0).X, 
                    kMask, xDim, yDim, XOR, polarity );            
        }
        else
        {
            int iXMin = (int)(m_akImageMinMax[0].X);
            int iXMax = (int)(m_akImageMinMax[1].X);
            
            int iZMin = (int)(m_akImageMinMax[0].Z);
            int iZMax = (int)(m_akImageMinMax[1].Z);

            int[][] aaiCrossingPoints = new int[iXMax - iXMin + 1][];
            int[] aiNumCrossings = new int[iXMax - iXMin + 1];

            for (int i = 0; i < (iXMax - iXMin + 1); i++) {
                aaiCrossingPoints[i] = new int[size()+2];
            }

            outlineRegion_Y(aaiCrossingPoints, aiNumCrossings, iXMin, iXMax);
            fill_Y(aaiCrossingPoints, aiNumCrossings, iXMin, iZMin, iXMax, iZMax, (int)elementAt(0).Y, 
                    kMask, xDim, yDim, XOR, polarity );      
            
        }

    }


    public void fillVolume( ModelImage kVolume, BitSet kMask, boolean bIntersection, int iValue )
    {
        if ( size() == 0 )
        {
            return;
        }
        getImageBoundingBox();
        if ( m_iPlane == ZPLANE )
        {
            int iXMin = (int)(m_akImageMinMax[0].X);
            int iXMax = (int)(m_akImageMinMax[1].X);

            int iYMin = (int)(m_akImageMinMax[0].Y);
            int iYMax = (int)(m_akImageMinMax[1].Y);

            int[][] aaiCrossingPoints = new int[iXMax - iXMin + 1][];
            int[] aiNumCrossings = new int[iXMax - iXMin + 1];

            for (int i = 0; i < (iXMax - iXMin + 1); i++) {
                aaiCrossingPoints[i] = new int[size()+2];
            }

            outlineRegion(aaiCrossingPoints, aiNumCrossings, iXMin, iXMax);
            fill(aaiCrossingPoints, aiNumCrossings, iXMin, iYMin, iXMax, iYMax, (int)elementAt(0).Z, kVolume, kMask, bIntersection, iValue);              
        }
        else if ( m_iPlane == XPLANE )
        {

            int iYMin = (int)(m_akImageMinMax[0].Y);
            int iYMax = (int)(m_akImageMinMax[1].Y);
            
            int iZMin = (int)(m_akImageMinMax[0].Z);
            int iZMax = (int)(m_akImageMinMax[1].Z);

            int[][] aaiCrossingPoints = new int[iYMax - iYMin + 1][];
            int[] aiNumCrossings = new int[iYMax - iYMin + 1];

            for (int i = 0; i < (iYMax - iYMin + 1); i++) {
                aaiCrossingPoints[i] = new int[size()+2];
            }

            outlineRegion_X(aaiCrossingPoints, aiNumCrossings, iYMin, iYMax);
            fill_X(aaiCrossingPoints, aiNumCrossings, iYMin, iZMin, iYMax, iZMax, (int)elementAt(0).X, kVolume, kMask, bIntersection, iValue);              
        }
        else
        {
            int iXMin = (int)(m_akImageMinMax[0].X);
            int iXMax = (int)(m_akImageMinMax[1].X);
            
            int iZMin = (int)(m_akImageMinMax[0].Z);
            int iZMax = (int)(m_akImageMinMax[1].Z);

            int[][] aaiCrossingPoints = new int[iXMax - iXMin + 1][];
            int[] aiNumCrossings = new int[iXMax - iXMin + 1];

            for (int i = 0; i < (iXMax - iXMin + 1); i++) {
                aaiCrossingPoints[i] = new int[size()+2];
            }

            outlineRegion_Y(aaiCrossingPoints, aiNumCrossings, iXMin, iXMax);
            fill_Y(aaiCrossingPoints, aiNumCrossings, iXMin, iZMin, iXMax, iZMax, (int)elementAt(0).Y, kVolume, kMask, bIntersection, iValue);           
        }

    }

    /**
     * fill: fill the sculpt outline drawn by the user. Pixels are determined to be inside or outside the sculpt region
     * based on the parameters, aaiCrossingPoints and aiNumCrossings, using a scan-conversion algorithm that traverses
     * each row and column of the bounding box of the sculpt region coloring inside points as it goes.
     *
     * @param  aaiCrossingPoints  DOCUMENT ME!
     * @param  aiNumCrossings     DOCUMENT ME!
     */
    protected void fill(int[][] aaiCrossingPoints, int[] aiNumCrossings,
            int iXMin, int iYMin, int iXMax, int iYMax, int iZ,
            ModelImage kVolume, BitSet kMask, boolean bIntersection, int iValue)
    {
        int iColumn = 0;
        /* Loop over the width of the sculpt region bounding-box: */
        for (int iX = iXMin; iX < iXMax; iX++) {
            boolean bInside = false;

            /* Loop over the height of the sculpt region bounding-box: */
            for (int iY = iYMin; iY < iYMax; iY++) {

                /* loop over each crossing point for this column: */
                for (int iCross = 0; iCross < aiNumCrossings[iColumn]; iCross++) {

                    if (iY == aaiCrossingPoints[iColumn][iCross]) {

                        /* Each time an edge is cross the point alternates
                         * from outside to inside: */
                        bInside = !bInside;
                    }
                }

                if (bInside == true) {

                    /* The current pixel is inside the sculpt region.  Get the
                     * image color from the canvas image and alpha-blend the sculpt color ontop, storing the result in
                     * the canvas image.
                     */

                    if ( bIntersection )
                    {
                        int iTemp = kVolume.getInt( iX, iY, iZ );

                        if ( kMask != null )
                        {
                            int iIndex = (int)iZ * kVolume.getExtents()[0] * kVolume.getExtents()[1];
                            iIndex += iY * kVolume.getExtents()[0];
                            iIndex += iX;
                            if ( iValue == 0 )
                            {
                                kMask.set( iIndex );
                            }
                            else if ( kMask.get( iIndex ) )
                            {
                                kMask.set( iIndex );
                            }
                        }
                        else
                        {
                            if ( iValue == 0 )
                            {
                                kVolume.set( iX, iY, iZ, 85 );
                            }
                            else if ( iTemp != 0 )
                            {
                                kVolume.set( iX, iY, iZ, 255 );
                            }
                        }
                    }
                    else
                    {
                        if ( kMask != null )
                        {
                            int iIndex = iZ * kVolume.getExtents()[0] * kVolume.getExtents()[1];
                            iIndex += iY * kVolume.getExtents()[0];
                            iIndex += iX;
                            kMask.set( iIndex );
                        }
                        else
                        {
                            kVolume.set( iX, iY, iZ, 255 );
                        }
                    }
                }
            }

            iColumn++;
        }
    }

    protected void fill(int[][] aaiCrossingPoints, int[] aiNumCrossings,
            int iXMin, int iYMin, int iXMax, int iYMax, int iZ, 
            BitSet kMask, int xDim, int yDim, boolean XOR, int polarity )
    {
        int iColumn = 0;
        /* Loop over the bounding-box: */
        for (int iX = iXMin; iX < iXMax; iX++) {
            boolean bInside = false;

            for (int iY = iYMin; iY < iYMax; iY++) {

                /* loop over each crossing point for this column: */
                for (int iCross = 0; iCross < aiNumCrossings[iColumn]; iCross++) {
                    if (iY == aaiCrossingPoints[iColumn][iCross]) {

                        /* Each time an edge is cross the point alternates
                         * from outside to inside: */
                        bInside = !bInside;
                    }
                }

                if (bInside == true) {
                    if ( kMask != null )
                    {
                        int iIndex = iZ * xDim * yDim;
                        iIndex += iY * xDim;
                        iIndex += iX;
                        if (polarity == VOI.ADDITIVE) {
                            if (XOR && kMask.get(iIndex)) {
                                kMask.clear(iIndex);
                            } else {
                                kMask.set(iIndex);
                            }
                        }
                        else if (polarity == VOI.SUBTRACTIVE) {
                            kMask.clear(iIndex);
                        } 
                    }
                }
            }

            iColumn++;
        }
    }
    /**
     * fill: fill the sculpt outline drawn by the user. Pixels are determined to be inside or outside the sculpt region
     * based on the parameters, aaiCrossingPoints and aiNumCrossings, using a scan-conversion algorithm that traverses
     * each row and column of the bounding box of the sculpt region coloring inside points as it goes.
     *
     * @param  aaiCrossingPoints  DOCUMENT ME!
     * @param  aiNumCrossings     DOCUMENT ME!
     */
    protected void fill_Y(int[][] aaiCrossingPoints, int[] aiNumCrossings,
            int iXMin, int iZMin, int iXMax, int iZMax, int iY,
            ModelImage kVolume, BitSet kMask, boolean bIntersection, int iValue)
    {
        int iColumn = 0;
        /* Loop over the width of the sculpt region bounding-box: */
        for (int iX = iXMin; iX < iXMax; iX++) {
            boolean bInside = false;

            /* Loop over the height of the sculpt region bounding-box: */
            for (int iZ = iZMin; iZ < iZMax; iZ++) {

                /* loop over each crossing point for this column: */
                for (int iCross = 0; iCross < aiNumCrossings[iColumn]; iCross++) {

                    if (iZ == aaiCrossingPoints[iColumn][iCross]) {

                        /* Each time an edge is cross the point alternates
                         * from outside to inside: */
                        bInside = !bInside;
                    }
                }

                if (bInside == true) {

                    /* The current pixel is inside the sculpt region.  Get the
                     * image color from the canvas image and alpha-blend the sculpt color ontop, storing the result in
                     * the canvas image.
                     */

                    if ( bIntersection )
                    {
                        int iTemp = kVolume.getInt( iX, iY, iZ );

                        if ( kMask != null )
                        {
                            int iIndex = (int)iZ * kVolume.getExtents()[0] * kVolume.getExtents()[1];
                            iIndex += iY * kVolume.getExtents()[0];
                            iIndex += iX;
                            if ( iValue == 0 )
                            {
                                kMask.set( iIndex );
                            }
                            else if ( kMask.get( iIndex ) )
                            {
                                kMask.set( iIndex );
                            }
                        }
                        else
                        {
                            if ( iValue == 0 )
                            {
                                kVolume.set( iX, iY, iZ, 85 );
                            }
                            else if ( iTemp != 0 )
                            {
                                kVolume.set( iX, iY, iZ, 255 );
                            }
                        }
                    }
                    else
                    {
                        if ( kMask != null )
                        {
                            int iIndex = iZ * kVolume.getExtents()[0] * kVolume.getExtents()[1];
                            iIndex += iY * kVolume.getExtents()[0];
                            iIndex += iX;
                            kMask.set( iIndex );
                        }
                        else
                        {
                            kVolume.set( iX, iY, iZ, 255 );
                        }
                    }
                }
            }

            iColumn++;
        }
    }

    protected void fill_Y(int[][] aaiCrossingPoints, int[] aiNumCrossings,
            int iXMin, int iZMin, int iXMax, int iZMax, int iY, 
            BitSet kMask, int xDim, int yDim, boolean XOR, int polarity )
    {
        int iColumn = 0;
        /* Loop over the bounding-box: */
        for (int iX = iXMin; iX < iXMax; iX++) {
            boolean bInside = false;

            for (int iZ = iZMin; iZ < iZMax; iZ++) {

                /* loop over each crossing point for this column: */
                for (int iCross = 0; iCross < aiNumCrossings[iColumn]; iCross++) {
                    if (iZ == aaiCrossingPoints[iColumn][iCross]) {

                        /* Each time an edge is cross the point alternates
                         * from outside to inside: */
                        bInside = !bInside;
                    }
                }

                if (bInside == true) {
                    if ( kMask != null )
                    {
                        int iIndex = iZ * xDim * yDim;
                        iIndex += iY * xDim;
                        iIndex += iX;
                        if (polarity == VOI.ADDITIVE) {
                            if (XOR && kMask.get(iIndex)) {
                                kMask.clear(iIndex);
                            } else {
                                kMask.set(iIndex);
                            }
                        }
                        else if (polarity == VOI.SUBTRACTIVE) {
                            kMask.clear(iIndex);
                        } 
                    }
                }
            }

            iColumn++;
        }
    }    /**
     * fill: fill the sculpt outline drawn by the user. Pixels are determined to be inside or outside the sculpt region
     * based on the parameters, aaiCrossingPoints and aiNumCrossings, using a scan-conversion algorithm that traverses
     * each row and column of the bounding box of the sculpt region coloring inside points as it goes.
     *
     * @param  aaiCrossingPoints  DOCUMENT ME!
     * @param  aiNumCrossings     DOCUMENT ME!
     */
    protected void fill_X(int[][] aaiCrossingPoints, int[] aiNumCrossings,
            int iYMin, int iZMin, int iYMax, int iZMax, int iX,
            ModelImage kVolume, BitSet kMask, boolean bIntersection, int iValue)
    {
        int iColumn = 0;
        /* Loop over the width of the sculpt region bounding-box: */
        for (int iY = iYMin; iY < iYMax; iY++) {
            boolean bInside = false;

            /* Loop over the height of the sculpt region bounding-box: */
            for (int iZ = iZMin; iZ < iZMax; iZ++) {

                /* loop over each crossing point for this column: */
                for (int iCross = 0; iCross < aiNumCrossings[iColumn]; iCross++) {

                    if (iZ == aaiCrossingPoints[iColumn][iCross]) {

                        /* Each time an edge is cross the point alternates
                         * from outside to inside: */
                        bInside = !bInside;
                    }
                }

                if (bInside == true) {

                    /* The current pixel is inside the sculpt region.  Get the
                     * image color from the canvas image and alpha-blend the sculpt color ontop, storing the result in
                     * the canvas image.
                     */

                    if ( bIntersection )
                    {
                        int iTemp = kVolume.getInt( iX, iY, iZ );

                        if ( kMask != null )
                        {
                            int iIndex = (int)iZ * kVolume.getExtents()[0] * kVolume.getExtents()[1];
                            iIndex += iY * kVolume.getExtents()[0];
                            iIndex += iX;
                            if ( iValue == 0 )
                            {
                                kMask.set( iIndex );
                            }
                            else if ( kMask.get( iIndex ) )
                            {
                                kMask.set( iIndex );
                            }
                        }
                        else
                        {
                            if ( iValue == 0 )
                            {
                                kVolume.set( iX, iY, iZ, 85 );
                            }
                            else if ( iTemp != 0 )
                            {
                                kVolume.set( iX, iY, iZ, 255 );
                            }
                        }
                    }
                    else
                    {
                        if ( kMask != null )
                        {
                            int iIndex = iZ * kVolume.getExtents()[0] * kVolume.getExtents()[1];
                            iIndex += iY * kVolume.getExtents()[0];
                            iIndex += iX;
                            kMask.set( iIndex );
                        }
                        else
                        {
                            kVolume.set( iX, iY, iZ, 255 );
                        }
                    }
                }
            }

            iColumn++;
        }
    }


    protected void fill_X(int[][] aaiCrossingPoints, int[] aiNumCrossings,
            int iYMin, int iZMin, int iYMax, int iZMax, int iX, 
            BitSet kMask, int xDim, int yDim, boolean XOR, int polarity )
    {
        int iColumn = 0;
        /* Loop over the bounding-box: */
        for (int iY = iYMin; iY < iYMax; iY++) {
            boolean bInside = false;

            for (int iZ = iZMin; iZ < iZMax; iZ++) {

                /* loop over each crossing point for this column: */
                for (int iCross = 0; iCross < aiNumCrossings[iColumn]; iCross++) {
                    if (iZ == aaiCrossingPoints[iColumn][iCross]) {

                        /* Each time an edge is cross the point alternates
                         * from outside to inside: */
                        bInside = !bInside;
                    }
                }

                if (bInside == true) {
                    if ( kMask != null )
                    {
                        int iIndex = iZ * xDim * yDim;
                        iIndex += iY * xDim;
                        iIndex += iX;
                        if (polarity == VOI.ADDITIVE) {
                            if (XOR && kMask.get(iIndex)) {
                                kMask.clear(iIndex);
                            } else {
                                kMask.set(iIndex);
                            }
                        }
                        else if (polarity == VOI.SUBTRACTIVE) {
                            kMask.clear(iIndex);
                        } 
                    }
                }
            }

            iColumn++;
        }
    }

    /**
     * This function computes the set of spans indicated by column crossings for the sculpt outline drawn by the user,
     * by doing a polygon scan conversion in gridded space. The outline must be closed with last point = first point.
     *
     * @param  aaiCrossingPoints  DOCUMENT ME!
     * @param  aiNumCrossings     DOCUMENT ME!
     */
    public void outlineRegion_X(int[][] aaiCrossingPoints, int[] aiNumCrossings, int iYMin, int iYMax)
    {        
        int iNumPts = size();
        double dNudge = 0.1;       
        double[][][] aaadEdgeList = new double[iNumPts][2][2];

        for (int i = 0; i < (iNumPts - 1); i++) {
            aaadEdgeList[i][0][0] = elementAt(i).Y - dNudge;
            aaadEdgeList[i][0][1] = elementAt(i).Z - dNudge;
            aaadEdgeList[i][1][0] = elementAt(i+1).Y - dNudge;
            aaadEdgeList[i][1][1] = elementAt(i+1).Z - dNudge;
        }
        aaadEdgeList[iNumPts-1][0][0] = lastElement().Y - dNudge;
        aaadEdgeList[iNumPts-1][0][1] = lastElement().Z - dNudge;
        aaadEdgeList[iNumPts-1][1][0] = firstElement().Y - dNudge;
        aaadEdgeList[iNumPts-1][1][1] = firstElement().Z - dNudge;
        iNumPts++;

        /*
         * Compute the crossing points for this column and produce spans.
         */
        for (int iColumn = iYMin; iColumn < iYMax; iColumn++) {
            int iIndex = iColumn - iYMin;

            /* for each edge, figure out if it crosses this column and add its
             * crossing point to the list if so. */
            aiNumCrossings[iIndex] = 0;

            for (int iPoint = 0; iPoint < (iNumPts - 1); iPoint++) {
                double dY0 = aaadEdgeList[iPoint][0][0];
                double dY1 = aaadEdgeList[iPoint][1][0];
                double dZ0 = aaadEdgeList[iPoint][0][1];
                double dZ1 = aaadEdgeList[iPoint][1][1];
                double dMinY = (dY0 <= dY1) ? dY0 : dY1;
                double dMaxY = (dY0 > dY1) ? dY0 : dY1;

                if ((dMinY < iColumn) && (dMaxY > iColumn)) {

                    /* The edge crosses this column, so compute the
                     * intersection.
                     */
                    double dDY = dY1 - dY0;
                    double dDZ = dZ1 - dZ0;
                    double dM = (dDY == 0) ? 0 : (dDZ / dDY);
                    double dB = (dDY == 0) ? 0 : (((dY1 * dZ0) - (dZ1 * dY0)) / dDY);

                    double dZCross = (dM * iColumn) + dB;
                    double dRound = 0.5;
                    aaiCrossingPoints[iIndex][aiNumCrossings[iIndex]] = (dZCross < 0) ? (int) (dZCross - dRound) :
                        (int) (dZCross + dRound);
                    aiNumCrossings[iIndex]++;
                }
            }

            /* sort the set of crossings for this column: */
            sortCrossingPoints(aaiCrossingPoints[iIndex], aiNumCrossings[iIndex]);
        }
        aaadEdgeList = null;
    }



    public void outlineRegion(int[][] aaiCrossingPoints, int[] aiNumCrossings, int iXMin, int iXMax)
    {        
        int iNumPts = size();
        double dNudge = 0.1;       
        double[][][] aaadEdgeList = new double[iNumPts][2][2];

        for (int i = 0; i < (iNumPts - 1); i++) {
            aaadEdgeList[i][0][0] = elementAt(i).X - dNudge;
            aaadEdgeList[i][0][1] = elementAt(i).Y - dNudge;
            aaadEdgeList[i][1][0] = elementAt(i+1).X - dNudge;
            aaadEdgeList[i][1][1] = elementAt(i+1).Y - dNudge;
        }
        aaadEdgeList[iNumPts-1][0][0] = lastElement().X - dNudge;
        aaadEdgeList[iNumPts-1][0][1] = lastElement().Y - dNudge;
        aaadEdgeList[iNumPts-1][1][0] = firstElement().X - dNudge;
        aaadEdgeList[iNumPts-1][1][1] = firstElement().Y - dNudge;
        iNumPts++;

        /*
         * Compute the crossing points for this column and produce spans.
         */
        for (int iColumn = iXMin; iColumn < iXMax; iColumn++) {
            int iIndex = iColumn - iXMin;

            /* for each edge, figure out if it crosses this column and add its
             * crossing point to the list if so. */
            aiNumCrossings[iIndex] = 0;

            for (int iPoint = 0; iPoint < (iNumPts - 1); iPoint++) {
                double dX0 = aaadEdgeList[iPoint][0][0];
                double dX1 = aaadEdgeList[iPoint][1][0];
                double dY0 = aaadEdgeList[iPoint][0][1];
                double dY1 = aaadEdgeList[iPoint][1][1];
                double dMinX = (dX0 <= dX1) ? dX0 : dX1;
                double dMaxX = (dX0 > dX1) ? dX0 : dX1;

                if ((dMinX < iColumn) && (dMaxX > iColumn)) {

                    /* The edge crosses this column, so compute the
                     * intersection.
                     */
                    double dDX = dX1 - dX0;
                    double dDY = dY1 - dY0;
                    double dM = (dDX == 0) ? 0 : (dDY / dDX);
                    double dB = (dDX == 0) ? 0 : (((dX1 * dY0) - (dY1 * dX0)) / dDX);

                    double dYCross = (dM * iColumn) + dB;
                    double dRound = 0.5;
                    aaiCrossingPoints[iIndex][aiNumCrossings[iIndex]] = (dYCross < 0) ? (int) (dYCross - dRound) :
                        (int) (dYCross + dRound);
                    aiNumCrossings[iIndex]++;
                }
            }

            /* sort the set of crossings for this column: */
            sortCrossingPoints(aaiCrossingPoints[iIndex], aiNumCrossings[iIndex]);
        }
        aaadEdgeList = null;
    }



    public void outlineRegion_Y(int[][] aaiCrossingPoints, int[] aiNumCrossings, int iXMin, int iXMax)
    {        
        int iNumPts = size();
        double dNudge = 0.1;       
        double[][][] aaadEdgeList = new double[iNumPts][2][2];

        for (int i = 0; i < (iNumPts - 1); i++) {
            aaadEdgeList[i][0][0] = elementAt(i).X - dNudge;
            aaadEdgeList[i][0][1] = elementAt(i).Z - dNudge;
            aaadEdgeList[i][1][0] = elementAt(i+1).X - dNudge;
            aaadEdgeList[i][1][1] = elementAt(i+1).Z - dNudge;
        }
        aaadEdgeList[iNumPts-1][0][0] = lastElement().X - dNudge;
        aaadEdgeList[iNumPts-1][0][1] = lastElement().Z - dNudge;
        aaadEdgeList[iNumPts-1][1][0] = firstElement().X - dNudge;
        aaadEdgeList[iNumPts-1][1][1] = firstElement().Z - dNudge;
        iNumPts++;

        /*
         * Compute the crossing points for this column and produce spans.
         */
        for (int iColumn = iXMin; iColumn < iXMax; iColumn++) {
            int iIndex = iColumn - iXMin;

            /* for each edge, figure out if it crosses this column and add its
             * crossing point to the list if so. */
            aiNumCrossings[iIndex] = 0;

            for (int iPoint = 0; iPoint < (iNumPts - 1); iPoint++) {
                double dX0 = aaadEdgeList[iPoint][0][0];
                double dX1 = aaadEdgeList[iPoint][1][0];
                double dY0 = aaadEdgeList[iPoint][0][1];
                double dY1 = aaadEdgeList[iPoint][1][1];
                double dMinX = (dX0 <= dX1) ? dX0 : dX1;
                double dMaxX = (dX0 > dX1) ? dX0 : dX1;

                if ((dMinX < iColumn) && (dMaxX > iColumn)) {

                    /* The edge crosses this column, so compute the
                     * intersection.
                     */
                    double dDX = dX1 - dX0;
                    double dDY = dY1 - dY0;
                    double dM = (dDX == 0) ? 0 : (dDY / dDX);
                    double dB = (dDX == 0) ? 0 : (((dX1 * dY0) - (dY1 * dX0)) / dDX);

                    double dYCross = (dM * iColumn) + dB;
                    double dRound = 0.5;
                    aaiCrossingPoints[iIndex][aiNumCrossings[iIndex]] = (dYCross < 0) ? (int) (dYCross - dRound) :
                        (int) (dYCross + dRound);
                    aiNumCrossings[iIndex]++;
                }
            }

            /* sort the set of crossings for this column: */
            sortCrossingPoints(aaiCrossingPoints[iIndex], aiNumCrossings[iIndex]);
        }
        aaadEdgeList = null;
    }



    public static void outlineRegion(int[][] aaiCrossingPoints, int[] aiNumCrossings,
            int iXMin, int iYMin, int iXMax, int iYMax, Vector3f[] akPoints )
    {        
        int iNumPts = akPoints.length;
        double dNudge = 0.1;       
        double[][][] aaadEdgeList = new double[iNumPts][2][2];

        for (int i = 0; i < (iNumPts - 1); i++) {
            aaadEdgeList[i][0][0] = akPoints[i].X - dNudge;
            aaadEdgeList[i][0][1] = akPoints[i].Y - dNudge;
            aaadEdgeList[i][1][0] = akPoints[i+1].X - dNudge;
            aaadEdgeList[i][1][1] = akPoints[i+1].Y - dNudge;
        }

        /*
         * Compute the crossing points for this column and produce spans.
         */
        for (int iColumn = iXMin; iColumn < iXMax; iColumn++) {
            int iIndex = iColumn - iXMin;

            /* for each edge, figure out if it crosses this column and add its
             * crossing point to the list if so. */
            aiNumCrossings[iIndex] = 0;

            for (int iPoint = 0; iPoint < (iNumPts - 1); iPoint++) {
                double dX0 = aaadEdgeList[iPoint][0][0];
                double dX1 = aaadEdgeList[iPoint][1][0];
                double dY0 = aaadEdgeList[iPoint][0][1];
                double dY1 = aaadEdgeList[iPoint][1][1];
                double dMinX = (dX0 <= dX1) ? dX0 : dX1;
                double dMaxX = (dX0 > dX1) ? dX0 : dX1;

                if ((dMinX < iColumn) && (dMaxX > iColumn)) {

                    /* The edge crosses this column, so compute the
                     * intersection.
                     */
                    double dDX = dX1 - dX0;
                    double dDY = dY1 - dY0;
                    double dM = (dDX == 0) ? 0 : (dDY / dDX);
                    double dB = (dDX == 0) ? 0 : (((dX1 * dY0) - (dY1 * dX0)) / dDX);

                    double dYCross = (dM * iColumn) + dB;
                    double dRound = 0.5;
                    aaiCrossingPoints[iIndex][aiNumCrossings[iIndex]] = (dYCross < 0) ? (int) (dYCross - dRound) :
                        (int) (dYCross + dRound);
                    aiNumCrossings[iIndex]++;
                }
            }

            /* sort the set of crossings for this column: */
            sortCrossingPoints(aaiCrossingPoints[iIndex], aiNumCrossings[iIndex]);
        }
        aaadEdgeList = null;
    }




    /**
     * Sorts the edge crossing points in place.
     *
     * @param  aiList        list of positions
     * @param  iNumElements  number of positions.
     */
    protected static void sortCrossingPoints(int[] aiList, int iNumElements) {
        boolean bDidSwap = true;

        while (bDidSwap) {
            bDidSwap = false;

            for (int iPoint = 0; iPoint < (iNumElements - 1); iPoint++) {

                if (aiList[iPoint] > aiList[iPoint + 1]) {
                    int iTmp = aiList[iPoint];
                    aiList[iPoint] = aiList[iPoint + 1];
                    aiList[iPoint + 1] = iTmp;
                    bDidSwap = true;
                }
            }
        }
    }

}
