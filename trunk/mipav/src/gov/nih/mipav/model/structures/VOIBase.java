package gov.nih.mipav.model.structures;

import gov.nih.mipav.MipavMath;
import gov.nih.mipav.model.file.FileInfoBase;
import gov.nih.mipav.view.dialogs.JDialogVOIStatistics;
import gov.nih.mipav.view.renderer.WildMagic.Render.VolumeImage;
import gov.nih.mipav.view.renderer.WildMagic.Render.VolumeVOI;

import java.awt.event.KeyEvent;
import java.util.Arrays;
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

    public static final int UPPER_LEFT = 0;
    public static final int UPPER_RIGHT = 1;
    public static final int LOWER_LEFT = 2;
    public static final int LOWER_RIGHT = 3;
    public static final int UPPER_MIDDLE = 4;
    public static final int LEFT_MIDDLE = 5;
    public static final int RIGHT_MIDDLE = 6;
    public static final int LOWER_MIDDLE = 7;
    
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
    /** Automatically generate the label, unless it is set explicitly: */
    protected boolean autoLabel = true;

    /** Keeps track of the last near point value (for keyboard moving of individual pts. */
    protected int lastPoint = 0;

    /** Flag used to indicate if the cursor is near a point of the VOI member. */
    protected int nearPoint = NOT_A_POINT;

    /** Flag used to indicate if the cursor is near a point on the VOI bounding box. */
    protected int nearBoundPoint = NOT_A_POINT;

    /** Reference to the containing VOI object. */
    protected VOI voiGroup;

    /**
     * If doGeometricCenterLabel = true and if active == false and if closed =
     * true, execute drawGeometricCenterLabel when in drawSelf
     */
    private boolean doGeometricCenterLabel = false;

    /** Set to true if the contour changes, so the mask will update when needed. */
    protected boolean m_bUpdateMask = true;
    
    /** Set to true if the contour bounding-box needs updating. */
    protected boolean m_bUpdateBounds = true;    
    /** Set to true if the geometric center needs updating. */
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
    /** True is this is a QuickLUT contour. */
    protected boolean m_bQuickLUT = false;

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
    
    protected BitSet m_kMask;
    private Vector<Vector3f> m_kMaskPositions = new Vector<Vector3f>();
    private Vector3f m_kPositionSum = new Vector3f();

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
     * Sorts the edge crossing points in place.
     *
     * @param  aiList        list of positions
     * @param  iNumElements  number of positions.
     */
    private static void sortCrossingPoints(float[] aiList, int iNumElements) {
        boolean bDidSwap = true;

        while (bDidSwap) {
            bDidSwap = false;

            for (int iPoint = 0; iPoint < (iNumElements - 1); iPoint++) {

                if (aiList[iPoint] > aiList[iPoint + 1]) {
                    float iTmp = aiList[iPoint];
                    aiList[iPoint] = aiList[iPoint + 1];
                    aiList[iPoint + 1] = iTmp;
                    bDidSwap = true;
                }
            }
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
    public float calcIntensity(ModelImage kImage, int t) {
        numPixels = 0;
        getMask();
        float sum = 0;
        for ( int i = 0; i < m_kMaskPositions.size(); i++ )
        {
            Vector3f kPos = m_kMaskPositions.elementAt(i);
            int x = (int)kPos.X;
            int y = (int)kPos.Y;
            int z = (int)kPos.Z;
            sum += kImage.getFloat(x,y,z,t);
            numPixels++;
        }
        return sum;
    }

    /**
     * Finds values contained within this contour, based on the rangeFlag, ignorMin and ignoreMax.
     * @param kImage input image.
     * @param ignoreMin intensity minimum.
     * @param ignoreMax intensity maximum.
     * @param rangeFlag flag indicating (between, outside, none).
     * @return list of values inside this contour that fit the parameters.
     */
    public Vector<Float> calcIntensity( ModelImage kImage, Vector3f kValues, float ignoreMin, float ignoreMax, int rangeFlag)
    {
        getMask();
        Vector<Float> values = new Vector<Float>();
        float fVal = 0;
        for ( int i = 0; i < m_kMaskPositions.size(); i++ )
        {
            Vector3f kPos = m_kMaskPositions.elementAt(i);
            int x = (int)kPos.X;
            int y = (int)kPos.Y;
            int z = (int)kPos.Z;
            fVal = kImage.getFloat(x,y,z);
            if ( !inRange( ignoreMin, ignoreMax, fVal, rangeFlag ) )
            {
                values.add( new Float(fVal) );
                if ( i == 0 )
                {
                    kValues.X = fVal;
                    kValues.Y = fVal;
                }
                kValues.X = Math.min( kValues.X, fVal );
                kValues.Y = Math.max( kValues.Y, fVal );
                kValues.Z += fVal;
            }
        }
        return values;
    }

    /**
     * Calculates the total intensity contained within this contour, equal to of greater than the threshold.
     * @param kImage input image
     * @param threshold 
     * @return total intensity of image contained within this contour, greater than threshold.
     */
    public float calcIntensityThreshold(ModelImage kImage, float threshold, int t) {
        numPixels = 0;
        getMask();
        float sum = 0;
        for ( int i = 0; i < m_kMaskPositions.size(); i++ )
        {
            Vector3f kPos = m_kMaskPositions.elementAt(i);
            int x = (int)kPos.X;
            int y = (int)kPos.Y;
            int z = (int)kPos.Z;
            float val = kImage.getFloat(x,y,z, t);
            if ( val >= threshold )
            {
                sum += val;
                numPixels++;
            }
        }
        return sum;
    }


    /**
     * Finds values contained within this contour, based on the rangeFlag, ignorMin and ignoreMax.
     * @param kImage input image.
     * @param ignoreMin intensity minimum.
     * @param ignoreMax intensity maximum.
     * @param rangeFlag flag indicating (between, outside, none).
     * @return list of values inside this contour that fit the parameters.
     */
    public Vector<ColorRGB> calcRGBIntensity(ModelImage kImage, ColorRGB kMin, ColorRGB kMax, ColorRGB kSum, float ignoreMin, float ignoreMax, int rangeFlag) {
        Vector<ColorRGB> values = new Vector<ColorRGB>();
        float r, g, b;
        getMask();
        for ( int i = 0; i < m_kMaskPositions.size(); i++ )
        {
            Vector3f kPos = m_kMaskPositions.elementAt(i);
            int x = (int)kPos.X;
            int y = (int)kPos.Y;
            int z = (int)kPos.Z;

            r = kImage.getFloatC(x,y,z,1);
            g = kImage.getFloatC(x,y,z,2);
            b = kImage.getFloatC(x,y,z,3);
            if ( !inRange( ignoreMin, ignoreMax, r, rangeFlag ) &&
                    !inRange( ignoreMin, ignoreMax, g, rangeFlag ) &&
                    !inRange( ignoreMin, ignoreMax, b, rangeFlag ) )
            {
                ColorRGB kColor = new ColorRGB(r,g,b);
                values.add( kColor );
                if ( i == 0 )
                {
                    kMin.Copy(kColor);
                    kMax.Copy(kColor);
                }
                kMin.Min(kColor);
                kMax.Max(kColor);
                kSum.Set( kSum.R + kColor.R,
                        kSum.G + kColor.G,
                        kSum.B + kColor.B );
            }
        }
        return values;
    }

    /**
     * Calculates the total intensity contained within this contour for the input color channel.
     * @param kImage
     * @param RGorB (Red = 0; Green = 1; Blue = 2)
     * @return total intensity contained within this contour, for the input image and color channel.
     */
    public float calcRGBIntensity(ModelImage kImage, int RGorB) {
        numPixels = 0;
        getMask();
        float sum = 0;
        for ( int i = 0; i < m_kMaskPositions.size(); i++ )
        {
            Vector3f kPos = m_kMaskPositions.elementAt(i);
            int x = (int)kPos.X;
            int y = (int)kPos.Y;
            int z = (int)kPos.Z;

            sum += kImage.getFloatC(x,y,z,RGorB+1);
            numPixels++;
        }
        return sum;
    }

    /**
     * Calculates the total intensity contained within this contour for the input color channel,
     * that is greater than or equal to the input threshold value.
     * @param kImage
     * @param RGorB (Red = 0; Green = 1; Blue = 2)
     * @param threshold
     * @return total intensity greater than the threshold contained within this contour, for the input image and color channel.
     */
    public float calcRGBIntensityThreshold(ModelImage kImage, int RGorB, float threshold) {
        numPixels = 0;
        getMask();
        float sum = 0;
        for ( int i = 0; i < m_kMaskPositions.size(); i++ )
        {
            Vector3f kPos = m_kMaskPositions.elementAt(i);
            int x = (int)kPos.X;
            int y = (int)kPos.Y;
            int z = (int)kPos.Z;

            float val = kImage.getFloatC(x,y,z,RGorB+1);
            if ( val >= threshold )
            {
                sum += val;
                numPixels++;
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
        Vector3f[] kBounds = getImageBoundingBox();
        if ( iX < kBounds[0].X || iX > kBounds[1].X ||
                iY < kBounds[0].Y || iY > kBounds[1].Y  )
        {
            return false;
        }        
        for ( int i = 0; i < size(); i++ )
        {
            if ( elementAt(i).X == iX && elementAt(i).Y == iY )
            {
                return true;
            }
        }
        getMask();
        int xDim = (int)kBounds[1].X;
        int yDim = (int)kBounds[1].Y;
        for ( int i = (int)kBounds[0].Z; i <= kBounds[1].Z; i++ )
        {
            int index = (int)(i * xDim * yDim + iY * xDim + iX);
            if( m_kMask.get( index ) )
            {
                return true;
            }
        }
        return false;
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
        Vector3f[] kBounds = getImageBoundingBox();
        if ( iX < kBounds[0].X || iX > kBounds[1].X ||
                iY < kBounds[0].Y || iY > kBounds[1].Y ||
                iZ < kBounds[0].Z || iZ > kBounds[1].Z   )
        {
            return false;
        }
        getMask();
        int xDim = (int)kBounds[1].X;
        int yDim = (int)kBounds[1].Y;
                
        int index = (int)(iZ * xDim * yDim + iY * xDim + iX);
        return m_kMask.get(index );
    }

    /**
     * Creates the VolumeVOI data structure for rendering this contour in the GPU VolumeRenderer
     * @param kVolumeImage
     * @param kTranslate
     * @return VolumeVOI
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
     * @param iPos position on the curve to delete.
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
     * @param iX input x-position
     * @param iY input y-position
     * @param iZ input z-position
     * @return smallest distance from the input to the contour.
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
     * Renders this contour into the input mask.
     * @param kMask input mask.
     * @param xDim x-dimensions of the input mask.
     * @param yDim y-dimension of the input mask.
     * @param XOR indicates that nested VOI contours will be exclusive ORed with other contours of the VOI 
     * @param polarity indicates if the VOI should mask ones or mask as zeros.
     */
    private void fillVolume( BitSet kMask, int xDim, int yDim, boolean XOR, int polarity )
    {
        if ( size() == 0 || kMask == null )
        {
            return;
        }
        getImageBoundingBox();
        if ( m_iPlane == NOT_A_PLANE )
        {
            m_bUpdatePlane = true;
            getPlane();
        }
        if ( m_iPlane == ZPLANE )
        {
            //long time = System.currentTimeMillis();
            //fillZ((int)elementAt(0).Z, 
            //        kMask, xDim, yDim, XOR, polarity );
            
            //System.out.println("fillZ " +(System.currentTimeMillis() - time));
            
            
            //time = System.currentTimeMillis();                        
            int iXMin = (int)(m_akImageMinMax[0].X);
            int iXMax = (int)(m_akImageMinMax[1].X);

            int[] aiNumCrossings = new int[iXMax - iXMin + 1];
            float[][] aafCrossingPoints = new float[iXMax - iXMin + 1][];
            for (int i = 0; i < (iXMax - iXMin + 1); i++) {
                aafCrossingPoints[i] = new float[size()+2];
            }
            outlineRegion(aafCrossingPoints, aiNumCrossings, iXMin, iXMax);
            fill(aafCrossingPoints, aiNumCrossings, iXMin, iXMax, (int)elementAt(0).Z, 
                    kMask, xDim, yDim, XOR, polarity );
            //System.out.println("outlineRegion/fill " +(System.currentTimeMillis() - time));
            
        }
        else if ( m_iPlane == XPLANE )
        {
            //long time = System.currentTimeMillis();
            //fillX((int)elementAt(0).X, 
            //        kMask, xDim, yDim, XOR, polarity );
            
            //System.out.println("fillX " +(System.currentTimeMillis() - time));
            
            int iYMin = (int)(m_akImageMinMax[0].Y);
            int iYMax = (int)(m_akImageMinMax[1].Y);

            int[] aiNumCrossings = new int[iYMax - iYMin + 1];
            float[][] aafCrossingPoints = new float[iYMax - iYMin + 1][];
            for (int i = 0; i < (iYMax - iYMin + 1); i++) {
                aafCrossingPoints[i] = new float[size()+2];
            }

            int iZMax = (int)(m_akImageMinMax[1].Z);
            outlineRegion_X(aafCrossingPoints, aiNumCrossings, iYMin, iYMax);
            fill_X(aafCrossingPoints, aiNumCrossings, iYMin, iYMax, iZMax, (int)elementAt(0).X, 
                    kMask, xDim, yDim, XOR, polarity );
                                
        }
        else
        {
            //long time = System.currentTimeMillis();
            //fillY((int)elementAt(0).Y, 
            //        kMask, xDim, yDim, XOR, polarity );
            
            //System.out.println("fillY " +(System.currentTimeMillis() - time));
           
            int iXMin = (int)(m_akImageMinMax[0].X);
            int iXMax = (int)(m_akImageMinMax[1].X);

            int[] aiNumCrossings = new int[iXMax - iXMin + 1];
            float[][] aafCrossingPoints = new float[iXMax - iXMin + 1][];
            for (int i = 0; i < (iXMax - iXMin + 1); i++) {
                aafCrossingPoints[i] = new float[size()+2];
            }
            outlineRegion_Y(aafCrossingPoints, aiNumCrossings, iXMin, iXMax);
            fill_Y(aafCrossingPoints, aiNumCrossings, iXMin, iXMax, (int)elementAt(0).Y, 
                    kMask, xDim, yDim, XOR, polarity );      
            
        }
        
        m_bUpdateMask = false;
    }
    
    /**
     * Renders this contour into the input ModelImage or the input BitSet mask.
     * @param kVolume if non-null this contour is rendered into the ModelImage.
     * @param kMask if non-null this contour is rendered into the mask.
     * @param bIntersection when true the contour rendered as an intersection with other contours.
     * @param iValue value to write into the input image.
     */
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

            int[] aiNumCrossings = new int[iXMax - iXMin + 1];
            float[][] aafCrossingPoints = new float[iXMax - iXMin + 1][];
            for (int i = 0; i < (iXMax - iXMin + 1); i++) {
                aafCrossingPoints[i] = new float[size()+2];
            }
            outlineRegion(aafCrossingPoints, aiNumCrossings, iXMin, iXMax);
            fill(aafCrossingPoints, aiNumCrossings, iXMin, iXMax, (int)elementAt(0).Z, kVolume, kMask, bIntersection, iValue);              
        }
        else if ( m_iPlane == XPLANE )
        {

            int iYMin = (int)(m_akImageMinMax[0].Y);
            int iYMax = (int)(m_akImageMinMax[1].Y);


            int[] aiNumCrossings = new int[iYMax - iYMin + 1];
            float[][] aafCrossingPoints = new float[iYMax - iYMin + 1][];
            for (int i = 0; i < (iYMax - iYMin + 1); i++) {
                aafCrossingPoints[i] = new float[size()+2];
            }
            outlineRegion_X(aafCrossingPoints, aiNumCrossings, iYMin, iYMax);
            fill_X(aafCrossingPoints, aiNumCrossings, iYMin, iYMax, (int)elementAt(0).X, kVolume, kMask, bIntersection, iValue);              
        }
        else
        {
            int iXMin = (int)(m_akImageMinMax[0].X);
            int iXMax = (int)(m_akImageMinMax[1].X);

            int[] aiNumCrossings = new int[iXMax - iXMin + 1];
            float[][] aafCrossingPoints = new float[iXMax - iXMin + 1][];
            for (int i = 0; i < (iXMax - iXMin + 1); i++) {
                aafCrossingPoints[i] = new float[size()+2];
            }
            outlineRegion_Y(aafCrossingPoints, aiNumCrossings, iXMin, iXMax);
            fill_Y(aafCrossingPoints, aiNumCrossings, iXMin, iXMax, (int)elementAt(0).Y, kVolume, kMask, bIntersection, iValue);           
        }

    }


    /**
     * Finds the position/intensity along a VOI.
     *
     * @param   positions     Vector that is filled with the distance along the VOI in millimeters for example
     * @param   colors    the corresponding intensities along the line
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
     * Finds the positions and intensities along a line-segment of the VOI.
     * @param kStart start position on the VOI.
     * @param kEnd end position on the VOI
     * @param kImage input image to read intensity values from
     * @param positions output list of positions
     * @param colors output list of colors.
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
        int zD = kImage.getExtents().length > 2 ? kImage.getExtents()[2] : 1;

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
     * Gets the center of mass of the contour for a given image
     * @param kImage input image.
     * @return center of mass for this contour and the input image.
     */
    public Vector3f getCenterOfMass(ModelImage kImage) {
        getCenterOfMass(kImage, cenMassPt, 0);
        return cenMassPt;
    }

    /**
     * Gets the center of mass of the contour for the image and color-channel c.
     * @param kImage input image.
     * @param centerPt output point.
     * @param c color channel.
     */
    public void getCenterOfMass(ModelImage kImage, Vector3f centerPt, int c) {
        getMask();
        float sum = 0;
        double sumX = 0.0;
        double sumY = 0.0;
        double sumZ = 0.0;
        float val;
        for ( int i = 0; i < m_kMaskPositions.size(); i++ )
        {
            Vector3f kPos = m_kMaskPositions.elementAt(i);
            int x = (int)kPos.X;
            int y = (int)kPos.Y;
            int z = (int)kPos.Z;
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
        centerPt.X = MipavMath.round(sumX / sum);
        centerPt.Y = MipavMath.round(sumY / sum);
        centerPt.Z = MipavMath.round(sumZ / sum);
    }

    /**
     * Gets the blue center of mass of the contour.
     * @param kImage input image.
     * @return returns the center of mass
     */
    public Vector3f getCenterOfMassB(ModelImage kImage) {
        getCenterOfMass(kImage, cenMassPtB, 3);
        return cenMassPtB;
    }

    /**
     * Gets the green center of mass of the contour.
     * @param kImage input image.
     * @return returns the center of mass
     */
    public Vector3f getCenterOfMassG(ModelImage kImage) {
        getCenterOfMass(kImage, cenMassPtG, 2);
        return cenMassPtG;
    }

    /**
     * Gets the red center of mass of the contour.
     * @param kImage input image.
     * @return returns the center of mass
     */
    public Vector3f getCenterOfMassR(ModelImage kImage) {
        getCenterOfMass(kImage, cenMassPtR, 1);
        return cenMassPtR;
    }

    /**
     * Returns the contour ID: the index of this contour into the VOI.
     * @return contour ID, or -1 if this contour is not contained in a VOI.
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
            return new Vector3f(gcPt);
        }  
        getMask();
        m_bUpdateGeometricCenter = false;
        return new Vector3f(gcPt);
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
     * @return Vector3f[] with the bounding-box minimum in [0] and the maximum in [1].
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
        if ( (autoLabel || label == null) && voiGroup != null )
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

    public int getNumVoxels()
    {
        getMask();
        return m_kMaskPositions.size();
    }
    
    /**
     * Returns the total length of this contour, based on the input resolutions.
     * @param resolutions.
     * @return total length of this contour, scaled by the resolutions.
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
     * @param iPos0 first position on this contour.
     * @param iPos1 second position on this contour.
     * @param afResolutions resolutions.
     * @param aiUnits units.
     * @return String with the length and units labeled.
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
    
    
    public BitSet getMask()
    {
        if ( !m_bUpdateMask )
        {
            return m_kMask;
        }  
        Vector3f[] kBounds = getImageBoundingBox();
        int xDim = (int)kBounds[1].X;
        int yDim = (int)kBounds[1].Y;
        int zDim = (int)kBounds[1].Z;
        if ( m_kMask == null )
        {
            m_kMask = new BitSet(xDim * yDim * zDim); 
        }                                                 
        m_kMask.clear();
        fillVolume( m_kMask, xDim, yDim, false, VOI.ADDITIVE );
        
        return m_kMask;
    }
    
    public Vector<Vector3f> getMaskPositions()
    {
        getMask();
        return m_kMaskPositions;
    }
    
    /**
     * Gets the name of the VOI that contains this contour, null if this contour is not contained in a VOI.
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
     * @return XPLANE, YPLANE, ZPLANE or NOT_A_PLANE.
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
     * Returns the total length of this contour as a string. 
     * Based on the input resolutions and units.
     * @param afResolutions
     * @param aiUnits
     * @return String with the length and units labeled.
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
     * @return type of this contour.
     */
    public int getType()
    {
        return m_iVOIType;
    }


    /**
     * Returns the VolumeVOI data member used to draw this contour in the GPU VOlumeRenderer.
     * @return VolumeVOI for rendering this contour in the GPU VolumeRenderer.
     */
    public VolumeVOI getVolumeVOI() 
    {
        return m_kVolumeVOI;
    }

    /**
     * Imports new position values into this contour.
     * @param x array of x-positions.
     * @param y array of y-positions.
     * @param z array of z-positions.
     * @param n number of values.
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
     * @param x array of x-positions.
     * @param y array of y-positions.
     * @param z array of z-positions.
     * @param n number of values.
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
     * @param pt array of points to import into this contour.
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
     * Returns true if this is a QuickLUT contour.
     * @return
     */
    public boolean isQuickLUT()
    {
        return m_bQuickLUT;
    }

    /**
     * Returns true if the input position is near the outline of this contour.
     * @param iX input x-position.
     * @param iY input y-position.
     * @param iZ input z-position
     * @return true if the input position is 'near' the outline of this contour.
     */
    public boolean nearLine(int iX, int iY, int iZ )
    {
        return nearLine(iX,iY,iZ,3);
    }


    /**
     * Returns true if the input position is near the outline of this contour.
     * Based on the input tolerance.
     * @param iX input x-position.
     * @param iY input y-position.
     * @param iZ input z-position
     * @param tol tolerance value for determining if the position is near this contour.
     * @return true if the input position is 'near' the outline of this contour.
     */
    public boolean nearLine(int iX, int iY, int iZ, int tol )
    {
        if ( distanceToVOI( iX, iY, iZ ) < tol )
        {
            return true;
        }
        return false;
    }

    /**
     * Returns true if the input position is near one of the points on this contour.
     * @param iX input x-position.
     * @param iY input y-position.
     * @param iZ input z-position
     * @return true if the input position is near one of the contour points.
     */
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
        if ( voiGroup != null )
        {
            // set the active flag to true if any are active, if all are inactive group 'global' active will be false.
            voiGroup.updateActive();
        }
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
        autoLabel = false;
        if (str == null) {
            label = null;
        } else {
            label = new String(str);
        }
    }

    public void setMask( BitSet kMask, int xDim, int yDim, boolean XOR, int polarity )
    {
        if ( m_bUpdateMask )
        {
            getMask();
        }
        for ( int i = 0; i < m_kMaskPositions.size(); i++ )
        {
            Vector3f kPos = m_kMaskPositions.elementAt(i);
            int x = (int)kPos.X;
            int y = (int)kPos.Y;
            int z = (int)kPos.Z;
            int index = z * xDim * yDim + y * xDim + x;

            if (polarity == VOI.ADDITIVE) {
                if (XOR && kMask.get(index)) {
                    kMask.clear(index);
                } else {
                    kMask.set(index);
                }
            }
            else if (polarity == VOI.SUBTRACTIVE) {
                kMask.clear(index);
            } 
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
     * Sets the near boundary point.
     * @param i
     */
    public void setNearBoundPoint( int i )
    {
        nearBoundPoint = i;
    }

    public int getNearBoundPoint( )
    {
        return nearBoundPoint;
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
        m_bSplit = bSplit;
    }

    /**
     * Sets the QuickLUT flag.
     * @param bQuickLUT
     */
    public void setQuickLUT(boolean bQuickLUT)
    {
        m_bQuickLUT = bQuickLUT;
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
     * Copies the input VOIBase, transformed by the input TransMatrix.
     * @param kBase VOI to copy.
     * @param tMatrix transformation
     */
    public void transform( TransMatrix tMatrix )
    {
        for (int i = 0; i < size(); i++) {
            Vector3f pt = new Vector3f();
            tMatrix.transformAsPoint3Df(elementAt(i), pt);
            set(i, pt);
        }
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
        m_bUpdateMask = true;
        m_bUpdateBounds = true;
        m_bUpdatePlane = true;
        m_bUpdateGeometricCenter = true;

        if ( m_kVolumeVOI != null )
        {
            m_kVolumeVOI.setVOI(this);
        }
    }

    /**
     * Set the color of this contour.
     * @param kColor new color for this contour.
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
     * The contour has been translated, this function translates the 
     * geometric center and bounding-box so they don't need to be recalculated.
     * @param kTranslate amount by which the contour was translated.
     */
    public void update(Vector3f kTranslate)
    {
        m_bUpdateMask = true;
        gcPt.Add(kTranslate);
        m_akImageMinMax[0].Add(kTranslate);
        m_akImageMinMax[1].Add(kTranslate);
        if ( m_kVolumeVOI != null )
        {
            m_kVolumeVOI.setVOI(this);
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
    
    private boolean containsZ(int x, int y)
    {
        float fx = x;// + 0.49f; // Matt add doc !!!
        float fy = y;// + 0.49f;

        boolean isInside = false;
        int j = size()-1;
        for (int i = 0; i < size(); i++) {
            Vector3f kVeci = elementAt(i);
            Vector3f kVecj = elementAt(j);

            if (((kVecj.Y <= fy) && (fy < kVeci.Y) && (areaTwice(kVeci.X,
                    kVeci.Y, kVecj.X, kVecj.Y, fx, fy) >= 0))
                    || ((kVeci.Y <= fy) && (fy < kVecj.Y) && (areaTwice(kVecj.X,
                            kVecj.Y, kVeci.X, kVeci.Y, fx, fy) >= 0))) {
                isInside = !isInside;
            }

            j = i;
        }
        return isInside;
    }
    
    private boolean containsX(int y, int z)
    {
        float fy = y;// + 0.49f; // Matt add doc !!!
        float fz = z;// + 0.49f;

        boolean isInside = false;
        int j = size()-1;
        for (int i = 0; i < size(); i++) {
            Vector3f kVeci = elementAt(i);
            Vector3f kVecj = elementAt(j);

            if (((kVecj.Z <= fz) && (fz < kVeci.Z) && (areaTwice(kVeci.Y,
                    kVeci.Z, kVecj.Y, kVecj.Z, fy, fz) >= 0))
                    || ((kVeci.Z <= fz) && (fz < kVecj.Z) && (areaTwice(kVecj.Y,
                            kVecj.Z, kVeci.Y, kVeci.Z, fy, fz) >= 0))) {
                isInside = !isInside;
            }

            j = i;
        }
        return isInside;
    }
    
    private boolean containsY(int x, int z)
    {
        float fx = x;// + 0.49f; // Matt add doc !!!
        float fz = z;// + 0.49f;

        boolean isInside = false;
        int j = size()-1;
        for (int i = 0; i < size(); i++) {
            Vector3f kVeci = elementAt(i);
            Vector3f kVecj = elementAt(j);

            if (((kVecj.Z <= fz) && (fz < kVeci.Z) && (areaTwice(kVeci.X,
                    kVeci.Z, kVecj.X, kVecj.Z, fx, fz) >= 0))
                    || ((kVeci.Z <= fz) && (fz < kVecj.Z) && (areaTwice(kVecj.X,
                            kVecj.Z, kVeci.X, kVeci.Z, fx, fz) >= 0))) {
                isInside = !isInside;
            }

            j = i;
        }
        return isInside;
    }
    
    private void fillZ(int iZ, 
            BitSet kMask, int xDim, int yDim, boolean XOR, int polarity ) {

        m_kMaskPositions.clear();
        m_kPositionSum.Set(0,0,0);
        gcPt.Set(0,0,0); 

        int iXMin = (int)(m_akImageMinMax[0].X);
        int iXMax = (int)(m_akImageMinMax[1].X);

        int iYMin = (int)(m_akImageMinMax[0].Y);
        int iYMax = (int)(m_akImageMinMax[1].Y);
        
        for ( int y = iYMin; y <= iYMax; y++ )
        {
            for ( int x = iXMin; x <= iXMax; x++ )
            {
                if ( containsZ(x,y) )
                {
                    Vector3f kPos = new Vector3f(x, y, iZ);                        
                    m_kMaskPositions.add(kPos);
                    m_kPositionSum.Add(kPos);
                    setMask( kMask, xDim, yDim, x, y, iZ, polarity, XOR );
                }
            }
        }
        float scale = 1f/m_kMaskPositions.size();
        gcPt.X = MipavMath.round( m_kPositionSum.X * scale );
        gcPt.Y = MipavMath.round( m_kPositionSum.Y * scale );
        gcPt.Z = MipavMath.round( m_kPositionSum.Z * scale );        
    }

    private void fillX(int iX, 
            BitSet kMask, int xDim, int yDim, boolean XOR, int polarity ) {

        m_kMaskPositions.clear();
        m_kPositionSum.Set(0,0,0);
        gcPt.Set(0,0,0); 
        
        int iYMin = (int)(m_akImageMinMax[0].Y);
        int iYMax = (int)(m_akImageMinMax[1].Y);

        int iZMin = (int)(m_akImageMinMax[0].Z);
        int iZMax = (int)(m_akImageMinMax[1].Z);
        
        for ( int y = iYMin; y <= iYMax; y++ )
        {
            for ( int z = iZMin; z <= iZMax; z++ )
            {
                if ( containsX(y,z) )
                {
                    Vector3f kPos = new Vector3f(iX, y, z);                        
                    m_kMaskPositions.add(kPos);
                    m_kPositionSum.Add(kPos);
                    
                    int iMaskIndex = z * xDim * yDim;
                    iMaskIndex += y * xDim;
                    iMaskIndex += iX;
                    kMask.set(iMaskIndex);            
                }
            }
        }
        float scale = 1f/m_kMaskPositions.size();
        gcPt.X = MipavMath.round( m_kPositionSum.X * scale );
        gcPt.Y = MipavMath.round( m_kPositionSum.Y * scale );
        gcPt.Z = MipavMath.round( m_kPositionSum.Z * scale );        
    }
    
    private void fillY(int iY, 
            BitSet kMask, int xDim, int yDim, boolean XOR, int polarity ) {

        m_kMaskPositions.clear();
        m_kPositionSum.Set(0,0,0);
        gcPt.Set(0,0,0); 
        
        int iXMin = (int)(m_akImageMinMax[0].X);
        int iXMax = (int)(m_akImageMinMax[1].X);

        int iZMin = (int)(m_akImageMinMax[0].Z);
        int iZMax = (int)(m_akImageMinMax[1].Z);
        
        for ( int x = iXMin; x <= iXMax; x++ )
        {
            for ( int z = iZMin; z <= iZMax; z++ )
            {
                if ( containsY(x,z) )
                {
                    Vector3f kPos = new Vector3f(x, iY, z);                        
                    m_kMaskPositions.add(kPos);
                    m_kPositionSum.Add(kPos);
                    
                    int iMaskIndex = z * xDim * yDim;
                    iMaskIndex += iY * xDim;
                    iMaskIndex += x;
                    kMask.set(iMaskIndex);            
                }
            }
        }
        float scale = 1f/m_kMaskPositions.size();
        gcPt.X = MipavMath.round( m_kPositionSum.X * scale );
        gcPt.Y = MipavMath.round( m_kPositionSum.Y * scale );
        gcPt.Z = MipavMath.round( m_kPositionSum.Z * scale );        
    }
    
    /*
    public boolean contains(int _x, int _y, boolean forceReload) {
        int i;
        int nPts = size();
        int j = nPts - 1;
        boolean isInside = false;
        float x = _x + 0.49f; // Matt add doc !!!
        float y = _y + 0.49f;

        // reloads points in this array for speed purposes
        // System.err.println("contains :!!!!!!!!!!!?");
        if ((forceReload == true) || (xPts == null) || (yPts == null)
                || (size() > xPts.length)) {
            reloadPoints();
        }

        // System.out.println("contains : npts = " + nPts);
        for (i = 0; i < nPts; i++) {

            if (((yPts[j] <= y) && (y < yPts[i]) && (areaTwice(xPts[i],
                    yPts[i], xPts[j], yPts[j], x, y) >= 0))
                    || ((yPts[i] <= y) && (y < yPts[j]) && (areaTwice(xPts[j],
                            yPts[j], xPts[i], yPts[i], x, y) >= 0))) {
                isInside = !isInside;
            }

            j = i;
        }

        // if not inside maybe it is a striaght polyline
        if ((isInside == false) && !closed) {
            // System.err.println("doing near line from contour");
            // isInside = nearLine(_x, _y, 10);
        }

        return isInside;
    }
    */


    
    
    
    /**
     * Helper function for rendering this contour into the input BitSet mask into the z-plane.
     * @param aaiCrossingPoints column crossing points.
     * @param aiNumCrossings number of crossing points per row.
     * @param iXMin contour bounding box x-minimum.
     * @param iYMin contour bounding box y-minimum.
     * @param iXMax contour bounding box x-maximum.
     * @param iYMax contour bounding box y-maximum.
     * @param iZ z-plane slice to render into.
     * @param kMask output: mask rendered into.
     * @param xDim mask image x-dimension.
     * @param yDim mask image y-dimension.
     * @param XOR indicates that nested VOI contours will be exclusive ORed with other contours of the VOI 
     * @param polarity indicates if the VOI should mask ones or mask as zeros.
     */
    private void fill(float[][] aaiCrossingPoints, int[] aiNumCrossings,
            int iXMin, int iXMax, int iZ, 
            BitSet kMask, int xDim, int yDim, boolean XOR, int polarity )
    {
        m_kMaskPositions.clear();
        m_kPositionSum.Set(0,0,0);
        gcPt.Set(0,0,0);               
        for (int iX = iXMin; iX <= iXMax; iX++) {
            int iIndex = iX - iXMin;
            if ( aiNumCrossings[iIndex] >= 2 )
            {
                for ( int i = 0; i < aiNumCrossings[iIndex]; i+= 2 )
                {
                    int yStart = (int)Math.floor(aaiCrossingPoints[iIndex][i]);
                    int yEnd = (int)Math.ceil(aaiCrossingPoints[iIndex][i+1]);
                    for ( int iY = yStart-3; iY <= yEnd+3; iY++ )
                    {              
                        int iMaskIndex = iZ * xDim * yDim;
                        iMaskIndex += iY * xDim;
                        iMaskIndex += iX;
                        if ( !kMask.get(iMaskIndex) )
                        {
                            if ( containsZ(iX,iY) )
                            {         
                                kMask.set(iMaskIndex);            
                                Vector3f kPos = new Vector3f(iX, iY, iZ); 
                                m_kMaskPositions.add(kPos);
                                m_kPositionSum.Add(kPos);
                            }              
                        }
                    }
                }
            }
        }
        float scale = 1f/m_kMaskPositions.size();
        gcPt.X = MipavMath.round( m_kPositionSum.X * scale );
        gcPt.Y = MipavMath.round( m_kPositionSum.Y * scale );
        gcPt.Z = MipavMath.round( m_kPositionSum.Z * scale );
    }


    /**
     * Helper function for rendering this contour onto the z-plane. Pixels are determined to be inside or outside the contour
     * based on the parameters, aaiCrossingPoints and aiNumCrossings, using a scan-conversion algorithm that traverses
     * each row and column of the bounding box of the sculpt region coloring inside points as it goes.
     * @param aaiCrossingPoints column crossing points.
     * @param aiNumCrossings number of crossing points per row.
     * @param iXMin contour bounding box x-minimum.
     * @param iYMin contour bounding box y-minimum.
     * @param iXMax contour bounding box x-maximum.
     * @param iYMax contour bounding box y-maximum.
     * @param iZ z-plane slice to render into.
     * @param kVolume output: when non-null this contour is rendered into the input image.
     * @param kMask output: when non-null this contour is rendered into the input BitSet mask.
     * @param bIntersection when true the contour rendered as an intersection with other contours.
     * @param iValue value to write into the input image.
     */
    private void fill(float[][] aaiCrossingPoints, int[] aiNumCrossings,
            int iXMin, int iXMax, int iZ,
            ModelImage kVolume, BitSet kMask, boolean bIntersection, int iValue)
    {

        for (int iX = iXMin; iX < iXMax; iX++) {
            int iIndex = iX - iXMin;
            if ( aiNumCrossings[iIndex] >= 2 )
            {
                for ( int i = 0; i < aiNumCrossings[iIndex]; i+= 2 )
                {
                    int yStart = Math.round(aaiCrossingPoints[iIndex][i]);
                    int yEnd = Math.round(aaiCrossingPoints[iIndex][i+1]);
                    for ( int iY = yStart; iY < yEnd; iY++ )
                    {
                        if ( bIntersection )
                        {
                            int iTemp = kVolume.getInt( iX, iY, iZ );

                            if ( kMask != null )
                            {
                                int imageIndex = iZ * kVolume.getExtents()[0] * kVolume.getExtents()[1];
                                imageIndex += iY * kVolume.getExtents()[0];
                                imageIndex += iX;
                                if ( iValue == 0 )
                                {
                                    kMask.set( imageIndex );
                                }
                                else if ( kMask.get( imageIndex ) )
                                {
                                    kMask.set( imageIndex );
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
                                int imageIndex = iZ * kVolume.getExtents()[0] * kVolume.getExtents()[1];
                                imageIndex += iY * kVolume.getExtents()[0];
                                imageIndex += iX;
                                kMask.set( imageIndex );
                            }
                            else
                            {
                                kVolume.set( iX, iY, iZ, 255 );
                            }
                        }

                    }
                }
            }
        }   
    }
    
    /**
     * Helper function for rendering this contour into the input BitSet mask into the x-plane.
     * @param aaiCrossingPoints column crossing points.
     * @param aiNumCrossings number of crossing points per row.
     * @param iYMin contour bounding box y-minimum.
     * @param iZMin contour bounding box z-minimum.
     * @param iYMax contour bounding box y-maximum.
     * @param iZMax contour bounding box z-maximum.
     * @param iX x-plane slice to render into.
     * @param kMask output: mask rendered into.
     * @param xDim mask image x-dimension.
     * @param yDim mask image y-dimension.
     * @param XOR indicates that nested VOI contours will be exclusive ORed with other contours of the VOI 
     * @param polarity indicates if the VOI should mask ones or mask as zeros.
     */
    private void fill_X(float[][] aaiCrossingPoints, int[] aiNumCrossings,
            int iYMin, int iYMax, int iZMax, int iX, 
            BitSet kMask, int xDim, int yDim, boolean XOR, int polarity )
    {
        //System.err.println( "fillX" );
        m_kMaskPositions.clear();
        m_kPositionSum.Set(0,0,0);
        gcPt.Set(0,0,0);        
        for (int iY = iYMin; iY <= iYMax; iY++) {
                      
            int iIndex = iY - iYMin;
            if ( aiNumCrossings[iIndex] >= 2 )
            {
                for ( int i = 0; i < aiNumCrossings[iIndex]; i+= 2 )
                {                   
                    int zStart = (int)Math.floor(aaiCrossingPoints[iIndex][i]);
                    int zEnd = (int)Math.ceil(aaiCrossingPoints[iIndex][i+1]);
                    for ( int iZ = zStart-3; iZ <= zEnd+3; iZ++ )
                    {              
                        int iMaskIndex = iZ * xDim * yDim;
                        iMaskIndex += iY * xDim;
                        iMaskIndex += iX;
                        if ( !kMask.get(iMaskIndex) )
                        {
                            if ( containsX(iY,iZ) )
                            {         
                                kMask.set(iMaskIndex);            
                                Vector3f kPos = new Vector3f(iX, iY, iZ); 
                                m_kMaskPositions.add(kPos);
                                m_kPositionSum.Add(kPos);
                            }              
                        }
                    }
                }
            }
        }
        float scale = 1f/m_kMaskPositions.size();
        gcPt.X = MipavMath.round( m_kPositionSum.X * scale );
        gcPt.Y = MipavMath.round( m_kPositionSum.Y * scale );
        gcPt.Z = MipavMath.round( m_kPositionSum.Z * scale );
    }

    /**
     * Helper function for rendering this contour onto the x-plane. Pixels are determined to be inside or outside the contour
     * based on the parameters, aaiCrossingPoints and aiNumCrossings, using a scan-conversion algorithm that traverses
     * each row and column of the bounding box of the sculpt region coloring inside points as it goes.
     * @param aaiCrossingPoints column crossing points.
     * @param aiNumCrossings number of crossing points per row.
     * @param iYMin contour bounding box y-minimum.
     * @param iZMin contour bounding box z-minimum.
     * @param iYMax contour bounding box y-maximum.
     * @param iZMax contour bounding box z-maximum.
     * @param iX x-plane slice to render into.
     * @param kVolume output: when non-null this contour is rendered into the input image.
     * @param kMask output: when non-null this contour is rendered into the input BitSet mask.
     * @param bIntersection when true the contour rendered as an intersection with other contours.
     * @param iValue value to write into the input image.
     */
    private void fill_X(float[][] aaiCrossingPoints, int[] aiNumCrossings,
            int iYMin, int iYMax, int iX,
            ModelImage kVolume, BitSet kMask, boolean bIntersection, int iValue)
    {
        for (int iY = iYMin; iY < iYMax; iY++) {
            int iIndex = iY - iYMin;
            if ( aiNumCrossings[iIndex] >= 2 )
            {
                for ( int i = 0; i < aiNumCrossings[iIndex]; i+= 2 )
                {
                    int zStart = Math.round(aaiCrossingPoints[iIndex][i]);
                    int zEnd = Math.round(aaiCrossingPoints[iIndex][i+1]);
                    for ( int iZ = zStart; iZ < zEnd; iZ++ )
                    {
                        if ( bIntersection )
                        {
                            int iTemp = kVolume.getInt( iX, iY, iZ );

                            if ( kMask != null )
                            {
                                int imageIndex = iZ * kVolume.getExtents()[0] * kVolume.getExtents()[1];
                                imageIndex += iY * kVolume.getExtents()[0];
                                imageIndex += iX;
                                if ( iValue == 0 )
                                {
                                    kMask.set( imageIndex );
                                }
                                else if ( kMask.get( imageIndex ) )
                                {
                                    kMask.set( imageIndex );
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
                                int imageIndex = iZ * kVolume.getExtents()[0] * kVolume.getExtents()[1];
                                imageIndex += iY * kVolume.getExtents()[0];
                                imageIndex += iX;
                                kMask.set( imageIndex );
                            }
                            else
                            {
                                kVolume.set( iX, iY, iZ, 255 );
                            }
                        }
                    }
                }
            }
        }
    }
    
    

    /**
     * Helper function for rendering this contour into the input BitSet mask into the y-plane.
     * @param aaiCrossingPoints column crossing points.
     * @param aiNumCrossings number of crossing points per row.
     * @param iXMin contour bounding box x-minimum.
     * @param iZMin contour bounding box z-minimum.
     * @param iXMax contour bounding box x-maximum.
     * @param iZMax contour bounding box z-maximum.
     * @param iY y-plane slice to render into.
     * @param kMask output: mask rendered into.
     * @param xDim mask image x-dimension.
     * @param yDim mask image y-dimension.
     * @param XOR indicates that nested VOI contours will be exclusive ORed with other contours of the VOI 
     * @param polarity indicates if the VOI should mask ones or mask as zeros.
     */
    private void fill_Y(float[][] aaiCrossingPoints, int[] aiNumCrossings,
            int iXMin, int iXMax, int iY, 
            BitSet kMask, int xDim, int yDim, boolean XOR, int polarity )
    {
        m_kMaskPositions.clear();
        m_kPositionSum.Set(0,0,0);
        gcPt.Set(0,0,0);
                
        for (int iX = iXMin+1; iX < iXMax; iX++) {
            int iIndex = iX - iXMin;
            if ( aiNumCrossings[iIndex] >= 2 )
            {
                for ( int i = 0; i < aiNumCrossings[iIndex]; i+= 2 )
                {
                    int zStart = Math.round(aaiCrossingPoints[iIndex][i]);
                    int zEnd = Math.round(aaiCrossingPoints[iIndex][i+1]);
                    for ( int iZ = zStart+1; iZ < zEnd; iZ++ )
                    {
                        Vector3f kPos = new Vector3f(iX, iY, iZ);                       
                        m_kMaskPositions.add(kPos);
                        m_kPositionSum.Add(kPos);
                        setMask( kMask, xDim, yDim, iX, iY, iZ, polarity, XOR );
                    }
                }
            }
        }
        
        float scale = 1f/m_kMaskPositions.size();
        gcPt.X = MipavMath.round( m_kPositionSum.X * scale );
        gcPt.Y = MipavMath.round( m_kPositionSum.Y * scale );
        gcPt.Z = MipavMath.round( m_kPositionSum.Z * scale );
    }

    /**
     * Helper function for rendering this contour onto the y-plane. Pixels are determined to be inside or outside the contour
     * based on the parameters, aaiCrossingPoints and aiNumCrossings, using a scan-conversion algorithm that traverses
     * each row and column of the bounding box of the sculpt region coloring inside points as it goes.
     * @param aaiCrossingPoints column crossing points.
     * @param aiNumCrossings number of crossing points per row.
     * @param iXMin contour bounding box x-minimum.
     * @param iZMin contour bounding box z-minimum.
     * @param iXMax contour bounding box x-maximum.
     * @param iZMax contour bounding box z-maximum.
     * @param iY y-plane slice to render into.
     * @param kVolume output: when non-null this contour is rendered into the input image.
     * @param kMask output: when non-null this contour is rendered into the input BitSet mask.
     * @param bIntersection when true the contour rendered as an intersection with other contours.
     * @param iValue value to write into the input image.
     */
    private void fill_Y(float[][] aaiCrossingPoints, int[] aiNumCrossings,
            int iXMin, int iXMax, int iY,
            ModelImage kVolume, BitSet kMask, boolean bIntersection, int iValue)
    {
        for (int iX = iXMin; iX < iXMax; iX++) {
            int iIndex = iX - iXMin;
            if ( aiNumCrossings[iIndex] >= 2 )
            {
                for ( int i = 0; i < aiNumCrossings[iIndex]; i+= 2 )
                {
                    int zStart = Math.round(aaiCrossingPoints[iIndex][i]);
                    int zEnd = Math.round(aaiCrossingPoints[iIndex][i+1]);
                    for ( int iZ = zStart; iZ < zEnd; iZ++ )
                    {
                        if ( bIntersection )
                        {
                            int iTemp = kVolume.getInt( iX, iY, iZ );

                            if ( kMask != null )
                            {
                                int imageIndex = iZ * kVolume.getExtents()[0] * kVolume.getExtents()[1];
                                imageIndex += iY * kVolume.getExtents()[0];
                                imageIndex += iX;
                                if ( iValue == 0 )
                                {
                                    kMask.set( imageIndex );
                                }
                                else if ( kMask.get( imageIndex ) )
                                {
                                    kMask.set( imageIndex );
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
                                int imageIndex = iZ * kVolume.getExtents()[0] * kVolume.getExtents()[1];
                                imageIndex += iY * kVolume.getExtents()[0];
                                imageIndex += iX;
                                kMask.set( imageIndex );
                            }
                            else
                            {
                                kVolume.set( iX, iY, iZ, 255 );
                            }
                        }
                    }
                }
            }
        }
    }

    /**
     * Determines if a value is within the range.
     * @param ignoreMin minimum intensity.
     * @param ignoreMax maximum intensity.
     * @param num value to test.
     * @param rangeFlag (no test, between, outside) the min and max.
     * @return true if num satisfies the test.
     */
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
                return true;
            }
            return false;
        }

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
    private boolean isInside( float fX1, float fY1, float fX2, float fY2, float fX, float fY )
    {
        if ( !(((fY1 <= fY) && (fY < fY2)) || ((fY2 <= fY) && (fY < fY1))) )
            return false;
        return ( ((fY1 <= fY) && (fY < fY2) && (areaTwice(fX2, fY2, fX1, fY1, fX, fY) >= 0)) ||
                ((fY2 <= fY) && (fY < fY1) && (areaTwice(fX1, fY1, fX2, fY2, fX, fY) >= 0)) );
    }
     */



    /**
     * Helper function computes the set of spans indicated by column crossings  for rendering this contour into the z-plane.
     * @param aaiCrossingPoints output: x-values that the contour crosses 
     * @param aiNumCrossings output: number of crossing points per row.
     * @param iXMin the minimum x-value of this contour.
     * @param iXMax the maximum x-value of this contour.
     */
    private void outlineRegion(float[][] aaiCrossingPoints, int[] aiNumCrossings, int iXMin, int iXMax)
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
        for (int iColumn = iXMin; iColumn <= iXMax; iColumn++) {
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
                    aaiCrossingPoints[iIndex][aiNumCrossings[iIndex]] = (float) Math.round(dYCross);
                    aiNumCrossings[iIndex]++;
                }
            }

            /* sort the set of crossings for this column: */
            sortCrossingPoints(aaiCrossingPoints[iIndex], aiNumCrossings[iIndex]);
        }
        aaadEdgeList = null;
    }



    /**
     * Helper function computes the set of spans indicated by column crossings for rendering this contour into the x-plane.
     * @param aaiCrossingPoints output: z-values that the contour crosses 
     * @param aiNumCrossings output: number of crossing points per row.
     * @param iXMin the minimum y-value of this contour.
     * @param iXMax the maximum y-value of this contour.
     */
    private void outlineRegion_X(float[][] aaiCrossingPoints, int[] aiNumCrossings, int iYMin, int iYMax)
    {        
        int iNumPts = size();
        double dNudge = 0.1;       
        double[][][] aaadEdgeList = new double[iNumPts][2][2];

        for (int i = 0; i < (iNumPts - 1); i++) {
            aaadEdgeList[i][0][0] = elementAt(i).Y - dNudge;
            aaadEdgeList[i][0][1] = elementAt(i).Z + dNudge;
            aaadEdgeList[i][1][0] = elementAt(i+1).Y - dNudge;
            aaadEdgeList[i][1][1] = elementAt(i+1).Z + dNudge;
        }
        aaadEdgeList[iNumPts-1][0][0] = lastElement().Y - dNudge;
        aaadEdgeList[iNumPts-1][0][1] = lastElement().Z + dNudge;
        aaadEdgeList[iNumPts-1][1][0] = firstElement().Y - dNudge;
        aaadEdgeList[iNumPts-1][1][1] = firstElement().Z + dNudge;
        iNumPts++;

        /*
         * Compute the crossing points for this column and produce spans.
         */
        for (int iColumn = iYMin; iColumn <= iYMax; iColumn++) {
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
                    aaiCrossingPoints[iIndex][aiNumCrossings[iIndex]] = (float)Math.round(dZCross);
                    aiNumCrossings[iIndex]++;
                }
            }

            /* sort the set of crossings for this column: */
            sortCrossingPoints(aaiCrossingPoints[iIndex], aiNumCrossings[iIndex]);
        }
        aaadEdgeList = null;
    }




    /**
     * Helper function computes the set of spans indicated by column crossings for rendering this contour into the y-plane.
     * @param aaiCrossingPoints output: z-values that the contour crosses 
     * @param aiNumCrossings output: number of crossing points per row.
     * @param iXMin the minimum x-value of this contour.
     * @param iXMax the maximum x-value of this contour.
     */
    private void outlineRegion_Y(float[][] aaiCrossingPoints, int[] aiNumCrossings, int iXMin, int iXMax)
    {        
        int iNumPts = size();
        double dNudge = 0.1;       
        double[][][] aaadEdgeList = new double[iNumPts][2][2];

        for (int i = 0; i < (iNumPts - 1); i++) {
            aaadEdgeList[i][0][0] = elementAt(i).X - dNudge;
            aaadEdgeList[i][0][1] = elementAt(i).Z - dNudge;
            aaadEdgeList[i][1][0] = elementAt(i+1).X - dNudge;
            aaadEdgeList[i][1][1] = elementAt(i+1).Z + dNudge;
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
                    aaiCrossingPoints[iIndex][aiNumCrossings[iIndex]] = (float)Math.round(dYCross);
                    aiNumCrossings[iIndex]++;
                }
            }

            /* sort the set of crossings for this column: */
            sortCrossingPoints(aaiCrossingPoints[iIndex], aiNumCrossings[iIndex]);
        }
        aaadEdgeList = null;
    }
    
    private void setMask( BitSet kMask, int xDim, int yDim, int iX, int iY, int iZ, int polarity, boolean XOR )
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
