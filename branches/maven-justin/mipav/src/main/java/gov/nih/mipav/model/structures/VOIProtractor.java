package gov.nih.mipav.model.structures;

import gov.nih.mipav.util.MipavMath;

import java.util.Vector;

import WildMagic.LibFoundation.Mathematics.Vector3f;


/**
 * This class is fundamental to the VOI class in which points are stored that describe a Protractor VOI. The points are
 * 3D and are floats (see Vector3f). It extends VOIBase and therefore it extends Vector. Point 1 is the common vertex of
 * the 2 intersecting lines, point 0 is the outer point of the initially shorter line, and point 2 is the outer point of
 * the initially longer line. This kind of VOI is used to measure angles.
 *
 * @see  VOIBase
 * @see  VOILine
 * @see  VOI
 * @see  VOIPoint
 */
public class VOIProtractor extends VOIBase {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** Use serialVersionUID for interoperability. */
    private static final long serialVersionUID = 4213485128976585397L;

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** Set to true, displays this protractor object for any z-value. */
    private boolean allSlices = false;
    
    /**
     * Default constructor.
     */
    public VOIProtractor( )
    {
        super( false,false);
        m_iVOIType = VOI.PROTRACTOR;
    } 
 
    /**
     * Constructor, sets the position of the protractor. 
     * @param kPositions
     */
    public VOIProtractor( Vector<Vector3f> kPositions )
    {
        super( false, false, kPositions );
        m_iVOIType = VOI.PROTRACTOR;
    }

    /**
     * Copy constructor.
     * @param kVOI
     */
    public VOIProtractor( VOIProtractor kVOI )
    {
        super(kVOI);
        m_iVOIType = VOI.PROTRACTOR;
    }

    /* (non-Javadoc)
     * @see gov.nih.mipav.model.structures.VOIBase#clone()
     */
    @Override
	public VOIProtractor clone() {
        return new VOIProtractor(this);
    }
    
    /**
     * Returns true if this protractor is displayed independent of z-value.
     * @return
     */
    public boolean getAllSlices()
    {
        return allSlices;
    }
    
    /**
     * Returns the angle of the protractor as a string. Based on the input image resolutions.
     * @param afResolutions
     * @return
     */
    public String getAngleString(double fAngle )
    {

        String degreeString = String.valueOf(fAngle); // since y decreases going down
        int i = degreeString.indexOf('.');

        if (degreeString.length() >= (i + 3)) {
            degreeString = degreeString.substring(0, i + 3);
        }

        degreeString += " deg";
        return degreeString;
    }

    /**
     * Returns the angle of the protractor as a string. Based on the input image resolutions.
     * @param afResolutions
     * @return
     */
    public String getAngleString( float[] afResolutions )
    {
        double fAngle = getTheta(afResolutions);

        String degreeString = String.valueOf(fAngle); // since y decreases going down
        int i = degreeString.indexOf('.');

        if (degreeString.length() >= (i + 3)) {
            degreeString = degreeString.substring(0, i + 3);
        }

        degreeString += " deg";
        return degreeString;
    }

    /**
     * Gets the angle between the two lines of the protractor.
     *
     * @return  theta
     */
    public double getTheta(float[] afResolutions) {
        Vector3f kScale = new Vector3f( afResolutions[0], afResolutions[1], afResolutions[2] );
        Vector3f kStart = get(1);
        Vector3f kEnd1 = get(0);
        Vector3f kEnd2 = get(2);

        Vector3f kV1 = Vector3f.sub( kEnd1, kStart );
        kV1.mult( kScale ).normalize();

        Vector3f kV2 = Vector3f.sub( kEnd2, kStart );
        kV2.mult( kScale ).normalize();

        double fAngle = (180.0 / Math.PI) * Vector3f.angle( kV1, kV2 );
        if (fAngle < -180.0) {
            fAngle = fAngle + 360.0;
        }
        if (fAngle > 180.0) {
            fAngle = fAngle - 360.0;
        }
        return fAngle;               
    }    
    
    /**
     * Returns the total length of this contour, based on the input resolutions.
     * @param resolutions.
     * @return total length of this contour, scaled by the resolutions.
     */
    public double getLengthPtToPt(float[] resolutions) {

        return MipavMath.distance(get(size()-2), lastElement(), resolutions);
    }


    /**
     * Sets the all-slice flag. Displays this protractor independent of z-value. 
     * Used in the TriPlanar View for rotating images.
     * @param bValue
     */
    public void setAllSlices( boolean bValue )
    {
        allSlices = bValue;
    }

    

    /**
     * Translates 3 points of the protractor VOI.
     *
     * @param  xT  amount to translate in the x direction
     * @param  yT  amount to translate in the y direction
     */
    public void translate(float xT, float yT) {
        int i;

        for (i = 0; i < 3; i++) {
            ((elementAt(i))).X += xT;
            ((elementAt(i))).Y += yT;
        }
    }

}
