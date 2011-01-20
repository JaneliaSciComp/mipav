package gov.nih.mipav.view;


import gov.nih.mipav.model.structures.*;


/**
 * Custom panel for placing statistics in a selectable checklist panel.
 *
 * <p>$Logfile: /mipav/src/gov/nih/mipav/view/JPanelStatisticsList.java $ $Revision: 11 $ $Date: 12/15/05 5:06p $</p>
 */
public class JPanelStatisticsList extends JPanelChecklist implements VOIStatisticList {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** Use serialVersionUID for interoperability. */
    private static final long serialVersionUID = -5517669600349449423L;

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** Whether this list is dealing with lists of 2D contours. */
    private boolean singleSlice = true;
    
    /** Whether this list is dealing with open contours. */
    private boolean openContour = false;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * constructor to build a panel allowing user to find which tags are available to anonymize.
     *
     * <p>by defualt, sets all checkboxes to enabled. To set the checkboxes to image-specific enabled, set the slice
     * count.</p>
     *
     * @see  #setSliceCount
     */
    public JPanelStatisticsList() {
        super();
        checkboxLabels = makeCheckboxLabels();
        setListLength();
        setBorder("Statistics to calculate:");
        buildLayout();
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * Creates the list of labels to use in the checkboxes.
     *
     * @return  DOCUMENT ME!
     */
    public String[] makeCheckboxLabels() {
        return statisticDescription;
    }

    public static String[] getCheckboxLabels() {
    	return statisticDescription;
    }
    
    /**
     * DOCUMENT ME!
     */
    public void setCheckBoxesDisabled() {

        for (int i = 0; i < listLength; i++) {
            setEnabledList(i, false);
        }

        super.setCheckBoxesEnabled();
    }

    /**
     * sets the enabled status of the checkboxes, after first finding the visible ones (ie., setting the visible status
     * of them), by using the super-class' implementation of setCheckBoxesEnabled.
     *
     * @see  #findVisible
     * @see  JPanelChecklist#setCheckBoxesEnabled
     */
    public void setCheckBoxesEnabled() {
        findVisible();
        super.setCheckBoxesEnabled();
    }

    /**
     * Controls how many slices are being used for statistics calculations.  Setting the sliceCount = 1 
     * allows only calculations that are valid for 2D VOIs to be selected in the statistics list.
     *
     * @param  sliceCount  The number of slices that are used for statistics calculations.
     */
    public void setSliceCount(int sliceCount) {
        singleSlice = (sliceCount == 1);
        setCheckBoxesEnabled();
    }
    
    /**
     * Controls whether non-closed contours are being used for statistics calculations.  If 
     * openContour = true, then several statistics will be disabled.
     *
     * @param  openContour whether non-closed contours are being used for statistics calculations
     */
    public void isOpenContour(boolean openContour) {
        this.openContour = openContour;
        setCheckBoxesEnabled();
    }

    /**
     * DOCUMENT ME!
     */
    protected void setListLength() {
        listLength = numberOfStatistics;
    }

    /**
     * only mark a VOI property to be visible for certain kinds of images. Namely, 2D images will not have VOIs with
     * volume regions, and since the eccentricity and principal axis calculations are only valid for 2D regions, turn
     * these options off for 3D images. (this is not altogether correct, but will suffice).
     */
    private void findVisible() {
        setEnabledList(true);

        if (singleSlice) {
            // turn off volume
            setEnabledList(volumeDescription, false);
            // turn off largest distance
            setEnabledList(largestDistanceDescription, false);
        } 
        
        if(openContour) { //the following statistics are invalid for non-closed contours
            setEnabledList(perimeterDescription, false);
            setEnabledList(skewnessDescription, false);
            setEnabledList(kurtosisDescription, false);
            setEnabledList(axisDescription, false);
            setEnabledList(eccentricityDescription, false);
            setEnabledList(majorAxisDescription, false);
            setEnabledList(minorAxisDescription, false);
            setEnabledList(largestSliceDistanceDescription, false);
            setEnabledList(largestDistanceDescription, false);
            setEnabledList(geometricCenterDescription, false); //TODO: this parameter cannot be calculated without a valid mask
        }
    }


}
