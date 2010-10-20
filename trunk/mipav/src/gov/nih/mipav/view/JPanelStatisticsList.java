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

    /** just so the thing is non-null. */
    private boolean singleSlice = true;

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
     * method here to preserve legacy functionality with the VOI class <code>statsList</code>. Creates a new ViewList
     * array holding the checkbox labels and the state of the checkbox.
     *
     * @see  ViewList
     * @see  VOI
     */
    public ViewList[] getViewList() {
        ViewList[] statList = new ViewList[listLength];

        for (int i = 0; i < listLength; i++) {
            statList[i] = new ViewList(checkboxLabels[i], getSelectedList(i));
        }

        return statList;
    }

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
            setVisibleList(i, false);
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
     * we may need to tailor the checklist programmatically, so we will need to set the selectable choices. Currently,
     * although this method takes the number of slices in the image, this is really no more than selecting whether or
     * not this image is 2D or 3D.
     *
     * @param  sliceCount  DOCUMENT ME!
     */
    public void setSliceCount(int sliceCount) {
        singleSlice = (sliceCount == 1);
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
        setVisibleList(true);

        if (singleSlice) {

            // turn off volume
            setVisibleList(1, false);
            // turn off largest distance
            setVisibleList(18, false);
        } else {

            // turn off eccentricity, principal axis, major axis, and
            // minor axis
            //setVisibleList(11, false);
            //setVisibleList(12, false);
            //setVisibleList(13, false);
            //setVisibleList(14, false);
        }
    }


}
