package gov.nih.mipav.view;

import gov.nih.mipav.model.structures.VOIStatisticList;


/**
 *  Custom panel for placing statistics in a selectable
 *  checklist panel.
 *
 *       $Logfile: /mipav/src/gov/nih/mipav/view/JPanelStatisticsList.java $
 *       $Revision: 11 $
 *       $Date: 12/15/05 5:06p $
 */
public class JPanelStatisticsList   extends JPanelChecklist
                                    implements VOIStatisticList
{
    // just so the thing is non-null
    private boolean singleSlice = true;
	/**
	*   constructor to build a panel allowing user to find which
	*   tags are available to anonymize.
	*   <p>
	*   by defualt, sets all checkboxes to enabled.  To set the
	*   checkboxes to image-specific enabled, set the slice count.
	*   @see #setSliceCount
	*/
    public JPanelStatisticsList() {
        super();
        checkboxLabels = makeCheckboxLabels();
        setListLength();
        setBorder("Statistics to calculate:");
        buildLayout();
	}

	/** we may need to tailor the checklist programmatically,
	*   so we will need to set the selectable choices.
	*   Currently, although this method takes the number of
	*   slices in the image, this is really no more than
	*   selecting whether or not this image is 2D or 3D.
	*/
	public void setSliceCount(int sliceCount) {
	    singleSlice = (sliceCount == 1);
        setCheckBoxesEnabled();

	}

    /** Creates the list of labels to use in the checkboxes. */
    public String[] makeCheckboxLabels() {
        return statisticDescription;
    }

    protected void setListLength() {
        listLength = numberOfStatistics;
    }

    /**
    *   only mark a VOI property to be visible for certain kinds of images.
	*	Namely, 2D images will not have VOIs with volume regions,
	*	and since the eccentricity and principal axis calculations are
	*	only valid for 2D regions, turn these options off for 3D images.
	*	(this is not altogether correct, but will suffice).
    *   @return nothing
    */
    private void findVisible()
    {
        setVisibleList(true);
        if (singleSlice) {
            // turn off volume
            setVisibleList(1, false);
        }
        else {
            // turn off eccentricity, principal axis, major axis, and
            // minor axis
            setVisibleList(listLength - 1, false);
            setVisibleList(listLength - 2, false);
            setVisibleList(listLength - 3, false);
            setVisibleList(listLength - 4, false);
        }
    }

	/** sets the enabled status of the checkboxes, after
	*	first finding the visible ones (ie., setting the
	*	visible status of them), by using the super-class'
	*	implementation of setCheckBoxesEnabled.
	*	@see #findVisible
	*	@see JPanelChecklist#setCheckBoxesEnabled
	*/
	public void setCheckBoxesEnabled() {
	    findVisible();
	    super.setCheckBoxesEnabled();
	}

    public void setCheckBoxesDisabled() {
        for (int i = 0; i < listLength; i++) {
            setVisibleList(i, false);
        }
        super.setCheckBoxesEnabled();
    }

    /** method here to preserve legacy functionality
    *   with the VOI class <code>statsList</code>.
    *   Creates a new ViewList array holding the checkbox
    *   labels and the state of the checkbox.
    *   @see ViewList
    *   @see VOI
    */
    public ViewList[] getViewList(){
        ViewList statList[] = new ViewList[listLength];
        for (int i=0; i < listLength; i++) {
            statList[i] = new ViewList(checkboxLabels[i], getSelectedList(i));
        }
        return statList;
    }


}
