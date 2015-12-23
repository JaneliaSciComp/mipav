package gov.nih.mipav.view;


import java.awt.*;


/**
 * <p>Title: PreviewImageContainer</p>
 *
 * <p>Description: This interface is used by ViewJComponentPreviewImage to determine the size to which it should scale
 * itself. Components that use ViewJComponentPreviewImage should implement this interface.</p>
 *
 * <p>Copyright: Copyright (c) 2005</p>
 *
 * <p>Company: NIH</p>
 *
 * @author   Lee Orsino
 * @version  1.0
 */
public interface PreviewImageContainer {

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * The method returns the size of the panel into which the ViewJComponentPreviewImage will be drawing itself. It
     * needs the Dimension so that it knows how large to scale.
     *
     * @return  Dimension
     */
    Dimension getPanelSize();
}
