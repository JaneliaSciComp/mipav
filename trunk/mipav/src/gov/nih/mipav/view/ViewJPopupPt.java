package gov.nih.mipav.view;


import java.awt.event.*;

import javax.swing.*;
import javax.swing.event.*;


/**
 * DOCUMENT ME!
 *
 * @version  1.0 July 10, 2001
 * @author   Harman Singh
 */
public class ViewJPopupPt extends JPanel implements ActionListener, PopupMenuListener, MouseListener {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** Use serialVersionUID for interoperability. */
    private static final long serialVersionUID = 393929198484576018L;

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** DOCUMENT ME! */
    private VOIHandlerInterface voiHandler;

    /** DOCUMENT ME! */
    private JMenuItem itemBuildPolyline;

    /** DOCUMENT ME! */
    private JMenuItem itemProps;

    /** DOCUMENT ME! */
    private JMenuItem itemShowGraph;

    /** DOCUMENT ME! */
    private JMenuItem itemShowPAAIDialog;

    /** DOCUMENT ME! */
    private JMenu propSubMenu;

    /** DOCUMENT ME! */
    private JPopupMenu ptPopup;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Creates a new ViewJPopupPt object.
     *
     * @param  comp  DOCUMENT ME!
     */
    public ViewJPopupPt(VOIHandlerInterface handler) {

        try {
            ptPopup = new JPopupMenu();
            propSubMenu = ViewMenuBuilder.buildMenu("Propagate", 0, false);
            itemShowGraph = ViewMenuBuilder.buildMenuItem("Show VOI Graph", "ShowGraph", 0, this, null, false);
            itemShowPAAIDialog = ViewMenuBuilder.buildMenuItem("Point area average intensities", "PAAI", 0,
                                                               this, null, false);
          //  itemBuildPolyline = ViewMenuBuilder.buildMenuItem("Convert to poly-line slice VOI", "BuildPoly", 0, this, null,
         //                                                     false);
           
            itemProps = ViewMenuBuilder.buildMenuItem("Properties", "Properties", 0, this, null, true);
            voiHandler = handler;
        } catch (OutOfMemoryError error) {
            System.gc();
            MipavUtil.displayError("Out of memory: ViewJPopupPt Constructor");

            return;
        }

        propSubMenu.add(ViewMenuBuilder.buildMenuItem("To Next Slice", "PropVOIUp", 0, this, "voipropu.gif", true));
        propSubMenu.add(ViewMenuBuilder.buildMenuItem("To Previous Slice", "PropVOIDown", 0, this, "voipropd.gif",
                                                      true));
        propSubMenu.add(ViewMenuBuilder.buildMenuItem("To All Slices", "PropVOIAll", 0, this, null, true));

        ptPopup.add(propSubMenu);
        ptPopup.add(ViewMenuBuilder.buildMenuItem("Delete", "deleteVOI", 0, this, "delete.gif", true));

        ptPopup.addSeparator();
        ptPopup.add(itemShowGraph);
        ptPopup.add(itemShowPAAIDialog);
        ptPopup.addSeparator();
       // ptPopup.add(itemBuildPolyline);


       // itemBuildPolyline.setEnabled(false);
    }

    //~ Methods --------------------------------------------------------------------------------------------------------


    /**
     * DOCUMENT ME!
     *
     * @param  event  DOCUMENT ME!
     */
    public void actionPerformed(ActionEvent event) {

        try {
            if (event.getActionCommand().equals("ShowGraph")) {
                voiHandler.setGraphVisible();
            } else if (event.getActionCommand().equals("PropVOIUp")) {
                voiHandler.propVOI(1, false);
            } else if (event.getActionCommand().equals("PropVOIDown")) {
                voiHandler.propVOI(-1, false);
            } else if (event.getActionCommand().equals("PropVOIAll")) {
                voiHandler.propVOIAll();
            } else if (event.getActionCommand().equals("PAAI")) {
                voiHandler.setPAAIGraphVisible();
            }  else if (event.getActionCommand().equals("Properties")) {
                voiHandler.showVOIProperties();
            } else if (event.getActionCommand().equals("BuildPoly")) {
                voiHandler.convertPointToPoly();
            } else if (event.getActionCommand().equals("deleteVOI")) {
                voiHandler.deleteSelectedVOI(true);
            }

        } catch (OutOfMemoryError error) {
            System.gc();
            MipavUtil.displayError("Out of memory: ViewJPopupPt.action.");

            return;
        }
    }

    /**
     * DOCUMENT ME!
     *
     * @param  event  DOCUMENT ME!
     */
    public void mouseClicked(MouseEvent event) {
        checkPopup(event);
    }

    /**
     * DOCUMENT ME!
     *
     * @param  event  DOCUMENT ME!
     */
    public void mouseEntered(MouseEvent event) { }

    /**
     * DOCUMENT ME!
     *
     * @param  event  DOCUMENT ME!
     */
    public void mouseExited(MouseEvent event) { }

    /**
     * DOCUMENT ME!
     *
     * @param  event  DOCUMENT ME!
     */
    public void mousePressed(MouseEvent event) {
        checkPopup(event);
    }

    /**
     * DOCUMENT ME!
     *
     * @param  event  DOCUMENT ME!
     */
    public void mouseReleased(MouseEvent event) {
        checkPopup(event);
    }

    /**
     * DOCUMENT ME!
     *
     * @param  event  DOCUMENT ME!
     */
    public void popupMenuCanceled(PopupMenuEvent event) {
        Preferences.debug("Popup menu will be visible!");
    }

    /**
     * DOCUMENT ME!
     *
     * @param  event  DOCUMENT ME!
     */
    public void popupMenuWillBecomeInvisible(PopupMenuEvent event) {
        Preferences.debug("Popup menu will be invisible!");
    }

    /**
     * DOCUMENT ME!
     *
     * @param  event  DOCUMENT ME!
     */
    public void popupMenuWillBecomeVisible(PopupMenuEvent event) {
        Preferences.debug("Popup menu will be visible!");
    }

    /**
     * Enables or disables the show graph menu item accordingly.
     *
     * @param  enab  whether or not to enable
     */
    public void setEnabledGraph(boolean enab) {
        itemShowGraph.setEnabled(enab);
    }

    /**
     * Enables or disables the propSub menu item accordingly.
     *
     * @param  enab  whether or not to enable
     */
    public void setEnabledProp(boolean enab) {
        propSubMenu.setEnabled(enab);
    }

    /**
     * DOCUMENT ME!
     *
     * @param  event  DOCUMENT ME!
     */
    private void checkPopup(MouseEvent event) {

        if (event.isPopupTrigger()) {
            ptPopup.show(voiHandler.getComponentImage(), event.getX(), event.getY());
        }
    }
}
