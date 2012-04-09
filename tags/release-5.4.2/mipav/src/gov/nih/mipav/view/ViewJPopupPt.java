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
            itemShowGraph = ViewMenuBuilder.buildMenuItem(CustomUIBuilder.PARAM_VOI_GRAPH_SHOW, this, false);
            itemShowPAAIDialog = ViewMenuBuilder.buildMenuItem(CustomUIBuilder.PARAM_VOI_GRAPH_PAAI, this, false);

            voiHandler = handler;
        } catch (OutOfMemoryError error) {
            System.gc();
            MipavUtil.displayError("Out of memory: ViewJPopupPt Constructor");

            return;
        }

        propSubMenu.add(ViewMenuBuilder.buildMenuItem(CustomUIBuilder.PARAM_VOI_PROPAGATE_UP, this, true));
        propSubMenu.add(ViewMenuBuilder.buildMenuItem(CustomUIBuilder.PARAM_VOI_PROPAGATE_DOWN, this, true));
        propSubMenu.add(ViewMenuBuilder.buildMenuItem(CustomUIBuilder.PARAM_VOI_PROPAGATE_ALL, this, true));

        ptPopup.add(propSubMenu);
        ptPopup.add(ViewMenuBuilder.buildMenuItem(CustomUIBuilder.PARAM_VOI_DELETE, this, true));
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

        if ( ViewMenuBar.isMenuCommand( ptPopup.getComponents(), event.getActionCommand() ) )
        {
            voiHandler.actionPerformed(event);
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
