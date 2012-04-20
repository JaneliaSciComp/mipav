package gov.nih.mipav.view;



import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.dialogs.*;


import java.awt.event.*;

import java.util.*;

import javax.swing.*;
import javax.swing.event.*;


/**
 * DOCUMENT ME!
 *
 * @version  1.0 July 27, 1999
 * @author   Matthew J. McAuliffe, Ph.D. (primary)
 * @author   Harman Singh
 */
public class ViewJPopupVOI extends JPanel implements ActionListener, PopupMenuListener, MouseListener {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** Use serialVersionUID for interoperability. */
    private static final long serialVersionUID = -7134535981914560813L;

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** DOCUMENT ME! */
    private JMenu contourOrderSubMenu;

    /** DOCUMENT ME! */
    private JMenu editSubMenu;

    /** DOCUMENT ME! */
    private JMenu flipSubMenu;

    /** DOCUMENT ME! */
    private JMenu graphSubMenu;

    /** DOCUMENT ME! */
    private JMenuItem itemClose;

    /** DOCUMENT ME! */
    private JMenuItem itemCrop;

    /** DOCUMENT ME! */
    private JMenuItem itemOutputDistance;

    /** DOCUMENT ME! */
    private JMenuItem itemProps;
    
    private JMenuItem itemTrim;

    /** DOCUMENT ME! */
    private JMenu orderSubMenu;
    
    private JMenuItem editCircleDiameter;
    
    private JMenuItem editSquareLength;

    /** DOCUMENT ME! */
    private JPopupMenu popup;

    /** DOCUMENT ME! */
    private JMenu propSubMenu;

    /** DOCUMENT ME! */
    private JMenu selectionMenu;

    /** DOCUMENT ME! */
    private VOIHandlerInterface voiHandler;
    
    private VOIBase selectedVOI = null;
    
    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Creates a new ViewJPopupVOI object.
     *
     * @param  handler  DOCUMENT ME!
     */
    public ViewJPopupVOI(VOIHandlerInterface handler) {
        voiHandler = handler;

        itemProps = ViewMenuBuilder.buildMenuItem(CustomUIBuilder.PARAM_VOI_PROPERTIES, this, false);
        
        itemTrim = ViewMenuBuilder.buildMenuItem(CustomUIBuilder.PARAM_VOI_TRIM, this, false);

        selectionMenu = ViewMenuBuilder.buildMenu("Select", 0, false);
        selectionMenu.add(ViewMenuBuilder.buildMenuItem(CustomUIBuilder.PARAM_VOI_SELECT_ALL, this, false));
        //selectionMenu.add(ViewMenuBuilder.buildMenuItem(CustomUIBuilder.PARAM_CONTOUR_SELECT_ALL, this, false));
        selectionMenu.add(ViewMenuBuilder.buildMenuItem(CustomUIBuilder.PARAM_VOI_SELECT_NONE, this, false));
        

        editSubMenu = ViewMenuBuilder.buildMenu("Edit", 0, false);
        editSubMenu.add(ViewMenuBuilder.buildMenuItem(CustomUIBuilder.PARAM_VOI_DELETE, this, true));
        editSubMenu.add(ViewMenuBuilder.buildMenuItem(CustomUIBuilder.PARAM_VOI_CUT, this, true));
        editSubMenu.add(ViewMenuBuilder.buildMenuItem(CustomUIBuilder.PARAM_VOI_COPY, this, true));
        editSubMenu.add(ViewMenuBuilder.buildMenuItem(CustomUIBuilder.PARAM_VOI_PASTE, this, true));
        editSubMenu.add(ViewMenuBuilder.buildMenuItem(CustomUIBuilder.PARAM_VOI_SHOW_CONTOUR_BOUNDING_BOX, this, true));
        
        
        //showBoundingBox = ViewMenuBuilder.buildMenuItem(CustomUIBuilder.PARAM_VOI_SHOW_CONTOUR_BOUNDING_BOX, this, false);

        orderSubMenu = ViewMenuBuilder.buildMenu("VOI Order", 0, false);
        orderSubMenu.add(ViewMenuBuilder.buildMenuItem(CustomUIBuilder.PARAM_VOI_FRONT, this, true));
        orderSubMenu.add(ViewMenuBuilder.buildMenuItem(CustomUIBuilder.PARAM_VOI_BACK, this, true));
        orderSubMenu.add(ViewMenuBuilder.buildMenuItem(CustomUIBuilder.PARAM_VOI_FORWARD, this, true));
        orderSubMenu.add(ViewMenuBuilder.buildMenuItem(CustomUIBuilder.PARAM_VOI_BACKWARD, this, true));

        contourOrderSubMenu = ViewMenuBuilder.buildMenu("Contour Order", 0, false);
        contourOrderSubMenu.add(ViewMenuBuilder.buildMenuItem(CustomUIBuilder.PARAM_CONTOUR_FRONT, this, true));
        contourOrderSubMenu.add(ViewMenuBuilder.buildMenuItem(CustomUIBuilder.PARAM_CONTOUR_BACK, this, true));
        contourOrderSubMenu.add(ViewMenuBuilder.buildMenuItem(CustomUIBuilder.PARAM_CONTOUR_FORWARD, this, true));
        contourOrderSubMenu.add(ViewMenuBuilder.buildMenuItem(CustomUIBuilder.PARAM_CONTOUR_BACKWARD, this, true));

        propSubMenu = ViewMenuBuilder.buildMenu("Propagate", 0, false);
        propSubMenu.add(ViewMenuBuilder.buildMenuItem(CustomUIBuilder.PARAM_VOI_PROPAGATE_UP, this, true));
        propSubMenu.add(ViewMenuBuilder.buildMenuItem(CustomUIBuilder.PARAM_VOI_PROPAGATE_DOWN, this, true));
        propSubMenu.add(ViewMenuBuilder.buildMenuItem(CustomUIBuilder.PARAM_VOI_PROPAGATE_ALL, this, true));

        flipSubMenu = ViewMenuBuilder.buildMenu("Flip VOI", 0, false);
        flipSubMenu.add(ViewMenuBuilder.buildMenuItem(CustomUIBuilder.PARAM_VOI_FLIPX, this, true));
        flipSubMenu.add(ViewMenuBuilder.buildMenuItem(CustomUIBuilder.PARAM_VOI_FLIPY, this, true));
        flipSubMenu.add(ViewMenuBuilder.buildMenuItem(CustomUIBuilder.PARAM_VOI_FLIPZ, this, true));

        graphSubMenu = ViewMenuBuilder.buildMenu("Graph", 0, false);
        graphSubMenu.add(ViewMenuBuilder.buildMenuItem(CustomUIBuilder.PARAM_VOI_GRAPH_BOUNDARY_INTENSITY, this, false));

       if ((handler.getActiveImage().getNDims() == 3) ||
                (handler.getActiveImage().getNDims() == 4)) {
            graphSubMenu.add(ViewMenuBuilder.buildMenuItem(CustomUIBuilder.PARAM_VOI_GRAPH_TOTAL_INTENSITY, this, false));
            graphSubMenu.add(ViewMenuBuilder.buildMenuItem(CustomUIBuilder.PARAM_VOI_GRAPH_AVERAGE_INTENSITY, this, false));
            graphSubMenu.add(ViewMenuBuilder.buildMenuItem(CustomUIBuilder.PARAM_VOI_GRAPH_TOTAL_INTENSITY_THRESHOLD, this, false));
            graphSubMenu.add(ViewMenuBuilder.buildMenuItem(CustomUIBuilder.PARAM_VOI_GRAPH_AVERAGE_INTENSITY_THRESHOLD, this, false));
        }

        itemClose = ViewMenuBuilder.buildMenuItem(CustomUIBuilder.PARAM_VOI_CLOSE, this, false);
        itemCrop = ViewMenuBuilder.buildMenuItem(CustomUIBuilder.PARAM_VOI_CROP, this, false);
        itemOutputDistance = ViewMenuBuilder.buildMenuItem(CustomUIBuilder.PARAM_VOI_PSLICE_DISTANCE, this, false);
        
        editCircleDiameter = ViewMenuBuilder.buildMenuItem(CustomUIBuilder.PARAM_VOI_EDIT_CIRCLE_DIAM, this, false);
        
        editSquareLength = ViewMenuBuilder.buildMenuItem(CustomUIBuilder.PARAM_VOI_EDIT_SQUARE_LENGTH, this, false);
        
        // popup will be filled by the checkPopup() method in response to specific mouse events with different types of
        // vois selected
        popup = new JPopupMenu();
        // popup.add(ViewMenuBuilder.buildCheckBoxMenuItem()
    }
    

    /**
     * Creates a new ViewJPopupVOI object.
     *
     * @param  handler  DOCUMENT ME!
     */
    public ViewJPopupVOI(VOIHandlerInterface handler, JFrame parentFrame, VOIBase kVOI) {
        this(handler);
        selectedVOI = kVOI;
    }

    //~ Methods --------------------------------------------------------------------------------------------------------


    /* (non-Javadoc)
     * @see java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent)
     */
    public void actionPerformed(ActionEvent event) {

        if ( ViewMenuBar.isMenuCommand( popup.getComponents(), event.getActionCommand() ) )
        {
            voiHandler.actionPerformed(event);
        } 
        if (event.getActionCommand().equals(CustomUIBuilder.PARAM_VOI_CROP.getActionCommand())) {
            new JDialogCrop(voiHandler.getActiveImage().getParentFrame(),
                    voiHandler.getActiveImage(), true);
        }
        if (event.getActionCommand().equals(CustomUIBuilder.PARAM_VOI_CLOSE.getActionCommand())) {
            closeVOI();
        }
    }

    /* (non-Javadoc)
     * @see java.awt.event.MouseListener#mouseClicked(java.awt.event.MouseEvent)
     */
    public void mouseClicked(MouseEvent event) {
        checkPopup(event);
    }

    /* (non-Javadoc)
     * @see java.awt.event.MouseListener#mouseEntered(java.awt.event.MouseEvent)
     */
    public void mouseEntered(MouseEvent event) { }

    /* (non-Javadoc)
     * @see java.awt.event.MouseListener#mouseExited(java.awt.event.MouseEvent)
     */
    public void mouseExited(MouseEvent event) { }

    /* (non-Javadoc)
     * @see java.awt.event.MouseListener#mousePressed(java.awt.event.MouseEvent)
     */
    public void mousePressed(MouseEvent event) {
        checkPopup(event);
    }

    /* (non-Javadoc)
     * @see java.awt.event.MouseListener#mouseReleased(java.awt.event.MouseEvent)
     */
    public void mouseReleased(MouseEvent event) {
        checkPopup(event);
    }

    /* (non-Javadoc)
     * @see javax.swing.event.PopupMenuListener#popupMenuCanceled(javax.swing.event.PopupMenuEvent)
     */
    public void popupMenuCanceled(PopupMenuEvent event) {
        Preferences.debug("Popup menu will be visible!");
    }

    /* (non-Javadoc)
     * @see javax.swing.event.PopupMenuListener#popupMenuWillBecomeInvisible(javax.swing.event.PopupMenuEvent)
     */
    public void popupMenuWillBecomeInvisible(PopupMenuEvent event) {
        Preferences.debug("Popup menu will be invisible!");
    }

    /* (non-Javadoc)
     * @see javax.swing.event.PopupMenuListener#popupMenuWillBecomeVisible(javax.swing.event.PopupMenuEvent)
     */
    public void popupMenuWillBecomeVisible(PopupMenuEvent event) {
        Preferences.debug("Popup menu will be visible!");
    }

    /**
     * DOCUMENT ME!
     *
     * @param  flag  DOCUMENT ME!
     */
    public void setEnabledOrder(boolean flag) {
        orderSubMenu.setEnabled(flag);
    }

    /**
     * DOCUMENT ME!
     *
     * @param  flag  DOCUMENT ME!
     */
    public void setEnabledPropagate(boolean flag) {
        propSubMenu.setEnabled(flag);
    }

    /**
     * DOCUMENT ME!
     *
     * @param  flag  DOCUMENT ME!
     */
    public void setEnabledProps(boolean flag) {
        itemProps.setEnabled(flag);
    }

    /**
     * Called from the VOIManager class. When a popup is trigged, this sets the VOIBase
     * that triggered the popup.
     * @param kVOI the VOIBase selected by right-mouse click.
     */
    public void setSelectedVOI( VOIBase kVOI )
    {
        selectedVOI = kVOI;
    }
    
    /**
     * DOCUMENT ME!
     *
     * @param  event  DOCUMENT ME!
     */
    private void checkPopup(MouseEvent event) {

        if (event.isPopupTrigger()) {
            popup.removeAll();

           
            popup.add(itemProps);
            popup.addSeparator();
            popup.add(itemTrim);
            popup.addSeparator();
            popup.add(selectionMenu);
            popup.add(editSubMenu);
           // popup.add(showBoundingBox);
            

            if (isPLineSliceVOI()) {
                popup.addSeparator();
                popup.add(itemOutputDistance);
            } else {
                popup.addSeparator();
                popup.add(orderSubMenu);
                popup.add(contourOrderSubMenu);

                popup.addSeparator();
                popup.add(propSubMenu);
                popup.add(flipSubMenu);
                popup.add(graphSubMenu);
                if (!isVOIOpen()) {
                    if(selectedVOI != null && !selectedVOI.isClosed()) {
                        popup.add(itemClose);  //allow drawn contour to become closed
                    }
                    popup.add(itemCrop);
                } else if(selectedVOI != null && selectedVOI.getGroup().getCurves().size() == 1){
                    popup.add(itemClose);
                }
            } 
            if(selectedVOI.getSubtype() == VOIBase.CIRCLE) {
            	popup.add(editCircleDiameter);
            }
            if(selectedVOI.getSubtype() == VOIBase.SQUARE) {
            	popup.add(editSquareLength);
            }
           
            int xAmount = 0;
            int yAmount = 0;
            if ( voiHandler.getActiveImage().getParentFrame()  != null ) {       
                if ( voiHandler.getActiveImage().getParentFrame().getScrollPane()  != null ) {                
                    //These are the number of pixels that the scrollbar of the activeImage has already scrolled and is offset by the actual location of the scroll pane
                    xAmount = voiHandler.getActiveImage().getParentFrame().getScrollPane().getHorizontalScrollBar().getModel().getValue()-voiHandler.getActiveImage().getParentFrame().getScrollPane().getLocation().x;
                }
            }
            if ( voiHandler.getActiveImage().getParentFrame() != null ) {   
                if ( voiHandler.getActiveImage().getParentFrame().getScrollPane()  != null ) {                
                    //These are the number of pixels that the scrollbar of the activeImage has already scrolled and is offset by the actual location of the scroll pane
                    yAmount = voiHandler.getActiveImage().getParentFrame().getScrollPane().getVerticalScrollBar().getModel().getValue()-voiHandler.getActiveImage().getParentFrame().getScrollPane().getLocation().y;
                }
            }
            popup.show(voiHandler.getComponentImage(), event.getX()-xAmount, event.getY()-yAmount);
        }
    }


    /**
     * Sets a POLYLINE VOI to closed CONTOUR(changes from polyline to polygon).
     */
    private void closeVOI() {        
        VOIVector VOIs = voiHandler.getActiveImage().getVOIs();

        Vector<VOIBase> curves = null;
        VOIBase tester = null;

        for (int i = 0; i < VOIs.size(); i++) {

            if (VOIs.VOIAt(i).isActive()) {
                VOIs.VOIAt(i).setCurveType(VOI.CONTOUR);
                curves = VOIs.VOIAt(i).getCurves();
                for (int k = 0; k < curves.size(); k++) {
                    tester = (VOIBase) curves.elementAt(k);
                    if (tester instanceof VOIContour) {
                        ((VOIContour) tester).setClosed(true);
                    }
                }

                voiHandler.getActiveImage().notifyImageDisplayListeners(null, true);
                return;
            }
        }
    }

    /**
     * DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    private boolean isPLineSliceVOI() {
        if ( selectedVOI != null )
        {
            return (selectedVOI.getGroup().getCurveType() == VOI.POLYLINE_SLICE); 
        }
        
        VOIVector VOIs = voiHandler.getActiveImage().getVOIs();

        for (int i = 0; i < VOIs.size(); i++) {

            if (VOIs.VOIAt(i).isActive() && (VOIs.VOIAt(i).getCurveType() == VOI.POLYLINE_SLICE)) {
                return true;
            }
        }


        return false;
    }

    /**
     * Checks to see if the active VOI is a polyline.
     *
     * @return  boolean is the active VOI a polyline (not polygon)
     */
    private boolean isVOIOpen() {
        if ( selectedVOI != null )
        {
            return (selectedVOI.getGroup().getCurveType() == VOI.POLYLINE); 
        }
        
        VOIVector VOIs = voiHandler.getActiveImage().getVOIs();

        for (int i = 0; i < VOIs.size(); i++) {

            if (VOIs.VOIAt(i).isActive()) {

                if (VOIs.VOIAt(i).getCurveType() == VOI.POLYLINE) {
                    return true;
                }
            }
        }

        return false;
    }

}
