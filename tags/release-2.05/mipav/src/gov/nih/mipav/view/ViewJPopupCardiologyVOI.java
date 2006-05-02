package gov.nih.mipav.view;


import java.awt.event.*;

import javax.swing.*;
import javax.swing.event.*;


/**
 * DOCUMENT ME!
 *
 * @version  1.0 July 27, 1999
 * @author   Matthew J. McAuliffe, Ph.D. (primary)
 * @author   Harman Singh
 */
public class ViewJPopupCardiologyVOI extends JPanel implements ActionListener, PopupMenuListener, MouseListener {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** Use serialVersionUID for interoperability. */
    private static final long serialVersionUID = -5496458990593918502L;

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** DOCUMENT ME! */
    private JMenu calcSubMenu;

    /** DOCUMENT ME! */
    private ViewJComponentCardiology component;

    /** DOCUMENT ME! */
    private JPopupMenu popup;

    /** DOCUMENT ME! */
    private JMenu sectionSubMenu;

    /** DOCUMENT ME! */
    private JMenu voiSubMenu;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Creates a new ViewJPopupCardiologyVOI object.
     *
     * @param  comp  DOCUMENT ME!
     */
    public ViewJPopupCardiologyVOI(ViewJComponentCardiology comp) {
        popup = new JPopupMenu();
        sectionSubMenu = new JMenu("Section");
        voiSubMenu = new JMenu("VOI");
        calcSubMenu = new JMenu("Calculate");
        component = comp;

        JMenuItem itemSection, itemCalc;
        JCheckBoxMenuItem itemVOI;

        itemSection = new JMenuItem("Section selector");
        itemSection.setActionCommand("selector");

        itemSection.addActionListener(this);
        sectionSubMenu.add(itemSection);


        itemSection = new JMenuItem("Select all");
        itemSection.setActionCommand("selectAll");


        itemSection.addActionListener(this);
        sectionSubMenu.add(itemSection);


        itemSection = new JMenuItem("Select none");
        itemSection.setActionCommand("selectNone");

        itemSection.addActionListener(this);
        sectionSubMenu.add(itemSection);


        itemVOI = new JCheckBoxMenuItem("Show points", true);
        itemVOI.setActionCommand("togglePoints");
        itemVOI.addActionListener(this);
        voiSubMenu.add(itemVOI);

        itemCalc = new JMenuItem("Calculate average lengths");
        itemCalc.setActionCommand("calcLengths");
        itemCalc.addActionListener(this);
        calcSubMenu.add(itemCalc);

        itemCalc = new JMenuItem("Calculate average intensities");
        itemCalc.setActionCommand("calcIntensities");
        itemCalc.addActionListener(this);
        calcSubMenu.add(itemCalc);


        popup.add(sectionSubMenu);
        popup.addSeparator();
        popup.add(voiSubMenu);
        popup.addSeparator();
        popup.add(calcSubMenu);
    }

    //~ Methods --------------------------------------------------------------------------------------------------------


    /**
     * DOCUMENT ME!
     *
     * @param  event  DOCUMENT ME!
     */
    public void actionPerformed(ActionEvent event) {

        if (event.getActionCommand().equals("calcLengths")) {
            component.calculateVOILengths();
        } else if (event.getActionCommand().equals("togglePoints")) {
            component.toggleVOIPoints();
        } else if (event.getActionCommand().equals("selectAll")) {
            component.selectAllVOISections(true);
        } else if (event.getActionCommand().equals("selectNone")) {
            component.selectAllVOISections(false);
        } else if (event.getActionCommand().equals("calcIntensities")) {
            component.calculateVOIIntensities();
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
     * DOCUMENT ME!
     *
     * @param  event  DOCUMENT ME!
     */
    private void checkPopup(MouseEvent event) {

        if (event.isPopupTrigger()) {
            popup.show(component, event.getX(), event.getY());
        }
    }


}
