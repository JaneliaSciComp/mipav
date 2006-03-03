package gov.nih.mipav.view;

import javax.swing.*;
import javax.swing.event.*;
import java.awt.event.*;

/**
*
*
*		@version    1.0 July 27, 1999
*		@author     Matthew J. McAuliffe, Ph.D. (primary)
*		@author     Harman Singh
*
*/
public class ViewJPopupCardiologyVOI extends JPanel implements ActionListener, PopupMenuListener, MouseListener {
    private JPopupMenu popup;
    private JMenu sectionSubMenu;
    private JMenu voiSubMenu;
    private JMenu calcSubMenu;
    private ViewJComponentCardiology component;

    public ViewJPopupCardiologyVOI(ViewJComponentCardiology comp) {
        popup           = new JPopupMenu();
        sectionSubMenu  = new JMenu("Section");
        voiSubMenu      = new JMenu("VOI");
        calcSubMenu     = new JMenu("Calculate");
        component       = comp;

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

    public void mousePressed(MouseEvent event)  { checkPopup(event);}
    public void mouseClicked(MouseEvent event)  { checkPopup(event);}
    public void mouseEntered(MouseEvent event)  { }
    public void mouseExited(MouseEvent event)   { }
    public void mouseReleased(MouseEvent event ) { checkPopup(event);}

    private void checkPopup(MouseEvent event) {
        if (event.isPopupTrigger()) {
            popup.show(component, event.getX(), event.getY());
       }
    }


    public void actionPerformed(ActionEvent event) {

      if (event.getActionCommand().equals("calcLengths")) {
        component.calculateVOILengths();
      }
      else if (event.getActionCommand().equals("togglePoints")) {
        component.toggleVOIPoints();
      }
      else if (event.getActionCommand().equals("selectAll")) {
        component.selectAllVOISections(true);
      }
      else if (event.getActionCommand().equals("selectNone")) {
        component.selectAllVOISections(false);
      }
      else if (event.getActionCommand().equals("calcIntensities")) {
        component.calculateVOIIntensities();
      }



    }



    public void popupMenuWillBecomeVisible(PopupMenuEvent event) {
        Preferences.debug("Popup menu will be visible!");
    }

    public void popupMenuWillBecomeInvisible(PopupMenuEvent event) {
        Preferences.debug("Popup menu will be invisible!");
    }

    public void popupMenuCanceled(PopupMenuEvent event) {
        Preferences.debug("Popup menu will be visible!");
    }


}
