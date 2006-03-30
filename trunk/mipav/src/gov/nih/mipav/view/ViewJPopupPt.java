package gov.nih.mipav.view;


import java.awt.event.*;
import javax.swing.*;
import javax.swing.event.*;

/**
*
*		@version    1.0 July 10, 2001
*		@author     Harman Singh
*
*/
public class ViewJPopupPt extends JPanel implements ActionListener, PopupMenuListener, MouseListener {

    private     JPopupMenu              ptPopup;
    private     JMenuItem               itemShowGraph;
    private     JMenuItem               itemShowPAAIDialog;
    private     JMenuItem               itemBuildPolyline;
    private     JMenuItem               itemProps;
    private     JCheckBoxMenuItem       itemShowVOIName;
    private     JMenu                   propSubMenu;
    private     ViewJComponentEditImage component;

    public ViewJPopupPt(ViewJComponentEditImage comp){

        try{
            ptPopup              = new JPopupMenu();
            propSubMenu          = ViewMenuBuilder.buildMenu("Propagate", 0, false);
            itemShowGraph        = ViewMenuBuilder.buildMenuItem("Show VOI Graph", "ShowGraph", 0, this, null, false);
            itemShowPAAIDialog   = ViewMenuBuilder.buildMenuItem("Point area average intensities", "ShowPAIIDialog", 0, this, null, false);
            itemBuildPolyline    = ViewMenuBuilder.buildMenuItem("Build inter-frame poly-line", "BuildPoly", 0, this, null, false);
            itemShowVOIName      = ViewMenuBuilder.buildCheckBoxMenuItem("Show VOI name", "ShowName", this,
                Preferences.is(Preferences.PREF_SHOW_VOI_NAME));
        itemProps = ViewMenuBuilder.buildMenuItem("Properties","Properties",0,this,null, true);
            component            = comp;
        }
        catch (OutOfMemoryError error) {
            System.gc();
            MipavUtil.displayError("Out of memory: ViewJPopupPt Constructor");
            return;
        }

        propSubMenu.add(ViewMenuBuilder.buildMenuItem("To Next Slice", "PropVOIUp", 0, this, "voipropu.gif", true));
        propSubMenu.add(ViewMenuBuilder.buildMenuItem("To Previous Slice", "PropVOIDown", 0, this, "voipropd.gif", true));
        propSubMenu.add(ViewMenuBuilder.buildMenuItem("To All Slices", "PropVOIAll", 0, this, null, true));

        ptPopup.add(propSubMenu);

        ptPopup.addSeparator();
        ptPopup.add(itemShowGraph);
        ptPopup.add(itemShowPAAIDialog);
        ptPopup.addSeparator();
        ptPopup.add(itemShowVOIName);
        ptPopup.add(itemBuildPolyline);

    }


    public void actionPerformed(ActionEvent event) {

        try {
            if (event.getActionCommand().equals("ShowGraph")) {
                component.setGraphVisible();
            }
            else if (event.getActionCommand().equals("PropVOIUp")){
                if (component.propVOI(1, false) == true ) {
                    ((ViewJFrameImage)component.getFrame()).incSlice();
                }
            }
            else if (event.getActionCommand().equals("PropVOIDown")){
                if (component.propVOI(-1, false) == true) {
                    ((ViewJFrameImage)component.getFrame()).decSlice();
                }
            }
            else if (event.getActionCommand().equals("PropVOIAll")){
                component.propVOIAll();
            }
            else if (event.getActionCommand().equals("ShowPAAIDialog")) {
              component.setPAAIGraphVisible();
            }
            else if (event.getActionCommand().equals("ShowName")) {
               Preferences.setProperty(Preferences.PREF_SHOW_VOI_NAME, Boolean.toString(itemShowVOIName.isSelected()));
               component.getFrame().updateImages();
            }
            else if (event.getActionCommand().equals("Properties")) {
                component.showVOIProperties(false);
            }

        }
        catch (OutOfMemoryError error) {
            System.gc();
            MipavUtil.displayError("Out of memory: ViewJPopupPt.action.");
            return;
        }
    }

    public void mousePressed(MouseEvent event)  { checkPopup(event);}
    public void mouseClicked(MouseEvent event)  { checkPopup(event);}
    public void mouseEntered(MouseEvent event)  { }
    public void mouseExited(MouseEvent event)   { }
    public void mouseReleased(MouseEvent event) { checkPopup(event);}

    private void checkPopup(MouseEvent event) {
        if (event.isPopupTrigger()) {
            itemShowVOIName.setSelected(Preferences.is(Preferences.PREF_SHOW_VOI_NAME));
            ptPopup.show(component, event.getX(), event.getY());
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

    /*
    * Enables or disables the show graph menu item accordingly
    * @param enab whether or not to enable
    */
    public void setEnabledGraph(boolean enab){
        itemShowGraph.setEnabled(enab);
    }

    /*
    * Enables or disables the propSub menu item accordingly
    * @param enab whether or not to enable
    */
    public void setEnabledProp(boolean enab){
        propSubMenu.setEnabled(enab);
    }
}

