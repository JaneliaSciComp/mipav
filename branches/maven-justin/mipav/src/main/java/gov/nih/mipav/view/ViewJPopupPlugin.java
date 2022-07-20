package gov.nih.mipav.view;




import gov.nih.mipav.view.dialogs.*;

import java.awt.event.*;



import javax.swing.*;
import javax.swing.event.*;


/**
 * Pop-up menu for uninstalling a plugin.
 *
 * @author   senseneyj
 */
public class ViewJPopupPlugin extends JPanel implements ActionListener, PopupMenuListener, MouseListener {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

	public static final String UNINSTALL = "Uninstall";
	
	/** There only needs to be one ViewJPopupPlugin in MIPAV*/
	protected static ViewJPopupPlugin popupPluginStaticRef = new ViewJPopupPlugin();
	
    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** The uninstall menu item */
    private JMenuItem itemProps;

    /** The stored popup menu */
    private JPopupMenu popup;
    
    /**String text of the button representing the plugin to be uninstalled. */
    private String pluginName;
    
    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Creates a new ViewJPopupVOI object.
     *
     * @param  handler  DOCUMENT ME!
     */
    public ViewJPopupPlugin() {

        itemProps = ViewMenuBuilder.buildMenuItem("Uninstall", UNINSTALL, 0, this, null, true);
        
        // popup will be filled by the checkPopup() method in response to specific 
        // mouse events while hovering over a particular plugin
        popup = new JPopupMenu();
    }
    
    public static ViewJPopupPlugin getReference() {
    	return popupPluginStaticRef;
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * Will execute the uninstall action
     */
    public void actionPerformed(ActionEvent event) {
        if (event.getActionCommand().equals(UNINSTALL)) {
            new JDialogUninstallPlugin(pluginName);
        } 
    }

    /**
     * Checks whether the mouse event is a normal system event for popups.
     */
    public void mouseClicked(MouseEvent event) {
        checkPopup(event);
    }

    public void mouseEntered(MouseEvent event) { }

    public void mouseExited(MouseEvent event) { }

    /**
     * Checks whether the mouse event is a normal system event for popups.
     */
    public void mousePressed(MouseEvent event) {
        checkPopup(event);
    }

    /**
     * Checks whether the mouse event is a normal system event for popups.
     */
    public void mouseReleased(MouseEvent event) {
        checkPopup(event);
    }

    /**
     * Popmenu canceled
     */
    public void popupMenuCanceled(PopupMenuEvent event) {
        Preferences.debug("Popup menu will be visible!");
    }

    /**
     * Popmenu unvisible
     */
    public void popupMenuWillBecomeInvisible(PopupMenuEvent event) {
        Preferences.debug("Popup menu will be invisible!");
    }


    /**
     * Popmenu unvisible
     */
    public void popupMenuWillBecomeVisible(PopupMenuEvent event) {
        Preferences.debug("Popup menu will be visible!");
    }

    /**
     * Checks whether the popup should be called
     *
     * @param  event A mouse event that may be a popup trigger
     */
    private void checkPopup(MouseEvent event) {

        if (event.isPopupTrigger()) {
            //popup.removeAll();

            popup.add(itemProps);
            
            if(event.getSource() instanceof JMenuItem) {
            	pluginName = ((JMenuItem)event.getSource()).getName();
            	popup.show(ViewUserInterface.getReference().getMainFrame(), ((JMenuItem)event.getSource()).getX(), ((JMenuItem)event.getSource()).getY());
            	
            	ViewMenuBuilder builder = ViewUserInterface.getReference().getMenuBuilder();
            	
            	JMenuItem item = ViewUserInterface.getReference().getMenuBuilder().getMenuItem("Install plugin");
            	item.setSelected(true);
            	item.getParent().setVisible(true);
            	((JMenuItem)event.getSource()).getParent().setVisible(true);
            	
            	popup.show(ViewUserInterface.getReference().getMainFrame(), ((JMenuItem)event.getSource()).getX(), ((JMenuItem)event.getSource()).getY());
            } else {
            	pluginName = "Unknown";
            }
        }
    }
}
