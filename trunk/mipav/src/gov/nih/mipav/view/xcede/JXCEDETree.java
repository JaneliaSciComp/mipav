package gov.nih.mipav.view.xcede;

import javax.swing.JTree;
import java.awt.event.ActionListener;
import java.awt.event.ActionEvent;
import java.awt.event.MouseListener;
import java.awt.event.MouseEvent;
import java.awt.event.MouseAdapter;

import javax.swing.JPopupMenu;
import javax.swing.JMenuItem;
import javax.swing.tree.TreePath;

import gov.nih.mipav.model.file.xcede.*;
import gov.nih.mipav.view.*;

public class JXCEDETree extends JTree implements ActionListener{
    /**
     * The root node of this tree.
     */
    private XCEDEElement root;
    
    private JPopupMenu popupMenu;
    
    private MouseListener popupListener;
    /**
     * Constructs a new JXCEDETree object.
     * @param root
     */
    public JXCEDETree(XCEDEElement root){
        this.root = root;
        setModel(new JXCEDETreeModel(root));
        JXCEDETreeCellRenderer renderer = new JXCEDETreeCellRenderer();
        setCellRenderer(renderer);
        setEditable(false);
        
    }
    
    protected void finalize() throws Throwable {
        System.out.println("Entering JXCEDETree's finalize() ... ");
        root = null;
        popupMenu = null;
        popupListener = null;
        super.finalize();
    }
    
    public void createPopupMenu(){
        popupMenu = new JPopupMenu();
        JMenuItem menuItem = new JMenuItem("Display");
        menuItem.addActionListener(this);
        popupMenu.add(menuItem );
        
        popupListener = new PopupListener();
        this.addMouseListener(popupListener);
    }
    
    /*********************************
     ***** Action Event Listener *****
     *********************************
     */
    public void actionPerformed(ActionEvent e){
        String command = e.getActionCommand();
        if(command.equals("Display")){
            TreePath[] selectedPaths = this.getSelectionPaths();
            if(selectedPaths != null){
                for(int i = 0; i < selectedPaths.length; i++){
                }
            }
        }
    }
    
    private class PopupListener extends MouseAdapter{
        public void mousePressed(MouseEvent e) {
            triggerPopup(e);
        }

        public void mouseReleased(MouseEvent e) {
            triggerPopup(e);
        }

        private void triggerPopup(MouseEvent e) {
            if (e.isPopupTrigger()) {
                if (getLastSelectedPathComponent() == null) {
                    return;
                }
                popupMenu.show(e.getComponent(), e.getX(), e.getY());
            }

        }
    }
}
