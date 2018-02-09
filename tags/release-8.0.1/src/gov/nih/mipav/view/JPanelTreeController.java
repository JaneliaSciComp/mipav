package gov.nih.mipav.view;


/* panel to hold and transfer items between two trees.
 * extend to modify the application of the 'left', 'right'
 * and delete.  delete has not been implemented.
 * parsonsd
 */

import java.awt.*;
import java.awt.event.*;

import java.util.ArrayList;


import javax.swing.*;
import javax.swing.event.*;
import javax.swing.tree.DefaultTreeModel;
import javax.swing.tree.MutableTreeNode;

import javax.swing.tree.TreeNode;
import javax.swing.tree.TreePath;


/**
 * panel to hold and transfer items between two trees. extend to modify the application of the 'left', 'right', and
 * 'delete' or 'up', 'down' and 'delete'. Use of delete has not been implemented, and left un-enabled; extend this panel
 * and extend the ActionEvent handler to define its use.
 *
 * <p>Creating an instance of this <code>JPanel</code> does not in fact create any visual representation of either tree.
 * It does display the tree-control buttons, but the <code>JTree</code>s held (<code>TreeA</code> and <code>
 * TreeB</code>) are references. This is done so that the display may have as much freedom for display as possible (ie,
 * control spacing, control alignment and position, or insert explanation text). As such, the display panel which
 * contains the trees is created seperately.</p>
 *
 * @author   senseneyj
 * @version  1
 *
 *           <p>$Logfile: /mipav/src/gov/nih/mipav/view/JPanelTreeController.java $</p>
 */
public class JPanelTreeController extends JPanel implements ActionListener, TreeSelectionListener {
    // left and right are very subjective terms here.  user should be careful to consider visual location when coding.

    //~ Static fields/initializers -------------------------------------------------------------------------------------

	public static final String TREEA = "treeA";
	
	public static final String TREEB = "treeB";
	
	public static final String ALLTREEA = "allTreeA";
	
	public static final String ALLTREEB = "allTreeB";

    //~ Instance fields ------------------------------------------------------------------------------------------------

    // references to where we point the directional arrows.
    // should be assigned when this has been added to the
    // component to which it treeens....

    /** upper or left-most tree. */
    protected JTree treeA;

    /** lower or right-most tree. */
    protected JTree treeB;

    /** 'X'. */
    private JButton deleteSelection;

    /** DOCUMENT ME! */
    private int treeLayout;

    /** DOCUMENT ME! */
    private JButton sendAllToTreeA;

    /** DOCUMENT ME! */
    private JButton sendAllToTreeB;

    /** left arrow or up arrow. */
    private JButton sendSelectionToTreeA;

    /** right arrow or down arrow. */
    private JButton sendSelectionToTreeB;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * defualt position is all buttons vertical. Images are by default shown on the buttons. Both arrows are enabled,
     * but Delete is disabled by default. panel is layed out as a BoxLayout.Y_AXIS
     *
     * @see  javax.swing.BoxLayout
     */
    public JPanelTreeController() {
        this(BoxLayout.Y_AXIS);
        /* add buttons.  assume graphic look
         *   ->  (send right)  <-  (send left)   X  (remove)
         */
    }

    /**
     * Creates a tree controller, with the buttons in the given layout, but which defaults to display images only. The
     * buttons point as if to trees in the orthogonal direction. Choose layout with: <code>BoxLayout.X_AXIS</code>, or
     * <code>BoxLayout.Y_AXIS</code>. Both arrows are enabled, but Delete is disabled by default.
     *
     * @see    javax.swing.BoxLayout
     *
     * @param  layout  The <CODE>BoxLayout</CODE> designation for the layout of the buttons in the panel. Use <CODE>
     *                 BoxLayout.X_AXIS</CODE> or <CODE>BoxLayout.Y_AXIS</CODE>.
     */
    public JPanelTreeController(int layout) {
        this(layout, false);
    }

    /**
     * Creates a tree controller, with the buttons in the default layout, but which can be set to display images only or
     * text. The buttons point as if to trees in the orthogonal direction. Both arrows are enabled, but Delete is
     * disabled by default. Panel is layed out as a <code>BoxLayout.Y_AXIS</code>, and points to trees that would be
     * laid-out along the x-axis of the display panel.
     *
     * @see    javax.swing.BoxLayout
     *
     * @param  noImages  Specifies if the buttons are to be displayed as images or with no images, using text instead.
     */
    public JPanelTreeController(boolean noImages) {
        this(BoxLayout.Y_AXIS, noImages);
    }

    /**
     * Creates a tree controller, with the buttons in the given layout, and which can be set to display images only or
     * text. The buttons point as if to trees in the orthogonal direction. Both arrows are enabled, but Delete is
     * disabled by default. Choose layout of the buttons with: <code>BoxLayout.X_AXIS</code>, or <code>
     * BoxLayout.Y_AXIS</code>.
     *
     * @see  javax.swing.BoxLayout
     */
    public JPanelTreeController(int layout, boolean noImages) {
        super();
        super.setLayout(new BoxLayout(this, layout));

        // the trees are layed out exactly opposite of the panel-layout.
        treeLayout = (layout == BoxLayout.X_AXIS) ? BoxLayout.Y_AXIS : BoxLayout.X_AXIS;

        // create button labels based on image preferences
        if (noImages) {
            createAllLeftButton(true);
            createLeftButton(true);
            createRightButton(true);
            createAllRightButton(true);
            createDeleteButton(true); // auto-set to disabled
        } else { // okay to use icons
            createAllLeftButton(false);
            createLeftButton(false);
            createRightButton(false);
            createAllRightButton(false);
            createDeleteButton(false); // auto-set to disabled
        }

        // put them onto the panel for display
        add(sendAllToTreeA);
        add(sendSelectionToTreeA);
        add(deleteSelection);
        add(sendSelectionToTreeB);
        add(sendAllToTreeB);
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * Creates a simple Tree controller display. one with the the given controls and with the orientation defined by the
     * control panel. The trees in the control panel are displayed, but if either are <code>null</code>, an new, empty
     * tree is provided.
     *
     * <p>The referenced trees are placed into <code>JScrollPane</code>s, and placed on either side of the control's
     * buttons, using a BorderLayout.</p>
     *
     * @param   controls  A JPanelTreeController to place into a visual display.
     *
     * @return  A JSplitPane containing the tree controller, formatted as the JPanelTreeController indicates (ie.,
     *          vertical or horizontal layout).
     */
    public static JSplitPane createTreeControllerDisplay(JPanelTreeController controls) {
        JSplitPane fullSelector;

        // add logical trees to the visual representation
        JPanel selectorVisual = new JPanel(new BorderLayout());

        // trees are vertical:
        if (controls.getTreeLayout() == BoxLayout.Y_AXIS) {

            if (controls.getUpperTree() == null) {
                controls.setUpperTree(new JTree());
            }

            if (controls.getLowerTree() == null) {
                controls.setLowerTree(new JTree());
            }

            selectorVisual.add(new JScrollPane(controls.getUpperTree()), BorderLayout.CENTER);
            selectorVisual.add(controls, BorderLayout.SOUTH);
            fullSelector = new JSplitPane(JSplitPane.VERTICAL_SPLIT, true, selectorVisual,
                                          new JScrollPane(controls.getLowerTree()));
        } else { // trees are along the horizontal:

            if (controls.getLeftTree() == null) {
                controls.setLeftTree(new JTree());
            }

            if (controls.getRightTree() == null) {
                controls.setRightTree(new JTree());
            }

            selectorVisual.add(new JScrollPane(controls.getLeftTree()), BorderLayout.CENTER);
            selectorVisual.add(controls, BorderLayout.EAST);
            fullSelector = new JSplitPane(JSplitPane.HORIZONTAL_SPLIT, true, selectorVisual,
                                          new JScrollPane(controls.getLowerTree()));
        }

        return fullSelector;
    }

    /**
     * responds to the button presses.
     *
     * <P>functionality of arrow/send-to- arrows is virtually identical. the 'delete' is not supported, and this class
     * must be extended to assume functionality for that button. There are good reasons for this: there are two panels,
     * and only one delete button, and delete functionality may be use-dependant. Indeed, for a panel such as a
     * file-tree, we may not <i>want</i> to support a 'delete' function. Hence, this aspect of the use, is left up to an
     * extending class.</P>
     *
     * @param  ae  The action to treeen for.
     */
    public void actionPerformed(ActionEvent ae) {
        String command = ae.getActionCommand();

        // process the button performed.

        // the buttons won't work getting trees if either are null.
        if ((treeB == null) || (treeA == null)) {
            return;
        }

        // user sending from the right panel to the left panel.
        if (command.equals(TREEA)) {
            copySelected(treeB, treeA);
        }

        // specific command to transfer the entire tree from B to A
        if (command.equals(ALLTREEA)) {
            treeA.setModel(treeB.getModel());
        }

        // user sending from the left panel to the right panel.
        if (command.equals(TREEB)) {
            copySelected(treeA, treeB);
        }

        // specific command to transfer the entire tree from A to B
        if (command.equals(ALLTREEB)) {
            treeB.setModel(treeA.getModel());
        }

    }

    /**
     * Convience method to get tree A.
     *
     * @see  #setTreeA
     */
    public JTree getLeftTree() {
        return getTreeA();
    }

    /**
     * there is no programmatic way to determine which arrow is pointing where. To compensate, the parent panel must
     * notify the panel who it is treeening to. Since it can, at most, point to one tree, only the most recent tree to
     * be submitted will be retainted.
     *
     * <p>This method gets the panel pointing to tree A. (upper or left most)</p>
     *
     * @return  DOCUMENT ME!
     */
    public JTree getTreeA() {
        return treeA;
    }

    /**
     * there is no programmatic way to determine which arrow is pointing where. To compensate, the parent panel must
     * notify the panel who it is treeening to. Since it can, at most, point to one tree, only the most recent tree to
     * be submitted will be retainted.
     *
     * <p>This method gets the panel pointing to the tree B (lower or right most)</p>
     *
     * @return  <CODE>TreeB</CODE> is returned.
     */
    public JTree getTreeB() {
        return treeB;
    }

    /**
     * Convience method to get Tree B.
     *
     * @see     #setLowerTree
     * @see     #getTreeB
     *
     * @return  <CODE>TreeB</CODE> is returned.
     */
    public JTree getLowerTree() {
        return getTreeB();
    }

    /**
     * Convience method to get Tree B.
     *
     * @see     #setRightTree
     * @see     #getTreeB
     *
     * @return  <CODE>TreeB</CODE> is returned.
     */
    public JTree getRightTree() {
        return getTreeB();
    }

    /**
     * Convience method to get Tree A.
     *
     * @see     #setTreeA
     *
     * @return  <CODE>TreeA</CODE> is returned.
     */
    public JTree getUpperTree() {
        return getTreeA();
    }

    /**
     * Programmatically performs a 'delete' button click, based on however the implemented class performs it. The delete
     * button must be enabled to perform a programmatic action.
     */
    public void performDelete() {

        if (deleteSelection.isEnabled()) {
            deleteSelection.doClick();
        }
    }

    /**
     * Programmatically performs a 'Send to tree a' button click, based on however the implemented class performs it.
     * The 'send to tree a' button must be enabled to perform a programmatic action.
     */
    public void performSendToTreeA() {

        if (sendSelectionToTreeA.isEnabled()) {
            sendSelectionToTreeA.doClick();
        }
    }

    /**
     * Programmatically performs a 'Send to tree b' button click, based on however the implemented class performs it.
     * The 'send to tree b' button must be enabled to perform a programmatic action.
     */
    public void performSendToTreeB() {
  
        if (sendSelectionToTreeB.isEnabled()) {
            sendSelectionToTreeB.doClick();
        }
    }

    /**
     * A convenience method to set the to-tree-A buttons (both selected and all) to the given enabled status.
     *
     * @param  b  The enabled status of the buttons.
     */
    public void setBackArrowEnabled(boolean b) {
        sendSelectionToTreeA.setEnabled(b);
        sendAllToTreeA.setEnabled(b);
    }

    /**
     * A method to set the delete button to the given enabled status. By default, the delete button is not enabled when
     * this object is created.
     *
     * @param  b  The enabled status of the button.
     */
    public void setDeleteEnabled(boolean b) {
        deleteSelection.setEnabled(b);
    }

    /**
     * A convenience method to set the to-tree-B buttons (both selected and all) to the given enabled status.
     *
     * @param  b  The enabled status of the buttons.
     */
    public void setForwardArrowEnabled(boolean b) {
        sendSelectionToTreeB.setEnabled(b);
        sendAllToTreeB.setEnabled(b);
    }
    
    /**
     * A convenience method to set the to-tree-A buttons (both selected and all) to the given visible status.
     *
     * @param  v  Whether to make buttons visible
     */
    public void setBackArrowVisble(boolean v) {
        sendSelectionToTreeA.setVisible(v);
        sendAllToTreeA.setVisible(v);
    }

    /**
     * A method to set the delete button to the given enabled status. By default, the delete button is visible when
     * this object is created.
     *
     * @param  v  Whether to make button visible
     */
    public void setDeleteVisble(boolean v) {
        deleteSelection.setVisible(v);
    }

    /**
     * A convenience method to set the to-tree-B buttons (both selected and all) to the given visible status.
     *
     * @param  v  Whether to make buttons visible
     */
    public void setForwardArrowVisible(boolean v) {
        sendSelectionToTreeB.setVisible(v);
        sendAllToTreeB.setVisible(v);
    }

    /**
     * Convenience method to set tree A.
     *
     * @see  #setTreeA
     */
    public void setLeftTree(JTree tree) {
        setTreeA(tree);
    }

    /**
     * there is no programmatic way to determine which arrow is pointing where. To compensate, the parent panel must
     * notify the panel who it is listening to. Since it can, at most, point to one tree, only the most recent tree to
     * be submitted will be retainted.
     *
     * <p>This method makes the pointing to the tree A (the upper or left most)</p>
     *
     * @param  tree  DOCUMENT ME!
     */
    public void setTreeA(JTree tree) {
        treeA = tree;
    }

    /**
     * there is no programmatic way to determine which arrow is pointing where. To compensate, the parent panel must
     * notify the panel who it is listening to. Since it can, at most, point to one tree, only the most recent tree to
     * be submitted will be retainted.
     *
     * <p>This method makes the pointing to the tree B (the lower, or right most)</p>
     *
     * @param  tree  DOCUMENT ME!
     */
    public void setTreeB(JTree tree) {
        treeB = tree;
    }

    /**
     * Convience method to set tree B.
     *
     * @see  #setTreeB
     */
    public void setLowerTree(JTree tree) {
        setTreeB(tree);
    }

    /**
     * Convience method to set tree B.
     *
     * @see  #setTreeB
     */
    public void setRightTree(JTree tree) {
        setTreeB(tree);
    }

    /**
     * Convience method to set tree A.
     *
     * @see  #setTreeA
     */
    public void setUpperTree(JTree tree) {
        setTreeA(tree);
    }

    /**
     * handles tree change events made when selecting from either left or right tree. <B>This method has not been
     * implemented</b>
     *
     * <p>If the event isn't from a JTree, an error messge is generated, and control is returned.</p>
     *
     * <p>Otherwise, the tree is copied to the appropriate storage <code>Vector</code>s to hold the tree.</p>
     *
     * @param  lse  The selection event to treeen for.
     */
    public void valueChanged(TreeSelectionEvent lse) {
        JTree tree;

        // only work if the event is type of JTree
        try {
            tree = (JTree) (lse.getSource());
        } catch (ClassCastException cce) {
            //System.out.println("not a JTree.");

            return;
        }

        if (tree == treeA) {
            // go through all the tree and if it was selected,
            // add it to the tree of elements to be held onto,
            // in case the send button is used.
            /*for (int i=0; i < tree.getModel().getSize(); i++) {
             *  if (tree.isSelectedIndex(i)) {     //leftSelections.add(tree.getModel().getElementAt(i));
             * System.out.println(tree.getModel().getElementAt(i)); }}*/
        } else if (tree == treeB) {
            // go through all the tree and if it was selected,
            // add it to the tree of elements to be held onto,
            // in case the send button is used.
            /*for (int i=0; i < tree.getModel().getSize(); i++) {
             *  if (tree.isSelectedIndex(i)) {     //rightSelections.add(tree.getModel().getElementAt(i)); } }
             */
        } else { /* nothing here to do */
        }
        // in particular, we are doing nothing with 'DELETE'
    }

    /**
     * Copies the selected items in tree A and appends them to tree B. The selected items in tree A are then
     * de-selected.
     *
     * @param  a  the source tree.
     * @param  b  the destination tree.
     */
    protected void copySelected(JTree a, JTree b) {

        // ignore if there are no selections made.
        if (a.isSelectionEmpty()) {
            return;
        }

        // Next, copy in the selected values at the bottom.
        TreePath[] selectedValues = a.getSelectionModel().getSelectionPaths();

        // Lastly, copy the total tree into the tree to view and clean up.
        b.addSelectionPaths(selectedValues);
       //((DefaultTreeModel) b.getModel()).in
        a.clearSelection();
    }

    /**
     * Removes all selected items in the given tree.
     *
     * @param  tree  The tree of items.
     */
    protected void deleteFrom(JTree tree) {

        // ignore if there are no selections made.
        if (tree == null) {
            return;
        }

        if (!tree.isSelectionEmpty()) {

        	TreeNode[] nodes = getSelectedNodes(tree);
        	for(int i=0; i<nodes.length; i++) {
        		((DefaultTreeModel)tree.getModel()).removeNodeFromParent((MutableTreeNode)nodes[i]);
        	}
        }
    }

    /**
     * Returns the layout of the associated display panel or &quot;how the arrows point&quot;, <em>not</em> the layout
     * of the buttons themselves.
     *
     * @return  Provides the <CODE>BoxLayout</CODE> code for the intended layout of the associated display panel. Either
     *          <CODE>BoxLayout.X_AXIS</CODE> or <CODE>BoxLayout.Y_AXIS</CODE>.
     */
    protected int getTreeLayout() {
        return treeLayout;
    }


    /**
     * adds the tooltip, and the action label to the buttoin, as well as setting the action treeener, font, borderpaint,
     * and the enable.
     *
     * @param  button       DOCUMENT ME!
     * @param  actionLabel  DOCUMENT ME!
     * @param  tip          DOCUMENT ME!
     */
    protected void setButtonFeatures(JButton button, String actionLabel, String tip) {
        button.setActionCommand(actionLabel);
        button.setToolTipText(tip);
        button.addActionListener(this);
        button.setFont(MipavUtil.font12);
        button.setBorderPainted(false);
        button.setFocusPainted(false);
        button.setEnabled(true);
    }
    
    private TreeNode[] getSelectedNodes(JTree t) {
    	ArrayList<TreeNode> selectedList = new ArrayList<TreeNode>();
    	TreePath[] selectedPaths = t.getSelectionModel().getSelectionPaths();
    	if(selectedPaths == null)
    		return new TreeNode[0];
    	for(int i=0; i<selectedPaths.length; i++) {
    		if(selectedPaths[i].getLastPathComponent() instanceof TreeNode) {
    			if(((TreeNode)selectedPaths[i].getLastPathComponent()).isLeaf()) {
    				selectedList.add((TreeNode)selectedPaths[i].getLastPathComponent());
    			}
    		}
    	}  
    	TreeNode[] selectedArr = new TreeNode[selectedList.size()];
    	for(int i=0; i<selectedArr.length; i++) {
    		selectedArr[i] = selectedList.get(i);
    	}
    	return selectedArr;
    }

    /**
     * convenience Method.
     *
     * @param  noImage  DOCUMENT ME!
     */
    @SuppressWarnings("unused")
    private void createAllDownButton(boolean noImage) {
        createAllTreeBButton(noImage);
    }

    /**
     * convenience Method.
     *
     * @param  noImage  DOCUMENT ME!
     */
    private void createAllLeftButton(boolean noImage) {
        createAllTreeAButton(noImage);
    }

    /**
     * creates a button with either the image of an arrow (<code>leftarrowcollection.gif</code>) or with the text "Send
     * All left" or "Send All up".
     *
     * @param  <code>  noImage DOCUMENT ME!</code> uses text. <code>false</code> displays the image.
     */
    private void createAllTreeAButton(boolean noImage) {

        if (treeLayout == BoxLayout.Y_AXIS) {

            if (noImage) {
                sendAllToTreeA = new JButton("Send all up");
            } // text Only
            else {
                sendAllToTreeA = new JButton(MipavUtil.getIcon("uparrowcollection.gif"));
            } // graphic button

            setButtonFeatures(sendAllToTreeA, ALLTREEA, "Send all up");
        } else {

            if (noImage) {
                sendAllToTreeA = new JButton("Send all left");
            } // text Only
            else {
                sendAllToTreeA = new JButton(MipavUtil.getIcon("leftarrowcollection.gif"));
            } // graphic button

            setButtonFeatures(sendAllToTreeA, ALLTREEA, "Send all left");
        }
    }

    /**
     * creates a button with either the image of an arrow (<code>rightarrowcollection.gif</code>) or with the text "Send
     * all right" or "Send all down".
     *
     * @param  <code>  noImage DOCUMENT ME!</code> uses text. <code>false</code> displays the image.
     */
    private void createAllTreeBButton(boolean noImage) {

        if (treeLayout == BoxLayout.Y_AXIS) {

            if (noImage) {
                sendAllToTreeB = new JButton("Send all down");
            } // text Only
            else {
                sendAllToTreeB = new JButton(MipavUtil.getIcon("downarrowcollection.gif"));
            } // graphic button

            setButtonFeatures(sendAllToTreeB, ALLTREEB, "Send all down");
        } else {

            if (noImage) {
                sendAllToTreeB = new JButton("Send all right");
            } // text Only
            else {
                sendAllToTreeB = new JButton(MipavUtil.getIcon("rightarrowcollection.gif"));
            } // graphic button

            setButtonFeatures(sendAllToTreeB, ALLTREEB, "Send all right");
        }
    }

    /**
     * convenience Method.
     *
     * @param  noImage  DOCUMENT ME!
     */
    private void createAllRightButton(boolean noImage) {
        createAllTreeBButton(noImage);
    }

    /**
     * convenience Method.
     *
     * @param  noImage  DOCUMENT ME!
     */
    @SuppressWarnings("unused")
    private void createAllUpButton(boolean noImage) {
        createAllTreeAButton(noImage);
    }


    /**
     * creates a button with either the image of an 'X' (<code>delete.gif</code>) or with the text "delete selection".
     * Delete selection is initially not enabled.
     *
     * @param  <code>  noImage DOCUMENT ME!</code> uses text. <code>false</code> displays the image.
     */
    private void createDeleteButton(boolean noImage) {

        if (noImage) {
            deleteSelection = new JButton("Delete selection");
        } // text Only
        else {
            deleteSelection = new JButton(MipavUtil.getIcon("delete.gif"));
        } // graphic button

        setButtonFeatures(deleteSelection, "delete", "Delete selection");
        setDeleteEnabled(false);
    }

    /**
     * convenience Method.
     *
     * @param  noImage  DOCUMENT ME!
     */
    @SuppressWarnings("unused")
    private void createDownButton(boolean noImage) {
        createTreeBButton(noImage);
    }

    /**
     * convenience Method.
     *
     * @param  noImage  DOCUMENT ME!
     */
    private void createLeftButton(boolean noImage) {
        createTreeAButton(noImage);
    }

    /**
     * creates a button with either the image of an arrow (<code>leftarrow.gif</code>) or with the text "Send left" or
     * "Send up".
     *
     * @param  <code>  noImage DOCUMENT ME!</code> uses text. <code>false</code> displays the image.
     */
    private void createTreeAButton(boolean noImage) {

        if (treeLayout == BoxLayout.Y_AXIS) {

            if (noImage) {
                sendSelectionToTreeA = new JButton("Send up");
            } // text Only
            else {
                sendSelectionToTreeA = new JButton(MipavUtil.getIcon("up.gif"));
            } // graphic button

            setButtonFeatures(sendSelectionToTreeA, TREEA, "Send selection up");
        } else {

            if (noImage) {
                sendSelectionToTreeA = new JButton("Send left");
            } // text Only
            else {
                sendSelectionToTreeA = new JButton(MipavUtil.getIcon("leftarrow.gif"));
            } // graphic button

            setButtonFeatures(sendSelectionToTreeA, TREEB, "Send selection left");
        }
    }

    /**
     * creates a button with either the image of an arrow (<code>rightarrow.gif</code>) or with the text "Send right" or
     * "Send down".
     *
     * @param  <code>  noImage DOCUMENT ME!</code> uses text. <code>false</code> displays the image.
     */
    private void createTreeBButton(boolean noImage) {

        if (treeLayout == BoxLayout.Y_AXIS) {

            if (noImage) {
                sendSelectionToTreeB = new JButton("Send down");
            } // text Only
            else {
                sendSelectionToTreeB = new JButton(MipavUtil.getIcon("down.gif"));
            } // graphic button

            setButtonFeatures(sendSelectionToTreeB, TREEB, "Send selection down");
        } else {

            if (noImage) {
                sendSelectionToTreeB = new JButton("Send right");
            } // text Only
            else {
                sendSelectionToTreeB = new JButton(MipavUtil.getIcon("rightarrow.gif"));
            } // graphic button

            setButtonFeatures(sendSelectionToTreeB, TREEB, "Send selection right");
        }
    }

    /**
     * convenience Method.
     *
     * @param  noImage  DOCUMENT ME!
     */
    private void createRightButton(boolean noImage) {
        createTreeBButton(noImage);
    }

    /**
     * convenience Method.
     *
     * @param  noImage  DOCUMENT ME!
     */
    @SuppressWarnings("unused")
    private void createUpButton(boolean noImage) {
        createTreeAButton(noImage);
    }
}
