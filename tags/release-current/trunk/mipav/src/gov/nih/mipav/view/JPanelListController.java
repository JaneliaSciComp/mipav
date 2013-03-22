package gov.nih.mipav.view;


/* panel to hold and transfer items between two lists.
 * extend to modify the application of the 'left', 'right'
 * and delete.  delete has not been implemented.
 * parsonsd
 */

import java.awt.*;
import java.awt.event.*;

import java.util.Vector;

import javax.swing.*;
import javax.swing.event.*;


/**
 * panel to hold and transfer items between two lists. extend to modify the application of the 'left', 'right', and
 * 'delete' or 'up', 'down' and 'delete'. Use of delete has not been implemented, and left un-enabled; extend this panel
 * and extend the ActionEvent handler to define its use.
 *
 * <p>Creating an instance of this <code>JPanel</code> does not in fact create any visual representation of either list.
 * It does display the list-control buttons, but the <code>JList</code>s held (<code>ListA</code> and <code>
 * ListB</code>) are references. This is done so that the display may have as much freedom for display as possible (ie,
 * control spacing, control alignment and position, or insert explanation text). As such, the display panel which
 * contains the lists is created seperatly.</p>
 *
 * @author   david parsons
 * @version  1
 *
 *           <p>$Logfile: /mipav/src/gov/nih/mipav/view/JPanelListController.java $</p>
 */
public class JPanelListController extends JPanel implements ActionListener, ListSelectionListener {
    // left and right are very subjective terms here.  user should be careful to consider visual location when coding.

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** Use serialVersionUID for interoperability. */
    private static final long serialVersionUID = -3060198445145485002L;

    //~ Instance fields ------------------------------------------------------------------------------------------------

    // references to where we point the directional arrows.
    // should be assigned when this has been added to the
    // component to which it listens....

    /** upper or left-most list. */
    protected JList listA;

    /** lower or right-most list. */
    protected JList listB;

    /** 'X'. */
    private JButton deleteSelection;

    /** DOCUMENT ME! */
    private int listLayout;

    /** DOCUMENT ME! */
    private JButton sendAllToListA;

    /** DOCUMENT ME! */
    private JButton sendAllToListB;

    /** left arrow or up arrow. */
    private JButton sendSelectionToListA;

    /** right arrow or down arrow. */
    private JButton sendSelectionToListB;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * defualt position is all buttons vertical. Images are by default shown on the buttons. Both arrows are enabled,
     * but Delete is disabled by default. panel is layed out as a BoxLayout.Y_AXIS
     *
     * @see  javax.swing.BoxLayout
     */
    public JPanelListController() {
        this(BoxLayout.Y_AXIS);
        /* add buttons.  assume graphic look
         *   ->  (send right)  <-  (send left)   X  (remove)
         */
    }

    /**
     * Creates a list controller, with the buttons in the given layout, but which defaults to display images only. The
     * buttons point as if to lists in the orthogonal direction. Choose layout with: <code>BoxLayout.X_AXIS</code>, or
     * <code>BoxLayout.Y_AXIS</code>. Both arrows are enabled, but Delete is disabled by default.
     *
     * @see    javax.swing.BoxLayout
     *
     * @param  layout  The <CODE>BoxLayout</CODE> designation for the layout of the buttons in the panel. Use <CODE>
     *                 BoxLayout.X_AXIS</CODE> or <CODE>BoxLayout.Y_AXIS</CODE>.
     */
    public JPanelListController(int layout) {
        this(layout, false);
    }

    /**
     * Creates a list controller, with the buttons in the default layout, but which can be set to display images only or
     * text. The buttons point as if to lists in the orthogonal direction. Both arrows are enabled, but Delete is
     * disabled by default. Panel is layed out as a <code>BoxLayout.Y_AXIS</code>, and points to lists that would be
     * laid-out along the x-axis of the display panel.
     *
     * @see    javax.swing.BoxLayout
     *
     * @param  noImages  Specifies if the buttons are to be displayed as images or with no images, using text instead.
     */
    public JPanelListController(boolean noImages) {
        this(BoxLayout.Y_AXIS, noImages);
    }

    /**
     * Creates a list controller, with the buttons in the given layout, and which can be set to display images only or
     * text. The buttons point as if to lists in the orthogonal direction. Both arrows are enabled, but Delete is
     * disabled by default. Choose layout of the buttons with: <code>BoxLayout.X_AXIS</code>, or <code>
     * BoxLayout.Y_AXIS</code>.
     *
     * @see  javax.swing.BoxLayout
     */
    public JPanelListController(int layout, boolean noImages) {
        super();
        super.setLayout(new BoxLayout(this, layout));

        // the lists are layed out exactly opposite of the panel-layout.
        listLayout = (layout == BoxLayout.X_AXIS) ? BoxLayout.Y_AXIS : BoxLayout.X_AXIS;

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
        add(sendAllToListA);
        add(sendSelectionToListA);
        add(deleteSelection);
        add(sendSelectionToListB);
        add(sendAllToListB);
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * Creates a simple List controller display. one with the the given controls and with the orientation defined by the
     * control panel. The lists in the control panel are displayed, but if either are <code>null</code>, an new, empty
     * list is provided.
     *
     * <p>The referenced lists are placed into <code>JScrollPane</code>s, and placed on either side of the control's
     * buttons, using a BorderLayout.</p>
     *
     * @param   controls  A JPanelListController to place into a visual display.
     *
     * @return  A JSplitPane containing the list controller, formatted as the JPanelListController indicates (ie.,
     *          vertical or horizontal layout).
     */
    public static JSplitPane createListControllerDisplay(JPanelListController controls) {
        JSplitPane fullSelector;

        // add logical lists to the visual representation
        JPanel selectorVisual = new JPanel(new BorderLayout());

        // lists are vertical:
        if (controls.getListLayout() == BoxLayout.Y_AXIS) {

            if (controls.getUpperList() == null) {
                controls.setUpperList(new JList());
            }

            if (controls.getLowerList() == null) {
                controls.setLowerList(new JList());
            }

            selectorVisual.add(new JScrollPane(controls.getUpperList()), BorderLayout.CENTER);
            selectorVisual.add(controls, BorderLayout.SOUTH);
            fullSelector = new JSplitPane(JSplitPane.VERTICAL_SPLIT, true, selectorVisual,
                                          new JScrollPane(controls.getLowerList()));
        } else { // lists are along the horizontal:

            if (controls.getLeftList() == null) {
                controls.setLeftList(new JList());
            }

            if (controls.getRightList() == null) {
                controls.setRightList(new JList());
            }

            selectorVisual.add(new JScrollPane(controls.getLeftList()), BorderLayout.CENTER);
            selectorVisual.add(controls, BorderLayout.EAST);
            fullSelector = new JSplitPane(JSplitPane.HORIZONTAL_SPLIT, true, selectorVisual,
                                          new JScrollPane(controls.getLowerList()));
        }

        return fullSelector;
    }

    /**
     * responds to the button presses.
     *
     * <P>functionality of arrow/send-to- arrows is virtually identical. the 'delete' is not supported, and this class
     * must be extended to assume functionality for that button. There are good reasons for this: there are two panels,
     * and only one delete button, and delete functionality may be use-dependant. Indeed, for a panel such as a
     * file-list, we may not <i>want</i> to support a 'delete' function. Hence, this aspect of the use, is left up to an
     * extending class.</P>
     *
     * @param  ae  The action to listen for.
     */
    public void actionPerformed(ActionEvent ae) {
        String command = ae.getActionCommand();

        // process the button performed.

        // the buttons won't work getting lists if either are null.
        if ((listB == null) || (listA == null)) {
            return;
        }

        // user sending from the right panel to the left panel.
        if (command.equals("listA")) {
            copySelected(listB, listA);
        }

        // specific command to transfer the entire list from B to A
        if (command.equals("allListA")) {
            listA.setModel(listB.getModel());
        }

        // user sending from the left panel to the right panel.
        if (command.equals("listB")) {
            copySelected(listA, listB);
        }

        // specific command to transfer the entire list from A to B
        if (command.equals("allListB")) {
            listB.setModel(listA.getModel());
        }

    }

    /**
     * Convience method to get list A.
     *
     * @see  #setListA
     */
    public JList getLeftList() {
        return getListA();
    }

    /**
     * there is no programmatic way to determine which arrow is pointing where. To compensate, the parent panel must
     * notify the panel who it is listening to. Since it can, at most, point to one list, only the most recent list to
     * be submitted will be retainted.
     *
     * <p>This method gets the panel pointing to list A. (upper or left most)</p>
     *
     * @return  DOCUMENT ME!
     */
    public JList getListA() {
        return listA;
    }

    /**
     * there is no programmatic way to determine which arrow is pointing where. To compensate, the parent panel must
     * notify the panel who it is listening to. Since it can, at most, point to one list, only the most recent list to
     * be submitted will be retainted.
     *
     * <p>This method gets the panel pointing to the list B (lower or right most)</p>
     *
     * @return  <CODE>ListB</CODE> is returned.
     */
    public JList getListB() {
        return listB;
    }

    /**
     * Convience method to get list B.
     *
     * @see     #setLowerList
     * @see     #getListB
     *
     * @return  <CODE>ListB</CODE> is returned.
     */
    public JList getLowerList() {
        return getListB();
    }

    /**
     * Convience method to get list B.
     *
     * @see     #setRightList
     * @see     #getListB
     *
     * @return  <CODE>ListB</CODE> is returned.
     */
    public JList getRightList() {
        return getListB();
    }

    /**
     * Convience method to get list A.
     *
     * @see     #setListA
     *
     * @return  <CODE>ListA</CODE> is returned.
     */
    public JList getUpperList() {
        return getListA();
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
     * Programmatically performs a 'Send to list a' button click, based on however the implemented class performs it.
     * The 'send to list a' button must be enabled to perform a programmatic action.
     */
    public void performSendToListA() {

        if (sendSelectionToListA.isEnabled()) {
            sendSelectionToListA.doClick();
        }
    }

    /**
     * Programmatically performs a 'Send to list b' button click, based on however the implemented class performs it.
     * The 'send to list b' button must be enabled to perform a programmatic action.
     */
    public void performSendToListB() {

        if (sendSelectionToListB.isEnabled()) {
            sendSelectionToListB.doClick();
        }
    }

    /**
     * A convenience method to set the to-list-A buttons (both selected and all) to the given enabled status.
     *
     * @param  b  The enabled status of the buttons.
     */
    public void setBackArrowEnabled(boolean b) {
        sendSelectionToListA.setEnabled(b);
        sendAllToListA.setEnabled(b);
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
     * A convenience method to set the to-list-B buttons (both selected and all) to the given enabled status.
     *
     * @param  b  The enabled status of the buttons.
     */
    public void setForwardArrowEnabled(boolean b) {
        sendSelectionToListB.setEnabled(b);
        sendAllToListB.setEnabled(b);
    }
    
    /**
     * A convenience method to set the to-list-A buttons (both selected and all) to the given visible status.
     *
     * @param  v  Whether to make buttons visible
     */
    public void setBackArrowVisble(boolean v) {
        sendSelectionToListA.setVisible(v);
        sendAllToListA.setVisible(v);
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
     * A convenience method to set the to-list-B buttons (both selected and all) to the given visible status.
     *
     * @param  v  Whether to make buttons visible
     */
    public void setForwardArrowVisible(boolean v) {
        sendSelectionToListB.setVisible(v);
        sendAllToListB.setVisible(v);
    }

    /**
     * Convience method to set list A.
     *
     * @see  #setListA
     */
    public void setLeftList(JList list) {
        setListA(list);
    }

    /**
     * there is no programmatic way to determine which arrow is pointing where. To compensate, the parent panel must
     * notify the panel who it is listening to. Since it can, at most, point to one list, only the most recent list to
     * be submitted will be retainted.
     *
     * <p>This method makes the pointing to the list A (the upper or left most)</p>
     *
     * @param  list  DOCUMENT ME!
     */
    public void setListA(JList list) {
        listA = list;
    }

    /**
     * there is no programmatic way to determine which arrow is pointing where. To compensate, the parent panel must
     * notify the panel who it is listening to. Since it can, at most, point to one list, only the most recent list to
     * be submitted will be retainted.
     *
     * <p>This method makes the pointing to the list B (the lower, or right most)</p>
     *
     * @param  list  DOCUMENT ME!
     */
    public void setListB(JList list) {
        listB = list;
    }

    /**
     * Convience method to set list B.
     *
     * @see  #setListB
     */
    public void setLowerList(JList list) {
        setListB(list);
    }

    /**
     * Convience method to set list B.
     *
     * @see  #setListB
     */
    public void setRightList(JList list) {
        setListB(list);
    }

    /**
     * Convience method to set list A.
     *
     * @see  #setListA
     */
    public void setUpperList(JList list) {
        setListA(list);
    }

    /**
     * handles list change events made when selecting from either left or right list. <B>This method has not been
     * implemented</b>
     *
     * <p>If the event isn't from a JList, an error messge is generated, and control is returned.</p>
     *
     * <p>Otherwise, the list is copied to the appropriate storage <code>Vector</code>s to hold the list.</p>
     *
     * @param  lse  The selection event to listen for.
     */
    public void valueChanged(ListSelectionEvent lse) {
        JList list;

        // only work if the event is type of JList
        try {
            list = (JList) (lse.getSource());
        } catch (ClassCastException cce) {
            //System.out.println("not a JList.");

            return;
        }

        // ignore if the user is still selecting
        // (as is true with a 'singleclick' on the mouse)
        if (lse.getValueIsAdjusting()) {
            return;
        }

        if (list == listA) {
            // go through all the list and if it was selected,
            // add it to the list of elements to be held onto,
            // in case the send button is used.
            /*for (int i=0; i < list.getModel().getSize(); i++) {
             *  if (list.isSelectedIndex(i)) {     //leftSelections.add(list.getModel().getElementAt(i));
             * System.out.println(list.getModel().getElementAt(i)); }}*/
        } else if (list == listB) {
            // go through all the list and if it was selected,
            // add it to the list of elements to be held onto,
            // in case the send button is used.
            /*for (int i=0; i < list.getModel().getSize(); i++) {
             *  if (list.isSelectedIndex(i)) {     //rightSelections.add(list.getModel().getElementAt(i)); } }
             */
        } else { /* nothing here to do */
        }
        // in particular, we are doing nothing with 'DELETE'
    }

    /**
     * Copies the selected items in list A and appends them to list B. The selected items in list A are then
     * de-selected.
     *
     * @param  a  the source list.
     * @param  b  the destination list.
     */
    protected void copySelected(JList a, JList b) {
        int i;
        Vector<Object> totalList = new Vector<Object>();

        // ignore if there are no selections made.
        if (a.isSelectionEmpty()) {
            return;
        }

        // First, copy the old list data in to a list.
        for (i = 0; i < b.getModel().getSize(); i++) {
            totalList.add(b.getModel().getElementAt(i));
        }

        // Next, copy in the selected values at the bottom.
        Object[] selectedValues = a.getSelectedValues();

        for (i = 0; i < selectedValues.length; i++) {
            totalList.add(selectedValues[i]);
        }

        // Lastly, copy the total list into the list to view and clean up.
        b.setListData(totalList);
        a.clearSelection();
    }

    /**
     * Removes all selected items in the given list.
     *
     * @param  list  The list of items.
     */
    protected void deleteFrom(JList list) {

        // ignore if there are no selections made.
        if (list == null) {
            return;
        }

        if (!list.isSelectionEmpty()) {

            // ge the index values of the list that are to be deleted.
            int[] selectedIndices = list.getSelectedIndices();

            // start with the first of these selected deletions
            int index = 0;

            // look through the list of items,
            // and collect all those that are
            // not selected.  Replace the old
            // list with the new list of
            // previously unselected items.
            Vector<Object> unselected = new Vector<Object>();

            for (int i = 0; i < list.getModel().getSize(); i++) {

                if (index < selectedIndices.length) {

                    if (i != selectedIndices[index]) {
                        unselected.add(list.getModel().getElementAt(i));
                    } else {
                        index++;
                    }
                } else {
                    unselected.add(list.getModel().getElementAt(i));
                }
            }

            list.setListData(unselected);
        }
    }

    /**
     * Returns the layout of the associated display panel or &quot;how the arrows point&quot;, <em>not</em> the layout
     * of the buttons themselves.
     *
     * @return  Provides the <CODE>BoxLayout</CODE> code for the intended layout of the associated display panel. Either
     *          <CODE>BoxLayout.X_AXIS</CODE> or <CODE>BoxLayout.Y_AXIS</CODE>.
     */
    protected int getListLayout() {
        return listLayout;
    }

    /**
     * Removes all repeated elements in the given list. All selections are lost after the method returns, however, as
     * the list's model has been changed.
     *
     * @param  list  The list which is to have the duplicate items removed.
     */
    protected void removeRepeatedElements(JList list) {
        Vector<Object> unrepeated = new Vector<Object>(); // holds the list of selected elements
        boolean repeatFound = false; // if routine found repeated element

        if ((list == null) || (list.getModel().getSize() == 0)) {
            return;
        }

        // check the elements in list in reverse order, from bottom to top,
        // to preserve order, albiet as a reverse-ordered list.
        for (int i = list.getModel().getSize() - 1; i > 0; i--) {
            repeatFound = false;

            for (int j = i - 1; j >= 0; j--) {

                if (list.getModel().getElementAt(j).equals(list.getModel().getElementAt(i))) {
                    repeatFound = true;
                }
            }

            if (!repeatFound) {
                unrepeated.add(list.getModel().getElementAt(i));
            }
        }

        unrepeated.add(list.getModel().getElementAt(0));

        // copy the list back, in restoring the original order for user's sake:
        int listSize = unrepeated.size();
        Object[] unr = new Object[listSize];

        for (int i = unrepeated.size() - 1, j = 0; i >= 0; i--, j++) {
            unr[j] = unrepeated.elementAt(i);
        }

        list.setListData(unr);
    }


    /**
     * adds the tooltip, and the action label to the buttoin, as well as setting the action listener, font, borderpaint,
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

    /**
     * convenience Method.
     *
     * @param  noImage  DOCUMENT ME!
     */
    @SuppressWarnings("unused")
    private void createAllDownButton(boolean noImage) {
        createAllListBButton(noImage);
    }

    /**
     * convenience Method.
     *
     * @param  noImage  DOCUMENT ME!
     */
    private void createAllLeftButton(boolean noImage) {
        createAllListAButton(noImage);
    }

    /**
     * creates a button with either the image of an arrow (<code>leftarrowcollection.gif</code>) or with the text "Send
     * All left" or "Send All up".
     *
     * @param  <code>  noImage DOCUMENT ME!</code> uses text. <code>false</code> displays the image.
     */
    private void createAllListAButton(boolean noImage) {

        if (listLayout == BoxLayout.Y_AXIS) {

            if (noImage) {
                sendAllToListA = new JButton("Send all up");
            } // text Only
            else {
                sendAllToListA = new JButton(MipavUtil.getIcon("uparrowcollection.gif"));
            } // graphic button

            setButtonFeatures(sendAllToListA, "allListA", "Send all up");
        } else {

            if (noImage) {
                sendAllToListA = new JButton("Send all left");
            } // text Only
            else {
                sendAllToListA = new JButton(MipavUtil.getIcon("leftarrowcollection.gif"));
            } // graphic button

            setButtonFeatures(sendAllToListA, "allListA", "Send all left");
        }
    }

    /**
     * creates a button with either the image of an arrow (<code>rightarrowcollection.gif</code>) or with the text "Send
     * all right" or "Send all down".
     *
     * @param  <code>  noImage DOCUMENT ME!</code> uses text. <code>false</code> displays the image.
     */
    private void createAllListBButton(boolean noImage) {

        if (listLayout == BoxLayout.Y_AXIS) {

            if (noImage) {
                sendAllToListB = new JButton("Send all down");
            } // text Only
            else {
                sendAllToListB = new JButton(MipavUtil.getIcon("downarrowcollection.gif"));
            } // graphic button

            setButtonFeatures(sendAllToListB, "allListB", "Send all down");
        } else {

            if (noImage) {
                sendAllToListB = new JButton("Send all right");
            } // text Only
            else {
                sendAllToListB = new JButton(MipavUtil.getIcon("rightarrowcollection.gif"));
            } // graphic button

            setButtonFeatures(sendAllToListB, "allListB", "Send all right");
        }
    }

    /**
     * convenience Method.
     *
     * @param  noImage  DOCUMENT ME!
     */
    private void createAllRightButton(boolean noImage) {
        createAllListBButton(noImage);
    }

    /**
     * convenience Method.
     *
     * @param  noImage  DOCUMENT ME!
     */
    @SuppressWarnings("unused")
    private void createAllUpButton(boolean noImage) {
        createAllListAButton(noImage);
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
        createListBButton(noImage);
    }

    /**
     * convenience Method.
     *
     * @param  noImage  DOCUMENT ME!
     */
    private void createLeftButton(boolean noImage) {
        createListAButton(noImage);
    }

    /**
     * creates a button with either the image of an arrow (<code>leftarrow.gif</code>) or with the text "Send left" or
     * "Send up".
     *
     * @param  <code>  noImage DOCUMENT ME!</code> uses text. <code>false</code> displays the image.
     */
    private void createListAButton(boolean noImage) {

        if (listLayout == BoxLayout.Y_AXIS) {

            if (noImage) {
                sendSelectionToListA = new JButton("Send up");
            } // text Only
            else {
                sendSelectionToListA = new JButton(MipavUtil.getIcon("up.gif"));
            } // graphic button

            setButtonFeatures(sendSelectionToListA, "listA", "Send selection up");
        } else {

            if (noImage) {
                sendSelectionToListA = new JButton("Send left");
            } // text Only
            else {
                sendSelectionToListA = new JButton(MipavUtil.getIcon("leftarrow.gif"));
            } // graphic button

            setButtonFeatures(sendSelectionToListA, "listA", "Send selection left");
        }
    }

    /**
     * creates a button with either the image of an arrow (<code>rightarrow.gif</code>) or with the text "Send right" or
     * "Send down".
     *
     * @param  <code>  noImage DOCUMENT ME!</code> uses text. <code>false</code> displays the image.
     */
    private void createListBButton(boolean noImage) {

        if (listLayout == BoxLayout.Y_AXIS) {

            if (noImage) {
                sendSelectionToListB = new JButton("Send down");
            } // text Only
            else {
                sendSelectionToListB = new JButton(MipavUtil.getIcon("down.gif"));
            } // graphic button

            setButtonFeatures(sendSelectionToListB, "listB", "Send selection down");
        } else {

            if (noImage) {
                sendSelectionToListB = new JButton("Send right");
            } // text Only
            else {
                sendSelectionToListB = new JButton(MipavUtil.getIcon("rightarrow.gif"));
            } // graphic button

            setButtonFeatures(sendSelectionToListB, "listB", "Send selection right");
        }
    }

    /**
     * convenience Method.
     *
     * @param  noImage  DOCUMENT ME!
     */
    private void createRightButton(boolean noImage) {
        createListBButton(noImage);
    }

    /**
     * convenience Method.
     *
     * @param  noImage  DOCUMENT ME!
     */
    @SuppressWarnings("unused")
    private void createUpButton(boolean noImage) {
        createListAButton(noImage);
    }
}
