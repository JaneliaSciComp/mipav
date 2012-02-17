/*
 * Reads files indicated in the JPanelFileSelection panels,
 * then displays the list-contents of each in the appropriate
 * ListController panels.  the buttons will let you run the process this does
 * either by saving the list as selectB, then running, running the B list, but
 * not saving it, or cancel.
 */

package gov.nih.mipav.view.dialogs;


import gov.nih.mipav.view.*;

import java.awt.*;
import java.awt.event.*;

import java.beans.*;

import java.util.*;

import javax.swing.*;
import javax.swing.border.*;
// for PropertyChange handling


/**
 * A basic dialog to read and parse the two options files, display the options in the files, and allow file action
 * between the two. One or both of the files can be displayed, or hide one selector to prevent it from being changed.
 *
 * @author  parsonsd
 */
public class JDialogListSaveSelection extends JDialogBase implements ActionListener, PropertyChangeListener {
    // actual things we can see...

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** Use serialVersionUID for interoperability. */
    private static final long serialVersionUID = -5740263457469223226L;

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /**
     * Indicates the source file for list A. The Default property name given to this file selector is
     * &quot;SelectFileA&quot;
     */
    protected JPanelFileSelection selectFileA;

    /**
     * Indicates the source file for list B. The Default property name given to this file selector is
     * &quot;SelectFileB&quot;
     */
    protected JPanelFileSelection selectFileB;

    /** Allows the user to modify listB based on either the file provided in selectFileA or selectFileB. */
    protected JDialogListSaveSelection.JPanelListSelection selector;

    /** List of selected VOIs. */
    private JList selectedList = new JList();

    /** List of available VOIs. */
    private JList sourceList = new JList();

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Creates a non-modal dialog, initializes the layout (@see initializeLayout) and registers the
     * PropertyChangeListeners to connect the lists with the file structure. The lists are left to be populated by the
     * child class calling <code>populateLists()</code>.
     */
    public JDialogListSaveSelection() {
        super(true);
        initializeLayout();
        registerChanges();
    }

    /**
     * Creates a dialog with the given modality, and will initialize the layout and register changes, but it will not
     * populate the lists.
     *
     * @param  modal  Creates a dialog which is modal if <code>true</code>, and non-modal if <code>false</CODE>.
     */
    public JDialogListSaveSelection(boolean modal) {
        super(modal);
        initializeLayout();
        registerChanges();
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * Invoked when an action occurs. Responds to the action command &quot;ok&quot; and the action command
     * &quot;cancel&quot;. When the action command &quot;ok&quot; occurs method calls <code>okAction</code>. Dismisses
     * dialog when the action command &quot;Cancel&quot; occurs.
     *
     * @param  e  The action event for the dialog.
     */
    public void actionPerformed(ActionEvent e) {

        if (e.getActionCommand().equalsIgnoreCase("ok")) {
            okAction();
        } else if (e.getActionCommand().equalsIgnoreCase("Cancel")) {
            cancelAction();
        }
    }

    /**
     * This method gets called when a bound property is changed. Responds to changes in the property
     * &quot;SelectFileA&quot; and &quot;SelectFileB&quot;, by calling the appropriatly named method; namely, <code>
     * propertyChangeInSelectA</code> and <code>propertyChangeInSelectB</code>.
     *
     * @param  evt  A PropertyChangeEvent object describing the event source and the property that has changed.
     *
     * @see    #propertyChangeInSelectA
     * @see    #propertyChangeInSelectB
     */
    public void propertyChange(PropertyChangeEvent evt) {

        try {

            if (evt.getPropertyName().equalsIgnoreCase("SelectFileA")) {
                propertyChangeInSelectA();
            }
        } catch (NullPointerException npe) { }

        try { //

            if (evt.getPropertyName().equalsIgnoreCase("SelectFileB")) {
                propertyChangeInSelectB();
            }
        } catch (NullPointerException npe) { }
    }

    /**
     * Creates the source panel which consists of the directory line, the browse button, and a check box approving the
     * anonymize in sub-directories.
     *
     * @param   sourceList  DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    protected JPanel buildSourcePanel() {
        JPanel srcp = new JPanel(new GridLayout(1, 2));
        srcp.setBorder(new TitledBorder(new EtchedBorder(), "Source List", TitledBorder.LEFT, TitledBorder.CENTER,
                                        MipavUtil.font12B, Color.black));

        srcp.add(buildSourceListingPanel(), BorderLayout.CENTER); // list of VOIs in the image.
        srcp.add(buildSelectedListing(), BorderLayout.EAST); // list of selected items

        return srcp;
    }

    /**
     * Calls <code>setVisible(false)</code> to close the dialog. When an cancel action occurs, it calls this method,
     * which is included for subclasses to cause independant actions, without needing to over-ride the action handler.
     */
    protected void cancelAction() {
        setVisible(false);
    }

    /**
     * Creates a display of the buttons (eg, OK, CANCEL, HELP) for the dialog. This method currently returns the panel
     * generated by <code>JDialogBase.buildButtons()</code>.
     *
     * <p>Override this method to create custom designs.</p>
     *
     * @see     JDialogBase#buildButtons()
     *
     * @return  The panel of buttons for the dialog, as in JDialogBase.buildButtons()
     */
    protected JComponent createButtonComponent() {
        return buildButtons();
    }

    /**
     * Creates the basic layout of the file selectors to be used in the dialog. Here, both file selectors
     * (<code>JPanelFileSelection</code>) are created and sit on top of each other. Although this is where the file
     * selector is created, the property name is added in <code>registerChanges</code>.
     *
     * <p>Override this method to create custom designs (eg, assume a particular file and leave out the selector).</p>
     *
     * @see     JPanelFileSelection
     * @see     #initializeLayout
     * @see     #registerChanges
     *
     * @return  JComponent which contains the file selectors so that they may be displayed.
     */
    protected JComponent createFileSelectorComponent() {
        Box fileSelectors = new Box(BoxLayout.Y_AXIS);
        selectFileA = new JPanelFileSelection();
        fileSelectors.add(selectFileA);

        selectFileB = new JPanelFileSelection();
        fileSelectors.add(selectFileB);

        return fileSelectors;
    }

    /**
     * Creates the List Controller, and its associated lists, links them, and builds a horizontal display for all three
     * using the default <code>JPanelListController.createListControllerDisplay(JPanelListController)</code> method.
     *
     * <p>Override this method to create custom designs.</p>
     *
     * @see     JPanelListController
     * @see     JPanelListController.createListControllerDisplay
     * @see     #initializeLayout
     *
     * @return  JComponent which contains a displayable JPanelListController.
     */
    protected JComponent createListComponent() {

        // create the logical list
        selector = new JPanelListSelection();

        if (this instanceof JDialogDicom2XMLSelection) {
            selector.buildDicomSortOptions();
        }

        getSourceList().setListData(new Vector<Object>());
        selector.setLeftList(getSourceList());

        JList selectedList = new JList();
        selectedList.setListData(new Vector<Object>());
        selector.setRightList(getSelectedList());

        return JPanelListController.createListControllerDisplay(selector);
    }

    /**
     * Accessor to the list used to display the selected list.
     *
     * @return  DOCUMENT ME!
     */
    protected JList getSelectedList() {
        return selectedList;
    }

    /**
     * Accessor to the list used to display the Source.
     *
     * @return  DOCUMENT ME!
     */
    protected JList getSourceList() {
        return sourceList;
    }

    /**
     * Creates the basic layout and adds it to the content pane. The layout of the content pane here places a panel for
     * selectors at the top, the JPanelListController is below both, with the buttons at the bottom.
     *
     * @see  #createFileSelectorComponent()
     * @see  #createListComponent()
     * @see  #createButtonComponent()
     */
    protected void initializeLayout() {
        getContentPane().setLayout(new BorderLayout());

        getContentPane().add(createFileSelectorComponent(), BorderLayout.NORTH);
        getContentPane().add(createListComponent(), BorderLayout.CENTER);
        getContentPane().add(createButtonComponent(), BorderLayout.SOUTH);
    }

    /**
     * When an OK action occurs, it calls this method, which is included for subclasses to cause independant actions,
     * without needing to over-ride the action handler.
     */
    protected void okAction() { }

    /**
     * permits control over what items go in which lists. Use getSelectedList() and getSourceList() to apply these
     * changes. Add items to preset the lists, and use the list controller and the property change listeners to control
     * list items afterward.
     */
    protected void populateLists() { }

    /**
     * Here to allow subclasses to select the action to occur when a property change occurs in the select A panel.
     */
    protected void propertyChangeInSelectA() {
        //System.out.println("SELECT FILE A");
    }

    /**
     * Here to allow subclasses to select the action to occur when a property change occurs in the select B panel.
     */
    protected void propertyChangeInSelectB() {
        //System.out.println("SELECT FILE B");
    }

    /**
     * Connects the file selection of each file selection panel to the appropriate list and ensures that it has the
     * correct property name. if a list doesn't exist (if a subclass calls this method, when it didn't get created), the
     * action will be ignored.
     *
     * @see  #selectFileA
     * @see  #selectFileB
     */
    protected void registerChanges() {

        try {
            selectFileA.setPropertyName("SelectFileA");
            selectFileA.addPropertyChangeListener(this);
        } catch (NullPointerException npe) { }

        try {
            selectFileB.setPropertyName("SelectFileB");
            selectFileB.addPropertyChangeListener(this);
        } catch (NullPointerException npe) { }
    }

    /**
     * Method to save the items in List B. This is called by the OKAY button, so it may be implementation dependant.
     */
    protected void saveListB() { }

    /**
     * creates the visual display in which to list all selected directories in the directory tree. The panel is 240
     * pixels wide though that is <i>supposed</i> to be the minimum size
     *
     * @return  the panel which is to hold the list of selected items
     */
    private JPanel buildSelectedListing() {

        // define an outside panel to hold all these components.
        JPanel selp = new JPanel(new BorderLayout());
        selp.add(Box.createHorizontalStrut(240), BorderLayout.NORTH); // width of text area.  seems to start out very
                                                                      // skinny.

        // this list to hold things so that they may be selectable/removable
        // panel to hold list access.
        selectedList.setListData(new Vector<Object>()); // = new JList();
        selp.add(new JScrollPane(selectedList), BorderLayout.CENTER);

        // build default arrowpanel
        if (selector == null) {
            selector = new JPanelListSelection();

            selector.setLeftList(sourceList);
            selector.setRightList(selectedList);

            sourceList.addListSelectionListener(selector);
            selectedList.addListSelectionListener(selector);

            selp.add(selector, BorderLayout.WEST);
        }

        return selp;
    }

    /**
     * Creates the panel holding the directory tree.
     *
     * @param   sourceVector  DOCUMENT ME!
     *
     * @return  Panel.
     */
    private JPanel buildSourceListingPanel() {
        JPanel srctreep = new JPanel(new BorderLayout());
        Vector<Object> volumesVector = new Vector<Object>();
        sourceList.setListData(volumesVector);
        sourceList.setSelectionMode(ListSelectionModel.MULTIPLE_INTERVAL_SELECTION);

        JScrollPane jsp = new JScrollPane(sourceList);
        srctreep.add(jsp, BorderLayout.CENTER);

        return srctreep;
    }

    //~ Inner Classes --------------------------------------------------------------------------------------------------

    /**
     * A type of JPanelListController which spefically defines the left-direction buttons to be disabled and the delete
     * button to be enabled. It defines the action of the right-directed button to remove repeated elements from the
     * right list; the delete button deletes selected items from the right list. It then calls the super's <CODE>
     * actionPerformed</CODE>.
     */
    public class JPanelListSelection extends JPanelListController {

        /** Use serialVersionUID for interoperability. */
        private static final long serialVersionUID = -4660560256657187754L;

        /** DOCUMENT ME! */
        TagComparator comp = null;

        /**
         * Creates a new JPanelListSelection object.
         */
        JPanelListSelection() {
            this(BoxLayout.Y_AXIS, false);
        }

        /**
         * I don't want to bother with any other layouts right now. JPanelListSelection(int layout) { this(layout,
         * false); }
         *
         * @param  noImages  DOCUMENT ME!
         */
        JPanelListSelection(boolean noImages) {
            this(BoxLayout.Y_AXIS, noImages);
        }

        /**
         * Creates a new JPanelListSelection object.
         *
         * @param  layout    DOCUMENT ME!
         * @param  noImages  DOCUMENT ME!
         */
        JPanelListSelection(int layout, boolean noImages) {
            super(layout, noImages);
            setBackArrowEnabled(false);
            setDeleteEnabled(true);
        }

        /**
         * Performs a directed action of the delete button or listB buttons. If the delete button is struck, it removes
         * the selected items from the right list, and if right list button is called, the repeated elements are to be
         * removed. No action occurs if either left or right list is <code>null</code>, but rather returns uneventfully.
         *
         * @param  ae  The Action Event.
         */
        public void actionPerformed(ActionEvent ae) {

            // the buttons won't work getting lists if either are null.
            if ((getRightList() == null) || (getLeftList() == null)) {
                return;
            }

            String command = ae.getActionCommand();

            if (command.equalsIgnoreCase("delete")) {
                deleteFrom(getRightList());
            }

            // prevent duplicate selections in listB:
            else if (command.equalsIgnoreCase("listB")) {
                removeRepeatedElements(getRightList());
            }
            // Sort the JList by either Tag or Name (if already on tag or name sort, will do reverse sort)
            else if (command.equalsIgnoreCase("sortTag") || command.equalsIgnoreCase("sortName")) {

                if (comp != null) {

                    if (command.equalsIgnoreCase("sortTag")) {

                        switch (comp.getCompareType()) {

                            case TagComparator.COMPARE_TAG:
                                comp.setCompareType(TagComparator.COMPARE_TAG_REV);
                                break;

                            default:
                                comp.setCompareType(TagComparator.COMPARE_TAG);
                        }
                    } else {

                        switch (comp.getCompareType()) {

                            case TagComparator.COMPARE_NAME:
                                comp.setCompareType(TagComparator.COMPARE_NAME_REV);
                                break;

                            default:
                                comp.setCompareType(TagComparator.COMPARE_NAME);
                        }
                    }

                    Vector<Object> vec = new Vector<Object>();

                    for (int i = 0; i < getSourceList().getModel().getSize(); i++) {
                        vec.add(getSourceList().getModel().getElementAt(i));
                    }

                    Collections.sort(vec, comp);
                    getSourceList().setListData(vec);

                    Vector<Object> vec2 = new Vector<Object>();

                    for (int i = 0; i < getSelectedList().getModel().getSize(); i++) {
                        vec2.add(getSelectedList().getModel().getElementAt(i));
                    }

                    Collections.sort(vec2, comp);
                    getSelectedList().setListData(vec2);
                }
            }

            // check on other actions.
            super.actionPerformed(ae);
        }

        /**
         * Method for building DICOM tag/name sorting options.
         */
        public void buildDicomSortOptions() {
            comp = new TagComparator(TagComparator.COMPARE_TAG);

            JButton sortTag = new JButton(MipavUtil.getIcon("tagsort.gif"));
            sortTag.setRolloverIcon(MipavUtil.getIcon("tagsortroll.gif"));
            setButtonFeatures(sortTag, "sortTag", "Sort list by tag");

            JButton sortName = new JButton(MipavUtil.getIcon("namesort.gif"));
            sortName.setRolloverIcon(MipavUtil.getIcon("namesortroll.gif"));
            setButtonFeatures(sortName, "sortName", "Sort list by name");

            add(sortTag);
            add(sortName);
        }
    }

    /**
     * DOCUMENT ME!
     */
    public class TagComparator implements Comparator<Object> {

        /** DOCUMENT ME! */
        public static final int COMPARE_TAG = 0;

        /** DOCUMENT ME! */
        public static final int COMPARE_TAG_REV = 1;

        /** DOCUMENT ME! */
        public static final int COMPARE_NAME = 2;

        /** DOCUMENT ME! */
        public static final int COMPARE_NAME_REV = 3;

        /** DOCUMENT ME! */
        private int compareType = COMPARE_TAG;

        /**
         * Creates a new TagComparator object.
         *
         * @param  type  DOCUMENT ME!
         */
        public TagComparator(int type) {
            this.compareType = type;
        }

        /**
         * DOCUMENT ME!
         *
         * @param   a  DOCUMENT ME!
         * @param   b  DOCUMENT ME!
         *
         * @return  DOCUMENT ME!
         */
        public int compare(Object a, Object b) {

            if (compareType == COMPARE_TAG) {
                return String.CASE_INSENSITIVE_ORDER.compare(((JDialogDicom2XMLSelection.DicomTagIdentifier) a).getKey().toString(),
                                                             ((JDialogDicom2XMLSelection.DicomTagIdentifier) b).getKey().toString());

            } else if (compareType == COMPARE_TAG_REV) {
                return String.CASE_INSENSITIVE_ORDER.compare(((JDialogDicom2XMLSelection.DicomTagIdentifier) b).getKey().toString(),
                                                             ((JDialogDicom2XMLSelection.DicomTagIdentifier) a).getKey().toString());
            } else if (compareType == COMPARE_NAME) {
                return String.CASE_INSENSITIVE_ORDER.compare(((JDialogDicom2XMLSelection.DicomTagIdentifier) a).getTag().getName(),
                                                             ((JDialogDicom2XMLSelection.DicomTagIdentifier) b).getTag().getName());
            } else {
                return String.CASE_INSENSITIVE_ORDER.compare(((JDialogDicom2XMLSelection.DicomTagIdentifier) b).getTag().getName(),
                                                             ((JDialogDicom2XMLSelection.DicomTagIdentifier) a).getTag().getName());
            }
        }

        /**
         * DOCUMENT ME!
         *
         * @return  DOCUMENT ME!
         */
        public int getCompareType() {
            return this.compareType;
        }

        /**
         * DOCUMENT ME!
         *
         * @param  type  DOCUMENT ME!
         */
        public void setCompareType(int type) {
            this.compareType = type;
        }
    }


}
