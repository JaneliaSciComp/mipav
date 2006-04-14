/*
 * Reads files indicated in the JPanelFileSelection panels,
 * then displays the list-contents of each in the appropriate
 * ListController panels.  the buttons will let you run the process this does
 * either by saving the list as selectB, then running, running the B list, but
 * not saving it, or cancel.
 */

package gov.nih.mipav.view.dialogs;

import gov.nih.mipav.view.*;
import java.util.*;
import javax.swing.*;
import javax.swing.border.*;
import java.awt.event.*;
import java.awt.*;
import java.beans.*;        // for PropertyChange handling


/** A basic dialog to read and parse the two options files, display the options
 *  in the files, and allow file action between the two.  One or both of the
 *  files can be displayed, or hide one selector to prevent it from being
 *  changed.
 *
 * @author  parsonsd
 */
public class JDialogListSaveSelection
    extends JDialogBase
    implements ActionListener, PropertyChangeListener

{
    // actual things we can see...
    /** List of available VOIs */
    private JList sourceList = new JList();
    /** List of selected VOIs */
    private JList selectedList = new JList();

    /** Indicates the source file for list A.
     *  The Default property name given to this file selector is
     *  &quot;SelectFileA&quot;
     */
    protected JPanelFileSelection selectFileA;

    /** Indicates the source file for list B.
     *  The Default property name given to this file selector is
     *  &quot;SelectFileB&quot;
     */
    protected JPanelFileSelection selectFileB;

    /** Allows the user to modify listB based on either the file provided in
     *  selectFileA or selectFileB.
     */
    protected JDialogListSaveSelection.JPanelListSelection selector;

    /** Creates a non-modal dialog, initializes the layout
     * (@see initializeLayout) and registers the PropertyChangeListeners to
     * connect the lists with the file structure.  The lists are left to be
     * populated by the child class calling <code>populateLists()</code>.
     *
     */
    public JDialogListSaveSelection() {
        super(true);
        initializeLayout();
        registerChanges();
    }

    /** Creates a dialog with the given modality, and will initialize the
     * layout and register changes, but it will not populate the lists.
     * @param modal Creates a dialog which is modal if <code>true</code>, and non-modal if
     * <code>false</CODE>.
     */
    public JDialogListSaveSelection(boolean modal) {
        super(modal);
        initializeLayout();
        registerChanges();
    }

    /** Creates the basic layout and adds it to the content pane.
     *  The layout of the content pane here places a panel for selectors at the
     *  top, the JPanelListController is below both, with the buttons at the
     *  bottom.
     *
     * @see #createFileSelectorComponent()
     * @see #createListComponent()
     * @see #createButtonComponent()
     */
    protected void initializeLayout() {
        getContentPane().setLayout(new BorderLayout());

        getContentPane().add(createFileSelectorComponent(), BorderLayout.NORTH);
        getContentPane().add(createListComponent(), BorderLayout.CENTER);
        getContentPane().add(createButtonComponent(), BorderLayout.SOUTH);
    }

    /** Creates the basic layout of the file selectors to be used in the
     * dialog.  Here, both file selectors (<code>JPanelFileSelection</code>)
     * are created and sit on top of each other.  Although this is where the
     * file selector is created, the property name is added in
     * <code>registerChanges</code>.
     * <p>
     * Override this method to create custom designs (eg, assume a
     * particular file and leave out the selector).
     *
     * @see JPanelFileSelection
     * @see #initializeLayout
     * @see #registerChanges
     *
     * @return JComponent which contains the file selectors so that they may be displayed.
     */
    protected JComponent createFileSelectorComponent() {
        Box fileSelectors = new Box(BoxLayout.Y_AXIS);
        selectFileA = new JPanelFileSelection();
        fileSelectors.add(selectFileA);

        selectFileB = new JPanelFileSelection();
        fileSelectors.add(selectFileB);

        return fileSelectors;
    }

    /** Creates the List Controller, and its associated lists, links them,
     * and builds a horizontal display for all three using the default
     * <code>JPanelListController.createListControllerDisplay(JPanelListController)</code>
     * method.
     * <p>
     * Override this method to create custom designs.
     *
     * @see JPanelListController
     * @see JPanelListController.createListControllerDisplay
     * @see #initializeLayout
     *
     * @return JComponent which contains a displayable JPanelListController.
     */
    protected JComponent createListComponent() {

        // create the logical list
        selector = new JPanelListSelection();

        if (this instanceof JDialogDicom2XMLSelection) {
          selector.buildDicomSortOptions();
        }

        getSourceList().setListData(new Vector());
        selector.setLeftList(getSourceList());
        JList selectedList = new JList();
        selectedList.setListData(new Vector());
        selector.setRightList(getSelectedList());

        return JPanelListController.createListControllerDisplay(selector);
    }

    /** Creates a display of the buttons (eg, OK, CANCEL, HELP) for the dialog.
     * This method currently returns the panel generated by
     * <code>JDialogBase.buildButtons()</code>.
     * <p>
     * Override this method to create custom designs.
     *
     * @see JDialogBase#buildButtons()
     * @return The panel of buttons for the dialog, as in JDialogBase.buildButtons()
     */
    protected JComponent createButtonComponent() {
        return buildButtons();
    }

    /** permits control over what items go in which lists.  Use
     * getSelectedList() and getSourceList() to apply these changes.
     * Add items to preset the lists, and use the list controller and the
     * property change listeners to control list items afterward.
     */
    protected void populateLists() {
    }

    /** Connects the file selection of each file selection panel to the
     *  appropriate list and ensures that it has the correct property name.
     *  if a list doesn't exist (if a subclass calls this
     *  method, when it didn't get created), the action will be ignored.
     *
     * @see #selectFileA
     * @see #selectFileB
     */
    protected void registerChanges() {
        try {
            selectFileA.setPropertyName("SelectFileA");
            selectFileA.addPropertyChangeListener(this);
        } catch (NullPointerException npe) {
        }

        try {
            selectFileB.setPropertyName("SelectFileB");
            selectFileB.addPropertyChangeListener(this);
        } catch (NullPointerException npe) {
        }
    }

    /** Invoked when an action occurs.
     *  Responds to the action command &quot;ok&quot; and the action command
     *  &quot;cancel&quot;.  When the action command &quot;ok&quot; occurs
     *  method calls <code>okAction</code>.  Dismisses dialog when
     *  the action command &quot;Cancel&quot; occurs.
     * @param e The action event for the dialog.
     */
    public void actionPerformed(ActionEvent e) {
        if (e.getActionCommand().equalsIgnoreCase("ok")) {
            okAction();
        }
        else if (e.getActionCommand().equalsIgnoreCase("Cancel")) {
            cancelAction();
        }
    }

    /** When an OK action occurs, it calls this method, which is included
     * for subclasses to cause independant actions, without needing to
     * over-ride the action handler.
     */
    protected void okAction() {
    }

    /** Calls <code>setVisible(false)</code> to close the dialog.
     * When an cancel action occurs, it calls this method, which is included
     * for subclasses to cause independant actions, without needing to
     * over-ride the action handler.
     */
    protected void cancelAction() {
        setVisible(false);
    }

    /** Method to save the items in List B.  This is called by the
     *  OKAY button, so it may be implementation dependant.
     */
    protected void saveListB() {
    }

    /** This method gets called when a bound property is changed.
     *  Responds to changes in the property &quot;SelectFileA&quot; and
     *  &quot;SelectFileB&quot;, by calling the appropriatly named method;
     *  namely, <code>propertyChangeInSelectA</code> and
     *  <code>propertyChangeInSelectB</code>.
     *
     * @param evt A PropertyChangeEvent object describing the event source
     *   	and the property that has changed.
     * @see #propertyChangeInSelectA
     * @see #propertyChangeInSelectB
     */
    public void propertyChange(PropertyChangeEvent evt) {
        try {
            if (evt.getPropertyName().equalsIgnoreCase("SelectFileA")) {
                propertyChangeInSelectA();
            }
        }
        catch (NullPointerException npe) {
        }

        try { //
            if (evt.getPropertyName().equalsIgnoreCase("SelectFileB")) {
                propertyChangeInSelectB();
            }
        }
        catch (NullPointerException npe) {
        }
    }

    /** Here to allow subclasses to select the action to occur when a
     *  property change occurs in the select A panel.
     */
    protected void propertyChangeInSelectA() {
                System.out.println("SELECT FILE A");
    }

    /** Here to allow subclasses to select the action to occur when a
     *  property change occurs in the select B panel.
     */
    protected void propertyChangeInSelectB() {
                System.out.println("SELECT FILE B");
    }

    /** A type of JPanelListController which spefically defines the left-direction
     * buttons to be disabled and the delete button to be enabled.  It defines the
     * action of the right-directed button to remove repeated elements from the right
     * list; the delete button deletes selected items from the right list.  It then
     * calls the super's <CODE>actionPerformed</CODE>.
     */
    public class JPanelListSelection
            extends JPanelListController
    {
      TagComparator comp = null;

        JPanelListSelection() {
            this(BoxLayout.Y_AXIS, false);
        }

        // I don't want to bother with any other layouts right now.
        //JPanelListSelection(int layout) {
        //    this(layout, false);
        //}

        JPanelListSelection(boolean noImages) {
            this(BoxLayout.Y_AXIS, noImages);
        }

        JPanelListSelection(int layout, boolean noImages) {
            super(layout, noImages);
            setBackArrowEnabled(false);
            setDeleteEnabled(true);
        }

        /**
         * Method for building DICOM tag/name sorting options
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

        /** Performs a directed action of the delete button or listB buttons.
         * If the delete button is struck, it removes the selected items from the right
         * list, and if right list button is called, the repeated elements are to be
         * removed.  No action occurs if either left or right list is <code>null</code>,
         * but rather returns uneventfully.
         * @param ae The Action Event.
         */
        public void actionPerformed(ActionEvent ae) {
            // the buttons won't work getting lists if either are null.
            if ( (getRightList() == null) || (getLeftList() == null)) {
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
            //Sort the JList by either Tag or Name (if already on tag or name sort, will do reverse sort)
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
                }
                else {
                  switch (comp.getCompareType()) {
                    case TagComparator.COMPARE_NAME:
                      comp.setCompareType(TagComparator.COMPARE_NAME_REV);
                      break;
                    default:
                      comp.setCompareType(TagComparator.COMPARE_NAME);
                  }
                }

                Vector vec = new Vector();
                for (int i = 0; i < getSourceList().getModel().getSize(); i++) {
                  vec.add(getSourceList().getModel().getElementAt(i));
                }
                Collections.sort(vec, comp);
                getSourceList().setListData(vec);

                Vector vec2 = new Vector();
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
    }

    /** Accessor to the list used to display the Source
     */
    protected JList getSourceList() {
        return sourceList;
    }

    /** Accessor to the list used to display the selected list.
     */
    protected JList getSelectedList() {
        return selectedList;
    }

    /** Creates the source panel which consists of the directory line, the browse
     * button, and a check box approving the anonymize in sub-directories.
     * @param sourceList
     */
  protected JPanel buildSourcePanel(Vector sourceList) {
    JPanel srcp = new JPanel(new GridLayout(1, 2));
    srcp.setBorder(new TitledBorder(new EtchedBorder(),
        "Source List", TitledBorder.LEFT, TitledBorder.CENTER,
        MipavUtil.font12B, Color.black));

    srcp.add(buildSourceListingPanel(sourceList), BorderLayout.CENTER); // list of VOIs in the image.
    srcp.add(buildSelectedListing(), BorderLayout.EAST); // list of selected items

    return srcp;
  }

  /**
   *   Creates the panel holding the directory tree.
   *   @return Panel.
   */
  private JPanel buildSourceListingPanel(Vector sourceVector) {
    JPanel srctreep = new JPanel(new BorderLayout());
    Vector volumesVector = new Vector();
    sourceList.setListData(volumesVector);
    sourceList.setSelectionMode(ListSelectionModel.MULTIPLE_INTERVAL_SELECTION);
    JScrollPane jsp = new JScrollPane(sourceList);
    srctreep.add(jsp, BorderLayout.CENTER);

    return srctreep;
  }

  /** creates the visual display in which to
   *   list all selected directories in the
   *   directory tree.  The panel is 240 pixels wide
   *   though that is <i>supposed</i> to be the minimum size
   *   @return the panel which is to hold the list of selected items
   */
  private JPanel buildSelectedListing() {
    // define an outside panel to hold all these components.
    JPanel selp = new JPanel(new BorderLayout());
    selp.add(Box.createHorizontalStrut(240), BorderLayout.NORTH); // width of text area.  seems to start out very skinny.

    // this list to hold things so that they may be selectable/removable
    // panel to hold list access.
    selectedList.setListData(new Vector()); // = new JList();
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

  public class TagComparator implements Comparator{

    public static final int COMPARE_TAG      = 0;
    public static final int COMPARE_TAG_REV  = 1;
    public static final int COMPARE_NAME     = 2;
    public static final int COMPARE_NAME_REV = 3;

    private int compareType = COMPARE_TAG;

    public TagComparator(int type) {
      this.compareType = type;
    }

    public void setCompareType(int type) {
      this.compareType = type;
    }

    public int getCompareType() {
      return this.compareType;
    }

    public int compare(Object a, Object b) {

      if (compareType == COMPARE_TAG) {
        return String.CASE_INSENSITIVE_ORDER.compare( ((JDialogDicom2XMLSelection.DicomTagIdentifier)a).getKey().toString(),
                                        ((JDialogDicom2XMLSelection.DicomTagIdentifier)b).getKey().toString());

      }
      else if (compareType == COMPARE_TAG_REV) {
        return String.CASE_INSENSITIVE_ORDER.compare( ((JDialogDicom2XMLSelection.DicomTagIdentifier)b).getKey().toString(),
                                               ((JDialogDicom2XMLSelection.DicomTagIdentifier)a).getKey().toString());
      }
      else if (compareType == COMPARE_NAME) {
        return String.CASE_INSENSITIVE_ORDER.compare( ((JDialogDicom2XMLSelection.DicomTagIdentifier)a).getTag().getName(),
                                               ((JDialogDicom2XMLSelection.DicomTagIdentifier)b).getTag().getName());
      }
      else {
        return String.CASE_INSENSITIVE_ORDER.compare( ((JDialogDicom2XMLSelection.DicomTagIdentifier)b).getTag().getName(),
                                               ((JDialogDicom2XMLSelection.DicomTagIdentifier)a).getTag().getName());
      }
    }
  }


}
