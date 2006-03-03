package gov.nih.mipav.view;

import java.io.FileNotFoundException;
import java.util.*;

import java.awt.*;
import java.awt.event.*;
import javax.swing.*;
import javax.swing.border.*;
import javax.swing.table.*;

/**
 * Window which shows the current list of registered images and image frames.
 * @version 1.0
 */
public class ViewJFrameActiveImages
    extends JFrame
    implements ActionListener {

  private JScrollPane scrollPane;
  private JTable imageTable;
  private DefaultTableModel imageTableModel;
  private ViewUserInterface UI;
  private VectorStack table;
  private Vector columnNames;
  private ImageTableCellRenderer imgTblCellRenderer;

  public ViewJFrameActiveImages(ViewUserInterface _UI) {
    super();
    UI = _UI;
    setTitle("Active Image Registry Monitor");

    try {
        setIconImage(MipavUtil.getIconImage(Preferences.getIconName()));
    } catch ( FileNotFoundException error ) {
        Preferences.debug("Exception ocurred while getting <" + error.getMessage() + ">.  Check that this file is available.\n");
        System.err.println("Exception ocurred while getting <" + error.getMessage() + ">.  Check that this file is available.\n");
    }

    this.getContentPane().setLayout(new BorderLayout());
    JPanel userPanel = new JPanel(new BorderLayout());

    // put the list together
    table = new VectorStack();
    imageTable = new JTable();
    imageTable.addMouseListener(new MouseClickAdapter());
    imageTableModel = new DefaultTableModel();

    columnNames = new Vector();
    columnNames.add("$Active");
    columnNames.add("Image Name");

    imageTableModel.setDataVector(table, columnNames);
    imageTable.setModel(imageTableModel);
    imgTblCellRenderer = new ImageTableCellRenderer();
    imageTable.getColumnModel().getColumn(1).setCellRenderer(imgTblCellRenderer);

    JPanel pan = new JPanel(new GridLayout(1, 1));
    TitledBorder border = new TitledBorder("Currently Active Images");
    border.setTitleColor(Color.black);
    border.setTitleFont(MipavUtil.font12B);
    border.setBorder(new EtchedBorder());
    pan.setBorder(border);

    scrollPane = new JScrollPane(imageTable);
    pan.add(scrollPane);

    JPanel buttonPanel = new JPanel();
    Box buttonBox = new Box(BoxLayout.X_AXIS);
    JButton upButton = new JButton(MipavUtil.getIcon("up.gif"));
    upButton.addActionListener(this);
    upButton.setRolloverIcon(MipavUtil.getIcon("uppress.gif"));
    upButton.setBorderPainted(false);
    upButton.setToolTipText("Move up");
    upButton.setActionCommand("up");

    JButton downButton = new JButton(MipavUtil.getIcon("down.gif"));
    downButton.addActionListener(this);
    downButton.setRolloverIcon(MipavUtil.getIcon("downpress.gif"));
    downButton.setToolTipText("Move down");
    downButton.setBorderPainted(false);
    downButton.setActionCommand("down");

    buttonBox.add(upButton);
    buttonBox.add(downButton);
    buttonPanel.add(buttonBox);

    userPanel.add(pan, BorderLayout.CENTER);
    this.getContentPane().add(userPanel, BorderLayout.CENTER);

    setVisible(false);
    setSize(250, 300);
    validate();

  }

  public void actionPerformed(ActionEvent e) {
    String command = e.getActionCommand();
    String selectedName = null;
    if (command.equals("up")) {
      if (!UI.isScriptRecording()) {
        int idx = imageTable.getSelectedRow();
        if (idx > -1) {
          selectedName = (String) imageTable.getValueAt(idx, 1);
        }
        if (idx > 0 && idx > -1) {
          Object curr = ( (Vector) (table.elementAt(idx))).elementAt(1);
          Object prev = ( (Vector) (table.elementAt(idx - 1))).elementAt(1);
          ( (Vector) (table.elementAt(idx - 1))).setElementAt(curr, 1);
          ( (Vector) (table.elementAt(idx))).setElementAt(prev, 1);
          imageTableModel.setDataVector(table, columnNames);
          imageTable.setModel(imageTableModel);
          imageTable.getColumnModel().getColumn(1).setCellRenderer(
              imgTblCellRenderer);
        }
        if (idx == 0 || (idx - 1) == 0) {
          imageToFront(selectedName);
        }
      }
    }
    else if (command.equals("down")) {
      if (!UI.isScriptRecording()) {
        int idx = imageTable.getSelectedRow();
        if (idx > -1) {
          selectedName = (String) imageTable.getValueAt(idx + 1, 1);
        }
        if (idx < (imageTable.getRowCount() - 1) && idx > -1) {
          Object curr = ( (Vector) (table.elementAt(idx))).elementAt(1);
          Object next = ( (Vector) (table.elementAt(idx + 1))).elementAt(1);
          ( (Vector) (table.elementAt(idx + 1))).setElementAt(curr, 1);
          ( (Vector) (table.elementAt(idx))).setElementAt(next, 1);
          imageTableModel.setDataVector(table, columnNames);
          imageTable.setModel(imageTableModel);
          imageTable.getColumnModel().getColumn(1).setCellRenderer(
              imgTblCellRenderer);
        }
        if (idx == 0) {
          imageToFront(selectedName);
        }
      }
    }

  }

  public JTable getImageTable() {
    return imageTable;
  }


  /** Using the supplied name as the image name, this method finds the
   *  frame associated with the image and brings it to the front.  Does
   *  nothing when selectedName is <CODE>null</CODE>.
   * @throws NullPointerException when the <CODE>selectedName</CODE> is in the image list, but not associated with any frame.
   * @throws IllegalArgumentException if the <CODE>selectedName</CODE> cannot be found in the image list
   */
  private void imageToFront(String selectedName) throws NullPointerException,
      IllegalArgumentException {
    // if (imageList.isSelectionEmpty())
    //  return; // no selected name.  fail quietly.

    UI.getFrameContainingImage(UI.getRegisteredImageByName(selectedName)).
        toFront();

  }

  public void removeName(String name) {
      for (int i = 0; i < table.length(); i++) {
        String objName = (String) ( (Vector) table.elementAt(i)).elementAt(1);
        if (objName != null &&
            name != null &&
            objName.equals(name)) {
          table.remove(i);
        }
      }
      for (int i = 0; i < table.length(); i++) {
        Vector obj = (Vector) table.elementAt(i);
        obj.setElementAt("$active" + (i + 1), 0);
      }
      imageTableModel.setDataVector(table, columnNames);
      imageTable.setModel(imageTableModel);
      imageTable.getColumnModel().getColumn(1).setCellRenderer(imgTblCellRenderer);
  }

  public void addName(String name) {
    if (!UI.isScriptRecording()) {
      Vector row = new Vector();
      row.add("$active");
      row.add(name);
      table.push(row);
      for (int i = 0; i < table.length(); i++) {
        Vector obj = (Vector) table.elementAt(i);
        obj.setElementAt("$active" + (i + 1), 0);
      }
      imageTableModel.setDataVector(table, columnNames);
      imageTable.setModel(imageTableModel);
      imageTable.getColumnModel().getColumn(1).setCellRenderer( imgTblCellRenderer);
    }
  }

  public class ImageTableCellRenderer
      extends JLabel
      implements TableCellRenderer {
    private ImageIcon frame = MipavUtil.getIcon("frame.gif");
    private ImageIcon floating = MipavUtil.getIcon("rect.gif");

    /** Identifies components that can be used as "rubber stamps" to paint
     *  the cells in a JList.  Preset the label to "opaque" true.
     */
    // if this class becomes public and is no longer "inner",
    // then there will need to be more arguments in the constructor...
    public ImageTableCellRenderer() {
      setOpaque(true);
    }

    /** Return a component that has been configured to display the specified
     * value. That component's <code>paint</code> method is then called to
     * "render" the cell.  If it is necessary to compute the dimensions
     * of a list because the list cells do not have a fixed size, this method
     * is called to generate a component on which <code>getPreferredSize</code>
     * can be invoked.
     * <p>
     * Images which are shown to have an associated frame (only Image A)
     * will show the "frame.gif" icon.  All others will show the "rect.gif"
     * icon.
     *
     * @param table The JTable we're painting.
     * @param value The value returned by list.getModel().getElementAt(index).
     * @param index The cells index.
     * @param isSelected True if the specified cell was selected.
     * @param cellHasFocus True if the specified cell has the focus.
         * @return A component whose paint() method will render the specified value.
     *
     * @see ViewUserInterface#getFrameContainingImage(ModelImage)
     * @see JList
     * @see ListSelectionModel
     * @see ListModel
     *
     */
    public Component getTableCellRendererComponent(JTable tabl, Object value,
        boolean isSelected, boolean cellHasFocus, int row, int col) {
      // if there is no associated frame with the name, use the "floating"
      // icon, else, we'll show it as having a "Frame" icon.
      //  note: IMAGE B is not found to have an associated frame!
      String name = value.toString();
      setText(name);

      if (UI.getFrameContainingImage(UI.getRegisteredImageByName(name)) == null) {
        setIcon(floating);
      }
      else {
        setIcon(frame);
      }

      // choose coloration
      if (isSelected) {
        setBackground(tabl.getSelectionBackground());
        setForeground(tabl.getSelectionForeground());
      }
      else {
        setBackground(tabl.getBackground());
        setForeground(tabl.getForeground());
      }
      setEnabled(tabl.isEnabled());
      setFont(tabl.getFont());
      setOpaque(true);
      return this;
    }

  } // end ImageCellRenderer

  /** As an extension of MouseAdapter, this class merely responds on
   *  clicked list events.  In particular, the double-clicked item is brought
   *  to the front if the image is associated with an image frame.
   * @see ViewJFrameRegisteredImages#bringToFront(String)
   */
  private class MouseClickAdapter
      extends MouseAdapter {
    /** Responds only on double-clicked events from the JList.
     *  The double-clicked list item brings to the front the frame
     *  of the associated image.
     *  <p>
     *  Ignores ClassCastExceptions, and will present to the
     *  user the error messages if there are NullPointerExceptions.
     *
     * @see ViewJFrameRegisteredImages#bringToFront(String)
     */
    public void mouseClicked(MouseEvent event) {
      if (event.getClickCount() == 1) {
        String selectedName = "''";
        try {
          JTable tbl = (JTable) event.getSource();
          int idx = tbl.getSelectedRow();
          if (idx >= 0) {
            selectedName = (String) tbl.getValueAt(idx, 1);
            imageToFront(selectedName);
          }
        }
        catch (ClassCastException cce) {
          Preferences.debug("ViewJFrameRegisteredImages." +
                            "MouseClickAdapter tried to handle " +
                            "something that wasn't a " +
                            "javax.swing.JList.\n", 4);
        }
        catch (NullPointerException npe) {
          MipavUtil.displayError("There is no associated " +
                                 "image-frame!\n" +
                                 "Can't bring " +
                                 selectedName +
                                 " to front");
          Preferences.debug("Image " +
                            selectedName +
                            " was not " +
                            "associated with an image frame.  " +
                            "Either the image is still in use, " +
                            "or it was not deleted in " +
                            "error.\n"); // log.
        }
        catch (IllegalArgumentException iae) {
          Preferences.debug("Illegal Argument Exception in " +
                            "ViewJFrameRegisteredImages when clicking on " +
                            "ToFront.  Somehow the Image list sent " +
                            "an incorrect name to the image image hashtable." +
                            "\n", 2);
        }
      }
    }
  } // end class MouseClickAdapter

  /**
   * Title: VectorStack
       * Description:  VectorStack implements the Stack interface, providing a set of
   * operations for manipulating a LIFO (Last In First Out) structure
   * that can be used to store references to objects.
   */
  public class VectorStack
      extends Vector
      implements Stack {
    /**
     * no constructor needed (default constructor calls the Vector
     * default constructor)
     */

    /**
     * pushes the given value on the top of the stack
     * @param value  Object
     */
    public void push(Object value) {
      addElement(value);
    }

    /**
     *  !empty(), removes and returns the value at the top of the stack;
     *       raises an exception if stack is empty
     * @return object  Return the top object on the stack.
     */
    public Object pop() {
      if (empty())
        throw new RuntimeException("Attempt to pop an empty stack");
      Object result = lastElement();
      removeElementAt(size()-1);
      return result;
    }

    /**
     * returns true if stack is empty; otherwise returns false
     * @return flag    true or false
     */
    public boolean empty() {
      return (size() == 0);
    }

    /**
     * returns the number of values currently stored in the stack
     * @return length  size of the stack
     */
    public int length() {
      return size();
    }
  }

// Interface Stack defines a set of operations for manipulating a LIFO
// (Last In First Out) structure that can be used to store references
// to objects.  It has the following public methods:
  interface Stack {
    /** pushes the given value on the top of the stack */
    public void push(Object value);

    /** removes and returns the value at the top of the stack */
    public Object pop();

    /** returns true if the stack is empty; otherwise returns false */
    public boolean empty();

    /** returns the number of elements currently stored in the stack */
    public int length();
  }

}
