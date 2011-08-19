package gov.nih.mipav.view;


import javax.swing.*;


/**
 * This class is simply a table that goes in the query GUI and displays progress and error messages. In order for
 * another function to call showMessage, it should call setMessageType first - the latter tells the table what column to
 * display the message in.
 *
 * @version  1.0 July 1, 1999
 * @author   Neva Cherniavsky
 * @see      ViewJFrameDICOMQuery
 */
public class DICOMDisplayer extends JTable {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** Use serialVersionUID for interoperability. */
    private static final long serialVersionUID = -3693967365082796271L;

    /** A status type message is "Sending query", "Moving images", etc. */
    public static final int STATUS = 0;

    /** A progress type message is the image number that is saved. */
    public static final int PROGRESS = 1;

    /** A source type message is the server from which the images are coming. */
    public static final int SOURCE = 2;

    /** A destination type message is the file directory to which the images are going. */
    public static final int DESTINATION = 3;

    /** An error type message is one thrown in an exception or if the files were not saved. */
    public static final int ERROR = 4;

    /** DOCUMENT ME! */
    public static int ID = 5;

    /** DOCUMENT ME! */
    private static int row = -1;

    /** DOCUMENT ME! */
    private static int col = -1;

    /** DOCUMENT ME! */
    private static boolean succeeded = false;

    /** DOCUMENT ME! */
    private static int[] message_id = new int[20];

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** DOCUMENT ME! */
    private String[] columnNames = { "Status", "#", "Source", "Destination", "Error", "ID" };

    /** DOCUMENT ME! */
    private ViewTableModel model;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Creates a new table and table model and sets up the headers.
     */
    public DICOMDisplayer() {
        super();
        model = new ViewTableModel();
        setModel(model);

        for (int i = 0; i < columnNames.length; i++) {
            model.addColumn(columnNames[i]);
        }

        getColumn(columnNames[0]).setMinWidth(100);
        getColumn(columnNames[0]).setMaxWidth(125);
        getColumn(columnNames[1]).setMinWidth(25);
        getColumn(columnNames[1]).setMaxWidth(25);
        getColumn(columnNames[2]).setMinWidth(50);
        getColumn(columnNames[2]).setMaxWidth(100);
        getColumn(columnNames[3]).setMinWidth(200);
        getColumn(columnNames[3]).setMaxWidth(1000);
        getColumn(columnNames[4]).setMinWidth(50);
        getColumn(columnNames[4]).setMaxWidth(600);
        getColumn(columnNames[5]).setMinWidth(50);
        getColumn(columnNames[5]).setMaxWidth(100);

    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * Returns the ID from the specificed row.
     *
     * @param   row  Row to get ID from.
     *
     * @return  The id.
     */
    public static int getIDFromRow(int row) {
        return message_id[row];
    }

    /**
     * Returns the row number that has a matching ID.
     *
     * @param   id  - to match to IDs in each row
     *
     * @return  the row that has the matching ID; If this fails return -1
     */
    public static int getRowFromID(int id) {

        for (int i = 0; i < message_id.length; i++) {

            if (message_id[i] == id) {
                return i;
            }
        }

        return -1;
    }

    /**
     * Static method that returns a boolean indicating if the program succeeded in saving the images to the home
     * directory.
     *
     * @return  flag indicating success
     */
    public static boolean getSucceeded() {
        return succeeded;
    }

    /**
     * Sets the type of message so it will be displayed in the appropriate column.
     *
     * @param  process  DOCUMENT ME!
     * @param  type     type to set column to.
     */
    public static void setMessageType(int process, int type) {
        row = process;
        col = type;
    }

    /**
     * Sets the success flag.
     *
     * @param  success  flag indicating whether or not the file save succeeded
     */
    public void setSucceeded(boolean success) {
        succeeded = success;
    }

    /**
     * Displays the message in the appropriate column.
     *
     * @param  s  message to be displayed.
     */
    public void showMessage(String s) {

        String[] empty = { "", "", "", "", "" };

        if (row == model.getRowCount()) {
            model.addRow(empty);
        }

        model.setValueAt(s, row, col);

        if (col == ID) {
            message_id[row] = Integer.valueOf(s).intValue();
        }

        repaint();
    }

    /**
     * Makes the row that's written into the next row available.
     *
     * @return  DOCUMENT ME!
     */
    public int updateRow() {
        row = model.getRowCount();

        return row;
    }
}
