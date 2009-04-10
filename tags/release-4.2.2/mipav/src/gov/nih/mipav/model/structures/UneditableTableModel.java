package gov.nih.mipav.model.structures;


import javax.swing.table.*;


/**
 * <p>Title: UneditableTableModel</p>
 *
 * <p>Description: The purpose of this class is to override the DefaultTableModel so that the corresponding JTable's
 * cells are not editable.</p>
 *
 * <p>Copyright: Copyright (c) 2005</p>
 *
 * <p>Company: NIH</p>
 *
 * @author   Lee Orsino
 * @version  1.0
 */
public class UneditableTableModel extends DefaultTableModel {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** Use serialVersionUID for interoperability. */
    private static final long serialVersionUID = 6185277710154266940L;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Creates a new UneditableTableModel object.
     */
    public UneditableTableModel() {
        super();
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * Called by table renderer. Always returns false.
     *
     * @param   row     int
     * @param   column  int
     *
     * @return  boolean
     */
    public boolean isCellEditable(int row, int column) {
        return false;
    }
}
