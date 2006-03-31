package gov.nih.mipav.model.structures;

import javax.swing.table.*;

/**
 *
 * <p>Title: UneditableTableModel</p>
 *
 * <p>Description: The purpose of this class is to override the DefaultTableModel so that the
 * corresponding JTable's cells are not editable.</p>
 *
 * <p>Copyright: Copyright (c) 2005</p>
 *
 * <p>Company: NIH</p>
 *
 * @author Lee Orsino
 * @version 1.0
 */
public class UneditableTableModel extends DefaultTableModel
{
    public UneditableTableModel()
    {
        super();
    }

    /**
     * Called by table renderer. Always returns false.
     *
     * @param row int
     * @param column int
     * @return boolean
     */
    public boolean isCellEditable(int row, int column)
    {
        return false;
    }
}
