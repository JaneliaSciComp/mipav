package gov.nih.mipav.view;


import java.awt.*;
import java.awt.event.*;

import java.util.*;

import javax.swing.*;


/**
 * DOCUMENT ME!
 *
 * @version  1.0 Mar 1, 1999
 * @author   Matthew J. McAuliffe, Ph.D.
 */

public class JVOIChecklist extends JPanel implements MouseListener {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** Use serialVersionUID for interoperability. */
    private static final long serialVersionUID = 1481985505949073115L;

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** DOCUMENT ME! */
    JList list;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Creates a new JVOIChecklist object.
     *
     * @param  lists  DOCUMENT ME!
     */
    public JVOIChecklist(ViewList[] lists) {

        int i;
        Font serif;
        Icon icon1;
        Icon icon2;
        Vector data;

        serif = MipavUtil.font12;
        setFont(serif);

        // setBackground(Color.lightGray);
        if (lists == null) {
            return;
        }

        list = new JList();
        list.setFont(serif);
        list.setBackground(Color.lightGray);
        list.setCellRenderer(new ListItemRenderer());

        data = new Vector();

        icon1 = MipavUtil.getIcon("boxs.gif");
        icon2 = MipavUtil.getIcon("boxu.gif");

        for (i = 0; i < lists.length; i++) {
            data.addElement(new ListItem(lists[i], icon1, icon2));
        }

        list.setListData(data);
        list.addMouseListener(this);
        list.setSelectedIndex(1);

        add(list, "Center");
        validate();
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * DOCUMENT ME!
     *
     * @param  event  DOCUMENT ME!
     */
    public void mouseClicked(MouseEvent event) { }

    /**
     * DOCUMENT ME!
     *
     * @param  event  DOCUMENT ME!
     */
    public void mouseEntered(MouseEvent event) { }

    /**
     * DOCUMENT ME!
     *
     * @param  event  DOCUMENT ME!
     */
    public void mouseExited(MouseEvent event) { }

    /**
     * DOCUMENT ME!
     *
     * @param  event  DOCUMENT ME!
     */
    public void mousePressed(MouseEvent event) { }

    /**
     * DOCUMENT ME!
     *
     * @param  event  DOCUMENT ME!
     */
    public void mouseReleased(MouseEvent event) {
        int index = list.getSelectedIndex();
        ListItem item = (ListItem) (list.getModel().getElementAt(index));
        item.toggleSelected();
        repaint();
    }


    /**
     * DOCUMENT ME!
     *
     * @param  lists  DOCUMENT ME!
     */
    public void updateList(ViewList[] lists) {
        ListItem item;

        for (int i = 0; i < lists.length; i++) {
            item = (ListItem) (list.getModel().getElementAt(i));

            if (lists[i].getState() == true) {

                if (!item.isSelected()) {
                    item.toggleSelected();
                }
            } else {

                if (item.isSelected()) {
                    item.toggleSelected();
                }
            }
        }

        repaint();
    }
}


/**
 * DOCUMENT ME!
 */
class ListItem {

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** DOCUMENT ME! */
    private Icon iconSelected;

    /** DOCUMENT ME! */
    private Icon iconUnselected;

    /** DOCUMENT ME! */
    private ViewList list;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Creates a new ListItem object.
     *
     * @param  _list  DOCUMENT ME!
     * @param  iconS  DOCUMENT ME!
     * @param  iconU  DOCUMENT ME!
     */
    public ListItem(ViewList _list, Icon iconS, Icon iconU) {

        list = _list;
        iconSelected = iconS;
        iconUnselected = iconU;
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    public Icon getIcon() {

        if (list.getState() == true) {
            return iconSelected;
        } else {
            return iconUnselected;
        }
    }

    /**
     * DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    public String getTitle() {
        return list.getString();
    }

    /**
     * DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    public boolean isSelected() {
        return list.getState();
    }

    /**
     * DOCUMENT ME!
     */
    public void toggleSelected() {

        if (list.getState() == true) {
            list.setState(false);
        } else {
            list.setState(true);
        }
    }

}

/**
 * DOCUMENT ME!
 */
class ListItemRenderer implements ListCellRenderer {

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** DOCUMENT ME! */
    JLabel label;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Creates a new ListItemRenderer object.
     */
    public ListItemRenderer() {

        label = new JLabel();
        label.setOpaque(true);
    }

    //~ Methods --------------------------------------------------------------------------------------------------------


    /**
     * DOCUMENT ME!
     *
     * @param   list          DOCUMENT ME!
     * @param   value         DOCUMENT ME!
     * @param   index         DOCUMENT ME!
     * @param   isSelected    DOCUMENT ME!
     * @param   cellHasFocus  DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    public Component getListCellRendererComponent(JList list, Object value, int index, boolean isSelected,
                                                  boolean cellHasFocus) {
        ListItem listItem = null;

        if (value instanceof ListItem) {
            listItem = (ListItem) value;
        }

        label.setFont(list.getFont());

        if (isSelected) {
            label.setBackground(list.getSelectionBackground());
            label.setForeground(list.getSelectionForeground());
        } else {
            label.setBackground(list.getBackground());
            label.setForeground(list.getForeground());
        }

        if (listItem != null) {
            label.setText(listItem.getTitle());
            label.setIcon(listItem.getIcon());
        } else {
            label.setText(value.toString());
        }

        return label;
    }
}
